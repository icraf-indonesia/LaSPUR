# R/engine/06_economic_valuation.R
# ============================================================
# Land Distribution & Economic Net Present Value (NPV) Valuation
# ============================================================

#' Calculate land cover proportions within planning units
#'
#' Computes the proportion of area covered by each LULC class within each planning unit,
#' either via spatial intersection with LULC vector polygons or using a predefined
#' zoning-to-LULC allocation matrix.
#'
#' @param pu `sf` polygon object representing planning units.
#' @param lulc Optional `sf` object of LULC polygons (used if calculate_from_matrix is FALSE).
#' @param id_pu Character naming planning unit ID column (default "id_pu").
#' @param lulc_class Character naming class column in LULC (default "class").
#' @param use_parallel Logical. Execute spatial intersection in parallel.
#' @param workers Number of parallel workers (default 4).
#' @param calculate_from_matrix Logical. If TRUE, allocates LULC proportions from matrix_tbl.
#' @param matrix_tbl Data frame with columns: type, class, proportion.
#' @param selected_zone Character naming zoning column in `pu` for matrix lookup.
#' @return Data frame with columns `id_pu`, `lulc_class`, and `proportion`.
#' @export
calculate_land_distribution <- function(pu, lulc = NULL,
                                        id_pu = "id_pu",
                                        lulc_class = "class",
                                        use_parallel = FALSE,
                                        workers = 4,
                                        calculate_from_matrix = FALSE,
                                        matrix_tbl = NULL,
                                        selected_zone = NULL) {
  if (!inherits(pu, "sf")) stop("pu must be an sf object")
  
  if (calculate_from_matrix) {
    if (is.null(matrix_tbl))
      stop("calculate_from_matrix = TRUE but matrix_tbl is NULL")
    if (!is.data.frame(matrix_tbl))
      stop("matrix_tbl must be a data frame")
    if (ncol(matrix_tbl) < 3)
      stop("matrix_tbl must have at least three columns: type, class, proportion (in that order)")
    if (is.null(selected_zone) || !is.character(selected_zone))
      stop("selected_zone must be a character string specifying the column in pu that contains planning unit types")
    if (!selected_zone %in% names(pu))
      stop(paste("Column", selected_zone, "not found in pu"))
    
    long_mat <- matrix_tbl[, 1:3]
    colnames(long_mat) <- c("type", "class", "proportion")
    long_mat$proportion <- as.numeric(long_mat$proportion)
    
    type_sums <- tapply(long_mat$proportion, long_mat$type, sum, na.rm = TRUE)
    if (any(abs(type_sums - 1) > 1e-6)) {
      bad_types <- names(type_sums)[abs(type_sums - 1) > 1e-6]
      stop(paste("For types", paste(bad_types, collapse = ", "),
                 "proportions do not sum to 1 (tolerance 1e-6)"))
    }
    
    result_list <- vector("list", length = nrow(pu))
    for (i in seq_len(nrow(pu))) {
      pu_type <- as.character(pu[[selected_zone]][i])
      if (is.na(pu_type)) next
      type_rows <- long_mat[long_mat$type == pu_type, ]
      if (nrow(type_rows) == 0) next
      pu_result <- data.frame(
        pu_id      = rep(pu[[id_pu]][i], nrow(type_rows)),
        class      = type_rows$class,
        proportion = type_rows$proportion,
        stringsAsFactors = FALSE
      )
      colnames(pu_result)[1] <- id_pu
      colnames(pu_result)[2] <- lulc_class
      result_list[[i]] <- pu_result
    }
    result <- do.call(rbind, Filter(Negate(is.null), result_list))
    rownames(result) <- NULL
    return(as.data.frame(result))
  }
  
  # LULC intersection
  if (is.null(lulc)) stop("When calculate_from_matrix = FALSE, lulc must be provided")
  if (!inherits(lulc, "sf")) stop("lulc must be an sf object")
  
  current_crs <- sf::st_crs(pu)
  if (current_crs$IsGeographic) {
    bbox        <- sf::st_bbox(pu)
    mean_lon    <- (bbox[["xmin"]] + bbox[["xmax"]]) / 2
    mean_lat    <- (bbox[["ymin"]] + bbox[["ymax"]]) / 2
    utm_zone    <- floor((mean_lon + 180) / 6) + 1
    epsg_metric <- if (mean_lat >= 0) 32600 + utm_zone else 32700 + utm_zone
    target_crs  <- sf::st_crs(epsg_metric)
    message("Geographic CRS detected. Transforming to UTM zone ", utm_zone,
            " (EPSG:", epsg_metric, ") for area calculation.")
    pu   <- sf::st_transform(pu, target_crs)
    lulc <- sf::st_transform(lulc, target_crs)
  } else {
    if (!sf::st_crs(lulc) == current_crs) {
      message("CRS mismatch: transforming LULC to match planning unit CRS.")
      lulc <- sf::st_transform(lulc, current_crs)
    }
  }
  
  if (use_parallel && requireNamespace("furrr", quietly = TRUE)) {
    future::plan(future::multisession, workers = workers)
    plan_list   <- split(pu, seq_len(nrow(pu)))
    result_list <- furrr::future_map_dfr(plan_list, function(pu_sub) {
      idx <- sf::st_intersects(pu_sub, lulc, sparse = FALSE)[1, ]
      if (!any(idx)) return(NULL)
      lulc_sub <- lulc[idx, ]
      inter    <- sf::st_intersection(pu_sub[, id_pu, drop = FALSE],
                                      lulc_sub[, lulc_class, drop = FALSE])
      if (nrow(inter) == 0) return(NULL)
      inter$area_m2 <- as.numeric(sf::st_area(inter))
      inter %>%
        as.data.frame() %>%
        dplyr::group_by(!!rlang::sym(id_pu), !!rlang::sym(lulc_class)) %>%
        dplyr::summarise(class_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop")
    }, .progress = TRUE)
    all_areas <- dplyr::bind_rows(result_list)
    
    if (nrow(all_areas) == 0) stop("No intersections found")
    total_area <- all_areas %>%
      dplyr::group_by(!!rlang::sym(id_pu)) %>%
      dplyr::summarise(total_m2 = sum(class_m2), .groups = "drop")
    result <- all_areas %>%
      dplyr::left_join(total_area, by = id_pu) %>%
      dplyr::mutate(proportion = class_m2 / total_m2) %>%
      dplyr::select(!!rlang::sym(id_pu), !!rlang::sym(lulc_class), proportion)
    
    future::plan(future::sequential)
    return(as.data.frame(result))
  } else {
    intersection <- sf::st_intersection(pu[, id_pu, drop = FALSE],
                                        lulc[, lulc_class, drop = FALSE])
    dt         <- data.table::as.data.table(intersection)
    dt[, area_m2 := as.numeric(sf::st_area(intersection))]
    class_area <- dt[, .(class_m2 = sum(area_m2)), by = c(id_pu, lulc_class)]
    total_area <- dt[, .(total_m2 = sum(area_m2)), by = id_pu]
    all_areas  <- merge(class_area, total_area, by = id_pu)
    all_areas[, proportion := class_m2 / total_m2]
    result     <- all_areas[, .(proportion), by = c(id_pu, lulc_class)]
    return(as.data.frame(result))
  }
}

#' Calculate Net Present Value (NPV) per hectare per unit
#'
#' Computes the area-weighted NPV per hectare for each spatial unit based on
#' LULC proportions and corresponding per-hectare economic values.
#'
#' @param land_distribution Data frame containing land use/land cover proportion columns.
#' @param npv_lulc Data frame with at least three columns (ID, LC class, npv_ha).
#' @return Input data frame with LULC proportion columns replaced by numeric `npv_ha`.
#' @export
calculate_npv_ha_per_unit <- function(land_distribution, npv_lulc) {
  if (ncol(npv_lulc) < 3) {
    stop("npv_lulc must have at least 3 columns (ID, LC, npv_ha)")
  }
  lc_vector  <- npv_lulc[[2]]
  npv_vector <- npv_lulc[[3]]
  npv_lookup <- stats::setNames(npv_vector, lc_vector)
  
  npv_idx <- grep("^npv_ha_actual", names(land_distribution))
  if (length(npv_idx) > 0) {
    start_idx <- max(npv_idx)
  } else {
    start_idx <- match("alt_RZWP3K", names(land_distribution))
  }
  
  if (is.na(start_idx) || start_idx >= ncol(land_distribution)) {
    candidate_cols <- names(land_distribution)
  } else {
    candidate_cols <- names(land_distribution)[(start_idx + 1):ncol(land_distribution)]
  }
  
  prop_cols <- intersect(candidate_cols, names(npv_lookup))
  
  if (length(prop_cols) == 0) {
    prop_cols <- intersect(names(land_distribution), names(npv_lookup))
  }
  
  result <- land_distribution %>%
    dplyr::mutate(
      npv_ha = purrr::pmap_dbl(
        dplyr::select(., dplyr::all_of(prop_cols)),
        function(...) {
          props <- c(...)
          props[is.na(props)] <- 0
          sum(props * npv_lookup[prop_cols])
        }
      )
    ) %>%
    dplyr::select(-dplyr::all_of(prop_cols))
  
  return(result)
}

#' Calculate comprehensive economic NPV evaluation for actual and recommended zoning
#'
#' @param alt_map_with_decision `sf` object containing planning units with zoning assignments.
#' @param npv_lulc Data frame with LULC classes and NPV/ha values.
#' @param matriks_land_distribution_rtrw Matrix or data frame of land distribution rules for RTRW zones.
#' @param matriks_land_distribution_rzwp3k Matrix or data frame of land distribution rules for RZWP3K zones.
#' @param class_name Character naming LULC class column (default "LC").
#' @param id_pu Character naming planning unit ID column (default "id_pu").
#' @param area_col Character naming area column in hectares (default "area_ha").
#' @param id_col Character naming unique feature ID column (default "id").
#' @return `sf` object augmented with actual, recommended, and delta NPV columns.
#' @export
calculate_economic_npv <- function(
    alt_map_with_decision,
    npv_lulc,
    matriks_land_distribution_rtrw,
    matriks_land_distribution_rzwp3k,
    class_name = "LC",
    id_pu = "id_pu",
    area_col = "area_ha",
    id_col = "id"
) {
  required_cols <- c(id_col, id_pu, "RTRW", "RZWP3K", area_col)
  missing <- setdiff(required_cols, names(alt_map_with_decision))
  if (length(missing) > 0) {
    stop("alt_map_with_decision missing columns: ", paste(missing, collapse = ", "))
  }
  
  pu_rtrw <- alt_map_with_decision %>%
    dplyr::filter(!is.na(RTRW)) %>%
    dplyr::select(-RZWP3K)
  
  pu_rzwp3k <- alt_map_with_decision %>%
    dplyr::filter(!is.na(RZWP3K)) %>%
    dplyr::select(-RTRW)
  
  get_distribution <- function(pu, matrix_tbl, zone_type) {
    calculate_land_distribution(
      pu = pu,
      id_pu = id_pu,
      lulc_class = class_name,
      calculate_from_matrix = TRUE,
      matrix_tbl = matrix_tbl,
      selected_zone = zone_type
    )
  }
  
  # RTRW actual
  land_dist_rtrw_long <- get_distribution(pu_rtrw, matriks_land_distribution_rtrw, "RTRW")
  land_dist_rtrw_wide <- land_dist_rtrw_long %>%
    tidyr::pivot_wider(
      names_from = dplyr::all_of(class_name),
      values_from = proportion,
      values_fill = 0
    )
  land_dist_rtrw <- sf::st_drop_geometry(pu_rtrw) %>%
    dplyr::left_join(land_dist_rtrw_wide, by = id_pu)
  
  # RZWP3K actual
  land_dist_rzwp3k_long <- get_distribution(pu_rzwp3k, matriks_land_distribution_rzwp3k, "RZWP3K")
  land_dist_rzwp3k_wide <- land_dist_rzwp3k_long %>%
    tidyr::pivot_wider(
      names_from = dplyr::all_of(class_name),
      values_from = proportion,
      values_fill = 0
    )
  land_dist_rzwp3k <- sf::st_drop_geometry(pu_rzwp3k) %>%
    dplyr::left_join(land_dist_rzwp3k_wide, by = id_pu)
  
  # Actual NPV/ha
  npv_actual_rtrw <- calculate_npv_ha_per_unit(land_dist_rtrw, npv_lulc) %>%
    dplyr::rename(npv_ha_actual_rtrw = npv_ha)
  
  npv_actual_rzwp3k <- calculate_npv_ha_per_unit(land_dist_rzwp3k, npv_lulc) %>%
    dplyr::rename(npv_ha_actual_rzwp3k = npv_ha)
  
  # Recommended RTRW
  land_dist_rtrw_recom_only <- calculate_land_distribution(
    pu = pu_rtrw,
    id_pu = id_pu,
    lulc_class = class_name,
    calculate_from_matrix = TRUE,
    matrix_tbl = matriks_land_distribution_rtrw,
    selected_zone = "alt_RTRW"
  ) %>%
    tidyr::pivot_wider(
      names_from = dplyr::all_of(class_name),
      values_from = proportion,
      values_fill = 0
    )
  if (inherits(land_dist_rtrw_recom_only, "sf")) {
    land_dist_rtrw_recom_only <- sf::st_drop_geometry(land_dist_rtrw_recom_only)
  }
  
  land_dist_rtrw_recom <- npv_actual_rtrw %>%
    dplyr::left_join(land_dist_rtrw_recom_only, by = id_pu)
  
  # Recommended RZWP3K
  land_dist_rzwp3k_recom_only <- calculate_land_distribution(
    pu = pu_rzwp3k,
    id_pu = id_pu,
    lulc_class = class_name,
    calculate_from_matrix = TRUE,
    matrix_tbl = matriks_land_distribution_rzwp3k,
    selected_zone = "alt_RZWP3K"
  ) %>%
    tidyr::pivot_wider(
      names_from = dplyr::all_of(class_name),
      values_from = proportion,
      values_fill = 0
    )
  if (inherits(land_dist_rzwp3k_recom_only, "sf")) {
    land_dist_rzwp3k_recom_only <- sf::st_drop_geometry(land_dist_rzwp3k_recom_only)
  }
  
  land_dist_rzwp3k_recom <- npv_actual_rzwp3k %>%
    dplyr::left_join(land_dist_rzwp3k_recom_only, by = id_pu)
  
  # Recommended NPV/ha
  npv_recom_rtrw <- calculate_npv_ha_per_unit(land_dist_rtrw_recom, npv_lulc) %>%
    dplyr::rename(npv_ha_recom_rtrw = npv_ha)
  
  npv_recom_rzwp3k <- calculate_npv_ha_per_unit(land_dist_rzwp3k_recom, npv_lulc) %>%
    dplyr::rename(npv_ha_recom_rzwp3k = npv_ha)
  
  # Total economic values
  npv_rtrw <- npv_recom_rtrw %>%
    dplyr::mutate(
      econ_rtrw_actual = .data[[area_col]] * npv_ha_actual_rtrw,
      econ_rtrw_recom  = .data[[area_col]] * npv_ha_recom_rtrw,
      econ_rtrw_delta  = econ_rtrw_recom - econ_rtrw_actual
    )
  
  npv_rzwp3k <- npv_recom_rzwp3k %>%
    dplyr::mutate(
      econ_rzwp3k_actual = .data[[area_col]] * npv_ha_actual_rzwp3k,
      econ_rzwp3k_recom  = .data[[area_col]] * npv_ha_recom_rzwp3k,
      econ_rzwp3k_delta  = econ_rzwp3k_recom - econ_rzwp3k_actual
    )
  
  new_cols_rtrw <- setdiff(names(npv_rtrw), names(alt_map_with_decision))
  new_cols_rzwp3k <- setdiff(names(npv_rzwp3k), names(alt_map_with_decision))
  
  adjacent_economy_map <- alt_map_with_decision %>%
    dplyr::left_join(
      npv_rtrw %>% dplyr::select(dplyr::all_of(c(id_col, id_pu, new_cols_rtrw))),
      by = c(id_col, id_pu)
    ) %>%
    dplyr::left_join(
      npv_rzwp3k %>% dplyr::select(dplyr::all_of(c(id_col, id_pu, new_cols_rzwp3k))),
      by = c(id_col, id_pu)
    )
  
  return(adjacent_economy_map)
}
