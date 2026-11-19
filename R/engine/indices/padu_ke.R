# R/engine/indices/padu_ke.R
# ============================================================
# PADU-KE (Keterpaduan Penggunaan Lahan dan Lautan) Engine 
# ============================================================

#' Calculate summary distribution of a PADU indicator score column
#'
#' @param data `sf` or `data.frame` containing `score_col`.
#' @param score_col Character string name of the score column.
#' @return `tibble` of distribution statistics (n, n_na, min, q25, median, mean, q75, max, sd).
#' @export
padu_distribution_summary <- function(data, score_col) {
  vals <- data[[score_col]]
  tibble::tibble(
    indicator = score_col,
    n = length(vals),
    n_na = sum(is.na(vals)),
    min = if (all(is.na(vals))) NA_real_ else min(vals, na.rm = TRUE),
    q25 = if (all(is.na(vals))) NA_real_ else as.numeric(stats::quantile(vals, 0.25, na.rm = TRUE, names = FALSE)),
    median = if (all(is.na(vals))) NA_real_ else stats::median(vals, na.rm = TRUE),
    mean = if (all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE),
    q75 = if (all(is.na(vals))) NA_real_ else as.numeric(stats::quantile(vals, 0.75, na.rm = TRUE, names = FALSE)),
    max = if (all(is.na(vals))) NA_real_ else max(vals, na.rm = TRUE),
    sd = if (all(is.na(vals)) || length(stats::na.omit(vals)) < 2) NA_real_ else stats::sd(vals, na.rm = TRUE)
  )
}

#' Generate empty or default PADU-KE matrix template from class table
#'
#' @param tbl Data frame containing class definitions (at least 2 columns).
#' @param fill_value Value for off-diagonal cells (default NA).
#' @param file_path Optional path to save Excel spreadsheet.
#' @return Tibble representing the square matrix.
#' @export
generate_matrix_padu_ke <- function(tbl, fill_value = NA, file_path = NULL) {
  if (!is.data.frame(tbl)) {
    stop("Input 'tbl' must be a data frame or tibble.")
  }
  if (ncol(tbl) < 2) {
    stop("Input must have at least two columns: id and class name.")
  }
  
  classes <- unique(as.character(tbl[[2]]))
  classes <- classes[!is.na(classes) & nzchar(trimws(classes))]
  n_classes <- length(classes)
  
  if (n_classes == 0) {
    stop("No valid class names found in column 2.")
  }
  
  # Square numeric matrix
  mat <- matrix(fill_value, nrow = n_classes, ncol = n_classes)
  diag(mat) <- 3
  
  matrix_tibble <- tibble::as_tibble(mat, .name_repair = "minimal")
  names(matrix_tibble) <- classes
  matrix_tibble <- dplyr::bind_cols(tibble::tibble(class = classes), matrix_tibble)
  
  if (is.null(file_path)) {
    return(matrix_tibble)
  }
  
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Package 'openxlsx2' is required but not installed.")
  }
  
  wb <- openxlsx2::wb_workbook()$add_worksheet(sheet = "Matriks PADU-KE")
  wb$add_data(x = matrix_tibble, col_names = TRUE, row_names = FALSE)
  
  nrows <- nrow(matrix_tibble) + 1
  ncols <- ncol(matrix_tibble)
  
  header_dims    <- paste0("A1:", openxlsx2::int2col(ncols), "1")
  first_col_dims <- paste0("A1:A", nrows)
  
  black <- openxlsx2::wb_color(hex = "FF000000")
  white <- openxlsx2::wb_color(hex = "FFFFFFFF")
  
  # Header row
  wb$
    add_fill(dims = header_dims, color = openxlsx2::wb_color(hex = "FF1F4E79"))$
    add_font(dims = header_dims, color = white, size = 11, bold = TRUE)$
    add_border(dims = header_dims,
               top_border = "thin", top_color = black,
               bottom_border = "thin", bottom_color = black,
               left_border = "thin", left_color = black,
               right_border = "thin", right_color = black,
               inner_vgrid = "thin", inner_vcolor = black)$
    add_cell_style(dims = header_dims, horizontal = "center", vertical = "center", wrap_text = TRUE)
  
  # First column
  wb$
    add_fill(dims = first_col_dims, color = openxlsx2::wb_color(hex = "FF1F4E79"))$
    add_font(dims = first_col_dims, color = white, size = 11, bold = TRUE)$
    add_border(dims = first_col_dims,
               top_border = "thin", top_color = black,
               bottom_border = "thin", bottom_color = black,
               left_border = "thin", left_color = black,
               right_border = "thin", right_color = black,
               inner_hgrid = "thin", inner_hcolor = black)$
    add_cell_style(dims = first_col_dims, horizontal = "center", vertical = "center", wrap_text = TRUE)
  
  # Matrix body
  if (nrows >= 2 && ncols >= 2) {
    body_dims <- paste0(openxlsx2::int2col(2), "2:", openxlsx2::int2col(ncols), nrows)
    wb$
      add_fill(dims = body_dims, color = openxlsx2::wb_color(hex = "FFF5F5DC"))$
      add_border(dims = body_dims,
                 top_border = "thin", top_color = black,
                 bottom_border = "thin", bottom_color = black,
                 left_border = "thin", left_color = black,
                 right_border = "thin", right_color = black,
                 inner_hgrid = "thin", inner_hcolor = black,
                 inner_vgrid = "thin", inner_vcolor = black)$
      add_cell_style(dims = body_dims, horizontal = "center", vertical = "center", wrap_text = TRUE)
  }
  
  wb$freeze_pane(first_row = TRUE, first_col = TRUE)
  wb$set_col_widths(cols = 1, width = 30)
  if (ncols >= 2) wb$set_col_widths(cols = 2:ncols, width = 25)
  
  wb$save(file_path, overwrite = TRUE)
  return(invisible(matrix_tibble))
}

#' Calculate LULC adjacency percentages per planning unit
#'
#' @param lulc `SpatRaster` or `sf` polygon object of land use / land cover.
#' @param admin_vector `sf` or `SpatVector` planning unit polygons.
#' @param id_col Character naming unique ID column in `admin_vector`.
#' @param class_col Character naming class column in `lulc` (for vector LULC).
#' @param parallel Logical. Run in parallel.
#' @param workers Number of parallel workers.
#' @param plan_strategy Parallel strategy ("multisession" or "multicore").
#' @param progress Logical. Display progress bar.
#' @return Data frame with columns `id_pu`, `Class_A`, `Class_B`, `Edge_Count`, `percentage`.
#' @export
calculate_lulc_adjacency <- function(lulc,
                                     admin_vector,
                                     id_col = "id_pu",
                                     class_col = NULL,
                                     parallel = FALSE,
                                     workers = NULL,
                                     plan_strategy = c("multisession", "multicore"),
                                     progress = TRUE) {
  UseMethod("calculate_lulc_adjacency")
}

#' @export
calculate_lulc_adjacency.SpatRaster <- function(lulc,
                                                admin_vector,
                                                id_col = "id_pu",
                                                class_col = NULL,
                                                parallel = FALSE,
                                                workers = NULL,
                                                plan_strategy = c("multisession", "multicore"),
                                                progress = TRUE) {
  plan_strategy <- match.arg(plan_strategy)
  
  if (inherits(admin_vector, "SpatVector")) {
    admin_sf <- sf::st_as_sf(admin_vector)
  } else if (inherits(admin_vector, "sf")) {
    admin_sf <- admin_vector
  } else {
    stop("admin_vector must be an sf or SpatVector object.")
  }
  
  if (!id_col %in% names(admin_sf)) stop("Column '", id_col, "' not found.")
  if (terra::crs(lulc) != sf::st_crs(admin_sf)$wkt) {
    message("CRS mismatch. Reprojecting admin_vector...")
    admin_sf <- sf::st_transform(admin_sf, terra::crs(lulc))
  }
  
  admin_vect <- terra::vect(admin_sf)
  admin_list <- split(admin_vect, f = id_col)
  
  if (parallel) {
    old_plan <- future::plan("list")
    on.exit(future::plan(old_plan), add = TRUE)
    if (is.null(workers)) workers <- future::availableCores()
    if (plan_strategy == "multisession") {
      future::plan(future::multisession, workers = workers)
    } else {
      future::plan(future::multicore, workers = workers)
    }
  }
  
  output_df <- furrr::future_map_dfr(
    admin_list,
    function(poly) {
      cropped <- terra::crop(lulc, poly, mask = TRUE, touches = FALSE)
      adj <- landscapemetrics::get_adjacencies(cropped, neighbourhood = 4, what = "triangle")
      adj_matrix <- adj[[1]]
      if (is.null(adj_matrix) || length(adj_matrix) == 0) return(NULL)
      
      adj_df <- as.data.frame(as.table(adj_matrix)) |>
        dplyr::rename(Class_A = Var1, Class_B = Var2, Edge_Count = Freq) |>
        dplyr::filter(!is.na(Edge_Count), Edge_Count > 0)
      
      if (nrow(adj_df) == 0) return(NULL)
      
      total_adj <- sum(adj_df$Edge_Count, na.rm = TRUE)
      adj_df |>
        dplyr::mutate(
          id_pu = as.character(terra::values(poly)[[id_col]][1]),
          percentage = (Edge_Count / total_adj) * 100
        )
    },
    .progress = progress,
    .options = furrr::furrr_options(
      packages = c("terra", "sf", "landscapemetrics", "dplyr")
    )
  )
  
  if (is.null(output_df) || nrow(output_df) == 0) {
    warning("No adjacency data found.")
    return(data.frame())
  }
  
  return(output_df[, c("id_pu", "Class_A", "Class_B", "Edge_Count", "percentage")])
}

#' @export
calculate_lulc_adjacency.sf <- function(lulc,
                                        admin_vector,
                                        id_col = "id_pu",
                                        class_col = NULL,
                                        parallel = FALSE,
                                        workers = NULL,
                                        plan_strategy = c("multisession", "multicore"),
                                        progress = TRUE) {
  plan_strategy <- match.arg(plan_strategy)
  
  if (is.null(class_col)) stop("For vector LULC, provide 'class_col' argument.")
  if (!inherits(lulc, "sf")) stop("lulc must be an sf object.")
  if (!class_col %in% names(lulc)) stop("Column '", class_col, "' not found in lulc.")
  
  geom_types <- sf::st_geometry_type(lulc)
  if (!any(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("LULC must contain polygon geometries.")
  }
  
  if (inherits(admin_vector, "SpatVector")) {
    admin_sf <- sf::st_as_sf(admin_vector)
  } else if (inherits(admin_vector, "sf")) {
    admin_sf <- admin_vector
  } else {
    stop("admin_vector must be an sf or SpatVector object.")
  }
  
  if (!id_col %in% names(admin_sf)) stop("Column '", id_col, "' not found.")
  
  message("Cleaning LULC geometries...")
  if (!all(sf::st_is_valid(lulc))) {
    lulc <- sf::st_make_valid(lulc) |> sf::st_buffer(dist = 0)
    if (any(sf::st_geometry_type(lulc) == "GEOMETRYCOLLECTION")) {
      lulc <- suppressWarnings(sf::st_collection_extract(lulc, "POLYGON", warn = FALSE))
    }
  }
  
  if (sf::st_crs(lulc) != sf::st_crs(admin_sf)) {
    message("Reprojecting admin_vector to match LULC CRS...")
    admin_sf <- sf::st_transform(admin_sf, sf::st_crs(lulc))
  }
  
  admin_ids <- unique(admin_sf[[id_col]])
  
  safe_extract_polygons <- function(x) {
    if (is.null(x) || nrow(x) == 0) return(NULL)
    if (any(sf::st_geometry_type(x) == "GEOMETRYCOLLECTION")) {
      x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON", warn = FALSE))
    }
    if (is.null(x) || nrow(x) == 0) return(NULL)
    x <- x[sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON"), ]
    if (nrow(x) == 0) return(NULL)
    x <- x[!sf::st_is_empty(x), ]
    if (nrow(x) == 0) return(NULL)
    x
  }
  
  if (parallel) {
    old_plan <- future::plan("list")
    on.exit(future::plan(old_plan), add = TRUE)
    if (is.null(workers)) workers <- future::availableCores()
    if (plan_strategy == "multisession") {
      future::plan(future::multisession, workers = workers)
    } else {
      future::plan(future::multicore, workers = workers)
    }
  }
  
  results <- furrr::future_map(
    admin_ids,
    function(uid) {
      poly <- admin_sf[admin_sf[[id_col]] == uid, ]
      
      zero_row <- function(uid) {
        data.frame(
          id_pu = as.character(uid),
          Class_A = NA_character_,
          Class_B = NA_character_,
          Edge_Count = 0,
          percentage = 0,
          stringsAsFactors = FALSE
        )
      }
      
      lulc_clip <- tryCatch(
        sf::st_intersection(lulc, poly),
        error = function(e) {
          warning("Admin unit ", uid, " error: ", e$message)
          return(NULL)
        }
      )
      if (is.null(lulc_clip) || nrow(lulc_clip) == 0) {
        return(list(success = zero_row(uid)))
      }
      
      lulc_clip <- safe_extract_polygons(lulc_clip)
      if (is.null(lulc_clip) || nrow(lulc_clip) == 0) {
        return(list(success = zero_row(uid)))
      }
      
      if (!all(sf::st_is_valid(lulc_clip))) {
        lulc_clip <- sf::st_make_valid(lulc_clip) |> sf::st_buffer(dist = 0)
        lulc_clip <- safe_extract_polygons(lulc_clip)
        if (is.null(lulc_clip) || nrow(lulc_clip) == 0) {
          return(list(success = zero_row(uid)))
        }
      }
      
      lulc_clip$class_code <- as.character(lulc_clip[[class_col]])
      
      s2_was_on <- sf::sf_use_s2()
      if (s2_was_on) sf::sf_use_s2(FALSE)
      
      touches_list <- tryCatch(
        sf::st_touches(lulc_clip, lulc_clip),
        error = function(e) {
          warning("Admin unit ", uid, " touches error: ", e$message)
          return(NULL)
        }
      )
      
      if (s2_was_on) sf::sf_use_s2(TRUE)
      
      if (is.null(touches_list)) {
        return(list(success = zero_row(uid)))
      }
      
      pair_counts <- data.frame()
      n <- nrow(lulc_clip)
      for (i in seq_len(n)) {
        if (length(touches_list[[i]]) == 0) next
        class_i <- lulc_clip$class_code[i]
        for (j in touches_list[[i]]) {
          if (i >= j) next
          class_j <- lulc_clip$class_code[j]
          pair <- sort(c(class_i, class_j))
          pair_counts <- rbind(pair_counts, data.frame(
            Class_A = pair[1], Class_B = pair[2], stringsAsFactors = FALSE
          ))
        }
      }
      
      if (nrow(pair_counts) == 0) {
        return(list(success = zero_row(uid)))
      }
      
      adj_df <- pair_counts |>
        dplyr::group_by(Class_A, Class_B) |>
        dplyr::summarise(Edge_Count = dplyr::n(), .groups = "drop")
      
      total_adj <- sum(adj_df$Edge_Count)
      adj_df <- adj_df |>
        dplyr::mutate(
          id_pu = as.character(uid),
          percentage = (Edge_Count / total_adj) * 100
        )
      
      return(list(success = adj_df[, c("id_pu", "Class_A", "Class_B", "Edge_Count", "percentage")]))
    },
    .progress = progress,
    .options = furrr::furrr_options(packages = c("sf", "dplyr"))
  )
  
  success_list <- purrr::map(results, "success")
  output_df <- dplyr::bind_rows(success_list)
  
  return(output_df)
}

#' Calculate PADU-KE Index (Konektivitas Ekologis)
#'
#' @param matriks_padu_ke Data frame with columns `class1`, `class2`, `idx_padu_ke`.
#' @param lulc_ref List or data frame mapping class IDs to class names.
#' @param lulc_adjacencies Data frame with `id_pu`, `Class_A`, `Class_B`, `percentage`.
#' @param idx_serasi_map `sf` object serving as base planning units.
#' @param normalize Logical. Scale index to 0-1 (default TRUE).
#' @return A standardized list containing:
#'   \item{data}{`sf` object with `idx_padu_ke` column}
#'   \item{score_col}{Character "idx_padu_ke"}
#'   \item{summary}{`tibble` with score distribution statistics}
#'   \item{metadata}{List of runtime parameters and timestamp}
#'   \item{idx_padu_ke_map}{Alias for `data`}
#'   \item{idx_padu_ke}{`tibble` table of scores}
#' @export
calculate_padu_ke <- function(matriks_padu_ke,
                              lulc_ref,
                              lulc_adjacencies,
                              idx_serasi_map,
                              normalize = TRUE) {
  # Convert class names to IDs in matriks_padu_ke
  matriks_padu_ke_id <- matriks_padu_ke %>%
    dplyr::transmute(
      class_id1 = lulc_ref[[1]][match(.data$class1, lulc_ref[[2]])],
      class_id2 = lulc_ref[[1]][match(.data$class2, lulc_ref[[2]])],
      adj_index = .data$idx_padu_ke
    )
  
  lulc_adjacencies <- lulc_adjacencies %>%
    dplyr::mutate(
      Class_A = as.integer(as.character(Class_A)),
      Class_B = as.integer(as.character(Class_B))
    )
  
  required_adj <- c("id_pu", "Class_A", "Class_B", "percentage")
  required_idx <- c("class_id1", "class_id2", "adj_index")
  
  missing_adj <- setdiff(required_adj, names(lulc_adjacencies))
  missing_idx <- setdiff(required_idx, names(matriks_padu_ke_id))
  
  if (length(missing_adj) > 0) {
    stop("lulc_adjacencies missing: ", paste(missing_adj, collapse = ", "))
  }
  if (length(missing_idx) > 0) {
    stop("matriks_padu_ke missing required columns: ", paste(missing_idx, collapse = ", "))
  }
  
  df <- lulc_adjacencies %>%
    dplyr::left_join(matriks_padu_ke_id,
                     by = c("Class_A" = "class_id1", "Class_B" = "class_id2"))
  
  unmatched <- sum(is.na(df$adj_index))
  if (unmatched > 0) warning(unmatched, " class pair(s) missing adj_index.")
  
  idx_padu_ke <- df %>%
    dplyr::filter(!is.na(adj_index)) %>%
    dplyr::mutate(weighted = percentage * adj_index) %>%
    dplyr::group_by(id_pu) %>%
    dplyr::summarise(abs_idx_padu_ke = sum(weighted, na.rm = TRUE), .groups = "drop")
  
  if (normalize) {
    max_val <- max(matriks_padu_ke_id$adj_index, na.rm = TRUE)
    idx_padu_ke <- idx_padu_ke %>%
      dplyr::mutate(idx_padu_ke = abs_idx_padu_ke / (max_val * 100))
  }
  
  idx_padu_ke_map <- idx_serasi_map %>%
    dplyr::mutate(id_pu = as.character(id_pu)) %>%
    dplyr::left_join(idx_padu_ke, by = "id_pu")
  
  score_col <- "idx_padu_ke"
  summary_tbl <- padu_distribution_summary(idx_padu_ke_map, score_col)
  
  list(
    data = idx_padu_ke_map,
    score_col = score_col,
    summary = summary_tbl,
    metadata = list(
      timestamp = Sys.time(),
      normalize = normalize
    ),
    idx_padu_ke_map = idx_padu_ke_map,
    idx_padu_ke = idx_padu_ke
  )
}
