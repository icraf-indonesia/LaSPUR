# R/engine/03_analysis_adjacent.R
# ============================================================
# Adjacency Analysis, Boundary Pairs & Topology Processing
# ============================================================
# Indentifikasi Area Bersentuhan ------------------------------------------

#' Identify adjacent pairs between two spatial layers (RTRW and RZWP3K)
#'
#' This function finds all pairs of features that touch each other between two
#' spatial layers (RTRW and RZWP3K), creates a new layer where each
#' touching pair is represented as two separate rows (one for each source
#' feature) linked by a common mapping unit ID (id_pu), and saves the result to a
#' GeoPackage. The function also applies optional geometry simplification and
#' area filtering.
#'
#' @param rtrw An `sf` object representing the first layer (e.g., RTRW).
#'            Must be a valid `sf` object (no `st_make_valid()` is performed
#'            inside the function – assume it is already valid).
#' @param rzwp An `sf` object representing the second layer (e.g., RZWP3K).
#'            Must be a valid `sf` object.
#' @param out_gpkg Character string: file path to the output GeoPackage.
#' @param min_area_ha Numeric: minimum area in hectares for features to be
#'        considered. Features below this threshold are excluded. Default = 0.
#' @param nama_field_rtrw Character: name of the column in `rtrw` that contains
#'        the area name. Default = "RTRW".
#' @param nama_field_rzwp Character: name of the column in `rzwp` that contains
#'        the area name. Default = "RZWP3K".
#' @param simplify_geometry Logical: if `TRUE`, simplifies geometries using
#'        `st_simplify()` to reduce file size. Default = FALSE.
#' @param simplify_tolerance Numeric: tolerance (in meters) for simplification
#'        when `simplify_geometry = TRUE`. Default = 5.
#' @param batch_progress_interval Integer: show progress message every
#'        N processed pairs. Default = 250.
#' @param show_detailed_progress Logical: if `TRUE`, shows progress per RTRW
#'        feature. Default = TRUE.
#'
#' @return Invisibly returns the resulting `sf` object (the combined adjacent
#'         pairs) after writing it to `out_gpkg`. The object contains columns:
#'         `id`, `id_pu` (mapping unit ID), `RTRW`, `RZWP3K`, and
#'         `geometry`.
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Aligns CRS if different (projects RZWP3K to RTRW's CRS).
#'   \item Optionally simplifies geometries using `st_simplify()`.
#'   \item Calculates area in hectares and filters features below `min_area_ha`.
#'   \item Finds touching pairs using `st_touches()` with a progress bar.
#'   \item Builds a data frame where each touching pair contributes two rows
#'         (one for RTRW feature, one for RZWP3K feature) sharing the same `id_pu`.
#'   \item Combines all rows, converts to `sf`, adds an `id`, and saves to
#'         GeoPackage.
#' }
#'
#' @note The function does NOT run `st_make_valid()` on the input objects.
#'       Ensure the input `sf` objects are geometrically valid before calling
#'       this function. All console messages are printed in Indonesian.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' rtrw <- st_read("path_to_rtrw.gpkg")
#' rzwp <- st_read("path_to_rzwp.gpkg")
#'
#' result <- identify_adjacent(
#'   rtrw = rtrw,
#'   rzwp = rzwp,
#'   out_gpkg = "adjacent_pairs.gpkg",
#'   min_area_ha = 0.5,
#'   nama_field_rtrw = "RTRW_NAME",
#'   simplify_geometry = TRUE,
#'   simplify_tolerance = 2
#' )
#' }
#'
#' @importFrom sf st_crs st_transform st_touches st_geometry st_as_sf st_write st_simplify
#' @importFrom dplyr bind_rows mutate select
#' @importFrom units set_units
#' @importFrom utils object.size
#' @export
identify_adjacent <- function(rtrw,
                              rzwp,
                              min_area_ha = 1,
                              nama_field_rtrw = "RTRW",
                              nama_field_rzwp = "RZWP3K",
                              batch_progress_interval = 250,
                              show_detailed_progress = TRUE,
                              m_precision = 1
) {
  # Input validation
  if (!inherits(rtrw, "sf")) stop("rtrw harus berupa objek sf")
  if (!inherits(rzwp, "sf")) stop("rzwp harus berupa objek sf")
  
  rtrw <- rtrw %>% sf::st_make_valid()
  rzwp <- rzwp %>% sf::st_make_valid()
  
  if (sf::st_crs(rtrw) != sf::st_crs(rzwp)) {
    rzwp <- sf::st_transform(rzwp, sf::st_crs(rtrw))
  } 
  
  # Calculate and filter based on area size
  rtrw_filter <- rtrw %>%
    dplyr::mutate(
      id_SRC  = paste0("R", dplyr::row_number()),
      area_ha = as.numeric(units::set_units(sf::st_area(geometry), "ha"))
    ) %>%
    dplyr::filter(area_ha >= min_area_ha)
  
  rzwp_filter <- rzwp %>%
    dplyr::mutate(
      id_SRC  = paste0("Z", dplyr::row_number()),
      area_ha = as.numeric(units::set_units(sf::st_area(geometry), "ha"))
    ) %>%
    dplyr::filter(area_ha >= min_area_ha)
  
  if (nrow(rtrw_filter) == 0 | nrow(rzwp_filter) == 0) {
    stop("Tidak ada pasangan yang mungkin karena salah satu layer kosong setelah filter area.")
  }
  
  # Convert n_precision (in meters) to acceptable value for sf_set_precision
  n_precision <- st_get_precision_for_meters(rtrw_filter, m_precision, silent = FALSE)
  
  # Set precision
  rtrw_fixed <- sf::st_set_precision(rtrw_filter, n_precision) |> sf::st_make_valid()
  rzwp_fixed <- sf::st_set_precision(rzwp_filter, n_precision) |> sf::st_make_valid() 
  
  # Identify touches
  message("Identifikasi area RTRW dan RZWP3K yang berdampingan.")
  pairs_idx <- sf::st_touches(rtrw_fixed, rzwp_fixed)
  total_pairs <- sum(lengths(pairs_idx))
  message("Total pasangan ditemukan: ", total_pairs)
  
  if (total_pairs == 0) {
    stop("Tidak ditemukan pasangan yang saling berdampingan antara RTRW dan RZWP3K.")
  }
  
  # Function to safely extract the class name
  safe_name <- function(row_sf, field_name) {
    if (field_name %in% names(row_sf)) {
      val <- row_sf[[field_name]][1]
      if (is.na(val) || length(val) == 0) return(NA_character_)
      val <- gsub("[^A-Za-z0-9[:space:]\\-\\(\\)]", "", as.character(val))
      val <- trimws(val)
      if (nchar(val) > 100) val <- substr(val, 1, 100)
      return(val)
    } else {
      return(NA_character_)
    }
  }
  
  # Construct pairing table
  message("Menyusun dataframe pasangan area berdampingan")
  
  df_list <- list()
  PU_counter <- 1
  pairs_processed <- 0
  features_with_pairs <- 0
  
  for (i in seq_len(nrow(rtrw_filter))) {
    idxs <- pairs_idx[[i]]
    if (length(idxs) > 0) {
      features_with_pairs <- features_with_pairs + 1
      if (show_detailed_progress && i %% 50 == 0) {
        pct_rtrw <- round(i / nrow(rtrw_filter) * 100, 1)
        message("      >> Memproses RTRW ke-", i, " dari ", nrow(rtrw_filter),
                " (", pct_rtrw, "%) - ditemukan ", length(idxs), " pasangan")
      }
      
      geom_rtrw <- sf::st_geometry(rtrw_filter[i, ])
      area_rtrw <- rtrw_filter$area_ha[i]
      
      for (j in idxs) {
        # Baris RTRW – id diambil dari id_SRC sumber
        df_list[[length(df_list) + 1]] <- data.frame(
          id = rtrw_filter$id_SRC[i],
          id_pu = PU_counter,
          RTRW = safe_name(rtrw_filter[i, ], nama_field_rtrw),
          RZWP3K = NA_character_,
          area_ha = area_rtrw, 
          geometry = geom_rtrw,
          stringsAsFactors = FALSE
        )
        
        # Baris RZWP3K – id diambil dari id_SRC sumber
        df_list[[length(df_list) + 1]] <- data.frame(
          id = rzwp_filter$id_SRC[j],
          id_pu = PU_counter,
          RTRW = NA_character_,
          RZWP3K = safe_name(rzwp_filter[j, ], nama_field_rzwp),
          area_ha = rzwp_filter$area_ha[j],
          geometry = sf::st_geometry(rzwp_filter[j, ]),
          stringsAsFactors = FALSE
        )
        
        PU_counter <- PU_counter + 1
        pairs_processed <- pairs_processed + 1
        
        # Notification for pair process progress
        if (pairs_processed %% batch_progress_interval == 0) {
          pct_complete <- round(pairs_processed / total_pairs * 100, 1)
          remaining_pairs <- total_pairs - pairs_processed
          
          message("      [PROGRESS] ", pct_complete, "% (", pairs_processed, "/",
                  total_pairs, " pasangan)")
          gc() # Free memory
        }
      }
    }
  }
  
  message("\n      - Total pasangan diproses: ", pairs_processed)
  
  # Convert dataframe to sf object
  message("Menggabungkan data frames dan mengkonversi ke objek sf.")
  combined_df <- dplyr::bind_rows(df_list)
  pu_sf <- sf::st_as_sf(combined_df, crs = sf::st_crs(rtrw_filter))
  
  pu_sf <- pu_sf %>%
    dplyr::select(id, id_pu, RTRW, RZWP3K, area_ha, geometry)
  
  return(invisible(pu_sf))
}

#' Identify multipair groups and degree in an adjacency sf object
#'
#' Assigns a `id_group` to each pair (`id_pu`) using the connected
#' components of the RTRW–RZWP3K bipartite graph, and adds the true
#' per-feature degree as `n_pairs`.
#'
#' The previous implementation picked a per-pair "center" via
#' `tie_break(id1, id2) = min(id1, id2)` whenever both endpoints had
#' degree > 1. This is not transitive: a hub connected to several
#' satellites would be fragmented into one group per satellite. Using
#' connected components makes grouping invariant to numeric id ordering.
#'
#' `id` values are expected to be layer-namespaced ("R110", "Z331").
#' If the input still carries legacy integer ids, `normalize_legacy_ids()`
#' is applied first.
#'
#' @param sf_obj An `sf` object with columns `id` and `id_pu`.
#'   Each `id_pu` must appear exactly twice (one RTRW row, one RZWP3K row).
#' @param tie_break Deprecated and ignored; kept for API compatibility.
#' @return The same `sf` object with `id_group` (integer) and `n_pairs`
#'   (integer) appended.
#' @export
identify_adjacent_group <- function(sf_obj, tie_break = NULL) {
  if (!inherits(sf_obj, "sf")) stop("sf_obj must be an sf object")
  if (!all(c("id", "id_pu") %in% names(sf_obj)))
    stop("sf_obj must have 'id' and 'id_pu' columns")
  if (!all(c("RTRW", "RZWP3K") %in% names(sf_obj)))
    stop("sf_obj must have 'RTRW' and 'RZWP3K' columns")

  sf_obj <- normalize_legacy_ids(sf_obj)

  degree <- sf_obj %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(n_pairs = dplyr::n_distinct(id_pu), .groups = "drop") %>%
    sf::st_drop_geometry()
  
  edges_df <- sf_obj %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(id_pu) %>%
    dplyr::summarise(
      r = dplyr::first(id[!is.na(RTRW)]),
      z = dplyr::first(id[!is.na(RZWP3K)]),
      .groups = "drop"
    )
  
  edges_df <- edges_df[!is.na(edges_df$r) & !is.na(edges_df$z), , drop = FALSE]

  if (nrow(edges_df) == 0) {
    result <- sf_obj %>%
      dplyr::left_join(degree, by = "id") %>%
      dplyr::mutate(
        id_group = 0L,
        n_pairs  = ifelse(is.na(n_pairs), 0L, n_pairs)
      )
    return(result)
  }
  
  g <- igraph::graph_from_data_frame(
    edges_df[, c("r", "z")],
    directed = FALSE
  )
  comp_membership <- igraph::components(g)$membership

  edge_group <- data.frame(
    id_pu    = edges_df$id_pu,
    id_group = as.integer(comp_membership[as.character(edges_df$r)]),
    stringsAsFactors = FALSE
  )

  result <- sf_obj %>%
    dplyr::left_join(edge_group, by = "id_pu") %>%
    dplyr::left_join(degree,     by = "id") %>%
    dplyr::mutate(
      id_group = ifelse(is.na(id_group), 0L, id_group),
      n_pairs  = ifelse(is.na(n_pairs),  0L, n_pairs)
    )
  
  trailing <- c("id_group", "n_pairs")
  result <- result[, c(setdiff(names(result), trailing), trailing)]
  
  stopifnot(nrow(result) == nrow(sf_obj))
  return(result)
}

#' Process adjacent polygons by computing shared boundary lengths and buffer areas
#'
#' This function takes a set of paired RTRW and RZWP3K polygons (as returned by
#' `identify_adjacent()`) and calculates the length of the shared boundary between
#' each pair. It then computes a buffer area by multiplying that length by a
#' user‑supplied distance. The result is appended to the original data.
#'
#' @param pu_sf An `sf` object produced by `identify_adjacent()`. It must contain
#'   at least the columns `id`, `id_pu`, `RTRW`, `RZWP3K`, `area_ha`, and the
#'   geometry. Rows with missing `RTRW` or `RZWP3K` are treated as separate groups.
#' @param buffer_m Numeric. The buffer width in metres. The buffer area (in hectares)
#'   is computed as `(length * buffer_m) / 10000`.
#' @param m_precision Numeric. Desired geometric precision in meters for
#'   `sf::st_set_precision()`. Default = 1 (one metre grid). Increase if you
#'   encounter topological errors.
#' @param snap_tolerance Numeric. Tolerance (in metres) used to snap the RZWP3K
#'   boundary to the RTRW boundary before intersection. This helps to avoid
#'   missing shared segments due to slight misalignments. Default = 0.5.
#' @param parallel Logical. If `TRUE`, the boundary length calculations for each
#'   pair are run in parallel. Default `FALSE`.
#' @param workers Number of parallel workers (default = `future::availableCores()`).
#' @param plan_strategy The `future` plan to use: `"multisession"` (all platforms)
#'   or `"multicore"` (Unix only).
#' @param progress Logical. Show a progress bar? Default `TRUE` (only used when
#'   `parallel = TRUE`; the sequential loop prints its own progress message).
#'
#' @return An `sf` object with the same geometry and CRS as the input `pu_sf`,
#'   augmented with two new columns:
#'   \item{length}{The length of the shared boundary between the RTRW and RZWP3K
#'     polygons for each `id_pu` (in metres).}
#'   \item{area_buffer_ha}{The buffer area in hectares, calculated as
#'     `(length * buffer_m) / 10000`.}
#'   The original columns (`id`, `id_pu`, `RTRW`, `RZWP3K`, `area_ha`) are preserved.
#'
#' @details The function proceeds as follows:
#'   \enumerate{
#'     \item If the input is in a geographic CRS (longlat), it is transformed to
#'       a suitable UTM zone (based on the centroid of the bounding box) for metric
#'       calculations. Otherwise the existing CRS is used.
#'     \item The geometry is made valid with `st_make_valid()` and its precision
#'       is set using `st_set_precision()` with the converted value from
#'       `st_get_precision_for_meters()`.
#'     \item The data are split into RTRW and RZWP3K subsets (ordered by `id_pu`).
#'       An error is thrown if the numbers of rows do not match.
#'     \item Boundaries are extracted with `st_boundary()`.
#'     \item For each pair, the RZWP3K boundary is snapped to the RTRW boundary
#'       using `snap_tolerance` to handle minor gaps, then the intersection is
#'       computed. The length of the intersection (or 0 if empty) is recorded.
#'       This step can be run in parallel when `parallel = TRUE`.
#'     \item The lengths are joined back to the original `pu_sf` (preserving its
#'       original CRS) and the buffer area is calculated.
#'   }
#'
#' @note The function processes pairs sequentially by default. For large datasets,
#'   set `parallel = TRUE` to speed up the boundary intersection steps.
#'
#' @seealso [identify_adjacent()] for creating the required input object.
#'
#' @examples
#' \dontrun{
#'   adj <- identify_adjacent(rtrw, rzwp, m_precision = 1)
#'   result <- process_adjacent(adj, buffer_m = 50, m_precision = 1, parallel = TRUE)
#' }
#'
#' @importFrom sf st_crs st_transform st_geometry st_boundary st_intersection
#'   st_length st_make_valid st_set_precision st_snap st_is_empty st_is_longlat
#'   st_bbox
#' @importFrom dplyr filter arrange left_join mutate select
#' @importFrom furrr future_map_dbl furrr_options
#' @importFrom future plan availableCores multisession multicore
#' @export
process_adjacent <- function(pu_sf,
                             buffer_m,
                             m_precision = 1,
                             snap_tolerance = 0.5,
                             parallel = FALSE,
                             workers = NULL,
                             plan_strategy = c("multisession", "multicore"),
                             progress = TRUE) {
  
  # Input validation
  if (!inherits(pu_sf, "sf")) stop("pu_sf harus berupa objek sf")
  if (!is.numeric(buffer_m) || buffer_m <= 0) stop("buffer_m harus berupa angka positif")
  if (!is.numeric(m_precision) || m_precision <= 0) stop("m_precision harus berupa angka positif")
  if (!is.numeric(snap_tolerance) || snap_tolerance < 0) stop("snap_tolerance harus berupa angka non-negatif")
  
  plan_strategy <- match.arg(plan_strategy)
  
  # CRS handling
  if (sf::st_is_longlat(pu_sf)) {
    bbox <- sf::st_bbox(pu_sf)
    mean_lon <- (bbox[["xmin"]] + bbox[["xmax"]]) / 2
    mean_lat <- (bbox[["ymin"]] + bbox[["ymax"]]) / 2
    utm_zone <- floor((mean_lon + 180) / 6) + 1
    epsg_metric <- if (mean_lat >= 0) 32600 + utm_zone else 32700 + utm_zone
    message("Transformasi dari CRS geografis ke UTM zone ", utm_zone, " (EPSG:", epsg_metric, ")")
    pu_sf_metric <- sf::st_transform(pu_sf, epsg_metric)
  } else {
    message("Menggunakan CRS yang sudah dalam satuan meter.")
    pu_sf_metric <- pu_sf
  }
  pu_sf_metric <- sf::st_make_valid(pu_sf_metric)
  
  # Convert m_precision to appropriate precision value for st_set_precision
  n_precision <- st_get_precision_for_meters(pu_sf_metric, m_precision, silent = FALSE)
  pu_sf_metric <- sf::st_set_precision(pu_sf_metric, n_precision)
  
  # Split into RTRW and RZWP3K parts (ordered by id_pu)
  rtrw_parts <- pu_sf_metric %>% dplyr::filter(!is.na(RTRW)) %>% dplyr::arrange(id_pu)
  rzwp_parts <- pu_sf_metric %>% dplyr::filter(!is.na(RZWP3K)) %>% dplyr::arrange(id_pu)
  
  if (nrow(rtrw_parts) != nrow(rzwp_parts)) {
    stop("Jumlah baris RTRW dan RZWP3K tidak sama. Pastikan data berasal dari identify_adjacent() yang benar.")
  }
  
  # Extract boundaries
  rtrw_boundaries <- sf::st_boundary(sf::st_geometry(rtrw_parts))
  rzwp_boundaries <- sf::st_boundary(sf::st_geometry(rzwp_parts))
  
  # Define the function that computes shared length for a single pair
  # (used both sequentially and in parallel)
  calc_single_length <- function(i) {
    rtrw_i <- rtrw_boundaries[i]
    rzwp_i <- rzwp_boundaries[i]
    rzwp_i_snapped <- sf::st_snap(rzwp_i, rtrw_i, tolerance = snap_tolerance)
    shared_line <- sf::st_intersection(rtrw_i, rzwp_i_snapped)
    if (length(shared_line) == 0 || sf::st_is_empty(shared_line)) {
      return(0)
    } else {
      return(as.numeric(sf::st_length(shared_line)))
    }
  }
  
  # Compute lengths
  if (!parallel) {
    message("Menghitung panjang garis irisan untuk setiap pasangan (sekuensial)...")
    calculated_lengths <- vapply(
      seq_along(rtrw_boundaries),
      calc_single_length,
      numeric(1)
    )
  } else {
    # Parallel execution
    old_plan <- future::plan("list")
    on.exit(future::plan(old_plan), add = TRUE)
    if (is.null(workers)) workers <- future::availableCores()
    if (plan_strategy == "multisession") {
      future::plan(future::multisession, workers = workers)
    } else {
      future::plan(future::multicore, workers = workers)
    }
    
    message("Menghitung panjang garis irisan untuk setiap pasangan (paralel, ", workers, " worker)...")
    calculated_lengths <- furrr::future_map_dbl(
      .x = seq_along(rtrw_boundaries),
      .f = calc_single_length,
      .progress = progress,
      .options = furrr::furrr_options(packages = "sf")
    )
  }
  
  # Join lengths back to original sf
  lengths_lookup <- data.frame(
    id_pu = rtrw_parts$id_pu,
    length = calculated_lengths,
    stringsAsFactors = FALSE
  )
  
  message("Menambahkan kolom length dan menghitung area buffer...")
  pu_sf_result <- pu_sf %>%
    dplyr::left_join(lengths_lookup, by = "id_pu") %>%
    dplyr::mutate(
      area_buffer_ha = (length * buffer_m) / 10000
    ) %>%
    dplyr::select(id, id_pu, id_group, RTRW, RZWP3K, area_ha, length, area_buffer_ha, n_pairs, geometry)
  
  return(pu_sf_result)
}

#' Convert meter precision to CRS-native precision for st_set_precision
#'
#' @param x sf object (used to determine CRS and bounding box)
#' @param m_precision Numeric: desired grid size in meters. Default = 1.
#' @param silent Logical: if FALSE, print conversion details. Default = FALSE.
