# Identifikasi Area Tumpang Tindih ----------------------------------------

#' Identify intersections between two polygon layers
#'
#' @description
#' Performs a spatial intersection operation between two polygon layers, returning
#' only the overlapping areas with preserved attributes from both layers.
#'
#' @param x First layer: `sf` object with polygons
#' @param y Second layer: `sf` object with polygons
#'
#' @return An `sf` object with columns:
#'   - `id_pu`: Sequential ID
#'   - `stat_pu`: Type ("intersection")
#'   - `id_rtrw`: Row number from original `x`
#'   - `id_rzwp3k`: Row number from original `y`
#'   - All attributes from both inputs
#'
#' @examples
#' \dontrun{
#' library(sf)
#' poly1 <- st_read("layer1.shp")
#' poly2 <- st_read("layer2.shp")
#' result <- identify_overlaps(poly1, poly2)
#' }
#'
#' @importFrom sf st_geometry_type st_as_sf st_sfc st_crs st_is_empty
#' @importFrom terra vect makeValid same.crs project intersect nrow crs
#' @importFrom dplyr mutate select rename row_number everything
#'
#' @export
identify_overlaps <- function(x, y) {
  if (!inherits(x, "sf")) stop("x must be an sf object")
  if (!inherits(y, "sf")) stop("y must be an sf object")
  
  geom_type <- c("POLYGON", "MULTIPOLYGON")
  if (!all(sf::st_geometry_type(x, by_geometry = FALSE) %in% geom_type))
    stop("x must contain polygons or multipolygons")
  if (!all(sf::st_geometry_type(y, by_geometry = FALSE) %in% geom_type))
    stop("y must contain polygons or multipolygons")
  
  x_tmp <- x |> dplyr::mutate(.temp_row_id_x = dplyr::row_number())
  y_tmp <- y |> dplyr::mutate(.temp_row_id_y = dplyr::row_number())
  
  x_v <- terra::vect(x_tmp) |> terra::makeValid()
  y_v <- terra::vect(y_tmp) |> terra::makeValid()
  
  if (!terra::same.crs(x_v, y_v)) {
    warning("CRS differ. Reprojecting y to the CRS of x.")
    y_v <- terra::project(y_v, terra::crs(x_v))
  }
  
  # Intersection 
  intersect_v <- terra::intersect(x_v, y_v)
  
  if (terra::nrow(intersect_v) == 0) {
    result <- sf::st_sf(
      geometry = sf::st_sfc(),
      crs = sf::st_crs(terra::crs(x_v))
    )
    result$id_pu <- integer(0)
    result$stat_pu <- character(0)
    result$id_rtrw <- integer(0)
    result$id_rzwp3k <- integer(0)
    all_attr <- unique(c(names(x), names(y)))
    for (col in all_attr) {
      if (!col %in% names(result)) {
        result[[col]] <- logical(0) 
      }
    }
    cols <- c("id_pu", "stat_pu", "id_rtrw", "id_rzwp3k",
              setdiff(names(result), c("id_pu", "stat_pu", "id_rtrw", "id_rzwp3k", "geometry")))
    result <- result[, cols]
    return(result)
  }
  
  result <- sf::st_as_sf(intersect_v)
  
  # Rename temporary row IDs to the required names
  result <- result |>
    dplyr::rename(id_rtrw = .temp_row_id_x,
                  id_rzwp3k = .temp_row_id_y)
  
  result$stat_pu <- "intersection"
  
  all_attr <- unique(c(names(x), names(y)))
  for (col in all_attr) {
    if (!col %in% names(result)) result[[col]] <- NA
  }
  
  result <- result[!sf::st_is_empty(result), ]
  result <- result |>
    dplyr::mutate(id_pu = dplyr::row_number())
  
  cols <- c("id_pu", "stat_pu", "id_rtrw", "id_rzwp3k",
            setdiff(names(result), c("id_pu", "stat_pu", "id_rtrw", "id_rzwp3k", "geometry")),
            "geometry")
  result <- result[, cols]
  
  return(result)
}

#' Identify overlaps between two polygon layers (ArcMap-like union)
#'
#' @description
#' Performs a spatial union operation between two polygon layers, returning
#' intersection, x-only, and y-only areas with preserved attributes.
#'
#' @param x First layer: `sf` object with polygons
#' @param y Second layer: `sf` object with polygons
#'
#' @return An `sf` object with columns:
#'   - `id_pu`: Sequential ID
#'   - `stat_pu`: Type ("intersection", x name, or y name)
#'   - All attributes from both inputs (NAs for missing side)
#'
#' @examples
#' \dontrun{
#' library(sf)
#' poly1 <- st_read("layer1.shp")
#' poly2 <- st_read("layer2.shp")
#' result <- identify_overlaps_union(poly1, poly2)
#' overlaps <- result[result$stat_pu == "intersection", ]
#' }
#'
#' @importFrom sf st_geometry_type st_as_sf st_sfc st_crs st_is_empty
#' @importFrom terra vect makeValid same.crs project intersect erase nrow crs
#' @importFrom dplyr bind_rows mutate select
#'
#' @export
identify_overlaps_union <- function(x, y) {
  if (!inherits(x, "sf")) stop("x must be an sf object")
  if (!inherits(y, "sf")) stop("y must be an sf object")
  
  geom_type <- c("POLYGON", "MULTIPOLYGON")
  if (!all(sf::st_geometry_type(x, by_geometry = FALSE) %in% geom_type))
    stop("x must contain polygons or multipolygons")
  if (!all(sf::st_geometry_type(y, by_geometry = FALSE) %in% geom_type))
    stop("y must contain polygons or multipolygons")
  
  x_v <- terra::vect(x) |> terra::makeValid()
  y_v <- terra::vect(y) |> terra::makeValid()
  
  if (!terra::same.crs(x_v, y_v)) {
    warning("CRS differ. Reprojecting y to the CRS of x.")
    y_v <- terra::project(y_v, terra::crs(x_v))
  }
  
  # Intersection
  intersect_v <- terra::intersect(x_v, y_v)
  if (terra::nrow(intersect_v) == 0) {
    intersect_sf <- sf::st_sf(geometry = sf::st_sfc(), crs = sf::st_crs(terra::crs(x_v)))
  } else {
    intersect_sf <- sf::st_as_sf(intersect_v)
    intersect_sf$stat_pu <- "intersection"
  }
  
  # X only
  x_only_v <- terra::erase(x_v, y_v)
  if (terra::nrow(x_only_v) == 0) {
    x_only_sf <- sf::st_sf(geometry = sf::st_sfc(), crs = sf::st_crs(terra::crs(x_v)))
  } else {
    x_only_sf <- sf::st_as_sf(x_only_v)
    x_only_sf$stat_pu <- as.character(names(x)[1])
    y_attr <- setdiff(names(y_v), names(x_only_sf))
    for (col in y_attr) x_only_sf[[col]] <- NA
  }
  
  # Y only
  y_only_v <- terra::erase(y_v, x_v)
  if (terra::nrow(y_only_v) == 0) {
    y_only_sf <- sf::st_sf(geometry = sf::st_sfc(), crs = sf::st_crs(terra::crs(x_v)))
  } else {
    y_only_sf <- sf::st_as_sf(y_only_v)
    y_only_sf$stat_pu <- as.character(names(y)[1])
    x_attr <- setdiff(names(x_v), names(y_only_sf))
    for (col in x_attr) y_only_sf[[col]] <- NA
  }
  
  # Combine
  result <- dplyr::bind_rows(x_only_sf, y_only_sf, intersect_sf)
  all_cols <- unique(c(names(x_v), names(y_v), "stat_pu"))
  for (col in all_cols) if (!col %in% names(result)) result[[col]] <- NA
  
  result <- result[, c(all_cols[all_cols != "geometry"], "geometry")]
  result <- result[!sf::st_is_empty(result), ]
  result <- result |>
    dplyr::mutate(id_pu = dplyr::row_number()) |>
    dplyr::select(id_pu, stat_pu, dplyr::everything())
  
  return(result)
}

#' Process overlap results: add IDs, area, and size flag
#'
#' @description
#' Adds sequential ID, calculates area in hectares, and flags features based
#' on area threshold. Automatically reprojects to appropriate UTM zone.
#'
#' @param sf_obj An `sf` object with polygon geometry containing a `stat_pu` column
#' @param threshold_ha Numeric threshold for area flag (default 156.25)
#'
#' @return An `sf` object with additional columns:
#'   - `id_pu`: Sequential ID
#'   - `area_ha`: Area in hectares
#'   - `area_flag`: "luas terpenuhi" or "luas tidak terpenuhi"
#'
#' @examples
#' \dontrun{
#' overlaps <- identify_overlaps(poly1, poly2)
#' result <- filter_overlaps(overlaps)
#' }
#'
#' @importFrom sf st_collection_extract st_is_empty st_make_valid st_union
#'   st_centroid st_coordinates st_is_longlat st_transform st_area
#' @importFrom dplyr mutate row_number select if_else
#'
#' @export
filter_overlaps <- function(sf_obj, threshold_ha = 156.25) {
  if (!inherits(sf_obj, "sf")) stop("sf_obj must be an sf object.")
  if (!"stat_pu" %in% names(sf_obj)) stop("sf_obj must contain 'stat_pu' column.")
  
  sf_obj <- sf::st_collection_extract(sf_obj, "POLYGON")
  sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
  if (nrow(sf_obj) == 0) stop("No polygon features remaining.")
  
  sf_obj <- sf::st_make_valid(sf_obj)
  sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
  
  get_utm_epsg <- function(x) {
    union_geom <- sf::st_union(x) |> sf::st_make_valid()
    centroid <- sf::st_centroid(union_geom)
    coords <- sf::st_coordinates(centroid)
    lon <- coords[1, "X"]; lat <- coords[1, "Y"]
    zone <- floor((lon + 180) / 6) + 1
    ifelse(lat >= 0, 32600 + zone, 32700 + zone)
  }
  
  if (sf::st_is_longlat(sf_obj)) {
    target_epsg <- get_utm_epsg(sf_obj)
    message("Reprojecting to UTM zone ", target_epsg %% 100, " (EPSG:", target_epsg, ")")
    sf_obj <- sf::st_transform(sf_obj, target_epsg)
  }
  
  sf_obj <- sf_obj |>
    dplyr::mutate(
      id_pu = dplyr::row_number(),
      area_ha = as.numeric(sf::st_area(geometry)) / 10000
    ) |>
    dplyr::select(id_pu, stat_pu, dplyr::everything()) |>
    dplyr::mutate(area_flag = dplyr::if_else(
      area_ha >= threshold_ha, "luas terpenuhi", "luas tidak terpenuhi"
    ))
  
  return(sf_obj)
}

#' Validate class names in overlap results against reference tibbles
#'
#' @description
#' Compares class values in columns 3 and 4 of an sf object against allowed
#' values from reference tibbles. NA values are ignored in the validation.
#'
#' @param sf_obj An `sf` object with at least 4 non-geometry columns
#' @param tibble1 Data frame with at least 2 columns; 2nd column contains allowed classes for column 3
#' @param tibble2 Data frame with at least 2 columns; 2nd column contains allowed classes for column 4
#'
#' @return Invisibly returns a list with `mismatch_col3` and `mismatch_col4`
#'
#' @examples
#' \dontrun{
#' validate_zone_class(overlap_result, tibble_class1, tibble_class2)
#' }
#'
#' @export
validate_zone_class <- function(sf_obj, tibble1, tibble2) {
  if (!inherits(sf_obj, "sf")) stop("sf_obj must be an sf object.")
  
  n_data_cols <- ncol(sf_obj) - 1
  if (n_data_cols < 4) stop("sf_obj must have at least 4 non-geometry columns.")
  
  col3_values <- sf_obj[["RTRW"]]
  col4_values <- sf_obj[["RZWP3K"]]
  allowed1 <- unique(tibble1[[2]])
  allowed2 <- unique(tibble2[[2]])
  
  # Ignore NA values when checking mismatches
  col3_non_na <- col3_values[!is.na(col3_values)]
  col4_non_na <- col4_values[!is.na(col4_values)]
  
  mismatches3 <- unique(col3_non_na[!col3_non_na %in% allowed1])
  mismatches4 <- unique(col4_non_na[!col4_non_na %in% allowed2])
  
  cat("\n=== Zone Class Validation Report ===\n")
  if (length(mismatches3) == 0) {
    cat("✓", names(sf_obj)[3], " : All non-NA classes match.\n", sep = "")
  } else {
    cat("✗", names(sf_obj)[3], " : Mismatches:\n", sep = "")
    for (val in mismatches3) cat("    - '", val, "'\n", sep = "")
  }
  if (length(mismatches4) == 0) {
    cat("✓", names(sf_obj)[4], " : All non-NA classes match.\n", sep = "")
  } else {
    cat("✗", names(sf_obj)[4], " : Mismatches:\n", sep = "")
    for (val in mismatches4) cat("    - '", val, "'\n", sep = "")
  }
  cat("===================================\n")
  
  invisible(list(mismatch_col3 = mismatches3, mismatch_col4 = mismatches4))
}


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
      id_SRC = dplyr::row_number(),
      area_ha = as.numeric(units::set_units(sf::st_area(geometry), "ha"))
    ) %>%
    dplyr::filter(area_ha >= min_area_ha)
  
  rzwp_filter <- rzwp %>%
    dplyr::mutate(
      id_SRC = dplyr::row_number(),
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
#' For each polygon (identified by `id`), this function counts how many distinct
#' `id_pu` pairs it appears in (the degree). It then adds:
#' \itemize{
#'   \item `multipair`: `"Yes"` if either polygon in the pair has degree > 1.
#'   \item `id_group`: numeric group for each center hub (0 for isolated pairs).
#'   \item `n_pairs`: the number of distinct `id_pu` pairs this polygon is in.
#' }
#'
#' @param sf_obj An `sf` object with columns `id` and `id_pu`. Each `id_pu` appears twice.
#' @param tie_break Function to choose a center when both polygons have degree > 1.
#'   Default: choose the smaller `id`.
#' @return The same `sf` object with added columns `multipair`, `id_group`, and `n_pairs`.
#' @export
identify_adjacent_group <- function(sf_obj,
                                    tie_break = function(x, y) if (x < y) x else y) {
  stopifnot(inherits(sf_obj, "sf"))
  stopifnot(all(c("id", "id_pu") %in% names(sf_obj)))
  
  # Compute degree (n_pairs) per polygon and drop geometry
  degree <- sf_obj %>%
    group_by(id) %>%
    summarise(n_pairs = n_distinct(id_pu), .groups = "drop") %>%
    st_drop_geometry()   
  
  deg_lookup <- setNames(degree$n_pairs, degree$id)
  
  # Build pair table (one row per id_pu) and drop geometry
  pairs <- sf_obj %>%
    group_by(id_pu) %>%
    summarise(id1 = first(id), id2 = last(id), .groups = "drop") %>%
    mutate(
      deg1 = deg_lookup[as.character(id1)],
      deg2 = deg_lookup[as.character(id2)]
    )
  
  pairs_df <- st_drop_geometry(pairs)  
  
  # Assign group numbers to centers 
  center_env <- new.env()
  center_env$next_group <- 1L
  get_group <- function(center_id) {
    key <- as.character(center_id)
    if (is.null(center_env[[key]])) {
      center_env[[key]] <- center_env$next_group
      center_env$next_group <- center_env$next_group + 1L
    }
    center_env[[key]]
  }
  
  pairs_df <- pairs_df %>%
    rowwise() %>%
    mutate(
      multipair_log = (deg1 > 1 | deg2 > 1),
      center = case_when(
        deg1 > 1 & deg2 > 1 ~ tie_break(id1, id2),
        deg1 > 1 ~ id1,
        deg2 > 1 ~ id2,
        TRUE ~ NA_integer_
      ),
      group = if (multipair_log) get_group(center) else 0L
    ) %>%
    ungroup()
  
  # Join back – first with pair info, then with degree
  result <- sf_obj %>%
    left_join(pairs_df, by = "id_pu") %>%
    left_join(degree, by = "id") %>%       
    mutate(
      multipair = ifelse(multipair_log, "Yes", "No"),
      id_group = group
    ) %>%
    select(-id1, -id2, -deg1, -deg2, -multipair_log, -multipair, -center, -group) %>%
    select(all_of(names(sf_obj)), id_group, n_pairs)
  
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
#' @return Numeric: grid size in CRS units (meters for projected, degrees for geographic)
st_get_precision_for_meters <- function(x, m_precision = 1, silent = FALSE) {
  if (sf::st_is_longlat(x)) {
    bbox <- sf::st_bbox(x)
    mean_lat <- mean(c(bbox["ymin"], bbox["ymax"]))
    
    meters_per_deg_lat <- 111132
    meters_per_deg_lon <- 111320 * cos(mean_lat * pi / 180)
    min_meters_per_deg <- min(meters_per_deg_lat, meters_per_deg_lon)
    
    precision_deg <- m_precision / min_meters_per_deg
    
    if (!silent) {
      message(sprintf(
        "CRS Geografis: 1° ≈ %.0f m → precision = %.8f° (≈ %.2f m grid)",
        min_meters_per_deg, precision_deg, m_precision
      ))
    }
    return(precision_deg)
  } else {
    if (!silent) {
      message(sprintf(
        "CRS Terproyeksi: precision = %.3f (satuan CRS = meter, grid = %.2f m)",
        m_precision, m_precision
      ))
    }
    return(m_precision)
  }
}

#' Helper: build a shape with real multi-line text.
#' create_shape() only supports one paragraph/run, so this splits `lines`
#' into separate runs joined by <a:br/> (proper line breaks).
create_multiline_shape <- function(lines, ...) {
  base_xml <- as.character(create_shape(text = "PLACEHOLDER", ...))
  run_match <- regmatches(base_xml, regexpr("<a:r>.*?</a:r>", base_xml, perl = TRUE))
  rpr <- regmatches(run_match, regexpr("<a:rPr.*?</a:rPr>|<a:rPr[^>]*/>", run_match, perl = TRUE))
  if (length(rpr) == 0) rpr <- ""
  
  make_run <- function(txt) {
    txt <- gsub("&", "&amp;", txt, fixed = TRUE)
    txt <- gsub("<", "&lt;",  txt, fixed = TRUE)
    txt <- gsub(">", "&gt;",  txt, fixed = TRUE)
    sprintf('<a:r>%s<a:t xml:space="preserve">%s</a:t></a:r>', rpr, txt)
  }
  
  runs <- paste(vapply(lines, make_run, NA_character_), collapse = "<a:br/>")
  out <- sub("<a:r>.*?</a:r>", runs, base_xml, perl = TRUE)
  read_xml(out, pointer = FALSE)
}

#' Generate a Compatibility Matrix (SERASI)
#'
#' @param sf_1 `sf` object; first column used for row classes
#' @param sf_2 `sf` object; first column used for column headers
#' @param fill_value Initial value for matrix cells (default NA)
#' @param file_path Optional path to write a styled Excel file. If NULL, returns the tibble.
#'
#' @return A tibble (if file_path is NULL), or invisibly returns the workbook object.
#' @export
generate_matrix_serasi <- function(sf_1, sf_2, fill_value = NA, file_path = NULL) {
  rows <- sf_1 |> sf::st_drop_geometry() |> dplyr::distinct(dplyr::across(1)) |> dplyr::pull(1)
  cols <- sf_2 |> sf::st_drop_geometry() |> dplyr::distinct(dplyr::across(1)) |> dplyr::pull(1)
  
  mat <- matrix(fill_value, nrow = length(rows), ncol = length(cols))
  colnames(mat) <- cols
  
  matrix_tibble <- tibble::as_tibble(mat) |>
    tibble::add_column(RTRW_RZWP3K = rows, .before = 1)
  
  if (is.null(file_path)) {
    return(matrix_tibble)
  }
  
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Package 'openxlsx2' is required but not installed.")
  }
  require(openxlsx2)
  
  wb <- wb_workbook()$add_worksheet(sheet = "Matriks SERASI")
  wb$add_data(x = matrix_tibble, col_names = TRUE, row_names = FALSE)
  
  nrows <- nrow(matrix_tibble) + 1
  ncols <- ncol(matrix_tibble)
  
  header_dims    <- paste0("A1:", int2col(ncols), "1")
  first_col_dims <- paste0("A1:A", nrows)
  
  black <- wb_color(hex = "FF000000")
  white <- wb_color(hex = "FFFFFFFF")
  
  # Header row
  wb$
    add_fill(dims = header_dims, color = wb_color(hex = "FF1F4E79"))$
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
    add_fill(dims = first_col_dims, color = wb_color(hex = "FF1F4E79"))$
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
    body_dims <- paste0(int2col(2), "2:", int2col(ncols), nrows)
    wb$
      add_fill(dims = body_dims, color = wb_color(hex = "FFF5F5DC"))$
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
  
  # Instructions as a floating text box 
  instr_row <- nrows + 4
  
  instr_lines <- c(
    "Template matriks SERASI ini menyatakan tingkat kesesuaian lintas-ruang (darat-laut) dan menjadi \u201ckamus kebijakan\u201d yang dipakai LaSPUR untuk menilai kesesuaian pasangan kategori (existing maupun usulan).",
    "",
    "Instruksi Pengisian:",
    "1. Matriks hanya boleh diisi dengan nilai numerik 0, 0.5, dan 1",
    "2. Pengisian nilai disesuaikan dengan hubungan pasangan kawasan, dengan deskripsi sebagai berikut:",
    "     1 = sangat sesuai / langsung selaras kebijakan;",
    "     0.5 = sesuai bersyarat (dapat berjalan dengan pengaturan/mitigasi);",
    "     0 = tidak sesuai (konflik mendasar/harus dihindari).",
    "3. Tidak diperkenankan mengubah header kolom dan baris serta mengisi cell di luar matriks"
  )
  
  shape_xml <- create_multiline_shape(
    instr_lines,
    shape      = "rect",
    name       = "instructions_box",
    fill_color = wb_color(hex = "FFF5F5DC"),
    text_color = black,
    line_color = black,
    text_align = "left"
  )
  
  instr_dims <- paste0("B", instr_row, ":", int2col(max(ncols, 4)), instr_row + 10)
  wb$add_drawing(dims = instr_dims, xml = shape_xml)
  
  wb$save(file_path, overwrite = TRUE)
  
  invisible(wb)
}

# Perhitungan Indeks PADU-KE ----------------------------------------------

#' Merge numeric compatibility values from lookup table
#'
#' @description
#' Joins compatibility values to sf object using columns 3 and 4 as keys.
#' If an `id_pu` column exists and each group has exactly one non‑missing
#' RTRW and one non‑missing RZWP3K, the function pairs them before lookup.
#'
#' @param sf_obj `sf` object with at least 4 non-geometry columns; columns 3 and 4 used as keys
#' @param lookup_table Data frame with 3 columns: RTRW class, RZWP3K class, numeric value
#' @param default_compat Numeric value for missing combinations (default NA_real_)
#'
#' @return Original `sf` object with added `idx_serasi` column
#'
#' @examples
#' \dontrun{
#' compat_long <- load_validate_matrix_table("matrix.xlsx")
#' result <- merge_attributes_to_map(overlap_sf, compat_long)
#' }
#'
#' @export
merge_attributes_to_map <- function(sf_obj, lookup_table, default_compat = NA_real_) {
  if (!inherits(sf_obj, "sf")) stop("sf_obj must be an sf object.")
  if (ncol(sf_obj) - 1 < 4) stop("sf_obj needs at least 4 non-geometry columns.")
  if (!is.data.frame(lookup_table)) stop("lookup_table must be a data frame.")
  if (ncol(lookup_table) < 3) stop("lookup_table needs at least 3 columns.")
  if (!is.numeric(lookup_table[[3]])) stop("Third column of lookup_table must be numeric.")
  if (!is.numeric(default_compat)) stop("default_compat must be numeric.")
  sf_col_names <- names(sf_obj)
  rtrw_col <- sf_col_names[sf_col_names == "RTRW"]
  rzpw_col <- sf_col_names[sf_col_names == "RZWP3K"]
  
  if ("id_pu" %in% sf_col_names) {
    sf_non_geo <- sf::st_drop_geometry(sf_obj)
    id_vals <- unique(sf_non_geo$id_pu)
    
    # check if every id_pu group has exactly one non‑NA in RTRW and one in RZWP3K
    is_paired <- TRUE
    for (pid in id_vals) {
      sub <- sf_non_geo[sf_non_geo$id_pu == pid, ]
      n_rtrw <- sum(!is.na(sub[[rtrw_col]]))
      n_rzpw <- sum(!is.na(sub[[rzpw_col]]))
      if (n_rtrw != 1 || n_rzpw != 1) {
        is_paired <- FALSE
        break
      }
    }
    
    if (is_paired) {
      # pre‑compute lookup keys and compatibility vector
      lookup_keys <- paste(lookup_table[[1]], lookup_table[[2]], sep = "||")
      compat_vec <- lookup_table[[3]]
      names(compat_vec) <- lookup_keys
      
      # build compatibility value per id_pu
      compat_by_id <- data.frame(id_pu = id_vals, compat = NA_real_)
      for (i in seq_along(id_vals)) {
        pid <- id_vals[i]
        sub <- sf_non_geo[sf_non_geo$id_pu == pid, ]
        rtrw_val <- sub[[rtrw_col]][!is.na(sub[[rtrw_col]])][1]
        rzpw_val <- sub[[rzpw_col]][!is.na(sub[[rzpw_col]])][1]
        key <- paste(rtrw_val, rzpw_val, sep = "||")
        compat <- compat_vec[key]
        if (is.na(compat)) compat <- default_compat
        compat_by_id$compat[i] <- compat
      }
      
      # assign the compatibility to all rows with matching id_pu
      sf_obj$idx_serasi <- compat_by_id$compat[match(sf_obj$id_pu, compat_by_id$id_pu)]
      return(sf_obj)
    }
  }
  
  sf_keys <- sf::st_drop_geometry(sf_obj)[, c(rtrw_col, rzpw_col)]
  sf_keys$key <- paste(sf_keys[[1]], sf_keys[[2]], sep = "||")
  lookup_keys <- paste(lookup_table[[1]], lookup_table[[2]], sep = "||")
  compat_vec <- lookup_table[[3]]
  names(compat_vec) <- lookup_keys
  matched_compat <- compat_vec[sf_keys$key]
  matched_compat[is.na(matched_compat)] <- default_compat
  sf_obj$idx_serasi <- matched_compat
  
  return(sf_obj)
}

#' Generate a square self-combination matrix from a class table
#'
#' @description
#' Creates a square matrix where both rows and columns are labelled with
#' unique class names from the second column of input table. If `file_path`
#' is supplied, writes a styled Excel workbook with instructions and
#' diagonal values set to 3.
#'
#' @param tbl Data frame with at least 2 columns; second column contains class names
#' @param fill_value Value to fill matrix cells (default NA)
#' @param file_path Optional path to write a styled Excel file. If NULL, returns the tibble.
#'
#' @return A tibble (if file_path is NULL), or invisibly returns the workbook object.
#'
#' @examples
#' \dontrun{
#' class_table <- tibble::tribble(
#'   ~id, ~class_name,
#'   1,   "Hutan Lindung",
#'   2,   "Kawasan Permukiman"
#' )
#' mat <- generate_matrix_padu_ke(class_table)
#' generate_matrix_padu_ke(class_table, file_path = "padu_ke.xlsx")
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
#' @importFrom openxlsx2 wb_workbook wb_color wb_add_data wb_add_fill wb_add_font
#'   wb_add_border wb_add_cell_style wb_freeze_pane wb_set_col_widths wb_add_drawing
#' @export
generate_matrix_padu_ke <- function(tbl, fill_value = NA, file_path = NULL) {
  if (!is.data.frame(tbl)) stop("Input 'tbl' must be a data frame.")
  if (ncol(tbl) < 2) stop("Input must have at least two columns.")
  
  class_col <- tbl[[2]]
  if (is.factor(class_col)) class_col <- as.character(class_col)
  classes <- unique(class_col[!is.na(class_col)])
  if (length(classes) == 0) stop("No valid class names found.")
  
  n <- length(classes)
  mat <- matrix(fill_value, nrow = n, ncol = n)
  colnames(mat) <- classes
  rownames(mat) <- classes
  
  # Set diagonal to 3
  diag(mat) <- 3
  
  result <- tibble::as_tibble(mat) |>
    dplyr::mutate(class = rownames(mat), .before = 1)
  
  if (is.null(file_path)) {
    return(result)
  }
  
  # write styled Excel 
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Package 'openxlsx2' is required but not installed.")
  }
  require(openxlsx2)
  
  wb <- wb_workbook()$add_worksheet(sheet = "Matriks PADU-KE")
  wb$add_data(x = result, col_names = TRUE, row_names = FALSE)
  
  nrows <- nrow(result) + 1   # +1 for header
  ncols <- ncol(result)
  
  header_dims    <- paste0("A1:", int2col(ncols), "1")
  first_col_dims <- paste0("A1:A", nrows)
  
  black <- wb_color(hex = "FF000000")
  white <- wb_color(hex = "FFFFFFFF")
  
  # Header row (all columns)
  wb$
    add_fill(dims = header_dims, color = wb_color(hex = "FF1F4E79"))$
    add_font(dims = header_dims, color = white, size = 11, bold = TRUE)$
    add_border(dims = header_dims,
               top_border = "thin", top_color = black,
               bottom_border = "thin", bottom_color = black,
               left_border = "thin", left_color = black,
               right_border = "thin", right_color = black,
               inner_vgrid = "thin", inner_vcolor = black)$
    add_cell_style(dims = header_dims, horizontal = "center", vertical = "center", wrap_text = TRUE)
  
  # First column (including header cell)
  wb$
    add_fill(dims = first_col_dims, color = wb_color(hex = "FF1F4E79"))$
    add_font(dims = first_col_dims, color = white, size = 11, bold = TRUE)$
    add_border(dims = first_col_dims,
               top_border = "thin", top_color = black,
               bottom_border = "thin", bottom_color = black,
               left_border = "thin", left_color = black,
               right_border = "thin", right_color = black,
               inner_hgrid = "thin", inner_hcolor = black)$
    add_cell_style(dims = first_col_dims, horizontal = "center", vertical = "center", wrap_text = TRUE)
  
  # Matrix body (from B2 to bottom-right)
  if (nrows >= 2 && ncols >= 2) {
    
    body_dims <- paste0(int2col(2), "2:", int2col(ncols), nrows)
    
    wb$
      add_fill(dims = body_dims, color = wb_color(hex = "FFF5F5DC"))$
      add_border(
        dims = body_dims,
        top_border = "thin", top_color = black,
        bottom_border = "thin", bottom_color = black,
        left_border = "thin", left_color = black,
        right_border = "thin", right_color = black,
        inner_hgrid = "thin", inner_hcolor = black,
        inner_vgrid = "thin", inner_vcolor = black
      )$
      add_cell_style(
        dims = body_dims,
        horizontal = "center",
        vertical = "center",
        wrap_text = TRUE
      )
    
    dark_gray <- wb_color(hex = "FF595959")
    
    for (i in seq_len(n)) {
      if (i < n) {
        for (j in (i + 1):n) {
          
          # +1 because column A is the class column
          excel_col <- j + 1
          
          # +1 because row 1 is the header
          excel_row <- i + 1
          
          cell <- paste0(int2col(excel_col), excel_row)
          
          wb$
            add_fill(
              dims = cell,
              color = dark_gray
            )$
            add_font(
              dims = cell,
              color = white,
              bold = TRUE
            )
        }
      }
    }
  }
  
  wb$freeze_pane(first_row = TRUE, first_col = TRUE)  
  wb$set_col_widths(cols = 1, width = 30)
  if (ncols >= 2) wb$set_col_widths(cols = 2:ncols, width = 25)
  
  # instruction box
  instr_row <- nrows + 4
  
  instr_lines <- c(
    "Template matriks PADU-KE ini menyatakan tingkat keterpaduan penggunaan lahan dan lautan dalam bentang darat-laut",
    "",
    "Instruksi Pengisian:",
    "1. Matriks hanya boleh diisi dengan nilai numerik 0, 1, 2, dan 3",
    "2. Pengisian nilai disesuaikan dengan hubungan pasangan jenis penutup lahan, dengan deskripsi sebagai berikut:",
    "    3 = Konektivitas alami tinggi",
    "    2 = Bisa berdampingan dengan pengaturan",
    "    1 = Kurang cocok/risiko",
    "    0 = Tidak cocok/terlarang",
    "3. Tidak diperkenankan mengubah header kolom dan baris serta mengisi cell di luar matriks"
  )
  
  shape_xml <- create_multiline_shape(
    instr_lines,
    shape      = "rect",
    name       = "instructions_box",
    fill_color = wb_color(hex = "FFF5F5DC"),
    text_color = black,
    line_color = black,
    text_align = "left"
  )
  
  # Position the box from column B down, spanning enough columns and rows
  # Use max(ncols, 6) to give reasonable width
  instr_dims <- paste0("B", instr_row, ":", int2col(max(ncols, 6)), instr_row + 10)
  wb$add_drawing(dims = instr_dims, xml = shape_xml)
  
  wb$save(file_path, overwrite = TRUE)
  
  invisible(wb)
}

#' Calculate LULC adjacency matrix by administrative unit
#'
#' @description
#' For raster LULC: pairwise edge counts between cells (4-directional rook's case).
#' For vector LULC: pairwise counts of touching polygons (queen's case).
#'
#' @details
#' **Parallel processing**  
#' Set `parallel = TRUE` to run the administrative‑unit loop in parallel.
#' The function temporarily sets a `future::multisession` plan (by default) with the
#' number of workers given by `workers` (default = all available cores). The previous
#' plan is restored on exit. If `parallel = FALSE`, the function uses whatever plan
#' is currently active (sequential if none was set).
#'
#' @param lulc Categorical `SpatRaster` or `sf` polygon object
#' @param admin_vector `sf` object with administrative boundaries
#' @param id_col Column name in `admin_vector` with unique identifiers
#' @param class_col For vector LULC only: column name with LULC class codes
#' @param parallel Logical. If `TRUE`, enable parallel processing.
#' @param workers Number of parallel workers (default = `future::availableCores()`).
#' @param plan_strategy The `future` plan to use: `"multisession"` (all platforms) or `"multicore"` (Unix only, lighter).
#' @param progress Logical. Show a progress bar? Default `TRUE`.
#'
#' @return `data.frame` with columns: `id_pu`, `Class_A`, `Class_B`, `Edge_Count`, `percentage`
#'
#' @examples
#' \dontrun{
#' # Raster LULC
#' lulc_rast <- rast("landcover.tif")
#' admin <- st_read("units.shp")
#' 
#' # Sequential (default)
#' res1 <- calculate_lulc_adjacency(lulc_rast, admin, id_col = "id")
#' 
#' # Parallel with 4 workers
#' res1 <- calculate_lulc_adjacency(lulc_rast, admin, id_col = "id",
#'                                  parallel = TRUE, workers = 4)
#'
#' # Vector LULC
#' lulc_sf <- st_read("lulc_polygons.gpkg")
#' res2 <- calculate_lulc_adjacency(lulc_sf, admin, id_col = "id",
#'                                  class_col = "PL2024_ID", parallel = TRUE)
#' }
#'
#' @importFrom furrr future_map_dfr furrr_options
#' @importFrom future plan availableCores multisession multicore
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
  }
  
  if (sf::st_crs(lulc) != sf::st_crs(admin_sf)) {
    message("Reprojecting admin_vector to match LULC CRS...")
    admin_sf <- sf::st_transform(admin_sf, sf::st_crs(lulc))
  }
  
  admin_ids <- unique(admin_sf[[id_col]])
  
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
      
      if (!all(sf::st_is_valid(lulc_clip))) {
        lulc_clip <- sf::st_make_valid(lulc_clip) |> sf::st_buffer(dist = 0)
      }
      
      safe_extract_polygons <- function(x) {
        if (any(sf::st_geometry_type(x) == "GEOMETRYCOLLECTION")) {
          x <- sf::st_collection_extract(x, "POLYGON", warn = FALSE)
        }
        if (is.null(x) || nrow(x) == 0) return(NULL)
        x <- x[sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON"), ]
        if (nrow(x) == 0) return(NULL)
        x <- x[!sf::st_is_empty(x), ]
        if (nrow(x) == 0) return(NULL)
        x
      }
      
      lulc_clip <- safe_extract_polygons(lulc_clip)
      if (is.null(lulc_clip) || nrow(lulc_clip) == 0) {
        return(list(success = zero_row(uid)))
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

#' Calculate PADU-KE index and return both index table and map-ready data
#'
#' @description
#' Preprocesses LULC adjacency and index matrices, computes the PADU-KE index
#' for each administrative unit, and merges it with a base map.
#'
#' @param matriks_padu_ke Data frame with columns: `class1`, `class2`, `adj_index`.
#'   Contains raw class names/IDs and the adjacency index for each pair.
#' @param lulc_ref List or data frame with two elements: first element contains
#'   numeric class IDs, second element contains corresponding class names.
#'   Used to map class names to IDs.
#' @param lulc_adjacencies Data frame with columns: `id_pu`, `Class_A`, `Class_B`,
#'   `Edge_Count`, `percentage`. Edge counts and percentages per LULC pair.
#' @param idx_serasi_map Data frame containing at least an `id_pu` column.
#'   This is the base map (e.g., filtered overlap area) to which the index will be joined.
#' @param normalize Logical; if `TRUE`, scales the index to a 0–1 range.
#'   Default is `TRUE`.
#'
#' @return A list with two components:
#'   \item{idx_padu_ke}{Data frame with columns `id_pu`, `idx_padu_ke_abs`
#'     and (if `normalize = TRUE`) `idx_padu_ke`.}
#'   \item{idx_padu_ke_map}{Data frame formed by left-joining `idx_serasi_map`
#'     with `idx_padu_ke` on `id_pu`.}
#'
#' @examples
#' \dontrun{
#' result <- calculate_padu_ke(matriks_padu_ke, lulc_ref, lulc_adjacencies, idx_serasi_map)
#' padu_ke_table <- result$idx_padu_ke
#' padu_ke_map    <- result$idx_padu_ke_map
#' }
#'
#' @importFrom dplyr left_join mutate filter group_by summarise
#' @export
calculate_padu_ke <- function(matriks_padu_ke, lulc_ref, lulc_adjacencies,
                              idx_serasi_map, normalize = TRUE) {
  
  # Convert class names to IDs in matriks_padu_ke 
  # lulc_ref is assumed to be a list where element 1 = IDs, element 2 = names
  matriks_padu_ke_id <- matriks_padu_ke %>%
    transmute(
      class_id1 = lulc_ref[[1]][match(.data$class1, lulc_ref[[2]])],
      class_id2 = lulc_ref[[1]][match(.data$class2, lulc_ref[[2]])],
      adj_index = .data$idx_padu_ke
    )
  # names(matriks_padu_ke_id) <- c("class_id1", "class_id2", "adj_index")
  
  lulc_adjacencies <- lulc_adjacencies %>%
    mutate(
      Class_A = as.integer(as.character(Class_A)),
      Class_B = as.integer(as.character(Class_B))
    )
  
  # Compute PADU-KE index 
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
    left_join(matriks_padu_ke_id,
              by = c("Class_A" = "class_id1", "Class_B" = "class_id2"))
  
  unmatched <- sum(is.na(df$adj_index))
  if (unmatched > 0) warning(unmatched, " class pair(s) missing adj_index.")
  
  idx_padu_ke <- df %>%
    filter(!is.na(adj_index)) %>%
    mutate(weighted = percentage * adj_index) %>%
    group_by(id_pu) %>%
    summarise(abs_idx_padu_ke = sum(weighted, na.rm = TRUE), .groups = "drop")
  
  if (normalize) {
    max_val <- max(matriks_padu_ke_id$adj_index, na.rm = TRUE)
    idx_padu_ke <- idx_padu_ke %>%
      mutate(idx_padu_ke = abs_idx_padu_ke / (max_val * 100))
  }
  
  # Merge with base map to create idx_padu_ke_map
  idx_padu_ke_map <- idx_serasi_map %>%
    mutate(id_pu = as.character(id_pu)) %>%
    left_join(idx_padu_ke, by = "id_pu")
  
  list(idx_padu_ke = idx_padu_ke, idx_padu_ke_map = idx_padu_ke_map)
}

# Perhitungan Indeks PADU-HS ----------------------------------------------

#' Calculate Euclidean distance from vector features to a set of planning units
#'
#' This function computes a Euclidean distance raster for a given set of source
#' vector features (`vector_obj`) within a planning unit area (`pu`). The source
#' features are first clipped to the planning unit boundary, then a distance
#' raster is generated at a specified resolution and masked to the planning units.
#'
#' @param vector_obj An `sf` object (points, lines, or polygons) representing the
#'   source features from which distances are calculated.
#' @param pu An `sf` object defining the planning unit area (polygon). The raster
#'   extent and mask are based on this object.
#' @param resolution Numeric. The resolution of the output distance raster
#'   (map units, default = 100). Higher values produce coarser rasters.
#'
#' @return A `SpatRaster` object (from the `terra` package) where each cell value
#'   is the Euclidean distance to the nearest source feature. Cells outside the
#'   planning unit area are `NA`.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates that both inputs are `sf` objects and harmonises their CRS
#'         (reprojecting `vector_obj` to the CRS of `pu` if needed).
#'   \item Clips `vector_obj` by `pu` using `sf::st_intersection()` and removes
#'         empty geometries.
#'   \item Creates a raster template from the bounding box of `pu` at the
#'         specified resolution.
#'   \item Computes Euclidean distance from each raster cell to the nearest
#'         source geometry using `terra::distance()`.
#'   \item Masks the distance raster to the exact outline of `pu`.
#' }
#'
#' @note
#' The function stops with an error if no part of `vector_obj` overlaps `pu`
#' after clipping. Both input objects are repaired with `sf::st_make_valid()`
#' to avoid geometry issues.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(terra)
#'
#' # Example source points
#' pts <- st_as_sf(data.frame(x = c(10, 20), y = c(15, 25)), coords = c("x", "y"))
#' st_crs(pts) <- 4326
#'
#' # Example planning unit (a simple polygon)
#' pu_poly <- st_as_sf(data.frame(x = c(0, 30, 30, 0), y = c(0, 0, 30, 30)),
#'                     coords = c("x", "y"), dim = "XY", crs = 4326) |>
#'            st_bbox() |> st_as_sfc()
#'
#' # Compute distance raster at 1 unit resolution
#' dist_rast <- calculate_euclidean_dist(pts, pu_poly, resolution = 1)
#' plot(dist_rast)
#' }
#'
#' @importFrom sf st_make_valid st_crs st_transform st_intersection st_is_empty st_bbox
#' @importFrom terra rast vect distance mask
#' @export
calculate_euclidean_dist <- function(vector_obj, pu, resolution = 100, clip_to_pu = TRUE) {
  # Input validation
  if (!inherits(vector_obj, "sf")) stop("vector_obj must be an sf object")
  if (!inherits(pu, "sf")) stop("pu must be an sf object")
  
  vector_obj <- sf::st_make_valid(vector_obj)
  pu <- sf::st_make_valid(pu)
  
  # Harmonise CRS
  if (!identical(sf::st_crs(vector_obj), sf::st_crs(pu))) {
    message("Reprojecting vector_obj to CRS of pu")
    vector_obj <- sf::st_transform(vector_obj, sf::st_crs(pu))
  }
  
  # Optionally clip vector_obj to pu extent; fall back to full vector if
  # intersection yields nothing (e.g. estuaries that border but don't overlap).
  if (clip_to_pu) {
    vector_clipped <- tryCatch(
      {
        clipped <- sf::st_intersection(vector_obj, pu)
        clipped <- handle_geom_collection(clipped)
        clipped <- clipped[!sf::st_is_empty(clipped), ]
        clipped
      },
      error = function(e) sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(pu)))
    )
    
    # Fall back to unclipped vector if intersection is empty
    if (nrow(vector_clipped) == 0) {
      message("Intersection with pu yielded no features; using full vector_obj extent for distance calculation.")
      vector_clipped <- vector_obj
    }
  } else {
    vector_clipped <- vector_obj
  }
  
  # Create raster template from pu bounding box
  bb <- sf::st_bbox(pu)
  r_template <- terra::rast(
    xmin = bb["xmin"], xmax = bb["xmax"],
    ymin = bb["ymin"], ymax = bb["ymax"],
    resolution = resolution,
    crs = sf::st_crs(pu)$wkt
  )
  
  # Calculate euclidean distance
  source_vect <- terra::vect(vector_clipped)
  dist_raster <- terra::distance(r_template, source_vect)
  
  # Mask to pu
  pu_vect <- terra::vect(pu)
  dist_raster <- terra::mask(dist_raster, pu_vect)
  
  return(dist_raster)
}

#' Extract raster values to sf polygons using exact extraction
#'
#' Extracts raster values for each polygon in an sf object using exactextractr,
#' which computes area-weighted means for polygons. The function handles CRS
#' mismatches by reprojecting the polygons to the raster's CRS and allows
#' filling missing values with a user-specified constant.
#'
#' @param pu An sf object (typically planning units or polygons) for which to extract raster values.
#' @param rast A SpatRaster object (from the \code{terra} package) or a RasterLayer.
#' @param id_col Character string naming the column in \code{pu} that uniquely identifies each feature.
#'        Currently not used in the function but reserved for future compatibility.
#' @param new_col Optional character string for the name of the new column in the output sf object.
#'        If \code{NULL} (default), the name is generated as \code{"{raster_layer_name}_weighted_mean"}.
#' @param na.rm Logical. Should missing values (NA) be removed before computing the weighted mean?
#'        Passed to \code{exactextractr::exact_extract} (default is \code{TRUE}).
#' @param fill_na Value to use for polygons where extraction results in \code{NA}. Default is \code{0}.
#'
#' @return The input sf object \code{pu} with an additional column (named \code{new_col})
#'         containing the area-weighted mean raster values for each polygon.
#'
#' @details
#' The function first checks if the CRS of \code{pu} matches that of \code{rast}. If not,
#' it reprojects the polygons to the raster's CRS. It then uses
#' \code{exactextractr::exact_extract} with \code{fun = "mean"} to compute the
#' area-weighted mean of raster values for each polygon. This method is more
#' accurate than using \code{terra::extract} because it accounts for partial
#' overlap of raster cells with polygon boundaries.
#'
#' The \code{id_col} parameter is included for API consistency with related
#' functions but is not currently used. Missing values (NAs) in the extracted
#' results are replaced with \code{fill_na}.
#'
#' @importFrom sf st_crs st_transform
#' @importFrom exactextractr exact_extract
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(terra)
#' library(exactextractr)
#'
#' # Create example raster
#' r <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
#' values(r) <- runif(100)
#'
#' # Create example polygon
#' pol <- st_sfc(st_polygon(list(cbind(c(2,5,5,2,2), c(2,2,5,5,2)))))
#' pu <- st_sf(id = 1, geometry = pol)
#'
#' # Extract weighted mean
#' result <- extract_raster_to_sf(pu, r, id_col = "id", new_col = "mean_val")
#' print(result)
#' }
#'
#' @export
extract_raster_to_sf <- function(pu, rast, id_col, new_col = NULL, na.rm = TRUE, fill_na = 0) {
  
  # Input validation
  if (is.null(new_col)) {
    new_col <- paste0(names(rast)[1], "_weighted_mean")
  }
  
  if (sf::st_crs(pu) != sf::st_crs(rast)) {
    message("Reprojecting polygons to match raster CRS...")
    pu <- sf::st_transform(pu, sf::st_crs(rast))
  }
  
  # Extraction using weighted mean
  results <- exactextractr::exact_extract(
    rast, 
    pu, 
    fun = "mean", 
    progress = TRUE
  )
  
  # Assign and Fill NAs
  pu[[new_col]] <- results
  pu[[new_col]][is.na(pu[[new_col]])] <- fill_na
  
  return(pu)
}

#' Calculate PADU-HS Index from Estuarine Distance and TSS
#'
#' @description
#' Computes the PADU-HS index for each spatial unit by combining normalized
#' estuarine Euclidean distance and TSS (Total Suspended Solids) values.
#'
#' @param idx_serasi_map `sf` data frame containing the spatial units
#'   (e.g., administrative polygons) with an identifier column.
#' @param estuari_euc_dist `SpatRaster` or `RasterLayer` of Euclidean distances
#'   to estuarine areas.
#' @param tss_rast `SpatRaster` or `RasterLayer` of TSS values.
#' @param id_col Character. Name of the identifier column in `idx_serasi_map`.
#'   Default is `"id_pu"`.
#' @param max_dist Numeric. Maximum distance (in map units) used to normalise
#'   estuarine distance. Distances beyond this value are clamped to 1.
#'   Default is `5000`.
#'
#' @return A list with two components:
#'   \item{idx_padu_hs_map}{An `sf` object containing the original geometry and
#'     all extracted variables, plus the calculated `idx_padu_hs` column.}
#'   \item{idx_padu_hs}{A tibble (data frame) with columns `id_pu` and `idx_padu_hs`,
#'     without geometry.}
#'
#' @details
#' The function uses `extract_raster_to_sf()` to extract mean raster values per
#' polygon. It then computes two sub‑indices:
#' \itemize{
#'   \item \strong{Estuary proximity}: `1 - min(abs(dist) / max_dist, 1)`, clamped to [0,1].
#'         Distances beyond `max_dist` get a score of 0.
#'   \item \strong{TSS quality}: `(max_tss - tss) / (max_tss - min_tss)`, clamped to [0,1].
#'         If TSS is constant, the score becomes 1 for all non‑missing values.
#' }
#' The final index is the arithmetic mean of the two sub‑indices.
#' Missing values in either component propagate as `NA`.
#'
#' @examples
#' \dontrun{
#' result <- calculate_padu_hs(idx_serasi_map, estuari_euc_dist, tss_rast)
#' hs_map <- result$idx_padu_hs_map
#' hs_tbl <- result$idx_padu_hs
#' }
#'
#' @importFrom dplyr left_join select mutate case_when
#' @importFrom sf st_drop_geometry
#' @importFrom tibble as_tibble
#' @export
calculate_padu_hs <- function(idx_serasi_map,
                              estuari_euc_dist,
                              tss_rast,
                              id_col = "id_pu",
                              max_dist = 5000) {
  
  estuari_dist_extracted <- extract_raster_to_sf(
    idx_serasi_map,
    estuari_euc_dist,
    id_col = id_col,
    new_col = "estuari_dist_mean"
  )
  
  tss_extracted <- extract_raster_to_sf(
    idx_serasi_map,
    tss_rast,
    id_col = id_col,
    new_col = "tss_mean"
  )
  
  tss_to_merge <- tss_extracted %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::all_of(c(id_col, "tss_mean")))
  
  if (is.na(max_dist) || max_dist <= 0) {
    max_dist_val <- max(estuari_dist_extracted$estuari_dist_mean, na.rm = TRUE)
    if (is.infinite(max_dist_val) || max_dist_val == 0) {
      max_dist_val <- 1
      warning("All distances are NA or zero. Estuary sub‑index set to 1 for all non‑NA rows.")
    }
  } else {
    max_dist_val <- max_dist
  }
  
  min_tss <- min(tss_extracted$tss_mean, na.rm = TRUE)
  max_tss <- max(tss_extracted$tss_mean, na.rm = TRUE)
  
  if (is.infinite(min_tss) || is.infinite(max_tss)) {
    min_tss <- 0
    max_tss <- 1
    tss_constant <- FALSE  
    warning("All TSS values are NA. TSS sub‑index will be NA for all rows.")
  } else if (max_tss == min_tss) {
    tss_constant <- TRUE
  } else {
    tss_constant <- FALSE
  }
  
  # Calculate index hs
  idx_padu_hs_map <- estuari_dist_extracted %>%
    dplyr::left_join(tss_to_merge, by = id_col) %>%
    dplyr::mutate(
      filter_estuari = pmax(0, pmin(1,
                                    1 - pmin(abs(.data$estuari_dist_mean) / max_dist_val, 1)
      )),
      
      tss_norm = dplyr::case_when(
        is.na(.data$tss_mean) ~ NA_real_,
        tss_constant ~ 1.0, 
        TRUE ~ pmax(0, pmin(1,
                            (max_tss - .data$tss_mean) / (max_tss - min_tss)
        ))
      ),
      
      idx_padu_hs = (filter_estuari + tss_norm) / 2
    ) %>%
    dplyr::select(-filter_estuari, -tss_norm)
  
  idx_padu_hs_tbl <- tibble::as_tibble(
    idx_padu_hs_map %>% sf::st_drop_geometry()
  )
  
  list(
    idx_padu_hs_map = idx_padu_hs_map,
    idx_padu_hs     = idx_padu_hs_tbl
  )
}

# Perhitungan Indeks PADU-KL ----------------------------------------------

#' Calculate area percentage within planning units
#'
#' This function calculates the area and percentage of overlapping areas
#' with each planning unit (PU) in a spatial dataset. It handles coordinate reference
#' system transformations and efficiently processes only intersecting features.
#'
#' @param pu sf object. Planning unit polygons with an `area_ha` column containing
#'   the area of each unit in hectares.
#' @param overlay_area sf object or list of sf objects. Area polygons to be overlapped with
#'   the planning units. If a list is provided, all sf objects in the list will be 
#'   combined into a single sf object before processing.
#' @param title character string. Prefix for the output column names. 
#'   For example, if title = "protected_area", columns will be named 
#'   "protected_area_ha" and "protected_area_pct". Default is "overlay_area".
#' @param parallel Logical. If `TRUE`, the overlapping calculations for intersecting
#'   planning units are run in parallel. Default `FALSE`.
#' @param workers Number of parallel workers (default = `future::availableCores()`).
#' @param plan_strategy The `future` plan to use: `"multisession"` (all platforms)
#'   or `"multicore"` (Unix only).
#' @param progress Logical. Show a progress bar? Default `TRUE` (only used when
#'   `parallel = TRUE`; the sequential loop prints its own progress message).
#'
#' @return The input `pu` sf object with two additional columns:
#'   \item{<title>_ha}{Area of overlay area within each planning unit (hectares)}
#'   \item{<title>_pct}{Percentage of the planning unit covered by overlay areas}
#'
#' @details
#' The function first checks if `overlay_area` is a list. If so, it combines all
#' sf objects in the list using `rbind` into a single sf object. It then ensures 
#' both spatial objects share the same CRS, transforming `overlay_area` to match 
#' `pu` if necessary. The function then identifies planning units that intersect 
#' with any overlay area and calculates overlaps only for those units, improving 
#' efficiency for large datasets.
#'
#' Area calculations are performed in square meters and converted to hectares
#' (1 hectare = 10,000 m²). Results are rounded to two decimal places (but only
#' in the output columns; raw values may be unrounded).
#'
#' **Parallel processing**  
#' When `parallel = TRUE`, the loop over intersecting planning units is executed
#' in parallel. The `overlay_area` object is sent to each worker – be mindful
#' of memory usage if it is very large. The original sequential message
#' (`cat(...)`) is still printed once before the parallel loop begins.
#'
#' @note
#' The `pu` object must contain an `area_ha` numeric column with pre-calculated
#' areas for each planning unit. This function does not recalculate PU areas.
#'
#' @examples
#' \dontrun{
#' # Load example data
#' pu <- st_read("planning_units.shp")
#' 
#' # Single overlay area
#' protected <- st_read("protected_areas.shp")
#' pu_with_protected <- calculate_overlay_pct(pu, protected, title = "protected_area")
#'
#' # Parallel computation
#' pu_with_protected <- calculate_overlay_pct(pu, protected,
#'                                            title = "protected_area",
#'                                            parallel = TRUE, workers = 4)
#'
#' # Multiple overlay areas as a list
#' forest <- st_read("forest.shp")
#' wetland <- st_read("wetland.shp")
#' grassland <- st_read("grassland.shp")
#' 
#' all_habitats <- list(forest, wetland, grassland)
#' pu_with_habitats <- calculate_overlay_pct(pu, all_habitats, title = "habitat")
#'
#' # View results
#' head(pu_with_protected[, c("protected_area_ha", "protected_area_pct")])
#' head(pu_with_habitats[, c("habitat_ha", "habitat_pct")])
#' }
#'
#' @importFrom sf st_crs st_transform st_intersects st_intersection st_area
#' @importFrom furrr future_map_dbl furrr_options
#' @importFrom future plan availableCores multisession multicore
#' @export
calculate_overlay_pct <- function(pu,
                                  overlay_area,
                                  title = "overlay_area",
                                  parallel = FALSE,
                                  workers = NULL,
                                  plan_strategy = c("multisession", "multicore"),
                                  progress = TRUE) {
  plan_strategy <- match.arg(plan_strategy)
  
  # Check if overlay_area is a list and combine if necessary
  if (is.list(overlay_area) && !inherits(overlay_area, "sf")) {
    cat("Combining", length(overlay_area), "sf objects from list\n")
    overlay_area <- do.call(rbind, overlay_area)
  }
  
  # Transform overlay_area to match pu CRS
  if (st_crs(pu) != st_crs(overlay_area)) {
    overlay_area <- st_transform(overlay_area, st_crs(pu))
  }
  
  # Create column names based on title
  ha_col <- paste0(title, "_ha")
  pct_col <- paste0(title, "_pct")
  
  # Initialize columns
  pu[[ha_col]] <- 0
  pu[[pct_col]] <- 0
  
  # Find which PU intersect with overlay_area
  intersects_idx <- st_intersects(pu, overlay_area)
  intersecting_pu <- which(lengths(intersects_idx) > 0)
  
  cat("Processing", length(intersecting_pu), "planning units that intersect with", title, "areas\n")
  
  if (length(intersecting_pu) == 0) {
    return(pu)
  }
  
  # Sequential
  if (!parallel) {
    for (i in intersecting_pu) {
      intersection <- st_intersection(pu[i, ], overlay_area)
      if (nrow(intersection) > 0) {
        overlap_area_ha <- sum(as.numeric(st_area(intersection))) / 10000
        pu[[ha_col]][i] <- overlap_area_ha
        pu[[pct_col]][i] <- (overlap_area_ha / pu$area_ha[i]) * 100
      }
    }
    return(pu)
  }
  
  # Parallel execution
  old_plan <- future::plan("list")
  on.exit(future::plan(old_plan), add = TRUE)
  if (is.null(workers)) workers <- future::availableCores()
  if (plan_strategy == "multisession") {
    future::plan(future::multisession, workers = workers)
  } else {
    future::plan(future::multicore, workers = workers)
  }
  
  # Compute overlap area for each intersecting PU in parallel
  overlap_areas <- furrr::future_map_dbl(
    .x = intersecting_pu,
    .f = function(i) {
      intersection <- sf::st_intersection(pu[i, ], overlay_area)
      if (nrow(intersection) > 0) {
        sum(as.numeric(sf::st_area(intersection))) / 10000
      } else {
        0
      }
    },
    .progress = progress,
    .options = furrr::furrr_options(packages = "sf")
  )
  
  # Assign results back to pu
  pu[[ha_col]][intersecting_pu] <- overlap_areas
  pu[[pct_col]][intersecting_pu] <- (overlap_areas / pu$area_ha[intersecting_pu]) * 100
  
  return(pu)
}

# Perhitungan Indeks PADU-RTp ---------------------------------------------

#' Handle geometry collections and multisurfaces in an sf object
#'
#' Converts geometry collections and multisurfaces to multipolygons by extracting
#' polygon components. Rows without any polygon data are dropped, and a warning
#' is issued if rows are removed.
#'
#' @param sf_obj An sf object containing simple feature geometries.
#'
#' @return An sf object with all geometries converted to `MULTIPOLYGON` type.
#'   Rows that contained no polygon data after extraction are removed.
#'
#' @details
#' The function first checks if any geometry type in `sf_obj` is either
#' `"GEOMETRYCOLLECTION"` or `"MULTISURFACE"`. If such types are present, it:
#' \enumerate{
#'   \item Applies `st_make_valid()` to repair invalid geometries.
#'   \item Extracts `"POLYGON"` components using `st_collection_extract()`.
#'   \item Casts the result to `"MULTIPOLYGON"` for consistency.
#' }
#' If the number of rows decreases after processing, a warning reports how many
#' rows were dropped (those without any polygon geometry).
#'
#' @examples
#' \dontrun{
#' library(sf)
#' # Create an sf object with a geometry collection
#' gc <- st_sfc(st_geometrycollection(list(st_point(c(0,0)), st_linestring(cbind(0:1,0:1)))))
#' poly <- st_sfc(st_polygon(list(cbind(c(0,1,1,0,0), c(0,0,1,1,0)))))
#' sf_mixed <- st_sf(geom = c(gc, poly), id = 1:2)
#' result <- handle_geom_collection(sf_mixed)
#' }
#' @export
#'
#' @importFrom sf st_geometry_type st_make_valid st_collection_extract st_cast
#' @importFrom magrittr %>%
handle_geom_collection <- function(sf_obj) {
  
  geom_types <- as.character(st_geometry_type(sf_obj))
  # Detect if any row is a collection type
  is_collection <- any(geom_types %in% c("GEOMETRYCOLLECTION", "MULTISURFACE"))
  
  if (is_collection) {
    n_before <- nrow(sf_obj)
    
    sf_obj <- sf_obj %>%
      st_make_valid() %>%
      st_collection_extract("POLYGON") %>%
      st_cast("MULTIPOLYGON")
    
    n_after <- nrow(sf_obj)
    
    if (n_before != n_after) {
      warning(paste("Dropped", n_before - n_after, "rows with no polygon data."))
    }
  }
  return(sf_obj)
}

#' Calculate PADU-RTp Index from Industry and Shipping Lane Distances
#'
#' @description
#' Computes the PADU-RTp index for each spatial unit by combining normalized
#' distances to industrial areas and shipping lanes (pelayaran).
#'
#' @param idx_serasi_map `sf` data frame containing the spatial units
#'   (e.g., overlap area polygons) with an identifier column.
#' @param industry_euc_dist `SpatRaster` or `RasterLayer` of Euclidean distances
#'   to industrial areas.
#' @param pelayaran_euc_dist `SpatRaster` or `RasterLayer` of Euclidean distances
#'   to shipping lanes (pelayaran).
#' @param id_col Character. Name of the identifier column in `idx_serasi_map`.
#'   Default is `"id_pu"`.
#' @param industry_max_dist Numeric. Maximum distance (in map units) for normalising
#'   industry distance. Distances beyond this value are clamped to 1.
#'   Default is `8000`.
#' @param pelayaran_max_dist Numeric. Maximum distance for normalising shipping lane
#'   distance. Default is `5000`.
#'
#' @return A list with two components:
#'   \item{idx_padu_rtp_map}{An `sf` object containing the original geometry and
#'     all extracted variables, plus the calculated `idx_padu_rtp` column.}
#'   \item{idx_padu_rtp}{A tibble (data frame) with columns `id_pu` and `idx_padu_rtp`,
#'     without geometry.}
#'
#' @details
#' The function extracts mean distances from each polygon using `extract_raster_to_sf()`.
#' Missing values are replaced with 0 (assuming no influence). Then:
#' \deqn{filter\_industry = 1 - \min(industry\_dist / industry\_max\_dist, 1)}
#' \deqn{filter\_pelayaran = 1 - \min(pelayaran\_dist / pelayaran\_max\_dist, 1)}
#' \deqn{idx\_padu\_rtp = \max(0, 1 - (filter\_industry + filter\_pelayaran) / 2)}
#'
#' The final index ranges from 0 (lowest pressure) to 1 (highest pressure)
#' based on proximity to both features.
#'
#' @examples
#' \dontrun{
#' result <- calculate_padu_rtp(idx_serasi_map, industry_euc_dist, pelayaran_euc_dist)
#' rtp_map <- result$idx_padu_rtp_map
#' rtp_tbl <- result$idx_padu_rtp
#' }
#'
#' @importFrom dplyr left_join select mutate if_else
#' @importFrom sf st_drop_geometry
#' @importFrom tibble as_tibble
#' @export
calculate_padu_rtp <- function(idx_serasi_map,
                               industry_euc_dist,
                               pelayaran_euc_dist,
                               id_col = "id_pu",
                               industry_max_dist = 8000,
                               pelayaran_max_dist = 5000) {
  
  # Extract industry distance
  industry_dist_extracted <- extract_raster_to_sf(
    idx_serasi_map,
    industry_euc_dist,
    id_col = id_col,
    new_col = "industry_dist_mean"
  )
  
  # Extract shipping lane distance
  pelayaran_dist_extracted <- extract_raster_to_sf(
    idx_serasi_map,
    pelayaran_euc_dist,
    id_col = id_col,
    new_col = "pelayaran_dist_mean"
  )
  
  # Drop geometry from industry for joining
  industry_to_merge <- industry_dist_extracted %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::all_of(c(id_col, "industry_dist_mean")))
  
  # Join and calculate PADU-RTp
  idx_padu_rtp_map <- pelayaran_dist_extracted %>%
    dplyr::left_join(industry_to_merge, by = id_col) %>%
    dplyr::mutate(
      # Replace NAs with 0 (assuming no influence if no data)
      industry_clean = dplyr::if_else(is.na(.data$industry_dist_mean), 0,
                                      pmax(.data$industry_dist_mean, 0)),
      pelayaran_clean = dplyr::if_else(is.na(.data$pelayaran_dist_mean), 0,
                                       pmax(.data$pelayaran_dist_mean, 0)),
      
      # Normalise distances (clamp at 1)
      filter_industry = 1 - pmin(.data$industry_clean / industry_max_dist, 1),
      filter_pelayaran = 1 - pmin(.data$pelayaran_clean / pelayaran_max_dist, 1),
      
      # Final index, ensure non-negative
      idx_padu_rtp = pmax(0, 1 - (.data$filter_industry + .data$filter_pelayaran) / 2)
    ) %>%
    dplyr::select(-dplyr::all_of(c("industry_clean", "pelayaran_clean",
                                   "filter_industry", "filter_pelayaran")))
  
  # Create geometry‑free tibble
  idx_padu_rtp <- tibble::as_tibble(
    idx_padu_rtp_map %>% sf::st_drop_geometry()
  )
  
  # Return as a list
  list(
    idx_padu_rtp_map = idx_padu_rtp_map,
    idx_padu_rtp     = idx_padu_rtp
  )
}

# Perhitungan Indeks PADU-KI ----------------------------------------------

#' Extract area-weighted mean from a spatial layer to planning units
#'
#' Intersects a set of planning units (polygons) with a source spatial layer
#' containing a value column, computes the overlap area for each intersection,
#' and calculates the area-weighted mean of the value within each planning unit.
#' Optionally reprojects the source layer to match the CRS of the planning units.
#'
#' @param pu          An `sf` polygon object representing planning units.
#' @param value_sf    An `sf` object (typically polygons or multi-polygons) that
#'                    contains a numeric attribute to be transferred. The function
#'                    assumes that the geometries have area (polygons).
#' @param value_col   Character string. Name of the column in `value_sf` holding
#'                    the numeric values to be averaged.
#' @param pu_id       Character string. Name of a unique identifier column in `pu`.
#'                    If `NULL` (default), a temporary ID column is created and
#'                    removed before returning.
#' @param new_col     Character string. Name of the new column to be added to `pu`
#'                    containing the weighted mean. Default is `"weighted_mean"`.
#' @param fill_na     Numeric value. Used to fill planning units that have no
#'                    overlap with `value_sf` (default = 0).
#' @param parallel    Logical. If `TRUE`, the computation is split into parallel
#'                    chunks. Default `FALSE`.
#' @param workers     Number of parallel workers (default = `future::availableCores()`).
#' @param plan_strategy The `future` plan to use: `"multisession"` (all platforms)
#'                    or `"multicore"` (Unix only).
#' @param progress    Logical. Show a progress bar? Default `TRUE` (only in
#'                    parallel mode; the sequential path has its own output).
#'
#' @return The input `pu` object with an additional column named `new_col`
#'         containing the area-weighted mean of `value_col` for each planning unit.
#'
#' @details
#' The weighted mean for a planning unit \eqn{i} is computed as:
#' \deqn{\bar{v}_i = \frac{\sum_j v_j \cdot a_{ij}}{\sum_j a_{ij}}}
#' where \eqn{v_j} is the value from `value_sf` polygon \eqn{j}, and \eqn{a_{ij}} is
#' the area of intersection between planning unit \eqn{i} and polygon \eqn{j}.
#'
#' If the CRS of `pu` and `value_sf` differ, `value_sf` is reprojected to the CRS
#' of `pu` (a message is printed). The function uses `sf::st_area()` to compute
#' overlap areas; therefore, the CRS should be a projected (Cartesian) coordinate
#' system to obtain meaningful areas. If no overlap exists between a planning unit
#' and the source layer, the `fill_na` value is assigned.
#'
#' **Parallel processing**  
#' When `parallel = TRUE`, the planning units are split into chunks and processed
#' in parallel. The full `value_sf` is sent to each worker – if `value_sf` is very
#' large, you may need to increase the future globals size limit via
#' `options(future.globals.maxSize = +Inf)`. Alternatively, the function could
#' be extended to pre-filter `value_sf` to only features that intersect the
#' planning units, but that is not implemented yet.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create two overlapping square polygons as planning units
#' pu <- st_sf(id = 1:2,
#'             geometry = st_sfc(
#'               st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
#'               st_polygon(list(rbind(c(0.5,0.5), c(1.5,0.5), c(1.5,1.5),
#'                                     c(0.5,1.5), c(0.5,0.5))))
#'             ))
#'
#' # Source layer: two rectangles with different values
#' vals <- st_sf(value = c(10, 20),
#'               geometry = st_sfc(
#'                 st_polygon(list(rbind(c(0,0), c(0.8,0), c(0.8,0.8),
#'                                       c(0,0.8), c(0,0)))),
#'                 st_polygon(list(rbind(c(0.7,0.7), c(1.7,0.7),
#'                                       c(1.7,1.7), c(0.7,1.7), c(0.7,0.7))))
#'               ))
#'
#' # Sequential extraction
#' pu_result <- extract_sf_to_sf(pu, vals, value_col = "value",
#'                               pu_id = "id", new_col = "wmean")
#'
#' # Parallel extraction
#' pu_result <- extract_sf_to_sf(pu, vals, value_col = "value",
#'                               pu_id = "id", new_col = "wmean",
#'                               parallel = TRUE, workers = 2)
#' }
#'
#' @importFrom sf st_crs st_transform st_intersection st_area
#' @importFrom furrr future_map_dfr furrr_options
#' @importFrom future plan availableCores multisession multicore
#' @export
extract_sf_to_sf <- function(pu,
                             value_sf,
                             value_col,
                             pu_id = NULL,
                             new_col = "weighted_mean",
                             fill_na = 0,
                             parallel = FALSE,
                             workers = NULL,
                             plan_strategy = c("multisession", "multicore"),
                             progress = TRUE) {
  plan_strategy <- match.arg(plan_strategy)
  
  # Handle projection
  if (!sf::st_crs(pu) == sf::st_crs(value_sf)) {
    message("Reprojecting value_sf to match pu CRS...")
    value_sf <- sf::st_transform(value_sf, sf::st_crs(pu))
  }
  
  added_tmp_id <- FALSE
  if (is.null(pu_id)) {
    pu$.tmp_id <- seq_len(nrow(pu))
    pu_id <- ".tmp_id"
    added_tmp_id <- TRUE
  }
  
  # Sequential path
  if (!parallel) {
    inter <- sf::st_intersection(pu[, pu_id, drop = FALSE],
                                 value_sf[, value_col, drop = FALSE])
    if (nrow(inter) == 0) {
      warning("No overlap between pu and value_sf. Returning fill_na for all units.")
      pu[[new_col]] <- fill_na
      if (added_tmp_id) pu$.tmp_id <- NULL
      return(pu)
    }
    inter$area_overlap <- as.numeric(sf::st_area(inter))
    inter$weighted <- inter[[value_col]] * inter$area_overlap
    
    agg <- aggregate(cbind(weighted, area_overlap) ~ inter[[pu_id]],
                     data = inter, FUN = sum)
    names(agg)[1] <- pu_id
    agg$mean <- agg$weighted / agg$area_overlap
    
    pu <- merge(pu, agg[, c(pu_id, "mean")], by = pu_id, all.x = TRUE)
    pu[[new_col]] <- pu$mean
    pu$mean <- NULL
    pu[[new_col]][is.na(pu[[new_col]])] <- fill_na
    if (added_tmp_id) pu$.tmp_id <- NULL
    return(pu)
  }
  
  # Parallel execution 
  old_plan <- future::plan("list")
  on.exit(future::plan(old_plan), add = TRUE)
  if (is.null(workers)) workers <- future::availableCores()
  if (plan_strategy == "multisession") {
    future::plan(future::multisession, workers = workers)
  } else {
    future::plan(future::multicore, workers = workers)
  }
  
  # Split pu into chunks (list of sf objects)
  n <- nrow(pu)
  idx_chunks <- split(seq_len(n), cut(seq_len(n), breaks = workers, labels = FALSE))
  pu_chunks <- lapply(idx_chunks, function(idx) pu[idx, ])
  
  # Worker function: process one chunk
  process_chunk <- function(chunk, value_sf, value_col, pu_id) {
    inter <- sf::st_intersection(
      chunk[, pu_id, drop = FALSE],
      value_sf[, value_col, drop = FALSE]
    )
    if (nrow(inter) == 0) {
      return(data.frame(
        tmp = character(0),
        weighted = numeric(0),
        area_overlap = numeric(0),
        mean = numeric(0),
        stringsAsFactors = FALSE
      ))
    }
    inter$area_overlap <- as.numeric(sf::st_area(inter))
    inter$weighted <- inter[[value_col]] * inter$area_overlap
    
    agg <- aggregate(cbind(weighted, area_overlap) ~ inter[[pu_id]],
                     data = inter, FUN = sum)
    names(agg)[1] <- pu_id
    agg$mean <- agg$weighted / agg$area_overlap
    # Return only the essential columns for merging
    agg[, c(pu_id, "mean"), drop = FALSE]
  }
  
  # Run in parallel
  agg_list <- furrr::future_map(
    .x = pu_chunks,
    .f = process_chunk,
    value_sf = value_sf,
    value_col = value_col,
    pu_id = pu_id,
    .progress = progress,
    .options = furrr::furrr_options(packages = "sf", seed = TRUE)
  )
  
  all_agg <- do.call(rbind, agg_list)
  
  # Merge back to pu
  if (nrow(all_agg) == 0) {
    warning("No overlap between pu and value_sf. Returning fill_na for all units.")
    pu[[new_col]] <- fill_na
  } else {
    pu <- merge(pu, all_agg, by = pu_id, all.x = TRUE)
    pu[[new_col]] <- pu$mean
    pu$mean <- NULL
    pu[[new_col]][is.na(pu[[new_col]])] <- fill_na
  }
  
  if (added_tmp_id) pu$.tmp_id <- NULL
  return(pu)
}

#' Calculate PADU-KI Index from Disaster Risk Values
#'
#' @description
#' Computes the PADU-KI index for each spatial unit by extracting mean disaster
#' risk values from a vector layer and transforming them to a 0–1 scale where
#' 1 represents lowest risk and 0 highest risk.
#'
#' @param idx_serasi_map `sf` data frame containing the spatial units
#'   (e.g., overlap area polygons) with an identifier column.
#' @param disaster_risk_vect `sf` object containing disaster risk polygons
#'   with a risk value column.
#' @param value_col Character. Name of the column in `disaster_risk_vect`
#'   containing the risk values (e.g., "Kerawanan").
#' @param pu_id Character. Name of the identifier column in `idx_serasi_map`.
#'   Default is `"id_pu"`.
#' @param new_col Character. Name of the column to store extracted mean risk
#'   values. Default is `"disaster_risk_mean"`.
#'
#' @return A list with two components:
#'   \item{idx_padu_ki_map}{An `sf` object containing the original geometry
#'     and the calculated `idx_padu_ki` column.}
#'   \item{idx_padu_ki}{A tibble (data frame) with columns `id_pu` and `idx_padu_ki`,
#'     without geometry.}
#'
#' @details
#' The function uses `extract_sf_to_sf()` to compute the mean of `value_col`
#' from `disaster_risk_vect` overlapping each polygon in `idx_serasi_map`.
#' The PADU-KI index is then defined as:
#' \deqn{idx\_padu\_ki = 1 - disaster\_risk\_mean}
#' with the assumption that disaster_risk_mean is on a 0–1 scale
#' (0 = low risk, 1 = high risk). Missing values are propagated as `NA`.
#'
#' @examples
#' \dontrun{
#' result <- calculate_padu_ki(idx_serasi_map, disaster_risk_vect,
#'                             value_col = "Kerawanan")
#' ki_map <- result$idx_padu_ki_map
#' ki_tbl <- result$idx_padu_ki
#' }
#'
#' @importFrom dplyr mutate select if_else
#' @importFrom sf st_drop_geometry
#' @importFrom tibble as_tibble
#' @export
calculate_padu_ki <- function(idx_serasi_map,
                              disaster_risk_vect,
                              value_col,
                              pu_id = "id_pu",
                              new_col = "disaster_risk_mean",
                              parallel = FALSE,
                              workers = NA) {
  
  # Extract disaster risk values
  disaster_risk_extracted <- extract_sf_to_sf(
    pu = idx_serasi_map,
    value_sf = disaster_risk_vect,
    value_col = value_col,
    new_col = new_col,
    pu_id = pu_id,
    parallel = parallel,
    workers = workers
  )
  
  # Get min and max of extracted values
  min_val <- min(disaster_risk_extracted[[new_col]], na.rm = TRUE)
  max_val <- max(disaster_risk_extracted[[new_col]], na.rm = TRUE)
  
  # Compute normalized risk (0–1)
  normalized <- (disaster_risk_extracted[[new_col]] - min_val) / (max_val - min_val)
  if (max_val == min_val) {
    normalized <- 0
  }
  
  # Build final index
  idx_padu_ki_map <- disaster_risk_extracted %>%
    dplyr::mutate(
      idx_padu_ki = dplyr::if_else(
        is.na(.data[[new_col]]),
        NA_real_,
        1 - normalized
      )
    ) %>%
    dplyr::select(-dplyr::all_of(new_col))
  
  idx_padu_ki <- tibble::as_tibble(
    idx_padu_ki_map %>% sf::st_drop_geometry()
  )
  
  list(
    idx_padu_ki_map = idx_padu_ki_map,
    idx_padu_ki     = idx_padu_ki
  )
}

# Perhitungan Indeks PADU Final -------------------------------------------

#' Calculate composite PADU index from multiple indicators
#'
#' @description
#' Joins multiple PADU index vectors into a single spatial object and computes a composite index.
#' Each input vector must contain an `id_pu` column and one `idx_padu_*` column.
#' The function dynamically detects available indices and applies:
#' \itemize{
#'   \item Weighted sum if all expected indices (7) are available
#'   \item Simple average if fewer than 7 indices are available
#' }
#' Weights are matched to index suffixes (e.g., `idx_padu_ke` → `"ke"`) using a lookup table.
#'
#' **Important:** If a column named `idx_padu_ke_abs` is present, it is silently ignored
#' (not used in the composite calculation and no weight is required for it).
#'
#' @param padu_list A list of `sf` objects containing individual PADU indices.
#'   Each object must include `id_pu` and one column matching pattern `idx_padu_*`.
#' @param idx_padu_map An `sf` object serving as the base spatial layer (e.g., planning units),
#'   containing at least the `id_pu` column.
#' @param padu_idx_weight A `data.frame` or tibble with at least two columns:
#'   the first column representing index codes (e.g., "KE", "HS") and the second column
#'   representing corresponding weights.
#'
#' @return An `sf` object with all joined `idx_padu_*` columns (except `idx_padu_ke_abs`)
#'   and an additional column:
#' \describe{
#'   \item{idx_padu_final}{Composite PADU index calculated per feature}
#' }
#'
#' @examples
#' \dontrun{
#' # Prepare list of PADU index layers
#' padu_list <- list(
#'   idx_padu_ke,
#'   idx_padu_hs,
#'   idx_padu_kl
#' )
#'
#' # Base spatial layer
#' idx_padu_map <- idx_serasi_map
#'
#' # Weight table
#' padu_idx_weight <- tibble::tibble(
#'   Kode = c("KE", "HS", "KL"),
#'   Bobot = c(0.2, 0.2, 0.15)
#' )
#'
#' # Calculate composite index
#' result <- calculate_padu_index(
#'   padu_list = padu_list,
#'   idx_padu_map = idx_padu_map,
#'   padu_idx_weight = padu_idx_weight
#' )
#' }
#'
#' @export
calculate_padu_index <- function(padu_list, idx_padu_map, padu_idx_weight) {
  idx_padu_map <- purrr::reduce(
    padu_list,
    .init = idx_padu_map,
    .f = function(x, y) {
      
      idx_col <- names(y)[grepl("^idx_padu_", names(y))][1]
      
      y_clean <- y %>%
        st_drop_geometry() %>%
        mutate(id_pu = as.integer(id_pu)) %>%
        select(id_pu, all_of(idx_col))
      
      left_join(x, y_clean, by = "id_pu")
    }
  )
  
  # Replace NA to 0
  idx_padu_map <- idx_padu_map %>%
    mutate(across(matches("^idx_padu_"), ~replace_na(., 0)))
  
  # Prepare weights
  weights <- padu_idx_weight %>%
    mutate(
      code  = tolower(.[[1]]),
      value = .[[2]]
    )
  
  idx_cols <- names(idx_padu_map)[grepl("^idx_padu_", names(idx_padu_map))]
  if ("idx_padu_ke_abs" %in% idx_cols) {
    message("Removing 'idx_padu_ke_abs' from composite calculation (ignored).")
    idx_cols <- setdiff(idx_cols, "idx_padu_ke_abs")
  }
  
  idx_code <- stringr::str_remove(idx_cols, "idx_padu_")
  
  # Validate weights for the remaining indices
  missing_weights <- idx_code[!idx_code %in% weights$code]
  if (length(missing_weights) > 0) {
    stop(paste("Missing weights for indices:", paste(missing_weights, collapse = ", ")))
  }
  
  n_idx <- length(idx_cols)
  
  if (n_idx < 7) {
    message(paste0("Only ", n_idx, " indices detected → using mean"))
  } else {
    message("All indices detected → using weighted sum")
  }
  
  idx_padu_map <- idx_padu_map %>%
    rowwise() %>%
    mutate(
      idx_padu_final = if (n_idx < 7) {
        mean(c_across(all_of(idx_cols)))
      } else {
        sum(
          c_across(all_of(idx_cols)) *
            weights$value[match(idx_code, weights$code)]
        )
      }
    ) %>%
    ungroup()
  
  return(idx_padu_map)
}

# Perhitungan Ekonomi ------------------------------------------------

#' Calculate land cover proportions within planning units
#'
#' This function either intersects land use/land cover (LULC) polygons with planning unit (PU) polygons
#' (original method) or uses a predefined allocation matrix (new method) to compute, for each planning unit,
#' the proportion of area covered by each LULC class.
#'
#' @param pu An `sf` object representing planning units. Must contain a unique identifier column
#'   and, when using the matrix method, a column specifying the planning unit type.
#' @param lulc Optional. An `sf` object representing land use/land cover classes.
#'   Required only when `calculate_from_matrix = FALSE`. Ignored otherwise.
#' @param id_pu Character string specifying the name of the column in `pu` that contains
#'   unique identifiers. Default `"id_pu"`.
#' @param lulc_class Character string specifying the name of the column in `lulc`
#'   that contains land cover class labels. Default `"class"`. Not used in matrix mode.
#' @param use_parallel Logical. If `TRUE` and the `furrr` package is available,
#'   the intersection and area calculation are performed in parallel. Default `FALSE`.
#'   (Only relevant in original LULC mode.)
#' @param workers Integer. Number of parallel workers. Default `4`.
#' @param calculate_from_matrix Logical. If `TRUE`, use the predefined allocation matrix
#'   instead of the actual LULC map. Default `FALSE`.
#' @param matrix_tbl Data frame. Required when `calculate_from_matrix = TRUE`.
#'   A data frame in **long format** with exactly three columns, in this order:
#'   \enumerate{
#'     \item Planning unit type (character or factor) – must match values in `selected_zone` column of `pu`.
#'     \item Land cover class (character or factor) – class names.
#'     \item Proportion (numeric) – values between 0 and 1. For each type, proportions must sum to 1.
#'   }
#' @param selected_zone Character string. Required when `calculate_from_matrix = TRUE`.
#'   Name of the column in `pu` that contains the planning unit type (matching the first column of `matrix_tbl`).
#'
#' @return A data frame with three columns:
#'   * The planning unit identifier column (name given by `id_pu`)
#'   * The land cover class column (name given by `lulc_class` – in matrix mode, taken from second column of `matrix_tbl`)
#'   * `proportion`: the proportion of the planning unit's total area covered by that land cover class.
#'
#' @details
#' When `calculate_from_matrix = FALSE`, the function behaves exactly as the original:
#'   - Intersects LULC polygons with planning units,
#'   - Computes areas in square meters,
#'   - Returns proportions per PU and class.
#'
#' When `calculate_from_matrix = TRUE`:
#'   - No spatial operations are performed.
#'   - For each planning unit, the value in `selected_zone` is used to look up the corresponding rows in
#'     `matrix_tbl` (first column = type, second = class, third = proportion).
#'   - The output is in long format with proportions taken directly from the matrix.
#'   - If a planning unit's `selected_zone` value does not exist in the first column of `matrix_tbl`, it is silently skipped.
#'   - Proportions for each type are checked to sum to 1 (tolerance 1e-6).
#'
#' @examples
#' \dontrun{
#' # Original mode (using actual LULC map)
#' result <- calculate_land_distribution(pu, lulc, id_pu = "id_pu")
#'
#' # Matrix mode
#' mat <- read.csv("allocation_matrix.csv")  # must have 3 columns: type, class, proportion
#' result <- calculate_land_distribution(pu,
#'   calculate_from_matrix = TRUE,
#'   matrix_tbl = mat,
#'   selected_zone = "alt_RTRW"
#' )
#' }
#'
#' @export
calculate_land_distribution <- function(pu, lulc = NULL,
                                        id_pu = "id_pu",
                                        lulc_class = "class",
                                        use_parallel = FALSE,
                                        workers = 4,
                                        calculate_from_matrix = FALSE,
                                        matrix_tbl = NULL,
                                        selected_zone = NULL) {
  
  # Input validation
  if (!inherits(pu, "sf")) stop("pu must be an sf object")
  
  if (calculate_from_matrix) {
    # Matrix mode
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
    
    # Use first three columns as (type, class, proportion)
    long_mat <- matrix_tbl[, 1:3]
    colnames(long_mat) <- c("type", "class", "proportion")
    long_mat$proportion <- as.numeric(long_mat$proportion)
    
    # Check that proportions sum to 1 per type
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
        dplyr::group_by(!!sym(id_pu), !!sym(lulc_class)) %>%
        dplyr::summarise(class_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop")
    }, .progress = TRUE)
    all_areas <- dplyr::bind_rows(result_list)
    
    if (nrow(all_areas) == 0) stop("No intersections found")
    total_area <- all_areas %>%
      dplyr::group_by(!!sym(id_pu)) %>%
      dplyr::summarise(total_m2 = sum(class_m2), .groups = "drop")
    result <- all_areas %>%
      dplyr::left_join(total_area, by = id_pu) %>%
      dplyr::mutate(proportion = class_m2 / total_m2) %>%
      dplyr::select(!!sym(id_pu), !!sym(lulc_class), proportion)
    
    # Reset parallel backend to sequential processing
    future::plan(future::sequential)
    
    return(as.data.frame(result))
    
  } else {
    # Sequential (non-parallel) mode
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

#' Calculate Net Present Value per hectare per unit
#'
#' This function computes the area-weighted Net Present Value (NPV) per hectare
#' for each spatial unit (e.g., raster cell or polygon) based on land use/land
#' cover (LULC) proportions and corresponding NPV values. It validates inputs,
#' identifies the proportion columns from a land distribution table, and
#' replaces them with a single `npv_ha` column.
#'
#' @param land_distribution A data frame containing land use/land cover
#'   proportion columns. It must include a column named `alt_RZWP3K` which marks
#'   the start of the proportion columns. All columns after that position are
#'   assumed to represent LULC class proportions (typically summing to 1).
#' @param npv_lulc A data frame with at least three columns:
#'   \itemize{
#'     \item Column 1: (optional ID, not used directly)
#'     \item Column 2: LULC class names (character or factor)
#'     \item Column 3: NPV per hectare values (numeric)
#'   }
#'   This table provides the lookup of NPV for each LULC class.
#'
#' @return A data frame derived from `land_distribution` with the following
#'   changes:
#'   \itemize{
#'     \item A new numeric column `npv_ha` is added, calculated as the sum over
#'       proportion columns multiplied by the corresponding NPV from `npv_lulc`.
#'       Missing proportions (`NA`) are treated as zero.
#'     \item All proportion columns (those after `alt_RZWP3K`) are removed.
#'   }
#'   The remaining columns (including `alt_RZWP3K`) are kept unchanged.
#'
#' @details The function performs the following steps:
#'   1. Checks that `npv_lulc` has at least three columns and creates a named
#'      lookup vector from the second (class) and third (NPV) columns.
#'   2. Locates the column `alt_RZWP3K` in `land_distribution`; fails if
#'      missing.
#'   3. Treats all columns after `alt_RZWP3K` as proportion columns.
#'   4. Verifies that every proportion column name exists as a class name in the
#'      NPV lookup; stops with an error if any class is missing.
#'   5. For each row, computes the weighted sum of NPV using the proportions
#'      (with `NA` replaced by 0) and the lookup values.
#'   6. Returns the input data frame with the proportion columns removed and the
#'      new `npv_ha` column added.
#'
#' @examples
#' \dontrun{
#' # Example land distribution data
#' land_data <- data.frame(
#'   cell_id = 1:3,
#'   alt_RZWP3K = c(0.2, 0.5, 0.3),
#'   forest = c(0.1, 0.4, NA),
#'   agriculture = c(0.9, 0.6, 1.0),
#'   urban = c(0.0, 0.0, 0.0)
#' )
#'
#' # Example NPV lookup table
#' npv_table <- data.frame(
#'   id = 1:3,
#'   lulc_class = c("forest", "agriculture", "urban"),
#'   npv_ha = c(1000, 500, 2000)
#' )
#'
#' # Calculate NPV per hectare
#' result <- calculate_npv_ha_per_unit(land_data, npv_table)
#' print(result)
#' # Should contain cell_id, alt_RZWP3K, and npv_ha only
#' }
#'
#' @importFrom dplyr mutate select all_of
#' @importFrom purrr pmap_dbl
#' @export
calculate_npv_ha_per_unit <- function(land_distribution, npv_lulc) {
  # Validate npv_lulc
  if (ncol(npv_lulc) < 3) {
    stop("npv_lulc must have at least 3 columns (ID, LC, npv_ha)")
  }
  lc_vector <- npv_lulc[[2]]
  npv_vector <- npv_lulc[[3]]
  npv_lookup <- setNames(npv_vector, lc_vector)
  
  # Identify proportion columns
  npv_idx <- grep("^npv_ha_actual", names(land_distribution))
  
  if (length(npv_idx) > 0) {
    start_idx <- max(npv_idx)
  } else {
    start_idx <- match("alt_RZWP3K", names(land_distribution))
  }
  
  prop_cols <- names(land_distribution)[(start_idx + 1):ncol(land_distribution)]
  
  # Compute npv_ha and drop the LULC proportion columns
  result <- land_distribution %>%
    mutate(
      npv_ha = pmap_dbl(
        select(., all_of(prop_cols)),
        function(...) {
          props <- c(...)
          props[is.na(props)] <- 0
          sum(props * npv_lookup[prop_cols])
        }
      )
    ) %>%
    select(-all_of(prop_cols))
  
  return(result)
}

# Perhitungan Rekomendasi -------------------------------------------------

# calculate_alternative_zone

#' Determine alternative zones using compatibility matrix
#'
#' @description
#' Given a pair zone (e.g., RZWP3K or RTRW) and a current zone value, this function
#' returns all candidate zones from the opposite classification system that have a
#' higher SERASI index than the current zone. If none are higher, it returns all
#' candidates with the same index (excluding the current zone itself). Ties are
#' included in full.
#'
#' @param pair_zone Character: the zone value used as the filter criterion.
#'   For `return_type = "RTRW"`, this should be an RZWP3K value.
#'   For `return_type = "RZWP3K"`, this should be an RTRW value.
#' @param current_zone Character: the current value of the zone type we are
#'   trying to replace. Used to determine its SERASI index from the matrix.
#' @param return_type Character: either `"RTRW"` or `"RZWP3K"`.
#'   - `"RTRW"`: filter by `class2` (RZWP3K), return a `class1` (RTRW) zone.
#'   - `"RZWP3K"`: filter by `class1` (RTRW), return a `class2` (RZWP3K) zone.
#' @param df A data frame (or tibble) containing the compatibility matrix with
#'   exactly three columns:
#'   \enumerate{
#'     \item `class1` – RTRW zone names
#'     \item `class2` – RZWP3K zone names
#'     \item `idx_serasi` – numeric compatibility scores
#'   }
#' @param n_alt Integer or `NULL`. Maximum number of alternatives to return.
#'   If `NULL` (default), all qualifying alternatives are returned.
#'   If a positive integer, returns at most that many (after sorting by
#'   descending score and then alphabetically).
#'
#' @return A character vector of alternative zone names (length 0 if none).
#'
#' @examples
#' \dontrun{
#' # Example compatibility matrix
#' mat <- tibble::tribble(
#'   ~class1,                          ~class2,                  ~idx_serasi,
#'   "Kawasan Lindung",                "Suaka",                  1.0,
#'   "Kawasan Perikanan",              "Suaka",                  0.5,
#'   "Kawasan Lindung",                "Taman",                  0.8,
#'   "Kawasan Perikanan",              "Taman",                  1.0,
#'   "Kawasan Budidaya",               "Suaka",                  1.0
#' )
#'
#' # Return at most 2 alternatives for RZWP3K = "Suaka", current RTRW = "Kawasan Lindung"
#' get_alternative_zone("Suaka", "Kawasan Lindung", "RTRW", mat, n_alt = 2)
#' # Returns "Kawasan Budidaya" (score 1.0) – only one qualifies, so returns one.
#' }
#'
#' @export
get_alternative_zone <- function(pair_zone, current_zone, return_type, df, n_alt = NULL) {
  
  # Validate n_alt if provided
  if (!is.null(n_alt)) {
    if (!is.numeric(n_alt) || length(n_alt) != 1 || n_alt < 1) {
      stop("n_alt must be NULL or a positive integer")
    }
    n_alt <- as.integer(n_alt)
  }
  
  # Early exit if pair_zone is missing
  if (is.na(pair_zone) || pair_zone == "") return(character(0))
  
  # Determine filter column and return column based on return_type
  if (return_type == "RTRW") {
    filter_col <- 2   # class2 (RZWP3K)
    return_col <- 1   # class1 (RTRW)
  } else if (return_type == "RZWP3K") {
    filter_col <- 1   # class1 (RTRW)
    return_col <- 2   # class2 (RZWP3K)
  } else {
    stop("return_type must be 'RTRW' or 'RZWP3K'")
  }
  
  # Filter rows where the filter column equals pair_zone
  filtered <- df[df[[filter_col]] == pair_zone, ]
  if (nrow(filtered) == 0) return(character(0))
  
  # Extract candidates and scores
  candidates <- filtered[[return_col]]
  scores <- filtered[[3]]  # idx_serasi
  
  # Find the score of the current zone
  current_idx <- which(candidates == current_zone)
  if (length(current_idx) == 0) {
    # If current_zone is not in the matrix for this pair, we cannot compare.
    # Return all candidates as a fallback (with warning).
    warning("current_zone not found in the filtered data for pair_zone = ", pair_zone,
            ". Returning all candidates.")
    selected <- candidates
  } else {
    current_score <- scores[current_idx[1]]  
    
    # Find candidates with a higher score than current
    higher_mask <- scores > current_score
    if (any(higher_mask)) {
      selected <- candidates[higher_mask]
    } else {
      # If none higher, select candidates with equal score but not the current zone itself
      equal_mask <- scores == current_score & candidates != current_zone
      selected <- candidates[equal_mask]
    }
  }
  
  # Sort selected by descending score and then alphabetically for ties
  if (length(selected) > 0) {
    # Get scores for selected
    sel_scores <- scores[candidates %in% selected]
    ord <- order(-sel_scores, selected)
    selected <- selected[ord]
    
    # Apply limit if n_alt is provided
    if (!is.null(n_alt)) {
      selected <- head(selected, n_alt)
    }
  }
  
  return(selected)
}

# determine serasi index for the alternative zones

#' Look up idx_serasi value from a compatibility matrix, trying both class orders
#'
#' @description
#' Searches for a matching pair `(class_a, class_b)` in the reference matrix
#' `serasi_df`. If no exact match is found, it tries the reversed order
#' `(class_b, class_a)`. Returns `NA` if neither order yields a match or if
#' any input is `NA`.
#'
#' @param class_a Character string: first class name
#' @param class_b Character string: second class name
#' @param serasi_df Data frame with columns `class1`, `class2`, `idx_serasi`
#'                  (the compatibility matrix)
#'
#' @return A numeric value (the `idx_serasi`) or `NA_real_` if not found.
#'
#' @examples
#' \dontrun{
#' # Sample matrix
#' mat <- data.frame(
#'   class1 = c("A", "B"),
#'   class2 = c("B", "C"),
#'   idx_serasi = c(0.5, 1)
#' )
#'
#' get_alternative_serasi("A", "B", mat)  # returns 0.5 (original order)
#' get_alternative_serasi("B", "A", mat)  # returns 0.5 (reversed order)
#' get_alternative_serasi("X", "Y", mat)  # returns NA
#' }
#'
#' @export
get_alternative_serasi <- function(class_a, class_b, serasi_df) {
  if (is.na(class_a) || is.na(class_b)) return(NA_real_)
  
  # Try original order
  match_row <- serasi_df[serasi_df$class1 == class_a & serasi_df$class2 == class_b, ]
  if (nrow(match_row) == 1) return(match_row$idx_serasi)
  
  # Try reversed order
  match_row <- serasi_df[serasi_df$class1 == class_b & serasi_df$class2 == class_a, ]
  if (nrow(match_row) == 1) return(match_row$idx_serasi)
  
  return(NA_real_)
}

#' Determine alternative zones and create an Excel workbook with dropdowns
#'
#' @param idx_padan_map_filter sf object. For step = "step2", must contain
#'   columns: id, id_pu, RTRW, RZWP3K, admin, area_ha, length, idx_serasi. For
#'   step = "step1", must contain columns: id_pu, id_rtrw, id_rzwp3k, RTRW,
#'   RZWP3K, area_ha, idx_serasi. The row order matters for step2's
#'   lead/lag operations.
#' @param serasi_matrix data.frame or tibble. Compatibility matrix used by
#'   `get_alternative_zone()`.
#' @param step character. Either "step2" (default, uses lead/lag on
#'   neighboring rows' RTRW/RZWP3K to derive alternatives) or "step1" (uses
#'   each row's own RTRW/RZWP3K directly, no lead/lag).
#' @param n_alt integer. Number of alternatives to consider (passed to
#'   `get_alternative_zone`). Default = 5.
#' @param output_dir character. Directory where the output Excel file will be
#'   saved. The file will be named "adjacent_alternative_zones_selections.xlsx"
#'   for step2, or "overlaps_alternative_zones_selections.xlsx" for step1.
#'   Default = "." (current working directory).
#'
#' @return A list with two components:
#'   \item{workbook}{The openxlsx workbook object (for further customization).}
#'   \item{data}{The final cleaned data frame (without the temporary alternative
#'      columns).}
#'
determine_alternative_zones <- function(idx_padan_map_filter,
                                        serasi_matrix,
                                        step = c("step2", "step1"),
                                        n_alt = 5,
                                        output_dir = ".") {
  
  step <- match.arg(step)
  
  # Package requirements
  if (!require(openxlsx)) stop("Package 'openxlsx' is required but not installed.")
  if (!require(dplyr))    stop("Package 'dplyr' is required but not installed.")
  if (!require(tidyr))    stop("Package 'tidyr' is required but not installed.")
  if (!require(sf))       stop("Package 'sf' is required but not installed.")
  
  # Input validation
  if (!inherits(idx_padan_map_filter, "sf")) {
    stop("'idx_padan_map_filter' must be an sf object.")
  }
  if (nrow(idx_padan_map_filter) == 0) {
    warning("Input idx_padan_map_filter has 0 rows. Returning empty workbook.")
    wb <- createWorkbook()
    addWorksheet(wb, "Data")
    addWorksheet(wb, "Validation_Lists")
    return(list(workbook = wb, data = data.frame()))
  }
  
  # Required columns and base export columns differ by step
  if (step == "step2") {
    required_cols <- c("id", "id_pu", "RTRW", "RZWP3K", "admin", "area_ha", "length", "idx_serasi")
    base_cols     <- c("id", "id_pu", "RTRW", "RZWP3K", "admin", "area_ha", "length", "idx_serasi")
  } else {
    required_cols <- c("id_pu", "id_rtrw", "id_rzwp3k", "RTRW", "RZWP3K", "admin", "area_ha", "idx_serasi")
    base_cols     <- c("id_pu", "id_rtrw", "id_rzwp3k", "RTRW", "RZWP3K", "admin", "area_ha", "idx_serasi")
  }
  
  missing <- setdiff(required_cols, names(idx_padan_map_filter))
  if (length(missing) > 0) {
    stop("Input data missing required columns: ", paste(missing, collapse = ", "))
  }
  
  if (!is.data.frame(serasi_matrix)) {
    stop("'serasi_matrix' must be a data.frame or tibble.")
  }
  matriks_serasi <- serasi_matrix
  
  if (step == "step2") {
    
    idx_padan_map_alt <- idx_padan_map_filter %>%
      mutate(
        RZWP3K_plus1 = lead(RZWP3K),
        RTRW_minus1  = lag(RTRW)
      ) %>%
      rowwise() %>%
      mutate(
        alt_RTRW_list = list(
          get_alternative_zone(RZWP3K_plus1, RTRW, "RTRW", matriks_serasi, n_alt = n_alt)
        ),
        alt_RZWP3K_list = list(
          get_alternative_zone(RTRW_minus1, RZWP3K, "RZWP3K", matriks_serasi, n_alt = n_alt)
        )
      ) %>%
      ungroup() %>%
      unnest_wider(alt_RTRW_list, names_sep = "_", names_repair = "unique") %>%
      unnest_wider(alt_RZWP3K_list, names_sep = "_", names_repair = "unique") %>%
      select(-RZWP3K_plus1, -RTRW_minus1) %>%
      rename_with(~ gsub("alt_RTRW_list_", "alt_RTRW_", .x), starts_with("alt_RTRW_list_")) %>%
      rename_with(~ gsub("alt_RZWP3K_list_", "alt_RZWP3K_", .x), starts_with("alt_RZWP3K_list_"))
    
  } else { # step1
    
    idx_padan_map_alt <- idx_padan_map_filter %>%
      rowwise() %>%
      mutate(
        alt_RTRW_list = list(
          get_alternative_zone(RZWP3K, RTRW, "RTRW", matriks_serasi, n_alt = n_alt)
        ),
        alt_RZWP3K_list = list(
          get_alternative_zone(RTRW, RZWP3K, "RZWP3K", matriks_serasi, n_alt = n_alt)
        )
      ) %>%
      ungroup() %>%
      unnest_wider(alt_RTRW_list, names_sep = "_", names_repair = "unique") %>%
      unnest_wider(alt_RZWP3K_list, names_sep = "_", names_repair = "unique") %>%
      rename_with(~ gsub("alt_RTRW_list_", "alt_RTRW_", .x), starts_with("alt_RTRW_list_")) %>%
      rename_with(~ gsub("alt_RZWP3K_list_", "alt_RZWP3K_", .x), starts_with("alt_RZWP3K_list_"))
    
  }
  
  # Prepare data frame for export and remove empty rows
  df_export <- idx_padan_map_alt %>%
    st_drop_geometry() %>%
    filter(!is.na(id_pu) & id_pu != "") %>%  
    select(all_of(base_cols), starts_with("alt_RTRW_"), starts_with("alt_RZWP3K_")) %>%
    mutate(alt_RTRW = NA_character_, alt_RZWP3K = NA_character_) %>%
    select(all_of(base_cols), alt_RTRW, alt_RZWP3K, everything())
  
  rtrw_cols <- grep("^alt_RTRW_", names(df_export), value = TRUE)
  rzwp3k_cols <- grep("^alt_RZWP3K_", names(df_export), value = TRUE)
  
  # Helper: clean alternatives
  clean_alternatives <- function(df, cols) {
    mat <- as.matrix(df[, cols, drop = FALSE])
    cleaned <- t(apply(mat, 1, function(x) {
      u <- unique(x[!is.na(x) & x != ""])
      if (length(u) == 0) u <- "No alternative"
      c(u, rep(NA, length(cols) - length(u)))
    }))
    as.data.frame(cleaned, stringsAsFactors = FALSE)
  }
  
  df_lists_rtrw <- clean_alternatives(df_export, rtrw_cols)
  df_lists_rzwp3k <- clean_alternatives(df_export, rzwp3k_cols)
  df_validation_lists <- cbind(df_lists_rtrw, df_lists_rzwp3k)
  names(df_validation_lists) <- c(
    paste0("RTRW_", seq_along(rtrw_cols)),
    paste0("RZWP3K_", seq_along(rzwp3k_cols))
  )
  
  df_export_clean <- df_export %>% select(-all_of(c(rtrw_cols, rzwp3k_cols)))
  
  # Create workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Data")
  addWorksheet(wb, "Validation_Lists")
  writeData(wb, "Data", df_export_clean, startRow = 1, startCol = 1)
  writeData(wb, "Validation_Lists", df_validation_lists, startRow = 1, startCol = 1)
  
  unique_pu <- unique(df_export_clean$id_pu)
  if (length(unique_pu) > 0) {
    color1 <- "#DCE6F1"  # light blue
    color2 <- "#FFFFFF"  # white
    style_group1 <- createStyle(fgFill = color1)
    style_group2 <- createStyle(fgFill = color2)
    
    # Excel row numbers
    for (i in seq_along(unique_pu)) {
      pu <- unique_pu[i]
      rows_data <- which(df_export_clean$id_pu == pu)
      rows_excel <- rows_data + 1
      style <- if (i %% 2 == 1) style_group1 else style_group2
      addStyle(wb, "Data", style = style,
               rows = rows_excel,
               cols = 1:ncol(df_export_clean),
               gridExpand = TRUE)
    }
  }
  
  alt_rtrw_col_idx <- which(names(df_export_clean) == "alt_RTRW")
  alt_rzwp3k_col_idx <- which(names(df_export_clean) == "alt_RZWP3K")
  
  rtrw_start <- int2col(1)
  rtrw_end   <- int2col(length(rtrw_cols))
  rzwp3k_start <- int2col(length(rtrw_cols) + 1)
  rzwp3k_end   <- int2col(length(rtrw_cols) + length(rzwp3k_cols))
  
  valid_rows_rtrw <- which(df_lists_rtrw[, 1] != "No alternative") + 1
  valid_rows_rzwp3k <- which(df_lists_rzwp3k[, 1] != "No alternative") + 1
  
  get_contiguous_blocks <- function(indices) {
    if (length(indices) == 0) return(list())
    split(indices, cumsum(c(1, diff(indices) != 1)))
  }
  
  blocks_rtrw <- get_contiguous_blocks(valid_rows_rtrw)
  blocks_rzwp3k <- get_contiguous_blocks(valid_rows_rzwp3k)
  
  # Apply dropdowns
  for (block in blocks_rtrw) {
    start_r <- min(block)
    end_r <- max(block)
    rtrw_formula <- sprintf("'Validation_Lists'!$%s%d:$%s%d", rtrw_start, start_r, rtrw_end, start_r)
    dataValidation(
      wb = wb, sheet = "Data", cols = alt_rtrw_col_idx, rows = start_r:end_r,
      type = "list", value = rtrw_formula
    )
  }
  
  for (block in blocks_rzwp3k) {
    start_r <- min(block)
    end_r <- max(block)
    rzwp3k_formula <- sprintf("'Validation_Lists'!$%s%d:$%s%d", rzwp3k_start, start_r, rzwp3k_end, start_r)
    dataValidation(
      wb = wb, sheet = "Data", cols = alt_rzwp3k_col_idx, rows = start_r:end_r,
      type = "list", value = rzwp3k_formula
    )
  }
  
  # Colour "No alternative" cells black 
  black_style <- createStyle(fgFill = "#000000", fontColour = "#000000")
  black_rows_rtrw <- which(df_lists_rtrw[, 1] == "No alternative") + 1
  black_rows_rzwp3k <- which(df_lists_rzwp3k[, 1] == "No alternative") + 1
  
  if (length(black_rows_rtrw) > 0) {
    addStyle(wb, "Data", style = black_style, cols = alt_rtrw_col_idx, rows = black_rows_rtrw, gridExpand = FALSE)
  }
  if (length(black_rows_rzwp3k) > 0) {
    addStyle(wb, "Data", style = black_style, cols = alt_rzwp3k_col_idx, rows = black_rows_rzwp3k, gridExpand = FALSE)
  }
  
  freezePane(wb, "Data", firstRow = TRUE)
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_filename <- if (step == "step2") {
    "adjacent_alternative_zones_selections.xlsx"
  } else {
    "overlaps_alternative_zones_selections.xlsx"
  }
  output_path <- file.path(output_dir, output_filename)
  saveWorkbook(wb, output_path, overwrite = TRUE)
  
  invisible(list(workbook = wb, data = df_export_clean))
}

#' Calculate economic values (NPV) from land use/cover distributions
#'
#' This function performs the full economic assessment (Stages 7.1–7.3) for
#' RTRW and RZWP3K zones. It computes actual and recommended land cover
#' distributions, NPV per hectare, and total economic values, then joins
#' the results back to the input spatial feature set.
#'
#' @param alt_map_with_decision An `sf` object containing planning units,
#'   with columns: `id`, `id_pu`, `RTRW`, `RZWP3K`, and `area_ha` (or as
#'   specified). The geometry is preserved.
#' @param npv_lulc A data frame with LULC classes and their NPV per hectare
#'   (columns as expected by `calculate_npv_ha_per_unit()`).
#' @param matriks_land_distribution_rtrw A matrix or data frame of land
#'   distribution rules for RTRW zones.
#' @param matriks_land_distribution_rzwp3k A matrix or data frame of land
#'   distribution rules for RZWP3K zones.
#' @param class_name Character. Name of the LULC class column (used in pivoting).
#'   Default `"LC"`. This must match the class names in the matrix tables.
#' @param id_pu Character. Name of the planning unit ID column. Default `"id_pu"`.
#' @param area_col Character. Name of the area column (in hectares). Default
#'   `"area_ha"`.
#' @param id_col Character. Name of the unique feature ID column (for merging).
#'   Default `"id"`.
#'
#' @return An `sf` object identical to `alt_map_with_decision` but augmented
#'   with additional columns:
#'   - `npv_ha_actual_rtrw`, `npv_ha_actual_rzwp3k`
#'   - `npv_ha_recom_rtrw`, `npv_ha_recom_rzwp3k`
#'   - `econ_rtrw_actual`, `econ_rtrw_recom`, `econ_rtrw_delta`
#'   - `econ_rzwp3k_actual`, `econ_rzwp3k_recom`, `econ_rzwp3k_delta`
#'
#' @importFrom dplyr filter select left_join mutate rename all_of
#' @importFrom tidyr pivot_wider
#' @importFrom sf st_drop_geometry
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
  # Input validation 
  required_cols <- c(id_col, id_pu, "RTRW", "RZWP3K", area_col)
  missing <- setdiff(required_cols, names(alt_map_with_decision))
  if (length(missing) > 0) {
    stop("alt_map_with_decision missing columns: ", paste(missing, collapse = ", "))
  }
  
  # Filter planning units with non-NA zone assignments
  pu_rtrw <- alt_map_with_decision %>%
    dplyr::filter(!is.na(RTRW)) %>%
    dplyr::select(-RZWP3K)
  
  pu_rzwp3k <- alt_map_with_decision %>%
    dplyr::filter(!is.na(RZWP3K)) %>%
    dplyr::select(-RTRW)
  
  # Helper to get distribution (long format)
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
      names_from = all_of(class_name),
      values_from = proportion,
      values_fill = 0
    )
  land_dist_rtrw <- sf::st_drop_geometry(pu_rtrw) %>%
    dplyr::left_join(land_dist_rtrw_wide, by = id_pu)
  
  # RZWP3K actual
  land_dist_rzwp3k_long <- get_distribution(pu_rzwp3k, matriks_land_distribution_rzwp3k, "RZWP3K")
  land_dist_rzwp3k_wide <- land_dist_rzwp3k_long %>%
    tidyr::pivot_wider(
      names_from = all_of(class_name),
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
  
  # Recommended LULC distribution and NPV/ha
  # RTRW recommended
  land_dist_rtrw_recom_only <- calculate_land_distribution(
    pu = pu_rtrw,
    id_pu = id_pu,
    lulc_class = class_name,
    calculate_from_matrix = TRUE,
    matrix_tbl = matriks_land_distribution_rtrw,
    selected_zone = "alt_RTRW"
  ) %>%
    tidyr::pivot_wider(
      names_from = all_of(class_name),
      values_from = proportion,
      values_fill = 0
    ) %>%
    sf::st_drop_geometry()
  
  land_dist_rtrw_recom <- npv_actual_rtrw %>%
    dplyr::left_join(land_dist_rtrw_recom_only, by = id_pu)
  
  # RZWP3K recommended
  land_dist_rzwp3k_recom_only <- calculate_land_distribution(
    pu = pu_rzwp3k,
    id_pu = id_pu,
    lulc_class = class_name,
    calculate_from_matrix = TRUE,
    matrix_tbl = matriks_land_distribution_rzwp3k,
    selected_zone = "alt_RZWP3K"
  ) %>%
    tidyr::pivot_wider(
      names_from = all_of(class_name),
      values_from = proportion,
      values_fill = 0
    ) %>%
    sf::st_drop_geometry()
  
  land_dist_rzwp3k_recom <- npv_actual_rzwp3k %>%
    dplyr::left_join(land_dist_rzwp3k_recom_only, by = id_pu)
  
  # Recommended NPV/ha
  npv_recom_rtrw <- calculate_npv_ha_per_unit(land_dist_rtrw_recom, npv_lulc) %>%
    dplyr::rename(npv_ha_recom_rtrw = npv_ha)
  
  npv_recom_rzwp3k <- calculate_npv_ha_per_unit(land_dist_rzwp3k_recom, npv_lulc) %>%
    dplyr::rename(npv_ha_recom_rzwp3k = npv_ha)
  
  # Total economic values
  # RTRW
  npv_rtrw <- npv_recom_rtrw %>%
    dplyr::mutate(
      econ_rtrw_actual = .data[[area_col]] * npv_ha_actual_rtrw,
      econ_rtrw_recom  = .data[[area_col]] * npv_ha_recom_rtrw,
      econ_rtrw_delta  = econ_rtrw_recom - econ_rtrw_actual
    )
  
  # RZWP3K
  npv_rzwp3k <- npv_recom_rzwp3k %>%
    dplyr::mutate(
      econ_rzwp3k_actual = .data[[area_col]] * npv_ha_actual_rzwp3k,
      econ_rzwp3k_recom  = .data[[area_col]] * npv_ha_recom_rzwp3k,
      econ_rzwp3k_delta  = econ_rzwp3k_recom - econ_rzwp3k_actual
    )
  
  # Merge into final map
  # Identify new columns not already in alt_map_with_decision
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

#' Generate an Excel file for reconciliation with dropdown validation
#'
#' This function creates an Excel workbook containing spatial reconciliation data
#' and provides dropdown lists for manual decision entries. It takes an `sf`
#' object, extracts its attribute table, adds decision columns, and sets up
#' data validation using predefined priority options. 
#'
#' @param recon_map An `sf` object (spatial data frame) containing the
#'   reconciliation map. Its geometry column is dropped before writing to Excel.
#' @param rtrw_prioritas A data frame with a column named `"RTRW"` containing
#'   the valid priority options for the RTRW decision dropdown. Non-`NA` values
#'   are used as the list of choices.
#' @param rzwp3k_prioritas A data frame with a column named `"RZWP3K"` containing
#'   the valid priority options for the RZWP3K decision dropdown. Non-`NA` values
#'   are used as the list of choices.
#' @param step Integer, must be `1` or `2`. If `2`, the workbook contains two
#'   decision columns (`decision_rtrw` and `decision_rzwp3k`) with separate
#'   dropdown lists. If `1`, only one column (`user_decision`) is added, with a
#'   dropdown combining both lists (simple concatenation, no deduplication).
#' @param output_dir Character string specifying the directory where the Excel
#'   file will be saved. The directory is created recursively if it does not exist.
#' @param file_name Character string giving the name of the output Excel file.
#'   Defaults to `"recon_map.xlsx"`.
#' @param group_col Character string naming a column in `recon_map` to group rows
#'   for alternating background colors. If `NULL` (default), the function looks
#'   for a column named `"id_pu"`; if found, it is used. If no suitable column
#'   exists, no row coloring is applied.
#'
#' @return The function is called for its side effect of creating an Excel file.
#'   It returns `NULL` invisibly.
#'
#' @details The workbook now contains three sheets:
#' \itemize{
#'   \item **Data**: Main data with dropdown validations.
#'   \item **Lists**: Option lists for dropdowns.
#'   \item **Glossary**: Column descriptions extracted from the documentation.
#' }
#' Additionally, the header row and the first four columns of the **Data** sheet
#' are frozen for easier navigation.
#'
#' @examples
#' \dontrun{
#' # Step 2 with grouping and glossary
#' generate_reconciliation_excel(recon_sf, rtrw_prior, rzp3k_prior, 
#'                               step = 2, output_dir = "out",
#'                               group_col = "id_pu")
#' }
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData dataValidation
#'   saveWorkbook createStyle addStyle freezePane
#' @importFrom sf st_drop_geometry
#' @export
generate_reconciliation_excel <- function(recon_map, 
                                          rtrw_prioritas, 
                                          rzwp3k_prioritas, 
                                          output_dir, 
                                          step, 
                                          file_name = "recon_map.xlsx",
                                          group_col = NULL) {
  
  # Validate step argument
  if (missing(step) || !(step %in% c(1, 2))) {
    stop("'step' must be explicitly provided and must be either 1 or 2.")
  }
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("package 'openxlsx' is required.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("package 'sf' is required.")
  
  df_flat <- sf::st_drop_geometry(recon_map)
  
  if (step == 2) {
    df_flat$user_decision_rtrw   <- NA_character_
    df_flat$user_decision_rzwp3k <- NA_character_
  } else { 
    df_flat$user_decision <- NA_character_
  }
  
  # Extract option lists 
  rtrw_opts   <- as.character(rtrw_prioritas$RTRW)
  rzwp3k_opts <- as.character(rzwp3k_prioritas$RZWP3K)
  rtrw_opts   <- rtrw_opts[!is.na(rtrw_opts)]
  rzwp3k_opts <- rzwp3k_opts[!is.na(rzwp3k_opts)]
  
  # Create Workbook and Sheets
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Data")
  openxlsx::addWorksheet(wb, "Lists")
  openxlsx::writeData(wb, "Data", df_flat)
  
  # Apply alternating row background colors based on grouping column
  use_col <- NULL
  if (!is.null(group_col) && group_col %in% names(df_flat)) {
    use_col <- group_col
  } else if ("id_pu" %in% names(df_flat)) {
    use_col <- "id_pu"
    message("Using column 'id_pu' for alternating row colors.")
  } else {
    message("No suitable grouping column found; skipping row coloring.")
  }
  
  if (!is.null(use_col)) {
    unique_vals <- unique(df_flat[[use_col]])
    if (length(unique_vals) > 0) {
      color1 <- "#DCE6F1"   # light blue
      color2 <- "#FFFFFF"   # white
      style_group1 <- openxlsx::createStyle(fgFill = color1)
      style_group2 <- openxlsx::createStyle(fgFill = color2)
      
      # Data rows start at row 2 (header is row 1)
      for (i in seq_along(unique_vals)) {
        val <- unique_vals[i]
        rows_data <- which(df_flat[[use_col]] == val)
        rows_excel <- rows_data + 1
        style <- if (i %% 2 == 1) style_group1 else style_group2
        openxlsx::addStyle(wb, "Data", style = style,
                           rows = rows_excel,
                           cols = 1:ncol(df_flat),
                           gridExpand = TRUE)
      }
    }
  }
  
  # Write option lists to the Lists sheet
  openxlsx::writeData(wb, "Lists", x = "RTRW Options", startCol = 1, startRow = 1)
  if (length(rtrw_opts) > 0) {
    openxlsx::writeData(wb, "Lists", x = rtrw_opts, startCol = 1, startRow = 2, colNames = FALSE)
  }
  openxlsx::writeData(wb, "Lists", x = "RZWP3K Options", startCol = 2, startRow = 1)
  if (length(rzwp3k_opts) > 0) {
    openxlsx::writeData(wb, "Lists", x = rzwp3k_opts, startCol = 2, startRow = 2, colNames = FALSE)
  }
  
  # If step == 1, write combined list to column C and set validation for user_decision
  if (step == 1) {
    combined_opts <- c(rtrw_opts, rzwp3k_opts)
    openxlsx::writeData(wb, "Lists", x = "Combined Options", startCol = 3, startRow = 1)
    if (length(combined_opts) > 0) {
      openxlsx::writeData(wb, "Lists", x = combined_opts, startCol = 3, startRow = 2, colNames = FALSE)
    }
    
    col_decision <- which(names(df_flat) == "user_decision")
    if (length(col_decision) == 0) stop("Column 'user_decision' not found in data frame.")
    rows <- 2:(nrow(df_flat) + 1)
    last_row_combined <- length(combined_opts) + 1 
    formula_combined <- paste0("=Lists!$C$2:$C$", last_row_combined)
    
    openxlsx::dataValidation(wb, "Data",
                             col = col_decision,
                             rows = rows,
                             type = "list",
                             value = formula_combined)
  } else {
    # step == 2: apply validations for both decision columns
    col_rtrw   <- which(names(df_flat) == "user_decision_rtrw")
    col_rzwp3k <- which(names(df_flat) == "user_decision_rzwp3k")
    if (length(col_rtrw) == 0 || length(col_rzwp3k) == 0) {
      stop("Required decision columns not found in data frame.")
    }
    rows <- 2:(nrow(df_flat) + 1)
    
    last_row_rtrw   <- length(rtrw_opts) + 1
    last_row_rzwp3k <- length(rzwp3k_opts) + 1
    formula_rtrw   <- paste0("=Lists!$A$2:$A$", last_row_rtrw)
    formula_rzwp3k <- paste0("=Lists!$B$2:$B$", last_row_rzwp3k)
    
    openxlsx::dataValidation(wb, "Data",
                             col = col_rtrw,
                             rows = rows,
                             type = "list",
                             value = formula_rtrw)
    
    openxlsx::dataValidation(wb, "Data",
                             col = col_rzwp3k,
                             rows = rows,
                             type = "list",
                             value = formula_rzwp3k)
  }
  
  glossary_data <- data.frame(
    Kelompok = c(
      rep("Kolom Identitas", 4),
      rep("Kolom Zona", 3),
      rep("Kolom Indeks SERASI & PADU", 9),
      rep("Kolom Indeks PADAN", 1),
      rep("Kolom Alternatif Zona", 2),
      rep("Kolom Indeks Alternatif", 4),
      rep("Kolom Rekomendasi & Keputusan Sistem", 3),
      rep("Kolom Keputusan Pengguna", 1)
    ),
    Kolom = c(
      "id_pu", "stat_pu", "id_rtrw", "id_rzwp3k",
      "RTRW", "RZWP3K", "area_ha",
      "idx_serasi", "idx_padu_hs", "idx_padu_ke", "idx_padu_kh",
      "idx_padu_ki", "idx_padu_kl", "idx_padu_se", "idx_padu_rtp", "idx_padu_final",
      "idx_padan",
      "alt_RTRW", "alt_RZWP3K",
      "idx_serasi_rtrw_alt", "idx_serasi_rzwp3k_alt",
      "idx_padan_rtrw_alt", "idx_padan_rzwp3k_alt",
      "recommendation", "decision", "idx_padan_final",
      "user_decision"
    ),
    Deskripsi = c(
      "ID unik unit perencanaan (Planning Unit). Merupakan penomoran tiap area konflik tumpang tindih yang diidentifikasi.",
      "Status geometri unit perencanaan. Nilai tipikal: `intersection` (area tumpang tindih antara RTRW dan RZWP3K).",
      "ID fitur RTRW asal yang membentuk unit perencanaan ini.",
      "ID fitur RZWP3K asal yang membentuk unit perencanaan ini.",
      "Nama kelas/zona kawasan RTRW yang berlaku pada unit perencanaan ini (kondisi eksisting).",
      "Nama kelas/zona RZWP3K yang berlaku pada unit perencanaan ini (kondisi eksisting).",
      "Luas unit perencanaan dalam satuan hektar (ha).",
      "Indeks SERASI — mengukur tingkat kesesuaian/kompatibilitas antara kelas RTRW dan RZWP3K berdasarkan matriks serasi. Rentang 0–1, semakin tinggi semakin serasi.",
      "Indeks PADU-HS (Hidrologi dan Sedimentasi) — menilai kepaduan lingkungan berdasarkan kedekatan terhadap estuari dan tingkat TSS.",
      "Indeks PADU-KE (Keterpaduan Penggunaan Lahan dan Perairan) — menilai kepaduan berdasarkan ketetanggaan kelas tutupan/penggunaan lahan dan perairan.",
      "Indeks PADU-KH (Komposisi Habitat) — menilai kepaduan berdasarkan persentase tutupan habitat pesisir (mangrove, terumbu karang, lamun).",
      "Indeks PADU-KI (Ketahanan Iklim) — menilai kepaduan berdasarkan tingkat risiko bencana pada unit perencanaan.",
      "Indeks PADU-KL (Kawasan Lindung) — menilai kepaduan berdasarkan kondisi kualitas lingkungan perairan.",
      "Indeks PADU-SE (Sosial & Ekonomi) — menilai kepaduan berdasarkan aktivitas manusia yang mengindikasikan adanya nilai ekonomi dan sosial.",
      "Indeks PADU-RTp (Risiko dan Tekanan) — menilai kepaduan berdasarkan jarak ke sumber tekanan (industri dan alur pelayaran).",
      "Indeks PADU gabungan — hasil pembobotan dari seluruh komponen PADU (KE, HS, KL, KH, RTp, SE, KI). Rentang 0–1.",
      "Indeks PADAN eksisting — nilai integrasi tata ruang darat-laut saat ini, dihitung dari kombinasi indeks SERASI dan PADU: `(α × idx_serasi) + ((1−α) × idx_padu_final)`.",
      "Kawasan/Zona RTRW alternatif yang direkomendasikan untuk menggantikan kawasan/zona RTRW eksisting guna meningkatkan integrasi (Indeks SERASI).",
      "Kawasan/Zona RZWP3K alternatif yang direkomendasikan untuk menggantikan kawasan/zona RZWP3K eksisting guna meningkatkan integrasi (Indeks SERASI).",
      "Indeks SERASI yang dihitung jika zona RTRW diganti dengan `alt_RTRW` (berpasangan dengan RZWP3K eksisting).",
      "Indeks SERASI yang dihitung jika zona RZWP3K diganti dengan `alt_RZWP3K` (berpasangan dengan RTRW eksisting).",
      "Indeks PADAN proyeksi jika zona RTRW diganti dengan `alt_RTRW`.",
      "Indeks PADAN proyeksi jika zona RZWP3K diganti dengan `alt_RZWP3K`.",
      "Rekomendasi awal dari sistem berdasarkan logika prioritas zona dan perbandingan indeks PADAN alternatif.",
      "Keputusan akhir sistem — hasil evaluasi apakah penggantian zona benar-benar meningkatkan indeks PADAN.",
      "Indeks PADAN setelah keputusan diterapkan. Jika keputusan adalah `Tetap/Koordinasi`, nilai ini sama dengan `idx_padan` eksisting.",
      "Kolom yang diisi oleh pengguna. Keputusan rekonsiliasi akhir yang dipilih berdasarkan keputusan pengguna untuk setiap unit perencanaan. Nilai yang valid adalah salah satu dari: zona RTRW baru, zona RZWP3K baru, atau `Tetap/Koordinasi`. Kolom ini menjadi input utama untuk proses rekonsiliasi spasial di modul berikutnya."
    ),
    stringsAsFactors = FALSE
  )
  
  openxlsx::addWorksheet(wb, "Glossary")
  openxlsx::writeData(wb, "Glossary", glossary_data, startRow = 1, startCol = 1)
  # Freeze header row in Glossary
  openxlsx::freezePane(wb, "Glossary", firstRow = TRUE)
  
  # Freeze header row and first 4 columns in Data sheet
  openxlsx::freezePane(wb, "Data", firstActiveRow = 2, firstActiveCol = 5)
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save workbook
  full_path <- file.path(output_dir, file_name)
  openxlsx::saveWorkbook(wb, full_path, overwrite = TRUE)
  
  message(paste("Workbook successfully saved to:", full_path))
  invisible(NULL)
}

#' Dissolve paired RTRW and RZWP3K polygons by `id_pu`
#'
#' This function takes an `sf` object where each `id_pu` groups exactly two 
#' features: one with a non-`NA` value in the `RTRW` column and one with a 
#' non-`NA` value in the `RZWP3K` column. It dissolves (unions) the geometries 
#' for each `id_pu`, creates a new identifier by concatenating the original 
#' `id` values from the RTRW and RZWP3K rows (separated by an underscore), 
#' sums the `area_ha` fields, concatenates the `admin` values, and retains 
#' the other attributes (assuming they are identical within the pair).
#'
#' @param sf_obj An `sf` object (simple feature collection) that must contain 
#'   the following columns: `id`, `id_pu`, `RTRW`, `RZWP3K`, `area_ha`, 
#'   `length`, `area_buffer_ha`, `idx_serasi`, and `admin`. Each `id_pu` should 
#'   have exactly two rows: one with a non-`NA` `RTRW` and the other with a 
#'   non-`NA` `RZWP3K`.
#'
#' @return An `sf` object with one feature per unique `id_pu`. The output 
#'   includes the following columns:
#'   \itemize{
#'     \item `id`: new identifier in the format `"<RTRW_id>_<RZWP3K_id>"`.
#'     \item `id_pu`: original grouping identifier.
#'     \item `RTRW`: the non-`NA` value from the RTRW row.
#'     \item `RZWP3K`: the non-`NA` value from the RZWP3K row.
#'     \item `area_ha`: numeric sum of the two areas.
#'     \item `admin`: concatenated admin values in the format 
#'       `"<RTRW_admin>_<RZWP3K_admin>"`.
#'     \item `length`: first value (assumed identical).
#'     \item `area_buffer_ha`: first value (assumed identical).
#'     \item `idx_serasi`: first value (assumed identical).
#'     \item `geometry`: unioned (dissolved) geometry.
#'   }
#'   The CRS is preserved from the input object.
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Splits the data into RTRW and RZWP3K subsets.
#'   \item Validates that the number of rows and `id_pu` values match between 
#'         the two subsets.
#'   \item Joins the attribute tables to combine `id`, `area_ha`, and `admin` 
#'         from both rows, then computes the new `id`, sums `area_ha`, and 
#'         concatenates `admin`.
#'   \item Unions the geometries for each `id_pu`.
#'   \item Merges the attributes with the unioned geometries and restores the CRS.
#' }
#'
#' @importFrom dplyr filter st_drop_geometry select inner_join mutate 
#'   group_by summarise rename st_as_sf
#' @importFrom sf st_union st_crs
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Assuming `idx_serasi_map` is your sf object with the required columns
#' dissolved <- dissolve_id_pu(idx_serasi_map)
#' plot(dissolved["area_ha"])
#' }
#'
#' @export
dissolve_id_pu <- function(sf_obj) {
  
  # Required columns
  required_cols <- c(
    "id",
    "id_pu",
    "id_group",
    "n_pairs",
    "RTRW",
    "RZWP3K",
    "area_ha",
    "length",
    "area_buffer_ha",
    "idx_serasi",
    "admin"
  )
  
  stopifnot(all(required_cols %in% colnames(sf_obj)))
  
  # Split into RTRW and RZWP3K rows
  rtrw <- sf_obj %>% 
    filter(!is.na(RTRW))
  
  rzwp3k <- sf_obj %>% 
    filter(!is.na(RZWP3K))
  
  # Basic validation
  if (nrow(rtrw) != nrow(rzwp3k)) {
    stop("Unequal number of RTRW and RZWP3K rows.")
  }
  
  if (!all(rtrw$id_pu == rzwp3k$id_pu)) {
    stop("Mismatched id_pu between RTRW and RZWP3K rows.")
  }
  
  # Join attributes
  combined <- rtrw %>%
    st_drop_geometry() %>%
    select(
      id_pu,
      id_rtrw = id,
      RTRW,
      area_ha_rtrw = area_ha,
      admin_rtrw = admin,
      length,
      area_buffer_ha,
      idx_serasi,
      id_group,
      n_pairs
    ) %>%
    inner_join(
      rzwp3k %>%
        st_drop_geometry() %>%
        select(
          id_pu,
          id_rzwp3k = id,
          RZWP3K,
          area_ha_rzwp3k = area_ha,
          admin_rzwp3k = admin
        ),
      by = "id_pu"
    ) %>%
    mutate(
      new_id = paste0(id_rtrw, "_", id_rzwp3k),
      area_ha = area_ha_rtrw + area_ha_rzwp3k,
      admin = paste0(admin_rtrw, "_", admin_rzwp3k)
    ) %>%
    select(
      id_pu,
      new_id,
      RTRW,
      RZWP3K,
      area_ha,
      admin,
      length,
      area_buffer_ha,
      idx_serasi,
      id_group,
      n_pairs
    )
  
  # Union geometries per id_pu
  geom_union <- sf_obj %>%
    group_by(id_pu) %>%
    summarise(
      geometry = st_union(geometry),
      .groups = "drop"
    )
  
  # Merge attributes with unioned geometries
  result <- geom_union %>%
    inner_join(combined, by = "id_pu") %>%
    rename(id = new_id) %>%
    select(
      id,
      id_pu,
      id_group,
      RTRW,
      RZWP3K,
      area_ha,
      admin,
      length,
      area_buffer_ha,
      n_pairs,
      idx_serasi,
      geometry
    ) %>%
    st_as_sf()
  
  st_crs(result) <- st_crs(sf_obj)
  
  return(result)
}

# Look up compatibility
get_compat <- function(x, y) {
  if (is.na(x) || is.na(y)) return(NA_real_)
  val <- matriks_serasi %>%
    filter(class1 == x, class2 == y) %>%
    pull(idx_serasi)
  if (length(val) == 0) NA_real_ else val
}

#' Reconcile spatial layer with update and exclusion mask (Step 2)
#'
#' This function integrates an updated spatial layer into a base map, using an exclusion mask
#' to prevent updates from extending into protected areas. It trims the updates, removes the
#' updated footprint from the base map, and combines the unchanged base areas with the updated
#' polygons. The result includes attributes indicating whether each polygon was reconciled,
#' whether it originated from the update layer (adjacent), and carries forward relevant identifiers.
#'
#' @param base_map An `sf` object representing the base map. Must contain an `id` column and a column
#'   named after `layer_name` (the original class column). If any of the required additional columns
#'   (`id_pu`, `idx_serasi`, `idx_padu_final`, `idx_padan`) are missing, they will be added as `NA`.
#' @param exclusion_mask An `sf` object containing polygons that define areas where updates should
#'   be excluded (e.g., protected areas). These areas are subtracted from the update polygons.
#' @param update_layer An `sf` object containing the updated polygons. Must contain columns named
#'   `layer_name` (original class) and `paste0("user_decision_", layer_name)` (new class).
#'   It may also contain `id`, `id_pu`, and the index columns; if missing, `NA` will be used.
#' @param layer_name A character string naming the thematic layer being processed (e.g., `"rtrw"`).
#'   This is used to construct column names.
#'
#' @return An `sf` object with `MULTIPOLYGON` geometry. The output includes the following columns:
#'   \item{id_pu}{Original polygon identifier.}
#'   \item{\{layer_name\}_old}{Original class values.}
#'   \item{\{layer_name\}_new}{New class values (named as `user_decision_{layer_name}`).}
#'   \item{Reconcile}{Character flag: `"Yes"` if the polygon was updated (class change), otherwise `"No"`.}
#'   \item{adjacent}{Character flag: `"Yes"` if the polygon originated from the update layer, `"No"` if it originated from the base map.}
#'   \item{idx_serasi}{Index value from base map (unchanged) or update layer (updated).}
#'   \item{idx_padu_final}{Likewise.}
#'   \item{idx_padan}{Likewise.}
#'   \item{geometry}{`MULTIPOLYGON` geometry.}
#'
#' @details The function performs several steps:
#' \enumerate{
#'   \item Ensures the required columns exist in both `base_map` and `update_layer`.
#'   \item Prepares the update layer, assigning `adjacent = "Yes"`.
#'   \item Trims the update polygons using [sf::st_difference()] against the exclusion mask.
#'   \item Subtracts the update footprint from the base map, assigning `adjacent = "No"` to the preserved base areas.
#'   \item Binds the layers and determines `Reconcile` status.
#' }
#'
#' @import sf dplyr rlang
#' @export
reconcile_map_step2 <- function(base_map, exclusion_mask, update_layer, layer_name) {
  col_orig <- layer_name
  col_new <- tolower(paste0("user_decision_", layer_name))
  col_old_output <- paste0(layer_name, "_old")
  
  # Columns to preserve
  id_cols <- c("id_pu")
  idx_cols <- c("idx_serasi", "idx_padu_final", "idx_padan")
  all_extra_cols <- c(id_cols, idx_cols)
  
  for (col in all_extra_cols) {
    if (!col %in% names(base_map)) base_map[[col]] <- NA
    if (!col %in% names(update_layer)) update_layer[[col]] <- NA
  }
  
  message(paste("Processing integration for:", layer_name, "..."))
  max_base_id <- max(base_map$id, na.rm = TRUE)
  
  # Prepare update layer
  update_prep <- update_layer %>%
    filter(!st_is_empty(geom)) %>%  
    filter(!is.na(!!sym(col_new))) %>%    
    select(Old = !!sym(col_orig), New = !!sym(col_new), 
           all_of(id_cols), all_of(idx_cols)) %>%  
    mutate(id = as.integer(max_base_id + row_number()),
           Adjacent = "Yes") %>%
    rename(geometry = geom) %>%
    st_make_valid()
  
  message("  > Generating exclusion mask...")
  mask_geom <- st_combine(st_make_valid(exclusion_mask))
  
  message("  > Trimming update boundaries...")
  update_trimmed <- st_difference(update_prep, mask_geom) %>%
    st_collection_extract("POLYGON")
  
  update_footprint <- st_union(update_trimmed)
  
  message("  > Updating base geometries...")
  base_cutout <- st_difference(st_make_valid(base_map), update_footprint) %>%
    rename(Old = !!sym(col_orig)) %>%
    mutate(New = Old,
           Adjacent = "No")
  
  message("  > Finalizing attributes...")
  final_map <- bind_rows(base_cutout, update_trimmed) %>%
    mutate(
      Reconcile = if_else(coalesce(Old, "") == coalesce(New, ""), "No", "Yes")
    ) %>%
    rename(!!sym(col_old_output) := Old,
           !!sym(col_new) := New) %>%
    select(id_pu, all_of(id_cols), all_of(col_old_output), all_of(col_new), 
           Reconcile, Adjacent, all_of(idx_cols), geometry) %>%
    st_make_valid() %>%
    st_cast("MULTIPOLYGON")
  
  message(paste("Success! Integrated map for", layer_name, "generated."))
  return(final_map)
}

#' Step 2 of reconciliation: process and integrate RTRW/RZWP3K with compatibility
#'
#' This function reads a reconciliation table, computes majority decisions,
#' joins them to a spatial layer, reconciles two base maps using a custom
#' reconciliation function, standardizes and merges the results, then computes
#' compatibility indices and updates the spatial data frame.
#'
#' @param recon_table_path Path to the Excel reconciliation table.
#' @param adjacent_recom_map An sf object containing the adjacent reconciliation polygons.
#'   Must have columns `id`, `id_pu`, and geometry.
#' @param rtrw_vect RTRW base map (spatial object) accepted by `reconcile_map_step2`.
#' @param rzwp3k_vect RZWP3K base map (spatial object) accepted by `reconcile_map_step2`.
#' @param matriks_serasi A pre-loaded compatibility matrix (as returned by
#'   `load_validate_matrix_table`). This matrix is used by `get_compat`.
#' @param alpha Numeric weight for the new compatibility index (default 0.5).
#'
#' @return An sf object `integrated_map_idx` with columns including
#'   `id_pu`, `Source`, `Zoning_Old`, `Zoning_New`, `Adjacent`, `Reconcile`,
#'   `idx_serasi`, `idx_padu_final`, `idx_padan`, `idx_serasi_new`,
#'   `idx_padan_new`, `delta_idx_padan`, and geometry.
#'
#' @details This function depends on the following external functions that must
#'   be defined in the calling environment:
#'   - `reconcile_map_step2(base_map, exclusion_mask, update_layer, layer_name)`
#'   - `get_compat(z_a, z_b)` – uses the loaded compatibility matrix (`matriks_serasi`).
#'
#'   The compatibility matrix is expected to be loaded externally (e.g., via
#'   `load_validate_matrix_table`) and passed as an argument to avoid reloading.
#'
#'   It also requires the following packages: `dplyr`, `tidyr`, `sf`, `readxl`, `purrr`.
#'
#' @examples
#' \dontrun{
#' matriks <- load_validate_matrix_table("path/to/matriks_serasi.xlsx", title = "serasi")
#' result <- reconcilliation_step2(
#'   recon_table_path = "D:/step2_sulteng/adjacent_reconcilliation_table_filled.xlsx",
#'   adjacent_recom_map = my_adjacent_map,
#'   rtrw_vect = rtrw_spatial,
#'   rzwp3k_vect = rzwp3k_spatial,
#'   matriks_serasi = matriks,
#'   alpha = 0.5
#' )
#' }
reconcilliation_step2 <- function(recon_table_path,
                                  adjacent_recom_map,
                                  rtrw_vect,
                                  rzwp3k_vect,
                                  matriks_serasi,
                                  alpha = 0.5) {
  
  get_compat <- function(x, y) {
    if (is.na(x) || is.na(y)) return(NA_real_)
    val <- matriks_serasi %>%
      dplyr::filter(class1 == x, class2 == y) %>%
      dplyr::pull(idx_serasi)
    if (length(val) == 0) NA_real_ else val[1]
  }
  
  # Read reconciliation table
  recon_table <- read_xlsx(recon_table_path)
  
  # Most common RTRW decision per (id, id_pu)
  decision_rtrw <- recon_table %>%
    filter(!is.na(user_decision_rtrw), user_decision_rtrw != "") %>%
    group_by(id, id_pu) %>%
    summarise(
      user_decision_rtrw = names(sort(table(user_decision_rtrw), decreasing = TRUE))[1],
      .groups = "drop"
    )
  
  # Most common RZWP3K decision per (id, id_pu)
  decision_rzwp3k <- recon_table %>%
    filter(!is.na(user_decision_rzwp3k), user_decision_rzwp3k != "") %>%
    group_by(id, id_pu) %>%
    summarise(
      user_decision_rzwp3k = names(sort(table(user_decision_rzwp3k), decreasing = TRUE))[1],
      .groups = "drop"
    )
  
  # Merge decisions into a clean table (one row per pair)
  recon_table_clean <- recon_table %>%
    distinct(id, id_pu, .keep_all = TRUE) %>%
    select(id, id_pu) %>%
    left_join(decision_rtrw, by = c("id", "id_pu")) %>%
    left_join(decision_rzwp3k, by = c("id", "id_pu"))
  
  # Join decisions to the spatial adjacent reconciliation layer
  adjacent_reconcile_map <- adjacent_recom_map %>%
    left_join(recon_table_clean, by = c("id", "id_pu"))
  
  # Reconcile RTRW and RZWP3K using the custom reconciliation function
  rtrw_reconciled <- reconcile_map_step2(
    base_map = rtrw_vect,
    exclusion_mask = rzwp3k_vect,
    update_layer = adjacent_reconcile_map,
    layer_name = "RTRW"
  )
  
  rzwp3k_reconciled <- reconcile_map_step2(
    base_map = rzwp3k_vect,
    exclusion_mask = rtrw_vect,
    update_layer = adjacent_reconcile_map,
    layer_name = "RZWP3K"
  )
  
  # Standardise column names and select relevant columns
  rtrw_clean <- rtrw_reconciled %>%
    rename(Zoning_Old = RTRW_old,
           Zoning_New = user_decision_rtrw) %>%
    mutate(Source = "RTRW") %>%
    select(id_pu, Source, Zoning_Old, Zoning_New, Adjacent, Reconcile,
           idx_serasi, idx_padu_final, idx_padan, geometry)
  
  rzwp3k_clean <- rzwp3k_reconciled %>%
    rename(Zoning_Old = RZWP3K_old,
           Zoning_New = user_decision_rzwp3k) %>%
    mutate(Source = "RZWP3K") %>%
    select(id_pu, Source, Zoning_Old, Zoning_New, Adjacent, Reconcile,
           idx_serasi, idx_padu_final, idx_padan, geometry)
  
  # Merge both layers into one integrated map
  integrated_map <- bind_rows(rtrw_clean, rzwp3k_clean) %>%
    st_make_valid()
  
  # Compute new compatibility indices for adjacent polygons
  compatibility_lookup <- integrated_map %>%
    filter(Adjacent == "Yes") %>%
    group_by(id_pu) %>%
    summarize(z_a = first(Zoning_New), z_b = last(Zoning_New), .groups = "drop") %>%
    st_drop_geometry() %>%
    mutate(idx_serasi_new = map2_dbl(z_a, z_b, get_compat)) %>%
    select(id_pu, idx_serasi_new)
  
  # Add new indices to the integrated map
  integrated_map_idx <- integrated_map %>%
    left_join(compatibility_lookup, by = "id_pu") %>%
    mutate(
      idx_serasi_new = as.numeric(idx_serasi_new),
      idx_padu_final = as.numeric(idx_padu_final),
      idx_padan      = as.numeric(idx_padan),
      idx_padan_new  = if_else(Adjacent == "Yes",
                               (alpha * idx_serasi_new) + ((1 - alpha) * idx_padu_final),
                               NA_real_),
      delta_idx_padan = if_else(Adjacent == "Yes",
                                idx_padan_new - idx_padan,
                                NA_real_)
    )
  
  return(integrated_map_idx)
}

#' Step 1 of reconciliation: process overlaps with priority-based decisions
#'
#' This function reconciles overlapping RTRW and RZWP3K polygons using priority
#' lists, computes compatibility indices for the chosen decisions, and returns
#' the two reconciled layers separately (not merged).
#'
#' @param rtrw_base   sf object of the original RTRW layer (must have columns
#'                    `id_pu`, `RTRW`, `idx_serasi`, `idx_padu_final`, `idx_padan`)
#' @param rzwp3k_base sf object of the original RZWP3K layer (similar columns)
#' @param overlaps_map sf object of overlapping polygons, with columns
#'                     `id_pu`, `RTRW`, `RZWP3K`, `user_decision`
#' @param rtrw_priority    data frame of RTRW priority classes (column `RTRW`)
#' @param rzwp3k_priority  data frame of RZWP3K priority classes (column `RZWP3K`)
#' @param alpha       numeric weight for new compatibility (default 0.5)
#'
#' @return A list with two `sf` objects:
#'   - `rtrw`: reconciled RTRW layer with new indices
#'   - `rzwp3k`: reconciled RZWP3K layer with new indices
#'
#'   Each has columns: `id_pu`, `Zoning_Old`, `Zoning_New`, `Overlap`, `Reconcile`,
#'   `idx_serasi`, `idx_padu_final`, `idx_padan`,
#'   `Overlap_Pair`, `idx_serasi_new`, `idx_padan_new`, `delta_idx_padan`,
#'   and `geometry`.
#'
#' @details Requires `get_compat` to be defined in the calling environment.
#'
#' @examples
#' \dontrun{
#' result <- reconciliation_step1(
#'   rtrw_base   = rtrw_vect,
#'   rzwp3k_base = rzwp3k_vect,
#'   overlaps_map = overlaps_reconcilliation_map,
#'   rtrw_priority    = rtrw_priority_table,
#'   rzwp3k_priority  = rzwp3k_priority_table,
#'   alpha       = 0.5
#' )
#' rtrw_final_idx   <- result$rtrw
#' rzwp3k_final_idx <- result$rzwp3k
#' }
reconciliation_step1 <- function(rtrw_base, rzwp3k_base, overlaps_map,
                                 rtrw_priority, rzwp3k_priority, matriks_serasi, alpha = 0.5) {
  
  if (missing(matriks_serasi) || is.null(matriks_serasi)) {
    stop("'matriks_serasi' must be provided (the loaded compatibility matrix).")
  }
  
  get_compat <- function(x, y) {
    if (is.na(x) || is.na(y)) return(NA_real_)
    val <- matriks_serasi %>%
      dplyr::filter(class1 == x, class2 == y) %>%
      dplyr::pull(idx_serasi)
    if (length(val) == 0) NA_real_ else val[1]
  }
  
  init_cols <- function(df) {
    df %>%
      mutate(
        id_pu = if ("id_pu" %in% names(.)) id_pu else NA_real_,
        idx_serasi = if ("idx_serasi" %in% names(.)) idx_serasi else NA_real_,
        idx_padu_final = if ("idx_padu_final" %in% names(.)) idx_padu_final else NA_real_,
        idx_padan = if ("idx_padan" %in% names(.)) idx_padan else NA_real_
      )
  }
  
  normalize_geometry <- function(x) {
    geom_col <- attr(x, "sf_column")
    
    if (!is.null(geom_col) && geom_col != "geometry") {
      names(x)[names(x) == geom_col] <- "geometry"
      st_geometry(x) <- "geometry"
    }
    
    x
  }
  
  # Clean geometries
  rtrw_base <- st_zm(rtrw_base, drop = TRUE) %>%
    init_cols() %>%
    normalize_geometry()
  
  rzwp3k_base <- st_zm(rzwp3k_base, drop = TRUE) %>%
    init_cols() %>%
    normalize_geometry()
  
  overlaps_map <- st_zm(overlaps_map, drop = TRUE) %>%
    normalize_geometry()
  
  # Standardize geometry column name
  geom_col <- attr(overlaps_map, "sf_column")
  if (geom_col != "geometry") {
    names(overlaps_map)[names(overlaps_map) == geom_col] <- "geometry"
    st_geometry(overlaps_map) <- "geometry"
  }
  
  # Determine winners
  overlaps_map <- overlaps_map %>%
    mutate(
      winner = case_when(
        user_decision %in% rtrw_priority$RTRW ~ "RTRW",
        user_decision %in% rzwp3k_priority$RZWP3K ~ "RZWP3K",
        TRUE ~ "None"
      )
    )
  
  # Remove overlap from base maps
  overlap_union <- st_union(st_make_valid(overlaps_map))
  
  rtrw_cutout <- st_difference(rtrw_base, overlap_union) %>%
    mutate(
      Zoning_Old = RTRW,
      Zoning_New = RTRW,
      Overlap = "No",
      Reconcile = "No"
    ) %>%
    select(
      id_pu, Zoning_Old, Zoning_New,
      Overlap, Reconcile,
      idx_serasi, idx_padu_final, idx_padan
    )
  
  rzwp3k_cutout <- st_difference(rzwp3k_base, overlap_union) %>%
    mutate(
      Zoning_Old = RZWP3K,
      Zoning_New = RZWP3K,
      Overlap = "No",
      Reconcile = "No"
    ) %>%
    select(
      id_pu, Zoning_Old, Zoning_New,
      Overlap, Reconcile,
      idx_serasi, idx_padu_final, idx_padan
    )
  
  # Winning polygons
  rtrw_wins <- overlaps_map %>%
    filter(winner == "RTRW") %>%
    mutate(
      Zoning_Old = RTRW,
      Zoning_New = user_decision,
      Overlap = "Yes",
      Reconcile = if_else(Zoning_Old == Zoning_New, "No", "Yes")
    ) %>%
    select(
      id_pu, Zoning_Old, Zoning_New,
      Overlap, Reconcile,
      idx_serasi, idx_padu_final, idx_padan
    )
  
  rzwp3k_wins <- overlaps_map %>%
    filter(winner == "RZWP3K") %>%
    mutate(
      Zoning_Old = RZWP3K,
      Zoning_New = user_decision,
      Overlap = "Yes",
      Reconcile = if_else(Zoning_Old == Zoning_New, "No", "Yes")
    ) %>%
    select(
      id_pu, Zoning_Old, Zoning_New,
      Overlap, Reconcile,
      idx_serasi, idx_padu_final, idx_padan
    )
  
  rtrw_final <- rbind(rtrw_cutout, rtrw_wins) %>%
    st_make_valid()
  
  rzwp3k_final <- rbind(rzwp3k_cutout, rzwp3k_wins) %>%
    st_make_valid()
  
  lookup <- overlaps_map %>%
    st_drop_geometry() %>%
    select(id_pu, RTRW, RZWP3K) %>%
    distinct(id_pu, .keep_all = TRUE)
  
  # RTRW output
  rtrw_idx <- rtrw_final %>%
    left_join(
      lookup %>% select(id_pu, RZWP3K),
      by = "id_pu"
    ) %>%
    mutate(
      Overlap_Pair = if_else(
        Overlap == "Yes",
        RZWP3K,
        NA_character_
      ),
      idx_serasi_new = if_else(
        Overlap == "Yes",
        map2_dbl(Zoning_New, RZWP3K, get_compat),
        NA_real_
      ),
      idx_padan_new = if_else(
        Overlap == "Yes",
        alpha * idx_serasi_new + (1 - alpha) * idx_padu_final,
        NA_real_
      ),
      delta_idx_padan = if_else(
        Overlap == "Yes",
        idx_padan_new - idx_padan,
        NA_real_
      )
    ) %>%
    select(-RZWP3K)
  
  # RZWP3K output
  rzwp3k_idx <- rzwp3k_final %>%
    left_join(
      lookup %>% select(id_pu, RTRW),
      by = "id_pu"
    ) %>%
    mutate(
      Overlap_Pair = if_else(
        Overlap == "Yes",
        RTRW,
        NA_character_
      ),
      idx_serasi_new = if_else(
        Overlap == "Yes",
        map2_dbl(RTRW, Zoning_New, get_compat),
        NA_real_
      ),
      idx_padan_new = if_else(
        Overlap == "Yes",
        alpha * idx_serasi_new + (1 - alpha) * idx_padu_final,
        NA_real_
      ),
      delta_idx_padan = if_else(
        Overlap == "Yes",
        idx_padan_new - idx_padan,
        NA_real_
      )
    ) %>%
    select(-RTRW)
  
  common_cols <- c(
    "id_pu", "Zoning_Old", "Zoning_New", "Overlap_Pair",
    "Overlap", "Reconcile",
    "idx_serasi", "idx_padu_final", "idx_padan",
    "idx_serasi_new", "idx_padan_new",
    "delta_idx_padan", "geometry"
  )
  
  rtrw_clean <- rtrw_idx %>%
    select(all_of(common_cols))
  
  rzwp3k_clean <- rzwp3k_idx %>%
    select(all_of(common_cols))
  
  list(
    rtrw = rtrw_clean,
    rzwp3k = rzwp3k_clean
  )
}