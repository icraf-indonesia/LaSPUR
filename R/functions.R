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
                              min_area_ha = 0,
                              nama_field_rtrw = "RTRW",
                              nama_field_rzwp = "RZWP3K",
                              batch_progress_interval = 250,
                              show_detailed_progress = TRUE) {
  
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
  
  # Identify touches
  message("Identifikasi area RTRW dan RZWP3K yang berdampingan.")
  pairs_idx <- sf::st_touches(rtrw_filter, rzwp_filter)
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
  
  # Convert dataframe to sf object – id kolom sudah berisi id_SRC
  message("Menggabungkan data frames dan mengkonversi ke objek sf.")
  combined_df <- dplyr::bind_rows(df_list)
  pu_sf <- sf::st_as_sf(combined_df, crs = sf::st_crs(rtrw_filter))
  
  # Urutkan kolom, tanpa menambah id baru
  pu_sf <- pu_sf %>%
    dplyr::select(id, id_pu, RTRW, RZWP3K, area_ha, geometry)
  
  return(invisible(pu_sf))
}

#' Process adjacent sf object
#'
#' This function processes adjacent polygons by calculating the shared boundary length
#' between RTRW and RZWP3K pairs, then computes buffer areas based on the specified
#' buffer distance.
#'
#' @param pu_sf The `sf` object returned by `identify_adjacent`. Must contain columns:
#'   `id_pu`, `RTRW`, `RZWP3K`, `area_ha`, and geometry.
#' @param buffer_m Numeric: The buffer distance in meters for the formula. This value
#'   is multiplied by the shared boundary length to calculate the buffer area.
#'
#' @return An `sf` object with the original columns plus two additional columns:
#'   \item{length}{The length of the shared boundary between RTRW and RZWP3K polygons (in meters)}
#'   \item{area_buffer_ha}{The calculated buffer area in hectares, computed as 
#'     (length * buffer_m) / 10000}
#'
#' @details The function performs the following steps:
#'   \enumerate{
#'     \item Transforms the input to a metric CRS (UTM) for accurate distance calculations
#'     \item Validates that RTRW and RZWP3K pairs have matching structure
#'     \item Extracts polygon boundaries
#'     \item Calculates shared boundary lengths between adjacent pairs
#'     \item Computes buffer areas based on the specified buffer distance
#'   }
#'
#' @note This function uses sequential processing. For large datasets, consider
#'   parallelizing manually using `future` and `furrr` packages if needed.
#'
#' @examples
#' \dontrun{
#'   result <- process_adjacent(adjacent_polygons, buffer_m = 50)
#' }
#'
#' @importFrom sf st_crs st_transform st_geometry st_boundary st_intersection st_length
#' @importFrom dplyr filter arrange left_join mutate select
#' @export
process_adjacent <- function(pu_sf, buffer_m) {
  
  # Automatically identify CRS
  current_crs <- sf::st_crs(pu_sf)
  if (current_crs$IsGeographic) {
    bbox <- sf::st_bbox(pu_sf)
    mean_lon <- (bbox[["xmin"]] + bbox[["xmax"]]) / 2
    mean_lat <- (bbox[["ymin"]] + bbox[["ymax"]]) / 2
    utm_zone <- floor((mean_lon + 180) / 6) + 1
    epsg_metric <- if (mean_lat >= 0) 32600 + utm_zone else 32700 + utm_zone
  } else {
    epsg_metric <- current_crs
  }
  pu_sf_metric <- sf::st_transform(pu_sf, epsg_metric)
  
  # Check id_pu structure 
  rtrw_parts <- pu_sf_metric %>% dplyr::filter(!is.na(RTRW)) %>% dplyr::arrange(id_pu)
  rzwp_parts <- pu_sf_metric %>% dplyr::filter(!is.na(RZWP3K)) %>% dplyr::arrange(id_pu)
  
  if (nrow(rtrw_parts) != nrow(rzwp_parts)) {
    stop("Ketidaksesuaian struktur pasangan data id_pu antara RTRW dan RZWP3K.")
  }
  
  # Extract edge geometry (wireframe)
  rtrw_boundaries <- sf::st_boundary(sf::st_geometry(rtrw_parts))
  rzwp_boundaries <- sf::st_boundary(sf::st_geometry(rzwp_parts))
  
  # Calculate length sequentially
  message("Menghitung irisan dan panjang garis setiap pasangan area berdampingan")
  calculated_lengths <- vapply(
    seq_along(rtrw_boundaries),
    function(i) {
      shared_line <- sf::st_intersection(rtrw_boundaries[i], rzwp_boundaries[i])
      return(as.numeric(sf::st_length(shared_line)))
    },
    numeric(1)
  )
  
  # Combine calculation result
  lengths_lookup <- data.frame(
    id_pu = rtrw_parts$id_pu,
    length = calculated_lengths,
    stringsAsFactors = FALSE
  )
  
  # Calculate buffer area
  message("Menghitung area buffer dalam hektar")
  pu_sf <- pu_sf %>% 
    dplyr::left_join(lengths_lookup, by = "id_pu") %>% 
    dplyr::mutate(
      area_buffer_ha = (length * buffer_m) / 10000
    )
  pu_sf <- pu_sf %>% 
    dplyr::select(id, id_pu, RTRW, RZWP3K, area_ha, length, area_buffer_ha, geometry)
  
  return(pu_sf)
}

# Perhitungan Indeks PADU-KE ----------------------------------------------

#' Generate a Compatibility Matrix (SERASI)
#'
#' @description
#' Creates a tibble matrix where unique classes from `sf_1` form rows and
#' unique classes from `sf_2` form columns.
#'
#' @param sf_1 `sf` object; first column used for row classes
#' @param sf_2 `sf` object; first column used for column headers
#' @param fill_value Initial value for matrix cells (default NA)
#'
#' @return A tibble with first column "RTRW_RZWP3K" and columns named after
#'   unique classes in `sf_2`
#'
#' @examples
#' result <- generate_matrix_serasi(rtrw_vect, rzwp3k_vect, fill_value = 0)
#'
#' @importFrom dplyr distinct pull
#' @importFrom sf st_drop_geometry
#' @importFrom tibble as_tibble add_column
#'
#' @export
generate_matrix_serasi <- function(sf_1, sf_2, fill_value = NA) {
  rows <- sf_1 |> 
    sf::st_drop_geometry() |> 
    dplyr::distinct(dplyr::across(1)) |> 
    dplyr::pull(1)
  
  cols <- sf_2 |> 
    sf::st_drop_geometry() |> 
    dplyr::distinct(dplyr::across(1)) |> 
    dplyr::pull(1)
  
  mat <- matrix(fill_value, nrow = length(rows), ncol = length(cols))
  colnames(mat) <- cols
  
  matrix_tibble <- tibble::as_tibble(mat) |>
    tibble::add_column(RTRW_RZWP3K = rows, .before = 1)
  
  return(matrix_tibble)
}

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
#' unique class names from the second column of input table.
#'
#' @param tbl Data frame with at least 2 columns; second column contains class names
#' @param fill_value Value to fill matrix cells (default NA)
#'
#' @return A tibble with first column "class" and remaining columns named after classes
#'
#' @examples
#' \dontrun{
#' class_table <- tibble::tribble(
#'   ~id, ~class_name,
#'   1,   "Hutan Lindung",
#'   2,   "Kawasan Permukiman"
#' )
#' mat_na <- generate_matrix_padu_ke(class_table)
#' mat_zero <- generate_matrix_padu_ke(class_table, fill_value = 0)
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
#'
#' @export
generate_matrix_padu_ke <- function(tbl, fill_value = NA) {
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
  
  result <- tibble::as_tibble(mat) |>
    dplyr::mutate(class = rownames(mat), .before = 1)
  
  return(result)
}

safe_extract_polygons <- function(x) {
  geom_types <- sf::st_geometry_type(x)
  poly_idx <- which(geom_types %in% c("POLYGON", "MULTIPOLYGON"))
  if (length(poly_idx) == 0) return(NULL)
  x <- x[poly_idx, ]
  if (any(sf::st_geometry_type(x) == "GEOMETRYCOLLECTION")) {
    x <- sf::st_collection_extract(x, "POLYGON")
    if (is.null(x) || nrow(x) == 0) return(NULL)
  }
  return(x)
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
  
  output_df <- furrr::future_map_dfr(
    admin_ids,
    function(uid) {
      poly <- admin_sf[admin_sf[[id_col]] == uid, ]
      
      lulc_clip <- tryCatch(sf::st_intersection(lulc, poly), error = function(e) {
        warning("Admin unit ", uid, " error: ", e$message)
        return(NULL)
      })
      if (is.null(lulc_clip) || nrow(lulc_clip) == 0) return(NULL)
      
      if (!all(sf::st_is_valid(lulc_clip))) {
        lulc_clip <- sf::st_make_valid(lulc_clip) |> sf::st_buffer(dist = 0)
      }
      
      lulc_clip <- safe_extract_polygons(lulc_clip)
      if (is.null(lulc_clip) || nrow(lulc_clip) == 0) return(NULL)
      
      lulc_clip$class_code <- as.character(lulc_clip[[class_col]])
      
      s2_was_on <- sf::sf_use_s2()
      if (s2_was_on) sf::sf_use_s2(FALSE)
      
      touches_list <- tryCatch(sf::st_touches(lulc_clip, lulc_clip), error = function(e) {
        warning("Admin unit ", uid, " touches error: ", e$message)
        return(NULL)
      })
      
      if (s2_was_on) sf::sf_use_s2(TRUE)
      if (is.null(touches_list)) return(NULL)
      
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
      
      if (nrow(pair_counts) == 0) return(NULL)
      
      adj_df <- pair_counts |>
        dplyr::group_by(Class_A, Class_B) |>
        dplyr::summarise(Edge_Count = dplyr::n(), .groups = "drop")
      
      total_adj <- sum(adj_df$Edge_Count)
      adj_df |>
        dplyr::mutate(
          id_pu = as.character(uid),
          percentage = (Edge_Count / total_adj) * 100
        )
    },
    .progress = progress,
    .options = furrr::furrr_options(
      packages = c("sf", "dplyr")
    )
  )
  
  if (is.null(output_df) || nrow(output_df) == 0) {
    warning("No adjacency data found.")
    return(data.frame())
  }
  
  return(output_df[, c("id_pu", "Class_A", "Class_B", "Edge_Count", "percentage")])
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
    summarise(idx_padu_ke_abs = sum(weighted, na.rm = TRUE), .groups = "drop")
  
  if (normalize) {
    max_val <- max(matriks_padu_ke_id$adj_index, na.rm = TRUE)
    idx_padu_ke <- idx_padu_ke %>%
      mutate(idx_padu_ke = idx_padu_ke_abs / (max_val * 100))
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
calculate_euclidean_dist <- function(vector_obj, pu, resolution = 100) {
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
  
  # Clip vector_obj by pu 
  vector_clipped <- sf::st_intersection(vector_obj, pu)
  vector_clipped <- handle_geom_collection(vector_clipped)
  vector_clipped <- vector_clipped[!sf::st_is_empty(vector_clipped), ]
  
  if (nrow(vector_clipped) == 0) {
    stop("After intersection, no part of vector_obj overlaps pu")
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
#' polygon. It then combines the two extracted variables:
#' \deqn{filter\_estuari = 1 - \min(estuari\_dist\_mean / max\_dist, 1)}
#' \deqn{idx\_padu\_hs = (filter\_estuari + tss\_mean) / 2}
#' Missing values in either component propagate as `NA` in the final index.
#'
#' @examples
#' \dontrun{
#' result <- calculate_padu_hs(idx_serasi_map, estuari_euc_dist, tss_rast)
#' hs_map <- result$idx_padu_hs_map
#' hs_tbl <- result$idx_padu_hs
#' }
#'
#' @importFrom dplyr left_join select mutate
#' @importFrom sf st_drop_geometry
#' @importFrom tibble as_tibble
#' @export
calculate_padu_hs <- function(idx_serasi_map,
                              estuari_euc_dist,
                              tss_rast,
                              id_col = "id_pu",
                              max_dist = 5000) {
  
  # Extract estuarine distance
  estuari_dist_extracted <- extract_raster_to_sf(
    idx_serasi_map,
    estuari_euc_dist,
    id_col = id_col,
    new_col = "estuari_dist_mean"
  )
  
  # Extract TSS
  tss_extracted <- extract_raster_to_sf(
    idx_serasi_map,
    tss_rast,
    id_col = id_col,
    new_col = "tss_mean"
  )
  
  # Drop geometry from TSS for joining
  tss_to_merge <- tss_extracted %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::all_of(c(id_col, "tss_mean")))
  
  # Join and calculate PADU-HS
  idx_padu_hs_map <- estuari_dist_extracted %>%
    dplyr::left_join(tss_to_merge, by = id_col) %>%
    dplyr::mutate(
      filter_estuari = ifelse(
        is.na(.data$estuari_dist_mean),
        NA,
        1 - pmin(.data$estuari_dist_mean / max_dist, 1)
      ),
      idx_padu_hs = (.data$filter_estuari + .data$tss_mean) / 2
    ) %>%
    dplyr::select(-dplyr::all_of("filter_estuari"))
  
  # Create geometry‑free tibble
  idx_padu_hs <- tibble::as_tibble(
    idx_padu_hs_map %>% sf::st_drop_geometry()
  )
  
  # Return as a list
  list(
    idx_padu_hs_map = idx_padu_hs_map,
    idx_padu_hs     = idx_padu_hs
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
    .options = furrr::furrr_options(packages = "sf")
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
  
  # Extract disaster risk values to overlap unit
  disaster_risk_extracted <- extract_sf_to_sf(
    pu = idx_serasi_map,
    value_sf = disaster_risk_vect,
    value_col = value_col,
    new_col = new_col,
    pu_id = pu_id,
    parallel = parallel,
    workers = workers
  )
  
  # Calculate PADU-KI index
  idx_padu_ki_map <- disaster_risk_extracted %>%
    dplyr::mutate(
      idx_padu_ki = dplyr::if_else(
        is.na(.data[[new_col]]),
        NA_real_,
        1 - .data[[new_col]]
      )
    ) %>%
    dplyr::select(-dplyr::all_of(new_col))
  
  # Create geometry‑free tibble
  idx_padu_ki <- tibble::as_tibble(
    idx_padu_ki_map %>% sf::st_drop_geometry()
  )
  
  # Return as a list
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

#' Determine alternative zone using compatibility matrix
#'
#' @description
#' Given a pair zone (e.g., RZWP3K or RTRW) and a current zone value, this function
#' returns either the best‑matching or second‑best‑matching zone from the opposite
#' classification system, based on a compatibility matrix. If the current zone
#' equals the best match, the second best is returned; otherwise the best match is
#' returned.
#'
#' @param pair_zone Character: the zone value used as the filter criterion.
#'   For `return_type = "RTRW"`, this should be an RZWP3K value.
#'   For `return_type = "RZWP3K"`, this should be an RTRW value.
#' @param current_zone Character: the current value of the zone type we are
#'   trying to replace. Used for comparison with the best match.
#' @param return_type Character: either `"RTRW"` or `"RZWP3K"`.
#'   - `"RTRW"`: filter by `class2` (RZWP3K), return a `class1` (RTRW) zone.
#'   - `"RZWP3K"`: filter by `class1` (RTRW), return a `class2` (RZWP3K) zone.
#' @param df A data frame (or tibble) containing the compatibility matrix with
#'   exactly three columns in this order:
#'   \enumerate{
#'     \item `class1` – RTRW zone names
#'     \item `class2` – RZWP3K zone names
#'     \item `idx_serasi` – numeric compatibility scores
#'   }
#'
#' @return A character string with the chosen alternative zone name, or `NA`
#'   if no valid alternative exists (e.g., input missing, no data, or no second
#'   best when needed).
#'
#' @examples
#' \dontrun{
#' # Example compatibility matrix (first few rows)
#' mat <- tibble::tribble(
#'   ~class1,                          ~class2,                  ~idx_serasi,
#'   "Kawasan Lindung",                "Suaka",                  1.0,
#'   "Kawasan Perikanan",              "Suaka",                  0.5,
#'   "Kawasan Lindung",                "Taman",                  0.8,
#'   "Kawasan Perikanan",              "Taman",                  1.0
#' )
#'
#' # Alternative RTRW for a polygon with RZWP3K = "Suaka" and current RTRW = "Kawasan Lindung"
#' get_alternative_zone("Suaka", "Kawasan Lindung", "RTRW", mat)
#' # Returns "Kawasan Perikanan" (second best)
#'
#' # Alternative RZWP3K for a polygon with RTRW = "Kawasan Lindung" and current RZWP3K = "Suaka"
#' get_alternative_zone("Kawasan Lindung", "Suaka", "RZWP3K", mat)
#' # Returns "Taman" (best match because "Suaka" is already best? Actually "Suaka" has score 1.0,
#' # which equals current, so second best "Taman" is returned)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#'
#' @export
get_alternative_zone <- function(pair_zone, current_zone, return_type, df) {
  
  if (is.na(pair_zone) || pair_zone == "") return(NA_character_)
  
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
  if (nrow(filtered) == 0) return(NA_character_)
  
  scores <- filtered[[3]]          # idx_serasi
  candidates <- filtered[[return_col]]
  
  max_score <- max(scores, na.rm = TRUE)
  best_idx <- which(scores == max_score)[1]
  best_zone <- candidates[best_idx]
  
  if (!is.na(current_zone) && best_zone == current_zone) {
    # Second best
    sorted_scores <- sort(scores, decreasing = TRUE)
    second_score <- sorted_scores[2]
    if (is.na(second_score)) return(NA_character_)
    second_idx <- which(scores == second_score)[1]
    return(candidates[second_idx])
  } else {
    return(best_zone)
  }
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
#'
#' @return The function is called for its side effect of creating an Excel file.
#'   It returns `NULL` invisibly.
#'
#' @details The Excel workbook contains two sheets:
#' \itemize{
#'   \item **Data**: Contains all columns from `recon_map` (with geometry
#'     removed) plus decision column(s) at the end. Data validation is applied
#'     to those columns.
#'   \item **Lists**: Holds the valid option lists. For both steps, column A
#'     holds RTRW options and column B holds RZWP3K options. When `step = 1`,
#'     column C holds the combined list used for the `user_decision` dropdown.
#' }
#'
#' The function requires the `openxlsx` and `sf` packages to be installed. It
#' checks for their presence and stops with an error if they are missing.
#'
#' @examples
#' \dontrun{
#' # Step 2 (two separate decisions)
#' generate_reconciliation_excel(recon_sf, rtrw_prior, rzp3k_prior, 
#'                               step = 2, output_dir = "out")
#'
#' # Step 1 (single combined decision)
#' generate_reconciliation_excel(recon_sf, rtrw_prior, rzp3k_prior,
#'                               step = 1, output_dir = "out")
#' }
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData dataValidation saveWorkbook
#' @importFrom sf st_drop_geometry
#' @export
generate_reconciliation_excel <- function(recon_map, 
                                          rtrw_prioritas, 
                                          rzwp3k_prioritas, 
                                          output_dir, 
                                          step, 
                                          file_name = "recon_map.xlsx") {
  
  # Validate step argument
  if (missing(step) || !(step %in% c(1, 2))) {
    stop("'step' must be explicitly provided and must be either 1 or 2.")
  }
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("package 'openxlsx' is required.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("package 'sf' is required.")
  
  df_flat <- sf::st_drop_geometry(recon_map)
  
  if (step == 2) {
    # Two separate decision columns
    df_flat$decision_rtrw   <- NA_character_
    df_flat$decision_rzwp3k <- NA_character_
  } else { # step == 1
    # Single combined decision column
    df_flat$user_decision <- NA_character_
  }
  
  # Extract option lists (remove NAs)
  rtrw_opts   <- as.character(rtrw_prioritas$RTRW)
  rzwp3k_opts <- as.character(rzwp3k_prioritas$RZWP3K)
  rtrw_opts   <- rtrw_opts[!is.na(rtrw_opts)]
  rzwp3k_opts <- rzwp3k_opts[!is.na(rzwp3k_opts)]
  
  # Create Workbook and Sheets
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Data")
  openxlsx::addWorksheet(wb, "Lists")
  
  # Write main data
  openxlsx::writeData(wb, "Data", df_flat)
  
  # Write option lists to the Lists sheet
  # Column A: RTRW options
  openxlsx::writeData(wb, "Lists", x = "RTRW Options", startCol = 1, startRow = 1)
  if (length(rtrw_opts) > 0) {
    openxlsx::writeData(wb, "Lists", x = rtrw_opts, startCol = 1, startRow = 2, colNames = FALSE)
  }
  
  # Column B: RZWP3K options
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
    
    # Find the column index of 'user_decision'
    col_decision <- which(names(df_flat) == "user_decision")
    if (length(col_decision) == 0) stop("Column 'user_decision' not found in data frame.")
    rows <- 2:(nrow(df_flat) + 1)
    last_row_combined <- length(combined_opts) + 1  # +1 for header
    formula_combined <- paste0("=Lists!$C$2:$C$", last_row_combined)
    
    openxlsx::dataValidation(wb, "Data",
                             col = col_decision,
                             rows = rows,
                             type = "list",
                             value = formula_combined)
  } else {
    # step == 2: apply validations for both decision columns
    col_rtrw   <- which(names(df_flat) == "decision_rtrw")
    col_rzwp3k <- which(names(df_flat) == "decision_rzwp3k")
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
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save workbook
  full_path <- file.path(output_dir, file_name)
  openxlsx::saveWorkbook(wb, full_path, overwrite = TRUE)
  
  message(paste("Workbook successfully saved to:", full_path))
  invisible(NULL)
}

#' Reconcile a land-use class column with decisions from an Excel reconciliation table
#'
#' Overwrites the specified class column (`RTRW` or `RZWP3K`) in an `sf` object
#' using the corresponding `decision_*` column from the Excel file. Only rows with
#' an `id` that appears in the Excel table and a non‑empty decision are updated.
#' Rows without a matching `id` or with an empty decision remain unchanged.
#'
#' @param sf_obj An `sf` object that must contain an integer column `id` and
#'   either a `RTRW` or `RZWP3K` column (as determined by `class_type`).
#' @param xlsx_path Path to the Excel file. The file must have a sheet named
#'   `"Data"` containing columns `id`, `decision_rtrw`, and `decision_rzwp3k`.
#' @param class_type Either `"RTRW"` or `"RZWP3K"`; determines which column is
#'   updated and which decision column is used.
#'
#' @return The input `sf` object with the specified class column overwritten by
#'   the reconciled values. All other columns and the geometry are unchanged.
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' rtrw <- reconcile_map(rtrw, "reconciliation.xlsx", "RTRW")
#' rz <- reconcile_map(rz, "reconciliation.xlsx", "RZWP3K")
#' }
reconcile_map <- function(sf_obj, xlsx_path, class_type = c("RTRW", "RZWP3K")) {
  class_type <- match.arg(class_type)
  
  decision_col <- paste0("decision_", tolower(class_type))
  orig_col <- class_type
  
  xlsx_data <- read_excel(xlsx_path, sheet = "Data")
  
  update_data <- xlsx_data %>%
    filter(!is.na(.data[[decision_col]]) & .data[[decision_col]] != "")
  
  if (!orig_col %in% names(sf_obj)) {
    stop("Column '", orig_col, "' not found in the input sf object.")
  }
  
  new_vals <- sf_obj[[orig_col]]
  
  for (i in seq_len(nrow(update_data))) {
    id_val <- update_data$id[i]
    new_val <- update_data[[decision_col]][i]
    match_idx <- which(sf_obj$id == id_val)
    if (length(match_idx) > 0) {
      new_vals[match_idx] <- new_val
    }
  }
  
  sf_obj[[orig_col]] <- new_vals
  return(sf_obj)
}

#' Reconcile overlapping polygons using user decisions and priority lists
#'
#' @param union An `sf` object with columns: id_pu, stat_pu, RTRW, RZWP3K,
#'   id_rtrw, id_rzwp3k, geometry.
#' @param recon_table A data frame with columns: id_rtrw, id_rzwp3k, user_decision.
#' @param rtrw_prioritas A data frame with a column `"RTRW"` containing valid
#'   class names for RTRW (non‑NA values used).
#' @param rzwp3k_prioritas A data frame with a column `"RZWP3K"` containing valid
#'   class names for RZWP3K (non‑NA values used).
#'
#' @return An `sf` object with additional columns `final_class` and
#'   `stat_pu_final` (never `"intersection"`).
#'
#' @importFrom sf st_as_sf
#' @importFrom dplyr left_join mutate case_when filter
#' @export
reconcile_map_overlap <- function(union, recon_table,
                                  rtrw_prioritas, rzwp3k_prioritas) {
  # Check packages
  if (!requireNamespace("sf", quietly = TRUE)) stop("package 'sf' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("package 'dplyr' is required.")
  
  # Check required columns
  required_union <- c("id_pu", "stat_pu", "RTRW", "RZWP3K", "id_rtrw", "id_rzwp3k")
  if (!all(required_union %in% colnames(union))) {
    stop("'union' must contain columns: ", paste(required_union, collapse = ", "))
  }
  required_recon <- c("id_rtrw", "id_rzwp3k", "user_decision")
  if (!all(required_recon %in% colnames(recon_table))) {
    stop("'recon_table' must contain columns: ", paste(required_recon, collapse = ", "))
  }
  
  # Extract priority lists (remove NAs)
  rtrw_classes <- as.character(rtrw_prioritas$RTRW)
  rtrw_classes <- rtrw_classes[!is.na(rtrw_classes)]
  rzwp3k_classes <- as.character(rzwp3k_prioritas$RZWP3K)
  rzwp3k_classes <- rzwp3k_classes[!is.na(rzwp3k_classes)]
  
  all_classes <- unique(c(rtrw_classes, rzwp3k_classes))
  source_map <- setNames(
    sapply(all_classes, function(cls) {
      in_rtrw <- cls %in% rtrw_classes
      in_rzwp3k <- cls %in% rzwp3k_classes
      if (in_rtrw && in_rzwp3k) "BOTH"
      else if (in_rtrw) "RTRW"
      else if (in_rzwp3k) "RZWP3K"
      else NA_character_
    }),
    all_classes
  )
  
  # Convert join keys to character
  union <- dplyr::mutate(union,
                         id_rtrw = as.character(id_rtrw),
                         id_rzwp3k = as.character(id_rzwp3k))
  recon_table <- dplyr::mutate(recon_table,
                               id_rtrw = as.character(id_rtrw),
                               id_rzwp3k = as.character(id_rzwp3k))
  
  # Join user_decision
  union_with_decision <- dplyr::left_join(
    union,
    recon_table[, c("id_rtrw", "id_rzwp3k", "user_decision")],
    by = c("id_rtrw", "id_rzwp3k")
  )
  
  # Warn about missing decisions for intersection polygons
  missing_dec <- dplyr::filter(union_with_decision,
                               stat_pu == "intersection" & is.na(user_decision))
  if (nrow(missing_dec) > 0) {
    warning("Intersection polygons with missing user_decision (id_pu = ",
            paste(missing_dec$id_pu, collapse = ", "), ") will have NA in final_class and stat_pu_final.")
  }
  
  final_sf <- union_with_decision %>%
    dplyr::mutate(
      final_class = dplyr::case_when(
        stat_pu == "intersection" ~ user_decision,
        stat_pu == "RTRW" ~ RTRW,
        stat_pu == "RZWP3K" ~ RZWP3K,
        TRUE ~ NA_character_
      ),
      stat_pu_final = dplyr::case_when(
        stat_pu == "RTRW" ~ "RTRW",
        stat_pu == "RZWP3K" ~ "RZWP3K",
        stat_pu == "intersection" & is.na(user_decision) ~ NA_character_,
        stat_pu == "intersection" & !is.na(user_decision) ~ source_map[user_decision],
        TRUE ~ NA_character_
      )
    )
  
  both_cases <- dplyr::filter(final_sf,
                              stat_pu == "intersection" & stat_pu_final == "BOTH")
  if (nrow(both_cases) > 0) {
    warning("Some intersection polygons have decisions that appear in BOTH priority lists (id_pu = ",
            paste(both_cases$id_pu, collapse = ", "), "). Set to 'BOTH'.")
  }
  neither_cases <- dplyr::filter(final_sf,
                                 stat_pu == "intersection" & is.na(stat_pu_final) & !is.na(user_decision))
  if (nrow(neither_cases) > 0) {
    warning("Some intersection polygons have decisions that appear in NEITHER priority list (id_pu = ",
            paste(neither_cases$id_pu, collapse = ", "), "). Set to NA.")
  }
  
  # Return as sf
  sf::st_as_sf(final_sf)
}
