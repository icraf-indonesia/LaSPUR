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
#' @importFrom dplyr mutate select
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
  
  x_v <- terra::vect(x) |> terra::makeValid()
  y_v <- terra::vect(y) |> terra::makeValid()
  
  if (!terra::same.crs(x_v, y_v)) {
    warning("CRS differ. Reprojecting y to the CRS of x.")
    y_v <- terra::project(y_v, terra::crs(x_v))
  }
  
  # Intersection 
  intersect_v <- terra::intersect(x_v, y_v)
  
  if (terra::nrow(intersect_v) == 0) {
    result <- sf::st_sf(geometry = sf::st_sfc(), crs = sf::st_crs(terra::crs(x_v)))
  } else {
    result <- sf::st_as_sf(intersect_v)
    result$stat_pu <- "intersection"
  }

  all_cols <- unique(c(names(x_v), names(y_v), "stat_pu"))
  for (col in all_cols) if (!col %in% names(result)) result[[col]] <- NA
  
  # Remove empty geometries
  result <- result[!sf::st_is_empty(result), ]
  
  # Add sequential ID and reorder columns if there are results
  if (nrow(result) > 0) {
    result <- result[, c(all_cols[all_cols != "geometry"], "geometry")]
    result <- result |> 
      dplyr::mutate(id_pu = dplyr::row_number()) |>
      dplyr::select(id_pu, stat_pu, dplyr::everything())
  } else {
    result$id_pu <- integer(0)
  }
  
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
#' result <- identify_overlaps(poly1, poly2)
#' overlaps <- result[result$stat_pu == "intersection", ]
#' }
#'
#' @importFrom sf st_geometry_type st_as_sf st_sfc st_crs st_is_empty
#' @importFrom terra vect makeValid same.crs project intersect erase nrow crs
#' @importFrom dplyr bind_rows mutate select
#'
#' @export
# identify_overlaps <- function(x, y) {
#   if (!inherits(x, "sf")) stop("x must be an sf object")
#   if (!inherits(y, "sf")) stop("y must be an sf object")
#   
#   geom_type <- c("POLYGON", "MULTIPOLYGON")
#   if (!all(sf::st_geometry_type(x, by_geometry = FALSE) %in% geom_type))
#     stop("x must contain polygons or multipolygons")
#   if (!all(sf::st_geometry_type(y, by_geometry = FALSE) %in% geom_type))
#     stop("y must contain polygons or multipolygons")
#   
#   x_v <- terra::vect(x) |> terra::makeValid()
#   y_v <- terra::vect(y) |> terra::makeValid()
#   
#   if (!terra::same.crs(x_v, y_v)) {
#     warning("CRS differ. Reprojecting y to the CRS of x.")
#     y_v <- terra::project(y_v, terra::crs(x_v))
#   }
#   
#   # Intersection
#   intersect_v <- terra::intersect(x_v, y_v)
#   if (terra::nrow(intersect_v) == 0) {
#     intersect_sf <- sf::st_sf(geometry = sf::st_sfc(), crs = sf::st_crs(terra::crs(x_v)))
#   } else {
#     intersect_sf <- sf::st_as_sf(intersect_v)
#     intersect_sf$stat_pu <- "intersection"
#   }
#   
#   # X only
#   x_only_v <- terra::erase(x_v, y_v)
#   if (terra::nrow(x_only_v) == 0) {
#     x_only_sf <- sf::st_sf(geometry = sf::st_sfc(), crs = sf::st_crs(terra::crs(x_v)))
#   } else {
#     x_only_sf <- sf::st_as_sf(x_only_v)
#     x_only_sf$stat_pu <- as.character(names(x)[1])
#     y_attr <- setdiff(names(y_v), names(x_only_sf))
#     for (col in y_attr) x_only_sf[[col]] <- NA
#   }
#   
#   # Y only
#   y_only_v <- terra::erase(y_v, x_v)
#   if (terra::nrow(y_only_v) == 0) {
#     y_only_sf <- sf::st_sf(geometry = sf::st_sfc(), crs = sf::st_crs(terra::crs(x_v)))
#   } else {
#     y_only_sf <- sf::st_as_sf(y_only_v)
#     y_only_sf$stat_pu <- as.character(names(y)[1])
#     x_attr <- setdiff(names(x_v), names(y_only_sf))
#     for (col in x_attr) y_only_sf[[col]] <- NA
#   }
#   
#   # Combine
#   result <- dplyr::bind_rows(x_only_sf, y_only_sf, intersect_sf)
#   all_cols <- unique(c(names(x_v), names(y_v), "stat_pu"))
#   for (col in all_cols) if (!col %in% names(result)) result[[col]] <- NA
#   
#   result <- result[, c(all_cols[all_cols != "geometry"], "geometry")]
#   result <- result[!sf::st_is_empty(result), ]
#   result <- result |> 
#     dplyr::mutate(id_pu = dplyr::row_number()) |>
#     dplyr::select(id_pu, stat_pu, dplyr::everything())
#   
#   return(result)
# }

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
#' values from reference tibbles.
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
  
  col3_values <- sf_obj[[3]]
  col4_values <- sf_obj[[4]]
  allowed1 <- unique(tibble1[[2]])
  allowed2 <- unique(tibble2[[2]])
  mismatches3 <- unique(col3_values[!col3_values %in% allowed1])
  mismatches4 <- unique(col4_values[!col4_values %in% allowed2])
  
  cat("\n=== Zone Class Validation Report ===\n")
  if (length(mismatches3) == 0) {
    cat("✓ Column 3 (", names(sf_obj)[3], ") : All classes match.\n", sep = "")
  } else {
    cat("✗ Column 3 (", names(sf_obj)[3], ") : Mismatches:\n", sep = "")
    for (val in mismatches3) cat("    - '", val, "'\n", sep = "")
  }
  if (length(mismatches4) == 0) {
    cat("✓ Column 4 (", names(sf_obj)[4], ") : All classes match.\n", sep = "")
  } else {
    cat("✗ Column 4 (", names(sf_obj)[4], ") : Mismatches:\n", sep = "")
    for (val in mismatches4) cat("    - '", val, "'\n", sep = "")
  }
  cat("===================================\n")
  
  invisible(list(mismatch_col3 = mismatches3, mismatch_col4 = mismatches4))
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
  
  rtrw_lookup <- lookup_table[[1]]
  rzpw_lookup <- lookup_table[[2]]
  compat_lookup <- lookup_table[[3]]
  sf_col_names <- names(sf_obj)
  rtrw_col <- sf_col_names[3]
  rzpw_col <- sf_col_names[4]
  
  sf_keys <- sf::st_drop_geometry(sf_obj)[, c(rtrw_col, rzpw_col)]
  sf_keys$key <- paste(sf_keys[[1]], sf_keys[[2]], sep = "||")
  lookup_keys <- paste(rtrw_lookup, rzpw_lookup, sep = "||")
  compat_vec <- compat_lookup
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

#' Calculate LULC adjacency matrix by administrative unit
#'
#' @description
#' For raster LULC: pairwise edge counts between cells (4-directional rook's case).
#' For vector LULC: pairwise counts of touching polygons (queen's case).
#'
#' @param lulc Categorical `SpatRaster` or `sf` polygon object
#' @param admin_vector `sf` object with administrative boundaries
#' @param id_col Column name in `admin_vector` with unique identifiers
#' @param class_col For vector LULC only: column name with LULC class codes
#'
#' @return `data.frame` with columns: `id_pu`, `Class_A`, `Class_B`, `Edge_Count`, `percentage`
#'
#' @examples
#' \dontrun{
#' # Raster LULC
#' lulc_rast <- rast("landcover.tif")
#' admin <- st_read("units.shp")
#' res1 <- calculate_lulc_adjacency(lulc_rast, admin, id_col = "id")
#'
#' # Vector LULC
#' lulc_sf <- st_read("lulc_polygons.gpkg")
#' res2 <- calculate_lulc_adjacency(lulc_sf, admin, id_col = "id", class_col = "PL2024_ID")
#' }
#'
#' @export
calculate_lulc_adjacency <- function(lulc, admin_vector, id_col = "id_pu", class_col = NULL) {
  UseMethod("calculate_lulc_adjacency")
}

#' @export
calculate_lulc_adjacency.SpatRaster <- function(lulc, admin_vector, id_col = "id_pu", class_col = NULL) {
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
  
  output_df <- purrr::map_df(admin_list, function(poly) {
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
  })
  
  if (is.null(output_df) || nrow(output_df) == 0) {
    warning("No adjacency data found.")
    return(data.frame())
  }
  
  return(output_df[, c("id_pu", "Class_A", "Class_B", "Edge_Count", "percentage")])
}

#' @export
calculate_lulc_adjacency.sf <- function(lulc, admin_vector, id_col = "id_pu", class_col = NULL) {
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
  
  admin_ids <- unique(admin_sf[[id_col]])
  
  output_df <- purrr::map_df(admin_ids, function(uid) {
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
  })
  
  if (is.null(output_df) || nrow(output_df) == 0) {
    warning("No adjacency data found.")
    return(data.frame())
  }
  
  return(output_df[, c("id_pu", "Class_A", "Class_B", "Edge_Count", "percentage")])
}

#' Calculate PADU-KE (Weighted LULC Adjacency Index)
#'
#' @description
#' Computes PADU-KE index as weighted sum of adjacency strength between LULC
#' class pairs within each administrative unit.
#'
#' @param adjacency_df Data frame with columns: `id_pu`, `Class_A`, `Class_B`,
#'   `Edge_Count`, `percentage`
#' @param index_matrix Data frame with columns: `class_id1`, `class_id2`, `adj_index`
#' @param normalize Logical; if TRUE scales index to 0-1 range
#'
#' @return Data frame with `id_pu`, `idx_padu_ke_abs`, and optionally `idx_padu_ke`
#'
#' @examples
#' \dontrun{
#' result <- calculate_padu_ke(adjacency_df, index_matrix, normalize = TRUE)
#' }
#'
#' @importFrom dplyr left_join filter mutate group_by summarise
#'
#' @export
calculate_padu_ke <- function(adjacency_df, index_matrix, normalize = TRUE) {
  required_adj <- c("id_pu", "Class_A", "Class_B", "percentage")
  required_idx <- c("class_id1", "class_id2", "adj_index")
  
  missing_adj <- setdiff(required_adj, names(adjacency_df))
  missing_idx <- setdiff(required_idx, names(index_matrix))
  
  if (length(missing_adj) > 0) {
    stop("adjacency_df missing: ", paste(missing_adj, collapse = ", "))
  }
  if (length(missing_idx) > 0) {
    stop("index_matrix missing: ", paste(missing_idx, collapse = ", "))
  }
  
  df <- adjacency_df |>
    dplyr::left_join(index_matrix,
                     by = c("Class_A" = "class_id1", "Class_B" = "class_id2"))
  
  unmatched <- sum(is.na(df$adj_index))
  if (unmatched > 0) warning(unmatched, " class pair(s) missing adj_index.")
  
  result <- df |>
    dplyr::filter(!is.na(adj_index)) |>
    dplyr::mutate(weighted = percentage * adj_index) |>
    dplyr::group_by(id_pu) |>
    dplyr::summarise(idx_padu_ke_abs = sum(weighted, na.rm = TRUE), .groups = "drop")
  
  if (normalize) {
    max_val <- max(index_matrix$adj_index, na.rm = TRUE)
    result <- result |>
      dplyr::mutate(idx_padu_ke = idx_padu_ke_abs / (max_val * 100))
  }
  
  return(result)
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
#' (1 hectare = 10,000 m²). Results are rounded to two decimal places.
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
#'
#' @export
calculate_overlay_pct <- function(pu, overlay_area, title = "overlay_area"){
  
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
  
  # Calculate overlap only for intersecting PU
  for (i in intersecting_pu) {
    intersection <- st_intersection(pu[i, ], overlay_area)
    
    if (nrow(intersection) > 0) {
      overlap_area_ha <- sum(as.numeric(st_area(intersection))) / 10000 # Convert m2 to ha
      pu[[ha_col]][i] <- overlap_area_ha
      pu[[pct_col]][i] <- (overlap_area_ha / pu$area_ha[i]) * 100
    }
  }
  return(pu)
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
#' @param padu_list A list of `sf` objects containing individual PADU indices.
#'   Each object must include `id_pu` and one column matching pattern `idx_padu_*`.
#' @param idx_padu_map An `sf` object serving as the base spatial layer (e.g., planning units),
#'   containing at least the `id_pu` column.
#' @param padu_idx_weight A `data.frame` or tibble with at least two columns:
#'   the first column representing index codes (e.g., "KE", "HS") and the second column
#'   representing corresponding weights.
#'
#' @return An `sf` object with all joined `idx_padu_*` columns and an additional column:
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
  # Join all PADU indices
  idx_padu_map <- reduce(
    padu_list,
    .init = idx_padu_map,
    .f = function(x, y) {
      
      idx_col <- names(y)[grepl("^idx_padu_[a-z]+$", names(y))]
      
      y_clean <- y %>%
        st_drop_geometry() %>%
        mutate(id_pu = as.integer(id_pu)) %>%
        select(id_pu, all_of(idx_col))
      
      left_join(x, y_clean, by = "id_pu")
    }
  )
  
  # Prepare weights 
  weights <- padu_idx_weight %>%
    mutate(
      code  = tolower(.[[1]]),
      value = .[[2]]
    )
  
  # Detect index columns
  idx_cols <- names(idx_padu_map)[grepl("^idx_padu_", names(idx_padu_map))]
  idx_code <- stringr::str_remove(idx_cols, "idx_padu_")
  
  # Count available indices
  n_idx <- length(idx_cols)
  
  if (n_idx < 7) {
    message(paste0(
      "Only ", n_idx, " PADU indices detected. ",
      "Using simple average instead of weighted calculation."
    ))
  } else {
    message("All 7 PADU indices detected. Using weighted calculation.")
  }
  
  # Calculate final index
  idx_padu_map <- idx_padu_map %>%
    rowwise() %>%
    mutate(
      idx_padu_final = if (n_idx < 7) {
        mean(c_across(all_of(idx_cols)), na.rm = TRUE)
      } else {
        sum(
          c_across(all_of(idx_cols)) *
            weights$value[match(idx_code, weights$code)],
          na.rm = TRUE
        )
      }
    ) %>%
    ungroup()
  
  return(idx_padu_map)
}

# Perhitungan Indeks PADAN ------------------------------------------------

# 9. calculate_padan()
# 10. calculate_recommendation()

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
