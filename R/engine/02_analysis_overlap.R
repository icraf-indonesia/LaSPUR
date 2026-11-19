# R/engine/02_analysis_overlap.R
# ============================================================
# Overlap Analysis, Sliver Filtering & Spatial Intersection
# ============================================================
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

#' Process overlap results: add IDs, area, size flag, and optional administrative partitioning
#'
#' @description
#' Adds sequential ID, calculates area in hectares, and flags features based
#' on area threshold. Automatically reprojects to appropriate UTM zone.
#' If administrative boundaries are provided, cleanly partitions parcels along
#' administrative borders into singlepart polygons.
#'
#' @param sf_obj An `sf` object with polygon geometry containing a `stat_pu` column
#' @param threshold_ha Numeric threshold for area flag (default 156.25)
#' @param min_sliver_ha Minimum area threshold in hectares to retain polygons (default 1.0)
#' @param admin_sf Optional `sf` object with administrative boundary polygons (default NULL)
#' @param admin_col Optional column name in `admin_sf` containing administrative unit names (default NULL)
#'
#' @return An `sf` object with additional columns:
#'   - `id_pu`: Sequential ID
#'   - `stat_pu`: Planning unit status
#'   - `admin`: Administrative unit name (if admin_sf was provided)
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
#'   st_centroid st_coordinates st_is_longlat st_transform st_area st_cast st_geometry
#'   st_intersection st_crs
#' @importFrom dplyr mutate row_number select if_else filter everything any_of
#'
#' @export
filter_overlaps <- function(sf_obj, threshold_ha = 156.25, min_sliver_ha = 1.0, admin_sf = NULL, admin_col = NULL) {
  if (!inherits(sf_obj, "sf")) stop("sf_obj must be an sf object.")
  if (!"stat_pu" %in% names(sf_obj)) stop("sf_obj must contain 'stat_pu' column.")
  
  if (!is.null(admin_sf)) {
    if (!inherits(admin_sf, "sf")) stop("admin_sf must be an sf object.")
    if (is.null(admin_col) || !is.character(admin_col) || length(admin_col) != 1 ||
        is.na(admin_col) || !nzchar(admin_col) ||
        !admin_col %in% names(admin_sf) ||
        admin_col == attr(admin_sf, "sf_column")) {
      stop("admin_col must be a valid column name in admin_sf.")
    }
  }
  
  # Remove empty geometries
  sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
  if (nrow(sf_obj) == 0) stop("No polygon features remaining.")
  
  # Extract polygon features from geometry collections if present
  geom_types <- as.character(sf::st_geometry_type(sf_obj))
  if (any(geom_types %in% c("GEOMETRYCOLLECTION", "GEOMETRY"))) {
    sf_obj <- suppressWarnings(sf::st_collection_extract(sf_obj, "POLYGON"))
    sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
    if (nrow(sf_obj) == 0) stop("No polygon features remaining.")
  }
  
  # Keep only polygon / multipolygon geometries (discard accidental points, linestrings)
  geom_types <- as.character(sf::st_geometry_type(sf_obj))
  is_poly <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
  if (!any(is_poly)) stop("No polygon features remaining.")
  sf_obj <- sf_obj[is_poly, ]
  
  # Ensure valid geometry
  sf_obj <- sf::st_make_valid(sf_obj)
  sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
  if (nrow(sf_obj) == 0) stop("No polygon features remaining.")
  
  # If st_make_valid introduced geometry collections, extract polygons again
  geom_types <- as.character(sf::st_geometry_type(sf_obj))
  if (any(geom_types %in% c("GEOMETRYCOLLECTION", "GEOMETRY"))) {
    sf_obj <- suppressWarnings(sf::st_collection_extract(sf_obj, "POLYGON"))
    sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
    if (nrow(sf_obj) == 0) stop("No polygon features remaining.")
  }
  
  get_utm_epsg <- function(x) {
    union_geom <- sf::st_union(x) |> sf::st_make_valid()
    centroid <- suppressWarnings(sf::st_centroid(union_geom))
    coords <- sf::st_coordinates(centroid)
    lon <- coords[1, "X"]; lat <- coords[1, "Y"]
    zone <- floor((lon + 180) / 6) + 1
    as.integer(ifelse(lat >= 0, 32600 + zone, 32700 + zone))
  }
  
  if (isTRUE(sf::st_is_longlat(sf_obj))) {
    target_epsg <- get_utm_epsg(sf_obj)
    message("Reprojecting to UTM zone ", target_epsg %% 100, " (EPSG:", target_epsg, ")")
    sf_obj <- sf::st_transform(sf_obj, target_epsg)
  }
  
  sf_obj <- suppressWarnings(sf::st_cast(sf_obj, "POLYGON", warn = FALSE))
  sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
  if (nrow(sf_obj) == 0) stop("No polygon features remaining.")
  
  # Administrative boundary partitioning (if provided)
  if (!is.null(admin_sf)) {
    admin_sub <- admin_sf[, admin_col, drop = FALSE]
    names(admin_sub)[names(admin_sub) == admin_col] <- "admin"
    admin_sub$admin <- as.character(admin_sub$admin)
    
    if (is.na(sf::st_crs(admin_sub)) && !is.na(sf::st_crs(sf_obj))) {
      stop("admin_sf has missing CRS while sf_obj has defined CRS.")
    }
    if (!is.na(sf::st_crs(admin_sub)) && is.na(sf::st_crs(sf_obj))) {
      stop("sf_obj has missing CRS while admin_sf has defined CRS.")
    }
    if (!is.na(sf::st_crs(sf_obj)) && !is.na(sf::st_crs(admin_sub)) && sf::st_crs(admin_sub) != sf::st_crs(sf_obj)) {
      admin_sub <- sf::st_transform(admin_sub, sf::st_crs(sf_obj))
    }
    admin_sub <- sf::st_make_valid(admin_sub)
    admin_types <- as.character(sf::st_geometry_type(admin_sub))
    if (any(admin_types %in% c("GEOMETRYCOLLECTION", "GEOMETRY"))) {
      admin_sub <- suppressWarnings(sf::st_collection_extract(admin_sub, "POLYGON"))
    }
    admin_sub <- admin_sub[!sf::st_is_empty(admin_sub), ]
    admin_types <- as.character(sf::st_geometry_type(admin_sub))
    admin_is_poly <- admin_types %in% c("POLYGON", "MULTIPOLYGON")
    if (!any(admin_is_poly)) stop("admin_sf must contain polygon or multipolygon geometries.")
    admin_sub <- admin_sub[admin_is_poly, ]
    if (nrow(admin_sub) == 0) stop("admin_sf has no valid polygon features.")
    
    if ("admin" %in% names(sf_obj) && attr(sf_obj, "sf_column") != "admin") {
      sf_obj$admin <- NULL
    }
    
    sf_obj <- suppressWarnings(sf::st_intersection(sf_obj, admin_sub))
    sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
    if (nrow(sf_obj) == 0) stop("No polygon features remaining.")
    
    # Ensure singlepart POLYGON integrity after intersection
    geom_types <- as.character(sf::st_geometry_type(sf_obj))
    if (any(geom_types %in% c("GEOMETRYCOLLECTION", "GEOMETRY"))) {
      sf_obj <- suppressWarnings(sf::st_collection_extract(sf_obj, "POLYGON"))
      sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
      if (nrow(sf_obj) == 0) stop("No polygon features remaining.")
    }
    geom_types <- as.character(sf::st_geometry_type(sf_obj))
    is_poly <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
    if (!any(is_poly)) stop("No polygon features remaining.")
    sf_obj <- sf_obj[is_poly, ]
    
    sf_obj <- suppressWarnings(sf::st_cast(sf_obj, "POLYGON", warn = FALSE))
    sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
    if (nrow(sf_obj) == 0) stop("No polygon features remaining.")
  }
  
  sf_obj$area_ha <- as.numeric(sf::st_area(sf::st_geometry(sf_obj))) / 10000
  
  sf_obj <- sf_obj |>
    dplyr::filter(area_ha >= min_sliver_ha) |>
    dplyr::mutate(
      id_pu = dplyr::row_number(),
      area_flag = dplyr::if_else(
        area_ha >= threshold_ha, "luas terpenuhi", "luas tidak terpenuhi"
      )
    ) |>
    dplyr::select(id_pu, stat_pu, dplyr::any_of("admin"), dplyr::everything())
  
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
  
  col3_name <- if ("RTRW" %in% names(sf_obj)) "RTRW" else names(sf_obj)[3]
  col4_name <- if ("RZWP3K" %in% names(sf_obj)) "RZWP3K" else names(sf_obj)[4]
  col3_values <- sf_obj[[col3_name]]
  col4_values <- sf_obj[[col4_name]]
  allowed1 <- unique(tibble1[[2]])
  allowed2 <- unique(tibble2[[2]])
  
  # Ignore NA values when checking mismatches
  col3_non_na <- col3_values[!is.na(col3_values)]
  col4_non_na <- col4_values[!is.na(col4_values)]
  
  mismatches3 <- unique(col3_non_na[!col3_non_na %in% allowed1])
  mismatches4 <- unique(col4_non_na[!col4_non_na %in% allowed2])
  
  cat("\n=== Zone Class Validation Report ===\n")
  if (length(mismatches3) == 0) {
    cat("✓", col3_name, " : All non-NA classes match.\n", sep = "")
  } else {
    cat("✗", col3_name, " : Mismatches:\n", sep = "")
    for (val in mismatches3) cat("    - '", val, "'\n", sep = "")
  }
  if (length(mismatches4) == 0) {
    cat("✓", col4_name, " : All non-NA classes match.\n", sep = "")
  } else {
    cat("✗", col4_name, " : Mismatches:\n", sep = "")
    for (val in mismatches4) cat("    - '", val, "'\n", sep = "")
  }
  cat("===================================\n")
  
  invisible(list(mismatch_col3 = mismatches3, mismatch_col4 = mismatches4))
}


