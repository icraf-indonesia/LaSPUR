# R/engine/01_spatial_primitives.R
# ============================================================
# Low-level Spatial Geometric Operations & Raster Utilities
# ============================================================

#' Calculate coordinate precision grid size for meters in CRS units
#'
#' @param x An `sf` object.
#' @param m_precision Target precision in meters (default 1).
#' @param silent Suppress informative messages.
#' @return Numeric: grid size in CRS units (meters for projected, degrees for geographic).
#' @export
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

#' Handle geometry collections and multisurfaces in an sf object
#'
#' Converts geometry collections and multisurfaces to multipolygons by extracting
#' polygon components. Rows without any polygon data are dropped.
#'
#' @param sf_obj `sf` object to clean.
#' @return `sf` object with valid MULTIPOLYGON geometries.
#' @export
handle_geom_collection <- function(sf_obj) {
  geom_types <- as.character(sf::st_geometry_type(sf_obj))
  is_collection <- any(geom_types %in% c("GEOMETRYCOLLECTION", "MULTISURFACE"))
  
  if (is_collection) {
    n_before <- nrow(sf_obj)
    
    sf_obj <- sf_obj %>%
      sf::st_make_valid() %>%
      sf::st_collection_extract("POLYGON") %>%
      sf::st_cast("MULTIPOLYGON")
    
    n_after <- nrow(sf_obj)
    
    if (n_before != n_after) {
      warning(paste("Dropped", n_before - n_after, "rows with no polygon data."))
    }
  }
  return(sf_obj)
}

#' Calculate Euclidean distance raster from vector features clipped/masked to planning units
#'
#' @param vector_obj `sf` object containing source features (e.g. estuaries, industry).
#' @param pu `sf` object defining the study area planning units.
#' @param resolution Numeric. Cell resolution in CRS units (default 100).
#' @param clip_to_pu Logical. Whether to clip source vector to planning unit bounds.
#' @return A `terra::SpatRaster` of Euclidean distances masked to planning units.
#' @export
calculate_euclidean_dist <- function(vector_obj, pu, resolution = 100, clip_to_pu = TRUE) {
  if (!inherits(vector_obj, "sf")) stop("vector_obj must be an sf object")
  if (!inherits(pu, "sf")) stop("pu must be an sf object")
  
  vector_obj <- sf::st_make_valid(vector_obj)
  pu <- sf::st_make_valid(pu)
  
  # Harmonise CRS
  if (!identical(sf::st_crs(vector_obj), sf::st_crs(pu))) {
    message("Reprojecting vector_obj to CRS of pu")
    vector_obj <- sf::st_transform(vector_obj, sf::st_crs(pu))
  }
  
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
    
    if (nrow(vector_clipped) == 0) {
      message("Intersection with pu yielded no features; using full vector_obj extent for distance calculation.")
      vector_clipped <- vector_obj
    }
  } else {
    vector_clipped <- vector_obj
  }
  
  bb <- sf::st_bbox(pu)
  r_template <- terra::rast(
    xmin = bb["xmin"], xmax = bb["xmax"],
    ymin = bb["ymin"], ymax = bb["ymax"],
    resolution = resolution,
    crs = sf::st_crs(pu)$wkt
  )
  
  source_vect <- terra::vect(vector_clipped)
  dist_raster <- terra::distance(r_template, source_vect)
  
  pu_vect <- terra::vect(pu)
  dist_raster <- terra::mask(dist_raster, pu_vect)
  
  return(dist_raster)
}

#' Extract raster values to sf polygons using exact area-weighted extraction
#'
#' @param pu `sf` object of polygons.
#' @param rast `SpatRaster` or `RasterLayer`.
#' @param id_col Character naming ID column.
#' @param new_col Character for new column name.
#' @param na.rm Logical. Remove NAs before computing weighted mean.
#' @param fill_na Value to replace NA extraction results.
#' @return `sf` object with extracted weighted mean values.
#' @export
extract_raster_to_sf <- function(pu, rast, id_col, new_col = NULL, na.rm = TRUE, fill_na = 0) {
  if (is.null(new_col)) {
    new_col <- paste0(names(rast)[1], "_weighted_mean")
  }
  
  if (sf::st_crs(pu) != sf::st_crs(rast)) {
    message("Reprojecting polygons to match raster CRS...")
    pu <- sf::st_transform(pu, sf::st_crs(rast))
  }
  
  results <- exactextractr::exact_extract(
    rast, 
    pu, 
    fun = "mean", 
    progress = FALSE
  )
  
  pu[[new_col]] <- results
  pu[[new_col]][is.na(pu[[new_col]])] <- fill_na
  
  return(pu)
}

#' Calculate percentage and hectare overlay between planning units and target polygon layers
#'
#' @param pu `sf` object of planning units with `area_ha` column.
#' @param overlay_area `sf` object of target overlay polygons.
#' @param title Character prefix for generated columns.
#' @param parallel Logical. Execute in parallel.
#' @param workers Number of parallel workers.
#' @param plan_strategy Parallel strategy ("multisession" or "multicore").
#' @param progress Logical. Display progress bar.
#' @return `sf` object with added `{title}_ha` and `{title}_pct` columns.
#' @export
calculate_overlay_pct <- function(pu,
                                  overlay_area,
                                  title = "overlay_area",
                                  parallel = FALSE,
                                  workers = NULL,
                                  plan_strategy = c("multisession", "multicore"),
                                  progress = TRUE) {
  plan_strategy <- match.arg(plan_strategy)
  
  if (is.list(overlay_area) && !inherits(overlay_area, "sf")) {
    overlay_area <- do.call(rbind, overlay_area)
  }
  
  if (sf::st_crs(pu) != sf::st_crs(overlay_area)) {
    overlay_area <- sf::st_transform(overlay_area, sf::st_crs(pu))
  }
  
  ha_col <- paste0(title, "_ha")
  pct_col <- paste0(title, "_pct")
  
  pu[[ha_col]] <- 0
  pu[[pct_col]] <- 0
  
  intersects_idx <- sf::st_intersects(pu, overlay_area)
  intersecting_pu <- which(lengths(intersects_idx) > 0)
  
  if (length(intersecting_pu) == 0) {
    return(pu)
  }
  
  if (!parallel) {
    for (i in intersecting_pu) {
      intersection <- sf::st_intersection(pu[i, ], overlay_area)
      if (nrow(intersection) > 0) {
        overlap_area_ha <- sum(as.numeric(sf::st_area(intersection))) / 10000
        pu[[ha_col]][i] <- overlap_area_ha
        pu[[pct_col]][i] <- (overlap_area_ha / pu$area_ha[i]) * 100
      }
    }
    return(pu)
  }
  
  old_plan <- future::plan("list")
  on.exit(future::plan(old_plan), add = TRUE)
  if (is.null(workers)) workers <- future::availableCores()
  if (plan_strategy == "multisession") {
    future::plan(future::multisession, workers = workers)
  } else {
    future::plan(future::multicore, workers = workers)
  }
  
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
  
  pu[[ha_col]][intersecting_pu] <- overlap_areas
  pu[[pct_col]][intersecting_pu] <- (overlap_areas / pu$area_ha[intersecting_pu]) * 100
  
  return(pu)
}

#' Extract area-weighted attribute values from one sf layer into planning units
#'
#' @param pu `sf` object of planning units.
#' @param value_sf `sf` object containing values to aggregate.
#' @param value_col Character naming attribute column to aggregate.
#' @param pu_id Character naming unique ID column in pu.
#' @param new_col Character naming new column in pu.
#' @param fill_na Value for units with no overlap.
#' @param parallel Logical. Execute in parallel.
#' @param workers Number of workers.
#' @param plan_strategy Parallel strategy.
#' @param progress Logical. Display progress bar.
#' @return `sf` object with added weighted attribute column.
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
    
    agg <- stats::aggregate(cbind(weighted, area_overlap) ~ inter[[pu_id]],
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
  
  old_plan <- future::plan("list")
  on.exit(future::plan(old_plan), add = TRUE)
  if (is.null(workers)) workers <- future::availableCores()
  if (plan_strategy == "multisession") {
    future::plan(future::multisession, workers = workers)
  } else {
    future::plan(future::multicore, workers = workers)
  }
  
  n <- nrow(pu)
  idx_chunks <- split(seq_len(n), cut(seq_len(n), breaks = workers, labels = FALSE))
  pu_chunks <- lapply(idx_chunks, function(idx) pu[idx, ])
  
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
    
    agg <- stats::aggregate(cbind(weighted, area_overlap) ~ inter[[pu_id]],
                            data = inter, FUN = sum)
    names(agg)[1] <- pu_id
    agg$mean <- agg$weighted / agg$area_overlap
    agg[, c(pu_id, "mean"), drop = FALSE]
  }
  
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
