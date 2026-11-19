# R/engine/indices/padu_se.R
# ============================================================
# PADU-SE (Sosial & Ekonomi) Engine
# ============================================================

#' Build Facility Layer for PADU-SE Hybrid Spatial Pipeline
#'
#' Ingests either vector facility features (points/lines/polygons in GeoPackage or Shapefile)
#' or pre-computed distance rasters (GeoTIFF), harmonizing them into a standard
#' facility structure containing a Euclidean distance raster in meters aligned to
#' the planning units.
#'
#' @param name Character string facility label/identifier (e.g. "Puskesmas", "Sekolah").
#' @param file_input Path to file, an sf object, a SpatRaster, or a Shiny fileInput data frame.
#' @param fac_type Character string, either \code{"vector"} or \code{"raster"}. Default is \code{"vector"}.
#' @param pu An \code{sf} polygon object representing planning units.
#' @param max_dist Numeric maximum distance threshold in meters. Default is 5000.
#' @param resolution Numeric resolution in meters for Euclidean distance rasterization. Default is 100.
#' @return A list with elements \code{name}, \code{dist_rast}, and \code{max_dist}.
#' @export
build_facility_layer <- function(name,
                                 file_input,
                                 fac_type = c("vector", "raster"),
                                 pu,
                                 max_dist = 5000,
                                 resolution = 100) {
  fac_type <- match.arg(fac_type)
  
  if (missing(name) || is.null(name) || length(name) != 1 || is.na(name) || !nzchar(trimws(as.character(name)))) {
    name <- "facility"
  } else {
    name <- trimws(as.character(name))
  }
  
  if (missing(max_dist) || is.null(max_dist) || !is.numeric(max_dist) || length(max_dist) != 1 || is.na(max_dist) || !is.finite(max_dist) || max_dist <= 0) {
    max_dist <- 5000
  } else {
    max_dist <- as.numeric(max_dist)
  }
  
  if (missing(pu) || !inherits(pu, "sf")) {
    stop("pu must be an sf object")
  }
  if (nrow(pu) == 0) {
    stop("pu must contain at least one feature")
  }
  if (is.na(sf::st_crs(pu))) {
    stop("pu must have a defined CRS")
  }
  
  dist_rast <- NULL
  
  if (fac_type == "vector") {
    vec_obj <- NULL
    
    if (inherits(file_input, "sf")) {
      vec_obj <- file_input
    } else if (is.character(file_input) && length(file_input) == 1 && file.exists(file_input)) {
      ext <- tolower(tools::file_ext(file_input))
      if (ext == "gpkg") {
        vec_obj <- sf::st_read(file_input, quiet = TRUE)
      } else if (ext == "shp") {
        if (exists("load_and_validate_shapefile", mode = "function")) {
          vec_obj <- load_and_validate_shapefile(file_input)
        } else {
          vec_obj <- sf::st_read(file_input, quiet = TRUE)
        }
      } else {
        stop(sprintf("Format file vektor tidak didukung untuk fasilitas '%s': .%s", name, ext))
      }
    } else if (is.data.frame(file_input) && "name" %in% names(file_input) && "datapath" %in% names(file_input)) {
      gpkg_idx <- grep("\\.gpkg$", file_input$name, ignore.case = TRUE)
      if (length(gpkg_idx) > 0) {
        vec_obj <- sf::st_read(file_input$datapath[gpkg_idx[1]], quiet = TRUE)
      } else {
        shp_idx <- grep("\\.shp$", file_input$name, ignore.case = TRUE)
        if (length(shp_idx) == 0) {
          stop(sprintf("Komponen shapefile (.shp) atau file .gpkg tidak ditemukan untuk fasilitas '%s'", name))
        }
        stage_dir <- tempfile(pattern = "fac_shp_stage_")
        dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
        stem_name <- tools::file_path_sans_ext(file_input$name[shp_idx[1]])
        for (i in seq_len(nrow(file_input))) {
          ext <- tools::file_ext(file_input$name[i])
          dest_file <- file.path(stage_dir, paste0(stem_name, ".", ext))
          if (normalizePath(file_input$datapath[i], mustWork = FALSE) != normalizePath(dest_file, mustWork = FALSE)) {
            file.copy(file_input$datapath[i], dest_file, overwrite = TRUE)
          }
        }
        target_shp <- file.path(stage_dir, paste0(stem_name, ".shp"))
        if (exists("load_and_validate_shapefile", mode = "function")) {
          vec_obj <- load_and_validate_shapefile(target_shp)
        } else {
          vec_obj <- sf::st_read(target_shp, quiet = TRUE)
        }
      }
    } else {
      stop(sprintf("file_input tidak valid untuk fasilitas vektor '%s'", name))
    }
    
    if (is.null(sf::st_geometry(vec_obj)) || nrow(vec_obj) == 0) {
      stop(sprintf("Data vektor fasilitas '%s' tidak memiliki fitur atau geometri valid", name))
    }
    if (is.na(sf::st_crs(vec_obj))) {
      stop(sprintf("Data vektor fasilitas '%s' tidak memiliki sistem koordinat (CRS) terdefinisi", name))
    }
    
    vec_obj <- sf::st_zm(vec_obj, drop = TRUE, what = "ZM")
    empty_idx <- sf::st_is_empty(vec_obj)
    if (any(empty_idx)) {
      vec_obj <- vec_obj[!empty_idx, ]
      if (nrow(vec_obj) == 0) {
        stop(sprintf("Seluruh geometri pada data fasilitas '%s' berupa empty geometry", name))
      }
    }
    
    invalid_mask <- !sf::st_is_valid(vec_obj)
    if (any(invalid_mask, na.rm = TRUE)) {
      vec_obj <- sf::st_make_valid(vec_obj)
    }
    
    geom_types <- unique(as.character(sf::st_geometry_type(vec_obj)))
    if (any(grepl("GEOMETRYCOLLECTION", geom_types))) {
      for (st in c("POINT", "LINESTRING", "POLYGON")) {
        extracted <- tryCatch(sf::st_collection_extract(vec_obj, st), error = function(e) NULL)
        if (!is.null(extracted) && nrow(extracted) > 0) {
          vec_obj <- extracted
          break
        }
      }
    }
    
    # Calculate Euclidean distance raster
    dist_rast <- calculate_euclidean_dist(
      vector_obj = vec_obj,
      pu         = pu,
      resolution = resolution,
      clip_to_pu = FALSE
    )
    
  } else {
    # fac_type == "raster"
    if (inherits(file_input, c("SpatRaster", "RasterLayer", "RasterBrick", "RasterStack"))) {
      if (inherits(file_input, "SpatRaster")) {
        dist_rast <- file_input
      } else {
        dist_rast <- terra::rast(file_input)
      }
    } else {
      path <- NULL
      if (is.character(file_input) && length(file_input) == 1 && file.exists(file_input)) {
        path <- file_input
      } else if (is.data.frame(file_input) && "name" %in% names(file_input) && "datapath" %in% names(file_input)) {
        tif_idx <- grep("\\.(tif|tiff|geotiff)$", file_input$name, ignore.case = TRUE)
        if (length(tif_idx) == 0) {
          stop(sprintf("Raster jarak untuk fasilitas '%s' harus berupa file GeoTIFF (.tif)", name))
        }
        path <- file_input$datapath[tif_idx[1]]
        ext <- tolower(tools::file_ext(path))
        if (!ext %in% c("tif", "tiff", "geotiff")) {
          stage_tif <- tempfile(pattern = "fac_rast_stage_", fileext = ".tif")
          file.copy(path, stage_tif, overwrite = TRUE)
          path <- stage_tif
        }
      } else {
        stop(sprintf("file_input tidak valid untuk fasilitas raster '%s'", name))
      }
      
      if (exists("load_and_validate_raster", mode = "function")) {
        dist_rast <- load_and_validate_raster(path)
      } else {
        dist_rast <- terra::rast(path)
      }
    }
    
    if (inherits(dist_rast, "SpatRaster") && terra::nlyr(dist_rast) > 1) {
      dist_rast <- dist_rast[[1]]
    }
  }
  
  list(
    name      = name,
    dist_rast = dist_rast,
    max_dist  = max_dist
  )
}

#' S3 method for names of padu_se_result
#' @param x A `padu_se_result` object.
#' @return Character vector of names.
#' @export
names.padu_se_result <- function(x) {
  c("idx_padu_se_map", "idx_padu_se_table", "idx_padu_se")
}

#' Calculate PADU-SE Index from NTL, Population Density, and Facilities
#'
#' Computes the PADU-SE (Socio-Economic Integration) index for planning units
#' by combining demographic and economic activity (Night Time Light and Population Density)
#' with optional infrastructure accessibility scores.
#'
#' @param idx_serasi_map `sf` data frame containing planning units with an identifier column.
#' @param ntl_rast `SpatRaster` or `RasterLayer` representing Night Time Light.
#' @param popdens_rast `SpatRaster` or `RasterLayer` representing Population Density.
#' @param facilities Named list of facility definitions (optional).
#' @param id_col Character. Identifier column name in \code{idx_serasi_map}. Default is \code{"id_pu"}.
#' @param fill_na Numeric or NA. Value used to fill NA cells during raster extraction. Default is \code{NA}.
#' @param keep_intermediates Logical. If \code{FALSE} (default), intermediate normalized columns are dropped.
#' @param standardized Logical. If \code{TRUE}, returns the standardized contract list \code{list(data, score_col, summary, metadata)}. Default is \code{FALSE} for backward compatibility.
#' @return A list conforming to the standardized PADU contract, or an extended backward-compatible list.
#' @export
calculate_padu_se <- function(idx_serasi_map,
                              ntl_rast,
                              popdens_rast,
                              facilities = list(),
                              id_col = "id_pu",
                              fill_na = NA,
                              keep_intermediates = FALSE,
                              standardized = FALSE) {
  # Input validation
  if (!inherits(idx_serasi_map, "sf")) {
    stop("idx_serasi_map must be an sf object")
  }
  if (nrow(idx_serasi_map) == 0) {
    stop("idx_serasi_map must contain at least one feature")
  }
  if (!id_col %in% names(idx_serasi_map)) {
    stop(sprintf("Column '%s' not found in idx_serasi_map", id_col))
  }
  if (any(duplicated(idx_serasi_map[[id_col]]))) {
    stop(sprintf("Column '%s' in idx_serasi_map contains duplicate values", id_col))
  }
  if (is.na(sf::st_crs(idx_serasi_map))) {
    stop("idx_serasi_map must have a defined CRS")
  }
  if (any(sf::st_is_empty(idx_serasi_map))) {
    stop("idx_serasi_map contains empty geometries")
  }
  if (!inherits(ntl_rast, c("SpatRaster", "RasterLayer", "RasterBrick", "RasterStack"))) {
    stop("ntl_rast must be a SpatRaster or RasterLayer object")
  }
  if (!inherits(popdens_rast, c("SpatRaster", "RasterLayer", "RasterBrick", "RasterStack"))) {
    stop("popdens_rast must be a SpatRaster or RasterLayer object")
  }
  if (is.null(facilities)) {
    facilities <- list()
  }
  if (!is.list(facilities)) {
    stop("facilities must be a list")
  }

  ensure_single_layer <- function(r) {
    if (inherits(r, "SpatRaster") && terra::nlyr(r) > 1) {
      return(r[[1]])
    } else if (inherits(r, c("RasterStack", "RasterBrick"))) {
      return(r[[1]])
    }
    r
  }
  ntl_rast <- ensure_single_layer(ntl_rast)
  popdens_rast <- ensure_single_layer(popdens_rast)

  calc_min_max_norm <- function(x) {
    valid <- x[is.finite(x)]
    if (length(valid) == 0) {
      return(rep(NA_real_, length(x)))
    }
    min_val <- min(valid)
    max_val <- max(valid)
    if (max_val == min_val) {
      out <- rep(1.0, length(x))
      out[!is.finite(x)] <- NA_real_
      return(out)
    }
    norm_val <- (x - min_val) / (max_val - min_val)
    norm_val <- pmax(0, pmin(1, norm_val))
    norm_val[!is.finite(x)] <- NA_real_
    norm_val
  }

  sanitize_fac_name <- function(name, idx = 1, existing_names = character(0)) {
    if (is.null(name) || length(name) != 1 || is.na(name) || !nzchar(trimws(as.character(name)))) {
      base <- paste0("facility_", idx)
    } else {
      s <- tolower(trimws(as.character(name)))
      s <- gsub("[^a-z0-9]+", "_", s)
      s <- gsub("^_+|_+$", "", s)
      base <- if (nzchar(s)) s else paste0("facility_", idx)
    }
    candidate <- base
    k <- 2
    while (candidate %in% existing_names) {
      candidate <- paste0(base, "_", k)
      k <- k + 1
    }
    candidate
  }

  res_sf <- idx_serasi_map

  # Extract zonal values for NTL and Population Density
  ntl_extracted <- extract_raster_to_sf(
    pu       = idx_serasi_map,
    rast     = ntl_rast,
    id_col   = id_col,
    new_col  = "ntl",
    fill_na  = fill_na
  )

  popdens_extracted <- extract_raster_to_sf(
    pu       = idx_serasi_map,
    rast     = popdens_rast,
    id_col   = id_col,
    new_col  = "popdens",
    fill_na  = fill_na
  )

  # Pillar 1: Min-Max Normalization and Activity Index
  res_sf$ntl <- ntl_extracted$ntl
  res_sf$ntl_norm <- calc_min_max_norm(res_sf$ntl)
  res_sf$popdens <- popdens_extracted$popdens
  res_sf$popdens_norm <- calc_min_max_norm(res_sf$popdens)
  res_sf$idx_aktivitas <- as.numeric((res_sf$ntl_norm + res_sf$popdens_norm) / 2)

  # Pillar 2: Facility Accessibility & Bounded Linear Decay
  acc_cols <- character(0)
  if (length(facilities) == 0) {
    res_sf$idx_aksesibilitas <- rep(NA_real_, nrow(res_sf))
    res_sf$idx_padu_se <- as.numeric(res_sf$idx_aktivitas)
  } else {
    seen_names <- character(0)
    for (i in seq_along(facilities)) {
      fac <- facilities[[i]]
      if (!is.list(fac)) {
        stop(sprintf("Facility at index %d must be a list", i))
      }
      raw_name <- if (!is.null(fac$name) && length(fac$name) == 1 && !is.na(fac$name) && nzchar(trimws(as.character(fac$name)))) {
        trimws(as.character(fac$name))
      } else if (!is.null(names(facilities)) && length(names(facilities)) >= i && !is.na(names(facilities)[i]) && nzchar(trimws(names(facilities)[i]))) {
        trimws(names(facilities)[i])
      } else {
        paste0("facility_", i)
      }

      clean_name <- sanitize_fac_name(raw_name, idx = i, existing_names = seen_names)
      seen_names <- c(seen_names, clean_name)
      dist_col <- paste0("dist_", clean_name)
      acc_col <- paste0("acc_", clean_name)

      if (is.null(fac$dist_rast) || !inherits(fac$dist_rast, c("SpatRaster", "RasterLayer", "RasterBrick", "RasterStack"))) {
        stop(sprintf("dist_rast for facility '%s' must be a SpatRaster or RasterLayer", raw_name))
      }
      fac_dist_rast <- ensure_single_layer(fac$dist_rast)

      max_d <- 5000
      if (!is.null(fac$max_dist)) {
        if (!is.numeric(fac$max_dist) || length(fac$max_dist) != 1 || is.na(fac$max_dist) || !is.finite(fac$max_dist) || fac$max_dist <= 0) {
          stop(sprintf("max_dist for facility '%s' must be a positive number", raw_name))
        }
        max_d <- as.numeric(fac$max_dist)
      }

      fac_extracted <- extract_raster_to_sf(
        pu      = idx_serasi_map,
        rast    = fac_dist_rast,
        id_col  = id_col,
        new_col = dist_col,
        fill_na = fill_na
      )
      res_sf[[dist_col]] <- fac_extracted[[dist_col]]

      d_val <- res_sf[[dist_col]]
      acc_val <- ifelse(is.na(d_val) | d_val < -1e-6, NA_real_, pmax(0, pmin(1, 1 - pmin(pmax(0, d_val) / max_d, 1))))
      res_sf[[acc_col]] <- acc_val
      acc_cols <- c(acc_cols, acc_col)
    }

    if (length(acc_cols) > 0) {
      acc_mat <- as.matrix(sf::st_drop_geometry(res_sf)[, acc_cols, drop = FALSE])
      res_sf$idx_aksesibilitas <- as.numeric(rowMeans(acc_mat, na.rm = FALSE))
      res_sf$idx_padu_se <- as.numeric(0.5 * res_sf$idx_aktivitas + 0.5 * res_sf$idx_aksesibilitas)
    } else {
      res_sf$idx_aksesibilitas <- rep(NA_real_, nrow(res_sf))
      res_sf$idx_padu_se <- as.numeric(res_sf$idx_aktivitas)
    }
  }

  if (!isTRUE(keep_intermediates)) {
    drop_cols <- c("ntl_norm", "popdens_norm", "idx_aktivitas", "idx_aksesibilitas", acc_cols)
    res_sf <- res_sf[, setdiff(names(res_sf), drop_cols), drop = FALSE]
  }

  res_table <- tibble::as_tibble(sf::st_drop_geometry(res_sf))
  score_col <- "idx_padu_se"
  summary_tbl <- padu_distribution_summary(res_sf, score_col)
  meta <- list(
    timestamp = Sys.time(),
    id_col = id_col,
    keep_intermediates = keep_intermediates
  )

  if (isTRUE(standardized)) {
    return(list(
      data = res_sf,
      score_col = score_col,
      summary = summary_tbl,
      metadata = meta,
      idx_padu_se_map = res_sf,
      idx_padu_se_table = res_table,
      idx_padu_se = res_table
    ))
  }

  # Return backward-compatible S3 list that also responds to $data, $score_col, etc.
  structure(
    list(
      idx_padu_se_map   = res_sf,
      idx_padu_se_table = res_table,
      idx_padu_se       = res_table,
      data              = res_sf,
      score_col         = score_col,
      summary           = summary_tbl,
      metadata          = meta
    ),
    class = c("padu_se_result", "list")
  )
}
