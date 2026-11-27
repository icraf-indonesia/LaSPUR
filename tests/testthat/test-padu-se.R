# tests/testthat/test-padu-se.R

library(testthat)
library(sf)
library(terra)
library(dplyr)
library(tibble)

# Helper function to generate square polygons in projected CRS (EPSG:3857)
make_test_pu_sf <- function(n = 3, id_col = "id_pu") {
  polys <- vector("list", n)
  for (i in seq_len(n)) {
    xmin <- (i - 1) * 1000
    xmax <- i * 1000
    ymin <- 0
    ymax <- 1000
    pts <- matrix(c(
      xmin, ymin,
      xmax, ymin,
      xmax, ymax,
      xmin, ymax,
      xmin, ymin
    ), ncol = 2, byrow = TRUE)
    polys[[i]] <- sf::st_polygon(list(pts))
  }
  sfc <- sf::st_sfc(polys, crs = 3857)
  df <- data.frame(
    id = paste0("PU_", seq_len(n)),
    RTRW = paste0("Zone_", seq_len(n)),
    stringsAsFactors = FALSE
  )
  names(df)[1] <- id_col
  sf::st_sf(df, geometry = sfc)
}

# Helper function to generate matching test raster in EPSG:3857
make_test_rast <- function(pu_sf, values_per_pu) {
  bbox <- sf::st_bbox(pu_sf)
  r <- terra::rast(
    xmin = bbox["xmin"], xmax = bbox["xmax"],
    ymin = bbox["ymin"], ymax = bbox["ymax"],
    resolution = 100, crs = "EPSG:3857"
  )
  # Populate values based on X coordinates
  n_pu <- length(values_per_pu)
  pu_width <- (bbox["xmax"] - bbox["xmin"]) / n_pu
  xy <- terra::xyFromCell(r, 1:terra::ncell(r))
  vals <- rep(values_per_pu[1], terra::ncell(r))
  for (i in seq_len(n_pu)) {
    x_start <- bbox["xmin"] + (i - 1) * pu_width
    x_end   <- bbox["xmin"] + i * pu_width
    mask <- xy[, 1] >= x_start & xy[, 1] <= x_end
    vals[mask] <- values_per_pu[i]
  }
  terra::values(r) <- vals
  r
}

test_that("calculate_padu_se exists and validates inputs including non-empty facilities", {
  expect_true(exists("calculate_padu_se", mode = "function"))

  pu <- make_test_pu_sf(3)
  r_dist <- make_test_rast(pu, c(0, 500, 1000))
  valid_fac <- list(list(name = "Sekolah", dist_rast = r_dist, max_dist = 1000))

  # Non-sf input for idx_serasi_map
  expect_error(
    calculate_padu_se(as.data.frame(pu), valid_fac),
    "sf object"
  )

  # Empty sf
  expect_error(
    calculate_padu_se(pu[0, ], valid_fac),
    "at least one feature"
  )

  # Missing id_col
  expect_error(
    calculate_padu_se(pu, valid_fac, id_col = "non_existent_id"),
    "Column 'non_existent_id' not found"
  )

  # Duplicate id_col values
  pu_dup <- pu
  pu_dup$id_pu[2] <- pu_dup$id_pu[1]
  expect_error(
    calculate_padu_se(pu_dup, valid_fac),
    "duplicate values"
  )

  # Missing CRS
  pu_nocrs <- pu
  sf::st_crs(pu_nocrs) <- NA
  expect_error(
    calculate_padu_se(pu_nocrs, valid_fac),
    "defined CRS"
  )

  # Empty geometry
  pu_empty_geom <- pu
  sf::st_geometry(pu_empty_geom)[1] <- sf::st_polygon()
  expect_error(
    calculate_padu_se(pu_empty_geom, valid_fac),
    "empty geometries"
  )

  # Missing / NULL / empty / invalid facilities
  expect_error(calculate_padu_se(pu), "facilities")
  expect_error(calculate_padu_se(pu, facilities = NULL), "at least one")
  expect_error(calculate_padu_se(pu, facilities = list()), "at least one")
  expect_error(calculate_padu_se(pu, facilities = "not_a_list"), "facilities must be a")
  expect_error(calculate_padu_se(pu, facilities = list("not_a_sublist")), "must be a list")

  # Invalid dist_rast and max_dist
  expect_error(
    calculate_padu_se(pu, facilities = list(list(name = "X", max_dist = 1000))),
    "dist_rast"
  )
  expect_error(
    calculate_padu_se(pu, facilities = list(list(name = "X", dist_rast = "bad", max_dist = 1000))),
    "dist_rast"
  )
  expect_error(
    calculate_padu_se(pu, facilities = list(list(name = "X", dist_rast = r_dist, max_dist = -100))),
    "positive number"
  )
  expect_error(
    calculate_padu_se(pu, facilities = list(list(name = "X", dist_rast = r_dist, max_dist = 0))),
    "positive number"
  )
  expect_error(
    calculate_padu_se(pu, facilities = list(list(name = "X", dist_rast = r_dist, max_dist = Inf))),
    "positive number"
  )
  expect_error(
    calculate_padu_se(pu, facilities = list(list(name = "X", dist_rast = r_dist, max_dist = c(100, 200)))),
    "positive number"
  )
})

test_that("calculate_padu_se computes single and multi-facility accessibility without legacy columns", {
  pu <- make_test_pu_sf(3)
  r_dist_fac1 <- make_test_rast(pu, c(0, 500, 1500))
  fac1 <- list(name = "Rumah Sakit", dist_rast = r_dist_fac1, max_dist = 1000)

  res_single <- calculate_padu_se(
    idx_serasi_map = pu,
    facilities     = list(fac1)
  )

  expect_type(res_single, "list")
  expect_named(res_single, c("idx_padu_se_map", "idx_padu_se_table", "idx_padu_se"), ignore.order = TRUE)
  expect_s3_class(res_single$idx_padu_se_map, "sf")
  expect_s3_class(res_single$idx_padu_se_table, "tbl_df")

  map_single <- res_single$idx_padu_se_map
  expect_true("dist_rumah_sakit" %in% names(map_single))
  expect_true("acc_rumah_sakit" %in% names(map_single))
  expect_true("idx_padu_se" %in% names(map_single))

  legacy_cols <- c("ntl", "ntl_norm", "popdens", "popdens_norm", "idx_aktivitas", "idx_aksesibilitas")
  for (col in legacy_cols) {
    expect_false(col %in% names(map_single), info = paste("Legacy column must be removed:", col))
    expect_false(col %in% names(res_single$idx_padu_se_table), info = paste("Legacy table col must be removed:", col))
  }

  expect_equal(map_single$dist_rumah_sakit, c(0, 500, 1500), tolerance = 1e-3)
  expect_equal(map_single$acc_rumah_sakit, c(1.0, 0.5, 0.0), tolerance = 1e-4)
  expect_equal(map_single$idx_padu_se, c(1.0, 0.5, 0.0), tolerance = 1e-4)

  # Multi-facility (N = 2): equal mean of acc_1 and acc_2
  r_dist_fac2 <- make_test_rast(pu, c(2000, 1000, 0))
  fac2 <- list(name = "Sekolah Menengah", dist_rast = r_dist_fac2, max_dist = 2000)

  res_multi <- calculate_padu_se(
    idx_serasi_map = pu,
    facilities     = list(fac1, fac2)
  )

  map_multi <- res_multi$idx_padu_se_map
  expect_equal(map_multi$acc_sekolah_menengah, c(0.0, 0.5, 1.0), tolerance = 1e-4)
  expect_equal(map_multi$idx_padu_se, c(0.5, 0.5, 0.5), tolerance = 1e-4)
  expect_equal(map_multi$RTRW, pu$RTRW)
})

test_that("calculate_padu_se works with custom id_col and preserves CRS and row alignment", {
  pu_3857 <- make_test_pu_sf(4, id_col = "custom_id")
  r_dist <- make_test_rast(pu_3857, c(0, 500, 1000, 1500))
  pu_4326 <- sf::st_transform(pu_3857[c(4, 2, 1), ], 4326)
  fac <- list(name = "Bandara", dist_rast = r_dist, max_dist = 2000)

  res <- calculate_padu_se(pu_4326, facilities = list(fac), id_col = "custom_id")
  map_res <- res$idx_padu_se_map

  expect_equal(sf::st_crs(map_res), sf::st_crs(4326))
  expect_equal(sf::st_geometry(map_res), sf::st_geometry(pu_4326))
  expect_equal(map_res$custom_id, c("PU_4", "PU_2", "PU_1"))
  expect_equal(map_res$dist_bandara, c(1500, 500, 0), tolerance = 1e-3)
  expect_equal(map_res$acc_bandara, c(0.25, 0.75, 1.0), tolerance = 1e-4)
  expect_equal(map_res$idx_padu_se, c(0.25, 0.75, 1.0), tolerance = 1e-4)
})

test_that("calculate_padu_se handles facility name collision and unnamed fallback", {
  pu <- make_test_pu_sf(3)
  r1 <- make_test_rast(pu, c(0, 500, 1000))
  r2 <- make_test_rast(pu, c(1000, 500, 0))
  fac1 <- list(name = "Klinik", dist_rast = r1, max_dist = 1000)
  fac2 <- list(name = "Klinik", dist_rast = r2, max_dist = 1000)

  res <- calculate_padu_se(pu, facilities = list(fac1, fac2))
  expect_true(all(c("dist_klinik", "dist_klinik_2", "acc_klinik", "acc_klinik_2") %in% names(res$idx_padu_se_map)))
  expect_equal(res$idx_padu_se_map$idx_padu_se, c(0.5, 0.5, 0.5), tolerance = 1e-4)

  fac_na <- list(name = NA, dist_rast = r1, max_dist = 1000)
  fac_ws <- list(name = "   ", dist_rast = r2, max_dist = 1000)
  res_fb <- calculate_padu_se(pu, facilities = list(fac_na, fac_ws))
  expect_true(all(c("dist_facility_1", "dist_facility_2", "acc_facility_1", "acc_facility_2") %in% names(res_fb$idx_padu_se_map)))
})

test_that("calculate_padu_se strictly respects bounded decay limits and beyond d_max", {
  pu <- make_test_pu_sf(4)
  r_dist <- make_test_rast(pu, c(0, 1000, 2000, 5000))
  fac <- list(name = "RSUD", dist_rast = r_dist, max_dist = 2000)

  res <- calculate_padu_se(pu, facilities = list(fac))
  map_res <- res$idx_padu_se_map

  expect_equal(map_res$dist_rsud, c(0, 1000, 2000, 5000), tolerance = 1e-3)
  expect_equal(map_res$acc_rsud, c(1.0, 0.5, 0.0, 0.0), tolerance = 1e-4)
  expect_equal(map_res$idx_padu_se, c(1.0, 0.5, 0.0, 0.0), tolerance = 1e-4)
})

test_that("calculate_padu_se aggregates N = 3 and N = 4 facilities correctly", {
  pu <- make_test_pu_sf(2)
  r1 <- make_test_rast(pu, c(0, 1000))
  r2 <- make_test_rast(pu, c(1000, 1000))
  r3 <- make_test_rast(pu, c(1000, 3000))
  r4 <- make_test_rast(pu, c(500, 500))

  fac1 <- list(name = "F1", dist_rast = r1, max_dist = 1000)
  fac2 <- list(name = "F2", dist_rast = r2, max_dist = 2000)
  fac3 <- list(name = "F3", dist_rast = r3, max_dist = 4000)
  fac4 <- list(name = "F4", dist_rast = r4, max_dist = 1000)

  res3 <- calculate_padu_se(pu, facilities = list(fac1, fac2, fac3))
  expect_equal(res3$idx_padu_se_map$idx_padu_se, c(0.75, 0.25), tolerance = 1e-4)

  res4 <- calculate_padu_se(pu, facilities = list(fac1, fac2, fac3, fac4))
  expect_equal(res4$idx_padu_se_map$idx_padu_se, c(0.6875, 0.3125), tolerance = 1e-4)
})

test_that("calculate_padu_se propagates NA in facility distance rasters (fill_na = NA, na.rm = FALSE)", {
  pu <- make_test_pu_sf(3)
  r_dist_partial <- make_test_rast(pu, c(500, NaN, 1000))
  r_dist_full    <- make_test_rast(pu, c(0, 500, 1000))
  fac_part <- list(name = "RS", dist_rast = r_dist_partial, max_dist = 1000)
  fac_full <- list(name = "Pasar", dist_rast = r_dist_full, max_dist = 1000)

  res_part <- calculate_padu_se(pu, facilities = list(fac_part, fac_full))
  map_part <- res_part$idx_padu_se_map

  expect_equal(map_part$acc_rs[1], 0.5, tolerance = 1e-4)
  expect_true(is.na(map_part$acc_rs[2]))
  expect_equal(map_part$acc_rs[3], 0.0, tolerance = 1e-4)
  expect_equal(map_part$idx_padu_se[1], 0.75, tolerance = 1e-4)
  expect_true(is.na(map_part$idx_padu_se[2]))
  expect_equal(map_part$idx_padu_se[3], 0.0, tolerance = 1e-4)

  # All NA distance raster
  r_dist_all_na <- make_test_rast(pu, c(NaN, NaN, NaN))
  res_all_na <- calculate_padu_se(pu, facilities = list(list(name = "RS", dist_rast = r_dist_all_na, max_dist = 1000)))
  expect_true(all(is.na(res_all_na$idx_padu_se_map$idx_padu_se)))
})

test_that("calculate_padu_se defaults max_dist to 5000, handles negative nodata, multi-band, and complex geometries", {
  pu <- make_test_pu_sf(3)
  r_dist_neg <- make_test_rast(pu, c(2500, -9999, 5000))
  r_multi <- terra::rast(list(r_dist_neg, r_dist_neg * 2))
  fac <- list(name = "Klinik", dist_rast = r_multi)

  res <- calculate_padu_se(pu, facilities = list(fac))
  map_res <- res$idx_padu_se_map
  expect_equal(map_res$acc_klinik[1], 0.5, tolerance = 1e-4)
  expect_true(is.na(map_res$acc_klinik[2]))
  expect_equal(map_res$acc_klinik[3], 0.0, tolerance = 1e-4)
  expect_null(names(map_res$idx_padu_se))
})

# ==============================================================================
# Ticket 03: build_facility_layer & Hybrid Spatial Pipeline Tests
# ==============================================================================

test_that("build_facility_layer processes vector point inputs and computes euclidean distance", {
  pu <- make_test_pu_sf(3) # 3 PUs from x = 0 to 3000, y = 0 to 1000
  
  # Point located in PU 1 at (500, 500)
  pts <- sf::st_sf(
    id = "P1",
    geometry = sf::st_sfc(sf::st_point(c(500, 500)), crs = 3857)
  )
  
  fac_layer <- build_facility_layer(
    name       = "Puskesmas",
    file_input = pts,
    fac_type   = "vector",
    pu         = pu,
    max_dist   = 3000,
    resolution = 100
  )
  
  expect_equal(fac_layer$name, "Puskesmas")
  expect_equal(fac_layer$max_dist, 3000)
  expect_true(inherits(fac_layer$dist_rast, "SpatRaster"))
  
  # Check distance raster properties
  dist_r <- fac_layer$dist_rast
  expect_equal(as.character(terra::crs(dist_r, proj = TRUE)), as.character(sf::st_crs(pu)$proj4string))
  
  # Cell nearest to (500, 500) should have distance near 0
  val_at_point <- terra::extract(dist_r, cbind(500, 500))[[1]]
  expect_lt(val_at_point, 100)
  
  # Cell far away at (2500, 500) should have distance near 2000 m
  val_far <- terra::extract(dist_r, cbind(2500, 500))[[1]]
  expect_equal(val_far, 2000, tolerance = 100)
})

test_that("build_facility_layer processes vector line inputs", {
  pu <- make_test_pu_sf(2) # x = 0 to 2000, y = 0 to 1000
  
  # Line along y = 500 from x = 0 to 2000
  line_geom <- sf::st_linestring(matrix(c(0, 500, 2000, 500), ncol = 2, byrow = TRUE))
  lines_sf <- sf::st_sf(
    id = "Jalan_Utama",
    geometry = sf::st_sfc(line_geom, crs = 3857)
  )
  
  fac_layer <- build_facility_layer(
    name       = "Jalan Arteri",
    file_input = lines_sf,
    fac_type   = "vector",
    pu         = pu,
    max_dist   = 1000,
    resolution = 100
  )
  
  expect_equal(fac_layer$name, "Jalan Arteri")
  expect_true(inherits(fac_layer$dist_rast, "SpatRaster"))
  
  # Point on the line (1000, 500) has distance near 0
  d_on_line <- terra::extract(fac_layer$dist_rast, cbind(1000, 500))[[1]]
  expect_lt(d_on_line, 100)
  
  # Point at (1000, 100) has distance near 400 m
  d_off_line <- terra::extract(fac_layer$dist_rast, cbind(1000, 100))[[1]]
  expect_equal(d_off_line, 400, tolerance = 100)
})

test_that("build_facility_layer handles GeoPackage and Shapefile files on disk", {
  tmp_dir <- tempfile("test_fac_files_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  pu <- make_test_pu_sf(2)
  pts <- sf::st_sf(
    id = c("S1", "S2"),
    geometry = sf::st_sfc(sf::st_point(c(200, 200)), sf::st_point(c(1200, 800)), crs = 3857)
  )
  
  # 1. GeoPackage file
  gpkg_file <- file.path(tmp_dir, "sekolah.gpkg")
  sf::st_write(pts, gpkg_file, quiet = TRUE)
  
  fac_gpkg <- build_facility_layer(
    name       = "Sekolah GPKG",
    file_input = gpkg_file,
    fac_type   = "vector",
    pu         = pu
  )
  expect_equal(fac_gpkg$name, "Sekolah GPKG")
  expect_true(inherits(fac_gpkg$dist_rast, "SpatRaster"))
  
  # 2. Shapefile file
  shp_file <- file.path(tmp_dir, "sekolah.shp")
  sf::st_write(pts, shp_file, quiet = TRUE)
  
  fac_shp <- build_facility_layer(
    name       = "Sekolah SHP",
    file_input = shp_file,
    fac_type   = "vector",
    pu         = pu
  )
  expect_equal(fac_shp$name, "Sekolah SHP")
  expect_true(inherits(fac_shp$dist_rast, "SpatRaster"))
  
  # 3. Shiny-like fileInput data frame for Shapefile (isolated from GPKG)
  shp_dir <- file.path(tmp_dir, "shp_only")
  dir.create(shp_dir, recursive = TRUE)
  shp_subfile <- file.path(shp_dir, "fasilitas_sd.shp")
  sf::st_write(pts, shp_subfile, quiet = TRUE)
  shp_only_files <- list.files(shp_dir, pattern = "^fasilitas_sd\\.(shp|dbf|shx|prj)$", full.names = TRUE)
  shiny_df <- data.frame(
    name     = basename(shp_only_files),
    datapath = shp_only_files,
    stringsAsFactors = FALSE
  )
  
  fac_shiny <- build_facility_layer(
    name       = "Sekolah Shiny",
    file_input = shiny_df,
    fac_type   = "vector",
    pu         = pu
  )
  expect_equal(fac_shiny$name, "Sekolah Shiny")
  expect_true(inherits(fac_shiny$dist_rast, "SpatRaster"))
})

test_that("build_facility_layer processes raster distance inputs (GeoTIFF and Shiny fileInput)", {
  tmp_dir <- tempfile("test_fac_tif_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  pu <- make_test_pu_sf(2)
  r_dist <- make_test_rast(pu, c(500, 1500))
  
  # 1. SpatRaster directly
  fac_r1 <- build_facility_layer(
    name       = "Pasar Direct",
    file_input = r_dist,
    fac_type   = "raster",
    pu         = pu,
    max_dist   = 2000
  )
  expect_equal(fac_r1$name, "Pasar Direct")
  expect_true(inherits(fac_r1$dist_rast, "SpatRaster"))
  expect_equal(fac_r1$max_dist, 2000)
  
  # 2. GeoTIFF on disk
  tif_file <- file.path(tmp_dir, "dist_pasar.tif")
  terra::writeRaster(r_dist, tif_file, overwrite = TRUE)
  
  fac_r2 <- build_facility_layer(
    name       = "Pasar File",
    file_input = tif_file,
    fac_type   = "raster",
    pu         = pu,
    max_dist   = 3000
  )
  expect_equal(fac_r2$name, "Pasar File")
  expect_true(inherits(fac_r2$dist_rast, "SpatRaster"))
  
  # 3. Shiny fileInput data frame
  shiny_tif_df <- data.frame(
    name = "dist_pasar.tif",
    datapath = tif_file,
    stringsAsFactors = FALSE
  )
  fac_r3 <- build_facility_layer(
    name       = "Pasar Shiny",
    file_input = shiny_tif_df,
    fac_type   = "raster",
    pu         = pu,
    max_dist   = 3000
  )
  expect_equal(fac_r3$name, "Pasar Shiny")
  expect_true(inherits(fac_r3$dist_rast, "SpatRaster"))
})

test_that("build_facility_layer validates inputs and handles edge cases robustly", {
  pu <- make_test_pu_sf(2)
  
  # Default name when empty / whitespace
  fac_empty_name <- build_facility_layer(
    name       = "   ",
    file_input = make_test_rast(pu, c(100, 200)),
    fac_type   = "raster",
    pu         = pu
  )
  expect_equal(fac_empty_name$name, "facility")
  
  # Default max_dist when <= 0 or NA
  fac_neg_dmax <- build_facility_layer(
    name       = "NegDmax",
    file_input = make_test_rast(pu, c(100, 200)),
    fac_type   = "raster",
    pu         = pu,
    max_dist   = -500
  )
  expect_equal(fac_neg_dmax$max_dist, 5000)
  
  # Stop when pu is invalid
  expect_error(
    build_facility_layer("test", make_test_rast(pu, c(100, 200)), "raster", pu = data.frame()),
    "pu must be an sf object"
  )
  
  # Stop when vector has no CRS
  pts_no_crs <- sf::st_set_crs(
    sf::st_sf(id = "P1", geometry = sf::st_sfc(sf::st_point(c(100, 100)))),
    sf::NA_crs_
  )
  expect_error(
    build_facility_layer("NoCRS", pts_no_crs, "vector", pu = pu),
    "CRS"
  )
  
  # Stop when vector has empty geometry
  empty_sf <- sf::st_sf(
    id = "Empty",
    geometry = sf::st_sfc(sf::st_point(), crs = 3857)
  )
  expect_error(
    build_facility_layer("EmptyGeom", empty_sf, "vector", pu = pu),
    "empty geometry"
  )
  
  # Strips Z/M dimensions automatically
  pt_3d <- sf::st_sf(
    id = "3D",
    geometry = sf::st_sfc(sf::st_point(c(500, 500, 100)), crs = 3857)
  )
  fac_3d <- build_facility_layer("Point3D", pt_3d, "vector", pu = pu)
  expect_true(inherits(fac_3d$dist_rast, "SpatRaster"))
})

test_that("Full hybrid pipeline: calculate_padu_se with vector + raster facilities exports all columns", {
  tmp_out_dir <- tempfile("test_padu_se_pipeline_")
  dir.create(tmp_out_dir, recursive = TRUE)
  on.exit(unlink(tmp_out_dir, recursive = TRUE), add = TRUE)

  pu <- make_test_pu_sf(3)

  rs_pts <- sf::st_sf(
    id = "RS1",
    geometry = sf::st_sfc(sf::st_point(c(500, 500)), crs = 3857)
  )
  fac1 <- build_facility_layer(
    name       = "Rumah Sakit",
    file_input = rs_pts,
    fac_type   = "vector",
    pu         = pu,
    max_dist   = 2000,
    resolution = 100
  )

  r_dist_pasar <- make_test_rast(pu, c(1000, 500, 0))
  fac2 <- build_facility_layer(
    name       = "Pasar",
    file_input = r_dist_pasar,
    fac_type   = "raster",
    pu         = pu,
    max_dist   = 1000
  )

  calc_res <- calculate_padu_se(
    idx_serasi_map = pu,
    facilities     = list(fac1, fac2),
    id_col         = "id_pu"
  )

  map_res <- calc_res$idx_padu_se_map
  tbl_res <- calc_res$idx_padu_se_table

  expected_cols <- c(
    "id_pu", "dist_rumah_sakit", "acc_rumah_sakit",
    "dist_pasar", "acc_pasar", "idx_padu_se"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(map_res), info = paste("Checking map col:", col))
    expect_true(col %in% names(tbl_res), info = paste("Checking tbl col:", col))
  }

  expect_equal(map_res$idx_padu_se, (map_res$acc_rumah_sakit + map_res$acc_pasar) / 2, tolerance = 1e-4)

  gpkg_path <- file.path(tmp_out_dir, "idx_padu_se.gpkg")
  xlsx_path <- file.path(tmp_out_dir, "idx_padu_se.xlsx")

  sf::st_write(map_res, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
  openxlsx::write.xlsx(tbl_res, xlsx_path)

  read_gpkg <- sf::st_read(gpkg_path, quiet = TRUE)
  read_xlsx <- openxlsx::read.xlsx(xlsx_path)
  for (col in expected_cols) {
    expect_true(col %in% names(read_gpkg), info = paste("Checking saved GPKG col:", col))
    expect_true(col %in% names(read_xlsx), info = paste("Checking saved Excel col:", col))
  }
})

test_that("build_facility_layer handles Shapefile with .shp datapath and GeoTIFF without extension", {
  tmp_dir <- tempfile("test_edge_uploads_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  pu <- make_test_pu_sf(2)
  pts <- sf::st_sf(
    id = "P1",
    geometry = sf::st_sfc(sf::st_point(c(500, 500)), crs = 3857)
  )
  
  # 1. Shapefile where file_input$datapath ends in .shp (verifies staging fixes file-copy-to-self)
  shp_dir <- file.path(tmp_dir, "shp_case")
  dir.create(shp_dir, recursive = TRUE)
  shp_path <- file.path(shp_dir, "roads.shp")
  sf::st_write(pts, shp_path, quiet = TRUE)
  shp_files <- list.files(shp_dir, pattern = "^roads\\.", full.names = TRUE)
  
  df_shp <- data.frame(
    name     = basename(shp_files),
    datapath = shp_files,
    stringsAsFactors = FALSE
  )
  fac_shp <- build_facility_layer("Roads", df_shp, "vector", pu = pu)
  expect_equal(fac_shp$name, "Roads")
  expect_true(inherits(fac_shp$dist_rast, "SpatRaster"))
  
  # 2. GeoTIFF where file_input$datapath has no extension (typical in some Shiny temp uploads)
  r <- make_test_rast(pu, c(200, 400))
  real_tif <- file.path(tmp_dir, "dist.tif")
  terra::writeRaster(r, real_tif, overwrite = TRUE)
  no_ext_temp <- tempfile(pattern = "shiny_upload_0_")
  file.copy(real_tif, no_ext_temp)
  
  df_rast <- data.frame(
    name     = "dist.tif",
    datapath = no_ext_temp,
    stringsAsFactors = FALSE
  )
  fac_rast <- build_facility_layer("DistNoExt", df_rast, "raster", pu = pu)
  expect_equal(fac_rast$name, "DistNoExt")
  expect_true(inherits(fac_rast$dist_rast, "SpatRaster"))
})

test_that("calculate_padu_se handles vector facilities completely outside planning units", {
  pu <- make_test_pu_sf(2)
  pts_far <- sf::st_sf(
    id = "Far1",
    geometry = sf::st_sfc(sf::st_point(c(50000, 50000)), crs = 3857)
  )
  fac_far <- build_facility_layer("FarAway", pts_far, "vector", pu = pu, max_dist = 5000)

  res <- calculate_padu_se(pu, facilities = list(fac_far))
  map_res <- res$idx_padu_se_map

  expect_true(all(map_res$dist_faraway > 5000))
  expect_equal(map_res$acc_faraway, c(0, 0))
  expect_equal(map_res$idx_padu_se, c(0, 0))
})

test_that("build_facility_layer handles geometry collections by extracting geometry elements", {
  pu <- make_test_pu_sf(2)
  # Create a geometrycollection containing a point and linestring
  gc <- sf::st_geometrycollection(list(
    sf::st_point(c(500, 500)),
    sf::st_linestring(matrix(c(100, 100, 800, 800), ncol = 2, byrow = TRUE))
  ))
  sfc <- sf::st_sfc(gc, crs = 3857)
  sf_gc <- sf::st_sf(id = "GC1", geometry = sfc)
  
  fac_gc <- build_facility_layer("GCFac", sf_gc, "vector", pu = pu)
  expect_equal(fac_gc$name, "GCFac")
  expect_true(inherits(fac_gc$dist_rast, "SpatRaster"))
})

# ==============================================================================
# End-to-End Verification & mod_padu_combine Integration Tests
# ==============================================================================

test_that("E2E Scenario 1: PADU-SE NA facility propagation preserves NA and rescales weights in calculate_padu_index", {
  pu <- make_test_pu_sf(3)
  pu$id_pu <- seq_len(nrow(pu))
  r_dist_na <- make_test_rast(pu, c(0, NaN, 1000))
  fac <- list(name = "Jalan", dist_rast = r_dist_na, max_dist = 1000)

  res <- calculate_padu_se(
    idx_serasi_map = pu,
    facilities     = list(fac),
    id_col         = "id_pu"
  )

  map_sf <- res$idx_padu_se_map
  expect_equal(map_sf$idx_padu_se[1], 1.0, tolerance = 1e-4)
  expect_true(is.na(map_sf$idx_padu_se[2]))
  expect_equal(map_sf$idx_padu_se[3], 0.0, tolerance = 1e-4)

  mock_ke <- pu %>% dplyr::mutate(idx_padu_ke = c(0.4, 0.5, 0.6))
  padu_weights <- data.frame(code = c("ke", "se"), value = c(0.5, 0.5), stringsAsFactors = FALSE)
  expect_warning(
    res_comb <- calculate_padu_index(list(ke = mock_ke, se = map_sf), pu, padu_weights),
    "partial NA"
  )

  expect_equal(res_comb$idx_padu_final[1], 0.5 * 0.4 + 0.5 * 1.0, tolerance = 1e-4)
  # NA idx_padu_se stays NA and is excluded per id_pu (weight rescales to ke = 1.0)
  expect_true(is.na(res_comb$idx_padu_se[2]))
  expect_equal(res_comb$idx_padu_final[2], 0.5, tolerance = 1e-4)
  expect_equal(res_comb$idx_padu_final[3], 0.5 * 0.6 + 0.5 * 0.0, tolerance = 1e-4)
})

test_that("E2E Scenario 2: Multi-facility hybrid vector and raster pipeline", {
  pu <- make_test_pu_sf(3)
  pu$id_pu <- seq_len(nrow(pu))

  pts <- sf::st_sf(
    id = "S1",
    geometry = sf::st_sfc(sf::st_point(c(500, 500)), crs = 3857)
  )
  fac_vector <- build_facility_layer(
    name       = "Sekolah",
    file_input = pts,
    fac_type   = "vector",
    pu         = pu,
    max_dist   = 3000,
    resolution = 100
  )

  r_rs_dist <- make_test_rast(pu, c(3000, 1500, 0))
  fac_raster <- build_facility_layer(
    name       = "Rumah Sakit",
    file_input = r_rs_dist,
    fac_type   = "raster",
    pu         = pu,
    max_dist   = 3000
  )

  res <- calculate_padu_se(
    idx_serasi_map = pu,
    facilities     = list(fac_vector, fac_raster),
    id_col         = "id_pu"
  )

  map_sf <- res$idx_padu_se_map
  tbl_df <- res$idx_padu_se_table

  expected_cols <- c(
    "id_pu", "dist_sekolah", "acc_sekolah",
    "dist_rumah_sakit", "acc_rumah_sakit", "idx_padu_se"
  )
  expect_true(all(expected_cols %in% names(map_sf)))
  expect_true(all(expected_cols %in% names(tbl_df)))
  expect_equal(map_sf$idx_padu_se, (map_sf$acc_sekolah + map_sf$acc_rumah_sakit) / 2)

  tdir <- tempfile("padu_se_e2e_scen2_")
  dir.create(tdir, recursive = TRUE)
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)

  gpkg_p <- file.path(tdir, "idx_padu_se.gpkg")
  xlsx_p <- file.path(tdir, "idx_padu_se.xlsx")
  sf::st_write(map_sf, gpkg_p, quiet = TRUE)
  openxlsx::write.xlsx(tbl_df, xlsx_p)

  gpkg_read <- sf::st_read(gpkg_p, quiet = TRUE)
  xlsx_read <- openxlsx::read.xlsx(xlsx_p)
  expect_true("acc_sekolah" %in% names(gpkg_read))
  expect_true("acc_rumah_sakit" %in% names(xlsx_read))

  mock_ke <- pu %>% dplyr::mutate(idx_padu_ke = c(0.5, 0.5, 0.5))
  padu_weights <- data.frame(code = c("ke", "se"), value = c(0.5, 0.5), stringsAsFactors = FALSE)
  res_comb <- calculate_padu_index(list(ke = mock_ke, se = map_sf), pu, padu_weights)
  expect_equal(res_comb$idx_padu_final, 0.5 * mock_ke$idx_padu_ke + 0.5 * map_sf$idx_padu_se)
})

test_that("keep_intermediates = FALSE removes acc_* columns while keeping dist_* and idx_padu_se", {
  pu <- make_test_pu_sf(3)
  pu$id_pu <- seq_len(nrow(pu))
  r_dist <- make_test_rast(pu, c(500, 1500, 2500))
  fac <- list(name = "Fasilitas Publik", dist_rast = r_dist, max_dist = 5000)

  res_clean <- calculate_padu_se(pu, facilities = list(fac), keep_intermediates = FALSE)
  map_clean <- res_clean$idx_padu_se_map
  tbl_clean <- res_clean$idx_padu_se_table

  expect_true(all(c("dist_fasilitas_publik", "idx_padu_se") %in% names(map_clean)))
  expect_true(all(c("dist_fasilitas_publik", "idx_padu_se") %in% names(tbl_clean)))

  forbidden <- c("ntl", "ntl_norm", "popdens", "popdens_norm",
                 "idx_aktivitas", "idx_aksesibilitas", "acc_fasilitas_publik")
  for (col in forbidden) {
    expect_false(col %in% names(map_clean), info = paste("map should not contain:", col))
    expect_false(col %in% names(tbl_clean), info = paste("table should not contain:", col))
  }
})




