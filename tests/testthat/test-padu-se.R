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

test_that("calculate_padu_se exists and validates inputs", {
  expect_true(exists("calculate_padu_se", mode = "function"))
  
  pu <- make_test_pu_sf(3)
  r_ntl <- make_test_rast(pu, c(10, 20, 30))
  r_pop <- make_test_rast(pu, c(100, 500, 900))
  
  # Non-sf input for idx_serasi_map
  expect_error(
    calculate_padu_se(as.data.frame(pu), r_ntl, r_pop),
    "sf object"
  )
  
  # Empty sf
  expect_error(
    calculate_padu_se(pu[0, ], r_ntl, r_pop),
    "at least one feature"
  )
  
  # Missing id_col
  expect_error(
    calculate_padu_se(pu, r_ntl, r_pop, id_col = "non_existent_id"),
    "Column 'non_existent_id' not found"
  )
  
  # Duplicate id_col values
  pu_dup <- pu
  pu_dup$id_pu[2] <- pu_dup$id_pu[1]
  expect_error(
    calculate_padu_se(pu_dup, r_ntl, r_pop),
    "duplicate values"
  )
  
  # Missing CRS
  pu_nocrs <- pu
  sf::st_crs(pu_nocrs) <- NA
  expect_error(
    calculate_padu_se(pu_nocrs, r_ntl, r_pop),
    "defined CRS"
  )
  
  # Empty geometry
  pu_empty_geom <- pu
  sf::st_geometry(pu_empty_geom)[1] <- sf::st_polygon()
  expect_error(
    calculate_padu_se(pu_empty_geom, r_ntl, r_pop),
    "empty geometries"
  )
  
  # Invalid ntl_rast
  expect_error(
    calculate_padu_se(pu, "invalid_raster", r_pop),
    "ntl_rast"
  )
  
  # Invalid popdens_rast
  expect_error(
    calculate_padu_se(pu, r_ntl, "invalid_raster"),
    "popdens_rast"
  )
  
  # Invalid facilities format
  expect_error(
    calculate_padu_se(pu, r_ntl, r_pop, facilities = "not_a_list"),
    "facilities must be a list"
  )
  expect_error(
    calculate_padu_se(pu, r_ntl, r_pop, facilities = list("not_a_sublist")),
    "must be a list"
  )
  
  # Invalid max_dist
  expect_error(
    calculate_padu_se(pu, r_ntl, r_pop, facilities = list(list(name = "X", dist_rast = r_ntl, max_dist = -100))),
    "positive number"
  )
  expect_error(
    calculate_padu_se(pu, r_ntl, r_pop, facilities = list(list(name = "X", dist_rast = r_ntl, max_dist = 0))),
    "positive number"
  )
})

test_that("calculate_padu_se backward compatibility with facilities = list()", {
  pu <- make_test_pu_sf(3)
  r_ntl <- make_test_rast(pu, c(10, 20, 30))
  r_pop <- make_test_rast(pu, c(100, 500, 900))
  
  res <- calculate_padu_se(
    idx_serasi_map = pu,
    ntl_rast       = r_ntl,
    popdens_rast   = r_pop,
    facilities     = list(),
    id_col         = "id_pu"
  )
  
  # Structure checks
  expect_type(res, "list")
  expect_named(res, c("idx_padu_se_map", "idx_padu_se_table", "idx_padu_se"), ignore.order = TRUE)
  expect_s3_class(res$idx_padu_se_map, "sf")
  expect_s3_class(res$idx_padu_se_table, "tbl_df")
  expect_equal(nrow(res$idx_padu_se_map), 3)
  expect_equal(nrow(res$idx_padu_se_table), 3)
  
  map_res <- res$idx_padu_se_map
  
  # Required columns present
  expected_cols <- c("id_pu", "ntl", "ntl_norm", "popdens", "popdens_norm",
                     "idx_aktivitas", "idx_aksesibilitas", "idx_padu_se")
  for (col in expected_cols) {
    expect_true(col %in% names(map_res), info = paste("Missing column:", col))
  }
  
  # Min-max normalization checks
  # ntl: c(10, 20, 30) -> norm: c(0.0, 0.5, 1.0)
  expect_equal(map_res$ntl_norm, c(0.0, 0.5, 1.0), tolerance = 1e-4)
  # popdens: c(100, 500, 900) -> norm: c(0.0, 0.5, 1.0)
  expect_equal(map_res$popdens_norm, c(0.0, 0.5, 1.0), tolerance = 1e-4)
  # idx_aktivitas = (ntl_norm + popdens_norm) / 2 -> c(0.0, 0.5, 1.0)
  expect_equal(map_res$idx_aktivitas, c(0.0, 0.5, 1.0), tolerance = 1e-4)
  
  # When facilities is empty:
  # idx_padu_se is identical to idx_aktivitas
  expect_equal(map_res$idx_padu_se, map_res$idx_aktivitas)
  # idx_aksesibilitas is all NA_real_
  expect_true(all(is.na(map_res$idx_aksesibilitas)))
  
  # Original attributes preserved
  expect_true("RTRW" %in% names(map_res))
  expect_equal(map_res$RTRW, pu$RTRW)
})

test_that("calculate_padu_se works with custom id_col", {
  pu <- make_test_pu_sf(3, id_col = "custom_id")
  r_ntl <- make_test_rast(pu, c(5, 10, 15))
  r_pop <- make_test_rast(pu, c(200, 400, 600))
  
  res <- calculate_padu_se(
    idx_serasi_map = pu,
    ntl_rast       = r_ntl,
    popdens_rast   = r_pop,
    facilities     = list(),
    id_col         = "custom_id"
  )
  
  expect_true("custom_id" %in% names(res$idx_padu_se_map))
  expect_equal(res$idx_padu_se_map$custom_id, pu$custom_id)
  expect_equal(res$idx_padu_se_map$idx_padu_se, c(0.0, 0.5, 1.0), tolerance = 1e-4)
})

test_that("calculate_padu_se handles constant/uniform raster values gracefully", {
  pu <- make_test_pu_sf(3)
  # Uniform NTL (constant value 25)
  r_ntl_const <- make_test_rast(pu, c(25, 25, 25))
  r_pop <- make_test_rast(pu, c(10, 20, 30))
  
  res <- calculate_padu_se(
    idx_serasi_map = pu,
    ntl_rast       = r_ntl_const,
    popdens_rast   = r_pop,
    facilities     = list()
  )
  
  # When max == min, normalized value must be set to 1.0
  expect_equal(res$idx_padu_se_map$ntl_norm, c(1.0, 1.0, 1.0))
  # popdens_norm: c(0.0, 0.5, 1.0)
  # idx_aktivitas = (1.0 + c(0.0, 0.5, 1.0)) / 2 = c(0.5, 0.75, 1.0)
  expect_equal(res$idx_padu_se_map$idx_aktivitas, c(0.5, 0.75, 1.0), tolerance = 1e-4)
  expect_equal(res$idx_padu_se_map$idx_padu_se, res$idx_padu_se_map$idx_aktivitas)
})

test_that("calculate_padu_se processes single and multi-facility decay correctly", {
  pu <- make_test_pu_sf(3)
  r_ntl <- make_test_rast(pu, c(10, 20, 30))      # norm: 0, 0.5, 1
  r_pop <- make_test_rast(pu, c(100, 500, 900))   # norm: 0, 0.5, 1
  # Activity index: c(0, 0.5, 1)
  
  # Facility 1: Distances = c(0, 500, 1500) m, max_dist = 1000 m
  # acc_1 = max(0, 1 - min(dist / 1000, 1))
  # Dist 0 -> acc = 1.0
  # Dist 500 -> acc = 0.5
  # Dist 1500 -> acc = 0.0
  r_dist_fac1 <- make_test_rast(pu, c(0, 500, 1500))
  
  fac1 <- list(
    name = "Rumah Sakit",
    dist_rast = r_dist_fac1,
    max_dist = 1000
  )
  
  res_single <- calculate_padu_se(
    idx_serasi_map = pu,
    ntl_rast       = r_ntl,
    popdens_rast   = r_pop,
    facilities     = list(fac1)
  )
  
  map_single <- res_single$idx_padu_se_map
  expect_true("dist_rumah_sakit" %in% names(map_single))
  expect_true("acc_rumah_sakit" %in% names(map_single))
  expect_equal(map_single$dist_rumah_sakit, c(0, 500, 1500), tolerance = 1e-3)
  expect_equal(map_single$acc_rumah_sakit, c(1.0, 0.5, 0.0), tolerance = 1e-3)
  expect_equal(map_single$idx_aksesibilitas, c(1.0, 0.5, 0.0), tolerance = 1e-3)
  
  # idx_padu_se = 0.5 * idx_aktivitas + 0.5 * idx_aksesibilitas
  # PU 1: 0.5 * 0.0 + 0.5 * 1.0 = 0.5
  # PU 2: 0.5 * 0.5 + 0.5 * 0.5 = 0.5
  # PU 3: 0.5 * 1.0 + 0.5 * 0.0 = 0.5
  expect_equal(map_single$idx_padu_se, c(0.5, 0.5, 0.5), tolerance = 1e-3)
  
  # Facility 2: Distances = c(2000, 1000, 0) m, max_dist = 2000 m
  # acc_2:
  # Dist 2000 -> acc = 0.0
  # Dist 1000 -> acc = 0.5
  # Dist 0 -> acc = 1.0
  r_dist_fac2 <- make_test_rast(pu, c(2000, 1000, 0))
  fac2 <- list(
    name = "Sekolah Menengah",
    dist_rast = r_dist_fac2,
    max_dist = 2000
  )
  
  res_multi <- calculate_padu_se(
    idx_serasi_map = pu,
    ntl_rast       = r_ntl,
    popdens_rast   = r_pop,
    facilities     = list(fac1, fac2)
  )
  
  map_multi <- res_multi$idx_padu_se_map
  expect_true("dist_sekolah_menengah" %in% names(map_multi))
  expect_true("acc_sekolah_menengah" %in% names(map_multi))
  expect_equal(map_multi$acc_sekolah_menengah, c(0.0, 0.5, 1.0), tolerance = 1e-3)
  
  # idx_aksesibilitas = (acc_1 + acc_2) / 2
  # PU 1: (1.0 + 0.0) / 2 = 0.5
  # PU 2: (0.5 + 0.5) / 2 = 0.5
  # PU 3: (0.0 + 1.0) / 2 = 0.5
  expect_equal(map_multi$idx_aksesibilitas, c(0.5, 0.5, 0.5), tolerance = 1e-3)
  
  # Composite: 0.5 * idx_aktivitas + 0.5 * idx_aksesibilitas
  # PU 1: 0.5 * 0.0 + 0.5 * 0.5 = 0.25
  # PU 2: 0.5 * 0.5 + 0.5 * 0.5 = 0.50
  # PU 3: 0.5 * 1.0 + 0.5 * 0.5 = 0.75
  expect_equal(map_multi$idx_padu_se, c(0.25, 0.50, 0.75), tolerance = 1e-3)
})

test_that("calculate_padu_se handles facility name collision by deduplicating columns", {
  pu <- make_test_pu_sf(3)
  r_ntl <- make_test_rast(pu, c(10, 20, 30))
  r_pop <- make_test_rast(pu, c(100, 500, 900))
  
  # Two facilities with identical names
  r1 <- make_test_rast(pu, c(0, 500, 1000))
  r2 <- make_test_rast(pu, c(1000, 500, 0))
  fac1 <- list(name = "Klinik", dist_rast = r1, max_dist = 1000)
  fac2 <- list(name = "Klinik", dist_rast = r2, max_dist = 1000)
  
  res <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list(fac1, fac2))
  
  # Both facilities should exist with unique sanitized names
  expect_true("dist_klinik" %in% names(res$idx_padu_se_map))
  expect_true("dist_klinik_2" %in% names(res$idx_padu_se_map))
  expect_true("acc_klinik" %in% names(res$idx_padu_se_map))
  expect_true("acc_klinik_2" %in% names(res$idx_padu_se_map))
  
  expect_equal(res$idx_padu_se_map$dist_klinik, c(0, 500, 1000), tolerance = 1e-3)
  expect_equal(res$idx_padu_se_map$dist_klinik_2, c(1000, 500, 0), tolerance = 1e-3)
  expect_equal(res$idx_padu_se_map$acc_klinik, c(1.0, 0.5, 0.0), tolerance = 1e-3)
  expect_equal(res$idx_padu_se_map$acc_klinik_2, c(0.0, 0.5, 1.0), tolerance = 1e-3)
  expect_equal(res$idx_padu_se_map$idx_aksesibilitas, c(0.5, 0.5, 0.5), tolerance = 1e-3)
})

test_that("calculate_padu_se handles partial NA and all-NA raster values gracefully", {
  pu <- make_test_pu_sf(3)
  r_ntl_partial_na <- make_test_rast(pu, c(10, NaN, 30))
  r_pop <- make_test_rast(pu, c(100, 500, 900))
  
  res_partial <- calculate_padu_se(pu, r_ntl_partial_na, r_pop)
  # Second PU should have NA for ntl_norm and idx_aktivitas
  expect_equal(res_partial$idx_padu_se_map$ntl_norm[1], 0.0, tolerance = 1e-4)
  expect_true(is.na(res_partial$idx_padu_se_map$ntl_norm[2]))
  expect_equal(res_partial$idx_padu_se_map$ntl_norm[3], 1.0, tolerance = 1e-4)
  expect_true(is.na(res_partial$idx_padu_se_map$idx_aktivitas[2]))
  expect_true(is.na(res_partial$idx_padu_se_map$idx_padu_se[2]))
  
  # All NA raster
  r_ntl_all_na <- make_test_rast(pu, c(NaN, NaN, NaN))
  res_all_na <- calculate_padu_se(pu, r_ntl_all_na, r_pop)
  expect_true(all(is.na(res_all_na$idx_padu_se_map$ntl_norm)))
  expect_true(all(is.na(res_all_na$idx_padu_se_map$idx_aktivitas)))
  expect_true(all(is.na(res_all_na$idx_padu_se_map$idx_padu_se)))
})

test_that("calculate_padu_se preserves original CRS and exact geometries when rasters have different CRS", {
  pu_3857 <- make_test_pu_sf(3)
  r_ntl <- make_test_rast(pu_3857, c(10, 20, 30))
  r_pop <- make_test_rast(pu_3857, c(100, 500, 900))
  
  # Transform PU to EPSG:4326
  pu_4326 <- sf::st_transform(pu_3857, 4326)
  
  res <- calculate_padu_se(pu_4326, r_ntl, r_pop)
  
  # Original CRS (EPSG:4326) must be preserved
  expect_equal(sf::st_crs(res$idx_padu_se_map), sf::st_crs(4326))
  # Exact geometry must be preserved
  expect_equal(sf::st_geometry(res$idx_padu_se_map), sf::st_geometry(pu_4326))
  expect_equal(res$idx_padu_se_map$idx_padu_se, c(0.0, 0.5, 1.0), tolerance = 1e-4)
})

test_that("calculate_padu_se strictly respects bounded decay limits (0, d_max/2, d_max, beyond d_max, negative)", {
  # 4 planning units
  pu <- make_test_pu_sf(4)
  r_ntl <- make_test_rast(pu, c(10, 20, 30, 40))
  r_pop <- make_test_rast(pu, c(100, 200, 300, 400))
  # max_dist = 2000
  # PU 1: dist = 0      -> acc = 1.0
  # PU 2: dist = 1000   -> acc = 0.5
  # PU 3: dist = 2000   -> acc = 0.0
  # PU 4: dist = 5000   -> acc = 0.0 (strictly 0.0, not negative)
  r_dist <- make_test_rast(pu, c(0, 1000, 2000, 5000))
  fac <- list(name = "RSUD", dist_rast = r_dist, max_dist = 2000)

  res <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list(fac))
  map_res <- res$idx_padu_se_map

  expect_equal(map_res$dist_rsud, c(0, 1000, 2000, 5000), tolerance = 1e-3)
  expect_equal(map_res$acc_rsud, c(1.0, 0.5, 0.0, 0.0), tolerance = 1e-4)
  expect_equal(map_res$idx_aksesibilitas, c(1.0, 0.5, 0.0, 0.0), tolerance = 1e-4)

  # Check 50:50 combination
  # Activity norm: c(0.0, 1/3, 2/3, 1.0)
  expected_akt <- seq(0, 1, length.out = 4)
  expect_equal(map_res$idx_aktivitas, expected_akt, tolerance = 1e-4)
  expected_padu <- 0.5 * expected_akt + 0.5 * c(1.0, 0.5, 0.0, 0.0)
  expect_equal(map_res$idx_padu_se, expected_padu, tolerance = 1e-4)
})

test_that("calculate_padu_se handles planning units completely beyond d_max for all facilities", {
  pu <- make_test_pu_sf(3)
  r_ntl <- make_test_rast(pu, c(10, 20, 30))
  r_pop <- make_test_rast(pu, c(100, 500, 900))

  # PU 1: within range of fac1, PU 2 & 3: completely beyond d_max for ALL facilities
  # fac1 (max = 1000): dists = c(200, 1500, 3000) -> acc = c(0.8, 0.0, 0.0)
  # fac2 (max = 500):  dists = c(100, 1000, 2000) -> acc = c(0.8, 0.0, 0.0)
  r_dist1 <- make_test_rast(pu, c(200, 1500, 3000))
  r_dist2 <- make_test_rast(pu, c(100, 1000, 2000))

  fac1 <- list(name = "Kesehatan", dist_rast = r_dist1, max_dist = 1000)
  fac2 <- list(name = "Pendidikan", dist_rast = r_dist2, max_dist = 500)

  res <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list(fac1, fac2))
  map_res <- res$idx_padu_se_map

  # PU 2 and PU 3 must have idx_aksesibilitas == 0.0
  expect_equal(map_res$acc_kesehatan, c(0.8, 0.0, 0.0), tolerance = 1e-4)
  expect_equal(map_res$acc_pendidikan, c(0.8, 0.0, 0.0), tolerance = 1e-4)
  expect_equal(map_res$idx_aksesibilitas, c(0.8, 0.0, 0.0), tolerance = 1e-4)

  # PU 2 & PU 3: idx_padu_se must strictly equal 0.5 * idx_aktivitas
  expect_equal(map_res$idx_padu_se[2], 0.5 * map_res$idx_aktivitas[2], tolerance = 1e-4)
  expect_equal(map_res$idx_padu_se[3], 0.5 * map_res$idx_aktivitas[3], tolerance = 1e-4)
  expect_equal(map_res$idx_padu_se[1], 0.5 * map_res$idx_aktivitas[1] + 0.5 * 0.8, tolerance = 1e-4)
})

test_that("calculate_padu_se aggregates N = 3 and N = 4 facilities correctly", {
  pu <- make_test_pu_sf(2)
  r_ntl <- make_test_rast(pu, c(10, 20))
  r_pop <- make_test_rast(pu, c(50, 100))

  # 3 facilities:
  # fac1 (max 1000): c(0, 1000)    -> acc = c(1.0, 0.0)
  # fac2 (max 2000): c(1000, 1000) -> acc = c(0.5, 0.5)
  # fac3 (max 4000): c(1000, 3000) -> acc = c(0.75, 0.25)
  r1 <- make_test_rast(pu, c(0, 1000))
  r2 <- make_test_rast(pu, c(1000, 1000))
  r3 <- make_test_rast(pu, c(1000, 3000))

  fac1 <- list(name = "F1", dist_rast = r1, max_dist = 1000)
  fac2 <- list(name = "F2", dist_rast = r2, max_dist = 2000)
  fac3 <- list(name = "F3", dist_rast = r3, max_dist = 4000)

  res3 <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list(fac1, fac2, fac3))
  map3 <- res3$idx_padu_se_map

  # Mean across 3 facilities:
  # PU 1: (1.0 + 0.5 + 0.75) / 3 = 2.25 / 3 = 0.75
  # PU 2: (0.0 + 0.5 + 0.25) / 3 = 0.75 / 3 = 0.25
  expect_equal(map3$idx_aksesibilitas, c(0.75, 0.25), tolerance = 1e-4)

  # Activity index: PU1 = 0, PU2 = 1.0
  # Composite:
  # PU 1: 0.5 * 0.0 + 0.5 * 0.75 = 0.375
  # PU 2: 0.5 * 1.0 + 0.5 * 0.25 = 0.625
  expect_equal(map3$idx_padu_se, c(0.375, 0.625), tolerance = 1e-4)

  # Add 4th facility:
  # fac4 (max 1000): c(500, 500) -> acc = c(0.5, 0.5)
  r4 <- make_test_rast(pu, c(500, 500))
  fac4 <- list(name = "F4", dist_rast = r4, max_dist = 1000)

  res4 <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list(fac1, fac2, fac3, fac4))
  map4 <- res4$idx_padu_se_map

  # Mean across 4 facilities:
  # PU 1: (1.0 + 0.5 + 0.75 + 0.5) / 4 = 2.75 / 4 = 0.6875
  # PU 2: (0.0 + 0.5 + 0.25 + 0.5) / 4 = 1.25 / 4 = 0.3125
  expect_equal(map4$idx_aksesibilitas, c(0.6875, 0.3125), tolerance = 1e-4)
  expect_equal(map4$idx_padu_se, 0.5 * map4$idx_aktivitas + 0.5 * c(0.6875, 0.3125), tolerance = 1e-4)
})

test_that("calculate_padu_se handles partial NA and all-NA in facility distance rasters", {
  pu <- make_test_pu_sf(3)
  r_ntl <- make_test_rast(pu, c(10, 20, 30))
  r_pop <- make_test_rast(pu, c(100, 500, 900))

  # Single facility with partial NA
  r_dist_partial <- make_test_rast(pu, c(500, NaN, 1000))
  fac_part <- list(name = "RS", dist_rast = r_dist_partial, max_dist = 1000)

  res_part <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list(fac_part))
  map_part <- res_part$idx_padu_se_map

  expect_equal(map_part$acc_rs[1], 0.5, tolerance = 1e-4)
  expect_true(is.na(map_part$acc_rs[2]))
  expect_equal(map_part$acc_rs[3], 0.0, tolerance = 1e-4)

  # idx_aksesibilitas and idx_padu_se should be NA for PU 2
  expect_equal(map_part$idx_aksesibilitas[1], 0.5, tolerance = 1e-4)
  expect_true(is.na(map_part$idx_aksesibilitas[2]))
  expect_equal(map_part$idx_aksesibilitas[3], 0.0, tolerance = 1e-4)

  expect_equal(map_part$idx_padu_se[1], 0.5 * 0.0 + 0.5 * 0.5, tolerance = 1e-4)
  expect_true(is.na(map_part$idx_padu_se[2]))
  expect_equal(map_part$idx_padu_se[3], 0.5 * 1.0 + 0.5 * 0.0, tolerance = 1e-4)

  # Facility with all NA distance
  r_dist_all_na <- make_test_rast(pu, c(NaN, NaN, NaN))
  fac_all_na <- list(name = "RS", dist_rast = r_dist_all_na, max_dist = 1000)

  res_all_na <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list(fac_all_na))
  map_all_na <- res_all_na$idx_padu_se_map

  expect_true(all(is.na(map_all_na$dist_rs)))
  expect_true(all(is.na(map_all_na$acc_rs)))
  expect_true(all(is.na(map_all_na$idx_aksesibilitas)))
  expect_true(all(is.na(map_all_na$idx_padu_se)))
})

test_that("calculate_padu_se handles constant NTL and Population together with facilities", {
  pu <- make_test_pu_sf(2)
  # Both NTL and Pop are constant
  r_ntl_const <- make_test_rast(pu, c(50, 50))
  r_pop_const <- make_test_rast(pu, c(200, 200))

  r_dist <- make_test_rast(pu, c(0, 1000))
  fac <- list(name = "Fasilitas", dist_rast = r_dist, max_dist = 1000)

  res <- calculate_padu_se(pu, r_ntl_const, r_pop_const, facilities = list(fac))
  map_res <- res$idx_padu_se_map

  # Both activity sub-indicators normalized to 1.0 when constant
  expect_equal(map_res$ntl_norm, c(1.0, 1.0))
  expect_equal(map_res$popdens_norm, c(1.0, 1.0))
  expect_equal(map_res$idx_aktivitas, c(1.0, 1.0))

  # Accessibility: PU1 = 1.0, PU2 = 0.0
  expect_equal(map_res$idx_aksesibilitas, c(1.0, 0.0), tolerance = 1e-4)

  # Composite:
  # PU 1: 0.5 * 1.0 + 0.5 * 1.0 = 1.0
  # PU 2: 0.5 * 1.0 + 0.5 * 0.0 = 0.5
  expect_equal(map_res$idx_padu_se, c(1.0, 0.5), tolerance = 1e-4)
})

test_that("calculate_padu_se defaults max_dist to 5000 when max_dist is omitted", {
  pu <- make_test_pu_sf(2)
  r_ntl <- make_test_rast(pu, c(10, 20))
  r_pop <- make_test_rast(pu, c(100, 200))

  # Distance = 2500m. If default max_dist is 5000m, acc should be 1 - 2500/5000 = 0.5
  r_dist <- make_test_rast(pu, c(2500, 5000))
  fac <- list(name = "DefaultMax", dist_rast = r_dist) # max_dist omitted

  res <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list(fac))
  map_res <- res$idx_padu_se_map

  expect_equal(map_res$acc_defaultmax, c(0.5, 0.0), tolerance = 1e-4)
})

test_that("calculate_padu_se validates dist_rast and max_dist invalid types", {
  pu <- make_test_pu_sf(2)
  r_ntl <- make_test_rast(pu, c(10, 20))
  r_pop <- make_test_rast(pu, c(100, 200))

  # Missing/invalid dist_rast
  expect_error(
    calculate_padu_se(pu, r_ntl, r_pop, facilities = list(list(name = "X", max_dist = 1000))),
    "dist_rast"
  )
  expect_error(
    calculate_padu_se(pu, r_ntl, r_pop, facilities = list(list(name = "X", dist_rast = "bad", max_dist = 1000))),
    "dist_rast"
  )

  # Invalid max_dist (vector, Inf, NaN)
  expect_error(
    calculate_padu_se(pu, r_ntl, r_pop, facilities = list(list(name = "X", dist_rast = r_ntl, max_dist = c(100, 200)))),
    "positive number"
  )
  expect_error(
    calculate_padu_se(pu, r_ntl, r_pop, facilities = list(list(name = "X", dist_rast = r_ntl, max_dist = Inf))),
    "positive number"
  )
  expect_error(
    calculate_padu_se(pu, r_ntl, r_pop, facilities = list(list(name = "X", dist_rast = r_ntl, max_dist = NaN))),
    "positive number"
  )
})

test_that("calculate_padu_se handles unnamed facility list and NA/empty name fallback", {
  pu <- make_test_pu_sf(2)
  r_ntl <- make_test_rast(pu, c(10, 20))
  r_pop <- make_test_rast(pu, c(100, 200))
  r_dist <- make_test_rast(pu, c(0, 1000))

  # 1. Unnamed facilities list where name is omitted in the facility object
  fac_no_name <- list(dist_rast = r_dist, max_dist = 2000)
  res_no_name <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list(fac_no_name))
  map_no_name <- res_no_name$idx_padu_se_map
  expect_true("dist_facility_1" %in% names(map_no_name))
  expect_true("acc_facility_1" %in% names(map_no_name))
  expect_equal(map_no_name$acc_facility_1, c(1.0, 0.5), tolerance = 1e-4)

  # 2. name is NA or empty or character(0)
  fac_na_name <- list(name = NA, dist_rast = r_dist, max_dist = 2000)
  fac_empty_name <- list(name = "   ", dist_rast = r_dist, max_dist = 2000)
  fac_zero_len <- list(name = character(0), dist_rast = r_dist, max_dist = 2000)

  res_multi_fallback <- calculate_padu_se(
    pu, r_ntl, r_pop,
    facilities = list(fac_na_name, fac_empty_name, fac_zero_len)
  )
  map_fallback <- res_multi_fallback$idx_padu_se_map
  expect_true("dist_facility_1" %in% names(map_fallback))
  expect_true("dist_facility_2" %in% names(map_fallback))
  expect_true("dist_facility_3" %in% names(map_fallback))
  expect_false("dist_na" %in% names(map_fallback))
})

test_that("calculate_padu_se handles negative distance nodata gracefully", {
  pu <- make_test_pu_sf(3)
  r_ntl <- make_test_rast(pu, c(10, 20, 30))
  r_pop <- make_test_rast(pu, c(100, 500, 900))

  # Distance has a negative value (-9999) representing nodata
  # PU 1: 500m -> acc = 0.5
  # PU 2: -9999m (nodata) -> acc = NA (must NOT evaluate to 1.0)
  # PU 3: 1000m -> acc = 0.0
  r_dist_neg <- make_test_rast(pu, c(500, -9999, 1000))
  fac_neg <- list(name = "Klinik", dist_rast = r_dist_neg, max_dist = 1000)

  res <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list(fac_neg))
  map_res <- res$idx_padu_se_map

  expect_equal(map_res$acc_klinik[1], 0.5, tolerance = 1e-4)
  expect_true(is.na(map_res$acc_klinik[2]))
  expect_equal(map_res$acc_klinik[3], 0.0, tolerance = 1e-4)
  expect_true(is.na(map_res$idx_aksesibilitas[2]))
  expect_true(is.na(map_res$idx_padu_se[2]))
})

test_that("calculate_padu_se strips matrix rownames from output indicator columns", {
  pu <- make_test_pu_sf(3)
  r_ntl <- make_test_rast(pu, c(10, 20, 30))
  r_pop <- make_test_rast(pu, c(100, 500, 900))
  r_dist <- make_test_rast(pu, c(200, 400, 600))
  fac <- list(name = "Pasar", dist_rast = r_dist, max_dist = 1000)

  res <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list(fac))
  map_res <- res$idx_padu_se_map

  expect_null(names(map_res$idx_aktivitas))
  expect_null(names(map_res$idx_aksesibilitas))
  expect_null(names(map_res$idx_padu_se))
})

test_that("calculate_padu_se handles multi-band rasters by selecting the first band", {
  pu <- make_test_pu_sf(2)
  r_single <- make_test_rast(pu, c(10, 20))
  # Create multi-band raster using terra::rast(list(...))
  r_multi <- terra::rast(list(r_single, r_single * 2, r_single * 3))
  expect_equal(terra::nlyr(r_multi), 3)

  r_dist <- make_test_rast(pu, c(500, 1000))
  r_dist_multi <- terra::rast(list(r_dist, r_dist * 2))

  fac <- list(name = "MultiBandFac", dist_rast = r_dist_multi, max_dist = 2000)

  res <- calculate_padu_se(pu, ntl_rast = r_multi, popdens_rast = r_multi, facilities = list(fac))
  map_res <- res$idx_padu_se_map

  expect_true("dist_multibandfac" %in% names(map_res))
  expect_true("acc_multibandfac" %in% names(map_res))
  expect_equal(map_res$acc_multibandfac, c(0.75, 0.5), tolerance = 1e-4)
  expect_type(map_res$dist_multibandfac, "double")
  expect_type(map_res$acc_multibandfac, "double")
})

test_that("calculate_padu_se handles multipolygon and polygon with hole geometries", {
  # Polygon 1: Multipolygon with two separated parts
  p1_a <- matrix(c(0, 0, 400, 0, 400, 400, 0, 400, 0, 0), ncol = 2, byrow = TRUE)
  p1_b <- matrix(c(600, 0, 1000, 0, 1000, 400, 600, 400, 600, 0), ncol = 2, byrow = TRUE)
  mp1 <- sf::st_multipolygon(list(list(p1_a), list(p1_b)))

  # Polygon 2: Polygon with an interior hole
  outer_ring <- matrix(c(1000, 0, 2000, 0, 2000, 1000, 1000, 1000, 1000, 0), ncol = 2, byrow = TRUE)
  inner_hole <- matrix(c(1300, 300, 1700, 300, 1700, 700, 1300, 700, 1300, 300), ncol = 2, byrow = TRUE)
  poly_hole <- sf::st_polygon(list(outer_ring, inner_hole))

  sfc <- sf::st_sfc(list(mp1, poly_hole), crs = 3857)
  pu_complex <- sf::st_sf(
    id_pu = c("PU_MULTI", "PU_HOLE"),
    geometry = sfc
  )

  r_ntl <- make_test_rast(pu_complex, c(15, 30))
  r_pop <- make_test_rast(pu_complex, c(200, 400))
  r_dist <- make_test_rast(pu_complex, c(500, 1500))

  fac <- list(name = "Terminal", dist_rast = r_dist, max_dist = 2000)

  res <- calculate_padu_se(pu_complex, r_ntl, r_pop, facilities = list(fac))
  map_res <- res$idx_padu_se_map

  expect_equal(nrow(map_res), 2)
  expect_equal(map_res$id_pu, c("PU_MULTI", "PU_HOLE"))
  expect_equal(map_res$acc_terminal, c(0.75, 0.25), tolerance = 1e-4)
  expect_equal(map_res$idx_padu_se, c(0.375, 0.625), tolerance = 1e-4)
})

test_that("calculate_padu_se preserves alignment when planning unit rows are subsetted or reordered", {
  pu <- make_test_pu_sf(4)
  r_ntl <- make_test_rast(pu, c(10, 20, 30, 40))
  r_pop <- make_test_rast(pu, c(100, 200, 300, 400))
  r_dist <- make_test_rast(pu, c(0, 500, 1000, 1500))

  # Reorder rows: 4, 2, 1
  pu_reordered <- pu[c(4, 2, 1), ]
  fac <- list(name = "Bandara", dist_rast = r_dist, max_dist = 2000)

  res <- calculate_padu_se(pu_reordered, r_ntl, r_pop, facilities = list(fac))
  map_res <- res$idx_padu_se_map

  # Row order must be preserved exactly: PU_4, PU_2, PU_1
  expect_equal(map_res$id_pu, c("PU_4", "PU_2", "PU_1"))
  expect_equal(map_res$dist_bandara, c(1500, 500, 0), tolerance = 1e-3)
  expect_equal(map_res$acc_bandara, c(0.25, 0.75, 1.0), tolerance = 1e-4)
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
  
  pu <- make_test_pu_sf(3) # 3 PUs
  r_ntl <- make_test_rast(pu, c(10, 20, 30))
  r_pop <- make_test_rast(pu, c(100, 200, 300))
  
  # Facility 1: Vector points (RS) near PU 1 (500, 500)
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
  
  # Facility 2: Raster distance (Pasar) with precomputed values
  r_dist_pasar <- make_test_rast(pu, c(1000, 500, 0))
  fac2 <- build_facility_layer(
    name       = "Pasar",
    file_input = r_dist_pasar,
    fac_type   = "raster",
    pu         = pu,
    max_dist   = 1000
  )
  
  # Run calculate_padu_se
  calc_res <- calculate_padu_se(
    idx_serasi_map = pu,
    ntl_rast       = r_ntl,
    popdens_rast   = r_pop,
    facilities     = list(fac1, fac2),
    id_col         = "id_pu"
  )
  
  map_res <- calc_res$idx_padu_se_map
  tbl_res <- calc_res$idx_padu_se_table
  
  # Verify all columns exist
  expected_cols <- c(
    "id_pu", "ntl", "ntl_norm", "popdens", "popdens_norm",
    "idx_aktivitas", "dist_rumah_sakit", "acc_rumah_sakit",
    "dist_pasar", "acc_pasar", "idx_aksesibilitas", "idx_padu_se"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(map_res), info = paste("Checking map col:", col))
    expect_true(col %in% names(tbl_res), info = paste("Checking tbl col:", col))
  }
  
  # Verify values of accessibility and composite index
  # PU 1: RS is close (acc near 0.75-0.85), Pasar is at 1000m (acc = 0)
  # idx_aksesibilitas is mean of acc_rumah_sakit and acc_pasar
  expect_equal(map_res$idx_aksesibilitas, (map_res$acc_rumah_sakit + map_res$acc_pasar) / 2, tolerance = 1e-4)
  expect_equal(map_res$idx_padu_se, 0.5 * map_res$idx_aktivitas + 0.5 * map_res$idx_aksesibilitas, tolerance = 1e-4)
  
  # Export to GPKG and XLSX
  gpkg_path <- file.path(tmp_out_dir, "idx_padu_se.gpkg")
  xlsx_path <- file.path(tmp_out_dir, "idx_padu_se.xlsx")
  
  sf::st_write(map_res, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
  openxlsx::write.xlsx(tbl_res, xlsx_path)
  
  expect_true(file.exists(gpkg_path))
  expect_true(file.exists(xlsx_path))
  
  # Read back from GPKG and verify all columns are preserved
  read_gpkg <- sf::st_read(gpkg_path, quiet = TRUE)
  for (col in expected_cols) {
    expect_true(col %in% names(read_gpkg), info = paste("Checking saved GPKG col:", col))
  }
  
  # Read back from Excel and verify columns
  read_xlsx <- openxlsx::read.xlsx(xlsx_path)
  for (col in expected_cols) {
    expect_true(col %in% names(read_xlsx), info = paste("Checking saved Excel col:", col))
  }
})

test_that("Plot generation: idx_aksesibilitas.png is created when facilities are provided", {
  tmp_log_dir <- tempfile("test_log_")
  dir.create(tmp_log_dir, recursive = TRUE)
  on.exit(unlink(tmp_log_dir, recursive = TRUE), add = TRUE)
  
  pu <- make_test_pu_sf(3)
  r_ntl <- make_test_rast(pu, c(10, 20, 30))
  r_pop <- make_test_rast(pu, c(100, 200, 300))
  
  # Case A: With facility
  r_dist <- make_test_rast(pu, c(200, 400, 600))
  fac <- list(name = "Klinik", dist_rast = r_dist, max_dist = 1000)
  res_with_fac <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list(fac))
  
  map_with_fac <- res_with_fac$idx_padu_se_map
  acc_png <- file.path(tmp_log_dir, "idx_aksesibilitas.png")
  
  if ("idx_aksesibilitas" %in% names(map_with_fac) && any(!is.na(map_with_fac$idx_aksesibilitas))) {
    plot_continuous_map(
      map      = map_with_fac,
      column   = "idx_aksesibilitas",
      title    = "Peta Indeks Aksesibilitas Fasilitas",
      legend   = "Indeks Aksesibilitas",
      low      = "red",
      high     = "lightgreen",
      filepath = acc_png
    )
  }
  expect_true(file.exists(acc_png))
  
  # Case B: Without facility (N = 0)
  res_no_fac <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list())
  map_no_fac <- res_no_fac$idx_padu_se_map
  acc_png_none <- file.path(tmp_log_dir, "idx_aksesibilitas_none.png")
  
  # Condition should evaluate to FALSE when idx_aksesibilitas is all NA
  plotted <- FALSE
  if ("idx_aksesibilitas" %in% names(map_no_fac) && any(!is.na(map_no_fac$idx_aksesibilitas))) {
    plotted <- TRUE
  }
  expect_false(plotted)
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
  # Facility is placed 50 km away
  pts_far <- sf::st_sf(
    id = "Far1",
    geometry = sf::st_sfc(sf::st_point(c(50000, 50000)), crs = 3857)
  )
  fac_far <- build_facility_layer("FarAway", pts_far, "vector", pu = pu, max_dist = 5000)
  
  r_ntl <- make_test_rast(pu, c(10, 20))
  r_pop <- make_test_rast(pu, c(100, 200))
  
  res <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list(fac_far))
  map_res <- res$idx_padu_se_map
  
  expect_true(all(map_res$dist_faraway > 5000))
  expect_equal(map_res$acc_faraway, c(0, 0))
  expect_equal(map_res$idx_aksesibilitas, c(0, 0))
  # With idx_aksesibilitas == 0, idx_padu_se == 0.5 * idx_aktivitas
  expect_equal(map_res$idx_padu_se, 0.5 * map_res$idx_aktivitas)
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
# Ticket 04: End-to-End Verification & mod_padu_combine Integration Tests
# ==============================================================================

test_that("Ticket 04 E2E Scenario 1: PADU-SE backward compatibility and mod_padu_combine integration", {
  pu <- make_test_pu_sf(3)
  pu$id_pu <- seq_len(nrow(pu)) # Integer IDs matching LaSPUR convention
  r_ntl <- make_test_rast(pu, c(20, 50, 80))
  r_pop <- make_test_rast(pu, c(100, 300, 500))
  
  # Run calculate_padu_se without facilities
  res <- calculate_padu_se(
    idx_serasi_map = pu,
    ntl_rast       = r_ntl,
    popdens_rast   = r_pop,
    facilities     = list(),
    id_col         = "id_pu"
  )
  
  map_sf <- res$idx_padu_se_map
  tbl_df <- res$idx_padu_se_table
  
  # Verify attributes and backward compatibility
  expect_equal(map_sf$idx_padu_se, map_sf$idx_aktivitas)
  expect_true(all(is.na(map_sf$idx_aksesibilitas)))
  
  # Save to disk as GPKG and XLSX
  tdir <- tempfile("padu_se_e2e_scen1_")
  dir.create(tdir, recursive = TRUE)
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)
  
  gpkg_p <- file.path(tdir, "idx_padu_se.gpkg")
  xlsx_p <- file.path(tdir, "idx_padu_se.xlsx")
  sf::st_write(map_sf, gpkg_p, quiet = TRUE)
  openxlsx::write.xlsx(tbl_df, xlsx_p)
  
  expect_true(file.exists(gpkg_p))
  expect_true(file.exists(xlsx_p))
  
  # Read back from GPKG and XLSX to ensure serialization fidelity
  gpkg_read <- sf::st_read(gpkg_p, quiet = TRUE)
  xlsx_read <- openxlsx::read.xlsx(xlsx_p)
  expect_equal(nrow(gpkg_read), 3)
  expect_equal(nrow(xlsx_read), 3)
  expect_equal(gpkg_read$idx_padu_se, map_sf$idx_padu_se, tolerance = 1e-6)
  expect_equal(xlsx_read$idx_padu_se, tbl_df$idx_padu_se, tolerance = 1e-6)
  
  # Downstream compatibility with calculate_padu_index (mod_padu_combine)
  # Simulate other dummy PADU indices
  mock_ke <- pu %>% dplyr::mutate(idx_padu_ke = c(0.4, 0.5, 0.6))
  mock_kh <- pu %>% dplyr::mutate(idx_padu_kh = c(0.7, 0.8, 0.9))
  
  padu_list <- list(
    ke = mock_ke,
    kh = mock_kh,
    se = map_sf
  )
  
  # In mod_padu_combine:
  # calculate_padu_index uses grep("^idx_padu_", names(y))[1]
  # Verify that idx_padu_se is uniquely detected
  se_cols <- names(map_sf)[grepl("^idx_padu_", names(map_sf))]
  expect_equal(se_cols, "idx_padu_se")
  
  padu_weights <- data.frame(
    code  = c("ke", "kh", "se"),
    value = c(0.4, 0.3, 0.3),
    stringsAsFactors = FALSE
  )
  
  res_comb <- calculate_padu_index(
    padu_list       = padu_list,
    idx_padu_map    = pu,
    padu_idx_weight = padu_weights
  )
  
  expect_true("idx_padu_final" %in% names(res_comb))
  expect_equal(nrow(res_comb), 3)
  expect_true(all(is.finite(res_comb$idx_padu_final)))
})

test_that("Ticket 04 E2E Scenario 2: Multi-facility hybrid vector and raster pipeline", {
  pu <- make_test_pu_sf(3)
  pu$id_pu <- seq_len(nrow(pu)) # Integer IDs matching LaSPUR convention
  r_ntl <- make_test_rast(pu, c(10, 40, 90))
  r_pop <- make_test_rast(pu, c(50, 150, 250))
  
  # Facility 1: Vector Point (Sekolah) located at (500, 500) in PU 1
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
  
  # Facility 2: Pre-computed Distance Raster (Rumah Sakit)
  # Distances: PU 1 = 3000m, PU 2 = 1500m, PU 3 = 0m
  r_rs_dist <- make_test_rast(pu, c(3000, 1500, 0))
  fac_raster <- build_facility_layer(
    name       = "Rumah Sakit",
    file_input = r_rs_dist,
    fac_type   = "raster",
    pu         = pu,
    max_dist   = 3000
  )
  
  # Run calculate_padu_se with both facilities
  res <- calculate_padu_se(
    idx_serasi_map = pu,
    ntl_rast       = r_ntl,
    popdens_rast   = r_pop,
    facilities     = list(fac_vector, fac_raster),
    id_col         = "id_pu"
  )
  
  map_sf <- res$idx_padu_se_map
  tbl_df <- res$idx_padu_se_table
  
  # Check expected columns
  expected_cols <- c(
    "id_pu", "ntl", "ntl_norm", "popdens", "popdens_norm", "idx_aktivitas",
    "dist_sekolah", "acc_sekolah", "dist_rumah_sakit", "acc_rumah_sakit",
    "idx_aksesibilitas", "idx_padu_se"
  )
  expect_true(all(expected_cols %in% names(map_sf)))
  expect_true(all(expected_cols %in% names(tbl_df)))
  
  # Check mathematical values:
  # In PU 1: acc_sekolah is near 1.0 (mean dist < 400m), acc_rumah_sakit = 0.0 (dist = 3000m == max_dist)
  # idx_aksesibilitas is exact mean of acc_sekolah and acc_rumah_sakit
  expect_equal(map_sf$idx_aksesibilitas, (map_sf$acc_sekolah + map_sf$acc_rumah_sakit) / 2)
  # idx_padu_se is exactly 0.5 * idx_aktivitas + 0.5 * idx_aksesibilitas
  expect_equal(map_sf$idx_padu_se, 0.5 * map_sf$idx_aktivitas + 0.5 * map_sf$idx_aksesibilitas)
  
  # Save to disk
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
  expect_true("idx_aksesibilitas" %in% names(xlsx_read))
  
  # Downstream compatibility with calculate_padu_index
  mock_ke <- pu %>% dplyr::mutate(idx_padu_ke = c(0.5, 0.5, 0.5))
  padu_list <- list(ke = mock_ke, se = map_sf)
  padu_weights <- data.frame(code = c("ke", "se"), value = c(0.5, 0.5), stringsAsFactors = FALSE)
  
  res_comb <- calculate_padu_index(padu_list, pu, padu_weights)
  expect_true("idx_padu_final" %in% names(res_comb))
  expect_equal(res_comb$idx_padu_final, 0.5 * mock_ke$idx_padu_ke + 0.5 * map_sf$idx_padu_se)
})

test_that("Revision 8: keep_intermediates = FALSE removes normalized, score, and intermediate index columns", {
  pu <- make_test_pu_sf(3)
  pu$id_pu <- seq_len(nrow(pu))
  r_ntl <- make_test_rast(pu, c(10, 20, 30))
  r_pop <- make_test_rast(pu, c(100, 200, 300))
  r_dist <- make_test_rast(pu, c(500, 1500, 2500))
  fac <- list(name = "Fasilitas Publik", dist_rast = r_dist, max_dist = 5000)
  
  res_clean <- calculate_padu_se(pu, r_ntl, r_pop, facilities = list(fac), keep_intermediates = FALSE)
  map_clean <- res_clean$idx_padu_se_map
  tbl_clean <- res_clean$idx_padu_se_table
  
  # Preserved columns
  expect_true(all(c("ntl", "popdens", "dist_fasilitas_publik", "idx_padu_se") %in% names(map_clean)))
  expect_true(all(c("ntl", "popdens", "dist_fasilitas_publik", "idx_padu_se") %in% names(tbl_clean)))
  
  # Removed columns (Revision 8 requirement)
  forbidden <- c("ntl_norm", "popdens_norm", "idx_aktivitas", "idx_aksesibilitas", "acc_fasilitas_publik")
  for (col in forbidden) {
    expect_false(col %in% names(map_clean), info = paste("map should not contain:", col))
    expect_false(col %in% names(tbl_clean), info = paste("table should not contain:", col))
  }
})




