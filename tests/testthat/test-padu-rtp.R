# tests/testthat/test-padu-rtp.R

library(testthat)
library(sf)
library(terra)
library(dplyr)
library(tibble)

make_rtp_pu_sf <- function(n = 3, id_col = "id_pu") {
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
    id = seq_len(n),
    RTRW = paste0("Zone_", seq_len(n)),
    stringsAsFactors = FALSE
  )
  names(df)[1] <- id_col
  sf::st_sf(df, geometry = sfc)
}

make_rtp_rast <- function(pu_sf, values_per_pu) {
  bbox <- sf::st_bbox(pu_sf)
  r <- terra::rast(
    xmin = bbox["xmin"], xmax = bbox["xmax"],
    ymin = bbox["ymin"], ymax = bbox["ymax"],
    resolution = 100, crs = "EPSG:3857"
  )
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

test_that("calculate_padu_rtp combines distance pressure and popdens 50:50 and retains intermediates", {
  pu <- make_rtp_pu_sf(3)
  # PU 1: ind=0, pel=0, pop=100 -> filter_ind=1, filter_pel=1, dist_pressure_mean=1, popdens_norm=1 -> idx=0.0
  # PU 2: ind=4000, pel=2500, pop=50 -> filter_ind=0.5, filter_pel=0.5, dist_pressure_mean=0.5, popdens_norm=0.5 -> idx=0.5
  # PU 3: ind=10000, pel=8000, pop=0 -> filter_ind=0, filter_pel=0, dist_pressure_mean=0, popdens_norm=0 -> idx=1.0
  r_ind <- make_rtp_rast(pu, c(0, 4000, 10000))
  r_pel <- make_rtp_rast(pu, c(0, 2500, 8000))
  r_pop <- make_rtp_rast(pu, c(100, 50, 0))

  res <- calculate_padu_rtp(
    idx_serasi_map     = pu,
    industry_euc_dist  = r_ind,
    pelayaran_euc_dist = r_pel,
    popdens_rast       = r_pop
  )

  expect_type(res, "list")
  expect_named(res, c("idx_padu_rtp_map", "idx_padu_rtp"), ignore.order = TRUE)
  expect_s3_class(res$idx_padu_rtp_map, "sf")
  expect_s3_class(res$idx_padu_rtp, "tbl_df")

  m <- res$idx_padu_rtp_map
  expected_cols <- c(
    "id_pu", "industry_dist_mean", "pelayaran_dist_mean", "popdens",
    "filter_industry", "filter_pelayaran", "dist_pressure_mean",
    "popdens_norm", "idx_padu_rtp"
  )
  expect_true(all(expected_cols %in% names(m)))
  expect_true(all(expected_cols %in% names(res$idx_padu_rtp)))

  expect_equal(m$filter_industry, c(1.0, 0.5, 0.0), tolerance = 1e-4)
  expect_equal(m$filter_pelayaran, c(1.0, 0.5, 0.0), tolerance = 1e-4)
  expect_equal(m$dist_pressure_mean, c(1.0, 0.5, 0.0), tolerance = 1e-4)
  expect_equal(m$popdens, c(100, 50, 0), tolerance = 1e-4)
  expect_equal(m$popdens_norm, c(1.0, 0.5, 0.0), tolerance = 1e-4)
  expect_equal(m$idx_padu_rtp, c(0.0, 0.5, 1.0), tolerance = 1e-4)
})

test_that("calculate_padu_rtp treats offshore NA popdens as 0 and uniform popdens as 0 pressure", {
  pu <- make_rtp_pu_sf(3)
  # Beyond max_dist for both distances -> dist_pressure_mean = 0
  r_ind <- make_rtp_rast(pu, c(10000, 10000, 10000))
  r_pel <- make_rtp_rast(pu, c(8000, 8000, 8000))

  # Case A: Partial NA (offshore marine PU) -> NA becomes 0 people/ha
  r_pop_offshore <- make_rtp_rast(pu, c(NaN, 50, 100))
  res_off <- calculate_padu_rtp(pu, r_ind, r_pel, r_pop_offshore)
  m_off <- res_off$idx_padu_rtp_map

  expect_equal(m_off$popdens, c(0, 50, 100), tolerance = 1e-4)
  expect_equal(m_off$popdens_norm, c(0.0, 0.5, 1.0), tolerance = 1e-4)
  expect_equal(m_off$idx_padu_rtp, c(1.0, 0.75, 0.5), tolerance = 1e-4)

  # Case B: All NA popdens -> all 0 people/ha -> constant 0 -> popdens_norm = 0
  r_pop_all_na <- make_rtp_rast(pu, c(NaN, NaN, NaN))
  res_all_na <- calculate_padu_rtp(pu, r_ind, r_pel, r_pop_all_na)
  expect_equal(res_all_na$idx_padu_rtp_map$popdens, c(0, 0, 0))
  expect_equal(res_all_na$idx_padu_rtp_map$popdens_norm, c(0, 0, 0))
  expect_equal(res_all_na$idx_padu_rtp_map$idx_padu_rtp, c(1.0, 1.0, 1.0))

  # Case C: Uniform non-zero popdens across all PUs -> popdens_norm = 0 (no differential pressure)
  r_pop_const <- make_rtp_rast(pu, c(42, 42, 42))
  res_const <- calculate_padu_rtp(pu, r_ind, r_pel, r_pop_const)
  expect_equal(res_const$idx_padu_rtp_map$popdens, c(42, 42, 42), tolerance = 1e-4)
  expect_equal(res_const$idx_padu_rtp_map$popdens_norm, c(0, 0, 0))
  expect_equal(res_const$idx_padu_rtp_map$idx_padu_rtp, c(1.0, 1.0, 1.0))
})

test_that("calculate_padu_rtp validates inputs and supports custom id_col, CRS, and multi-band rasters", {
  pu <- make_rtp_pu_sf(3, id_col = "custom_id")
  r_ind <- make_rtp_rast(pu, c(0, 4000, 8000))
  r_pel <- make_rtp_rast(pu, c(0, 2500, 5000))
  r_pop <- make_rtp_rast(pu, c(100, 50, 0))

  expect_error(calculate_padu_rtp(as.data.frame(pu), r_ind, r_pel, r_pop), "sf object")
  expect_error(calculate_padu_rtp(pu[0, ], r_ind, r_pel, r_pop), "at least one feature")
  expect_error(calculate_padu_rtp(pu, r_ind, r_pel, r_pop, id_col = "missing_col"), "not found")
  expect_error(calculate_padu_rtp(pu, r_ind, r_pel, "not_a_raster", id_col = "custom_id"), "popdens_rast")
  expect_error(calculate_padu_rtp(pu, r_ind, r_pel, r_pop, id_col = "custom_id", industry_max_dist = 0), "positive number")
  expect_error(calculate_padu_rtp(pu, r_ind, r_pel, r_pop, id_col = "custom_id", pelayaran_max_dist = -10), "positive number")

  # Reorder and reproject PUs + use multi-band popdens raster
  pu_4326 <- sf::st_transform(pu[c(3, 1, 2), ], 4326)
  r_pop_multi <- terra::rast(list(r_pop, r_pop * 10))

  res <- calculate_padu_rtp(pu_4326, r_ind, r_pel, r_pop_multi, id_col = "custom_id")
  m <- res$idx_padu_rtp_map
  expect_equal(sf::st_crs(m), sf::st_crs(4326))
  expect_equal(m$custom_id, c(3, 1, 2))
  expect_equal(m$idx_padu_rtp, c(1.0, 0.0, 0.5), tolerance = 1e-4)
})

test_that("calculate_padu_rtp exports GPKG/XLSX cleanly and integrates with calculate_padu_index", {
  pu <- make_rtp_pu_sf(3)
  r_ind <- make_rtp_rast(pu, c(0, 4000, 8000))
  r_pel <- make_rtp_rast(pu, c(0, 2500, 5000))
  r_pop <- make_rtp_rast(pu, c(100, 50, 0))

  res <- calculate_padu_rtp(pu, r_ind, r_pel, r_pop)
  map_sf <- res$idx_padu_rtp_map
  tbl_df <- res$idx_padu_rtp

  tdir <- tempfile("padu_rtp_e2e_")
  dir.create(tdir, recursive = TRUE)
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)

  gpkg_p <- file.path(tdir, "idx_padu_rtp.gpkg")
  xlsx_p <- file.path(tdir, "idx_padu_rtp.xlsx")
  sf::st_write(map_sf, gpkg_p, quiet = TRUE)
  openxlsx::write.xlsx(tbl_df, xlsx_p)

  gpkg_read <- sf::st_read(gpkg_p, quiet = TRUE)
  xlsx_read <- openxlsx::read.xlsx(xlsx_p)
  expected_cols <- c(
    "industry_dist_mean", "pelayaran_dist_mean", "popdens",
    "filter_industry", "filter_pelayaran", "dist_pressure_mean",
    "popdens_norm", "idx_padu_rtp"
  )
  expect_true(all(expected_cols %in% names(gpkg_read)))
  expect_true(all(expected_cols %in% names(xlsx_read)))
  expect_equal(gpkg_read$idx_padu_rtp, c(0.0, 0.5, 1.0), tolerance = 1e-4)
  expect_equal(xlsx_read$idx_padu_rtp, c(0.0, 0.5, 1.0), tolerance = 1e-4)

  # Downstream calculate_padu_index integration
  pu_comb <- pu %>% dplyr::mutate(fungsi = c("lindung", "budidaya", "campuran"))
  mock_ke <- pu %>% dplyr::mutate(idx_padu_ke = c(0.6, 0.6, 0.6))
  weights <- data.frame(Kode = c("ke", "rtp"), Bobot = c(0.5, 0.5), stringsAsFactors = FALSE)
  res_comb <- calculate_padu_index(list(ke = mock_ke, rtp = map_sf), pu_comb, weights)
  expect_equal(res_comb$idx_padu_budidaya[2], 0.5, tolerance = 1e-4)
  expect_equal(res_comb$idx_padu_final[2], 0.5, tolerance = 1e-4)
  expect_equal(res_comb$idx_padu_final[3], 0.5 * 0.6 + 0.5 * 1.0, tolerance = 1e-4)
})

