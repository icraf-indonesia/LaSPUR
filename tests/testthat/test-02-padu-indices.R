library(testthat)
library(sf)
library(terra)
library(dplyr)
library(tibble)

# Determine project root safely
cur_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
if (basename(cur_dir) == "testthat") {
  proj_root <- dirname(dirname(cur_dir))
} else if (basename(cur_dir) == "tests") {
  proj_root <- dirname(cur_dir)
} else {
  proj_root <- cur_dir
}

if (file.exists(file.path(proj_root, "R/init.R"))) {
  source(file.path(proj_root, "R/init.R"), local = FALSE)
}

make_square_sf <- function(id = 1, xmin = 0, ymin = 0, xmax = 1000, ymax = 1000, crs = 32751) {
  poly <- sf::st_polygon(list(matrix(c(
    xmin, ymin,
    xmax, ymin,
    xmax, ymax,
    xmin, ymax,
    xmin, ymin
  ), ncol = 2, byrow = TRUE)))
  sf::st_sf(
    id_pu = as.integer(id),
    area_ha = as.numeric((xmax - xmin) * (ymax - ymin)) / 10000,
    geometry = sf::st_sfc(poly, crs = crs)
  )
}

test_that("All 7 PADU calculation functions exist in the environment", {
  expect_true(exists("calculate_padu_ke", mode = "function"))
  expect_true(exists("calculate_padu_hs", mode = "function"))
  expect_true(exists("calculate_padu_kl", mode = "function"))
  expect_true(exists("calculate_padu_kh", mode = "function"))
  expect_true(exists("calculate_padu_rtp", mode = "function"))
  expect_true(exists("calculate_padu_se", mode = "function"))
  expect_true(exists("calculate_padu_ki", mode = "function"))
  expect_true(exists("calculate_padu_index", mode = "function"))
  expect_true(exists("validate_padu_combine_serasi", mode = "function"))
  expect_true(exists("standardize_padu_result", mode = "function"))
})

test_that("calculate_padu_kl operates as a pure function and conforms to standardized contract", {
  pu <- make_square_sf(id = 1, xmin = 0, ymin = 0, xmax = 1000, ymax = 1000)
  # Half overlapping protected area
  protected <- make_square_sf(id = 99, xmin = 0, ymin = 0, xmax = 500, ymax = 1000)
  
  res <- calculate_padu_kl(pu, protected)
  
  expect_type(res, "list")
  expect_true(all(c("data", "score_col", "summary", "metadata") %in% names(res)))
  expect_equal(res$score_col, "idx_padu_kl")
  expect_s3_class(res$data, "sf")
  expect_s3_class(res$summary, "tbl_df")
  expect_equal(nrow(res$data), 1)
  expect_equal(res$data$idx_padu_kl, 0.5, tolerance = 1e-4)
})

test_that("calculate_padu_kh operates as a pure function and conforms to standardized contract", {
  pu <- make_square_sf(id = 1, xmin = 0, ymin = 0, xmax = 1000, ymax = 1000)
  # Quarter overlapping habitat area
  habitat <- make_square_sf(id = 99, xmin = 0, ymin = 0, xmax = 500, ymax = 500)
  
  res <- calculate_padu_kh(pu, habitat_area = habitat)
  
  expect_type(res, "list")
  expect_true(all(c("data", "score_col", "summary", "metadata") %in% names(res)))
  expect_equal(res$score_col, "idx_padu_kh")
  expect_s3_class(res$data, "sf")
  expect_equal(res$data$idx_padu_kh, 0.25, tolerance = 1e-4)
})

test_that("calculate_padu_index synthesizes multiple indicators with pillar rules", {
  pu <- make_square_sf(id = 1)
  pu$fungsi <- "lindung"
  
  # Mock indicator data frames
  df_ke <- tibble::tibble(id_pu = 1L, idx_padu_ke = 0.8)
  df_hs <- tibble::tibble(id_pu = 1L, idx_padu_hs = 0.6)
  df_se <- tibble::tibble(id_pu = 1L, idx_padu_se = 0.4)
  
  comp <- calculate_padu_index(
    padu_list = list(df_ke, df_hs, df_se),
    idx_padu_map = pu
  )
  
  expect_s3_class(comp, "sf")
  expect_true("idx_padu_final" %in% names(comp))
  expect_true("idx_padu_lindung" %in% names(comp))
  expect_true("idx_padu_budidaya" %in% names(comp))
  
  # For Kawasan Lindung, idx_padu_final matches idx_padu_lindung
  expect_equal(comp$idx_padu_final, comp$idx_padu_lindung)
})

test_that("04_analysis_interdependent exists as a placeholder stub", {
  expect_true(exists("analyze_interdependent_units", mode = "function"))
  pu <- make_square_sf(id = 1)
  stub_res <- analyze_interdependent_units(pu)
  expect_equal(attr(stub_res, "interdependent_analysis_status"), "placeholder_stub")
})
