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

test_that("R/init.R exists and initializes the environment", {
  init_path <- file.path(proj_root, "R", "init.R")
  expect_true(file.exists(init_path))
  
  # Source init.R
  expect_no_error(source(init_path, local = FALSE))
})

test_that("storage_service manages module directories and artifact persistence", {
  expect_true(exists("storage_get_module_dir", mode = "function"))
  expect_true(exists("storage_save_spatial", mode = "function"))
  expect_true(exists("storage_check_module_ready", mode = "function"))
  
  test_out_dir <- file.path(tempdir(), paste0("test_out_", as.integer(Sys.time())))
  dir.create(test_out_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(test_out_dir, recursive = TRUE), add = TRUE)
  
  # Check directory resolution for standard modules
  padu_kh_dir <- storage_get_module_dir(test_out_dir, "padu_kh", create = TRUE)
  expect_true(dir.exists(padu_kh_dir))
  expect_match(basename(padu_kh_dir), "Analisis PADU-KH")
  
  # Create a dummy sf polygon to save
  poly <- sf::st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol = 2, byrow = TRUE)))
  test_sf <- sf::st_sf(id = 1, name = "Parcel A", geom = sf::st_sfc(poly, crs = 4326))
  
  # Save spatial artifacts
  saved <- storage_save_spatial(test_sf, padu_kh_dir, base_filename = "idx_padu_kh")
  expect_true(file.exists(saved$gpkg))
  expect_true(file.exists(saved$xlsx))
  expect_true(file.exists(saved$rds))
  
  # Verify status check
  expect_true(storage_check_module_ready(test_out_dir, "padu_kh"))
  expect_false(storage_check_module_ready(test_out_dir, "non_existent_module"))
})

test_that("io_validators and formatters work correctly", {
  expect_true(exists("validate_output_dir", mode = "function"))
  expect_true(exists("ensure_geometry_name", mode = "function"))
  expect_true(exists("fmt2", mode = "function"))
  expect_true(exists("laspur_format_numbers", mode = "function"))
  
  # Test fmt2 formatting
  expect_equal(fmt2(12.3456), "12.35")
  expect_equal(fmt2(12), "12.00")
  expect_true(is.na(fmt2(NA)))
  
  # Test ensure_geometry_name
  poly <- sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)))
  sf_custom <- sf::st_sf(id = 1, the_geom = sf::st_sfc(poly, crs = 4326))
  sf_renamed <- ensure_geometry_name(sf_custom)
  expect_equal(attr(sf_renamed, "sf_column"), "geometry")
})

test_that("spatial_primitives execute pure geometric operations", {
  expect_true(exists("st_get_precision_for_meters", mode = "function"))
  expect_true(exists("handle_geom_collection", mode = "function"))
  expect_true(exists("calculate_euclidean_dist", mode = "function"))
  expect_true(exists("extract_raster_to_sf", mode = "function"))
  expect_true(exists("calculate_overlay_pct", mode = "function"))
  
  # Test precision
  poly <- sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)))
  sf_poly <- sf::st_sf(id = 1, geometry = sf::st_sfc(poly, crs = 4326))
  prec <- st_get_precision_for_meters(sf_poly, m_precision = 10, silent = TRUE)
  expect_true(is.numeric(prec))
  expect_gt(prec, 0)
})
