test_that("ensure_geometry_name renames non-standard geometry column to 'geometry'", {
  # Create synthetic sf with custom geometry column name 'geom'
  poly <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  sfc <- sf::st_sfc(poly, crs = 4326)
  df <- data.frame(id = 1, val = "A")
  sf_obj <- sf::st_sf(df, geom = sfc)
  
  expect_equal(attr(sf_obj, "sf_column"), "geom")
  expect_true("geom" %in% names(sf_obj))
  
  # Standardize
  standardized <- ensure_geometry_name(sf_obj)
  expect_equal(attr(standardized, "sf_column"), "geometry")
  expect_true("geometry" %in% names(standardized))
  expect_false("geom" %in% names(standardized))
})

test_that("ensure_geometry_name leaves already standard geometry untouched", {
  poly <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  sf_obj <- sf::st_sf(id = 1, geometry = sf::st_sfc(poly, crs = 4326))
  
  standardized <- ensure_geometry_name(sf_obj)
  expect_equal(attr(standardized, "sf_column"), "geometry")
  expect_equal(standardized, sf_obj)
})

test_that("normalize_legacy_ids prefixes integer IDs with 'R' and 'Z'", {
  # Feature-level adjacent form: 2 rows per id_pu, one RTRW, one RZWP3K
  df_legacy <- data.frame(
    id = c("1", "1", "2", "2"),
    id_pu = c(1, 1, 2, 2),
    RTRW = c("Hutan", NA, "Permukiman", NA),
    RZWP3K = c(NA, "Zona Tangkap", NA, "Zona Pelabuhan"),
    stringsAsFactors = FALSE
  )
  
  result <- normalize_legacy_ids(df_legacy)
  expect_equal(result$id, c("R1", "Z1", "R2", "Z2"))
})

test_that("normalize_legacy_ids ignores already namespaced or non-applicable data", {
  # Already namespaced
  df_namespaced <- data.frame(
    id = c("R1", "Z1"),
    id_pu = c(1, 1),
    RTRW = c("Hutan", NA),
    RZWP3K = c(NA, "Zona Tangkap"),
    stringsAsFactors = FALSE
  )
  expect_equal(normalize_legacy_ids(df_namespaced)$id, c("R1", "Z1"))
  
  # Missing required columns
  df_other <- data.frame(id = c("1", "2"), val = c("a", "b"))
  expect_equal(normalize_legacy_ids(df_other), df_other)
  
  # NULL input
  expect_null(normalize_legacy_ids(NULL))
})

test_that("validate_output_dir validates directory existence", {
  expect_false(validate_output_dir(NULL))
  expect_false(validate_output_dir(""))
  
  non_existent_dir <- file.path(tempdir(), paste0("non_existent_", as.integer(runif(1, 1e5, 9e5))))
  expect_false(validate_output_dir(non_existent_dir))
  
  existing_dir <- tempdir()
  expect_true(validate_output_dir(existing_dir))
})

test_that("load_validate_matrix_table enforces extensions and file existence", {
  expect_error(load_validate_matrix_table("non_existent_matrix.csv"), "Invalid extension")
  expect_error(load_validate_matrix_table("non_existent_matrix.xlsx"), "File not found")
})

test_that("load_validate_matrix_table validates dimensions and converts to long format", {
  skip_if_not_installed("openxlsx")
  
  temp_xlsx <- file.path(tempdir(), paste0("test_matrix_", as.integer(runif(1, 1e5, 9e5)), ".xlsx"))
  on.exit(unlink(temp_xlsx), add = TRUE)
  
  # Valid 2x2 matrix (headers + 2 classes)
  # Row 1: Header (RTRW/RZWP, Laut, Pesisir)
  # Row 2: Hutan, 1, 2
  # Row 3: Sawah, 3, 4
  mat_data <- data.frame(
    C1 = c("Header", "Hutan", "Sawah"),
    C2 = c("Laut", 1, 3),
    C3 = c("Pesisir", 2, 4),
    stringsAsFactors = FALSE
  )
  openxlsx::write.xlsx(mat_data, file = temp_xlsx, colNames = FALSE)
  
  res <- load_validate_matrix_table(temp_xlsx, title = "serasi")
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("class1", "class2", "idx_serasi"))
  expect_equal(nrow(res), 4) # 2 x 2 combinations
  expect_setequal(res$class1, c("Hutan", "Sawah"))
  expect_setequal(res$class2, c("Laut", "Pesisir"))
})

test_that("validate_zone_class verifies class overlap against allowed references", {
  poly <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  sf_obj <- sf::st_sf(
    id_pu = 1,
    stat_pu = "intersection",
    RTRW = "Hutan",
    RZWP3K = "Pelabuhan_Unknown",
    geometry = sf::st_sfc(poly, crs = 4326)
  )
  
  ref_rtrw <- data.frame(code = 1, name = c("Hutan", "Sawah"))
  ref_rzwp <- data.frame(code = 1, name = c("Zona Tangkap", "Konservasi"))
  
  # Non-sf error
  expect_error(validate_zone_class(data.frame(a = 1), ref_rtrw, ref_rzwp), "sf_obj must be an sf object")
  
  # Mismatch detection
  res <- validate_zone_class(sf_obj, ref_rtrw, ref_rzwp)
  expect_equal(length(res$mismatch_col3), 0) # Hutan is valid
  expect_equal(res$mismatch_col4, "Pelabuhan_Unknown") # Not in ref_rzwp
})
