test_that("generate_matrix_serasi builds cross-tabulation matrix with correct dimensions", {
  poly <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  sfc <- sf::st_sfc(poly, crs = 4326)
  
  sf_rtrw <- sf::st_sf(data.frame(RTRW = c("Hutan", "Sawah", "Hutan")), geometry = rep(sfc, 3))
  sf_rzwp <- sf::st_sf(data.frame(RZWP3K = c("Laut", "Pesisir", "Pelabuhan")), geometry = rep(sfc, 3))
  
  mat <- generate_matrix_serasi(sf_rtrw, sf_rzwp, fill_value = 0)
  
  expect_s3_class(mat, "tbl_df")
  expect_named(mat, c("RTRW_RZWP3K", "Laut", "Pesisir", "Pelabuhan"))
  expect_equal(nrow(mat), 2) # Unique RTRW classes: Hutan, Sawah
  expect_equal(mat$RTRW_RZWP3K, c("Hutan", "Sawah"))
  expect_true(all(mat$Laut == 0))
})

test_that("generate_matrix_padu_ke creates square matrix with diagonal set to 3", {
  tbl_classes <- data.frame(
    id = 1:3,
    class_name = c("Hutan Lindung", "Perkebunan", "Pemukiman"),
    stringsAsFactors = FALSE
  )
  
  mat <- generate_matrix_padu_ke(tbl_classes, fill_value = NA)
  
  expect_s3_class(mat, "tbl_df")
  expect_equal(names(mat)[1], "class")
  expect_equal(mat$class, c("Hutan Lindung", "Perkebunan", "Pemukiman"))
  
  # Inner 3x3 numeric matrix
  inner <- as.matrix(mat[, -1])
  expect_equal(nrow(inner), 3)
  expect_equal(ncol(inner), 3)
  
  # Diagonal must be 3 by domain rule
  expect_equal(unname(diag(inner)), c(3, 3, 3))
  
  # Off-diagonals must be fill_value (NA)
  off_diag <- inner[row(inner) != col(inner)]
  expect_true(all(is.na(off_diag)))
})

test_that("generate_matrix_padu_ke validates input data frame", {
  expect_error(generate_matrix_padu_ke("not_a_df"), "Input 'tbl' must be a data frame")
  expect_error(generate_matrix_padu_ke(data.frame(a = 1)), "Input must have at least two columns")
  expect_error(generate_matrix_padu_ke(data.frame(a = 1, b = NA)), "No valid class names found")
})

test_that("merge_attributes_to_map attaches compatibility scores to spatial units", {
  poly <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  sfc <- sf::st_sfc(poly, crs = 4326)
  
  # Create an overlap sf with 4 non-geometry columns: id_pu, stat_pu, RTRW, RZWP3K
  sf_overlap <- sf::st_sf(
    data.frame(
      id_pu = c(1, 2, 3),
      stat_pu = "intersection",
      RTRW = c("Hutan", "Sawah", "Kota"),
      RZWP3K = c("Laut", "Pesisir", "Laut")
    ),
    geometry = rep(sfc, 3)
  )
  
  lookup <- data.frame(
    class1 = c("Hutan", "Sawah"),
    class2 = c("Laut", "Pesisir"),
    score = c(3.0, 2.5),
    stringsAsFactors = FALSE
  )
  
  res <- merge_attributes_to_map(sf_overlap, lookup, default_compat = 0.0)
  
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 3)
  expect_true("idx_serasi" %in% names(res))
  expect_equal(res$idx_serasi[1], 3.0)  # Hutan + Laut
  expect_equal(res$idx_serasi[2], 2.5)  # Sawah + Pesisir
  expect_equal(res$idx_serasi[3], 0.0)  # Kota + Laut is not in lookup, receives default_compat
})

test_that("get_alternative_serasi handles bidirectional lookups and missing values", {
  serasi_df <- data.frame(
    class1 = c("Hutan", "Perkebunan"),
    class2 = c("Mangrove", "Tambak"),
    idx_serasi = c(3.0, 1.5),
    stringsAsFactors = FALSE
  )
  
  # Direct match
  expect_equal(get_alternative_serasi("Hutan", "Mangrove", serasi_df), 3.0)
  
  # Reversed match (class_b then class_a)
  expect_equal(get_alternative_serasi("Mangrove", "Hutan", serasi_df), 3.0)
  
  # Missing pair
  expect_true(is.na(get_alternative_serasi("Hutan", "Tambak", serasi_df)))
  
  # NA input
  expect_true(is.na(get_alternative_serasi(NA, "Mangrove", serasi_df)))
  expect_true(is.na(get_alternative_serasi("Hutan", NA, serasi_df)))
})
