test_that("modules/mod_overlap.R contains correct filter_overlaps call and no obsolete st_join", {
  mod_path <- file.path(proj_root, "modules/mod_overlap.R")
  expect_true(file.exists(mod_path))
  
  lines <- readLines(mod_path, warn = FALSE)
  content <- paste(lines, collapse = "\n")
  
  # Ensure obsolete downstream st_join(..., largest = TRUE) is removed
  expect_false(grepl("largest\\s*=\\s*TRUE", content))
  
  # Ensure filter_overlaps is invoked with admin_sf and admin_col
  expect_true(grepl("filter_overlaps\\s*\\([^)]*admin_sf\\s*=\\s*rv\\$admin_vect[^)]*admin_col\\s*=\\s*rv\\$admin_col", content))
})

test_that("Overlap pipeline with admin boundary produces 100% singlepart POLYGONs and retains admin", {
  # Build UTM polygons (EPSG: 32748)
  # RTRW polygon: (0, 0) to (2000, 2000) = 400 ha
  # RZWP3K polygon: (500, 0) to (2500, 2000) = 400 ha
  # Overlap (intersection): (500, 0) to (2000, 2000) = 1500m x 2000m = 300 ha
  
  poly_rtrw <- sf::st_polygon(list(matrix(c(
    0, 0,
    2000, 0,
    2000, 2000,
    0, 2000,
    0, 0
  ), ncol = 2, byrow = TRUE)))
  
  poly_rzwp <- sf::st_polygon(list(matrix(c(
    500, 0,
    2500, 0,
    2500, 2000,
    500, 2000,
    500, 0
  ), ncol = 2, byrow = TRUE)))
  
  crs_utm <- sf::st_crs(32748)
  sf_rtrw <- sf::st_sf(data.frame(RTRW = "Hutan Lindung"), geometry = sf::st_sfc(poly_rtrw, crs = crs_utm))
  sf_rzwp <- sf::st_sf(data.frame(RZWP3K = "Zona Konservasi"), geometry = sf::st_sfc(poly_rzwp, crs = crs_utm))
  
  # Admin boundary dividing at x = 1250:
  # District North-West / West: x in [0, 1250] -> "Kecamatan Barat"
  # District East: x in [1250, 3000] -> "Kecamatan Timur"
  # Overlap area: [500, 1250] x [0, 2000] = 750m x 2000m = 150 ha (< 156.25 ha -> luas tidak terpenuhi)
  # Overlap area: [1250, 2000] x [0, 2000] = 750m x 2000m = 150 ha (< 156.25 ha -> luas tidak terpenuhi)
  poly_adm1 <- sf::st_polygon(list(matrix(c(
    0, 0,
    1250, 0,
    1250, 2000,
    0, 2000,
    0, 0
  ), ncol = 2, byrow = TRUE)))
  
  poly_adm2 <- sf::st_polygon(list(matrix(c(
    1250, 0,
    3000, 0,
    3000, 2000,
    1250, 2000,
    1250, 0
  ), ncol = 2, byrow = TRUE)))
  
  sf_admin <- sf::st_sf(
    data.frame(kecamatan = c("Kecamatan Barat", "Kecamatan Timur")),
    geometry = sf::st_sfc(poly_adm1, poly_adm2, crs = crs_utm)
  )
  
  # Step 1: identify_overlaps
  union_sf <- identify_overlaps(sf_rtrw, sf_rzwp)
  expect_equal(nrow(union_sf), 1)
  expect_equal(union_sf$stat_pu, "intersection")
  
  # Step 2: filter_overlaps with admin_sf
  filtered_sf <- filter_overlaps(
    union_sf,
    threshold_ha = 156.25,
    min_sliver_ha = 1.0,
    admin_sf = sf_admin,
    admin_col = "kecamatan"
  )
  
  expect_equal(nrow(filtered_sf), 2)
  expect_true(all(sf::st_geometry_type(filtered_sf) == "POLYGON"))
  expect_false(any(sf::st_geometry_type(filtered_sf) == "MULTIPOLYGON"))
  expect_true("admin" %in% names(filtered_sf))
  expect_equal(sort(filtered_sf$admin), c("Kecamatan Barat", "Kecamatan Timur"))
  expect_equal(filtered_sf$id_pu, c(1, 2))
  expect_equal(filtered_sf$area_ha, c(150, 150), tolerance = 1e-4)
  expect_equal(filtered_sf$area_flag, c("luas tidak terpenuhi", "luas tidak terpenuhi"))
  
  # Step 3: validate_zone_class
  prio_rtrw <- data.frame(code = 1, RTRW = "Hutan Lindung", Prioritas = 1, stringsAsFactors = FALSE)
  prio_rzwp <- data.frame(code = 1, RZWP3K = "Zona Konservasi", Prioritas = 1, stringsAsFactors = FALSE)
  val_res <- validate_zone_class(filtered_sf, prio_rtrw, prio_rzwp)
  expect_equal(length(val_res$mismatch_col3), 0)
  expect_equal(length(val_res$mismatch_col4), 0)
  
  # Step 4: merge_attributes_to_map
  matrix_compat <- data.frame(
    class1 = "Hutan Lindung",
    class2 = "Zona Konservasi",
    compat = 3.0,
    stringsAsFactors = FALSE
  )
  idx_map <- merge_attributes_to_map(filtered_sf, matrix_compat, default_compat = 0)
  expect_true("idx_serasi" %in% names(idx_map))
  expect_equal(idx_map$idx_serasi, c(3.0, 3.0))
  # Crucial check: admin column retained!
  expect_true("admin" %in% names(idx_map))
  expect_equal(sort(idx_map$admin), c("Kecamatan Barat", "Kecamatan Timur"))
  
  # Step 5: assign_area_function
  idx_map <- assign_area_function(idx_map, prio_rtrw, prio_rzwp)
  expect_true("fungsi" %in% names(idx_map))
  expect_equal(idx_map$fungsi, c("Lindung", "Lindung"))
  # Crucial check: admin column retained!
  expect_true("admin" %in% names(idx_map))
  expect_equal(sort(idx_map$admin), c("Kecamatan Barat", "Kecamatan Timur"))
  
  # Verify singlepart integrity in final idx_serasi_map
  geom_types <- as.character(sf::st_geometry_type(idx_map))
  expect_true(all(geom_types == "POLYGON"))
  expect_false(any(geom_types == "MULTIPOLYGON"))
  
  # Verify export to GPKG and Excel
  tmp_gpkg <- tempfile(fileext = ".gpkg")
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(tmp_gpkg, tmp_xlsx)), add = TRUE)
  
  sf::st_write(idx_map, tmp_gpkg, quiet = TRUE)
  idx_table <- tibble::as_tibble(sf::st_drop_geometry(idx_map))
  openxlsx::write.xlsx(idx_table, tmp_xlsx)
  
  # Read back GPKG and verify 0 MULTIPOLYGON and admin column intact
  gpkg_read <- sf::st_read(tmp_gpkg, quiet = TRUE)
  expect_equal(nrow(gpkg_read), 2)
  expect_true(all(sf::st_geometry_type(gpkg_read) == "POLYGON"))
  expect_false(any(sf::st_geometry_type(gpkg_read) == "MULTIPOLYGON"))
  expect_true("admin" %in% names(gpkg_read))
  expect_equal(sort(gpkg_read$admin), c("Kecamatan Barat", "Kecamatan Timur"))
  
  # Read back Excel and verify admin column
  xlsx_read <- openxlsx::read.xlsx(tmp_xlsx)
  expect_true("admin" %in% names(xlsx_read))
  expect_equal(sort(xlsx_read$admin), c("Kecamatan Barat", "Kecamatan Timur"))
  expect_equal(xlsx_read$id_pu, c(1, 2))
})

test_that("Overlap pipeline without admin boundary produces 100% singlepart POLYGONs without errors", {
  poly_rtrw <- sf::st_polygon(list(matrix(c(
    0, 0,
    2000, 0,
    2000, 2000,
    0, 2000,
    0, 0
  ), ncol = 2, byrow = TRUE)))
  
  poly_rzwp <- sf::st_polygon(list(matrix(c(
    500, 0,
    2500, 0,
    2500, 2000,
    500, 2000,
    500, 0
  ), ncol = 2, byrow = TRUE)))
  
  crs_utm <- sf::st_crs(32748)
  sf_rtrw <- sf::st_sf(data.frame(RTRW = "Hutan Lindung"), geometry = sf::st_sfc(poly_rtrw, crs = crs_utm))
  sf_rzwp <- sf::st_sf(data.frame(RZWP3K = "Zona Konservasi"), geometry = sf::st_sfc(poly_rzwp, crs = crs_utm))
  
  # Step 1: identify_overlaps
  union_sf <- identify_overlaps(sf_rtrw, sf_rzwp)
  
  # Step 2: filter_overlaps with NULL admin
  filtered_sf <- filter_overlaps(
    union_sf,
    threshold_ha = 156.25,
    min_sliver_ha = 1.0,
    admin_sf = NULL,
    admin_col = NULL
  )
  
  expect_equal(nrow(filtered_sf), 1)
  expect_true(all(sf::st_geometry_type(filtered_sf) == "POLYGON"))
  expect_false(any(sf::st_geometry_type(filtered_sf) == "MULTIPOLYGON"))
  expect_false("admin" %in% names(filtered_sf))
  expect_equal(filtered_sf$area_ha, 300, tolerance = 1e-4)
  expect_equal(filtered_sf$area_flag, "luas terpenuhi")
  
  # Step 3 & 4 & 5
  prio_rtrw <- data.frame(code = 1, RTRW = "Hutan Lindung", Prioritas = 1, stringsAsFactors = FALSE)
  prio_rzwp <- data.frame(code = 1, RZWP3K = "Zona Konservasi", Prioritas = 1, stringsAsFactors = FALSE)
  matrix_compat <- data.frame(
    class1 = "Hutan Lindung",
    class2 = "Zona Konservasi",
    compat = 3.0,
    stringsAsFactors = FALSE
  )
  
  idx_map <- merge_attributes_to_map(filtered_sf, matrix_compat, default_compat = 0)
  idx_map <- assign_area_function(idx_map, prio_rtrw, prio_rzwp)
  
  expect_true(all(sf::st_geometry_type(idx_map) == "POLYGON"))
  expect_false(any(sf::st_geometry_type(idx_map) == "MULTIPOLYGON"))
  expect_equal(idx_map$idx_serasi, 3.0)
  expect_equal(idx_map$fungsi, "Lindung")
  
  # Export and verify
  tmp_gpkg <- tempfile(fileext = ".gpkg")
  on.exit(unlink(tmp_gpkg), add = TRUE)
  sf::st_write(idx_map, tmp_gpkg, quiet = TRUE)
  gpkg_read <- sf::st_read(tmp_gpkg, quiet = TRUE)
  expect_true(all(sf::st_geometry_type(gpkg_read) == "POLYGON"))
})

test_that("Row selection targeting matches exactly one contiguous singlepart polygon", {
  # Disjoint multipart polygon cut by admin boundary into 2 polygons in Admin A and 1 in Admin B
  poly_a1 <- sf::st_polygon(list(matrix(c(0, 0, 100, 0, 100, 100, 0, 100, 0, 0), ncol = 2, byrow = TRUE)))
  poly_a2 <- sf::st_polygon(list(matrix(c(200, 0, 300, 0, 300, 100, 200, 100, 200, 0), ncol = 2, byrow = TRUE)))
  poly_b1 <- sf::st_polygon(list(matrix(c(500, 0, 600, 0, 600, 100, 500, 100, 500, 0), ncol = 2, byrow = TRUE)))
  
  sfc <- sf::st_sfc(poly_a1, poly_a2, poly_b1, crs = 32748)
  raw_sf <- sf::st_sf(
    data.frame(
      stat_pu = "intersection",
      RTRW = c("Hutan", "Hutan", "Sawah"),
      RZWP3K = c("Laut", "Laut", "Pesisir")
    ),
    geometry = sfc
  )
  
  res <- filter_overlaps(raw_sf, threshold_ha = 0.5, min_sliver_ha = 0.1)
  
  # 3 distinct rows, each with unique id_pu
  expect_equal(nrow(res), 3)
  expect_equal(res$id_pu, c(1, 2, 3))
  expect_true(all(sf::st_geometry_type(res) == "POLYGON"))
  
  # For each row selected in the table, selecting by id_pu yields exactly 1 contiguous polygon
  for (selected_id in res$id_pu) {
    selected_parcel <- res[res$id_pu == selected_id, ]
    expect_equal(nrow(selected_parcel), 1)
    expect_equal(as.character(sf::st_geometry_type(selected_parcel)), "POLYGON")
    # Verify it has exactly 1 outer boundary ring (not a collection or multipolygon)
    coords <- sf::st_coordinates(selected_parcel)
    expect_true("L1" %in% colnames(coords))
    expect_equal(length(unique(coords[, "L1"])), 1)
  }
})

test_that("validate_zone_class reports RTRW and RZWP3K accurately when admin column is present", {
  poly <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  sf_with_admin <- sf::st_sf(
    id_pu = 1,
    stat_pu = "intersection",
    admin = "Kecamatan A",
    id_rtrw = 1,
    id_rzwp3k = 1,
    RTRW = "Hutan Lindung",
    RZWP3K = "Zona Konservasi",
    geometry = sf::st_sfc(poly, crs = 32748)
  )
  
  ref_rtrw <- data.frame(code = 1, name = "Hutan Lindung", stringsAsFactors = FALSE)
  ref_rzwp <- data.frame(code = 1, name = "Zona Konservasi", stringsAsFactors = FALSE)
  
  out <- capture.output({
    res <- validate_zone_class(sf_with_admin, ref_rtrw, ref_rzwp)
  })
  
  expect_equal(length(res$mismatch_col3), 0)
  expect_equal(length(res$mismatch_col4), 0)
  
  # Report output must explicitly reference RTRW and RZWP3K, not admin or id_rtrw
  report_text <- paste(out, collapse = "\n")
  expect_true(grepl("RTRW\\s*:\\s*All non-NA classes match", report_text))
  expect_true(grepl("RZWP3K\\s*:\\s*All non-NA classes match", report_text))
  expect_false(grepl("admin\\s*:\\s*All non-NA classes match", report_text))
})

test_that("modules/mod_overlap.R guards against administrative shapefiles with 0 attribute columns", {
  mod_path <- file.path(proj_root, "modules/mod_overlap.R")
  lines <- readLines(mod_path, warn = FALSE)
  content <- paste(lines, collapse = "\n")
  
  # Ensure zero-attribute admin shapefile is checked and rejected with stop()
  expect_true(grepl("if\\s*\\(length\\(col_names\\)\\s*==\\s*0\\)\\s*\\{\\s*stop\\(", content))
  # Ensure admin_field observeEvent requires input$admin_field
  expect_true(grepl("observeEvent\\(input\\$admin_field,\\s*\\{\\s*req\\(input\\$admin_field\\)", content))
})
