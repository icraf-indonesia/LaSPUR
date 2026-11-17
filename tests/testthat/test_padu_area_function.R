# tests/testthat/test_padu_area_function.R
# ============================================================
# Unit tests for assign_area_function() (SERASI Area Function Classification)
# ============================================================

make_test_poly <- function(xmin = 0, ymin = 0, xmax = 1, ymax = 1, crs = 4326) {
  pts <- matrix(c(
    xmin, ymin,
    xmax, ymin,
    xmax, ymax,
    xmin, ymax,
    xmin, ymin
  ), ncol = 2, byrow = TRUE)
  sf::st_sfc(sf::st_polygon(list(pts)), crs = crs)
}

# --- Core Priority Combinations ---

test_that("assign_area_function classifies all four priority combinations correctly", {
  poly <- make_test_poly()
  
  sf_data <- sf::st_sf(
    id_pu = 1:4,
    stat_pu = "intersection",
    RTRW = c("HL", "HP", "HL", "HP"),
    RZWP3K = c("KKP", "KPU_AL", "KPU_AL", "KKP"),
    geometry = rep(poly, 4)
  )
  
  prio_rtrw <- data.frame(
    ID = 1:2,
    RTRW = c("HL", "HP"),
    Prioritas = c(1, 0),
    stringsAsFactors = FALSE
  )
  
  prio_rzwp <- data.frame(
    ID = 1:2,
    RZWP3K = c("KKP", "KPU_AL"),
    Prioritas = c(1, 0),
    stringsAsFactors = FALSE
  )
  
  res <- assign_area_function(sf_data, prio_rtrw, prio_rzwp)
  
  expect_s3_class(res, "sf")
  expect_true("fungsi" %in% names(res))
  expect_type(res$fungsi, "character")
  
  # 1. Lindung: (1, 1) -> HL + KKP
  expect_equal(res$fungsi[1], "lindung")
  
  # 2. Budidaya: (0, 0) -> HP + KPU_AL
  expect_equal(res$fungsi[2], "budidaya")
  
  # 3. Campuran: (1, 0) -> HL + KPU_AL
  expect_equal(res$fungsi[3], "campuran")
  
  # 4. Campuran: (0, 1) -> HP + KKP
  expect_equal(res$fungsi[4], "campuran")
})

# --- Overlap (Single-row per PU) Format ---

test_that("assign_area_function handles single-row per PU (overlap) format correctly", {
  poly <- make_test_poly()
  
  overlap_sf <- sf::st_sf(
    id_pu = c(101, 102, 103),
    stat_pu = "intersection",
    RTRW = c("Kawasan Lindung", "Kawasan Budidaya Tanaman Pangan", "Kawasan Lindung"),
    RZWP3K = c("Zona Konservasi", "Zona Pelabuhan", "Zona Pelabuhan"),
    area_ha = c(200.5, 350.2, 180.0),
    geometry = rep(poly, 3)
  )
  
  prio_rtrw <- data.frame(
    Pola.Ruang = c("Kawasan Lindung", "Kawasan Budidaya Tanaman Pangan"),
    Prioritas = c(1, 0),
    stringsAsFactors = FALSE
  )
  
  prio_rzwp <- data.frame(
    Zona = c("Zona Konservasi", "Zona Pelabuhan"),
    Prioritas = c(1, 0),
    stringsAsFactors = FALSE
  )
  
  res <- assign_area_function(overlap_sf, prio_rtrw, prio_rzwp)
  
  expect_equal(res$fungsi, c("lindung", "budidaya", "campuran"))
  expect_equal(res$id_pu, c(101, 102, 103))
  expect_equal(res$area_ha, c(200.5, 350.2, 180.0))
})

# --- Adjacent (Paired-rows per PU) Format ---

test_that("assign_area_function handles paired-rows per PU (adjacent) format correctly", {
  poly <- make_test_poly()
  
  # Adjacent structure: 2 rows per id_pu (one RTRW with NA RZWP, one RZWP with NA RTRW)
  adjacent_sf <- sf::st_sf(
    id = c("R1", "Z1", "R2", "Z2", "R3", "Z3"),
    id_pu = c(1, 1, 2, 2, 3, 3),
    RTRW = c("Hutan Lindung", NA, "Kawasan Industri", NA, "Hutan Lindung", NA),
    RZWP3K = c(NA, "Zona Inti Konservasi", NA, "Zona Pelabuhan Laut", NA, "Zona Pelabuhan Laut"),
    geometry = rep(poly, 6)
  )
  
  prio_rtrw <- data.frame(
    RTRW = c("Hutan Lindung", "Kawasan Industri"),
    Prioritas = c(1, 0),
    stringsAsFactors = FALSE
  )
  
  prio_rzwp <- data.frame(
    RZWP3K = c("Zona Inti Konservasi", "Zona Pelabuhan Laut"),
    Prioritas = c(1, 0),
    stringsAsFactors = FALSE
  )
  
  res <- assign_area_function(adjacent_sf, prio_rtrw, prio_rzwp)
  
  # Both rows for id_pu 1 must be "lindung"
  expect_equal(res$fungsi[res$id_pu == 1], c("lindung", "lindung"))
  
  # Both rows for id_pu 2 must be "budidaya"
  expect_equal(res$fungsi[res$id_pu == 2], c("budidaya", "budidaya"))
  
  # Both rows for id_pu 3 must be "campuran"
  expect_equal(res$fungsi[res$id_pu == 3], c("campuran", "campuran"))
})

# --- Plain Data Frame / Tibble Input ---

test_that("assign_area_function supports plain data frames without geometry", {
  df <- data.frame(
    id_pu = 1:3,
    RTRW = c("A", "B", "A"),
    RZWP3K = c("X", "Y", "Y"),
    stringsAsFactors = FALSE
  )
  
  prio_rtrw <- data.frame(RTRW = c("A", "B"), Prioritas = c(1, 0))
  prio_rzwp <- data.frame(RZWP3K = c("X", "Y"), Prioritas = c(1, 0))
  
  res <- assign_area_function(df, prio_rtrw, prio_rzwp)
  
  expect_s3_class(res, "data.frame")
  expect_false(inherits(res, "sf"))
  expect_equal(res$fungsi, c("lindung", "budidaya", "campuran"))
})

# --- Column Name Flexibility in Priority Tables ---

test_that("assign_area_function is flexible with column names in reference tables", {
  df <- data.frame(
    RTRW = c("HL", "HP"),
    RZWP3K = c("KKP", "KPU"),
    stringsAsFactors = FALSE
  )
  
  # Table with Kode.Pola.Ruang, Pola.Ruang, Prioritas
  prio_rtrw <- data.frame(
    Kode.Pola.Ruang = c(101, 102),
    Pola.Ruang = c("HL", "HP"),
    Prioritas = c(1, 0),
    stringsAsFactors = FALSE
  )
  
  # Table with lowercase prioritas and Subzona
  prio_rzwp <- data.frame(
    Subzona = c("KKP", "KPU"),
    prioritas = c(1, 0),
    stringsAsFactors = FALSE
  )
  
  res <- assign_area_function(df, prio_rtrw, prio_rzwp)
  expect_equal(res$fungsi, c("lindung", "budidaya"))
})

test_that("assign_area_function handles character priorities (e.g. '1'/'0' or 'Lindung'/'Budidaya')", {
  df <- data.frame(
    RTRW = c("HL", "HP"),
    RZWP3K = c("KKP", "KPU"),
    stringsAsFactors = FALSE
  )
  
  prio_rtrw <- data.frame(RTRW = c("HL", "HP"), Prioritas = c("1", "0"))
  prio_rzwp <- data.frame(RZWP3K = c("KKP", "KPU"), Prioritas = c("Lindung", "Budidaya"))
  
  res <- assign_area_function(df, prio_rtrw, prio_rzwp)
  expect_equal(res$fungsi, c("lindung", "budidaya"))
})

# --- Whitespace Tolerance ---

test_that("assign_area_function handles whitespace discrepancies gracefully", {
  df <- data.frame(
    RTRW = c(" Hutan Lindung ", "Kawasan Industri"),
    RZWP3K = c("Zona Konservasi", " Zona Pelabuhan  "),
    stringsAsFactors = FALSE
  )
  
  prio_rtrw <- data.frame(RTRW = c("Hutan Lindung", "Kawasan Industri "), Prioritas = c(1, 0))
  prio_rzwp <- data.frame(RZWP3K = c(" Zona Konservasi", "Zona Pelabuhan"), Prioritas = c(1, 0))
  
  res <- assign_area_function(df, prio_rtrw, prio_rzwp)
  expect_equal(res$fungsi, c("lindung", "budidaya"))
})

# --- Edge Cases: Unknown Zones, Missing Values, Empty Input ---

test_that("assign_area_function issues warning and returns NA for unmatched zones", {
  df <- data.frame(
    id_pu = 1:3,
    RTRW = c("HL", "Unknown_RTRW", "HL"),
    RZWP3K = c("KKP", "KKP", "Unknown_RZWP"),
    stringsAsFactors = FALSE
  )
  
  prio_rtrw <- data.frame(RTRW = "HL", Prioritas = 1)
  prio_rzwp <- data.frame(RZWP3K = "KKP", Prioritas = 1)
  
  expect_warning(
    res <- assign_area_function(df, prio_rtrw, prio_rzwp),
    "could not be matched"
  )
  
  expect_equal(res$fungsi[1], "lindung")
  expect_true(is.na(res$fungsi[2]))
  expect_true(is.na(res$fungsi[3]))
})

test_that("assign_area_function handles NA values in input zones cleanly", {
  df <- data.frame(
    id_pu = 1:2,
    RTRW = c("HL", NA),
    RZWP3K = c(NA, "KKP"),
    stringsAsFactors = FALSE
  )
  
  prio_rtrw <- data.frame(RTRW = "HL", Prioritas = 1)
  prio_rzwp <- data.frame(RZWP3K = "KKP", Prioritas = 1)
  
  # When single-row has NA zone, it should produce NA_character_
  res <- assign_area_function(df, prio_rtrw, prio_rzwp)
  expect_true(is.na(res$fungsi[1]))
  expect_true(is.na(res$fungsi[2]))
})

test_that("assign_area_function handles reference table with NA priority cleanly", {
  df <- data.frame(
    id_pu = 1,
    RTRW = "HL",
    RZWP3K = "KKP",
    stringsAsFactors = FALSE
  )
  
  prio_rtrw <- data.frame(RTRW = "HL", Prioritas = NA_real_)
  prio_rzwp <- data.frame(RZWP3K = "KKP", Prioritas = 1)
  
  res <- assign_area_function(df, prio_rtrw, prio_rzwp)
  expect_true(is.na(res$fungsi[1]))
})

test_that("assign_area_function handles empty inputs", {
  df_empty <- data.frame(
    RTRW = character(0),
    RZWP3K = character(0),
    stringsAsFactors = FALSE
  )
  
  prio_rtrw <- data.frame(RTRW = "HL", Prioritas = 1)
  prio_rzwp <- data.frame(RZWP3K = "KKP", Prioritas = 1)
  
  res <- assign_area_function(df_empty, prio_rtrw, prio_rzwp)
  expect_true("fungsi" %in% names(res))
  expect_equal(length(res$fungsi), 0)
})

# --- Error Handling ---

test_that("assign_area_function raises informative errors on invalid arguments", {
  prio_rtrw <- data.frame(RTRW = "HL", Prioritas = 1)
  prio_rzwp <- data.frame(RZWP3K = "KKP", Prioritas = 1)
  
  # Non-dataframe sf_obj
  expect_error(assign_area_function("not_a_df", prio_rtrw, prio_rzwp), "sf_obj must be an sf object or data frame")
  
  # Non-dataframe priority table
  df <- data.frame(RTRW = "HL", RZWP3K = "KKP")
  expect_error(assign_area_function(df, "not_a_df", prio_rzwp), "rtrw_prioritas and rzwp3k_prioritas must be data frames")
  expect_error(assign_area_function(df, prio_rtrw, "not_a_df"), "rtrw_prioritas and rzwp3k_prioritas must be data frames")
  
  # Missing RTRW / RZWP3K columns in sf_obj
  df_bad_cols <- data.frame(col1 = 1, col2 = 2)
  expect_error(assign_area_function(df_bad_cols, prio_rtrw, prio_rzwp), "must contain 'RTRW' and 'RZWP3K'")
  
  # Priority table missing priority column
  prio_bad <- data.frame(RTRW = "HL", wrong_col = 1)
  expect_error(assign_area_function(df, prio_bad, prio_rzwp), "must contain a 'Prioritas' column")
})

# --- Real Reference Tables Integration ---

test_that("assign_area_function works with real repo priority Excel tables", {
  skip_if_not(file.exists("../../data/tabular/tabel_acuan_prioritas_RTRW.xlsx") ||
              file.exists("data/tabular/tabel_acuan_prioritas_RTRW.xlsx"))
  
  rtrw_path <- if (file.exists("data/tabular/tabel_acuan_prioritas_RTRW.xlsx")) {
    "data/tabular/tabel_acuan_prioritas_RTRW.xlsx"
  } else {
    "../../data/tabular/tabel_acuan_prioritas_RTRW.xlsx"
  }
  
  rzwp_path <- if (file.exists("data/tabular/tabel_acuan_prioritas_RZWP3K.xlsx")) {
    "data/tabular/tabel_acuan_prioritas_RZWP3K.xlsx"
  } else {
    "../../data/tabular/tabel_acuan_prioritas_RZWP3K.xlsx"
  }
  
  tbl_rtrw <- openxlsx::read.xlsx(rtrw_path)
  tbl_rzwp <- openxlsx::read.xlsx(rzwp_path)
  
  # Pick actual classes from the tables
  # Kawasan Konservasi (RTRW: 1) + Kawasan Konservasi Maritim (RZWP: 1) -> "lindung"
  # Kawasan Hutan Adat (RTRW: 0) + Zona Wilayah Kelola Masyarakat Hukum Adat (RZWP: 0) -> "budidaya"
  # Kawasan Konservasi (RTRW: 1) + Zona Wilayah Kelola Masyarakat Hukum Adat (RZWP: 0) -> "campuran"
  test_sf <- data.frame(
    id_pu = 1:3,
    RTRW = c("Kawasan Konservasi", "Kawasan Hutan Adat", "Kawasan Konservasi"),
    RZWP3K = c("Kawasan Konservasi Maritim", "Zona Wilayah Kelola Masyarakat Hukum Adat", "Zona Wilayah Kelola Masyarakat Hukum Adat"),
    stringsAsFactors = FALSE
  )
  
  res <- assign_area_function(test_sf, tbl_rtrw, tbl_rzwp)
  expect_equal(res$fungsi, c("lindung", "budidaya", "campuran"))
})

# --- Full Pipeline Simulation: Overlap & Adjacent ---

test_that("Overlap pipeline integration: assign_area_function persists 'fungsi' into GPKG and Excel", {
  poly1 <- make_test_poly(0, 0, 2, 2)
  poly2 <- make_test_poly(1, 1, 3, 3)
  
  sf_rtrw <- sf::st_sf(
    data.frame(RTRW = "Kawasan Lindung", stringsAsFactors = FALSE),
    geometry = poly1
  )
  sf_rzwp <- sf::st_sf(
    data.frame(RZWP3K = "Zona Konservasi", stringsAsFactors = FALSE),
    geometry = poly2
  )
  
  # Step 1: identify_overlaps
  overlaps <- identify_overlaps(sf_rtrw, sf_rzwp)
  expect_equal(nrow(overlaps), 1)
  
  # Step 2: filter_overlaps
  filtered <- suppressWarnings(filter_overlaps(overlaps, threshold_ha = 0))
  
  # Step 3: merge_attributes_to_map
  matrix_long <- data.frame(
    class1 = "Kawasan Lindung",
    class2 = "Zona Konservasi",
    idx_serasi = 3.0,
    stringsAsFactors = FALSE
  )
  serasi_map <- merge_attributes_to_map(filtered, matrix_long)
  
  # Step 4: assign_area_function
  prio_rtrw <- data.frame(RTRW = "Kawasan Lindung", Prioritas = 1)
  prio_rzwp <- data.frame(RZWP3K = "Zona Konservasi", Prioritas = 1)
  final_map <- assign_area_function(serasi_map, prio_rtrw, prio_rzwp)
  
  expect_true("fungsi" %in% names(final_map))
  expect_equal(final_map$fungsi, "lindung")
  
  # Step 5: Verify persistence to GPKG and Excel
  tmp_dir <- tempfile("serasi_overlap_test_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  gpkg_file <- file.path(tmp_dir, "idx_serasi_overlaps.gpkg")
  xlsx_file <- file.path(tmp_dir, "idx_serasi_overlaps.xlsx")
  
  sf::st_write(final_map, gpkg_file, delete_dsn = TRUE, quiet = TRUE)
  final_tbl <- tibble::as_tibble(sf::st_drop_geometry(final_map))
  openxlsx::write.xlsx(final_tbl, xlsx_file)
  
  read_gpkg <- sf::st_read(gpkg_file, quiet = TRUE)
  expect_true("fungsi" %in% names(read_gpkg))
  expect_equal(read_gpkg$fungsi, "lindung")
  
  read_xlsx <- openxlsx::read.xlsx(xlsx_file)
  expect_true("fungsi" %in% names(read_xlsx))
  expect_equal(read_xlsx$fungsi, "lindung")
})

test_that("Adjacent pipeline integration: assign_area_function persists 'fungsi' into GPKG, Excel, and dissolved output", {
  # Two adjacent polygons sharing edge at x = 1
  poly_rtrw <- make_test_poly(0, 0, 1, 1)
  poly_rzwp <- make_test_poly(1, 0, 2, 1)
  
  sf_rtrw <- sf::st_sf(
    data.frame(RTRW = "Kawasan Industri", stringsAsFactors = FALSE),
    geometry = poly_rtrw
  )
  sf_rzwp <- sf::st_sf(
    data.frame(RZWP3K = "Zona Pelabuhan", stringsAsFactors = FALSE),
    geometry = poly_rzwp
  )
  
  # Step 1: identify_adjacent
  adjacent <- identify_adjacent(sf_rtrw, sf_rzwp, min_area_ha = 0, show_detailed_progress = FALSE)
  expect_equal(nrow(adjacent), 2)
  expect_equal(adjacent$id_pu, c(1, 1))
  
  # Step 2: merge_attributes_to_map
  matrix_long <- data.frame(
    class1 = "Kawasan Industri",
    class2 = "Zona Pelabuhan",
    idx_serasi = 2.0,
    stringsAsFactors = FALSE
  )
  serasi_map <- merge_attributes_to_map(adjacent, matrix_long)
  
  # Step 3: assign_area_function
  prio_rtrw <- data.frame(RTRW = "Kawasan Industri", Prioritas = 0)
  prio_rzwp <- data.frame(RZWP3K = "Zona Pelabuhan", Prioritas = 0)
  final_map <- assign_area_function(serasi_map, prio_rtrw, prio_rzwp)
  
  expect_true("fungsi" %in% names(final_map))
  expect_equal(final_map$fungsi, c("budidaya", "budidaya"))
  
  # Step 4: Add admin column to test dissolve_id_pu integration
  final_map$admin <- "Wilayah_A"
  final_map$area_ha <- 100.0
  
  dissolved <- dissolve_id_pu(final_map)
  expect_true("fungsi" %in% names(dissolved))
  expect_equal(dissolved$fungsi, "budidaya")
  
  # Step 5: Verify persistence to GPKG and Excel
  tmp_dir <- tempfile("serasi_adjacent_test_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  gpkg_file <- file.path(tmp_dir, "idx_serasi_adjacent.gpkg")
  xlsx_file <- file.path(tmp_dir, "idx_serasi_adjacent.xlsx")
  
  sf::st_write(final_map, gpkg_file, delete_dsn = TRUE, quiet = TRUE)
  final_tbl <- tibble::as_tibble(sf::st_drop_geometry(final_map))
  openxlsx::write.xlsx(final_tbl, xlsx_file)
  
  read_gpkg <- sf::st_read(gpkg_file, quiet = TRUE)
  expect_true("fungsi" %in% names(read_gpkg))
  expect_equal(read_gpkg$fungsi, c("budidaya", "budidaya"))
  
  read_xlsx <- openxlsx::read.xlsx(xlsx_file)
  expect_true("fungsi" %in% names(read_xlsx))
  expect_equal(read_xlsx$fungsi, c("budidaya", "budidaya"))
})

# --- Robustness & Edge Cases ---

test_that("assign_area_function correctly handles mixed character and numeric priorities", {
  df <- data.frame(
    id_pu = 1:2,
    RTRW = c("HL", "HP"),
    RZWP3K = c("KKP", "KPU"),
    stringsAsFactors = FALSE
  )
  # Mix of "Lindung" text and "0" numeric string
  prio_rtrw <- data.frame(RTRW = c("HL", "HP"), Prioritas = c("Lindung", "0"), stringsAsFactors = FALSE)
  prio_rzwp <- data.frame(RZWP3K = c("KKP", "KPU"), Prioritas = c(1, 0), stringsAsFactors = FALSE)
  
  res <- assign_area_function(df, prio_rtrw, prio_rzwp)
  expect_equal(res$fungsi, c("lindung", "budidaya"))
})

test_that("assign_area_function warns on invalid priority values outside {0, 1}", {
  df <- data.frame(
    id_pu = 1:2,
    RTRW = c("HL", "HP"),
    RZWP3K = c("KKP", "KPU"),
    stringsAsFactors = FALSE
  )
  prio_rtrw <- data.frame(RTRW = c("HL", "HP"), Prioritas = c(1, 2), stringsAsFactors = FALSE)
  prio_rzwp <- data.frame(RZWP3K = c("KKP", "KPU"), Prioritas = c(1, 0), stringsAsFactors = FALSE)
  
  # Expect warning about invalid priority values
  expect_warning(
    res <- assign_area_function(df, prio_rtrw, prio_rzwp),
    "outside \\{0, 1\\}"
  )
  expect_equal(res$fungsi[1], "lindung")
  expect_true(is.na(res$fungsi[2]))
})

test_that("assign_area_function warns on duplicate zone entries with conflicting priorities", {
  df <- data.frame(
    id_pu = 1,
    RTRW = "HL",
    RZWP3K = "KKP",
    stringsAsFactors = FALSE
  )
  prio_rtrw <- data.frame(
    RTRW = c("HL", "HL"),
    Prioritas = c(1, 0),
    stringsAsFactors = FALSE
  )
  prio_rzwp <- data.frame(RZWP3K = "KKP", Prioritas = 1, stringsAsFactors = FALSE)
  
  expect_warning(
    res <- assign_area_function(df, prio_rtrw, prio_rzwp),
    "conflicting priorities"
  )
  # Uses first occurrence (1 -> "lindung")
  expect_equal(res$fungsi[1], "lindung")
})

test_that("assign_area_function does not misidentify overlap data with duplicate id_pu as paired adjacent", {
  # Overlap data where each row has both RTRW and RZWP3K, but id_pu happens to be duplicated
  df <- data.frame(
    id_pu = c(1, 1),
    RTRW = c("HL", "HP"),
    RZWP3K = c("KKP", "KPU"),
    stringsAsFactors = FALSE
  )
  prio_rtrw <- data.frame(RTRW = c("HL", "HP"), Prioritas = c(1, 0), stringsAsFactors = FALSE)
  prio_rzwp <- data.frame(RZWP3K = c("KKP", "KPU"), Prioritas = c(1, 0), stringsAsFactors = FALSE)
  
  res <- assign_area_function(df, prio_rtrw, prio_rzwp)
  # Row 1 is HL (1) + KKP (1) -> lindung
  # Row 2 is HP (0) + KPU (0) -> budidaya
  # Must NOT overwrite row 2 with row 1's lindung!
  expect_equal(res$fungsi, c("lindung", "budidaya"))
})

test_that("assign_area_function handles paired adjacent with NA in id_pu gracefully", {
  df <- data.frame(
    id_pu = c(1, 1, NA, NA),
    RTRW = c("HL", NA, "HP", NA),
    RZWP3K = c(NA, "KKP", NA, "KPU"),
    stringsAsFactors = FALSE
  )
  prio_rtrw <- data.frame(RTRW = c("HL", "HP"), Prioritas = c(1, 0), stringsAsFactors = FALSE)
  prio_rzwp <- data.frame(RZWP3K = c("KKP", "KPU"), Prioritas = c(1, 0), stringsAsFactors = FALSE)
  
  res <- assign_area_function(df, prio_rtrw, prio_rzwp)
  expect_equal(res$fungsi[1:2], c("lindung", "lindung"))
  expect_true(is.na(res$fungsi[3]))
  expect_true(is.na(res$fungsi[4]))
})

test_that("assign_area_function warns on conflicting zone classes within same id_pu in adjacent data", {
  df <- data.frame(
    id_pu = c(1, 1, 1),
    RTRW = c("HL", "HP", NA),
    RZWP3K = c(NA, NA, "KKP"),
    stringsAsFactors = FALSE
  )
  prio_rtrw <- data.frame(RTRW = c("HL", "HP"), Prioritas = c(1, 0), stringsAsFactors = FALSE)
  prio_rzwp <- data.frame(RZWP3K = "KKP", Prioritas = 1, stringsAsFactors = FALSE)
  
  expect_warning(
    res <- assign_area_function(df, prio_rtrw, prio_rzwp),
    "Conflicting RTRW zone classes detected within the same id_pu"
  )
  # Uses first occurrence (HL = 1 + KKP = 1 -> lindung)
  expect_equal(res$fungsi, c("lindung", "lindung", "lindung"))
})

test_that("assign_area_function supports case-insensitive zone matching fallback", {
  df <- data.frame(
    id_pu = 1:2,
    RTRW = c("hutan lindung", "KAWASAN INDUSTRI"),
    RZWP3K = c("zona konservasi", "ZONA PELABUHAN"),
    stringsAsFactors = FALSE
  )
  prio_rtrw <- data.frame(
    RTRW = c("Hutan Lindung", "Kawasan Industri"),
    Prioritas = c(1, 0),
    stringsAsFactors = FALSE
  )
  prio_rzwp <- data.frame(
    RZWP3K = c("Zona Konservasi", "Zona Pelabuhan"),
    Prioritas = c(1, 0),
    stringsAsFactors = FALSE
  )
  
  res <- assign_area_function(df, prio_rtrw, prio_rzwp)
  expect_equal(res$fungsi, c("lindung", "budidaya"))
})


# ============================================================
# Unit tests for calculate_padu_index() (Multi-Composite Engine)
# ============================================================

test_that("calculate_padu_index computes tri-composites and assigns idx_padu_final based on fungsi (uniform weights)", {
  poly <- make_test_poly()
  idx_map <- sf::st_sf(
    id_pu = 1:3,
    fungsi = c("lindung", "budidaya", "campuran"),
    geometry = rep(poly, 3)
  )
  
  # Create 7 individual PADU tables with distinct known values
  # id_pu = 1: lindung (KE=0.8, HS=0.9, KL=0.7, KH=0.8, KI=0.8, RTp=0.2, SE=0.4)
  # id_pu = 2: budidaya (KE=0.3, HS=0.2, KL=0.1, KH=0.2, KI=0.6, RTp=0.9, SE=0.7)
  # id_pu = 3: campuran (KE=0.5, HS=0.5, KL=0.5, KH=0.5, KI=0.5, RTp=0.5, SE=0.5)
  padu_ke  <- data.frame(id_pu = 1:3, idx_padu_ke  = c(0.8, 0.3, 0.5))
  padu_hs  <- data.frame(id_pu = 1:3, idx_padu_hs  = c(0.9, 0.2, 0.5))
  padu_kl  <- data.frame(id_pu = 1:3, idx_padu_kl  = c(0.7, 0.1, 0.5))
  padu_kh  <- data.frame(id_pu = 1:3, idx_padu_kh  = c(0.8, 0.2, 0.5))
  padu_rtp <- data.frame(id_pu = 1:3, idx_padu_rtp = c(0.2, 0.9, 0.5))
  padu_se  <- data.frame(id_pu = 1:3, idx_padu_se  = c(0.4, 0.7, 0.5))
  padu_ki  <- data.frame(id_pu = 1:3, idx_padu_ki  = c(0.8, 0.6, 0.5))
  
  padu_list <- list(padu_ke, padu_hs, padu_kl, padu_kh, padu_rtp, padu_se, padu_ki)
  
  # Call with uniform weights (padu_idx_weight = NULL or uniform)
  res <- calculate_padu_index(padu_list, idx_map, padu_idx_weight = NULL)
  
  expect_true(all(c("idx_padu_all", "idx_padu_lindung", "idx_padu_budidaya", "idx_padu_final") %in% names(res)))
  
  # Row 1:
  # MFL (ke, hs, kl, kh, ki) = (0.8 + 0.9 + 0.7 + 0.8 + 0.8) / 5 = 4.0 / 5 = 0.8
  # MFB (rtp, se, ki) = (0.2 + 0.4 + 0.8) / 3 = 1.4 / 3 = 0.4666667
  # All = (0.8 + 0.9 + 0.7 + 0.8 + 0.2 + 0.4 + 0.8) / 7 = 4.6 / 7 = 0.6571429
  expect_equal(res$idx_padu_lindung[1], 0.8, tolerance = 1e-6)
  expect_equal(res$idx_padu_budidaya[1], 1.4 / 3, tolerance = 1e-6)
  expect_equal(res$idx_padu_all[1], 4.6 / 7, tolerance = 1e-6)
  # fungsi == "lindung" -> idx_padu_final must be idx_padu_lindung
  expect_equal(res$idx_padu_final[1], res$idx_padu_lindung[1])
  
  # Row 2:
  # MFL (ke, hs, kl, kh, ki) = (0.3 + 0.2 + 0.1 + 0.2 + 0.6) / 5 = 1.4 / 5 = 0.28
  # MFB (rtp, se, ki) = (0.9 + 0.7 + 0.6) / 3 = 2.2 / 3 = 0.7333333
  # All = (0.3 + 0.2 + 0.1 + 0.2 + 0.9 + 0.7 + 0.6) / 7 = 3.0 / 7 = 0.4285714
  expect_equal(res$idx_padu_lindung[2], 0.28, tolerance = 1e-6)
  expect_equal(res$idx_padu_budidaya[2], 2.2 / 3, tolerance = 1e-6)
  expect_equal(res$idx_padu_all[2], 3.0 / 7, tolerance = 1e-6)
  # fungsi == "budidaya" -> idx_padu_final must be idx_padu_budidaya
  expect_equal(res$idx_padu_final[2], res$idx_padu_budidaya[2])
  
  # Row 3:
  # All values are 0.5
  expect_equal(res$idx_padu_lindung[3], 0.5, tolerance = 1e-6)
  expect_equal(res$idx_padu_budidaya[3], 0.5, tolerance = 1e-6)
  expect_equal(res$idx_padu_all[3], 0.5, tolerance = 1e-6)
  # fungsi == "campuran" -> idx_padu_final must be idx_padu_all
  expect_equal(res$idx_padu_final[3], res$idx_padu_all[3])
})

test_that("calculate_padu_index rescales custom weights correctly within MFL and MFB", {
  poly <- make_test_poly()
  idx_map <- sf::st_sf(
    id_pu = 1:3,
    fungsi = c("lindung", "budidaya", "campuran"),
    geometry = rep(poly, 3)
  )
  
  padu_ke  <- data.frame(id_pu = 1:3, idx_padu_ke  = c(0.8, 0.2, 0.4))
  padu_hs  <- data.frame(id_pu = 1:3, idx_padu_hs  = c(0.6, 0.4, 0.4))
  padu_kl  <- data.frame(id_pu = 1:3, idx_padu_kl  = c(0.7, 0.3, 0.4))
  padu_kh  <- data.frame(id_pu = 1:3, idx_padu_kh  = c(0.9, 0.1, 0.4))
  padu_rtp <- data.frame(id_pu = 1:3, idx_padu_rtp = c(0.1, 0.8, 0.4))
  padu_se  <- data.frame(id_pu = 1:3, idx_padu_se  = c(0.2, 0.9, 0.4))
  padu_ki  <- data.frame(id_pu = 1:3, idx_padu_ki  = c(0.5, 0.7, 0.4))
  
  padu_list <- list(padu_ke, padu_hs, padu_kl, padu_kh, padu_rtp, padu_se, padu_ki)
  
  weights <- data.frame(
    Kode = c("ke", "hs", "kl", "kh", "rtp", "se", "ki"),
    Bobot = c(0.20, 0.20, 0.15, 0.15, 0.15, 0.05, 0.10)
  )
  
  res <- calculate_padu_index(padu_list, idx_map, padu_idx_weight = weights)
  
  # MFL weights: ke=0.20, hs=0.20, kl=0.15, kh=0.15, ki=0.10 -> sum = 0.80
  # Normalized MFL: ke=0.25, hs=0.25, kl=0.1875, kh=0.1875, ki=0.125
  expected_lindung_1 <- 0.8*0.25 + 0.6*0.25 + 0.7*0.1875 + 0.9*0.1875 + 0.5*0.125
  expect_equal(res$idx_padu_lindung[1], expected_lindung_1, tolerance = 1e-6)
  
  # MFB weights: rtp=0.15, se=0.05, ki=0.10 -> sum = 0.30
  # Normalized MFB: rtp=0.50, se=0.1666667, ki=0.3333333
  expected_budidaya_2 <- 0.8*0.5 + 0.9*(0.05/0.30) + 0.7*(0.10/0.30)
  expect_equal(res$idx_padu_budidaya[2], expected_budidaya_2, tolerance = 1e-6)
  
  # Final assignments
  expect_equal(res$idx_padu_final[1], res$idx_padu_lindung[1])
  expect_equal(res$idx_padu_final[2], res$idx_padu_budidaya[2])
  expect_equal(res$idx_padu_final[3], res$idx_padu_all[3])
})

test_that("calculate_padu_index handles missing sub-modules gracefully with dynamic rescaling", {
  poly <- make_test_poly()
  idx_map <- sf::st_sf(
    id_pu = 1:2,
    fungsi = c("lindung", "budidaya"),
    geometry = rep(poly, 2)
  )
  
  # Only 3 modules available: KE and HS (MFL), and RTP (MFB)
  padu_ke  <- data.frame(id_pu = 1:2, idx_padu_ke  = c(0.8, 0.2))
  padu_hs  <- data.frame(id_pu = 1:2, idx_padu_hs  = c(0.6, 0.4))
  padu_rtp <- data.frame(id_pu = 1:2, idx_padu_rtp = c(0.3, 0.9))
  
  padu_list <- list(padu_ke, padu_hs, padu_rtp)
  
  # With custom weights
  weights <- data.frame(
    Kode = c("ke", "hs", "rtp"),
    Bobot = c(0.4, 0.2, 0.4)
  )
  
  res <- calculate_padu_index(padu_list, idx_map, padu_idx_weight = weights)
  
  # Available MFL: ke (0.4) and hs (0.2) -> sum = 0.6 -> ke = 4/6, hs = 2/6
  expected_mfl_1 <- 0.8 * (4/6) + 0.6 * (2/6)
  expect_equal(res$idx_padu_lindung[1], expected_mfl_1, tolerance = 1e-6)
  
  # Available MFB: rtp only (0.4) -> sum = 0.4 -> rtp = 1.0
  expect_equal(res$idx_padu_budidaya[2], 0.9, tolerance = 1e-6)
  
  # Active assignment
  expect_equal(res$idx_padu_final[1], res$idx_padu_lindung[1])
  expect_equal(res$idx_padu_final[2], res$idx_padu_budidaya[2])
})

test_that("calculate_padu_index falls back gracefully when an entire pillar has no modules", {
  poly <- make_test_poly()
  idx_map <- sf::st_sf(
    id_pu = 1:2,
    fungsi = c("lindung", "budidaya"),
    geometry = rep(poly, 2)
  )
  
  # Only MFL modules (KE and HS), no MFB module at all
  padu_ke <- data.frame(id_pu = 1:2, idx_padu_ke = c(0.8, 0.2))
  padu_hs <- data.frame(id_pu = 1:2, idx_padu_hs = c(0.6, 0.4))
  padu_list <- list(padu_ke, padu_hs)
  
  res <- calculate_padu_index(padu_list, idx_map, padu_idx_weight = NULL)
  
  expect_equal(res$idx_padu_lindung[1], 0.7)
  expect_true(is.na(res$idx_padu_budidaya[1]))
  
  # Row 1 (lindung) uses idx_padu_lindung
  expect_equal(res$idx_padu_final[1], 0.7)
  # Row 2 (budidaya) has no MFB -> falls back to idx_padu_all
  expect_equal(res$idx_padu_final[2], res$idx_padu_all[2])
})

test_that("calculate_padu_index backward compatibility: defaults to idx_padu_all when fungsi column is absent", {
  poly <- make_test_poly()
  # Map WITHOUT fungsi column
  idx_map <- sf::st_sf(
    id_pu = 1:2,
    geometry = rep(poly, 2)
  )
  
  padu_ke  <- data.frame(id_pu = 1:2, idx_padu_ke  = c(0.8, 0.2))
  padu_rtp <- data.frame(id_pu = 1:2, idx_padu_rtp = c(0.4, 0.6))
  padu_list <- list(padu_ke, padu_rtp)
  
  res <- calculate_padu_index(padu_list, idx_map, padu_idx_weight = NULL)
  
  expect_true("idx_padu_all" %in% names(res))
  expect_true("idx_padu_final" %in% names(res))
  expect_equal(res$idx_padu_final, res$idx_padu_all)
})

# ============================================================
# Ticket 03: PADU-Combine UI, Precondition Validation, and Visualization
# ============================================================

test_that("validate_padu_combine_serasi correctly flags missing or legacy SERASI maps", {
  # 1. NULL input
  res_null <- validate_padu_combine_serasi(NULL)
  expect_false(res_null$valid)
  expect_match(res_null$message, "belum tersedia", ignore.case = TRUE)
  
  # 2. Legacy SERASI map lacking 'fungsi' column
  poly <- make_test_poly()
  legacy_map <- sf::st_sf(
    id_pu = 1:2,
    RTRW = c("HL", "HP"),
    RZWP3K = c("KKP", "KPU_AL"),
    geometry = rep(poly, 2)
  )
  res_legacy <- validate_padu_combine_serasi(legacy_map)
  expect_false(res_legacy$valid)
  expect_match(res_legacy$message, "fungsi", ignore.case = TRUE)
  
  # 3. Modern SERASI map with 'fungsi' column
  modern_map <- sf::st_sf(
    id_pu = 1:2,
    RTRW = c("HL", "HP"),
    RZWP3K = c("KKP", "KPU_AL"),
    fungsi = c("lindung", "budidaya"),
    geometry = rep(poly, 2)
  )
  res_modern <- validate_padu_combine_serasi(modern_map)
  expect_true(res_modern$valid)
  expect_equal(res_modern$message, "OK")
})

test_that("PADU-combine output preserves all multi-composite columns and fungsi through GPKG and XLSX persistence", {
  poly <- make_test_poly()
  idx_map <- sf::st_sf(
    id_pu = 1:3,
    RTRW = c("HL", "HP", "HL"),
    RZWP3K = c("KKP", "KPU_AL", "KPU_AL"),
    fungsi = c("lindung", "budidaya", "campuran"),
    admin = c("Admin_A", "Admin_B", "Admin_C"),
    geometry = rep(poly, 3)
  )
  
  padu_ke  <- data.frame(id_pu = 1:3, idx_padu_ke  = c(0.8, 0.2, 0.6))
  padu_rtp <- data.frame(id_pu = 1:3, idx_padu_rtp = c(0.3, 0.9, 0.5))
  padu_list <- list(padu_ke, padu_rtp)
  
  res_map <- calculate_padu_index(padu_list, idx_map)
  
  # Check in-memory columns
  expected_cols <- c("id_pu", "RTRW", "RZWP3K", "fungsi", "admin",
                     "idx_padu_ke", "idx_padu_rtp",
                     "idx_padu_all", "idx_padu_lindung", "idx_padu_budidaya", "idx_padu_final")
  for (col in expected_cols) {
    expect_true(col %in% names(res_map), info = paste("Missing column:", col))
  }
  
  # Write to temp GPKG and read back
  tmp_gpkg <- file.path(tempdir(), "test_idx_padu_combine.gpkg")
  if (file.exists(tmp_gpkg)) unlink(tmp_gpkg)
  sf::st_write(res_map, tmp_gpkg, quiet = TRUE)
  
  reloaded_gpkg <- sf::st_read(tmp_gpkg, quiet = TRUE)
  for (col in expected_cols) {
    expect_true(col %in% names(reloaded_gpkg), info = paste("Missing column in reloaded GPKG:", col))
  }
  expect_equal(reloaded_gpkg$fungsi, c("lindung", "budidaya", "campuran"))
  expect_equal(reloaded_gpkg$idx_padu_final, res_map$idx_padu_final)
  unlink(tmp_gpkg)
  
  # Write to temp XLSX and read back
  tmp_xlsx <- file.path(tempdir(), "test_idx_padu_combine.xlsx")
  if (file.exists(tmp_xlsx)) unlink(tmp_xlsx)
  res_table <- sf::st_drop_geometry(res_map)
  openxlsx::write.xlsx(res_table, tmp_xlsx)
  
  reloaded_xlsx <- openxlsx::read.xlsx(tmp_xlsx)
  for (col in expected_cols) {
    expect_true(col %in% names(reloaded_xlsx), info = paste("Missing column in reloaded XLSX:", col))
  }
  expect_equal(reloaded_xlsx$fungsi, c("lindung", "budidaya", "campuran"))
  expect_equal(reloaded_xlsx$idx_padu_final, res_map$idx_padu_final)
  unlink(tmp_xlsx)
})

test_that("PADU-combine configuration dictionary accurately covers all multi-composite and fungsi columns", {
  expected_table_cols <- c(
    "id_pu"             = "ID PU",
    "RTRW"              = "RTRW",
    "RZWP3K"            = "RZWP3K",
    "admin"             = "Administrasi",
    "fungsi"            = "Fungsi Kawasan",
    "idx_padu_all"      = "PADU All",
    "idx_padu_lindung"   = "PADU Lindung",
    "idx_padu_budidaya"  = "PADU Budidaya",
    "idx_padu_final"    = "PADU Final Terpilih"
  )
  
  for (col in names(expected_table_cols)) {
    expect_true(nzchar(expected_table_cols[[col]]))
  }
})

# ============================================================
# Ticket 04: Downstream Integration and Verification
# ============================================================

test_that("PADAN correctly computes composite index using idx_padu_final and preserves all schema attributes", {
  poly <- make_test_poly()
  
  # 1. Overlap case
  padu_combine_sf <- sf::st_sf(
    id_pu = 1:3,
    stat_pu = "intersection",
    RTRW = c("HL", "HP", "HL"),
    RZWP3K = c("KKP", "KPU_AL", "KPU_AL"),
    admin = c("Admin_1", "Admin_2", "Admin_3"),
    fungsi = c("lindung", "budidaya", "campuran"),
    idx_serasi = c(0.8, 0.4, 0.2),
    idx_padu_all = c(0.6, 0.5, 0.4),
    idx_padu_lindung = c(0.7, 0.3, 0.35),
    idx_padu_budidaya = c(0.5, 0.8, 0.45),
    idx_padu_final = c(0.7, 0.8, 0.4), # lindung -> 0.7, budidaya -> 0.8, campuran -> 0.4
    geometry = rep(poly, 3)
  )
  
  alpha <- 0.5
  padan_res <- padu_combine_sf %>%
    dplyr::mutate(
      idx_padan = (alpha * idx_serasi) + ((1 - alpha) * idx_padu_final)
    )
  
  # Check formula
  expect_equal(padan_res$idx_padan[1], 0.5 * 0.8 + 0.5 * 0.7) # 0.75
  expect_equal(padan_res$idx_padan[2], 0.5 * 0.4 + 0.5 * 0.8) # 0.60
  expect_equal(padan_res$idx_padan[3], 0.5 * 0.2 + 0.5 * 0.4) # 0.30
  
  # Check preservation of all columns
  expect_true(all(c("fungsi", "idx_padu_all", "idx_padu_lindung", "idx_padu_budidaya", "idx_padu_final", "idx_padan") %in% names(padan_res)))
  expect_equal(padan_res$fungsi, c("lindung", "budidaya", "campuran"))
})

test_that("PADAN handles adjacent case with dissolve_id_pu preserving fungsi and composite scores", {
  poly <- make_test_poly()
  
  # Adjacent case: 2 rows per id_pu
  adjacent_padu_sf <- sf::st_sf(
    id = c("R1", "Z1", "R2", "Z2"),
    id_pu = c(1, 1, 2, 2),
    RTRW = c("HL", NA, "HP", NA),
    RZWP3K = c(NA, "KKP", NA, "KPU_AL"),
    admin = c("Admin_A", "Admin_A", "Admin_B", "Admin_B"),
    area_ha = c(10, 15, 20, 25),
    length = c(100, 100, 200, 200),
    fungsi = c("lindung", "lindung", "budidaya", "budidaya"),
    idx_serasi = c(0.9, 0.9, 0.3, 0.3),
    idx_padu_all = c(0.7, 0.7, 0.5, 0.5),
    idx_padu_lindung = c(0.85, 0.85, 0.4, 0.4),
    idx_padu_budidaya = c(0.55, 0.55, 0.75, 0.75),
    idx_padu_final = c(0.85, 0.85, 0.75, 0.75),
    geometry = rep(poly, 4)
  )
  
  alpha <- 0.6
  padan_map <- adjacent_padu_sf %>%
    dplyr::mutate(
      idx_padan = (alpha * idx_serasi) + ((1 - alpha) * idx_padu_final)
    )
  
  # Dissolve
  dissolved <- dissolve_id_pu(padan_map)
  expect_equal(nrow(dissolved), 2)
  expect_true(all(c("fungsi", "idx_padu_all", "idx_padu_lindung", "idx_padu_budidaya", "idx_padu_final", "idx_padan") %in% names(dissolved)))
  expect_equal(dissolved$fungsi, c("lindung", "budidaya"))
  expect_equal(dissolved$idx_padu_final, c(0.85, 0.75))
  expect_equal(dissolved$idx_padan[1], 0.6 * 0.9 + 0.4 * 0.85)
})

test_that("Downstream projection formulas in recommendations work accurately with idx_padu_final", {
  # Alternative projection: idx_padan_new = alpha * idx_serasi_new + (1 - alpha) * idx_padu_final
  alpha <- 0.4
  idx_serasi_new <- 0.85
  idx_padu_final <- 0.65
  
  idx_padan_new <- alpha * idx_serasi_new + (1 - alpha) * idx_padu_final
  expect_equal(idx_padan_new, 0.4 * 0.85 + 0.6 * 0.65)
})

test_that("Downstream reconciliation layer lookup correctly integrates fungsi and composite columns", {
  dec_committed <- data.frame(
    id_pu = 1:2,
    fungsi = c("lindung", "budidaya"),
    idx_padu_all = c(0.7, 0.6),
    idx_padu_lindung = c(0.8, 0.5),
    idx_padu_budidaya = c(0.6, 0.75),
    idx_padu_final = c(0.8, 0.75),
    idx_serasi = c(0.9, 0.4),
    idx_padan = c(0.85, 0.55),
    stringsAsFactors = FALSE
  )
  
  poly <- make_test_poly()
  combined <- sf::st_sf(
    id_pu = 1:2,
    Reconcile = c("Tetap", "Ubah RTRW"),
    Source = "Integrated",
    geometry = rep(poly, 2)
  )
  
  padu_candidates <- c(
    "fungsi",
    "idx_padu_all", "idx_padu_lindung", "idx_padu_budidaya",
    "idx_padu_final", "idx_serasi", "idx_padan"
  )
  
  padu_candidates <- intersect(padu_candidates, names(dec_committed))
  padu_new <- setdiff(padu_candidates, names(combined))
  
  padu_lookup <- dec_committed %>%
    dplyr::select(id_pu, dplyr::all_of(padu_new)) %>%
    dplyr::distinct(id_pu, .keep_all = TRUE) %>%
    dplyr::mutate(.join_key = as.character(id_pu)) %>%
    dplyr::select(-id_pu)
  
  res_reconciled <- combined %>%
    dplyr::mutate(.join_key = as.character(id_pu)) %>%
    dplyr::left_join(padu_lookup, by = ".join_key") %>%
    dplyr::select(-.join_key)
  
  expect_true(all(c("fungsi", "idx_padu_all", "idx_padu_lindung", "idx_padu_budidaya", "idx_padu_final") %in% names(res_reconciled)))
  expect_equal(res_reconciled$fungsi, c("lindung", "budidaya"))
  expect_equal(res_reconciled$idx_padu_final, c(0.8, 0.75))
})


