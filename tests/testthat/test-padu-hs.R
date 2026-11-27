# tests/testthat/test-padu-hs.R

library(testthat)
library(sf)
library(terra)
library(dplyr)
library(tibble)

make_hs_pu_sf <- function(n = 3, id_col = "id_pu", crs = 32750) {
  # Create n adjacent 1000m x 1000m squares along x axis (in UTM 50S by default)
  # Base origin (700000, 9900000) in EPSG:32750 (~118.8E, -0.9S, Central Sulawesi)
  x0 <- 700000
  y0 <- 9900000
  polys <- vector("list", n)
  for (i in seq_len(n)) {
    xmin <- x0 + (i - 1) * 1000
    xmax <- x0 + i * 1000
    ymin <- y0
    ymax <- y0 + 1000
    pts <- matrix(c(
      xmin, ymin,
      xmax, ymin,
      xmax, ymax,
      xmin, ymax,
      xmin, ymin
    ), ncol = 2, byrow = TRUE)
    polys[[i]] <- sf::st_polygon(list(pts))
  }
  sfc <- sf::st_sfc(polys, crs = 32750)
  df <- data.frame(
    id = seq_len(n),
    RTRW = paste0("Zone_", seq_len(n)),
    stringsAsFactors = FALSE
  )
  names(df)[1] <- id_col
  out <- sf::st_sf(df, geometry = sfc)
  if (crs != 32750) {
    out <- sf::st_transform(out, crs)
  }
  out
}

make_hs_river_sf <- function(x_coords = c(700500, 701500), y_coords = c(9900500, 9900500),
                             type = "LINESTRING", crs = 32750) {
  if (type == "LINESTRING") {
    geom <- sf::st_linestring(cbind(x_coords, y_coords))
  } else if (type == "POLYGON") {
    xmin <- min(x_coords); xmax <- max(x_coords)
    ymin <- min(y_coords) - 50; ymax <- max(y_coords) + 50
    pts <- matrix(c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
                  ncol = 2, byrow = TRUE)
    geom <- sf::st_polygon(list(pts))
  } else if (type == "POINT") {
    geom <- sf::st_point(c(x_coords[1], y_coords[1]))
  }
  out <- sf::st_sf(name = "Sungai Uji", geometry = sf::st_sfc(geom, crs = 32750))
  if (crs != 32750) {
    out <- sf::st_transform(out, crs)
  }
  out
}

make_hs_tss_rast <- function(pu_sf, values_per_pu) {
  pu_32750 <- sf::st_transform(pu_sf, 32750)
  bbox <- sf::st_bbox(pu_32750)
  r <- terra::rast(
    xmin = bbox["xmin"], xmax = bbox["xmax"],
    ymin = bbox["ymin"], ymax = bbox["ymax"],
    resolution = 100, crs = "EPSG:32750"
  )
  n_pu <- length(values_per_pu)
  pu_width <- (bbox["xmax"] - bbox["xmin"]) / n_pu
  xy <- terra::xyFromCell(r, 1:terra::ncell(r))
  vals <- rep(NA_real_, terra::ncell(r))
  for (i in seq_len(n_pu)) {
    x_start <- bbox["xmin"] + (i - 1) * pu_width
    x_end   <- bbox["xmin"] + i * pu_width
    mask <- xy[, 1] >= x_start & xy[, 1] <= x_end
    vals[mask] <- values_per_pu[i]
  }
  terra::values(r) <- vals
  r
}

test_that("calculate_padu_hs qualifies PUs by river buffer, normalizes qualified TSS invertedly, and computes idx_padu_hs", {
  # 4 PUs: each 1000m wide (x = 700000..704000)
  # River at x = 700500 (inside PU 1). With buffer_radius = 1000 m:
  # PU 1 (700000..701000): overlaps
  # PU 2 (701000..702000): overlaps (distance 500 m <= 1000 m)
  # PU 3 (702000..703000): outside (min distance 1500 m > 1000 m) -> river_index = NA
  # PU 4 (703000..704000): outside (min distance 2500 m > 1000 m) -> river_index = NA
  pu <- make_hs_pu_sf(4)
  river <- make_hs_river_sf(x_coords = c(700500, 700500), y_coords = c(9900200, 9900800))
  # Out-of-buffer PUs have extreme TSS (0 and 200) that MUST NOT affect qualified [min_tss, max_tss] = [10, 30]
  tss <- make_hs_tss_rast(pu, c(10, 30, 0, 200))

  res <- calculate_padu_hs(
    idx_serasi_map = pu,
    river_vect     = river,
    tss_rast       = tss,
    buffer_radius  = 1000
  )

  expect_type(res, "list")
  expect_named(res, c("idx_padu_hs_map", "idx_padu_hs"), ignore.order = TRUE)
  expect_s3_class(res$idx_padu_hs_map, "sf")
  expect_s3_class(res$idx_padu_hs, "tbl_df")

  m <- res$idx_padu_hs_map
  expected_cols <- c("id_pu", "river_index", "tss_mean", "tss_index", "idx_padu_hs")
  expect_true(all(expected_cols %in% names(m)))
  expect_true(all(expected_cols %in% names(res$idx_padu_hs)))
  expect_false("estuari_dist_mean" %in% names(m))

  expect_equal(m$river_index, c(1, 1, NA_real_, NA_real_))
  expect_equal(m$tss_mean, c(10, 30, NA_real_, NA_real_), tolerance = 1e-4)
  expect_equal(m$tss_index, c(1.0, 0.0, NA_real_, NA_real_), tolerance = 1e-4)
  expect_equal(m$idx_padu_hs, c(1.0, 0.5, NA_real_, NA_real_), tolerance = 1e-4)
})

test_that("calculate_padu_hs responds to buffer_radius and scales intermediate TSS linearly", {
  pu <- make_hs_pu_sf(4)
  river <- make_hs_river_sf(x_coords = c(700500, 700500), y_coords = c(9900200, 9900800))
  tss <- make_hs_tss_rast(pu, c(10, 20, 30, 50))

  # Narrow buffer (400 m): only PU 1 intersects (distance to PU 2 boundary is 500 m)
  res_narrow <- calculate_padu_hs(pu, river, tss, buffer_radius = 400)
  expect_equal(res_narrow$idx_padu_hs_map$river_index, c(1, NA_real_, NA_real_, NA_real_))

  # Wide buffer (2000 m): PUs 1, 2, 3 intersect (distances 0, 500, 1500 <= 2000); PU 4 outside (2500 > 2000)
  res_wide <- calculate_padu_hs(pu, river, tss, buffer_radius = 2000)
  m <- res_wide$idx_padu_hs_map
  expect_equal(m$river_index, c(1, 1, 1, NA_real_))
  expect_equal(m$tss_mean, c(10, 20, 30, NA_real_), tolerance = 1e-4)
  expect_equal(m$tss_index, c(1.0, 0.5, 0.0, NA_real_), tolerance = 1e-4)
  expect_equal(m$idx_padu_hs, c(1.0, 0.75, 0.5, NA_real_), tolerance = 1e-4)
})

test_that("calculate_padu_hs supports POLYGON/MULTILINESTRING rivers, geographic CRS, custom id_col, and multi-band TSS", {
  pu <- make_hs_pu_sf(3, id_col = "custom_id")
  tss <- make_hs_tss_rast(pu, c(10, 20, 30))
  tss_multi <- terra::rast(list(tss, tss * 10))

  # Polygon river inside PU 1
  river_poly <- make_hs_river_sf(x_coords = c(700400, 700600), y_coords = c(9900400, 9900600), type = "POLYGON")
  res_poly <- calculate_padu_hs(pu, river_poly, tss_multi, id_col = "custom_id", buffer_radius = 600)
  expect_equal(res_poly$idx_padu_hs_map$river_index, c(1, 1, NA_real_))
  expect_equal(res_poly$idx_padu_hs_map$tss_index, c(1.0, 0.0, NA_real_), tolerance = 1e-4)

  # Geographic CRS (EPSG:4326) with reordered PUs & Z-dim MULTILINESTRING in mismatched CRS (EPSG:32750)
  pu_4326 <- sf::st_transform(pu[c(3, 1, 2), ], 4326)
  river_multiline <- sf::st_sf(
    name = "Sungai Multi",
    geometry = sf::st_sfc(
      sf::st_multilinestring(list(
        cbind(c(700500, 700500), c(9900200, 9900500), c(10, 10)),
        cbind(c(700500, 700500), c(9900500, 9900800), c(10, 10))
      )),
      crs = 32750
    )
  )
  res_4326 <- calculate_padu_hs(pu_4326, river_multiline, tss_multi, id_col = "custom_id", buffer_radius = 1000)

  m4326 <- res_4326$idx_padu_hs_map
  expect_equal(sf::st_crs(m4326), sf::st_crs(4326))
  expect_equal(m4326$custom_id, c(3, 1, 2))
  # PU 3 is outside 1000m buffer; PU 1 and PU 2 qualify
  expect_equal(m4326$river_index, c(NA_real_, 1, 1))
  expect_equal(m4326$tss_index, c(NA_real_, 1.0, 0.0), tolerance = 1e-4)
  expect_equal(m4326$idx_padu_hs, c(NA_real_, 1.0, 0.5), tolerance = 1e-4)
})

test_that("calculate_padu_hs preserves NA for uncovered TSS cells instead of zero-filling", {
  pu <- make_hs_pu_sf(3)
  river <- make_hs_river_sf(x_coords = c(700500, 702500), y_coords = c(9900500, 9900500))
  # PU 1 has no TSS coverage (NaN); PU 2 = 20, PU 3 = 40
  # Without zero-fill: min_tss = 20, max_tss = 40 -> PU 2 scores 1.0, PU 3 scores 0.0, PU 1 stays NA
  tss <- make_hs_tss_rast(pu, c(NaN, 20, 40))

  res <- calculate_padu_hs(pu, river, tss, buffer_radius = 1000)
  m <- res$idx_padu_hs_map

  expect_equal(m$river_index, c(1, 1, 1))
  expect_equal(m$tss_mean, c(NA_real_, 20, 40), tolerance = 1e-4)
  expect_equal(m$tss_index, c(NA_real_, 1.0, 0.0), tolerance = 1e-4)
  expect_equal(m$idx_padu_hs, c(NA_real_, 1.0, 0.5), tolerance = 1e-4)
})

test_that("calculate_padu_hs evaluates uniform TSS against tss_good_threshold benchmark", {
  pu <- make_hs_pu_sf(2)
  river <- make_hs_river_sf(x_coords = c(700500, 701500), y_coords = c(9900500, 9900500))

  # Below threshold (10 < 20): 1 - 10 / 40 = 0.75 -> idx = (1 + 0.75) / 2 = 0.875
  res_low <- calculate_padu_hs(pu, river, make_hs_tss_rast(pu, c(10, 10)), tss_good_threshold = 20)
  expect_equal(res_low$idx_padu_hs_map$tss_index, c(0.75, 0.75), tolerance = 1e-4)
  expect_equal(res_low$idx_padu_hs_map$idx_padu_hs, c(0.875, 0.875), tolerance = 1e-4)

  # Equal to threshold (20 == 20): 1 - 20 / 40 = 0.5 -> idx = 0.75
  res_eq <- calculate_padu_hs(pu, river, make_hs_tss_rast(pu, c(20, 20)), tss_good_threshold = 20)
  expect_equal(res_eq$idx_padu_hs_map$tss_index, c(0.5, 0.5), tolerance = 1e-4)
  expect_equal(res_eq$idx_padu_hs_map$idx_padu_hs, c(0.75, 0.75), tolerance = 1e-4)

  # Above threshold (30 > 20): 1 - 30 / 40 = 0.25 -> idx = 0.625
  res_high <- calculate_padu_hs(pu, river, make_hs_tss_rast(pu, c(30, 30)), tss_good_threshold = 20)
  expect_equal(res_high$idx_padu_hs_map$tss_index, c(0.25, 0.25), tolerance = 1e-4)
  expect_equal(res_high$idx_padu_hs_map$idx_padu_hs, c(0.625, 0.625), tolerance = 1e-4)

  # Very turbid (> 2 * threshold = 40): clamped at 0 -> idx = 0.5
  res_clamped <- calculate_padu_hs(pu, river, make_hs_tss_rast(pu, c(55, 55)), tss_good_threshold = 20)
  expect_equal(res_clamped$idx_padu_hs_map$tss_index, c(0.0, 0.0), tolerance = 1e-4)
  expect_equal(res_clamped$idx_padu_hs_map$idx_padu_hs, c(0.5, 0.5), tolerance = 1e-4)
})

test_that("calculate_padu_hs handles zero overlapping PUs and all-NA qualified TSS with warnings", {
  pu <- make_hs_pu_sf(2)
  tss_valid <- make_hs_tss_rast(pu, c(10, 30))

  # Zero overlapping PUs (river 50 km away)
  river_far <- make_hs_river_sf(x_coords = c(750000, 751000), y_coords = c(9900500, 9900500))
  expect_warning(
    res_zero <- calculate_padu_hs(pu, river_far, tss_valid, buffer_radius = 5000),
    "No planning units intersect"
  )
  expect_true(all(is.na(res_zero$idx_padu_hs_map$river_index)))
  expect_true(all(is.na(res_zero$idx_padu_hs_map$tss_mean)))
  expect_true(all(is.na(res_zero$idx_padu_hs_map$tss_index)))
  expect_true(all(is.na(res_zero$idx_padu_hs_map$idx_padu_hs)))

  # Overlapping PUs exist, but all qualified PUs have NA TSS
  river_near <- make_hs_river_sf(x_coords = c(700500, 701500), y_coords = c(9900500, 9900500))
  tss_all_na <- make_hs_tss_rast(pu, c(NaN, NaN))
  expect_warning(
    res_na <- calculate_padu_hs(pu, river_near, tss_all_na, buffer_radius = 1000),
    "All TSS values"
  )
  expect_equal(res_na$idx_padu_hs_map$river_index, c(1, 1))
  expect_true(all(is.na(res_na$idx_padu_hs_map$tss_mean)))
  expect_true(all(is.na(res_na$idx_padu_hs_map$tss_index)))
  expect_true(all(is.na(res_na$idx_padu_hs_map$idx_padu_hs)))
})

test_that("calculate_padu_hs validates inputs and rejects point/empty/CRS-less geometries", {
  pu <- make_hs_pu_sf(2)
  river <- make_hs_river_sf()
  tss <- make_hs_tss_rast(pu, c(10, 20))

  expect_error(calculate_padu_hs(as.data.frame(pu), river, tss), "sf object")
  expect_error(calculate_padu_hs(pu[0, ], river, tss), "at least one feature")
  expect_error(calculate_padu_hs(pu, river, tss, id_col = "missing"), "not found")
  expect_error(calculate_padu_hs(pu, "not_sf", tss), "river_vect must be an sf")
  expect_error(calculate_padu_hs(pu, river[0, ], tss), "at least one feature")

  river_no_crs <- sf::st_set_crs(river, NA)
  expect_error(calculate_padu_hs(pu, river_no_crs, tss), "defined CRS")

  river_empty <- river
  sf::st_geometry(river_empty) <- sf::st_sfc(sf::st_linestring(), crs = 32750)
  expect_error(calculate_padu_hs(pu, river_empty, tss), "empty")

  river_point <- make_hs_river_sf(type = "POINT")
  expect_error(calculate_padu_hs(pu, river_point, tss), "linear.*polygonal")

  tss_no_crs <- tss
  terra::crs(tss_no_crs) <- ""
  expect_error(calculate_padu_hs(pu, river, tss_no_crs), "defined CRS")

  expect_error(calculate_padu_hs(pu, river, tss, buffer_radius = 0), "positive number")
  expect_error(calculate_padu_hs(pu, river, tss, buffer_radius = -100), "positive number")
  expect_error(calculate_padu_hs(pu, river, tss, tss_good_threshold = 0), "positive number")
})

test_that("calculate_padu_hs exports GPKG/XLSX cleanly and integrates with calculate_padu_index", {
  pu <- make_hs_pu_sf(3)
  river <- make_hs_river_sf(x_coords = c(700500, 700500), y_coords = c(9900200, 9900800))
  tss <- make_hs_tss_rast(pu, c(10, 30, 50))

  res <- calculate_padu_hs(pu, river, tss, buffer_radius = 1000)
  map_sf <- res$idx_padu_hs_map
  tbl_df <- res$idx_padu_hs

  tdir <- tempfile("padu_hs_e2e_")
  dir.create(tdir, recursive = TRUE)
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)

  gpkg_p <- file.path(tdir, "idx_padu_hs.gpkg")
  xlsx_p <- file.path(tdir, "idx_padu_hs.xlsx")
  sf::st_write(map_sf, gpkg_p, quiet = TRUE)
  openxlsx::write.xlsx(tbl_df, xlsx_p)

  gpkg_read <- sf::st_read(gpkg_p, quiet = TRUE)
  xlsx_read <- openxlsx::read.xlsx(xlsx_p)
  expected_cols <- c("river_index", "tss_mean", "tss_index", "idx_padu_hs")
  expect_true(all(expected_cols %in% names(gpkg_read)))
  expect_true(all(expected_cols %in% names(xlsx_read)))
  expect_equal(gpkg_read$idx_padu_hs, c(1.0, 0.5, NA_real_), tolerance = 1e-4)
  expect_equal(xlsx_read$idx_padu_hs, c(1.0, 0.5, NA_real_), tolerance = 1e-4)

  # Downstream calculate_padu_index smoke check
  pu_comb <- pu %>% dplyr::mutate(fungsi = c("lindung", "budidaya", "campuran"))
  mock_ke <- pu %>% dplyr::mutate(idx_padu_ke = c(0.6, 0.6, 0.6))
  weights <- data.frame(Kode = c("ke", "hs"), Bobot = c(0.5, 0.5), stringsAsFactors = FALSE)
  res_comb <- calculate_padu_index(list(ke = mock_ke, hs = map_sf), pu_comb, weights)
  expect_true("idx_padu_final" %in% names(res_comb))
  expect_equal(res_comb$idx_padu_lindung[1], 0.5 * 0.6 + 0.5 * 1.0, tolerance = 1e-4)
})

test_that("calculate_padu_hs buffers in true meters for non-meter projected CRS and rejects non-polygon PUs", {
  pu <- make_hs_pu_sf(3)
  river <- make_hs_river_sf(x_coords = c(700500, 700500), y_coords = c(9900200, 9900800))
  tss <- make_hs_tss_rast(pu, c(10, 30, 50))

  km_crs <- "+proj=utm +zone=50 +south +datum=WGS84 +units=km +no_defs"
  pu_km <- sf::st_transform(pu, km_crs)
  river_km <- sf::st_transform(river, km_crs)

  res_m <- calculate_padu_hs(pu, river, tss, buffer_radius = 1000)
  res_km <- calculate_padu_hs(pu_km, river_km, tss, buffer_radius = 1000)

  expect_equal(res_km$idx_padu_hs_map$river_index, res_m$idx_padu_hs_map$river_index)
  expect_equal(res_km$idx_padu_hs_map$idx_padu_hs, res_m$idx_padu_hs_map$idx_padu_hs, tolerance = 1e-4)
  expect_equal(sf::st_crs(res_km$idx_padu_hs_map), sf::st_crs(pu_km))

  # Reject POINT PU geometries upfront even if 0 overlaps would occur
  pu_pt <- sf::st_sf(
    id_pu = 1:2,
    geometry = sf::st_sfc(sf::st_point(c(750000, 9900500)), sf::st_point(c(751000, 9900500)), crs = 32750)
  )
  expect_error(calculate_padu_hs(pu_pt, river, tss, buffer_radius = 1000), "POLYGON")
})

test_that("padu_hs_server handles SHP/GPKG uploads, rejects POINT/mixed uploads, and renders 0-overlap outputs", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("DT")

  root_dir <- if (exists("proj_root")) proj_root else normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  old_wd <- getwd()
  setwd(root_dir)
  on.exit(setwd(old_wd), add = TRUE)

  source("modules/mod_padu_hs.R", local = TRUE)

  pu <- make_hs_pu_sf(3) %>%
    dplyr::mutate(RTRW = c("A", "B", "C"), RZWP3K = c("X", "Y", "Z"), admin = "Adm", area_ha = 100)
  river <- make_hs_river_sf(x_coords = c(700500, 700500), y_coords = c(9900200, 9900800))
  tss <- make_hs_tss_rast(pu, c(10, 30, 50))

  tdir <- tempfile("shiny_hs_unit_")
  dir.create(tdir, recursive = TRUE)
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)

  out_dir <- file.path(tdir, "outputs")
  dir.create(out_dir, recursive = TRUE)

  tss_path <- file.path(tdir, "tss.tif")
  terra::writeRaster(tss, tss_path, overwrite = TRUE)

  gpkg_path <- file.path(tdir, "river.gpkg")
  sf::st_write(river, gpkg_path, quiet = TRUE)

  shp_dir <- file.path(tdir, "shp_uc")
  dir.create(shp_dir)
  sf::st_write(river, file.path(shp_dir, "river.shp"), quiet = TRUE)
  shp_files <- list.files(shp_dir, full.names = TRUE)
  uc_df <- data.frame(
    name = paste0(tools::file_path_sans_ext(basename(shp_files)), ".", toupper(tools::file_ext(shp_files))),
    size = file.info(shp_files)$size,
    type = "",
    datapath = shp_files,
    stringsAsFactors = FALSE
  )

  pt_dir <- file.path(tdir, "shp_pt")
  dir.create(pt_dir)
  pt_sf <- sf::st_sf(name = "pt", geometry = sf::st_sfc(sf::st_point(c(700500, 9900500)), crs = 32750))
  sf::st_write(pt_sf, file.path(pt_dir, "pt.shp"), quiet = TRUE)
  pt_files <- list.files(pt_dir, full.names = TRUE)
  pt_df <- data.frame(
    name = basename(pt_files),
    size = file.info(pt_files)$size,
    type = "",
    datapath = pt_files,
    stringsAsFactors = FALSE
  )

  far_gpkg <- file.path(tdir, "river_far.gpkg")
  far_river <- make_hs_river_sf(x_coords = c(750000, 751000), y_coords = c(9900200, 9900800))
  sf::st_write(far_river, far_gpkg, quiet = TRUE)

  out_rv <- shiny::reactiveVal(out_dir)

  shiny::testServer(padu_hs_server, args = list(output_dir = out_rv), {
    session$userData$module_results <- list(
      serasi = list(
        inputs = list(idx_serasi_path = "serasi.gpkg", case = "overlaps"),
        result = list(idx_serasi_map = pu)
      )
    )
    session$flushReact()

    session$setInputs(tss_file = data.frame(
      name = "tss.tif", size = file.info(tss_path)$size, type = "image/tiff",
      datapath = tss_path, stringsAsFactors = FALSE
    ))
    expect_false(is.null(rv$tss_rast))

    # Reject POINT shapefile
    session$setInputs(river_file = pt_df)
    expect_null(rv$river_vect)

    # Reject mixed GPKG + SHP upload
    mixed_df <- rbind(
      data.frame(name = "river.gpkg", size = file.info(gpkg_path)$size, type = "", datapath = gpkg_path, stringsAsFactors = FALSE),
      uc_df
    )
    session$setInputs(river_file = mixed_df)
    expect_null(rv$river_vect)

    # Accept uppercase SHP sidecars
    session$setInputs(river_file = uc_df)
    expect_s3_class(rv$river_vect, "sf")

    # Accept GPKG river
    session$setInputs(river_file = data.frame(
      name = "river.gpkg", size = file.info(gpkg_path)$size, type = "",
      datapath = gpkg_path, stringsAsFactors = FALSE
    ))
    expect_s3_class(rv$river_vect, "sf")

    session$setInputs(btn_next_1 = 1)
    expect_gte(rv$unlocked, 2)

    session$setInputs(buffer_radius = 1000, btn_run = 1)
    expect_equal(rv$analysis_result$map$idx_padu_hs, c(1.0, 0.5, NA_real_), tolerance = 1e-4)
    expect_true(file.exists(rv$gpkg_path))
    expect_true(file.exists(rv$xlsx_path))
    log_dir <- file.path(out_dir, "Analisis PADU-HS", "log")
    expect_true(file.exists(file.path(log_dir, "idx_padu_hs.png")))
    expect_true(file.exists(file.path(log_dir, "buffer_sungai.png")))
    expect_true(file.exists(file.path(log_dir, "tss.png")))
    expect_true(file.exists(file.path(log_dir, "idx_padu_hs_log.rda")))

    # Ensure 0-overlap run succeeds with all-NA outputs and renders map/table cleanly
    session$setInputs(river_file = data.frame(
      name = "river_far.gpkg", size = file.info(far_gpkg)$size, type = "",
      datapath = far_gpkg, stringsAsFactors = FALSE
    ))
    session$setInputs(buffer_radius = 1000, btn_run = 2)
    expect_true(all(is.na(rv$analysis_result$map$idx_padu_hs)))
    expect_silent(output$result_map)
    expect_silent(output$result_table)
  })
})
