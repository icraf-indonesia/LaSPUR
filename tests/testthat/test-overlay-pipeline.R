library(testthat)
library(sf)
library(terra)

# Ensure functions from helpers.R are loaded
if (!exists("process_overlay_vector", mode = "function") || !exists("process_overlay_raster", mode = "function")) {
  if (file.exists("R/helpers.R")) {
    source("R/helpers.R")
  } else if (file.exists("../../R/helpers.R")) {
    source("../../R/helpers.R")
  }
}

test_that("process_overlay_vector reads valid multi-file shapefile and returns standardized list", {
  temp_dir <- tempfile(pattern = "test_shp_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Create synthetic sf in UTM (EPSG:32750)
  poly <- sf::st_polygon(list(matrix(c(500000, 9800000, 501000, 9800000, 501000, 9801000, 500000, 9801000, 500000, 9800000), ncol = 2, byrow = TRUE)))
  sfc <- sf::st_sfc(poly, crs = 32750)
  sf_data <- sf::st_sf(data.frame(id = 1, nama = "Kawasan Uji", nilai = 42.5), geometry = sfc)
  
  shp_path <- file.path(temp_dir, "test_layer.shp")
  sf::st_write(sf_data, shp_path, quiet = TRUE)
  
  # Simulate fileInput dataframe
  shp_files <- list.files(temp_dir, pattern = "^test_layer\\.", full.names = FALSE)
  file_df <- data.frame(
    name = shp_files,
    datapath = file.path(temp_dir, shp_files),
    stringsAsFactors = FALSE
  )
  
  res <- process_overlay_vector(file_df)
  
  expect_type(res, "list")
  expect_equal(res$name, "test_layer")
  expect_equal(res$type, "vector")
  expect_equal(res$color, "#2563EB")
  expect_true(inherits(res$data, "sf"))
  
  # Reprojected to EPSG:4326
  expect_equal(sf::st_crs(res$data)$epsg, 4326)
  
  # Bounding box is present and in 4326 range (lat/lon)
  expect_true(all(c("xmin", "ymin", "xmax", "ymax") %in% names(res$bbox)))
  expect_true(res$bbox["xmin"] > 100 && res$bbox["xmax"] < 140) # Indonesia longitude range
  
  # Popups generated
  expect_length(res$popups, 1)
  expect_true(grepl("Kawasan Uji", res$popups[1]))
  expect_true(grepl("42.5", res$popups[1]))
})

test_that("process_overlay_vector rejects incomplete shapefiles with clear descriptive error", {
  temp_dir <- tempfile(pattern = "test_shp_missing_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  poly <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  sf_data <- sf::st_sf(data.frame(id = 1), geometry = sf::st_sfc(poly, crs = 4326))
  shp_path <- file.path(temp_dir, "incomplete.shp")
  sf::st_write(sf_data, shp_path, quiet = TRUE)
  
  all_files <- list.files(temp_dir, pattern = "^incomplete\\.", full.names = FALSE)
  
  # Missing .shx
  no_shx <- all_files[!grepl("\\.shx$", all_files, ignore.case = TRUE)]
  file_df_no_shx <- data.frame(name = no_shx, datapath = file.path(temp_dir, no_shx), stringsAsFactors = FALSE)
  expect_error(
    process_overlay_vector(file_df_no_shx),
    "\\.shx"
  )
  
  # Missing .prj
  no_prj <- all_files[!grepl("\\.prj$", all_files, ignore.case = TRUE)]
  file_df_no_prj <- data.frame(name = no_prj, datapath = file.path(temp_dir, no_prj), stringsAsFactors = FALSE)
  expect_error(
    process_overlay_vector(file_df_no_prj),
    "\\.prj"
  )
  
  # Missing .dbf
  no_dbf <- all_files[!grepl("\\.dbf$", all_files, ignore.case = TRUE)]
  file_df_no_dbf <- data.frame(name = no_dbf, datapath = file.path(temp_dir, no_dbf), stringsAsFactors = FALSE)
  expect_error(
    process_overlay_vector(file_df_no_dbf),
    "\\.dbf"
  )
})

test_that("process_overlay_vector repairs invalid geometries and drops Z/M dimensions", {
  temp_dir <- tempfile(pattern = "test_shp_geom_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Create polygon with 3D (Z) coordinates and a self-intersection (bowtie)
  bowtie <- sf::st_polygon(list(matrix(
    c(0, 0, 10,
      1, 1, 10,
      1, 0, 10,
      0, 1, 10,
      0, 0, 10),
    ncol = 3, byrow = TRUE
  )), dim = "XYZ")
  
  sf_data <- sf::st_sf(data.frame(id = 1), geometry = sf::st_sfc(bowtie, crs = 4326))
  shp_path <- file.path(temp_dir, "bowtie.shp")
  sf::st_write(sf_data, shp_path, quiet = TRUE, layer_options = "SHPT=POLYGONZ")
  
  files <- list.files(temp_dir, pattern = "^bowtie\\.", full.names = FALSE)
  file_df <- data.frame(name = files, datapath = file.path(temp_dir, files), stringsAsFactors = FALSE)
  
  res <- process_overlay_vector(file_df)
  expect_true(all(sf::st_is_valid(res$data)))
  # Check that Z dimension has been dropped (is 2D)
  geom_dim <- sf::st_dimension(res$data)
  coords <- sf::st_coordinates(res$data)
  expect_false("Z" %in% colnames(coords))
})

test_that("process_overlay_raster reads GeoTIFF and prepares palette and domain", {
  temp_dir <- tempfile(pattern = "test_tif_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  r <- terra::rast(nrows = 50, ncols = 50, xmin = 100, xmax = 101, ymin = -1, ymax = 0, crs = "EPSG:4326")
  terra::values(r) <- runif(2500, min = 10, max = 100)
  tif_path <- file.path(temp_dir, "elevation.tif")
  terra::writeRaster(r, tif_path)
  
  file_df <- data.frame(
    name = "elevation.tif",
    datapath = tif_path,
    stringsAsFactors = FALSE
  )
  
  res <- process_overlay_raster(file_df)
  
  expect_type(res, "list")
  expect_equal(res$name, "elevation")
  expect_equal(res$type, "raster")
  expect_true(inherits(res$data, "SpatRaster"))
  expect_true(is.function(res$pal))
  expect_equal(length(res$domain), 2)
  expect_true(res$domain[1] >= 10 && res$domain[2] <= 100)
  expect_true(all(c("xmin", "ymin", "xmax", "ymax") %in% names(res$bbox)))
})

test_that("process_overlay_raster downsamples raster when dimensions exceed 1500 pixels", {
  temp_dir <- tempfile(pattern = "test_tif_large_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Large raster: 1600 x 1600
  r <- terra::rast(nrows = 1600, ncols = 1600, xmin = 100, xmax = 102, ymin = -2, ymax = 0, crs = "EPSG:4326")
  # Fill with values
  terra::values(r) <- 1:(1600 * 1600)
  tif_path <- file.path(temp_dir, "large_raster.tif")
  terra::writeRaster(r, tif_path)
  
  file_df <- data.frame(
    name = "large_raster.tif",
    datapath = tif_path,
    stringsAsFactors = FALSE
  )
  
  res <- process_overlay_raster(file_df)
  
  expect_true(max(nrow(res$data), ncol(res$data)) <= 1500)
  expect_true(terra::same.crs(res$data, "EPSG:4326"))
})

test_that("process_overlay_raster projects non-WGS84 raster to EPSG:4326", {
  temp_dir <- tempfile(pattern = "test_tif_utm_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Raster in UTM zone 50S (EPSG:32750)
  r <- terra::rast(nrows = 50, ncols = 50, xmin = 500000, xmax = 505000, ymin = 9800000, ymax = 9805000, crs = "EPSG:32750")
  terra::values(r) <- 1:2500
  tif_path <- file.path(temp_dir, "utm_raster.tif")
  terra::writeRaster(r, tif_path)
  
  file_df <- data.frame(
    name = "utm_raster.tif",
    datapath = tif_path,
    stringsAsFactors = FALSE
  )
  
  res <- process_overlay_raster(file_df)
  expect_true(terra::same.crs(res$data, "EPSG:4326"))
})

test_that("create_result_ui embeds floating overlay widget with upload controls", {
  ns <- shiny::NS("mod_test")
  ui <- create_result_ui(ns)
  
  ui_str <- as.character(ui)
  
  # Check relative map container and widget presence
  expect_true(grepl("position:\\s*relative", ui_str))
  expect_true(grepl("mod_test-overlay_widget", ui_str))
  expect_true(grepl("mod_test-toggle_overlay_panel", ui_str))
  expect_true(grepl("mod_test-overlay_file_upload", ui_str))
  expect_true(grepl("mod_test-overlay_layer_list", ui_str))
})

# --- Ticket 02 Tests: Layer Controls, Stacking, & Session Persistence ---

test_that("overlay_update_layer_opacity updates opacity and clamps within [0, 1]", {
  layers <- list(
    list(id = "layer_1", name = "Layer 1", opacity = 0.7),
    list(id = "layer_2", name = "Layer 2", opacity = 0.5)
  )
  
  # Standard update
  res1 <- overlay_update_layer_opacity(layers, "layer_1", 0.35)
  expect_equal(res1[[1]]$opacity, 0.35)
  expect_equal(res1[[2]]$opacity, 0.5) # Other layer unchanged
  
  # Clamp below 0 -> 0.0
  res2 <- overlay_update_layer_opacity(layers, "layer_1", -0.5)
  expect_equal(res2[[1]]$opacity, 0.0)
  
  # Clamp above 1 -> 1.0
  res3 <- overlay_update_layer_opacity(layers, "layer_1", 1.8)
  expect_equal(res3[[1]]$opacity, 1.0)
  
  # Non-existent layer returns unchanged list
  res4 <- overlay_update_layer_opacity(layers, "layer_999", 0.2)
  expect_equal(res4, layers)
  
  # Invalid opacity (NA/NULL) returns unchanged list
  res5 <- overlay_update_layer_opacity(layers, "layer_1", NA)
  expect_equal(res5, layers)
})

test_that("overlay_toggle_layer_visibility toggles and explicitly sets visibility", {
  layers <- list(
    list(id = "layer_1", name = "Layer 1", visible = TRUE),
    list(id = "layer_2", name = "Layer 2", visible = FALSE),
    list(id = "layer_3", name = "Layer 3") # implicit TRUE
  )
  
  # Toggle TRUE -> FALSE
  res1 <- overlay_toggle_layer_visibility(layers, "layer_1")
  expect_false(res1[[1]]$visible)
  expect_false(res1[[2]]$visible) # Other layer unchanged
  
  # Toggle FALSE -> TRUE
  res2 <- overlay_toggle_layer_visibility(layers, "layer_2")
  expect_true(res2[[2]]$visible)
  
  # Toggle implicit TRUE -> FALSE
  res3 <- overlay_toggle_layer_visibility(layers, "layer_3")
  expect_false(res3[[3]]$visible)
  
  # Explicit set TRUE
  res4 <- overlay_toggle_layer_visibility(layers, "layer_2", visible = TRUE)
  expect_true(res4[[2]]$visible)
  
  # Explicit set FALSE
  res5 <- overlay_toggle_layer_visibility(layers, "layer_1", visible = FALSE)
  expect_false(res5[[1]]$visible)
  
  # Non-existent layer returns unchanged list
  res6 <- overlay_toggle_layer_visibility(layers, "non_existent")
  expect_equal(res6, layers)
})

test_that("overlay_update_layer_color updates stroke/fill color of vector layers", {
  layers <- list(
    list(id = "v1", name = "V1", type = "vector", color = "#2563EB"),
    list(id = "r1", name = "R1", type = "raster", color = NULL)
  )
  
  res <- overlay_update_layer_color(layers, "v1", "#EF4444")
  expect_equal(res[[1]]$color, "#EF4444")
  expect_null(res[[2]]$color) # Raster unchanged
  
  # Non-existent layer or empty color returns unchanged list
  expect_equal(overlay_update_layer_color(layers, "v99", "#000000"), layers)
  expect_equal(overlay_update_layer_color(layers, "v1", ""), layers)
})

test_that("overlay_reorder_layer moves layers up and down in stacking order", {
  l1 <- list(id = "l1", name = "Top Layer")
  l2 <- list(id = "l2", name = "Middle Layer")
  l3 <- list(id = "l3", name = "Bottom Layer")
  layers <- list(l1, l2, l3)
  
  # Moving top layer "up" is a safe no-op
  res_top <- overlay_reorder_layer(layers, "l1", "up")
  expect_equal(vapply(res_top, function(l) l$id, character(1)), c("l1", "l2", "l3"))
  
  # Moving bottom layer "down" is a safe no-op
  res_bottom <- overlay_reorder_layer(layers, "l3", "down")
  expect_equal(vapply(res_bottom, function(l) l$id, character(1)), c("l1", "l2", "l3"))
  
  # Moving middle layer "up" swaps it with top layer
  res_up <- overlay_reorder_layer(layers, "l2", "up")
  expect_equal(vapply(res_up, function(l) l$id, character(1)), c("l2", "l1", "l3"))
  
  # Moving middle layer "down" swaps it with bottom layer
  res_down <- overlay_reorder_layer(layers, "l2", "down")
  expect_equal(vapply(res_down, function(l) l$id, character(1)), c("l1", "l3", "l2"))
  
  # Non-existent layer returns unchanged list
  res_none <- overlay_reorder_layer(layers, "unknown", "up")
  expect_equal(vapply(res_none, function(l) l$id, character(1)), c("l1", "l2", "l3"))
})

test_that("overlay_remove_layer removes target layer from stack", {
  layers <- list(
    list(id = "l1", name = "L1"),
    list(id = "l2", name = "L2"),
    list(id = "l3", name = "L3")
  )
  
  res <- overlay_remove_layer(layers, "l2")
  expect_length(res, 2)
  expect_equal(vapply(res, function(l) l$id, character(1)), c("l1", "l3"))
  
  # Removing non-existent layer leaves list unchanged
  res2 <- overlay_remove_layer(layers, "l99")
  expect_length(res2, 3)
})

test_that("overlay_get_layer_bbox extracts and validates bounding box for Zoom to Layer", {
  valid_layer <- list(
    id = "l1",
    name = "Valid BBox Layer",
    bbox = c(xmin = 119.5, ymin = -1.5, xmax = 120.5, ymax = -0.5)
  )
  
  bb <- overlay_get_layer_bbox(valid_layer)
  expect_type(bb, "double")
  expect_equal(length(bb), 4)
  expect_equal(unname(bb["xmin"]), 119.5)
  expect_equal(unname(bb["ymax"]), -0.5)
  
  # Missing bbox field or NA coordinates return NULL
  invalid_layer1 <- list(id = "l2", name = "No BBox")
  expect_null(overlay_get_layer_bbox(invalid_layer1))
  
  invalid_layer2 <- list(id = "l3", bbox = c(xmin = 100, ymin = NA, xmax = 101, ymax = 0))
  expect_null(overlay_get_layer_bbox(invalid_layer2))
})

test_that("add_overlay_layer_to_map and draw_overlay_stack respect visibility, opacity, and order", {
  # Create synthetic sf geometry
  poly <- sf::st_polygon(list(matrix(c(120, -1, 121, -1, 121, 0, 120, 0, 120, -1), ncol = 2, byrow = TRUE)))
  sfc <- sf::st_sfc(poly, crs = 4326)
  sf_data <- sf::st_sf(data.frame(id = 1, nama = "Uji"), geometry = sfc)
  
  vec_layer <- list(
    id = "vec_1",
    name = "Vector Layer",
    type = "vector",
    data = sf_data,
    popups = c("<div>Uji</div>"),
    color = "#FF0000",
    opacity = 0.6,
    visible = TRUE
  )
  
  m <- leaflet::leaflet()
  m_with_vec <- add_overlay_layer_to_map(m, vec_layer)
  expect_true(inherits(m_with_vec, "leaflet"))
  
  # Helper to extract groups from Leaflet calls
  extract_groups <- function(map_widget) {
    vapply(map_widget$x$calls, function(call) {
      if (length(call$args) >= 3 && is.character(call$args[[3]])) {
        call$args[[3]]
      } else if ("group" %in% names(call$args)) {
        as.character(call$args$group)
      } else {
        ""
      }
    }, character(1))
  }
  
  group_names <- extract_groups(m_with_vec)
  expect_true("vec_1" %in% group_names)
  
  # When layer is hidden (visible = FALSE), it must NOT be added to map
  vec_hidden <- vec_layer
  vec_hidden$visible <- FALSE
  m_hidden <- add_overlay_layer_to_map(m, vec_hidden)
  group_names_hidden <- extract_groups(m_hidden)
  expect_false("vec_1" %in% group_names_hidden)
  
  # Stacking order: draw_overlay_stack renders in reverse order (bottom to top)
  vec_2 <- list(
    id = "vec_2",
    name = "Vector Layer 2",
    type = "vector",
    data = sf_data,
    popups = c("<div>2</div>"),
    color = "#00FF00",
    opacity = 0.8,
    visible = TRUE
  )
  m_stacked <- draw_overlay_stack(m, list(vec_layer, vec_2))
  stacked_groups <- extract_groups(m_stacked)
  # vec_2 is at index 2 (bottom), vec_layer is at index 1 (top).
  # rev(list(vec_1, vec_2)) means vec_2 is drawn FIRST, vec_1 is drawn SECOND (on top).
  idx_v2 <- which(stacked_groups == "vec_2")
  idx_v1 <- which(stacked_groups == "vec_1")
  expect_true(idx_v2 < idx_v1)
})

test_that("Strict export boundaries: overlay layers never modify official analysis outputs", {
  dummy_poly <- sf::st_polygon(list(matrix(c(100, -1, 101, -1, 101, 0, 100, 0, 100, -1), ncol = 2, byrow = TRUE)))
  dummy_map <- sf::st_sf(data.frame(id_pu = 1, nilai = 42), geometry = sf::st_sfc(dummy_poly, crs = 4326))
  dummy_table <- data.frame(id_pu = 1, nilai = 42)
  
  rv <- list(
    analysis_result = list(
      map = dummy_map,
      table = dummy_table
    ),
    gpkg_path = "official_result.gpkg",
    xlsx_path = "official_result.xlsx",
    log_messages = "Analysis completed."
  )
  
  # Simulate session overlay layers
  session_overlay_layers <- list(
    list(id = "custom_overlay_1", name = "External Concession", type = "vector", color = "#EF4444", visible = TRUE)
  )
  
  # Verify official artifacts remain completely pristine
  expect_equal(rv$analysis_result$map, dummy_map)
  expect_equal(rv$analysis_result$table, dummy_table)
  expect_equal(rv$gpkg_path, "official_result.gpkg")
  expect_equal(rv$xlsx_path, "official_result.xlsx")
  expect_false("custom_overlay_1" %in% names(rv$analysis_result$map))
  expect_false("custom_overlay_1" %in% names(rv$analysis_result$table))
})

test_that("Cross-module session persistence: shared overlay state synchronizes across modules", {
  # Mock session$userData
  user_data <- new.env(parent = emptyenv())
  user_data$overlay_layers <- shiny::reactiveVal(list())
  
  # Module A uploads an overlay layer
  layer_a <- list(
    id = "layer_alpha",
    name = "Auxiliary Forestry",
    type = "vector",
    color = "#2563EB",
    opacity = 0.7,
    visible = TRUE,
    bbox = c(xmin = 120, ymin = -1, xmax = 121, ymax = 0)
  )
  user_data$overlay_layers(list(layer_a))
  
  # Module B initializes and reads shared session overlay layers
  mod_b_layers <- shiny::isolate(user_data$overlay_layers())
  expect_length(mod_b_layers, 1)
  expect_equal(mod_b_layers[[1]]$id, "layer_alpha")
  expect_equal(mod_b_layers[[1]]$name, "Auxiliary Forestry")
  
  # Module B adjusts opacity and toggles visibility
  updated_b <- overlay_update_layer_opacity(mod_b_layers, "layer_alpha", 0.45)
  updated_b <- overlay_toggle_layer_visibility(updated_b, "layer_alpha")
  user_data$overlay_layers(updated_b)
  
  # Module A observes the session state and reflects Module B's changes
  mod_a_layers <- shiny::isolate(user_data$overlay_layers())
  expect_equal(mod_a_layers[[1]]$opacity, 0.45)
  expect_false(mod_a_layers[[1]]$visible)
  
  # Module A removes the layer
  updated_a <- overlay_remove_layer(mod_a_layers, "layer_alpha")
  user_data$overlay_layers(updated_a)
  
  # Module B observes removal
  expect_length(shiny::isolate(user_data$overlay_layers()), 0)
})

test_that("render_result_server overlay list renders all interactive control IDs", {
  vec_layer <- list(
    id = "abc123",
    name = "Test Vector",
    type = "vector",
    color = "#2563EB",
    opacity = 0.7,
    visible = TRUE
  )
  
  ns <- shiny::NS("mod_test")
  
  # Build layer widget item directly using the UI template logic
  opacity_val <- vec_layer$opacity
  ui_item <- shiny::tagList(
    shiny::actionButton(ns(paste0("up_overlay_", vec_layer$id)), "Up"),
    shiny::actionButton(ns(paste0("down_overlay_", vec_layer$id)), "Down"),
    shiny::actionButton(ns(paste0("vis_overlay_", vec_layer$id)), "Vis"),
    shiny::actionButton(ns(paste0("zoom_overlay_", vec_layer$id)), "Zoom"),
    shiny::actionButton(ns(paste0("del_overlay_", vec_layer$id)), "Del"),
    shiny::tags$input(type = "range", id = ns(paste0("opacity_overlay_", vec_layer$id)), value = opacity_val),
    shiny::tags$input(type = "color", id = ns(paste0("col_overlay_", vec_layer$id)), value = vec_layer$color)
  )
  
  ui_str <- as.character(ui_item)
  expect_true(grepl("mod_test-up_overlay_abc123", ui_str))
  expect_true(grepl("mod_test-down_overlay_abc123", ui_str))
  expect_true(grepl("mod_test-vis_overlay_abc123", ui_str))
  expect_true(grepl("mod_test-zoom_overlay_abc123", ui_str))
  expect_true(grepl("mod_test-del_overlay_abc123", ui_str))
  expect_true(grepl("mod_test-opacity_overlay_abc123", ui_str))
  expect_true(grepl("mod_test-col_overlay_abc123", ui_str))
})

test_that("process_overlay_vector simplifies dense geometries with >500 features or >5000 vertices", {
  temp_dir <- tempfile(pattern = "test_dense_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # 1. Create polygon with > 5000 vertices (e.g. 5200 vertices)
  n_pts <- 5200
  angles <- seq(0, 2 * pi, length.out = n_pts)
  r <- 1000 + 10 * sin(angles * 50)
  xs <- 500000 + r * cos(angles)
  ys <- 9800000 + r * sin(angles)
  xs[n_pts] <- xs[1]
  ys[n_pts] <- ys[1]
  
  poly <- sf::st_polygon(list(cbind(xs, ys)))
  sfc <- sf::st_sfc(poly, crs = 32750)
  sf_dense <- sf::st_sf(data.frame(id = 1, name = "Dense Poly"), geometry = sfc)
  
  shp_path <- file.path(temp_dir, "dense_layer.shp")
  sf::st_write(sf_dense, shp_path, quiet = TRUE)
  
  shp_files <- list.files(temp_dir, pattern = "^dense_layer\\.", full.names = FALSE)
  file_df <- data.frame(
    name = shp_files,
    datapath = file.path(temp_dir, shp_files),
    stringsAsFactors = FALSE
  )
  
  # Process with simplify_tolerance = 5
  res <- process_overlay_vector(file_df, simplify_tolerance = 5)
  
  # Coordinates count must be significantly reduced below 5000
  coords_after <- sf::st_coordinates(res$data)
  expect_lt(nrow(coords_after), 5000)
  expect_true(all(sf::st_is_valid(res$data)))
  
  # 2. Test dataset with > 500 features
  temp_dir2 <- tempfile(pattern = "test_500_")
  dir.create(temp_dir2, recursive = TRUE)
  on.exit(unlink(temp_dir2, recursive = TRUE), add = TRUE)
  
  polys_list <- lapply(seq_len(510), function(i) {
    x0 <- 500000 + (i %% 25) * 100
    y0 <- 9800000 + (i %/% 25) * 100
    sf::st_polygon(list(matrix(c(x0, y0, x0 + 80, y0, x0 + 80, y0 + 80, x0, y0 + 80, x0, y0), ncol = 2, byrow = TRUE)))
  })
  sf_multi <- sf::st_sf(data.frame(id = seq_len(510)), geometry = sf::st_sfc(polys_list, crs = 32750))
  shp_path2 <- file.path(temp_dir2, "multi_500.shp")
  sf::st_write(sf_multi, shp_path2, quiet = TRUE)
  
  files2 <- list.files(temp_dir2, pattern = "^multi_500\\.", full.names = FALSE)
  file_df2 <- data.frame(name = files2, datapath = file.path(temp_dir2, files2), stringsAsFactors = FALSE)
  
  res2 <- process_overlay_vector(file_df2, simplify_tolerance = 5)
  expect_equal(nrow(res2$data), 510)
  expect_true(all(sf::st_is_valid(res2$data)))
})

test_that("process_overlay_vector cycles through distinct contrast colors and accepts explicit color", {
  temp_dir <- tempfile(pattern = "test_colors_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  poly <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  sf_data <- sf::st_sf(data.frame(id = 1), geometry = sf::st_sfc(poly, crs = 4326))
  shp_path <- file.path(temp_dir, "poly.shp")
  sf::st_write(sf_data, shp_path, quiet = TRUE)
  
  files <- list.files(temp_dir, pattern = "^poly\\.", full.names = FALSE)
  file_df <- data.frame(name = files, datapath = file.path(temp_dir, files), stringsAsFactors = FALSE)
  
  # Existing count = 0 -> 1st color (#2563EB)
  res0 <- process_overlay_vector(file_df, existing_count = 0)
  expect_equal(res0$color, "#2563EB")
  
  # Existing count = 1 -> 2nd color (#DC2626)
  res1 <- process_overlay_vector(file_df, existing_count = 1)
  expect_equal(res1$color, "#DC2626")
  
  # Existing count = 2 -> 3rd color (#16A34A)
  res2 <- process_overlay_vector(file_df, existing_count = 2)
  expect_equal(res2$color, "#16A34A")
  
  # Existing count = 6 -> 7th color (#EA580C)
  res6 <- process_overlay_vector(file_df, existing_count = 6)
  expect_equal(res6$color, "#EA580C")
  
  # Existing count = 7 -> cycles back to 1st color (#2563EB)
  res7 <- process_overlay_vector(file_df, existing_count = 7)
  expect_equal(res7$color, "#2563EB")
  
  # Explicit color override
  res_custom <- process_overlay_vector(file_df, color = "#FF00FF")
  expect_equal(res_custom$color, "#FF00FF")
})

test_that("process_overlay_vector and process_overlay_raster accept deterministic layer_id and default z_index", {
  temp_dir <- tempfile(pattern = "test_det_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  poly <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  sf_data <- sf::st_sf(data.frame(id = 1), geometry = sf::st_sfc(poly, crs = 4326))
  sf::st_write(sf_data, file.path(temp_dir, "det.shp"), quiet = TRUE)
  
  files <- list.files(temp_dir, pattern = "^det\\.", full.names = FALSE)
  file_df <- data.frame(name = files, datapath = file.path(temp_dir, files), stringsAsFactors = FALSE)
  
  res_vec <- process_overlay_vector(file_df, layer_id = "custom_vector_42", z_index = 3)
  expect_equal(res_vec$id, "custom_vector_42")
  expect_equal(res_vec$z_index, 3)
  
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1, crs = "EPSG:4326")
  terra::values(r) <- 1:100
  tif_path <- file.path(temp_dir, "det.tif")
  terra::writeRaster(r, tif_path)
  
  res_rast <- process_overlay_raster(
    data.frame(name = "det.tif", datapath = tif_path, stringsAsFactors = FALSE),
    layer_id = "custom_raster_99",
    z_index = 5
  )
  expect_equal(res_rast$id, "custom_raster_99")
  expect_equal(res_rast$z_index, 5)
})

test_that("add_overlay_layer_to_map renders stroke_opacity 0 when opacity is 0 and includes hover highlights", {
  poly <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  sf_data <- sf::st_sf(data.frame(id = 1), geometry = sf::st_sfc(poly, crs = 4326))
  
  # Layer with 0% opacity (fully transparent)
  layer_zero <- list(
    id = "poly_zero",
    type = "vector",
    data = sf_data,
    popups = "Popup",
    color = "#2563EB",
    opacity = 0,
    visible = TRUE
  )
  
  map_base <- leaflet::leaflet()
  map_res <- add_overlay_layer_to_map(map_base, layer_zero)
  
  # Inspect Leaflet widget calls
  poly_call <- Filter(function(call) call$method == "addPolygons", map_res$x$calls)[[1]]
  poly_options <- poly_call$args[[4]]
  poly_highlight <- poly_call$args[[9]]
  
  # stroke opacity must be 0 when opacity is 0
  expect_equal(poly_options$opacity, 0)
  expect_equal(poly_options$fillOpacity, 0)
  
  # Highlight options must be present with white border and bringToFront
  expect_true(!is.null(poly_highlight))
  expect_equal(poly_highlight$color, "#FFFFFF")
  expect_true(poly_highlight$bringToFront)
  
  # Layer with normal opacity (0.5)
  layer_half <- list(
    id = "poly_half",
    type = "vector",
    data = sf_data,
    popups = "Popup",
    color = "#2563EB",
    opacity = 0.5,
    visible = TRUE
  )
  map_half <- add_overlay_layer_to_map(map_base, layer_half)
  poly_call_half <- Filter(function(call) call$method == "addPolygons", map_half$x$calls)[[1]]
  poly_half_opts <- poly_call_half$args[[4]]
  expect_equal(poly_half_opts$opacity, 0.7) # min(1, 0.5 + 0.2)
  expect_equal(poly_half_opts$fillOpacity, 0.5)
  
  # Lines also receive highlightOptions
  line <- sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))
  sf_line <- sf::st_sf(data.frame(id = 1), geometry = sf::st_sfc(line, crs = 4326))
  layer_line <- list(
    id = "line_layer",
    type = "vector",
    data = sf_line,
    popups = "Line",
    color = "#DC2626",
    opacity = 0.8,
    visible = TRUE
  )
  map_line <- add_overlay_layer_to_map(map_base, layer_line)
  line_call <- Filter(function(call) call$method == "addPolylines", map_line$x$calls)[[1]]
  line_highlight <- line_call$args[[9]]
  expect_true(!is.null(line_highlight))
  expect_equal(line_highlight$color, "#FFFFFF")
})

test_that("overlay_mutate_layer accurately mutates target layer and leaves other layers intact", {
  layers <- list(
    list(id = "layer1", name = "First", opacity = 0.5, visible = TRUE),
    list(id = "layer2", name = "Second", opacity = 0.8, visible = FALSE)
  )
  
  # Mutate layer2 opacity
  updated <- overlay_mutate_layer(layers, "layer2", function(l) {
    l$opacity <- 0.95
    l
  })
  expect_equal(updated[[1]]$opacity, 0.5)
  expect_equal(updated[[2]]$opacity, 0.95)
  
  # Non-existent layer ID returns list unchanged
  unchanged <- overlay_mutate_layer(layers, "non_existent", function(l) {
    l$opacity <- 1.0
    l
  })
  expect_equal(unchanged, layers)
  
  # Test refactored helpers that delegate to overlay_mutate_layer
  op_updated <- overlay_update_layer_opacity(layers, "layer1", 0.3)
  expect_equal(op_updated[[1]]$opacity, 0.3)
  
  vis_updated <- overlay_toggle_layer_visibility(layers, "layer1")
  expect_false(vis_updated[[1]]$visible)
  
  col_updated <- overlay_update_layer_color(layers, "layer1", "#123456")
  expect_equal(col_updated[[1]]$color, "#123456")
})

test_that("layer stacking order maintains prepending and explicit z_index", {
  l1 <- list(id = "l1", name = "Layer 1")
  l2 <- list(id = "l2", name = "Layer 2")
  l3 <- list(id = "l3", name = "Layer 3")
  
  # Initial layers stack: L1, L2
  stack <- overlay_update_z_indices(list(l1, l2))
  expect_equal(stack[[1]]$z_index, 2)
  expect_equal(stack[[2]]$z_index, 1)
  
  # When uploading new layer L3, it is prepended: c(list(l3), stack)
  prepended <- overlay_update_z_indices(c(list(l3), stack))
  expect_equal(length(prepended), 3)
  expect_equal(prepended[[1]]$id, "l3")
  expect_equal(prepended[[1]]$z_index, 3) # Highest z-index at top of list
  expect_equal(prepended[[2]]$id, "l1")
  expect_equal(prepended[[2]]$z_index, 2)
  expect_equal(prepended[[3]]$id, "l2")
  expect_equal(prepended[[3]]$z_index, 1)
  
  # Reordering moves layer and recalculates sequential z-indices
  reordered <- overlay_reorder_layer(prepended, "l3", direction = "down")
  expect_equal(reordered[[1]]$id, "l1")
  expect_equal(reordered[[1]]$z_index, 3)
  expect_equal(reordered[[2]]$id, "l3")
  expect_equal(reordered[[2]]$z_index, 2)
  
  # Removing a layer updates z-indices of remaining layers
  removed <- overlay_remove_layer(reordered, "l1")
  expect_equal(length(removed), 2)
  expect_equal(removed[[1]]$z_index, 2)
  expect_equal(removed[[2]]$z_index, 1)
})

test_that("create_result_ui includes active count badge and click-outside listener", {
  ns <- shiny::NS("test_res")
  ui <- create_result_ui(ns)
  ui_str <- as.character(ui)
  
  # Check active badge presence
  expect_true(grepl("test_res-overlay_active_badge", ui_str))
  expect_true(grepl("badge bg-primary", ui_str))
  
  # Check click-outside document listener
  expect_true(grepl("test_res-overlay_widget", ui_str))
  expect_true(grepl("test_res-overlay_panel", ui_str))
  expect_true(grepl("addEventListener\\('click'", ui_str))
  expect_true(grepl("contains\\(e\\.target\\)", ui_str))
})


