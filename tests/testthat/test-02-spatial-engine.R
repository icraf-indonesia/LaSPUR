make_square_sf <- function(xmin, ymin, xmax, ymax, crs = 4326, attrs = list()) {
  pts <- matrix(c(
    xmin, ymin,
    xmax, ymin,
    xmax, ymax,
    xmin, ymax,
    xmin, ymin
  ), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(pts))
  sfc <- sf::st_sfc(poly, crs = crs)
  df <- as.data.frame(attrs, stringsAsFactors = FALSE)
  if (nrow(df) == 0) df <- data.frame(id_test = 1)
  sf::st_sf(df, geometry = sfc)
}

test_that("identify_overlaps finds intersection between overlapping polygons", {
  poly_rtrw <- make_square_sf(0, 0, 2, 2, crs = 4326, attrs = list(RTRW = "Hutan Lindung"))
  poly_rzwp <- make_square_sf(1, 1, 3, 3, crs = 4326, attrs = list(RZWP3K = "Zona Konservasi"))
  
  res <- identify_overlaps(poly_rtrw, poly_rzwp)
  
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1)
  expect_equal(res$stat_pu, "intersection")
  expect_equal(res$RTRW, "Hutan Lindung")
  expect_equal(res$RZWP3K, "Zona Konservasi")
  expect_true("id_pu" %in% names(res))
})

test_that("identify_overlaps returns empty sf when polygons are disjoint", {
  poly1 <- make_square_sf(0, 0, 1, 1, crs = 4326, attrs = list(RTRW = "Hutan"))
  poly2 <- make_square_sf(5, 5, 6, 6, crs = 4326, attrs = list(RZWP3K = "Laut"))
  
  res <- suppressWarnings(identify_overlaps(poly1, poly2))
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
})

test_that("identify_overlaps rejects non-polygon input geometry", {
  pt <- sf::st_sf(data.frame(a = 1), geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326))
  poly <- make_square_sf(0, 0, 1, 1, crs = 4326)
  
  expect_error(identify_overlaps(pt, poly), "x must contain polygons or multipolygons")
  expect_error(identify_overlaps(poly, pt), "y must contain polygons or multipolygons")
  expect_error(identify_overlaps(data.frame(a = 1), poly), "x must be an sf object")
})

test_that("filter_overlaps flags features by area threshold", {
  # 1 degree in UTM roughly translates to ~10,000 km^2, well above 156.25 ha
  poly_large <- make_square_sf(0, 0, 1, 1, crs = 4326, attrs = list(stat_pu = "intersection", name = "Large"))
  
  # Large threshold: 1e9 ha -> should be "luas tidak terpenuhi"
  res_unmet <- suppressWarnings(filter_overlaps(poly_large, threshold_ha = 1e9))
  expect_true("area_ha" %in% names(res_unmet))
  expect_true("area_flag" %in% names(res_unmet))
  expect_equal(res_unmet$area_flag, "luas tidak terpenuhi")
  
  # Small threshold: 1 ha -> should be "luas terpenuhi"
  res_met <- suppressWarnings(filter_overlaps(poly_large, threshold_ha = 1))
  expect_equal(res_met$area_flag, "luas terpenuhi")
  
  # Missing stat_pu column throws error
  poly_no_stat <- make_square_sf(0, 0, 1, 1, crs = 4326, attrs = list(name = "NoStat"))
  expect_error(filter_overlaps(poly_no_stat), "stat_pu")
})

test_that("identify_overlaps_union captures intersection and both unshared sides", {
  poly1 <- make_square_sf(0, 0, 2, 2, crs = 4326, attrs = list(RTRW = "Hutan"))
  poly2 <- make_square_sf(1, 1, 3, 3, crs = 4326, attrs = list(RZWP3K = "Laut"))
  
  res <- identify_overlaps_union(poly1, poly2)
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 3) # poly1-only, poly2-only, intersection
  expect_true("intersection" %in% res$stat_pu)
})

test_that("identify_adjacent detects touching pairs and namespaces IDs", {
  # Two adjacent polygons sharing edge x = 1 from y = 0 to y = 1
  poly_rtrw <- make_square_sf(0, 0, 1, 1, crs = 4326, attrs = list(RTRW = "Hutan Produksi"))
  poly_rzwp <- make_square_sf(1, 0, 2, 1, crs = 4326, attrs = list(RZWP3K = "Zona Wisata Bahari"))
  
  res <- identify_adjacent(
    rtrw = poly_rtrw,
    rzwp = poly_rzwp,
    min_area_ha = 0,
    show_detailed_progress = FALSE
  )
  
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 2) # Exactly 2 rows (one RTRW, one RZWP3K for the touching pair)
  expect_equal(res$id_pu, c(1, 1))
  expect_equal(res$id, c("R1", "Z1"))
  expect_equal(res$RTRW[!is.na(res$RTRW)], "Hutan Produksi")
  expect_equal(res$RZWP3K[!is.na(res$RZWP3K)], "Zona Wisata Bahari")
})

test_that("identify_adjacent_group assigns graph connected components", {
  # Build a 2-pair adjacent dataset (4 rows, 2 planning units)
  # Pair 1: R1 and Z1
  # Pair 2: R2 and Z2
  poly_dummy <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  sfc_dummy <- sf::st_sfc(rep(list(poly_dummy), 4), crs = 4326)
  
  adj_df <- sf::st_sf(
    id = c("R1", "Z1", "R2", "Z2"),
    id_pu = c(1, 1, 2, 2),
    RTRW = c("Hutan", NA, "Kebun", NA),
    RZWP3K = c(NA, "Laut", NA, "Pesisir"),
    geometry = sfc_dummy
  )
  
  grouped <- identify_adjacent_group(adj_df)
  expect_true("id_group" %in% names(grouped))
  expect_true("n_pairs" %in% names(grouped))
  # R1-Z1 and R2-Z2 are disjoint components, so they receive different group IDs
  expect_equal(length(unique(grouped$id_group)), 2)
})
