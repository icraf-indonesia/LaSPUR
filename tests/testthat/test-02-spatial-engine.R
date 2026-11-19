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
  res_unmet <- suppressWarnings(suppressMessages(filter_overlaps(poly_large, threshold_ha = 1e9)))
  expect_true("area_ha" %in% names(res_unmet))
  expect_true("area_flag" %in% names(res_unmet))
  expect_equal(res_unmet$area_flag, "luas tidak terpenuhi")
  
  # Small threshold: 1 ha -> should be "luas terpenuhi"
  res_met <- suppressWarnings(suppressMessages(filter_overlaps(poly_large, threshold_ha = 1)))
  expect_equal(res_met$area_flag, "luas terpenuhi")
  
  # Missing stat_pu column throws error
  poly_no_stat <- make_square_sf(0, 0, 1, 1, crs = 4326, attrs = list(name = "NoStat"))
  expect_error(filter_overlaps(poly_no_stat), "stat_pu")
})

test_that("filter_overlaps explodes multipart features into singlepart polygons", {
  # Create a MULTIPOLYGON feature with 2 disconnected squares in projected UTM (EPSG:32748)
  # Square 1: (0, 0) to (200, 200) -> 40,000 m^2 = 4 ha
  # Square 2: (500, 500) to (700, 700) -> 40,000 m^2 = 4 ha
  sq1_pts <- matrix(c(0, 0, 200, 0, 200, 200, 0, 200, 0, 0), ncol = 2, byrow = TRUE)
  sq2_pts <- matrix(c(500, 500, 700, 500, 700, 700, 500, 700, 500, 500), ncol = 2, byrow = TRUE)
  mp <- sf::st_multipolygon(list(list(sq1_pts), list(sq2_pts)))
  sf_multi <- sf::st_sf(
    data.frame(stat_pu = "intersection", zone = "TestZone", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(mp, crs = 32748)
  )
  
  res <- filter_overlaps(sf_multi)
  
  expect_equal(nrow(res), 2)
  expect_true(all(sf::st_geometry_type(res) == "POLYGON"))
  expect_equal(res$id_pu, c(1, 2))
  expect_equal(res$area_ha, c(4, 4))
  expect_equal(res$stat_pu, c("intersection", "intersection"))
  expect_equal(res$zone, c("TestZone", "TestZone"))
})

test_that("filter_overlaps excludes slivers with area < 1.0 ha", {
  # Projected UTM squares:
  # Square 1: Normal parcel (200m x 200m = 4 ha >= 1.0 ha)
  # Square 2: Sliver parcel (50m x 50m = 0.25 ha < 1.0 ha)
  sq_normal <- matrix(c(0, 0, 200, 0, 200, 200, 0, 200, 0, 0), ncol = 2, byrow = TRUE)
  sq_sliver <- matrix(c(1000, 1000, 1050, 1000, 1050, 1050, 1000, 1050, 1000, 1000), ncol = 2, byrow = TRUE)
  
  # Separate rows input
  p1 <- sf::st_polygon(list(sq_normal))
  p2 <- sf::st_polygon(list(sq_sliver))
  sf_parcels <- sf::st_sf(
    data.frame(stat_pu = c("intersection", "intersection"), type = c("normal", "sliver"), stringsAsFactors = FALSE),
    geometry = sf::st_sfc(p1, p2, crs = 32748)
  )
  
  res <- filter_overlaps(sf_parcels)
  
  expect_equal(nrow(res), 1)
  expect_equal(res$type, "normal")
  expect_equal(res$id_pu, 1)
  expect_equal(res$area_ha, 4)
  
  # Sliver inside a MULTIPOLYGON input
  mp <- sf::st_multipolygon(list(list(sq_normal), list(sq_sliver)))
  sf_mp <- sf::st_sf(
    data.frame(stat_pu = "intersection", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(mp, crs = 32748)
  )
  res_mp <- filter_overlaps(sf_mp)
  expect_equal(nrow(res_mp), 1)
  expect_equal(res_mp$id_pu, 1)
  expect_equal(res_mp$area_ha, 4)
  expect_equal(as.character(sf::st_geometry_type(res_mp)), "POLYGON")
})

test_that("filter_overlaps flags area between 1.0 ha and 156.25 ha as luas tidak terpenuhi", {
  # 500m x 500m = 250,000 m^2 = 25 ha (1.0 <= 25 < 156.25)
  sq_mid <- matrix(c(0, 0, 500, 0, 500, 500, 0, 500, 0, 0), ncol = 2, byrow = TRUE)
  sf_mid <- sf::st_sf(
    data.frame(stat_pu = "intersection", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_polygon(list(sq_mid)), crs = 32748)
  )
  
  res <- filter_overlaps(sf_mid, threshold_ha = 156.25)
  
  expect_equal(nrow(res), 1)
  expect_equal(res$area_ha, 25)
  expect_equal(res$area_flag, "luas tidak terpenuhi")
  expect_equal(res$id_pu, 1)

  # Exact boundary condition: 100m x 100m = 10,000 m^2 = exactly 1.0 ha
  sq_exact_1ha <- matrix(c(0, 0, 100, 0, 100, 100, 0, 100, 0, 0), ncol = 2, byrow = TRUE)
  sf_exact_1ha <- sf::st_sf(
    data.frame(stat_pu = "intersection", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_polygon(list(sq_exact_1ha)), crs = 32748)
  )
  res_1ha <- filter_overlaps(sf_exact_1ha, threshold_ha = 156.25, min_sliver_ha = 1.0)
  expect_equal(nrow(res_1ha), 1)
  expect_equal(res_1ha$area_ha, 1.0)
  expect_equal(res_1ha$area_flag, "luas tidak terpenuhi")
})

test_that("filter_overlaps flags area >= 156.25 ha as luas terpenuhi", {
  # 1500m x 1500m = 2,250,000 m^2 = 225 ha (>= 156.25 ha)
  sq_large <- matrix(c(0, 0, 1500, 0, 1500, 1500, 0, 1500, 0, 0), ncol = 2, byrow = TRUE)
  sf_large <- sf::st_sf(
    data.frame(stat_pu = "intersection", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_polygon(list(sq_large)), crs = 32748)
  )
  
  res <- filter_overlaps(sf_large, threshold_ha = 156.25)
  
  expect_equal(nrow(res), 1)
  expect_equal(res$area_ha, 225)
  expect_equal(res$area_flag, "luas terpenuhi")
  expect_equal(res$id_pu, 1)

  # Exact boundary condition: 1250m x 1250m = 1,562,500 m^2 = exactly 156.25 ha
  sq_exact_thresh <- matrix(c(0, 0, 1250, 0, 1250, 1250, 0, 1250, 0, 0), ncol = 2, byrow = TRUE)
  sf_exact_thresh <- sf::st_sf(
    data.frame(stat_pu = "intersection", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_polygon(list(sq_exact_thresh)), crs = 32748)
  )
  res_exact <- filter_overlaps(sf_exact_thresh, threshold_ha = 156.25)
  expect_equal(nrow(res_exact), 1)
  expect_equal(res_exact$area_ha, 156.25)
  expect_equal(res_exact$area_flag, "luas terpenuhi")
})

test_that("filter_overlaps safely handles mixed geometry types and GEOMETRYCOLLECTION", {
  # 1. GEOMETRYCOLLECTION containing a polygon (4 ha) and a linestring
  p1 <- sf::st_polygon(list(matrix(c(0, 0, 200, 0, 200, 200, 0, 200, 0, 0), ncol = 2, byrow = TRUE)))
  l1 <- sf::st_linestring(matrix(c(0, 0, 500, 500), ncol = 2, byrow = TRUE))
  gc <- sf::st_geometrycollection(list(p1, l1))
  sf_gc <- sf::st_sf(
    data.frame(stat_pu = "intersection", stringsAsFactors = FALSE),
    geom = sf::st_sfc(gc, crs = 32748)
  )
  res_gc <- filter_overlaps(sf_gc)
  expect_equal(nrow(res_gc), 1)
  expect_equal(res_gc$area_ha, 4)
  expect_equal(as.character(sf::st_geometry_type(res_gc)), "POLYGON")

  # 2. Mixed POLYGON and standalone LINESTRING rows
  sf_mixed <- sf::st_sf(
    data.frame(stat_pu = c("intersection", "intersection"), stringsAsFactors = FALSE),
    geom = sf::st_sfc(p1, l1, crs = 32748)
  )
  res_mixed <- filter_overlaps(sf_mixed)
  expect_equal(nrow(res_mixed), 1)
  expect_equal(res_mixed$area_ha, 4)
  expect_equal(as.character(sf::st_geometry_type(res_mixed)), "POLYGON")

  # 3. Pure non-polygon features throw clear error
  sf_line <- sf::st_sf(
    data.frame(stat_pu = "intersection", stringsAsFactors = FALSE),
    geom = sf::st_sfc(l1, crs = 32748)
  )
  expect_error(filter_overlaps(sf_line), "No polygon features remaining.")
})

test_that("filter_overlaps handles multipart exploding and reprojection with WGS84 geographic coordinates", {
  # In longlat: 0.05 deg ~ 5.5 km -> ~3,000 ha (large)
  # 0.0001 deg ~ 11 m -> ~0.012 ha (sliver < 1 ha)
  sq_geo_large <- matrix(c(106.0, -6.0, 106.05, -6.0, 106.05, -5.95, 106.0, -5.95, 106.0, -6.0), ncol = 2, byrow = TRUE)
  sq_geo_sliver <- matrix(c(106.1, -6.0, 106.1001, -6.0, 106.1001, -5.9999, 106.1, -5.9999, 106.1, -6.0), ncol = 2, byrow = TRUE)
  
  mp_geo <- sf::st_multipolygon(list(list(sq_geo_large), list(sq_geo_sliver)))
  sf_geo <- sf::st_sf(
    data.frame(stat_pu = "intersection", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(mp_geo, crs = 4326)
  )
  
  res <- suppressMessages(filter_overlaps(sf_geo))
  
  expect_equal(nrow(res), 1) # Sliver filtered out
  expect_true(res$area_ha >= 156.25)
  expect_equal(res$area_flag, "luas terpenuhi")
  expect_equal(as.character(sf::st_geometry_type(res)), "POLYGON")
  expect_false(sf::st_is_longlat(res)) # Successfully projected to UTM
})

test_that("filter_overlaps cleanly splits straddling parcels along administrative boundaries", {
  # Parcel: 400m x 200m = 80,000 m^2 = 8 ha in UTM
  p_pts <- matrix(c(0, 0, 400, 0, 400, 200, 0, 200, 0, 0), ncol = 2, byrow = TRUE)
  sf_parcel <- sf::st_sf(
    data.frame(stat_pu = "intersection", RTRW = "Hutan", RZWP3K = "Zona A", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_polygon(list(p_pts)), crs = 32748)
  )
  
  # Admin zones: District A (0 to 200 on X), District B (200 to 400 on X)
  a_pts <- matrix(c(0, -50, 200, -50, 200, 250, 0, 250, 0, -50), ncol = 2, byrow = TRUE)
  b_pts <- matrix(c(200, -50, 450, -50, 450, 250, 200, 250, 200, -50), ncol = 2, byrow = TRUE)
  admin_sf <- sf::st_sf(
    data.frame(district_name = c("District A", "District B"), stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_polygon(list(a_pts)), sf::st_polygon(list(b_pts)), crs = 32748)
  )
  
  res <- filter_overlaps(sf_parcel, threshold_ha = 156.25, min_sliver_ha = 1.0, admin_sf = admin_sf, admin_col = "district_name")
  
  expect_equal(nrow(res), 2)
  expect_true(all(sf::st_geometry_type(res) == "POLYGON"))
  expect_equal(res$id_pu, c(1, 2))
  expect_equal(res$stat_pu, c("intersection", "intersection"))
  expect_true("admin" %in% names(res))
  expect_equal(res$admin, c("District A", "District B"))
  expect_equal(res$area_ha, c(4, 4))
  expect_equal(res$area_flag, c("luas tidak terpenuhi", "luas tidak terpenuhi"))
  expect_equal(res$RTRW, c("Hutan", "Hutan"))
  expect_equal(res$RZWP3K, c("Zona A", "Zona A"))
  # Confirm admin column placement
  expect_equal(names(res)[1:3], c("id_pu", "stat_pu", "admin"))
})

test_that("filter_overlaps discards sub-parcels below min_sliver_ha produced by admin boundary cuts", {
  # Parcel: 400m x 200m = 8 ha in UTM
  p_pts <- matrix(c(0, 0, 400, 0, 400, 200, 0, 200, 0, 0), ncol = 2, byrow = TRUE)
  sf_parcel <- sf::st_sf(
    data.frame(stat_pu = "intersection", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_polygon(list(p_pts)), crs = 32748)
  )
  
  # Admin cut: District A (0 to 360m -> 7.2 ha), District B (360 to 450m -> 0.8 ha sliver < 1.0 ha)
  a_pts <- matrix(c(0, -50, 360, -50, 360, 250, 0, 250, 0, -50), ncol = 2, byrow = TRUE)
  b_pts <- matrix(c(360, -50, 450, -50, 450, 250, 360, 250, 360, -50), ncol = 2, byrow = TRUE)
  admin_sf <- sf::st_sf(
    data.frame(kab = c("Kabupaten A", "Kabupaten B"), stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_polygon(list(a_pts)), sf::st_polygon(list(b_pts)), crs = 32748)
  )
  
  res <- filter_overlaps(sf_parcel, min_sliver_ha = 1.0, admin_sf = admin_sf, admin_col = "kab")
  
  expect_equal(nrow(res), 1)
  expect_equal(res$id_pu, 1)
  expect_equal(res$admin, "Kabupaten A")
  expect_equal(res$area_ha, 7.2)
  expect_equal(as.character(sf::st_geometry_type(res)), "POLYGON")
})

test_that("filter_overlaps behaves cleanly when admin_sf is NULL or omitted", {
  p_pts <- matrix(c(0, 0, 400, 0, 400, 200, 0, 200, 0, 0), ncol = 2, byrow = TRUE)
  sf_parcel <- sf::st_sf(
    data.frame(stat_pu = "intersection", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_polygon(list(p_pts)), crs = 32748)
  )
  
  # admin_sf explicitly NULL
  res_null <- filter_overlaps(sf_parcel, admin_sf = NULL)
  expect_equal(nrow(res_null), 1)
  expect_false("admin" %in% names(res_null))
  expect_equal(res_null$area_ha, 8)
  expect_equal(res_null$id_pu, 1)
  
  # admin_sf omitted (default argument)
  res_default <- filter_overlaps(sf_parcel)
  expect_equal(nrow(res_default), 1)
  expect_false("admin" %in% names(res_default))
  expect_equal(res_default$area_ha, 8)
  expect_equal(res_default$id_pu, 1)
})

test_that("filter_overlaps reprojects admin_sf if CRS differs from sf_obj", {
  p_pts <- matrix(c(0, 0, 400, 0, 400, 200, 0, 200, 0, 0), ncol = 2, byrow = TRUE)
  sf_parcel <- sf::st_sf(
    data.frame(stat_pu = "intersection", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_polygon(list(p_pts)), crs = 32748)
  )
  
  a_pts <- matrix(c(0, -50, 200, -50, 200, 250, 0, 250, 0, -50), ncol = 2, byrow = TRUE)
  b_pts <- matrix(c(200, -50, 450, -50, 450, 250, 200, 250, 200, -50), ncol = 2, byrow = TRUE)
  admin_utm <- sf::st_sf(
    data.frame(nama_wilayah = c("Wilayah 1", "Wilayah 2"), stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_polygon(list(a_pts)), sf::st_polygon(list(b_pts)), crs = 32748)
  )
  admin_4326 <- sf::st_transform(admin_utm, 4326)
  
  res <- filter_overlaps(sf_parcel, admin_sf = admin_4326, admin_col = "nama_wilayah")
  expect_equal(nrow(res), 2)
  expect_equal(res$admin, c("Wilayah 1", "Wilayah 2"))
  expect_equal(sf::st_crs(res), sf::st_crs(32748))
  expect_true(all(abs(res$area_ha - 4.0) < 0.05)) # within reprojection tolerance
})

test_that("filter_overlaps validates admin_sf and admin_col parameters", {
  p_pts <- matrix(c(0, 0, 400, 0, 400, 200, 0, 200, 0, 0), ncol = 2, byrow = TRUE)
  sf_parcel <- sf::st_sf(
    data.frame(stat_pu = "intersection", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_polygon(list(p_pts)), crs = 32748)
  )
  
  # Non-sf admin_sf
  expect_error(filter_overlaps(sf_parcel, admin_sf = data.frame(a = 1), admin_col = "a"), "admin_sf must be an sf object")
  
  # Missing or invalid admin_col
  adm_valid <- sf::st_sf(
    data.frame(wilayah = "A", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol=2, byrow=TRUE))), crs = 32748)
  )
  expect_error(filter_overlaps(sf_parcel, admin_sf = adm_valid, admin_col = NULL), "admin_col must be a valid column name")
  expect_error(filter_overlaps(sf_parcel, admin_sf = adm_valid, admin_col = "nonexistent"), "admin_col must be a valid column name")
  expect_error(filter_overlaps(sf_parcel, admin_sf = adm_valid, admin_col = ""), "admin_col must be a valid column name")
  expect_error(filter_overlaps(sf_parcel, admin_sf = adm_valid, admin_col = NA_character_), "admin_col must be a valid column name")
  expect_error(filter_overlaps(sf_parcel, admin_sf = adm_valid, admin_col = "geometry"), "admin_col must be a valid column name")
  
  # admin_sf with pure LINESTRING geometry
  l_pts <- matrix(c(0, 100, 400, 100), ncol = 2, byrow = TRUE)
  adm_lines <- sf::st_sf(
    data.frame(wilayah = "LineA", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_linestring(l_pts), crs = 32748)
  )
  expect_error(filter_overlaps(sf_parcel, admin_sf = adm_lines, admin_col = "wilayah"), "admin_sf must contain polygon or multipolygon geometries")

  # admin_sf with missing CRS when sf_obj has CRS
  adm_no_crs <- sf::st_set_crs(adm_valid, NA)
  expect_error(filter_overlaps(sf_parcel, admin_sf = adm_no_crs, admin_col = "wilayah"), "admin_sf has missing CRS")
})

test_that("filter_overlaps handles integer/numeric admin columns and preexisting admin column", {
  p_pts <- matrix(c(0, 0, 400, 0, 400, 200, 0, 200, 0, 0), ncol = 2, byrow = TRUE)
  # sf_parcel already has a preexisting 'admin' column (e.g. from previous step)
  sf_parcel <- sf::st_sf(
    data.frame(stat_pu = "intersection", admin = "LegacyAdmin", stringsAsFactors = FALSE),
    geometry = sf::st_sfc(sf::st_polygon(list(p_pts)), crs = 32748)
  )
  
  a_pts <- matrix(c(0, -50, 200, -50, 200, 250, 0, 250, 0, -50), ncol = 2, byrow = TRUE)
  b_pts <- matrix(c(200, -50, 450, -50, 450, 250, 200, 250, 200, -50), ncol = 2, byrow = TRUE)
  # admin_sf uses integer district codes
  admin_int <- sf::st_sf(
    data.frame(kode_kab = c(101L, 102L)),
    geometry = sf::st_sfc(sf::st_polygon(list(a_pts)), sf::st_polygon(list(b_pts)), crs = 32748)
  )
  
  res <- filter_overlaps(sf_parcel, admin_sf = admin_int, admin_col = "kode_kab")
  expect_equal(nrow(res), 2)
  expect_equal(res$admin, c("101", "102"))
  expect_type(res$admin, "character")
  expect_equal(res$id_pu, c(1, 2))
  expect_equal(res$area_ha, c(4, 4))
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
