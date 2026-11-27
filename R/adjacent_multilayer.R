# R/adjacent_multilayer.R
# ==============================================================================
# Multi-Layer Adjacent Contacts & Voronoi Cookie-Cutter Evaluation Units
# ==============================================================================
# Implements geometry-deduplicated adjacent contact extraction, pair
# enrichment, Voronoi-partitioned adaptive cookie-cutter evaluation units, and
# multi-layer GeoPackage I/O for LaSPUR Step 2.

#' Ensure Geometry Is a Valid POLYGON or MULTIPOLYGON sfc
#'
#' @description
#' Normalizes an `sfc` geometry vector into a single valid `MULTIPOLYGON` `sfc`
#' object, using a fast path when the input is already a single `POLYGON` or
#' `MULTIPOLYGON` and extracting polygons from `GEOMETRYCOLLECTION` or
#' `MULTISURFACE` geometries when needed.
#'
#' @param geom_sfc An `sfc` geometry object.
#' @param crs A CRS object or EPSG code passed to [sf::st_sfc()].
#'
#' @return A length-1 `sfc_MULTIPOLYGON` object with the specified `crs`.
#' @keywords internal
.ensure_multipolygon_sfc <- function(geom_sfc, crs) {
  if (length(geom_sfc) == 0 || all(sf::st_is_empty(geom_sfc))) {
    return(sf::st_sfc(sf::st_multipolygon(), crs = crs))
  }
  if (length(geom_sfc) == 1L &&
      as.character(sf::st_geometry_type(geom_sfc)) %in% c("POLYGON", "MULTIPOLYGON")) {
    if (!isTRUE(sf::st_is_valid(geom_sfc))) geom_sfc <- sf::st_make_valid(geom_sfc)
    if (as.character(sf::st_geometry_type(geom_sfc)) %in% c("POLYGON", "MULTIPOLYGON")) {
      return(sf::st_cast(sf::st_sfc(geom_sfc[[1]], crs = crs), "MULTIPOLYGON"))
    }
  }
  g_sf <- sf::st_sf(geometry = geom_sfc, crs = crs)
  invalid <- !sf::st_is_valid(g_sf)
  if (any(invalid)) g_sf[invalid, ] <- sf::st_make_valid(g_sf[invalid, ])
  g_types <- as.character(sf::st_geometry_type(g_sf))
  if (any(g_types %in% c("GEOMETRYCOLLECTION", "MULTISURFACE"))) {
    g_sf <- suppressWarnings(sf::st_collection_extract(g_sf, "POLYGON", warn = FALSE))
  }
  g_sf <- g_sf[as.character(sf::st_geometry_type(g_sf)) %in% c("POLYGON", "MULTIPOLYGON") &
                 !sf::st_is_empty(g_sf), , drop = FALSE]
  if (nrow(g_sf) == 0) {
    return(sf::st_sfc(sf::st_multipolygon(), crs = crs))
  }
  geom_union <- if (nrow(g_sf) == 1L) sf::st_geometry(g_sf) else sf::st_union(sf::st_geometry(g_sf))
  if (!isTRUE(sf::st_is_valid(geom_union))) geom_union <- sf::st_make_valid(geom_union)
  if (as.character(sf::st_geometry_type(geom_union)) == "GEOMETRYCOLLECTION") {
    geom_union <- suppressWarnings(sf::st_collection_extract(
      sf::st_sf(geometry = geom_union), "POLYGON", warn = FALSE
    )) |> sf::st_geometry() |> sf::st_union()
  }
  sf::st_cast(sf::st_sfc(geom_union[[1]], crs = crs), "MULTIPOLYGON")
}

#' Extract Shared Contact Line Between Two Boundary Geometries
#'
#' @description
#' Pre-crops two boundary geometries (`r_b` and `z_b`) to their padded mutual
#' bounding boxes, snaps `z_crop` to `r_crop` within `tol`, and computes the
#' shared line (or point) intersection.
#'
#' @param r_b Boundary `sfg` geometry for the `RTRW` feature.
#' @param z_b Boundary `sfg` geometry for the `RZWP3K` feature.
#' @param tol Numeric snapping tolerance in CRS units (meters). Default `0.5`.
#' @param crs CRS object for the output `sfc`.
#'
#' @return A length-1 `sfc` containing the shared contact geometry (empty
#'   `LINESTRING` on no contact or error).
#' @keywords internal
.extract_shared_line <- function(r_b, z_b, tol = 0.5, crs = NA) {
  if (is.null(r_b) || is.null(z_b) || sf::st_is_empty(r_b) || sf::st_is_empty(z_b)) {
    return(sf::st_sfc(sf::st_linestring(), crs = crs))
  }
  tryCatch({
    pad <- tol + 5
    r_sfc <- sf::st_sfc(r_b, crs = crs)
    z_sfc <- sf::st_sfc(z_b, crs = crs)
    r_crop <- suppressWarnings(sf::st_crop(r_sfc, sf::st_bbox(z_b) + c(-pad, -pad, pad, pad)))
    z_crop <- suppressWarnings(sf::st_crop(z_sfc, sf::st_bbox(r_b) + c(-pad, -pad, pad, pad)))
    if (length(r_crop) == 0 || sf::st_is_empty(r_crop) ||
        length(z_crop) == 0 || sf::st_is_empty(z_crop)) {
      return(sf::st_sfc(sf::st_linestring(), crs = crs))
    }
    z_snapped <- sf::st_snap(z_crop, r_crop, tolerance = tol)
    inter <- sf::st_intersection(r_crop, z_snapped)
    if (length(inter) == 0 || sf::st_is_empty(inter)) {
      return(sf::st_sfc(sf::st_linestring(), crs = crs))
    }
    inter_types <- as.character(sf::st_geometry_type(inter))
    if (any(inter_types %in% c("GEOMETRYCOLLECTION"))) {
      lines <- suppressWarnings(sf::st_collection_extract(inter, "LINESTRING", warn = FALSE))
      if (length(lines) > 0 && !all(sf::st_is_empty(lines))) {
        return(sf::st_sfc(sf::st_union(lines), crs = crs))
      }
      pts <- suppressWarnings(sf::st_collection_extract(inter, "POINT", warn = FALSE))
      if (length(pts) > 0 && !all(sf::st_is_empty(pts))) {
        return(sf::st_sfc(sf::st_union(pts), crs = crs))
      }
      return(sf::st_sfc(sf::st_linestring(), crs = crs))
    }
    sf::st_sfc(sf::st_union(inter), crs = crs)
  }, error = function(e) {
    sf::st_sfc(sf::st_linestring(), crs = crs)
  })
}

#' Parallel Worker Task for Shared Line Extraction
#'
#' @description
#' Executes [.extract_shared_line()] for a single pair task inside a `furrr`
#' worker with `s2` disabled.
#'
#' @param t List containing `r_b` and `z_b` boundary `sfg` geometries.
#' @param snap_tolerance Numeric snapping tolerance in meters.
#' @param crs_metric Metric CRS object.
#'
#' @return A list with `geom` (`sfg`) and `len` (numeric length in meters).
#' @keywords internal
.extract_shared_line_task <- function(t, snap_tolerance, crs_metric) {
  sf::sf_use_s2(FALSE)
  ln <- .extract_shared_line(t$r_b, t$z_b, tol = snap_tolerance, crs = crs_metric)
  list(geom = ln[[1]], len = as.numeric(sf::st_length(ln)))
}

#' Clean Zone Class Name
#'
#' @description
#' Strips non-alphanumeric characters (preserving spaces, hyphens, and
#' parentheses), trims whitespace, and truncates zone names to 100 characters
#' for consistency with `identify_adjacent()`.
#'
#' @param val Scalar value representing a zone class name.
#'
#' @return A cleaned character scalar, or `NA_character_` if missing/empty.
#' @keywords internal
.clean_zone_name <- function(val) {
  if (is.na(val) || length(val) == 0) return(NA_character_)
  val <- gsub("[^A-Za-z0-9[:space:]\\-\\(\\)]", "", as.character(val))
  val <- trimws(val)
  if (nchar(val) > 100) val <- substr(val, 1, 100)
  val
}

#' Extract Adjacent Contacts Without Geometry Duplication
#'
#' @description
#' Identifies touching `RTRW` and `RZWP3K` polygon pairs, assigns connected
#' component group IDs (`id_group`) and pair degrees (`n_pairs`), stores each
#' participating polygon only once in `features`, and extracts the shared
#' contact line and boundary length for every `id_pu` pair.
#'
#' @param rtrw An `sf` polygon object representing `RTRW` zones.
#' @param rzwp An `sf` polygon object representing `RZWP3K` zones.
#' @param min_area_ha Numeric minimum polygon area in hectares. Default `1`.
#' @param nama_field_rtrw Character column name for `RTRW` zone classes.
#'   Default `"RTRW"`.
#' @param nama_field_rzwp Character column name for `RZWP3K` zone classes.
#'   Default `"RZWP3K"`.
#' @param m_precision Numeric precision in meters passed to
#'   [st_get_precision_for_meters()]. Default `1`.
#' @param snap_tolerance Numeric snapping tolerance in meters for shared line
#'   extraction. Default `0.5`.
#' @param buffer_m Numeric reference buffer width in meters used to compute
#'   `area_buffer_ha`. Default `100`.
#' @param parallel Logical; whether to extract shared lines in parallel using
#'   `furrr`. Default `FALSE`.
#' @param workers Integer number of parallel workers, or `NULL` for auto.
#'
#' @return A list with three elements:
#'   - `features`: `sf` of unique participating `RTRW` (`R*`) and `RZWP3K` (`Z*`)
#'     polygons.
#'   - `pairs`: `tibble` with one row per `id_pu` pair and contact metadata.
#'   - `shared_lines`: `sf` of extracted contact lines keyed by `id_pu`.
#' @export
extract_adjacent_contacts <- function(rtrw,
                                      rzwp,
                                      min_area_ha = 1,
                                      nama_field_rtrw = "RTRW",
                                      nama_field_rzwp = "RZWP3K",
                                      m_precision = 1,
                                      snap_tolerance = 0.5,
                                      buffer_m = 100,
                                      parallel = FALSE,
                                      workers = NULL) {
  stopifnot(inherits(rtrw, "sf"), inherits(rzwp, "sf"))

  rtrw <- sf::st_zm(rtrw, drop = TRUE, what = "ZM") |> sf::st_make_valid()
  rzwp <- sf::st_zm(rzwp, drop = TRUE, what = "ZM") |> sf::st_make_valid()

  if (isTRUE(sf::st_is_longlat(rtrw))) {
    bbox <- sf::st_bbox(rtrw)
    mean_lon <- (bbox[["xmin"]] + bbox[["xmax"]]) / 2
    mean_lat <- (bbox[["ymin"]] + bbox[["ymax"]]) / 2
    utm_zone <- floor((mean_lon + 180) / 6) + 1
    epsg_metric <- if (mean_lat >= 0) 32600 + utm_zone else 32700 + utm_zone
    message("Transformasi ke CRS metrik EPSG:", epsg_metric)
    rtrw <- sf::st_transform(rtrw, epsg_metric) |> sf::st_make_valid()
    rzwp <- sf::st_transform(rzwp, epsg_metric) |> sf::st_make_valid()
  } else if (sf::st_crs(rtrw) != sf::st_crs(rzwp)) {
    rzwp <- sf::st_transform(rzwp, sf::st_crs(rtrw)) |> sf::st_make_valid()
  }

  crs_metric <- sf::st_crs(rtrw)

  rtrw_filter <- rtrw %>%
    dplyr::mutate(
      id         = paste0("R", dplyr::row_number()),
      source     = "RTRW",
      zone_class = vapply(.data[[nama_field_rtrw]], .clean_zone_name, character(1)),
      area_ha    = as.numeric(units::set_units(sf::st_area(geometry), "ha"))
    ) %>%
    dplyr::filter(area_ha >= min_area_ha) %>%
    dplyr::select(id, source, zone_class, area_ha, geometry)

  rzwp_filter <- rzwp %>%
    dplyr::mutate(
      id         = paste0("Z", dplyr::row_number()),
      source     = "RZWP3K",
      zone_class = vapply(.data[[nama_field_rzwp]], .clean_zone_name, character(1)),
      area_ha    = as.numeric(units::set_units(sf::st_area(geometry), "ha"))
    ) %>%
    dplyr::filter(area_ha >= min_area_ha) %>%
    dplyr::select(id, source, zone_class, area_ha, geometry)

  n_precision <- st_get_precision_for_meters(rtrw_filter, m_precision, silent = TRUE)
  rtrw_fixed <- sf::st_set_precision(rtrw_filter, n_precision) |> sf::st_make_valid()
  rzwp_fixed <- sf::st_set_precision(rzwp_filter, n_precision) |> sf::st_make_valid()

  pairs_idx <- sf::st_touches(rtrw_fixed, rzwp_fixed)
  total_pairs <- sum(lengths(pairs_idx))
  if (total_pairs == 0) stop("Tidak ada pasangan bertetangga yang terdeteksi.")
  message("Total pasangan terdeteksi: ", total_pairs)

  pair_rows <- vector("list", total_pairs)
  k <- 1L
  for (i in seq_len(nrow(rtrw_fixed))) {
    idxs <- pairs_idx[[i]]
    if (length(idxs) == 0) next
    for (j in idxs) {
      pair_rows[[k]] <- data.frame(
        id_pu          = k,
        id_rtrw        = rtrw_fixed$id[i],
        id_rzwp3k      = rzwp_fixed$id[j],
        RTRW           = rtrw_fixed$zone_class[i],
        RZWP3K         = rzwp_fixed$zone_class[j],
        area_ha_rtrw   = rtrw_fixed$area_ha[i],
        area_ha_rzwp3k = rzwp_fixed$area_ha[j],
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }
  pairs_df <- dplyr::bind_rows(pair_rows) %>% tibble::as_tibble()

  g <- igraph::graph_from_data_frame(pairs_df[, c("id_rtrw", "id_rzwp3k")], directed = FALSE)
  comp <- igraph::components(g)$membership
  pairs_df$id_group <- as.integer(comp[pairs_df$id_rtrw])

  deg_tbl <- tibble::tibble(
    id      = c(pairs_df$id_rtrw, pairs_df$id_rzwp3k),
    id_pu   = c(pairs_df$id_pu, pairs_df$id_pu)
  ) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(n_pairs = dplyr::n_distinct(id_pu), .groups = "drop")

  used_r <- unique(pairs_df$id_rtrw)
  used_z <- unique(pairs_df$id_rzwp3k)

  features_sf <- dplyr::bind_rows(
    rtrw_fixed[rtrw_fixed$id %in% used_r, ],
    rzwp_fixed[rzwp_fixed$id %in% used_z, ]
  ) %>%
    dplyr::left_join(deg_tbl, by = "id") %>%
    dplyr::mutate(geometry = sf::st_cast(geometry, "MULTIPOLYGON")) %>%
    dplyr::select(id, source, zone_class, area_ha, n_pairs, geometry)

  b_r <- sf::st_boundary(sf::st_geometry(features_sf[features_sf$source == "RTRW", ]))
  names(b_r) <- features_sf$id[features_sf$source == "RTRW"]
  b_z <- sf::st_boundary(sf::st_geometry(features_sf[features_sf$source == "RZWP3K", ]))
  names(b_z) <- features_sf$id[features_sf$source == "RZWP3K"]

  pair_r_ids <- pairs_df$id_rtrw
  pair_z_ids <- pairs_df$id_rzwp3k

  res_pairs <- NULL
  if (isTRUE(parallel) && requireNamespace("furrr", quietly = TRUE)) {
    pair_tasks <- lapply(seq_len(nrow(pairs_df)), function(idx) {
      list(r_b = b_r[[pair_r_ids[idx]]], z_b = b_z[[pair_z_ids[idx]]])
    })
    res_pairs <- tryCatch({
      old_plan <- future::plan("list")
      on.exit(future::plan(old_plan), add = TRUE)
      old_max_size <- getOption("future.globals.maxSize")
      on.exit(options(future.globals.maxSize = old_max_size), add = TRUE)
      options(future.globals.maxSize = 16 * 1024^3)
      if (is.null(workers)) workers <- max(1L, future::availableCores() - 1L)
      future::plan(future::multisession, workers = workers)

      furrr::future_map(
        pair_tasks,
        .extract_shared_line_task,
        snap_tolerance = snap_tolerance,
        crs_metric     = crs_metric,
        .options       = furrr::furrr_options(
          packages = "sf",
          globals  = c(".extract_shared_line", "snap_tolerance", "crs_metric"),
          seed     = TRUE
        )
      )
    }, error = function(e) {
      warning(sprintf("[extract_adjacent_contacts] Eksekusi paralel gagal (%s); beralih ke mode sekuensial.",
                      conditionMessage(e)), call. = FALSE)
      NULL
    })
  }

  if (!is.null(res_pairs)) {
    shared_list <- lapply(res_pairs, `[[`, "geom")
    lengths_m   <- vapply(res_pairs, `[[`, numeric(1), "len")
  } else {
    shared_list <- vector("list", nrow(pairs_df))
    lengths_m   <- numeric(nrow(pairs_df))
    for (idx in seq_len(nrow(pairs_df))) {
      ln <- .extract_shared_line(
        b_r[[pair_r_ids[idx]]],
        b_z[[pair_z_ids[idx]]],
        tol = snap_tolerance,
        crs = crs_metric
      )
      shared_list[[idx]] <- ln[[1]]
      lengths_m[idx]     <- as.numeric(sf::st_length(ln))
    }
  }

  pairs_df$length         <- lengths_m
  pairs_df$area_buffer_ha <- (lengths_m * buffer_m) / 10000

  shared_lines_sf <- sf::st_sf(
    id_pu    = pairs_df$id_pu,
    geometry = sf::st_sfc(shared_list, crs = crs_metric)
  )

  list(
    features     = features_sf,
    pairs        = pairs_df,
    shared_lines = shared_lines_sf
  )
}

#' Enrich Adjacent Pairs with SERASI Scores, Zone Function, and Admin Units
#'
#' @description
#' Attaches administrative unit attributes (joined once per unique feature),
#' SERASI compatibility scores (`idx_serasi`), and zone function classifications
#' (`fungsi` via [assign_area_function()]) to the multi-layer adjacent object,
#' optionally filtering out compatible pairs (`idx_serasi == 1`).
#'
#' @param adj_obj Output list from [extract_adjacent_contacts()].
#' @param matriks_serasi Data frame/tibble of SERASI compatibility matrix with
#'   `RTRW`, `RZWP3K`, and compatibility score in columns 1–3.
#' @param rtrw_prioritas Reference table of `RTRW` zone priorities.
#' @param rzwp3k_prioritas Reference table of `RZWP3K` zone priorities.
#' @param admin_sf Optional `sf` polygon layer of administrative boundaries.
#' @param admin_col Optional character column name in `admin_sf` to join.
#' @param filter_serasi_1 Logical; if `TRUE` (default), drops pairs where
#'   `idx_serasi == 1` and synchronizes `features` and `shared_lines`.
#'
#' @return The enriched multi-layer list (`features`, `pairs`, `shared_lines`).
#' @export
enrich_adjacent_pairs <- function(adj_obj,
                                  matriks_serasi,
                                  rtrw_prioritas,
                                  rzwp3k_prioritas,
                                  admin_sf = NULL,
                                  admin_col = NULL,
                                  filter_serasi_1 = TRUE) {
  features_sf     <- adj_obj$features
  pairs_df        <- adj_obj$pairs
  shared_lines_sf <- adj_obj$shared_lines

  if (!is.null(admin_sf) && !is.null(admin_col) && nzchar(admin_col)) {
    if (sf::st_crs(admin_sf) != sf::st_crs(features_sf)) {
      admin_sf <- sf::st_transform(admin_sf, sf::st_crs(features_sf))
    }
    features_sf <- sf::st_join(
      features_sf,
      admin_sf[, admin_col, drop = FALSE],
      join = sf::st_intersects,
      largest = TRUE
    )
    names(features_sf)[names(features_sf) == admin_col] <- "admin"

    adm_lookup <- stats::setNames(as.character(features_sf$admin), features_sf$id)
    pairs_df <- pairs_df %>%
      dplyr::mutate(
        admin_rtrw   = unname(adm_lookup[id_rtrw]),
        admin_rzwp3k = unname(adm_lookup[id_rzwp3k]),
        admin        = paste0(admin_rtrw, "_", admin_rzwp3k)
      )
  }

  lookup_keys <- paste(matriks_serasi[[1]], matriks_serasi[[2]], sep = "||")
  compat_vec  <- stats::setNames(as.numeric(matriks_serasi[[3]]), lookup_keys)
  pair_keys   <- paste(pairs_df$RTRW, pairs_df$RZWP3K, sep = "||")
  pairs_df$idx_serasi <- unname(compat_vec[pair_keys])

  pairs_df <- assign_area_function(pairs_df, rtrw_prioritas, rzwp3k_prioritas)

  if (isTRUE(filter_serasi_1)) {
    pairs_df <- pairs_df %>% dplyr::filter(is.na(idx_serasi) | idx_serasi != 1)
    shared_lines_sf <- shared_lines_sf[shared_lines_sf$id_pu %in% pairs_df$id_pu, , drop = FALSE]
    used_ids <- unique(c(pairs_df$id_rtrw, pairs_df$id_rzwp3k))
    features_sf <- features_sf[features_sf$id %in% used_ids, , drop = FALSE]
  }

  list(
    features     = features_sf,
    pairs        = pairs_df,
    shared_lines = shared_lines_sf
  )
}

#' Sample Interior Points Along a Shared Contact Line
#'
#' @description
#' Proportionally allocates and samples regular points along each linestring
#' segment of a shared contact geometry, bounded between `min_pts` and
#' `max_pts` (falling back to centroid if no linestrings exist).
#'
#' @param g An `sfc` or `sfg` shared contact geometry.
#' @param pid Integer or character planning unit ID (`id_pu`).
#' @param crs CRS object for the output `sf`.
#' @param sample_step_m Target spacing between sampled points in meters.
#'   Default `100`.
#' @param min_pts Minimum number of points to sample. Default `8L`.
#' @param max_pts Maximum number of points to sample. Default `80L`.
#'
#' @return An `sf` point object with columns `id_pu` and `geometry`.
#' @keywords internal
.sample_line_points <- function(g, pid, crs, sample_step_m = 100, min_pts = 8L, max_pts = 80L) {
  g_types <- as.character(sf::st_geometry_type(g))
  if (any(g_types %in% c("LINESTRING", "MULTILINESTRING", "GEOMETRYCOLLECTION"))) {
    lns <- suppressWarnings(sf::st_collection_extract(g, "LINESTRING", warn = FALSE))
    if (length(lns) > 0) {
      lns_single <- suppressWarnings(sf::st_cast(lns, "LINESTRING"))
      seg_lens   <- as.numeric(sf::st_length(lns_single))
      keep_seg   <- is.finite(seg_lens) & seg_lens > 0
      lns_single <- lns_single[keep_seg]
      seg_lens   <- seg_lens[keep_seg]

      if (length(lns_single) > 0) {
        total_len <- sum(seg_lens)
        target_n  <- max(min_pts, min(max_pts, as.integer(ceiling(total_len / sample_step_m))))
        alloc <- as.integer(floor(target_n * (seg_lens / total_len)))
        rem   <- target_n - sum(alloc)
        if (rem > 0L) {
          ord     <- order(seg_lens, decreasing = TRUE)
          add_idx <- ord[seq_len(min(rem, length(ord)))]
          alloc[add_idx] <- alloc[add_idx] + 1L
        }
        seg_pts <- lapply(which(alloc > 0L), function(s) {
          sampled <- sf::st_line_sample(lns_single[s], n = alloc[s], type = "regular")
          suppressWarnings(sf::st_cast(sampled, "POINT"))
        })
        pts <- do.call(c, seg_pts)
        pts <- pts[!sf::st_is_empty(pts)]
        if (length(pts) > 0) {
          return(sf::st_sf(id_pu = rep(pid, length(pts)), geometry = pts, crs = crs))
        }
      }
    }
  }
  sf::st_sf(id_pu = pid, geometry = sf::st_centroid(g), crs = crs)
}

#' Build Perpendicular Voronoi Territories Around a Large Polygon
#'
#' @description
#' Samples points along all `shared_lines_sub` attached to the same large
#' polygon and constructs disjoint Voronoi territories so adjacent
#' cookie-cutter buffers partition perpendicularly without spilling into
#' neighboring territories.
#'
#' @param shared_lines_sub Subset `sf` of `shared_lines` belonging to one
#'   `big_id` group.
#' @param big_geom `sfg` geometry of the shared large polygon (used for the
#'   Voronoi envelope).
#' @param sample_step_m Point sampling interval in meters along each shared
#'   line. Default `100`.
#' @param min_pts Minimum sampled points per line. Default `8L`.
#' @param max_pts Maximum sampled points per line. Default `80L`.
#'
#' @return An `sf` polygon object with one row per `id_pu` territory.
#' @export
build_voronoi_territories <- function(shared_lines_sub,
                                      big_geom,
                                      sample_step_m = 100,
                                      min_pts = 8L,
                                      max_pts = 80L) {
  crs <- sf::st_crs(shared_lines_sub)
  if (nrow(shared_lines_sub) <= 1L) {
    return(sf::st_sf(
      id_pu    = shared_lines_sub$id_pu,
      geometry = sf::st_sfc(big_geom, crs = crs)
    ))
  }

  pts_list <- lapply(seq_len(nrow(shared_lines_sub)), function(i) {
    .sample_line_points(
      g             = sf::st_geometry(shared_lines_sub[i, ]),
      pid           = shared_lines_sub$id_pu[i],
      crs           = crs,
      sample_step_m = sample_step_m,
      min_pts       = min_pts,
      max_pts       = max_pts
    )
  })
  pts_sf <- do.call(rbind, Filter(Negate(is.null), pts_list))
  env    <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(sf::st_sfc(big_geom, crs = crs), 50)))
  v_raw  <- sf::st_voronoi(sf::st_union(pts_sf), envelope = env)
  v_poly <- sf::st_sf(
    geometry = suppressWarnings(sf::st_collection_extract(v_raw, "POLYGON", warn = FALSE)),
    crs = crs
  )
  if (nrow(v_poly) == 0) {
    return(sf::st_sf(
      id_pu    = shared_lines_sub$id_pu,
      geometry = sf::st_sfc(replicate(nrow(shared_lines_sub), big_geom, simplify = FALSE), crs = crs)
    ))
  }
  v_ids     <- pts_sf$id_pu[sf::st_nearest_feature(v_poly, pts_sf)]
  v_geom    <- sf::st_geometry(v_poly)
  idx_by_pu <- split(seq_along(v_ids), v_ids)

  terr_ids   <- as.integer(names(idx_by_pu))
  terr_geoms <- lapply(names(idx_by_pu), function(pid_chr) {
    sf::st_union(v_geom[idx_by_pu[[pid_chr]]])[[1]]
  })
  sf::st_sf(id_pu = terr_ids, geometry = sf::st_sfc(terr_geoms, crs = crs))
}

#' Process Evaluation Units for One Large-Polygon Group
#'
#' @description
#' Buffers each pair's shared contact line by its adaptive radius `clip_radius_m`,
#' optionally clips each buffer to its Voronoi territory, intersects the
#' corridors with `big_geom` in a single vectorized call, and unions the slice
#' with the corresponding small polygon.
#'
#' @param task List containing `grp` (pairs subset), `big_geom`, `small_map`,
#'   and `grp_lines`.
#' @param use_voronoi Logical; whether to partition buffers via
#'   [build_voronoi_territories()]. Default `TRUE`.
#' @param sample_step_m Numeric point sampling interval in meters for Voronoi
#'   partitioning. Default `100`.
#'
#' @return A named list of `MULTIPOLYGON` `sfg` geometries keyed by `id_pu`.
#' @keywords internal
.process_big_group_eval <- function(task, use_voronoi = TRUE, sample_step_m = 100) {
  sf::sf_use_s2(FALSE)
  grp       <- task$grp
  big_geom  <- task$big_geom
  small_map <- task$small_map
  grp_lines <- task$grp_lines
  crs       <- sf::st_crs(grp_lines)
  big_sfc   <- sf::st_sfc(big_geom, crs = crs)

  buf_list <- lapply(seq_len(nrow(grp)), function(k) {
    lg       <- sf::st_geometry(grp_lines[k, ])
    X_m      <- grp$clip_radius_m[k]
    simp_tol <- min(50, X_m * 0.02)
    gt_l     <- as.character(sf::st_geometry_type(lg))
    if (any(gt_l %in% c("LINESTRING", "MULTILINESTRING", "GEOMETRYCOLLECTION"))) {
      lg_ln <- suppressWarnings(sf::st_collection_extract(lg, "LINESTRING", warn = FALSE))
      if (length(lg_ln) > 0 && sum(as.numeric(sf::st_length(lg_ln))) > 0) {
        lg_u <- sf::st_union(lg_ln)
        lm   <- if (inherits(lg_u, "sfc_MULTILINESTRING")) {
          suppressWarnings(sf::st_line_merge(lg_u))
        } else {
          lg_u
        }
        lg_s <- sf::st_simplify(lm, dTolerance = simp_tol, preserveTopology = FALSE)
        if (sf::st_is_empty(lg_s)) lg_s <- lm
        return(sf::st_buffer(lg_s, dist = X_m, nQuadSegs = 8)[[1]])
      }
    }
    sm_s <- sf::st_simplify(sf::st_sfc(small_map[[grp$small_id[k]]], crs = crs),
                            dTolerance = simp_tol, preserveTopology = TRUE)
    sf::st_buffer(sm_s, dist = X_m, nQuadSegs = 8)[[1]]
  })
  buf_sfc <- sf::st_sfc(buf_list, crs = crs)

  if (isTRUE(use_voronoi) && nrow(grp) > 1L) {
    terr_sf <- build_voronoi_territories(
      shared_lines_sub = grp_lines,
      big_geom         = big_geom,
      sample_step_m    = sample_step_m
    )
    buf_vor_list <- lapply(seq_len(nrow(grp)), function(k) {
      ti <- match(grp$id_pu[k], terr_sf$id_pu)
      if (is.na(ti)) return(buf_sfc[[k]])
      inter <- suppressWarnings(sf::st_intersection(buf_sfc[k], sf::st_geometry(terr_sf[ti, ])))
      .ensure_multipolygon_sfc(inter, crs)[[1]]
    })
    buf_vor_sfc <- sf::st_sfc(buf_vor_list, crs = crs)
  } else {
    buf_vor_sfc <- buf_sfc
  }

  bv_sf     <- sf::st_sf(id_pu = grp$id_pu, geometry = buf_vor_sfc)
  slices_sf <- suppressWarnings(sf::st_intersection(bv_sf, big_sfc))

  out <- vector("list", nrow(grp))
  names(out) <- as.character(grp$id_pu)
  for (k in seq_len(nrow(grp))) {
    pid     <- grp$id_pu[k]
    small_g <- sf::st_sfc(small_map[[grp$small_id[k]]], crs = crs)
    sl_rows <- slices_sf[slices_sf$id_pu == pid, , drop = FALSE]
    sl_mp   <- if (nrow(sl_rows) > 0) {
      .ensure_multipolygon_sfc(sf::st_geometry(sl_rows), crs)
    } else {
      sf::st_sfc(sf::st_multipolygon(), crs = crs)
    }
    if (all(sf::st_is_empty(sl_mp)) || as.numeric(sf::st_area(sl_mp)) == 0) {
      slice_fb <- suppressWarnings(
        sf::st_intersection(sf::st_buffer(small_g, dist = grp$clip_radius_m[k], nQuadSegs = 8), big_sfc)
      )
      sl_mp <- .ensure_multipolygon_sfc(slice_fb, crs)
    }
    combined <- sf::st_union(small_g, sl_mp)
    out[[as.character(pid)]] <- .ensure_multipolygon_sfc(combined, crs)[[1]]
  }
  out
}

#' Build Symmetric Adaptive Cookie-Cutter Evaluation Units
#'
#' @description
#' Constructs one evaluation polygon per `id_pu` (`eval_units`) for downstream
#' PADU modules. When the area ratio between the `RTRW` and `RZWP3K` polygons
#' exceeds `ratio_k`, only the larger polygon is clipped around `shared_line`
#' using the equivalent circular diameter `max(min_buffer_m, 2 * sqrt(A_small / pi))`
#' and optional perpendicular Voronoi partitioning.
#'
#' @param adj_enriched Enriched multi-layer list from [enrich_adjacent_pairs()]
#'   or [filter_serasi_penafisan()].
#' @param ratio_k Numeric area ratio threshold above which the larger polygon
#'   is clipped. Default `5`.
#' @param min_buffer_m Numeric minimum clip radius in meters. Default `100`.
#' @param use_voronoi Logical; whether to partition overlapping cookie-cutter
#'   corridors on the same large polygon with Voronoi territories. Default `TRUE`.
#' @param sample_step_m Numeric point sampling interval in meters for Voronoi
#'   partitioning. Default `100`.
#' @param parallel Logical; whether to process large-polygon groups in parallel
#'   via `furrr`. Default `FALSE`.
#' @param workers Integer number of parallel workers, or `NULL` for auto.
#'
#' @return A list containing `features`, `pairs` (augmented with
#'   `clipped_side`, `clip_radius_m`, `area_eval_ha`), `eval_units` (`sf`), and
#'   `shared_lines`.
#' @export
build_eval_units <- function(adj_enriched,
                             ratio_k = 5,
                             min_buffer_m = 100,
                             use_voronoi = TRUE,
                             sample_step_m = 100,
                             parallel = FALSE,
                             workers = NULL) {
  features_sf     <- adj_enriched$features
  pairs_df        <- adj_enriched$pairs
  shared_lines_sf <- adj_enriched$shared_lines
  crs             <- sf::st_crs(features_sf)

  feat_geom_map <- sf::st_geometry(features_sf)
  names(feat_geom_map) <- features_sf$id

  pairs_df <- pairs_df %>%
    dplyr::mutate(
      area_r_m2     = area_ha_rtrw * 10000,
      area_z_m2     = area_ha_rzwp3k * 10000,
      area_ratio    = pmax(area_r_m2, area_z_m2) / pmin(area_r_m2, area_z_m2),
      needs_clip    = area_ratio > ratio_k,
      clipped_side  = dplyr::case_when(
        !needs_clip             ~ "none",
        area_z_m2 > area_r_m2   ~ "RZWP3K",
        TRUE                    ~ "RTRW"
      ),
      big_id        = dplyr::case_when(
        !needs_clip             ~ NA_character_,
        clipped_side == "RZWP3K"~ id_rzwp3k,
        TRUE                    ~ id_rtrw
      ),
      small_id      = dplyr::case_when(
        !needs_clip             ~ NA_character_,
        clipped_side == "RZWP3K"~ id_rtrw,
        TRUE                    ~ id_rzwp3k
      ),
      area_small_m2 = pmin(area_r_m2, area_z_m2),
      clip_radius_m = dplyr::if_else(
        needs_clip,
        pmax(min_buffer_m, 2 * sqrt(area_small_m2 / pi)),
        NA_real_
      )
    )

  eval_geoms <- vector("list", nrow(pairs_df))
  names(eval_geoms) <- as.character(pairs_df$id_pu)

  no_clip_idx <- which(!pairs_df$needs_clip)
  for (i in no_clip_idx) {
    rg <- feat_geom_map[[pairs_df$id_rtrw[i]]]
    zg <- feat_geom_map[[pairs_df$id_rzwp3k[i]]]
    u  <- sf::st_union(sf::st_sfc(rg, crs = crs), sf::st_sfc(zg, crs = crs))
    eval_geoms[[as.character(pairs_df$id_pu[i])]] <- .ensure_multipolygon_sfc(u, crs)[[1]]
  }

  clip_df <- pairs_df[pairs_df$needs_clip, , drop = FALSE]
  if (nrow(clip_df) > 0) {
    big_groups <- split(clip_df, clip_df$big_id)
    tasks <- lapply(names(big_groups), function(bid) {
      grp <- big_groups[[bid]]
      list(
        grp       = grp,
        big_geom  = feat_geom_map[[bid]],
        small_map = feat_geom_map[unique(grp$small_id)],
        grp_lines = shared_lines_sf[match(grp$id_pu, shared_lines_sf$id_pu), , drop = FALSE]
      )
    })

    grp_results <- NULL
    if (isTRUE(parallel) && requireNamespace("furrr", quietly = TRUE)) {
      grp_results <- tryCatch({
        old_plan <- future::plan("list")
        on.exit(future::plan(old_plan), add = TRUE)
        old_max_size <- getOption("future.globals.maxSize")
        on.exit(options(future.globals.maxSize = old_max_size), add = TRUE)
        options(future.globals.maxSize = 16 * 1024^3)
        if (is.null(workers)) workers <- max(1L, future::availableCores() - 1L)
        future::plan(future::multisession, workers = workers)

        furrr::future_map(
          tasks,
          .process_big_group_eval,
          use_voronoi   = use_voronoi,
          sample_step_m = sample_step_m,
          .options      = furrr::furrr_options(
            packages = "sf",
            globals  = c(".process_big_group_eval", ".ensure_multipolygon_sfc",
                         ".sample_line_points", "build_voronoi_territories",
                         "use_voronoi", "sample_step_m"),
            seed     = TRUE
          )
        )
      }, error = function(e) {
        warning(sprintf("[build_eval_units] Eksekusi paralel gagal (%s); beralih ke mode sekuensial.",
                        conditionMessage(e)), call. = FALSE)
        NULL
      })
    }
    if (is.null(grp_results)) {
      grp_results <- lapply(
        tasks,
        .process_big_group_eval,
        use_voronoi   = use_voronoi,
        sample_step_m = sample_step_m
      )
    }

    for (res_map in grp_results) {
      eval_geoms[names(res_map)] <- res_map
    }
  }

  eval_sfc <- sf::st_sfc(unname(eval_geoms[as.character(pairs_df$id_pu)]), crs = crs)
  pairs_df$area_eval_ha <- as.numeric(units::set_units(sf::st_area(eval_sfc), "ha"))

  eval_units_sf <- sf::st_sf(
    id_pu         = pairs_df$id_pu,
    id_group      = pairs_df$id_group,
    id_rtrw       = pairs_df$id_rtrw,
    id_rzwp3k     = pairs_df$id_rzwp3k,
    RTRW          = pairs_df$RTRW,
    RZWP3K        = pairs_df$RZWP3K,
    area_ha       = pairs_df$area_eval_ha,
    length        = pairs_df$length,
    idx_serasi    = pairs_df$idx_serasi,
    fungsi        = pairs_df$fungsi,
    clipped_side  = pairs_df$clipped_side,
    clip_radius_m = pairs_df$clip_radius_m,
    geometry      = eval_sfc
  )

  pairs_clean <- pairs_df %>%
    dplyr::select(-area_r_m2, -area_z_m2, -needs_clip, -big_id, -small_id, -area_small_m2)

  list(
    features     = features_sf,
    pairs        = pairs_clean,
    eval_units   = eval_units_sf,
    shared_lines = shared_lines_sf
  )
}

#' Write Multi-Layer Adjacent Data to GeoPackage
#'
#' @description
#' Writes `features`, `pairs` (non-spatial table), and optionally `eval_units`
#' and `shared_lines` layers into a single GeoPackage file.
#'
#' @param gpkg_path Character output file path ending in `.gpkg`.
#' @param features_sf `sf` polygon layer of unique participating features.
#' @param pairs_df Data frame/tibble of `id_pu` pairs and scores.
#' @param eval_units_sf Optional `sf` polygon layer of cookie-cutter evaluation
#'   units.
#' @param shared_lines_sf Optional `sf` line layer of shared contact boundaries.
#'
#' @return Invisibly returns `gpkg_path`.
#' @export
write_adjacent_multilayer_gpkg <- function(gpkg_path,
                                           features_sf,
                                           pairs_df,
                                           eval_units_sf = NULL,
                                           shared_lines_sf = NULL) {
  sf::st_write(features_sf, gpkg_path, layer = "features", delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(pairs_df,    gpkg_path, layer = "pairs",    delete_layer = TRUE, quiet = TRUE)
  if (!is.null(eval_units_sf)) {
    sf::st_write(eval_units_sf, gpkg_path, layer = "eval_units", delete_layer = TRUE, quiet = TRUE)
  }
  if (!is.null(shared_lines_sf)) {
    sf::st_write(shared_lines_sf, gpkg_path, layer = "shared_lines", delete_layer = TRUE, quiet = TRUE)
  }
  invisible(gpkg_path)
}

#' Read Multi-Layer Adjacent GeoPackage
#'
#' @description
#' Reads the `features`, `pairs`, and `eval_units` layers from a GeoPackage
#' created by [write_adjacent_multilayer_gpkg()].
#'
#' @param gpkg_path Character path to the multi-layer `.gpkg` file.
#'
#' @return A list with `features` (`sf`), `pairs` (`tibble`), and `eval_units`
#'   (`sf`).
#' @export
read_adjacent_multilayer_gpkg <- function(gpkg_path) {
  list(
    features   = sf::st_read(gpkg_path, layer = "features",   quiet = TRUE),
    pairs      = tibble::as_tibble(sf::st_read(gpkg_path, layer = "pairs", quiet = TRUE)),
    eval_units = sf::st_read(gpkg_path, layer = "eval_units", quiet = TRUE)
  )
}
