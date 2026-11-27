
# Export bipartite diagram  -----------------------------------------------

.bipartite_defaults <- list(
  base_fill             = "#B0B0B0",
  r_hub_palette         = c("#FF8C00", "#8B008B", "#008B8B", "#B8860B",
                            "#A0522D", "#556B2F", "#4B0082", "#CD5C5C"),
  z_hub_palette         = c("#FF1493", "#00CED1", "#7CFC00", "#FFD700",
                            "#DA70D6", "#40E0D0", "#FF6347", "#9370DB"),
  pointsize             = 16,
  res                   = 120,
  node_spacing_px       = 34,
  min_plot_area_px      = 700,
  plot_area_width_px    = 950,
  max_height_px         = 16000,
  max_width_px          = 4000,
  top_margin_lines      = 7,    
  side_margin_pad_lines = 1.4,
  table_n_cols          = 5,
  table_header_lines    = 0.9,
  table_row_step        = 0.9,
  note_lines            = 1.5,  
  bottom_pad_lines      = 3.0,  
  title_cex             = 1.4,  
  subtitle_cex          = 0.85,
  max_groups            = 200
)

# Internal helpers
.vis_natural_sort <- function(x) {
  x[order(suppressWarnings(as.integer(sub("^[A-Za-z]+", "", x))))]
}

.vis_label_text <- function(ids, deg) sprintf("%s (%d)", ids, deg)

.vis_label_cex_for <- function(n_nodes) {
  if (n_nodes > 40) 0.85 else if (n_nodes > 15) 1.0 else 1.15
}

.vis_open_png_device <- function(file, width, height, res, pointsize) {
  ok <- tryCatch({
    grDevices::png(file, width = width, height = height, res = res,
                   pointsize = pointsize, type = "cairo",
                   antialias = "subpixel")
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    grDevices::png(file, width = width, height = height, res = res,
                   pointsize = pointsize)
  }
  invisible(TRUE)
}

.vis_measure_max_strwidth_in <- function(labels, cex, res, pointsize,
                                         font = 2) {
  if (length(labels) == 0) return(0)
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 200, height = 200, res = res,
                 pointsize = pointsize, type = "cairo")
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  par(family = "sans")
  max(strwidth(labels, units = "inches", cex = cex, font = font))
}

.vis_table_lines_needed <- function(n_items, n_cols, header_lines, row_step) {
  if (n_items == 0) return(0)
  header_lines + ceiling(n_items / n_cols) * row_step
}

.vis_group_layout <- function(sub, cfg) {
  r_nodes <- .vis_natural_sort(unique(sub$r))
  z_nodes <- .vis_natural_sort(unique(sub$z))
  n_r <- length(r_nodes); n_z <- length(z_nodes)
  n_nodes <- max(n_r, n_z)
  
  r_deg <- as.integer(table(factor(sub$r, levels = r_nodes)))
  z_deg <- as.integer(table(factor(sub$z, levels = z_nodes)))
  is_r_hub <- r_deg > 1
  is_z_hub <- z_deg > 1
  
  r_lines <- .vis_table_lines_needed(sum(is_r_hub), cfg$table_n_cols,
                                     cfg$table_header_lines, cfg$table_row_step)
  z_lines <- .vis_table_lines_needed(sum(is_z_hub), cfg$table_n_cols,
                                     cfg$table_header_lines, cfg$table_row_step)
  gap <- if (r_lines > 0 && z_lines > 0) 1 else 0
  
  bottom_lines <- max(4, r_lines + gap + z_lines +
                        cfg$note_lines + cfg$bottom_pad_lines)
  
  label_cex <- .vis_label_cex_for(n_nodes)
  inches_per_line <- cfg$pointsize / 72 * 1.2
  px_per_line <- inches_per_line * cfg$res
  
  r_label_w_in <- .vis_measure_max_strwidth_in(
    .vis_label_text(r_nodes, r_deg), label_cex, cfg$res, cfg$pointsize)
  z_label_w_in <- .vis_measure_max_strwidth_in(
    .vis_label_text(z_nodes, z_deg), label_cex, cfg$res, cfg$pointsize)
  
  left_lines  <- max(3, r_label_w_in / inches_per_line + cfg$side_margin_pad_lines)
  right_lines <- max(3, z_label_w_in / inches_per_line + cfg$side_margin_pad_lines)
  
  list(r_nodes = r_nodes, z_nodes = z_nodes,
       n_r = n_r, n_z = n_z, n_nodes = n_nodes,
       r_deg = r_deg, z_deg = z_deg,
       is_r_hub = is_r_hub, is_z_hub = is_z_hub,
       bottom_lines = bottom_lines, label_cex = label_cex,
       left_lines = left_lines, right_lines = right_lines,
       px_per_line = px_per_line)
}

.vis_png_dimensions <- function(layout, cfg) {
  plot_h <- max(cfg$min_plot_area_px, cfg$node_spacing_px * layout$n_nodes)
  v_margins_px <- (cfg$top_margin_lines + layout$bottom_lines) * layout$px_per_line
  height <- min(round(plot_h + v_margins_px), cfg$max_height_px)
  
  h_margins_px <- (layout$left_lines + layout$right_lines) * layout$px_per_line
  width <- min(round(cfg$plot_area_width_px + h_margins_px), cfg$max_width_px)
  
  list(width = width, height = height)
}

.vis_draw_table_section <- function(items, header, start_line, n_cols,
                                    header_lines, row_step) {
  if (length(items) == 0) return(0)
  n_rows   <- ceiling(length(items) / n_cols)
  item_col <- ((seq_along(items) - 1) %% n_cols) + 1
  item_row <- ((seq_along(items) - 1) %/% n_cols) + 1
  x_left  <- -0.02
  x_right <-  1.02
  x_starts <- x_left + (0:(n_cols - 1)) * (x_right - x_left) / n_cols
  
  mtext(header, side = 1, line = start_line, at = x_left, adj = 0,
        cex = 0.85, col = "#1F2937", font = 2)
  sep <- paste(rep("\u2500", 60), collapse = "")
  mtext(sep, side = 1, line = start_line + 0.5, at = x_left, adj = 0,
        cex = 0.6, col = "#9CA3AF", font = 1)
  for (i in seq_along(items)) {
    x_pos  <- x_starts[item_col[i]]
    line_y <- start_line + header_lines + item_row[i] * row_step
    mtext(items[i], side = 1, line = line_y, at = x_pos, adj = 0,
          cex = 0.75, col = "#111827", font = 1)
  }
  header_lines + n_rows * row_step
}

.vis_draw_group <- function(sub, grp, layout, cfg) {
  r_nodes <- layout$r_nodes; z_nodes <- layout$z_nodes
  n_r <- layout$n_r; n_z <- layout$n_z; n_nodes <- layout$n_nodes
  is_r_hub <- layout$is_r_hub; is_z_hub <- layout$is_z_hub
  r_deg <- layout$r_deg; z_deg <- layout$z_deg
  
  make_y <- function(n) if (n == 1) 0.5 else seq(1, 0, length.out = n)
  r_y <- make_y(n_r); z_y <- make_y(n_z)
  r_x <- rep(0, n_r); z_x <- rep(1, n_z)
  
  r_hub_cols <- setNames(
    cfg$r_hub_palette[((seq_len(sum(is_r_hub)) - 1) %% length(cfg$r_hub_palette)) + 1],
    r_nodes[is_r_hub])
  z_hub_cols <- setNames(
    cfg$z_hub_palette[((seq_len(sum(is_z_hub)) - 1) %% length(cfg$z_hub_palette)) + 1],
    z_nodes[is_z_hub])
  
  r_fill <- ifelse(is_r_hub, r_hub_cols[r_nodes], cfg$base_fill)
  z_fill <- ifelse(is_z_hub, z_hub_cols[z_nodes], cfg$base_fill)
  
  r_items <- if (any(is_r_hub))
    .vis_label_text(r_nodes[is_r_hub], r_deg[is_r_hub]) else character(0)
  z_items <- if (any(is_z_hub))
    .vis_label_text(z_nodes[is_z_hub], z_deg[is_z_hub]) else character(0)

  par(mar = c(layout$bottom_lines, layout$left_lines,
              cfg$top_margin_lines, layout$right_lines),
      xaxs = "i", yaxs = "i", family = "sans")
  plot.new()
  plot.window(xlim = c(-0.02, 1.02), ylim = c(-0.03, 1.03))
  
  edge_alpha_hub    <- 0.75
  edge_alpha_nonhub <- 0.32
  for (k in seq_len(nrow(sub))) {
    ri <- match(sub$r[k], r_nodes)
    zi <- match(sub$z[k], z_nodes)
    if (is_z_hub[zi]) {
      col <- adjustcolor(z_hub_cols[[z_nodes[zi]]], alpha.f = edge_alpha_hub)
    } else if (is_r_hub[ri]) {
      col <- adjustcolor(r_hub_cols[[r_nodes[ri]]], alpha.f = edge_alpha_hub)
    } else {
      col <- adjustcolor("grey55", alpha.f = edge_alpha_nonhub)
    }
    segments(r_x[ri], r_y[ri], z_x[zi], z_y[zi], col = col, lwd = 2)
  }

  base_cex <- if (n_nodes > 40) 1.6 else if (n_nodes > 15) 1.9 else 2.2
  hub_cex  <- base_cex + 0.5
  r_cex <- ifelse(is_r_hub, hub_cex, base_cex)
  z_cex <- ifelse(is_z_hub, hub_cex, base_cex)
  points(r_x, r_y, pch = 21, bg = r_fill, col = "black", lwd = 2, cex = r_cex)
  points(z_x, z_y, pch = 21, bg = z_fill, col = "black", lwd = 2, cex = z_cex)

  label_cex <- layout$label_cex
  r_label_col <- ifelse(is_r_hub, "#0033CC", "#4B5563")
  z_label_col <- ifelse(is_z_hub, "#DC143C", "#4B5563")
  r_labels <- .vis_label_text(r_nodes, r_deg)
  z_labels <- .vis_label_text(z_nodes, z_deg)
  for (i in seq_len(n_r)) {
    mtext(r_labels[i], side = 2, at = r_y[i], line = 0.3, las = 1,
          font = 2, cex = label_cex, col = r_label_col[i])
  }
  for (i in seq_len(n_z)) {
    mtext(z_labels[i], side = 4, at = z_y[i], line = 0.3, las = 1,
          font = 2, cex = label_cex, col = z_label_col[i])
  }

  mtext(sprintf("Group %s  \u2014  %d R \u00d7 %d Z  (%d edges)",
                grp, n_r, n_z, nrow(sub)),
        side = 3, line = 5.0, at = 0.5, adj = 0.5,
        cex = cfg$title_cex, col = "#111827", font = 2)

  mtext("Colored nodes/edges = hubs; label shows degree",
        side = 3, line = 3.0, at = 0.5, adj = 0.5,
        cex = cfg$subtitle_cex, col = "#6B7280", font = 3)
  
  mtext("RTRW",   side = 3, at = 0,   line = 0.8,
        col = "#0033CC", font = 2, cex = 1.4)
  mtext("RZWP3K", side = 3, at = 1,   line = 0.8,
        col = "#DC143C", font = 2, cex = 1.4)

  cursor <- 1
  if (length(r_items) > 0) {
    used <- .vis_draw_table_section(
      items = r_items,
      header = sprintf("\u25CF RTRW-hubs (%d)", length(r_items)),
      start_line = cursor, n_cols = cfg$table_n_cols,
      header_lines = cfg$table_header_lines, row_step = cfg$table_row_step)
    cursor <- cursor + used + (if (length(z_items) > 0) 1 else 0)
  }
  if (length(z_items) > 0) {
    used <- .vis_draw_table_section(
      items = z_items,
      header = sprintf("\u25CF RZWP3K-hubs (%d)", length(z_items)),
      start_line = cursor, n_cols = cfg$table_n_cols,
      header_lines = cfg$table_header_lines, row_step = cfg$table_row_step)
    cursor <- cursor + used
  }

  note_y <- cursor + 1.5
  mtext("Note: colored nodes are hubs (degree > 1). Edge color follows the hub; grey edges link two leaves. Labels show 'ID (member count)'.",
        side = 1, line = note_y, at = -0.02, adj = 0,
        cex = 0.72, col = "#6B7280", font = 3)
}

#' Write one bipartite diagnostic PNG per id_group
#'
#' @param map      An sf object or data.frame with columns id, id_pu,
#'                 id_group, RTRW, RZWP3K. Typically the adjacent_map
#'                 produced by process_adjacent().
#' @param out_dir  Directory where PNGs will be written. Created if
#'                 missing.
#' @param ...      Optional overrides for any of the plotting defaults
#'                 (see .bipartite_defaults at the top of this file).
#' @param verbose  Print one line per written file (default TRUE).
#'
#' @return A list: n_groups (total), n_written, n_failed, out_dir.
#' @export
write_id_group_bipartite_plots <- function(map, out_dir, ..., verbose = TRUE) {
  cfg <- utils::modifyList(.bipartite_defaults, list(...))
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(out_dir)) {
    stop("Cannot create output directory: ", out_dir)
  }
  
  df <- if (inherits(map, "sf")) {
    as.data.frame(sf::st_drop_geometry(map))
  } else if (is.data.frame(map)) {
    as.data.frame(map)
  } else {
    stop("'map' must be an sf object or data frame.")
  }
  
  required <- c("id", "id_pu", "id_group", "RTRW", "RZWP3K")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  
  df$RTRW[df$RTRW == ""]     <- NA
  df$RZWP3K[df$RZWP3K == ""] <- NA
  
  # Build one row per id_pu with the R-side and Z-side ids
  pu_list <- split(seq_len(nrow(df)), df$id_pu)
  edges <- data.frame(
    id_pu    = names(pu_list),
    r        = vapply(pu_list, function(idx) {
      x <- df[idx, , drop = FALSE]
      x$id[which(!is.na(x$RTRW))][1]
    }, character(1)),
    z        = vapply(pu_list, function(idx) {
      x <- df[idx, , drop = FALSE]
      x$id[which(!is.na(x$RZWP3K))][1]
    }, character(1)),
    id_group = vapply(pu_list, function(idx) {
      as.character(df$id_group[idx[1]])
    }, character(1)),
    stringsAsFactors = FALSE
  )
  
  if (any(is.na(edges$r)) || any(is.na(edges$z))) {
    stop("Some id_pu groups lack an R-side or Z-side row.")
  }
  if (any(edges$r == edges$z)) {
    stop("Some pairs have r == z; the id column was not namespaced per layer.")
  }
  
  all_groups <- unique(edges$id_group)
  if (length(all_groups) == 0) {
    warning("No groups to plot.")
    return(list(n_groups = 0L, n_written = 0L, n_failed = 0L, out_dir = out_dir))
  }
  
  # Sort groups by node count (max of R-side and Z-side member count)
  sizes <- vapply(all_groups, function(g) {
    sub <- edges[edges$id_group == g, , drop = FALSE]
    max(length(unique(sub$r)), length(unique(sub$z)))
  }, integer(1))
  ord <- order(sizes, decreasing = TRUE)
  all_groups <- all_groups[ord]
  
  if (length(all_groups) > cfg$max_groups) {
    warning(sprintf(
      "Found %d groups; keeping only the %d largest by node count (max_groups).",
      length(all_groups), cfg$max_groups))
    all_groups <- all_groups[seq_len(cfg$max_groups)]
  }
  
  n_written <- 0L
  n_failed  <- 0L
  
  for (grp in all_groups) {
    sub <- edges[edges$id_group == grp, , drop = FALSE]
    device_open <- FALSE
    
    ok <- tryCatch({
      layout <- .vis_group_layout(sub, cfg)
      dims   <- .vis_png_dimensions(layout, cfg)
      fname  <- sprintf("bipartite_group_%s_%dR_%dZ.png",
                        grp, layout$n_r, layout$n_z)
      fpath  <- file.path(out_dir, fname)
      
      .vis_open_png_device(fpath, dims$width, dims$height,
                           cfg$res, cfg$pointsize)
      device_open <- TRUE
      
      .vis_draw_group(sub, grp, layout, cfg)
      grDevices::dev.off()
      device_open <- FALSE
      
      if (verbose) {
        cat(sprintf("Wrote %s (%dx%d, %d R, %d Z)\n",
                    fname, dims$width, dims$height,
                    layout$n_r, layout$n_z))
      }
      TRUE
    }, error = function(e) {
      if (device_open && grDevices::dev.cur() != 1) {
        try(grDevices::dev.off(), silent = TRUE)
      }
      warning(sprintf("Failed to render group %s: %s",
                      grp, conditionMessage(e)))
      FALSE
    })
    
    if (isTRUE(ok)) n_written <- n_written + 1L else n_failed <- n_failed + 1L
  }
  
  list(
    n_groups  = length(all_groups),
    n_written = n_written,
    n_failed  = n_failed,
    out_dir   = out_dir
  )
}

# Notebook Radial-Sunburst Widget -----------------------------------------

#' Interactive Planetary-Boundaries Radial & Sunburst Widget for PADU/PADAN
#'
#' Builds a self-contained offline HTML/SVG+JS widget (`htmltools` + `jsonlite`)
#' for inspecting individual Planning Units (`id_pu`) in static Quarto HTML
#' notebooks. Includes a searchable `id_pu` selector, Prev/Next navigation,
#' PADU vs PADAN view toggle, Yellow-to-Blue shared radial gradient bars,
#' foreground `0-1` concentric axis labels, radial spoke dividers, striped
#' `NA` sectors, and `Lindung`/`Budidaya` inner sunburst arcs with dual-band
#' overlap over `PADU-KI`.
#'
#' @param idx_map An `sf` or `data.frame` containing `id_col` and one or more
#'   `idx_padu_*` indicator columns (plus optional `RTRW`, `RZWP3K`, `fungsi`,
#'   `idx_serasi`, `idx_padu_all`, `idx_padu_lindung`, `idx_padu_budidaya`,
#'   `idx_padu_final`, `idx_padan`).
#' @param padu_idx_weight Optional weight table (`data.frame` with code in col 1
#'   and numeric weight in col 2) matching `calculate_padu_index()`.
#' @param alpha Numeric scalar in `[0, 1]` balancing `idx_serasi` and
#'   `idx_padu_final` in PADAN mode. Default is `0.5`.
#' @param id_col Character. Name of the Planning Unit identifier column. Default `"id_pu"`.
#' @param title Character. Widget heading shown above controls.
#' @param indicators Optional character vector of indicator codes (e.g.
#'   `c("ke", "hs", "kh", "ki", "rtp", "se")`) to display and order explicitly.
#'
#' @return A `browsable` `htmltools::tagList` suitable for Quarto HTML rendering.
#' @export
plot_padu_radial_widget <- function(idx_map,
                                    padu_idx_weight = NULL,
                                    alpha = 0.5,
                                    id_col = "id_pu",
                                    title = "Visualisasi Radial–Sunburst Indeks PADU & PADAN",
                                    indicators = NULL) {
  if (is.null(idx_map) || (!inherits(idx_map, "sf") && !is.data.frame(idx_map))) {
    stop("idx_map must be an sf or data.frame object")
  }
  df <- if (inherits(idx_map, "sf")) sf::st_drop_geometry(idx_map) else as.data.frame(idx_map)
  if (nrow(df) == 0) {
    stop("idx_map must contain at least one row")
  }
  if (!is.character(id_col) || length(id_col) != 1 || !id_col %in% names(df)) {
    stop(sprintf("Column '%s' not found in idx_map", id_col))
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || !is.finite(alpha) ||
      alpha < 0 || alpha > 1) {
    stop("alpha must be a finite numeric scalar between 0 and 1")
  }

  idx_cols <- names(df)[grepl("^idx_padu_", names(df))]
  idx_cols <- setdiff(
    idx_cols,
    c("idx_padu_ke_abs", "idx_padu_all", "idx_padu_lindung", "idx_padu_budidaya", "idx_padu_final")
  )
  if (length(idx_cols) == 0) {
    stop("No valid 'idx_padu_*' indicator columns found in idx_map")
  }

  present_codes <- tolower(sub("^idx_padu_", "", idx_cols))
  canonical_order <- c("ke", "hs", "kl", "kh", "ki", "rtp", "se")
  if (!is.null(indicators)) {
    req_codes <- tolower(trimws(as.character(indicators)))
    active_codes <- intersect(req_codes, present_codes)
  } else {
    ordered_known <- intersect(canonical_order, present_codes)
    extra_codes <- setdiff(present_codes, canonical_order)
    active_codes <- c(ordered_known, extra_codes)
  }
  if (length(active_codes) == 0) {
    stop("No matching indicator columns found in idx_map")
  }

  meta_catalog <- list(
    ke  = list(label = "PADU-KE",  full = "Konektivitas Ekologi"),
    hs  = list(label = "PADU-HS",  full = "Hidrologi & Sedimentasi"),
    kl  = list(label = "PADU-KL",  full = "Kawasan Lindung"),
    kh  = list(label = "PADU-KH",  full = "Komposisi Habitat"),
    ki  = list(label = "PADU-KI",  full = "Ketahanan Iklim"),
    rtp = list(label = "PADU-RTp", full = "Risiko & Tekanan"),
    se  = list(label = "PADU-SE",  full = "Sosial & Ekonomi")
  )
  ind_meta <- lapply(active_codes, function(code) {
    entry <- meta_catalog[[code]]
    if (is.null(entry)) {
      list(key = code, label = paste0("PADU-", toupper(code)), full = toupper(code))
    } else {
      list(key = code, label = entry$label, full = entry$full)
    }
  })

  # Parse weights aligned with calculate_padu_index() per-pillar matching
  weights_df <- if (exists(".parse_padu_weights_df", mode = "function")) {
    .parse_padu_weights_df(padu_idx_weight)
  } else NULL
  raw_weights <- stats::setNames(rep(NA_real_, length(active_codes)), active_codes)
  if (!is.null(weights_df)) {
    raw_weights <- stats::setNames(
      weights_df$value[match(active_codes, weights_df$code)],
      active_codes
    )
  }

  safe_num <- function(x) {
    v <- suppressWarnings(as.numeric(x))
    if (length(v) != 1 || !is.finite(v)) NA_real_ else v
  }
  safe_chr <- function(x, default = "-") {
    v <- as.character(x)
    if (length(v) != 1 || is.na(v) || !nzchar(trimws(v))) default else trimws(v)
  }

  has_serasi <- "idx_serasi" %in% names(df) && any(is.finite(suppressWarnings(as.numeric(df$idx_serasi))))
  has_padan  <- has_serasi && ("idx_padan" %in% names(df) || "idx_padu_final" %in% names(df))

  col_map <- stats::setNames(idx_cols[match(active_codes, present_codes)], active_codes)
  w_list <- as.list(raw_weights)

  records <- lapply(seq_len(nrow(df)), function(i) {
    sc_vals <- lapply(active_codes, function(code) safe_num(df[[col_map[[code]]]][i]))
    names(sc_vals) <- active_codes
    s_val <- if ("idx_serasi" %in% names(df)) safe_num(df$idx_serasi[i]) else NA_real_

    list(
      id_pu = safe_chr(df[[id_col]][i], as.character(i)),
      RTRW = if ("RTRW" %in% names(df)) safe_chr(df$RTRW[i]) else "-",
      RZWP3K = if ("RZWP3K" %in% names(df)) safe_chr(df$RZWP3K[i]) else "-",
      fungsi = if ("fungsi" %in% names(df)) tolower(safe_chr(df$fungsi[i], "campuran")) else "campuran",
      idx_serasi = s_val,
      scores = sc_vals,
      weights = w_list
    )
  })

  payload_json <- jsonlite::toJSON(
    list(
      has_padan = isTRUE(has_padan),
      alpha = as.numeric(alpha),
      indicators = ind_meta,
      records = records
    ),
    auto_unbox = TRUE,
    na = "null"
  )

  uid <- sprintf("%s_%06d", format(Sys.time(), "%H%M%S"), sample.int(999999L, 1L))
  root_id <- paste0("laspur-radial-", uid)

  css_text <- sprintf(
    "#%1$s{font-family:system-ui,-apple-system,'Segoe UI',Roboto,sans-serif;font-size:14px;color:#0f172a;background:#fff;border:1px solid #e2e8f0;border-radius:12px;padding:18px;box-shadow:0 1px 3px rgba(15,23,42,.06);margin:16px 0;}
#%1$s .lp-hdr{display:flex;flex-wrap:wrap;justify-content:space-between;align-items:center;gap:10px;padding-bottom:12px;border-bottom:1px solid #e2e8f0;margin-bottom:14px;}
#%1$s .lp-ttl{margin:0;font-size:1rem;font-weight:700;}
#%1$s .lp-ctrls{display:flex;flex-wrap:wrap;align-items:center;gap:8px;}
#%1$s .lp-btn{padding:4px 9px;border:1px solid #cbd5e1;background:#fff;border-radius:6px;cursor:pointer;font-weight:600;color:#0f172a;}
#%1$s .lp-btn:hover{border-color:#0f766e;}
#%1$s select,#%1$s input{padding:4px 8px;border:1px solid #cbd5e1;border-radius:6px;background:#fff;color:#0f172a;font-size:.86rem;}
#%1$s .lp-tabs{display:inline-flex;border:1px solid #e2e8f0;border-radius:8px;padding:3px;background:#f1f5f9;}
#%1$s .lp-tab{border:0;background:transparent;padding:4px 11px;border-radius:6px;cursor:pointer;font-weight:600;color:#64748b;font-size:.84rem;}
#%1$s .lp-tab.active{background:#0f766e;color:#fff;}
#%1$s .lp-grid{display:grid;grid-template-columns:1.2fr .9fr;gap:18px;align-items:center;}
@media(max-width:760px){#%1$s .lp-grid{grid-template-columns:1fr;}}
#%1$s .lp-svg-wrap{display:flex;justify-content:center;position:relative;}
#%1$s svg{width:100%%;max-width:430px;height:auto;user-select:none;}
#%1$s .wedge-path{transition:opacity .15s ease;cursor:pointer;}
#%1$s .wedge-path:hover{filter:brightness(1.06);}
#%1$s .lp-panel{border:1px solid #e2e8f0;border-radius:10px;padding:12px;background:#f8fafc;margin-bottom:10px;}
#%1$s .lp-ptitle{display:flex;justify-content:space-between;align-items:center;font-weight:700;margin-bottom:4px;}
#%1$s .lp-badge{padding:2px 8px;border-radius:999px;font-size:.72rem;font-weight:700;text-transform:uppercase;}
#%1$s .lp-b-lindung{background:#d1fae5;color:#065f46;}
#%1$s .lp-b-budidaya{background:#fef3c7;color:#92400e;}
#%1$s .lp-b-campuran{background:#e0f2fe;color:#075985;}
#%1$s .lp-sgrid{display:grid;grid-template-columns:1fr 1fr;gap:5px 10px;margin-top:8px;padding-top:8px;border-top:1px solid #e2e8f0;font-variant-numeric:tabular-nums;font-size:.83rem;}
#%1$s .lp-muted{color:#64748b;}
#%1$s .lp-ramp{height:10px;flex:1;border-radius:4px;background:linear-gradient(90deg,#facc15 0%%,#7dd3fc 50%%,#1d4ed8 100%%);}
#%1$s .lp-lrow{display:flex;align-items:center;gap:8px;margin:7px 0;}
#%1$s .lp-lgrid{display:grid;grid-template-columns:1fr 1fr;gap:5px;font-size:.78rem;color:#64748b;}
#%1$s .lp-sw{display:inline-block;width:12px;height:10px;border-radius:2px;vertical-align:middle;margin-right:4px;}
#%1$s-tip{position:fixed;pointer-events:none;display:none;z-index:9999;background:#0f172a;color:#f8fafc;padding:7px 10px;border-radius:6px;font-size:.79rem;line-height:1.4;max-width:250px;box-shadow:0 6px 18px rgba(15,23,42,.22);}",
    root_id
  )

  js_template <- "(function(){
  var root = document.getElementById('__ROOT_ID__');
  if (!root) return;
  var payload = JSON.parse(root.getAttribute('data-laspur-payload'));
  var uid = '__UID__';
  var DATA = payload.records || [];
  var INDICATORS = payload.indicators || [];
  var ALPHA = Number(payload.alpha);
  var HAS_PADAN = Boolean(payload.has_padan);
  var MFL_KEYS = ['ke','hs','kl','kh','ki'];
  var MFB_KEYS = ['ki','rtp','se'];
  var state = { idx: 0, mode: 'padu' };

  var svg = root.querySelector('.lp-svg');
  var sel = root.querySelector('.lp-select');
  var searchEl = root.querySelector('.lp-search');
  var prevBtn = root.querySelector('.lp-prev');
  var nextBtn = root.querySelector('.lp-next');
  var btnPadu = root.querySelector('.lp-btn-padu');
  var btnPadan = root.querySelector('.lp-btn-padan');
  var metaTitle = root.querySelector('.lp-mtitle');
  var metaFungsi = root.querySelector('.lp-mfungsi');
  var metaZones = root.querySelector('.lp-mzones');
  var metaScores = root.querySelector('.lp-mscores');
  var tip = document.getElementById('__ROOT_ID__-tip');

  if (!HAS_PADAN && btnPadan) { btnPadan.style.display = 'none'; }

  function computeComposites(pu) {
    var calcPillar = function(keys) {
      var present = INDICATORS.map(function(d){ return d.key; }).filter(function(k){ return keys.indexOf(k) >= 0; });
      if (present.length === 0) return { score: null, effW: {} };
      var allWPresent = present.every(function(k){
        var wv = pu.weights[k];
        return wv !== null && wv !== undefined && !isNaN(wv);
      });
      var rawTotal = allWPresent ? present.reduce(function(a, k){ return a + Number(pu.weights[k]); }, 0) : 0;
      var baseW = {};
      present.forEach(function(k){
        baseW[k] = (allWPresent && rawTotal > 0) ? Number(pu.weights[k]) : (1 / present.length);
      });

      var valid = present.filter(function(k){ return pu.scores[k] !== null && pu.scores[k] !== undefined && !isNaN(pu.scores[k]); });
      if (valid.length === 0) return { score: null, effW: {} };
      var wSum = valid.reduce(function(a, k){ return a + baseW[k]; }, 0);
      var effW = {};
      if (wSum <= 0) valid.forEach(function(k){ effW[k] = 1 / valid.length; });
      else valid.forEach(function(k){ effW[k] = baseW[k] / wSum; });
      var score = valid.reduce(function(a, k){ return a + effW[k] * Number(pu.scores[k]); }, 0);
      return { score: score, effW: effW };
    };
    var allKeys = INDICATORS.map(function(d){ return d.key; });
    var all = calcPillar(allKeys);
    var mfl = calcPillar(MFL_KEYS);
    var mfb = calcPillar(MFB_KEYS);

    var activeRes = all, activeLabel = 'Campuran (All)', activeKeys = allKeys;
    if (pu.fungsi === 'lindung' && mfl.score !== null) {
      activeRes = mfl; activeLabel = 'Lindung'; activeKeys = allKeys.filter(function(k){ return MFL_KEYS.indexOf(k) >= 0; });
    } else if (pu.fungsi === 'budidaya' && mfb.score !== null) {
      activeRes = mfb; activeLabel = 'Budidaya'; activeKeys = allKeys.filter(function(k){ return MFB_KEYS.indexOf(k) >= 0; });
    } else if (pu.fungsi !== 'campuran' && all.score !== null) {
      activeLabel = 'Fallback (All)';
    }

    var fin = activeRes.score;
    var padanV = (fin !== null && pu.idx_serasi !== null && pu.idx_serasi !== undefined && !isNaN(pu.idx_serasi))
      ? (ALPHA * Number(pu.idx_serasi) + (1 - ALPHA) * fin) : null;

    return {
      all: all.score, lindung: mfl.score, budidaya: mfb.score, idx_padu_final: fin, idx_padan: padanV,
      effW: activeRes.effW, activeLabel: activeLabel, activeKeys: activeKeys
    };
  }

  function scoreColor(val) {
    if (val === null || val === undefined || isNaN(val)) return '#94a3b8';
    var t = Math.max(0, Math.min(1, Number(val)));
    var stops = [[250, 204, 21], [125, 211, 252], [29, 78, 216]];
    var c0 = t <= 0.5 ? stops[0] : stops[1];
    var c1 = t <= 0.5 ? stops[1] : stops[2];
    var u  = t <= 0.5 ? (t / 0.5) : ((t - 0.5) / 0.5);
    return 'rgb(' + Math.round(c0[0] + (c1[0] - c0[0]) * u) + ',' + Math.round(c0[1] + (c1[1] - c0[1]) * u) + ',' + Math.round(c0[2] + (c1[2] - c0[2]) * u) + ')';
  }

  function polarToCartesian(cx, cy, r, angleDeg) {
    var rad = (angleDeg - 90) * Math.PI / 180;
    return [cx + r * Math.cos(rad), cy + r * Math.sin(rad)];
  }

  function sectorPath(cx, cy, r0, r1, a0, a1) {
    if ((a1 - a0) >= 359.99) a1 = a0 + 359.99;
    var large = (a1 - a0) > 180 ? 1 : 0;
    var p0 = polarToCartesian(cx, cy, r1, a0), p1 = polarToCartesian(cx, cy, r1, a1);
    var p2 = polarToCartesian(cx, cy, r0, a1), p3 = polarToCartesian(cx, cy, r0, a0);
    return 'M ' + p0[0] + ' ' + p0[1] + ' A ' + r1 + ' ' + r1 + ' 0 ' + large + ' 1 ' + p1[0] + ' ' + p1[1] +
           ' L ' + p2[0] + ' ' + p2[1] + ' A ' + r0 + ' ' + r0 + ' 0 ' + large + ' 0 ' + p3[0] + ' ' + p3[1] + ' Z';
  }

  function el(tag, attrs, text) {
    var node = document.createElementNS('http://www.w3.org/2000/svg', tag);
    if (attrs) Object.keys(attrs).forEach(function(k){ node.setAttribute(k, attrs[k]); });
    if (text !== undefined && text !== null) node.textContent = text;
    return node;
  }

  function arcTextPath(cx, cy, r, a0, a1, reverse) {
    var large = Math.abs(a1 - a0) > 180 ? 1 : 0;
    var s = polarToCartesian(cx, cy, r, reverse ? a1 : a0);
    var e = polarToCartesian(cx, cy, r, reverse ? a0 : a1);
    return 'M ' + s[0] + ' ' + s[1] + ' A ' + r + ' ' + r + ' 0 ' + large + ' ' + (reverse ? 0 : 1) + ' ' + e[0] + ' ' + e[1];
  }

  function addCurvedLabel(defs, id, cx, cy, r, a0, a1, label) {
    var mid = (a0 + a1) / 2;
    var reverse = (mid > 90 && mid < 270);
    defs.appendChild(el('path', { id: id, d: arcTextPath(cx, cy, r, a0, a1, reverse) }));
    var txt = el('text', { 'font-size':'8.5','font-weight':'700',fill:'#ffffff','dominant-baseline':'central','letter-spacing':'0.4','pointer-events':'none' });
    txt.appendChild(el('textPath', { href:'#' + id, startOffset:'50%', 'text-anchor':'middle' }, label));
    svg.appendChild(txt);
  }

  function attachTip(node, html) {
    node.addEventListener('mouseenter', function(){ tip.innerHTML = html; tip.style.display = 'block'; });
    node.addEventListener('mousemove', function(e){
      tip.style.left = Math.min(window.innerWidth - 260, e.clientX + 12) + 'px';
      tip.style.top  = Math.min(window.innerHeight - 100, e.clientY + 12) + 'px';
    });
    node.addEventListener('mouseleave', function(){ tip.style.display = 'none'; });
  }

  function fmt(v) { return (v === null || v === undefined || isNaN(v)) ? 'NA' : Number(v).toFixed(2); }

  function buildDefs(cx, cy, rPad, rMax) {
    var defs = el('defs');
    var grad = el('radialGradient', { id: 'pbRadialGrad-' + uid, gradientUnits: 'userSpaceOnUse', cx: cx, cy: cy, r: rMax });
    var p0 = (rPad / rMax) * 100, span = 100 - p0;
    [[0.00,'#facc15'],[0.50,'#7dd3fc'],[1.00,'#1d4ed8']].forEach(function(st){
      grad.appendChild(el('stop', { offset: (p0 + st[0] * span).toFixed(2) + '%', 'stop-color': st[1] }));
    });
    defs.appendChild(grad);

    var pat = el('pattern', { id: 'naStripePattern-' + uid, width: '6', height: '6', patternTransform: 'rotate(45 0 0)', patternUnits: 'userSpaceOnUse' });
    pat.appendChild(el('rect', { width: '6', height: '6', fill: 'rgba(148,163,184,0.10)' }));
    pat.appendChild(el('line', { x1:'0',y1:'0',x2:'0',y2:'6',stroke:'#94a3b8','stroke-width':'2','stroke-opacity':'0.55' }));
    defs.appendChild(pat);
    svg.appendChild(defs);
    return defs;
  }

  function drawSpoke(cx, cy, r0, r1, angleDeg) {
    var p0 = polarToCartesian(cx, cy, r0, angleDeg), p1 = polarToCartesian(cx, cy, r1, angleDeg);
    svg.appendChild(el('line', { x1:p0[0],y1:p0[1],x2:p1[0],y2:p1[1],stroke:'#475569','stroke-opacity':'0.35','stroke-width':'1' }));
  }

  function drawOuterWedge(cx, cy, rPad, rMax, a0, a1, val, isActive, tipHtml) {
    var isNA = val === null || val === undefined || isNaN(val);
    var rOut = isNA ? rMax : (rPad + Math.max(0.06, Number(val)) * (rMax - rPad));
    var wAttrs = {
      d: sectorPath(cx, cy, rPad, rOut, a0, a1),
      fill: isNA ? ('url(#naStripePattern-' + uid + ')') : ('url(#pbRadialGrad-' + uid + ')'),
      class: 'wedge-path'
    };
    if (isNA) {
      wAttrs.stroke = '#64748b'; wAttrs['stroke-dasharray'] = '4 3'; wAttrs['stroke-width'] = '1.1';
      wAttrs.opacity = isActive ? '0.95' : '0.5';
    } else if (!isActive) {
      wAttrs.opacity = '0.30'; wAttrs.stroke = '#64748b'; wAttrs['stroke-dasharray'] = '2 2';
    } else {
      wAttrs.opacity = '0.95'; wAttrs.stroke = '#ffffff'; wAttrs['stroke-width'] = '0.9';
    }
    var path = el('path', wAttrs);
    attachTip(path, tipHtml);
    svg.appendChild(path);
    return isNA;
  }

  function render() {
    if (DATA.length === 0) return;
    var pu = DATA[state.idx];
    var comp = computeComposites(pu);
    svg.innerHTML = '';

    var cx = 180, cy = 170, rCenter = 41, rSun0 = 45, rSun1 = 69, rPad = 74, rMax = 142;
    var defs = buildDefs(cx, cy, rPad, rMax);

    if (state.mode === 'padu') {
      var n = INDICATORS.length, step = 360 / n, gap = 2.6;
      var mflIdx = [], mfbIdx = [], bothIdx = [];
      INDICATORS.forEach(function(d, idx){
        var inMfl = MFL_KEYS.indexOf(d.key) >= 0;
        var inMfb = MFB_KEYS.indexOf(d.key) >= 0;
        if (inMfl && !inMfb) mflIdx.push(idx);
        else if (!inMfl && inMfb) mfbIdx.push(idx);
        else if (inMfl && inMfb) bothIdx.push(idx);
      });

      var mflOp = (pu.fungsi === 'budidaya' && comp.budidaya !== null) ? '0.38' : '0.92';
      var mfbOp = (pu.fungsi === 'lindung'  && comp.lindung  !== null) ? '0.38' : '0.92';

      if (mflIdx.length > 0) {
        var a0L = mflIdx[0] * step + gap / 2, a1L = (mflIdx[mflIdx.length - 1] + 1) * step - gap / 2;
        var nL = el('path', { d: sectorPath(cx, cy, rSun0, rSun1, a0L, a1L), fill: '#2e7d6b', opacity: mflOp, class: 'wedge-path' });
        attachTip(nL, '<b>Lindung</b><br/>Skor: ' + fmt(comp.lindung));
        svg.appendChild(nL);
        addCurvedLabel(defs, 'lbl-lin-' + uid, cx, cy, (rSun0 + rSun1) / 2, a0L, a1L, 'Lindung');
      }
      if (mfbIdx.length > 0) {
        var a0B = mfbIdx[0] * step + gap / 2, a1B = (mfbIdx[mfbIdx.length - 1] + 1) * step - gap / 2;
        var nB = el('path', { d: sectorPath(cx, cy, rSun0, rSun1, a0B, a1B), fill: '#d97706', opacity: mfbOp, class: 'wedge-path' });
        attachTip(nB, '<b>Budidaya</b><br/>Skor: ' + fmt(comp.budidaya));
        svg.appendChild(nB);
        addCurvedLabel(defs, 'lbl-bud-' + uid, cx, cy, (rSun0 + rSun1) / 2, a0B, a1B, 'Budidaya');
      }
      bothIdx.forEach(function(bi){
        var a0K = bi * step + gap / 2, a1K = (bi + 1) * step - gap / 2;
        var nK1 = el('path', { d: sectorPath(cx, cy, (rSun0 + rSun1) / 2 + 0.5, rSun1, a0K, a1K), fill: '#2e7d6b', opacity: mflOp, class: 'wedge-path' });
        var nK2 = el('path', { d: sectorPath(cx, cy, rSun0, (rSun0 + rSun1) / 2 - 0.5, a0K, a1K), fill: '#d97706', opacity: mfbOp, class: 'wedge-path' });
        var kTip = '<b>Irisan Lindung &amp; Budidaya (' + INDICATORS[bi].label + ')</b><br/>Lindung: ' + fmt(comp.lindung) + ' | Budidaya: ' + fmt(comp.budidaya);
        attachTip(nK1, kTip); attachTip(nK2, kTip);
        svg.appendChild(nK1); svg.appendChild(nK2);
      });

      INDICATORS.forEach(function(ind, i){
        drawSpoke(cx, cy, rSun0, rMax + 4, i * step);
        var a0 = i * step + gap / 2, a1 = (i + 1) * step - gap / 2;
        var val = pu.scores[ind.key];
        var isActive = comp.activeKeys.indexOf(ind.key) >= 0;
        var wPct = comp.effW[ind.key] ? ((comp.effW[ind.key] * 100).toFixed(1) + '%') : '0% (non-aktif/NA)';
        var isNA = val === null || val === undefined || isNaN(val);
        var stStr = isNA ? 'NA (dikecualikan)' : (isActive ? 'Aktif dalam bobot' : 'Tidak masuk bobot aktif');
        drawOuterWedge(cx, cy, rPad, rMax, a0, a1, val, isActive,
          '<b>' + ind.label + '</b> — ' + ind.full + '<br/>Nilai: <b>' + fmt(val) + '</b><br/>Bobot efektif: ' + wPct + '<br/>Status: ' + stStr);

        var tPos = polarToCartesian(cx, cy, rMax + 15, (a0 + a1) / 2);
        svg.appendChild(el('text', {
          x: tPos[0], y: tPos[1] + 3, 'font-size':'9.5', 'font-weight': isActive ? '700' : '500',
          fill:'#0f172a', opacity: isActive ? '0.95' : '0.5', 'text-anchor':'middle'
        }, ind.label.replace('PADU-', '') + (isNA ? ' (NA)' : '')));
      });
    } else {
      var serasiDeg = ALPHA * 360, paduDeg = 360 - serasiDeg, gapP = (ALPHA > 0 && ALPHA < 1) ? 3 : 0.4;
      if (ALPHA > 0 && ALPHA < 1) {
        drawSpoke(cx, cy, rSun0, rMax + 4, 0);
        drawSpoke(cx, cy, rSun0, rMax + 4, serasiDeg);
      }

      if (serasiDeg > 1) {
        var a0S = gapP / 2, a1S = serasiDeg - gapP / 2;
        var sArc = el('path', { d: sectorPath(cx, cy, rSun0, rSun1, a0S, a1S), fill: '#0f766e', opacity: '0.92', class: 'wedge-path' });
        attachTip(sArc, '<b>SERASI (&alpha; = ' + (ALPHA * 100).toFixed(0) + '%)</b><br/>idx_serasi: <b>' + fmt(pu.idx_serasi) + '</b>');
        svg.appendChild(sArc);
        if (serasiDeg >= 25) addCurvedLabel(defs, 'lbl-ser-' + uid, cx, cy, (rSun0 + rSun1) / 2, a0S, a1S, 'SERASI');

        var serVal = pu.idx_serasi;
        drawOuterWedge(cx, cy, rPad, rMax, a0S, a1S, serVal, true,
          '<b>Indeks SERASI</b><br/>' + pu.RTRW + ' &times; ' + pu.RZWP3K + '<br/>Nilai: <b>' + fmt(serVal) + '</b> (&alpha; = ' + (ALPHA * 100).toFixed(0) + '%)');
        var stPos = polarToCartesian(cx, cy, rMax + 15, serasiDeg / 2);
        svg.appendChild(el('text', { x: stPos[0], y: stPos[1] + 3, 'font-size':'9.5', 'font-weight':'700', fill:'#0f172a', 'text-anchor':'middle' }, 'SERASI (' + fmt(serVal) + ')'));
      }

      if (paduDeg > 1) {
        var a0P = serasiDeg + gapP / 2, a1P = 360 - gapP / 2;
        var pArc = el('path', { d: sectorPath(cx, cy, rSun0, rSun1, a0P, a1P), fill: '#1d4ed8', opacity: '0.92', class: 'wedge-path' });
        attachTip(pArc, '<b>PADU (1&minus;&alpha; = ' + ((1 - ALPHA) * 100).toFixed(0) + '%)</b><br/>idx_padu_final: <b>' + fmt(comp.idx_padu_final) + '</b><br/>Pilar: ' + comp.activeLabel);
        svg.appendChild(pArc);
        if (paduDeg >= 25) addCurvedLabel(defs, 'lbl-pad-' + uid, cx, cy, (rSun0 + rSun1) / 2, a0P, a1P, 'PADU');

        var contribInds = INDICATORS.filter(function(d){
          return comp.activeKeys.indexOf(d.key) >= 0 && (comp.effW[d.key] || 0) > 0;
        });
        var availDeg = paduDeg - gapP;
        if (contribInds.length === 0) {
          drawOuterWedge(cx, cy, rPad, rMax, a0P, a1P, null, true, '<b>PADU</b><br/>Seluruh indikator aktif bernilai NA');
          var naPos = polarToCartesian(cx, cy, rMax + 15, serasiDeg + paduDeg / 2);
          svg.appendChild(el('text', { x: naPos[0], y: naPos[1] + 3, 'font-size':'9.5', 'font-weight':'700', fill:'#0f172a', 'text-anchor':'middle' }, 'PADU (NA)'));
        } else {
          var cursor = a0P;
          var totalShare = contribInds.reduce(function(acc, d){ return acc + comp.effW[d.key]; }, 0);
          contribInds.forEach(function(ind, idx){
            if (idx > 0) drawSpoke(cx, cy, rPad, rMax + 4, cursor);
            var span = availDeg * (comp.effW[ind.key] / totalShare);
            var pad = Math.min(0.9, span * 0.15);
            var a0 = cursor + pad, a1 = cursor + span - pad;
            cursor += span;

            var val = pu.scores[ind.key];
            var pShare = ((1 - ALPHA) * comp.effW[ind.key] * 100).toFixed(1) + '%';
            drawOuterWedge(cx, cy, rPad, rMax, a0, a1, val, true,
              '<b>' + ind.label + '</b> — ' + ind.full + '<br/>Nilai: <b>' + fmt(val) + '</b><br/>Kontribusi ke PADAN: ' + pShare);
            var tPos = polarToCartesian(cx, cy, rMax + 15, (a0 + a1) / 2);
            svg.appendChild(el('text', { x: tPos[0], y: tPos[1] + 3, 'font-size':'9', 'font-weight':'700', fill:'#0f172a', 'text-anchor':'middle' }, ind.label.replace('PADU-', '')));
          });
        }
      }
    }

    // Foreground 0-1 concentric rings and axis labels
    [0, 0.25, 0.5, 0.75, 1.0].forEach(function(lvl){
      var rr = rPad + lvl * (rMax - rPad);
      if (lvl > 0) {
        svg.appendChild(el('circle', {
          cx: cx, cy: cy, r: rr, fill: 'none', 'pointer-events': 'none', stroke: '#475569',
          'stroke-opacity': lvl === 1.0 ? '0.4' : '0.25',
          'stroke-dasharray': lvl === 1.0 ? 'none' : '3 3', 'stroke-width': '1'
        }));
      }
      var lPos = polarToCartesian(cx, cy, rr, 12);
      svg.appendChild(el('text', {
        x: lPos[0], y: lPos[1] + 3, 'font-size':'8.5', 'font-weight':'700',
        fill:'#0f172a', stroke:'#ffffff', 'stroke-width':'2.4', 'paint-order':'stroke',
        'stroke-opacity':'0.9', 'text-anchor':'middle', 'pointer-events':'none'
      }, lvl === 0 ? '0' : (lvl === 1 ? '1' : lvl.toFixed(2))));
    });

    var mainScore = state.mode === 'padu' ? comp.idx_padu_final : comp.idx_padan;
    svg.appendChild(el('circle', { cx: cx, cy: cy, r: rCenter, fill: '#ffffff', stroke: scoreColor(mainScore), 'stroke-width': '3.5' }));
    svg.appendChild(el('text', { x: cx, y: cy - 13, 'font-size':'8.5', fill:'#64748b', 'text-anchor':'middle', 'font-weight':'700' }, state.mode === 'padu' ? 'PADU' : 'PADAN'));
    svg.appendChild(el('text', { x: cx, y: cy + 4,  'font-size':'16.5', fill:'#0f172a', 'text-anchor':'middle', 'font-weight':'800' }, fmt(mainScore)));
    svg.appendChild(el('text', { x: cx, y: cy + 16, 'font-size':'7.5', fill:'#475569', 'text-anchor':'middle', 'font-weight':'700' }, 'id_pu ' + pu.id_pu));
    svg.appendChild(el('text', { x: cx, y: cy + 26, 'font-size':'7.5', fill:'#64748b', 'text-anchor':'middle' }, String(pu.fungsi || '').toUpperCase()));

    metaTitle.textContent = 'Planning Unit #' + pu.id_pu;
    metaFungsi.textContent = pu.fungsi;
    var fCls = (pu.fungsi === 'lindung' || pu.fungsi === 'budidaya' || pu.fungsi === 'campuran') ? pu.fungsi : 'campuran';
    metaFungsi.className = 'lp-badge lp-b-' + fCls;
    metaZones.textContent = pu.RTRW + ' ' + String.fromCharCode(215) + ' ' + pu.RZWP3K;
    metaScores.innerHTML =
      '<div><span class=\"lp-muted\">PADU Lindung:</span> <b>' + fmt(comp.lindung) + '</b></div>' +
      '<div><span class=\"lp-muted\">PADU Budidaya:</span> <b>' + fmt(comp.budidaya) + '</b></div>' +
      '<div><span class=\"lp-muted\">PADU All:</span> <b>' + fmt(comp.all) + '</b></div>' +
      '<div><span class=\"lp-muted\">PADU Final:</span> <b>' + fmt(comp.idx_padu_final) + '</b></div>' +
      '<div><span class=\"lp-muted\">SERASI:</span> <b>' + fmt(pu.idx_serasi) + '</b></div>' +
      '<div><span class=\"lp-muted\">PADAN:</span> <b>' + fmt(comp.idx_padan) + '</b></div>';

    btnPadu.classList.toggle('active', state.mode === 'padu');
    if (btnPadan) btnPadan.classList.toggle('active', state.mode === 'padan');
  }

  function populateSelect(filterText) {
    var q = String(filterText || '').trim().toLowerCase();
    sel.innerHTML = '';
    DATA.forEach(function(d, i){
      if (q && String(d.id_pu).toLowerCase().indexOf(q) < 0 && String(d.fungsi).toLowerCase().indexOf(q) < 0) return;
      var opt = document.createElement('option');
      opt.value = i; opt.textContent = 'id_pu ' + d.id_pu + ' (' + d.fungsi + ')';
      sel.appendChild(opt);
    });
    if (sel.options.length > 0) {
      var found = false;
      for (var k = 0; k < sel.options.length; k++) {
        if (Number(sel.options[k].value) === state.idx) { found = true; break; }
      }
      if (!found) state.idx = Number(sel.options[0].value);
      sel.value = state.idx;
    }
    render();
  }

  if (searchEl) searchEl.addEventListener('input', function(e){ populateSelect(e.target.value); });
  sel.addEventListener('change', function(e){ state.idx = Number(e.target.value); render(); });
  prevBtn.addEventListener('click', function(){
    if (DATA.length === 0) return;
    state.idx = (state.idx - 1 + DATA.length) % DATA.length; sel.value = state.idx; render();
  });
  nextBtn.addEventListener('click', function(){
    if (DATA.length === 0) return;
    state.idx = (state.idx + 1) % DATA.length; sel.value = state.idx; render();
  });
  btnPadu.addEventListener('click', function(){ state.mode = 'padu'; render(); });
  if (btnPadan) btnPadan.addEventListener('click', function(){ if (HAS_PADAN) { state.mode = 'padan'; render(); } });

  populateSelect('');
})();"
  js_text <- gsub("__ROOT_ID__", root_id, js_template, fixed = TRUE)
  js_text <- gsub("__UID__", uid, js_text, fixed = TRUE)

  widget <- htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(css_text)),
    htmltools::tags$div(
      id = root_id,
      `data-laspur-payload` = payload_json,
      htmltools::tags$div(
        class = "lp-hdr",
        htmltools::tags$div(
          htmltools::tags$h4(class = "lp-ttl", title),
          htmltools::tags$div(
            class = "lp-muted", style = "font-size:.82rem;",
            htmltools::HTML("Planetary Boundaries (Kuning 0 &rarr; Biru 1, garis arsir = NA)")
          )
        ),
        htmltools::tags$div(
          class = "lp-ctrls",
          htmltools::tags$input(type = "text", class = "lp-search", placeholder = "Cari id_pu...", style = "width:105px;"),
          htmltools::tags$button(type = "button", class = "lp-btn lp-prev", title = "id_pu sebelumnya", htmltools::HTML("&#9664;")),
          htmltools::tags$select(class = "lp-select"),
          htmltools::tags$button(type = "button", class = "lp-btn lp-next", title = "id_pu berikutnya", htmltools::HTML("&#9654;")),
          htmltools::tags$div(
            class = "lp-tabs",
            htmltools::tags$button(type = "button", class = "lp-tab lp-btn-padu active", "PADU"),
            htmltools::tags$button(
              type = "button",
              class = "lp-tab lp-btn-padan",
              paste0("PADAN (", intToUtf8(0x03b1), "=", format(alpha, trim = TRUE), ")")
            )
          )
        )
      ),
      htmltools::tags$div(
        class = "lp-grid",
        htmltools::tags$div(
          class = "lp-svg-wrap",
          htmltools::tags$svg(class = "lp-svg", viewBox = "0 0 360 340")
        ),
        htmltools::tags$div(
          htmltools::tags$div(
            class = "lp-panel",
            htmltools::tags$div(
              class = "lp-ptitle",
              htmltools::tags$span(class = "lp-mtitle", "Planning Unit"),
              htmltools::tags$span(class = "lp-badge lp-mfungsi", "-")
            ),
            htmltools::tags$div(class = "lp-muted lp-mzones", style = "font-size:.83rem;"),
            htmltools::tags$div(class = "lp-sgrid lp-mscores")
          ),
          htmltools::tags$div(
            class = "lp-panel",
            htmltools::tags$div(style = "font-weight:700;margin-bottom:4px;", "Gradien Radial & Simbol"),
            htmltools::tags$div(
              class = "lp-lrow",
              htmltools::tags$span(class = "lp-muted", "0.0"),
              htmltools::tags$div(class = "lp-ramp"),
              htmltools::tags$span(class = "lp-muted", "1.0")
            ),
            htmltools::tags$div(
              class = "lp-lgrid",
              htmltools::tags$div(htmltools::tags$span(class = "lp-sw", style = "background:#2e7d6b;"), "Lindung"),
              htmltools::tags$div(htmltools::tags$span(class = "lp-sw", style = "background:#d97706;"), "Budidaya"),
              htmltools::tags$div(
                htmltools::tags$span(
                  class = "lp-sw",
                  style = "border:1px dashed #64748b;background:repeating-linear-gradient(45deg,rgba(148,163,184,0.45) 0 2px,transparent 2px 4px);"
                ),
                "NA (Dikecualikan)"
              ),
              htmltools::tags$div(
                htmltools::tags$span(
                  class = "lp-sw",
                  style = "background:linear-gradient(90deg,#facc15,#1d4ed8);opacity:0.4;border:1px dashed #64748b;"
                ),
                "Non-aktif (fungsi)"
              )
            )
          )
        )
      )
    ),
    htmltools::tags$div(id = paste0(root_id, "-tip")),
    htmltools::tags$script(htmltools::HTML(js_text))
  )

  htmltools::browsable(widget)
}