
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