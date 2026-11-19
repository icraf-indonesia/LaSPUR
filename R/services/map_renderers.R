# R/services/map_renderers.R
# ============================================================
# Map Renderers: Publication & Diagnostic Plotting Services
# ============================================================

#' Reproject SpatRaster or sf object to Web Mercator (EPSG:3857)
#' @keywords internal
.reproject_to_3857 <- function(map) {
  target <- "EPSG:3857"
  if (inherits(map, "SpatRaster")) {
    cur <- terra::crs(map)
    if (is.na(cur) || is.null(cur) || identical(cur, "")) return(map)
    if (!identical(terra::crs(map, proj = TRUE),
                   terra::crs(target, proj = TRUE))) {
      map <- terra::project(map, target)
    }
  } else if (inherits(map, "sf")) {
    cur <- sf::st_crs(map)
    if (is.na(cur)) return(map)
    if (cur != sf::st_crs(target)) {
      map <- sf::st_transform(map, target)
    }
  }
  map
}

#' Fetch Esri.WorldGrayCanvas tiles and return them as a ggplot layer
#' @keywords internal
.get_esri_gray_basemap <- function(map) {
  if (!requireNamespace("maptiles", quietly = TRUE)) {
    warning("Package 'maptiles' is required to fetch the Esri.WorldGrayCanvas ",
            "basemap. Install with install.packages('maptiles'). ",
            "Proceeding without basemap.", call. = FALSE)
    return(NULL)
  }
  tryCatch({
    tiles <- maptiles::get_tiles(
      x        = map,
      provider = "Esri.WorldGrayCanvas",
      crop     = TRUE,
      project  = TRUE  
    )
    tidyterra::geom_spatraster_rgb(data = tiles)
  }, error = function(e) {
    warning("Failed to fetch Esri.WorldGrayCanvas basemap: ",
            conditionMessage(e),
            ". Proceeding without basemap.", call. = FALSE)
    NULL
  })
}

#' Plot Continuous Raster or Vector (sf) Map with Optional PNG Export
#'
#' @param map A `SpatRaster` or `sf` object.
#' @param title Optional character string for plot title.
#' @param column Character string column name (required if `sf`).
#' @param legend Character string legend title.
#' @param low Character string low-end gradient color.
#' @param high Character string high-end gradient color.
#' @param na_color Character string color for NA values (default "white").
#' @param filepath Optional character path to save PNG.
#' @param width Width in inches.
#' @param height Height in inches.
#' @param dpi Resolution in dots per inch.
#' @return A `ggplot` object.
#' @export
plot_continuous_map <- function(map, title = NULL, column = NULL, legend, low, high,
                                na_color = "white", filepath = NULL,
                                width = 7, height = 5, dpi = 300) {
  
  build_plot <- function(map_data, basemap_layer = NULL) {
    
    if (inherits(map_data, "SpatRaster")) {
      
      p <- ggplot2::ggplot()
      if (!is.null(basemap_layer)) p <- p + basemap_layer
      p <- p +
        tidyterra::geom_spatraster(data = map_data) +
        ggplot2::scale_fill_gradient(
          low = low,
          high = high,
          na.value = na_color,
          name = if (!is.null(legend)) legend else NULL
        )
      
    } else if (inherits(map_data, "sf")) {
      
      if (is.null(column)) {
        stop("`column` must be specified when `map` is an sf object.")
      }
      if (!column %in% names(map_data)) {
        stop(sprintf("Column '%s' not found in `map`.", column))
      }
      
      map_data <- sf::st_zm(map_data, drop = TRUE, what = "ZM")
      map_data <- sf::st_make_valid(map_data)
      map_data <- map_data[!sf::st_is_empty(map_data), ]
      map_data[[column]] <- as.numeric(map_data[[column]])
      map_data[[column]][!is.finite(map_data[[column]])] <- NA
      
      geom_types <- unique(as.character(sf::st_geometry_type(map_data)))
      is_polygon <- any(grepl("POLYGON", geom_types))
      
      p <- ggplot2::ggplot()
      if (!is.null(basemap_layer)) p <- p + basemap_layer
      
      if (is_polygon) {
        p <- p +
          ggplot2::geom_sf(data = map_data, ggplot2::aes(fill = .data[[column]]), color = NA) +
          ggplot2::scale_fill_gradient(
            low = low,
            high = high,
            na.value = na_color,
            name = if (!is.null(legend)) legend else NULL
          )
      } else {
        p <- p +
          ggplot2::geom_sf(data = map_data, ggplot2::aes(color = .data[[column]])) +
          ggplot2::scale_color_gradient(
            low = low,
            high = high,
            na.value = na_color,
            name = if (!is.null(legend)) legend else NULL
          )
      }
      
    } else {
      stop("`map` must be a SpatRaster or an sf object.")
    }
    
    p +
      ggplot2::theme_bw() +
      ggplot2::labs(fill = NULL, title = title) +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::guides(
        fill = ggplot2::guide_colorbar(
          title.position = "top",
          direction = "vertical",
          barwidth = grid::unit(0.4, "cm"),
          barheight = grid::unit(0.8 * height, "in")
        ),
        color = ggplot2::guide_colorbar(
          title.position = "top",
          direction = "vertical",
          barwidth = grid::unit(0.4, "cm"),
          barheight = grid::unit(0.8 * height, "in")
        )
      ) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 8),
        axis.text.y = ggplot2::element_text(size = 8),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0),
        legend.title = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 10),
        legend.position = "right",
        legend.justification = c(0, 0.5),
        legend.box.spacing = grid::unit(0.5, "cm"),
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        plot.margin = ggplot2::margin(t = 5, r = 5, b = 2, l = 2)
      )
  }
  
  plot_lc <- build_plot(map)
  
  if (!is.null(filepath)) {
    map_3857 <- .reproject_to_3857(map)
    bm <- .get_esri_gray_basemap(map_3857)
    export_plot <- if (!is.null(bm)) build_plot(map_3857, bm) else plot_lc
    ggplot2::ggsave(filename = filepath, plot = export_plot,
                    width = width, height = height, dpi = dpi)
  }
  
  return(plot_lc)
}

#' Plot Categorical Raster or Vector (sf) Map with Optional PNG Export
#'
#' @param map A `SpatRaster` or `sf` object.
#' @param title Optional title string.
#' @param column Character string column name (if `sf`).
#' @param lookup Path to lookup file (if `SpatRaster`).
#' @param id_col Lookup ID column name (default "ID").
#' @param class_col Lookup class column name (default "class").
#' @param legend Legend title.
#' @param colors Vector of colors.
#' @param na_color Color for NA values.
#' @param label_wrap_width Width to wrap labels.
#' @param legend_ncol Number of columns in legend.
#' @param filepath Optional filepath to save PNG.
#' @param width Width in inches.
#' @param height Height in inches.
#' @param dpi DPI for rasterization.
#' @return A `ggplot` object.
#' @export
plot_categorical_map <- function(map, title = NULL, column = NULL, lookup = NULL,
                                 id_col = "ID", class_col = "class", legend = NULL,
                                 colors = NULL, na_color = "white",
                                 label_wrap_width = 30, legend_ncol = 1,
                                 filepath = NULL, width = 7, height = 5, dpi = 300) {
  
  read_lookup <- function(path) {
    ext <- tolower(tools::file_ext(path))
    if (ext == "csv") {
      utils::read.csv(path, stringsAsFactors = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package 'readxl' is required to read xlsx/xls lookup files.")
      }
      readxl::read_excel(path)
    } else {
      stop("`lookup` must be a .csv, .xlsx, or .xls file.")
    }
  }
  
  build_plot <- function(map_data, basemap_layer = NULL) {
    
    if (inherits(map_data, "SpatRaster")) {
      
      if (is.null(lookup)) {
        stop("`lookup` (a .csv/.xlsx file with ID and class columns) is required for SpatRaster input.")
      }
      
      lookup_df <- read_lookup(lookup)
      if (!all(c(id_col, class_col) %in% names(lookup_df))) {
        stop(sprintf("`lookup` must contain columns '%s' and '%s'.", id_col, class_col))
      }
      
      cat_df <- data.frame(
        id    = lookup_df[[id_col]],
        class = as.character(lookup_df[[class_col]])
      )
      terra::levels(map_data) <- cat_df
      
      class_labels <- as.character(terra::levels(map_data)[[1]][[2]])
      
      if (is.null(colors)) {
        pal <- stats::setNames(scales::hue_pal()(length(class_labels)), class_labels)
      } else {
        pal <- colors
      }
      
      p <- ggplot2::ggplot()
      if (!is.null(basemap_layer)) p <- p + basemap_layer
      p <- p +
        tidyterra::geom_spatraster(data = map_data) +
        ggplot2::scale_fill_manual(
          values   = pal,
          na.value = na_color,
          labels   = scales::label_wrap(label_wrap_width),
          name     = if (!is.null(legend)) legend else NULL
        )
      
    } else if (inherits(map_data, "sf")) {
      
      if (is.null(column) || is.na(column) || column == "") {
        map_data <- sf::st_zm(map_data, drop = TRUE, what = "ZM")
        map_data <- sf::st_make_valid(map_data)
        map_data <- map_data[!sf::st_is_empty(map_data), ]
        
        geom_types <- unique(as.character(sf::st_geometry_type(map_data)))
        is_polygon <- any(grepl("POLYGON", geom_types))
        
        p <- ggplot2::ggplot()
        if (!is.null(basemap_layer)) p <- p + basemap_layer
        
        if (is_polygon) {
          p <- p + ggplot2::geom_sf(data = map_data, fill = "lightblue",
                                    color = "darkblue", size = 0.2)
        } else {
          p <- p + ggplot2::geom_sf(data = map_data, color = "darkblue")
        }
        
      } else {
        if (!column %in% names(map_data)) {
          stop(sprintf("Column '%s' not found in `map`.", column))
        }
        
        map_data <- sf::st_zm(map_data, drop = TRUE, what = "ZM")
        map_data <- sf::st_make_valid(map_data)
        map_data <- map_data[!sf::st_is_empty(map_data), ]
        
        map_data[[column]] <- factor(map_data[[column]])
        class_labels <- levels(map_data[[column]])
        
        if (is.null(colors)) {
          pal <- stats::setNames(scales::hue_pal()(length(class_labels)), class_labels)
        } else {
          pal <- colors
        }
        
        geom_types <- unique(as.character(sf::st_geometry_type(map_data)))
        is_polygon <- any(grepl("POLYGON", geom_types))
        
        p <- ggplot2::ggplot()
        if (!is.null(basemap_layer)) p <- p + basemap_layer
        
        if (is_polygon) {
          p <- p +
            ggplot2::geom_sf(data = map_data, ggplot2::aes(fill = .data[[column]]), color = NA) +
            ggplot2::scale_fill_manual(
              values   = pal,
              na.value = na_color,
              labels   = scales::label_wrap(label_wrap_width),
              name     = if (!is.null(legend)) legend else NULL
            )
        } else {
          p <- p +
            ggplot2::geom_sf(data = map_data, ggplot2::aes(color = .data[[column]])) +
            ggplot2::scale_color_manual(
              values   = pal,
              na.value = na_color,
              labels   = scales::label_wrap(label_wrap_width),
              name     = if (!is.null(legend)) legend else NULL
            )
        }
      }
      
    } else {
      stop("`map` must be a SpatRaster or an sf object.")
    }
    
    p +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title) +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 8),
        axis.text.y = ggplot2::element_text(size = 8),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0),
        plot.margin = ggplot2::margin(t = 5, r = 5, b = 2, l = 2)
      )
    
    if (inherits(map_data, "sf") && (is.null(column) || is.na(column) || column == "")) {
      p <- p + ggplot2::theme(legend.position = "none")
    } else {
      p <- p +
        ggplot2::guides(
          fill = ggplot2::guide_legend(
            title.position = "top",
            ncol      = legend_ncol,
            keywidth  = grid::unit(0.4, "cm"),
            keyheight = grid::unit(0.4, "cm")
          ),
          color = ggplot2::guide_legend(
            title.position = "top",
            ncol      = legend_ncol,
            keywidth  = grid::unit(0.4, "cm"),
            keyheight = grid::unit(0.4, "cm")
          )
        ) +
        ggplot2::theme(
          legend.title = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 9),
          legend.position = "right",
          legend.justification = c(0, 0.5),
          legend.box.spacing = grid::unit(0.5, "cm"),
          legend.margin = ggplot2::margin(0, 0, 0, 0)
        )
    }
    
    p
  }
  
  plot_lc <- build_plot(map)
  
  if (!is.null(filepath)) {
    map_3857 <- .reproject_to_3857(map)
    bm <- .get_esri_gray_basemap(map_3857)
    export_plot <- if (!is.null(bm)) build_plot(map_3857, bm) else plot_lc
    ggplot2::ggsave(filename = filepath, plot = export_plot,
                    width = width, height = height, dpi = dpi)
  }
  
  return(plot_lc)
}
