# R/ui_components/result_views.R
# ============================================================
# Reusable Shiny Result Containers (UI and Server)
# ============================================================

#' Create Standard Result Visualization UI
#'
#' @description
#' Generates the standardized output panel containing:
#' 1. "Visualisasi Hasil" tab (Leaflet map + interactive DT table).
#' 2. Optional module-specific extra tab.
#' 3. "Log" tab for runtime logs and validation diagnostics.
#' 4. Download buttons for GPKG and XLSX.
#'
#' @param ns Shiny namespace function (`session$ns` or `NS(id)`).
#' @param extra_tab Optional `nav_panel` to inject between map/table and log.
#' @return A Shiny UI object.
#' @export
create_result_ui <- function(ns, extra_tab = NULL) {
  tagList(
    bslib::navset_tab(
      bslib::nav_panel(
        "Visualisasi Hasil",
        fluidRow(
          column(
            width = 12,
            style = "margin-top: 10px;",
            leaflet::leafletOutput(ns("result_map"), height = "450px")
          )
        ),
        hr(style = "margin: 15px 0; border-top: 1px solid #dee2e6;"),
        fluidRow(
          column(
            width = 12,
            div(
              style = "max-height: 400px; overflow-y: auto; width: 100%;",
              DT::DTOutput(ns("result_table"), width = "100%")
            )
          )
        )
      ),
      if (!is.null(extra_tab)) extra_tab,
      bslib::nav_panel(
        "Log",
        div(
          style = "max-height: 300px; overflow-y: auto; background-color: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 0.9rem; white-space: pre-wrap;",
          verbatimTextOutput(ns("validation_log"))
        )
      )
    ),
    
    div(
      style = "display: flex; gap: 8px; flex-wrap: wrap; margin-top: 12px;",
      downloadButton(ns("dl_gpkg"), "Unduh GPKG", class = "btn-outline-secondary btn-sm"),
      downloadButton(ns("dl_xlsx"), "Unduh XLSX", class = "btn-outline-secondary btn-sm")
    )
  )
}

#' Render Result Visualization Server Logic
#'
#' @description
#' Registers the shared Shiny server-side outputs used across analysis modules:
#' a Leaflet map, a DT table, a validation log, and download handlers. Renders
#' map geometry with optional simplification, dynamic popups/labels, table
#' column subsetting and rounding, and synchronized table-to-map selection.
#'
#' @param input,output,session Standard Shiny server arguments from calling module.
#' @param rv Reactive values object containing `analysis_result$map`,
#'   `analysis_result$table`, `log_messages`, `gpkg_path`, and `xlsx_path`.
#' @param config A list of configuration options (columns, palettes, titles).
#' @export
render_result_server <- function(input, output, session, rv, config) {
  
  # Default configurations
  map_color_col <- config$map_color_col
  map_title <- config$map_title
  map_palette <- if (!is.null(config$map_palette)) config$map_palette else "RdYlGn"
  table_cols <- config$table_cols
  table_round_cols <- config$table_round_cols
  table_optional_cols <- config$table_optional_cols
  
  # Simplification tolerance (map units, typically meters for UTM data)
  map_simplify_tolerance <- if (!is.null(config$map_simplify_tolerance)) {
    config$map_simplify_tolerance
  } else {
    5 # meters
  }
  
  output$result_map <- leaflet::renderLeaflet({
    shiny::req(rv$analysis_result)
    
    map_sf <- rv$analysis_result$map
    
    # Build a display-only copy with simplified geometry for rendering.
    if (isTRUE(map_simplify_tolerance > 0) && nrow(map_sf) > 0 && !sf::st_is_longlat(map_sf)) {
      map_sf <- tryCatch({
        simplified <- sf::st_simplify(
          map_sf,
          dTolerance = map_simplify_tolerance,
          preserveTopology = TRUE
        )
        if (any(!sf::st_is_valid(simplified))) {
          simplified <- sf::st_make_valid(simplified)
        }
        simplified
      }, error = function(e) {
        map_sf
      })
    }
    
    # Check if geometry is empty or invalid
    if (nrow(map_sf) == 0 || all(sf::st_is_empty(map_sf))) {
      return(
        leaflet::leaflet() %>%
          leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
          leaflet::addPopups(0, 0, "Data spasial tidak memiliki geometri yang valid.")
      )
    }
    
    # Transform to WGS84 for leaflet display
    if (!sf::st_is_longlat(map_sf)) {
      map_sf <- tryCatch({
        sf::st_transform(map_sf, 4326)
      }, error = function(e) {
        showNotification(paste("Gagal mentransformasi koordinat peta:", e$message), type = "error")
        return(NULL)
      })
      if (is.null(map_sf)) return(NULL)
    }
    
    # Pre-calculate centroids for zoom-to-feature from table selection
    centroids_wgs84 <- tryCatch({
      cents <- sf::st_centroid(sf::st_geometry(map_sf))
      coords <- sf::st_coordinates(cents)
      data.frame(lng = coords[, 1], lat = coords[, 2])
    }, error = function(e) NULL)
    
    # Determine Color Palette
    is_numeric <- is.numeric(map_sf[[map_color_col]])
    
    if (is_numeric) {
      vals <- map_sf[[map_color_col]]
      vals_no_na <- vals[!is.na(vals)]
      
      if (length(vals_no_na) == 0) {
        pal <- function(x) rep("#808080", length(x))
      } else {
        min_v <- min(vals_no_na)
        max_v <- max(vals_no_na)
        domain_v <- if (min_v == max_v) c(min_v - 0.001, max_v + 0.001) else c(min_v, max_v)
        
        pal <- leaflet::colorNumeric(
          palette = map_palette,
          domain = domain_v,
          na.color = "#808080"
        )
      }
    } else {
      pal <- leaflet::colorFactor(
        palette = map_palette,
        domain = map_sf[[map_color_col]],
        na.color = "#808080"
      )
    }
    
    # Build Hover Labels
    label_cols <- if (!is.null(config$map_label_cols)) config$map_label_cols else names(map_sf)
    
    labels <- lapply(seq_len(nrow(map_sf)), function(i) {
      lbl_parts <- character(0)
      for (col_alias in names(label_cols)) {
        col_actual <- label_cols[[col_alias]]
        val <- map_sf[[col_actual]][i]
        
        if (!is.null(val) && !is.na(val)) {
          formatted_val <- if (is.numeric(val)) format(round(val, 2), nsmall = 2) else as.character(val)
          lbl_parts <- c(lbl_parts, sprintf("<strong>%s:</strong> %s", col_alias, formatted_val))
        }
      }
      htmltools::HTML(paste(lbl_parts, collapse = "<br/>"))
    })
    
    # Render Base Leaflet Map
    map_proxy <- leaflet::leaflet(map_sf, options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Positron (Default)") %>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satelit (Esri)") %>%
      leaflet::addPolygons(
        group = "data_layer",
        layerId = ~seq_len(nrow(map_sf)),
        fillColor = ~pal(map_sf[[map_color_col]]),
        fillOpacity = 0.7,
        weight = 1.2,
        color = "#ffffff",
        opacity = 0.9,
        label = labels,
        labelOptions = leaflet::labelOptions(
          style = list("font-weight" = "normal", padding = "4px 8px"),
          textsize = "12px",
          direction = "auto"
        ),
        highlightOptions = leaflet::highlightOptions(
          weight = 3,
          color = "#0056b3",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      leaflet::addLayersControl(
        baseGroups = c("Positron (Default)", "OpenStreetMap", "Satelit (Esri)"),
        overlayGroups = c("data_layer"),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) %>%
      leaflet.extras::addResetMapButton()
    
    # Add Search Control if ID PU exists
    if ("id_pu" %in% names(map_sf)) {
      map_proxy <- map_proxy %>%
        leaflet.extras::addSearchFeatures(
          targetGroups = "data_layer",
          options = leaflet.extras::searchFeaturesOptions(
            zoom = 14,
            openPopup = TRUE,
            firstTipSubmit = TRUE,
            autoCollapse = TRUE,
            hideMarkerOnCollapse = TRUE,
            textPlaceholder = "Cari ID PU..."
          )
        )
    }
    
    # Add Legend
    vals <- map_sf[[map_color_col]]
    if (is_numeric) {
      vals_no_na <- vals[!is.na(vals)]
      if (length(vals_no_na) > 0) {
        min_v <- min(vals_no_na)
        max_v <- max(vals_no_na)
        domain_v <- if (min_v == max_v) c(min_v - 0.001, max_v + 0.001) else c(min_v, max_v)
        map_proxy <- map_proxy %>%
          leaflet::addLegend(
            pal = pal,
            values = domain_v,
            title = map_title,
            position = "bottomright",
            opacity = 0.8,
            labFormat = leaflet::labelFormat(digits = 2)
          )
      }
    } else {
      map_proxy <- map_proxy %>%
        leaflet::addLegend(
          pal = pal,
          values = vals,
          title = map_title,
          position = "bottomright",
          opacity = 0.8
        )
    }
    
    map_proxy
  })
  
  # Render DT Result Table
  output$result_table <- DT::renderDT({
    shiny::req(rv$analysis_result)
    
    table_df <- rv$analysis_result$table
    if (inherits(table_df, "sf")) {
      table_df <- sf::st_drop_geometry(table_df)
    }
    table_df <- tibble::as_tibble(table_df)
    
    # Conditionally drop optional columns that are entirely NA
    effective_table_cols <- table_cols
    if (!is.null(table_optional_cols) && length(table_optional_cols) > 0) {
      all_na_keys <- table_optional_cols[vapply(table_optional_cols, function(k) {
        !k %in% names(table_df) || all(is.na(table_df[[k]]))
      }, logical(1))]
      if (length(all_na_keys) > 0) {
        effective_table_cols <- effective_table_cols[!effective_table_cols %in% all_na_keys]
      }
    }
    
    # Filter and Rename Columns if configuration provided
    if (!is.null(effective_table_cols)) {
      valid_cols <- intersect(effective_table_cols, names(table_df))
      col_labels <- names(effective_table_cols)[effective_table_cols %in% valid_cols]
      
      table_df <- table_df[, valid_cols, drop = FALSE]
      names(table_df) <- col_labels
    }
    
    # Build Interactive DataTable
    dt <- DT::datatable(
      table_df,
      options = list(
        pageLength = 10,
        lengthMenu = list(c(5, 10, 25, 50, -1), c("5", "10", "25", "50", "Semua")),
        scrollX = TRUE,
        autoWidth = FALSE,
        language = list(
          search = "Cari:",
          lengthMenu = "Tampilkan _MENU_ data",
          info = "Menampilkan _START_ sampai _END_ dari _TOTAL_ data",
          paginate = list(first = "Pertama", previous = "Sebelumnya", `next` = "Berikutnya", last = "Terakhir"),
          emptyTable = "Tidak ada data tersedia"
        )
      ),
      selection = "single",
      rownames = FALSE,
      class = "table table-hover table-striped table-bordered"
    )
    
    # Round specified columns
    if (!is.null(table_round_cols)) {
      valid_round_cols <- intersect(table_round_cols, names(table_df))
      if (length(valid_round_cols) > 0) {
        dt <- dt %>% DT::formatRound(columns = valid_round_cols, digits = 2)
      }
    }
    
    dt
  })
  
  # Synchronize Table Selection to Map Center/Zoom
  shiny::observeEvent(input$result_table_rows_selected, {
    selected_row <- input$result_table_rows_selected
    shiny::req(selected_row, rv$analysis_result)
    
    map_sf <- rv$analysis_result$map
    if (selected_row > nrow(map_sf)) return()
    
    # Calculate target feature centroid in 4326
    feature <- map_sf[selected_row, ]
    if (!sf::st_is_longlat(feature)) {
      feature <- tryCatch(sf::st_transform(feature, 4326), error = function(e) NULL)
      if (is.null(feature)) return()
    }
    
    centroid <- tryCatch({
      cents <- sf::st_centroid(sf::st_geometry(feature))
      sf::st_coordinates(cents)
    }, error = function(e) NULL)
    
    if (!is.null(centroid) && nrow(centroid) > 0) {
      leaflet::leafletProxy("result_map") %>%
        leaflet::setView(lng = centroid[1, 1], lat = centroid[1, 2], zoom = 12)
    }
  })
  
  # Render Validation Log Text
  output$validation_log <- shiny::renderPrint({
    if (!is.null(rv$log_messages) && nzchar(rv$log_messages)) {
      cat(rv$log_messages)
    } else {
      cat("Belum ada proses yang dijalankan.")
    }
  })
  
  # Download Handlers
  output$dl_gpkg <- shiny::downloadHandler(
    filename = function() {
      if (!is.null(rv$gpkg_path)) basename(rv$gpkg_path) else "hasil_analisis.gpkg"
    },
    content = function(file) {
      shiny::req(rv$gpkg_path)
      file.copy(rv$gpkg_path, file, overwrite = TRUE)
    }
  )
  
  output$dl_xlsx <- shiny::downloadHandler(
    filename = function() {
      if (!is.null(rv$xlsx_path)) basename(rv$xlsx_path) else "hasil_analisis.xlsx"
    },
    content = function(file) {
      shiny::req(rv$xlsx_path)
      file.copy(rv$xlsx_path, file, overwrite = TRUE)
    }
  )
}
