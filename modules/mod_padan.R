# ui/modules/mod_padan.R
# ============================================================
#  MODULE: PADAN Analysis (3. PADAN)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── UI ──────────────────────────────────────────────────────────
padan_ui <- function(id) {
  ns <- NS(id)
  
  # Define the Query Tab UI
  query_tab <- nav_panel(
    "Telusuri Unit Perencanaan",
    div(
      class = "query-tab-wrapper",
      tags$style(HTML("
        .query-tab-wrapper,
        .query-tab-wrapper .card,
        .query-tab-wrapper .card-body,
        .query-tab-wrapper .tab-pane,
        .query-tab-wrapper .shiny-input-container,
        .query-tab-wrapper .selectize-control,
        .query-tab-wrapper .selectize-dropdown {
          overflow: visible !important;
        }
        .query-tab-wrapper .selectize-dropdown {
          z-index: 10000 !important;
        }
        .query-tab-wrapper .panel-toggle-btn { display: none !important; }

        /* Attribute table: near-normal font, moderate padding */
        .query-tab-wrapper .dataTables_wrapper,
        .query-tab-wrapper .dataTables_wrapper table.dataTable,
        .query-tab-wrapper .dataTables_wrapper table.dataTable thead th,
        .query-tab-wrapper .dataTables_wrapper table.dataTable tbody td {
          font-size: 0.9rem !important;
        }
        .query-tab-wrapper .dataTables_wrapper table.dataTable thead th,
        .query-tab-wrapper .dataTables_wrapper table.dataTable tbody td {
          padding: 4px 6px !important;
          line-height: 1.2 !important;
          white-space: nowrap;
        }
      ")),
      
      # Control Bar 
      card(
        card_header("Filter Pencarian"),
        fluidRow(
          column(4, uiOutput(ns("query_id_type_ui"))),
          column(4, 
                 numericInput(ns("query_id_val"), "Masukkan ID Numerik", value = 1, min = 1),
                 uiOutput(ns("query_id_hint_ui")) 
          ),
          column(4, 
                 div(style = "margin-top: 25px;",
                     actionButton(ns("btn_query"), "Cari Data", 
                                  class = "btn-primary btn-sm w-100",
                                  icon = icon("search"))
                 )
          )
        )
      ),
      
      # Dynamic Headers 
      uiOutput(ns("query_header_and_cards_ui")),
      
      # Main Content
      bslib::layout_columns(
        col_widths = c(8, 4), 
        card(
          card_header("Visualisasi Peta"),
          leafletOutput(ns("query_map"), height = "500px")
        ),
        card(
          card_header("Detail Atribut"),
          uiOutput(ns("group_member_selector_ui")), 
          div(style = "max-height: 450px; overflow-y: auto;",
              DT::DTOutput(ns("query_result_table"))
          )
        )
      )
    )
  )
  
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("3. Analisis PADAN", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menghitung nilai akhir integrasi (Indeks PADAN) berdasarkan penggabungan Indeks SERASI dan Indeks PADU.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),
    
    fluidRow(
      class = "g-3",
      
      # ── Left column: Input & Parameter (1/3) ─────────────────
      column(
        width = 4,
        card(
          card_header("Input & Parameter"),
          
          fileInput(ns("idx_padu_file"), "Pilih Peta Hasil Analisis PADU (.gpkg)",
                    accept = ".gpkg"),
          tags$div(
            class = "form-text text-muted",
            style = "margin-top: -8px; margin-bottom: 12px; font-size: 0.8rem;",
            "Gunakan file 'idx_padu.gpkg' dari modul 2.8"
          ),
          
          hr(),
          
          sliderInput(ns("alpha_val"), 
                      label = tags$span("Proporsi Alpha (\u03B1)", 
                                        tags$i(class = "bi bi-info-circle ms-1", 
                                               title = "Alpha: Bobot untuk SERASI. (1-Alpha): Bobot untuk PADU Final")),
                      min = 0, max = 1, value = 0.5, step = 0.1),
          
          div(
            style = "background: #f8f9fa; padding: 10px; border-radius: 6px; font-size: 0.85rem;",
            tags$strong("Formula:"), br(),
            tags$code("(\u03B1 * idx_serasi) + ((1 - \u03B1) * idx_padu_final)")
          ),
          
          hr(),
          
          div(
            style = "display: flex; gap: 8px; flex-wrap: wrap;",
            actionButton(ns("btn_run"),
                         tagList(tags$i(class = "bi bi-play-fill me-1"),
                                 "Lakukan Analisis PADAN"),
                         class = "btn-success btn-sm")
          )
        )
      ),
      
      # ── Right column: Output & Hasil (2/3) ───────────────────
      column(
        width = 8,
        card(
          card_header("Output & Hasil"),
          
          uiOutput(ns("status_box")),
          
          hr(),
          
          create_result_ui(ns, extra_tab = query_tab)
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────────────────────
padan_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Reactive values ──────────────────────────────────────────────────────
    rv <- reactiveValues(
      analysis_result = NULL,
      gpkg_path = NULL,
      xlsx_path = NULL,
      log_messages = "",
      raw_map = NULL,
      has_id_group = FALSE,
      query_geom = NULL,
      query_result = NULL
    )
    
    # ── Log helper ───────────────────────────────────────────────────────────
    append_log <- function(msg) {
      rv$log_messages <- paste0(rv$log_messages, format(Sys.time(), "[%H:%M:%S] "), msg, "\n")
    }
    
    # ── Run analysis ─────────────────────────────────────────────────────────
    observeEvent(input$btn_run, {
      
      # Check output directory 
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification(
          "Direktori output belum diatur. Harap atur direktori output terlebih dahulu.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      req(input$idx_padu_file)
      
      # Reset previous results
      rv$analysis_result <- NULL
      rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL
      rv$log_messages <- ""
      rv$raw_map <- NULL
      rv$has_id_group <- FALSE
      rv$query_geom <- NULL
      rv$query_result <- NULL
      
      append_log("Memulai analisis PADAN...")
      
      withProgress(message = "Menjalankan Analisis PADAN", value = 0, {
        
        tryCatch({
          # Step 1: Load data (progress 20%)
          incProgress(0.2, detail = "Memuat file PADU...")
          idx_padu_map <- sf::st_read(input$idx_padu_file$datapath, quiet = TRUE)
          append_log("File PADU berhasil dimuat.")
          
          # Validate required columns
          required_cols <- c("idx_serasi", "idx_padu_final")
          missing_cols <- setdiff(required_cols, names(idx_padu_map))
          if (length(missing_cols) > 0) {
            stop(paste("Kolom berikut tidak ditemukan:", paste(missing_cols, collapse = ", ")))
          }
          append_log("Kolom yang diperlukan ditemukan.")
          
          # Step 2: Calculate PADAN (progress 60%)
          incProgress(0.4, detail = "Menghitung indeks PADAN...")
          alpha <- input$alpha_val
          append_log(paste("Menggunakan alpha =", alpha))
          
          idx_padan_map <- idx_padu_map %>%
            dplyr::mutate(
              idx_padan = (alpha * idx_serasi) + ((1 - alpha) * idx_padu_final)
            )
          append_log("Perhitungan indeks PADAN selesai.")
          
          # Conditional Dissolve 
          idx_padan_map_viz <- tryCatch({
            df_for_dissolve <- idx_padan_map
            if (!"geometry" %in% names(df_for_dissolve)) {
              sf::st_geometry(df_for_dissolve) <- "geometry"   
            }
            dissolve_id_pu(df_for_dissolve)
          }, error = function(e) {
            append_log(paste("ERROR DISSOLVE:", conditionMessage(e)))
            idx_padan_map
          })

          rv$raw_map <- idx_padan_map_viz
          rv$has_id_group <- "id_group" %in% names(idx_padan_map_viz)
          
          # Step 3: Save results (progress 90%)
          incProgress(0.3, detail = "Menyimpan hasil...")
          
          padan_dir <- file.path(output_dir(), "Analisis PADAN")
          if (!dir.exists(padan_dir)) {
            dir.create(padan_dir, recursive = TRUE, showWarnings = FALSE)
          }
          
          if (!dir.exists(padan_dir)) {
            stop("Tidak dapat membuat atau mengakses direktori: ", padan_dir)
          }
          
          gpkg_path <- file.path(padan_dir, "idx_padan.gpkg")
          xlsx_path <- file.path(padan_dir, "idx_padan.xlsx")

          sf::st_write(idx_padan_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          res_table <- sf::st_drop_geometry(idx_padan_map)
          openxlsx::write.xlsx(res_table, xlsx_path)
          
          rv$gpkg_path <- gpkg_path
          rv$xlsx_path <- xlsx_path
          
          rv$analysis_result <- list(map = idx_padan_map_viz, table = sf::st_drop_geometry(idx_padan_map_viz))
          
          # Store result for report generation ──
          out <- list(
            inputs = list(
              start_time = Sys.time(),
              idx_padu_path = input$idx_padu_file,
              alpha = input$alpha_val,
              output_dir = output_dir()
            ),
            result = list(
              idx_padan_map = idx_padan_map, 
              idx_padan_table = res_table
            )
          )
          
          # Export log
          log_dir <- file.path(padan_dir, "log")
          if (!dir.exists(log_dir)) {
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
          }
          log_path <- file.path(log_dir, "idx_padan_log.rda")
          if (dir.exists(log_dir)) {
            tryCatch({
              inputs <- out$inputs
              save(inputs, file = log_path)
            }, error = function(e) {
              warning("Gagal menulis file log: ", e$message)
            })
          } else {
            warning("Direktori log tidak tersedia, lewati penulisan log.")
          }
          
          # Store in shared environment
          session$userData$module_results$padan <- out
          
          # Export static maps
          idx_padan_viz <- plot_continuous_map(
            map      = idx_padan_map_viz,
            column   = "idx_padan",         
            title    = "Peta Indeks PADAN",
            legend   = "Indeks PADAN",
            low      = "red",
            high     = "lightgreen",
            filepath = file.path(log_dir, "idx_padan.png")
          )
          
          append_log(paste("Peta disimpan →", gpkg_path))
          append_log(paste("Tabel disimpan →", xlsx_path))
          append_log("Analisis PADAN berhasil diselesaikan.")
          
          incProgress(0.1, detail = "Selesai!")
          showNotification(paste("Analisis selesai. Hasil disimpan ke", gpkg_path),
                           type = "message", duration = 5)
          
        }, error = function(e) {
          msg <- conditionMessage(e)
          if (is.null(msg) || msg == "") msg <- "Error tidak diketahui (lihat konsol untuk detail)"
          append_log(paste("ERROR:", msg))
          showNotification(paste("Analisis gagal:", msg), type = "error", duration = 10)
        })
        
      }) 
    })
    
    # ── Status box ───────────────────────────────────────────────────────────
    output$status_box <- renderUI({
      if (!is.null(rv$analysis_result)) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Analisis selesai.")
      } else if (!is.null(input$idx_padu_file)) {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Siap menjalankan analisis.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Unggah file dan klik Jalankan Analisis.")
      }
    })
    
    # ── Query Tab Logic ──────────────────────────────────────────────────────
    output$query_id_type_ui <- renderUI({
      if (is.null(rv$raw_map)) return(NULL)
      choices <- c("id_pu")
      if (isTRUE(rv$has_id_group)) choices <- c(choices, "id_group")
      selectInput(ns("query_id_type"), "Pilih Tipe ID", choices = choices)
    })
    
    # ── Hint Text for Available IDs ─────────────────────────────
    output$query_id_hint_ui <- renderUI({
      req(rv$raw_map, input$query_id_type)
      id_type <- input$query_id_type
      
      available_all <- unique(as.numeric(rv$raw_map[[id_type]]))
      n_avail <- length(available_all)
      max_avail <- suppressWarnings(max(available_all, na.rm = TRUE))
      
      if (!is.finite(max_avail)) max_avail <- "—"
      
      tags$small(
        sprintf("Tersedia: %d %s, dengan nilai maksimum %s.", n_avail, id_type, max_avail),
        style = "color: #6c757d; display: block; margin-top: 4px; font-size: 0.75em;"
      )
    })
    
    observeEvent(input$btn_query, {
      req(rv$raw_map, input$query_id_val, input$query_id_type)
      
      df <- rv$raw_map
      id_val <- input$query_id_val
      id_type <- input$query_id_type
      
      if (id_type == "id_pu") {
        filtered <- df[df$id_pu == id_val, ]
      } else {
        filtered <- df[df$id_group == id_val, ]
      }
      
      if (nrow(filtered) == 0) {
        showNotification("ID tidak ditemukan dalam data.", type = "warning")
        rv$query_geom <- NULL
        rv$query_result <- NULL
        return()
      }
      
      rv$query_geom <- filtered
      rv$query_result <- NULL
    })
    
    # ── Dashboard Headers & Cards UI ─────────────────────────────────────────
    output$query_header_and_cards_ui <- renderUI({
      req(rv$query_geom)
      
      id_type <- input$query_id_type
      id_val <- input$query_id_val
      
      # Extract values
      if (id_type == "id_pu") {
        val_padan  <- rv$query_geom$idx_padan[1]
        val_serasi <- rv$query_geom$idx_serasi[1]
        val_padu   <- rv$query_geom$idx_padu_final[1]
      } else {
        numeric_df <- rv$query_geom %>% sf::st_drop_geometry() %>% dplyr::select(where(is.numeric))
        n_members <- nrow(rv$query_geom)
        
        get_group_val <- function(col_name) {
          if (!col_name %in% names(numeric_df)) return(NA_real_)
          vals <- numeric_df[[col_name]]
          vals <- vals[!is.na(vals)]
          if (length(vals) == 0) return(NA_real_)
          if (n_members == 1 || length(vals) == 1) return(vals[1])
          c(min(vals), max(vals))
        }
        
        val_padan  <- get_group_val("idx_padan")
        val_serasi <- get_group_val("idx_serasi")
        val_padu   <- get_group_val("idx_padu_final")
      }
      
      fmt_val <- function(x) {
        if (length(x) == 0) return("N/A")
        if (all(is.na(x))) return("N/A")
        if (length(x) >= 2) {
          mn <- x[1]; mx <- x[2]
          if (is.na(mn) || is.na(mx)) return("N/A")
          if (mn == mx) return(format(round(mn, 2), nsmall = 2))
          return(paste0(format(round(mn, 2), nsmall = 2),
                        " \u2013 ",  # en-dash
                        format(round(mx, 2), nsmall = 2)))
        }
        val <- x[1]
        if (is.na(val)) return("N/A")
        format(round(as.numeric(val), 3), nsmall = 3)
      }
      
      # Dynamic Headers
      if (id_type == "id_pu") {
        is_adjacent <- "id_group" %in% names(rv$query_geom)
        case <- if(is_adjacent) "Bertetangga" else "Tumpang Tindih"
        h1 <- sprintf("Unit Perencanaan %s - Kasus %s %s dan %s", 
                      id_val, case, rv$query_geom$RTRW[1], rv$query_geom$RZWP3K[1])
        
        if (is_adjacent) {
          h2 <- sprintf("Lokasi: %s - Unit Grup: %s", 
                        rv$query_geom$admin[1], rv$query_geom$id_group[1])
        } else {
          h2 <- sprintf("Lokasi: %s", rv$query_geom$admin[1])
        }
      } else {
        # Hub detection logic 
        tbl_rtrw <- table(rv$query_geom$RTRW)
        tbl_rzwp <- table(rv$query_geom$RZWP3K)
        max_rtrw <- max(tbl_rtrw)
        max_rzwp <- max(tbl_rzwp)
        hub_class <- if (max_rtrw >= max_rzwp) names(tbl_rtrw)[which.max(tbl_rtrw)] else names(tbl_rzwp)[which.max(tbl_rzwp)]
        
        n_pairs <- nrow(rv$query_geom)
        h1 <- sprintf("Unit Grup %s - Kasus Bertetangga %s dengan %d pasangan", 
                      id_val, hub_class, n_pairs)
        
        admins <- paste(unique(rv$query_geom$admin), collapse = ", ")
        id_pus <- paste(rv$query_geom$id_pu, collapse = ", ")
        h2 <- sprintf("Lokasi: %s - Unit Perencanaan: %s", admins, id_pus)
      }
      
      # Render Headers and Cards
      tagList(
        div(
          style = "margin-bottom: 16px;",
          h5(h1, style = "font-weight: 700; color: #1e293b; margin-bottom: 4px;"),
          h6(h2, style = "font-weight: 500; color: #64748b; font-size: 0.9rem; margin: 0;")
        ),
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          # Card 1: SERASI (Blue)
          card(
            class = "p-3",
            style = "border-left: 5px solid #1e88e5; background-color: #e3f2fd;",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              div(
                h6("Indeks SERASI", style = "color: #1e88e5; font-size: 0.75rem; margin-bottom: 4px; font-weight: 700; text-transform: uppercase;"),
                h3(fmt_val(val_serasi), style = "font-weight: 700; color: #0d47a1; margin: 0;")
              ),
              icon("handshake", style = "font-size: 2rem; color: #bbdefb;")
            )
          ),
          # Card 2: PADU Kombinasi (Green)
          card(
            class = "p-3",
            style = "border-left: 5px solid #43a047; background-color: #e8f5e9;",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              div(
                h6("Indeks PADU", style = "color: #43a047; font-size: 0.75rem; margin-bottom: 4px; font-weight: 700; text-transform: uppercase;"),
                h3(fmt_val(val_padu), style = "font-weight: 700; color: #1b5e20; margin: 0;")
              ),
              icon("layer-group", style = "font-size: 2rem; color: #c8e6c9;")
            )
          ),
          # Card 3: PADAN (Yellow)
          card(
            class = "p-3",
            style = "border-left: 5px solid #fbc02d; background-color: #fff8e1;",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              div(
                h6("Indeks PADAN", style = "color: #fbc02d; font-size: 0.75rem; margin-bottom: 4px; font-weight: 700; text-transform: uppercase;"),
                h3(fmt_val(val_padan), style = "font-weight: 700; color: #f57f17; margin: 0;")
              ),
              icon("calculator", style = "font-size: 2rem; color: #fff176;")
            )
          )
        )
      )
    })
    
    # ── Group Member Selector Dropdown ───────────────────────────
    output$group_member_selector_ui <- renderUI({
      req(rv$query_geom)
      if (input$query_id_type == "id_group") {
        selectInput(
          ns("selected_group_member"), 
          "Pilih ID PU untuk melihat detail:", 
          choices = rv$query_geom$id_pu, 
          selected = rv$query_geom$id_pu[1]
        )
      } else {
        return(NULL)
      }
    })
    
    # ── Reactive Data for Attribute Table ────────────────────────
    query_table_data <- reactive({
      req(rv$query_geom)
      
      if (input$query_id_type == "id_pu") {
        df <- rv$query_geom
      } else {
        req(input$selected_group_member)
        df <- rv$query_geom[rv$query_geom$id_pu == as.numeric(input$selected_group_member), ]
      }
      
      # Remove specified columns
      cols_to_remove <- c("id_pu", "id_group", "RTRW", "RZWP3K", "admin")
      df_clean <- df[, !names(df) %in% cols_to_remove, drop = FALSE]
      
      # Transpose for display
      row_data <- sf::st_drop_geometry(df_clean)[1, , drop = FALSE]
      res <- data.frame(
        Parameter = names(row_data),
        Nilai = as.character(row_data[1, ]),
        stringsAsFactors = FALSE
      )
      
      # Format decimals to 2 digits for numeric values
      res$Nilai <- sapply(res$Nilai, function(x) {
        if (grepl("^-?[0-9.]+$", x)) {
          num <- suppressWarnings(as.numeric(x))
          if (!is.na(num)) return(format(round(num, 2), nsmall = 2))
        }
        return(x)
      })
      
      return(res)
    })
    
    output$query_result_table <- DT::renderDT({
      req(query_table_data())
      DT::datatable(
        query_table_data(), 
        options = list(
          paging = FALSE, 
          dom = 't',      
          scrollX = TRUE,
          scrollY = "400px"
        ), 
        rownames = FALSE
      )
    })
    
    # Popup builder for query map 
    build_query_popup <- function(df) {
      sapply(seq_len(nrow(df)), function(i) {
        r <- df[i, ]
        parts <- c(
          paste0("<b>ID PU</b>: ", if ("id_pu" %in% names(r)) r$id_pu else "-"),
          if ("RTRW"  %in% names(r)) paste0("<b>RTRW</b>: ",  r$RTRW)  else NULL,
          if ("RZWP3K" %in% names(r)) paste0("<b>RZWP3K</b>: ", r$RZWP3K) else NULL
        )
        paste(parts, collapse = "<br>")
      })
    }
    
    # ── Base Map Rendering ────────────────────────────────────────────────────
    output$query_map <- renderLeaflet({
      req(rv$query_geom)
      map_data <- rv$query_geom
      if (!sf::st_is_longlat(map_data)) {
        map_data <- sf::st_transform(map_data, 4326)
      }
      
      popup_html <- build_query_popup(map_data)
      
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addPolygons(
          data = map_data,
          fillColor = "red",
          fillOpacity = 0.4,
          stroke = FALSE,
          label = ~paste("ID PU:", id_pu),
          popup = popup_html
        )
    })
    
    # ── Highlight Logic for Group Members ─────────────────────────────────────
    observeEvent(input$selected_group_member, {
      req(rv$query_geom, input$selected_group_member)
      if (input$query_id_type != "id_group") return()
      
      selected_id <- as.numeric(input$selected_group_member)
      highlight_data <- rv$query_geom[rv$query_geom$id_pu == selected_id, ]
      
      if (!sf::st_is_longlat(highlight_data)) {
        highlight_data <- sf::st_transform(highlight_data, 4326)
      }
      
      popup_html <- build_query_popup(highlight_data)
      
      leafletProxy(ns("query_map")) %>%
        clearGroup("highlight") %>%
        addPolygons(
          data = highlight_data,
          fillColor = "yellow",
          fillOpacity = 0.8,
          color = "black",
          weight = 3,
          group = "highlight",
          label = ~paste("ID PU:", id_pu),
          popup = popup_html
        )
    })
    
    # ── Result Visualization (Main Tab) ───────────────────────────────────────
    padan_config <- list(
      map_color_col = "idx_padan",
      map_title = "Indeks PADAN",
      map_palette = "RdYlGn",
      map_label_cols = c(
        "ID PU"        = "id_pu",
        "RTRW"         = "RTRW",
        "RZWP3K"       = "RZWP3K",
        "Indeks SERASI" = "idx_serasi",
        "Indeks PADU"  = "idx_padu_final",
        "Indeks PADAN" = "idx_padan"
      ),
      table_cols = c(
        "id_pu"         = "ID PU",
        "RTRW"          = "RTRW",
        "RZWP3K"        = "RZWP3K",
        "idx_serasi"    = "Indeks SERASI",
        "idx_padu_final" = "Indeks PADU",
        "idx_padan"     = "Indeks PADAN"
      ),
      table_round_cols = c("Indeks SERASI", "Indeks PADU", "Indeks PADAN")
    )
    
    render_result_server(input, output, session, rv, padan_config)
    
  })
}