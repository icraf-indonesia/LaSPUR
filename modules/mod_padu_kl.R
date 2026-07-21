# ui/modules/mod_padu_kl.R
# ============================================================
#  MODULE: PADU-KL (2.3 Conservation Area Analysis)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── UI ──────────────────────────────────────────────────────────
padu_kl_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("2.3 PADU-KL (Kawasan Lindung)", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menilai kepaduan lingkungan berdasarkan komposisi kawasan lindung pada bentang darat dan laut untuk menghasilkan nilai indeks PADU-KL.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),
    
    fluidRow(
      class = "g-3",
      
      # ── Left column: Input & Parameter (1/3) ────────────────
      column(
        width = 4,
        card(
          card_header("Input & Parameter"),
          
          tags$p(tags$i(class = "bi bi-info-circle me-1"),
                 "Peta Indeks SERASI (.gpkg atau .shp)",
                 style = "font-weight: 600; margin-bottom: 4px;"),
          tags$small(
            style = "color: #6c757d; display: block; margin-bottom: 8px;",
            "Output dari modul 'Identifikasi Konflik Spasial' (idx_serasi.gpkg). Menerima .gpkg atau .shp."
          ),
          fileInput(ns("idx_serasi_file"),
                    label    = NULL,
                    accept   = c(".gpkg", ".shp", ".dbf", ".prj", ".shx", ".cpg"),
                    multiple = TRUE),
          
          hr(),
          
          tags$p(tags$i(class = "bi bi-shield-check me-1"),
                 "Peta Kawasan Lindung (.shp)",
                 style = "font-weight: 600; margin-bottom: 4px;"),
          tags$small(
            style = "color: #6c757d; display: block; margin-bottom: 8px;",
            "Layer vektor kawasan lindung/konservasi yang akan ditumpangtindihkan dengan unit perencanaan."
          ),
          fileInput(ns("protected_area_file"),
                    label    = NULL,
                    accept   = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                    multiple = TRUE),
          
          hr(),
          
          # ── Pengaturan lanjutan (collapsible) ──────────────
          accordion(
            accordion_panel(
              title = "Pengaturan lanjutan",
              icon = icon("gear"),
              open = FALSE,
              checkboxInput(ns("parallel"), "Aktifkan pemrosesan paralel", value = FALSE),
              numericInput(ns("workers"), "Jumlah kanal komputasi (cores)", value = 2, min = 1, step = 1)
            )
          ),
          
          hr(),
          
          div(
            style = "display: flex; gap: 8px; flex-wrap: wrap;",
            actionButton(ns("btn_run"),
                         tagList(tags$i(class = "bi bi-play-fill me-1"),
                                 "Lakukan Analisis PADU-KL"),
                         class = "btn-success btn-sm")
          )
        )
      ),
      
      # ── Right column: Output & Hasil (2/3) ──────────────────
      column(
        width = 8,
        card(
          card_header("Output & Hasil"),
          
          uiOutput(ns("status_box")),
          
          hr(),
          
          navset_tab(
            nav_panel(
              "Peta",
              leafletOutput(ns("result_map"), height = "500px")
            ),
            nav_panel(
              "Tabel",
              div(
                style = "height: 500px; overflow: auto;",
                DT::DTOutput(ns("result_table"))
              )
            ),
            nav_panel(
              "Log Validasi",
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
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────────
padu_kl_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Reactive values ──────────────────────────────────────────
    rv <- reactiveValues(
      analysis_result = NULL,
      gpkg_path = NULL,
      xlsx_path = NULL,
      log_messages = ""
    )
    
    # ── Robust helper to extract shapefile path ────────────────
    extract_shp_path <- function(file_input) {
      shp_row <- file_input[grepl("\\.shp$", file_input$name, ignore.case = TRUE), ]
      validate(need(
        nrow(shp_row) == 1,
        "Harap unggah semua komponen shapefile (.shp, .dbf, .prj, .shx)"
      ))
      base_name <- tools::file_path_sans_ext(shp_row$name)
      temp_dir <- file.path(tempdir(), paste0("shp_", sample(1e9, 1)))
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
      for (i in seq_len(nrow(file_input))) {
        ext <- tools::file_ext(file_input$name[i])
        new_path <- file.path(temp_dir, paste0(base_name, ".", ext))
        file.copy(file_input$datapath[i], new_path, overwrite = TRUE)
      }
      file.path(temp_dir, paste0(base_name, ".shp"))
    }
    
    # ── Helper to extract vector path (gpkg or shp) ─────────────
    extract_vector_path <- function(file_input) {
      gpkg_row <- file_input[grepl("\\.gpkg$", file_input$name, ignore.case = TRUE), ]
      if (nrow(gpkg_row) == 1) return(gpkg_row$datapath)
      extract_shp_path(file_input)
    }
    
    # ── Log helper ──────────────────────────────────────────────
    append_log <- function(msg) {
      rv$log_messages <- paste0(rv$log_messages, format(Sys.time(), "[%H:%M:%S] "), msg, "\n")
    }
    
    # ── Run analysis ──────────────────────────────────────────
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
      
      req(input$idx_serasi_file, input$protected_area_file)
      
      # Reset previous results
      rv$analysis_result <- NULL
      rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL
      rv$log_messages <- ""
      
      append_log("Memulai analisis PADU-KL...")
      
      withProgress(message = "Menjalankan Analisis PADU-KL", value = 0, {
        
        tryCatch({
          # Step 1: Load data (progress 10%)
          incProgress(0.1, detail = "Memuat data...")
          pu_raw <- load_and_validate_shapefile(extract_vector_path(input$idx_serasi_file))
          pu_raw <- ensure_geometry_name(pu_raw)  
          
          # Conditional dissolve idx_serasi_map
          if ("length" %in% colnames(pu_raw)) {
            pu <- dissolve_id_pu(pu_raw)
          } else {
            pu <- pu_raw  
          }
          
          overlay <- load_and_validate_shapefile(extract_shp_path(input$protected_area_file))
          overlay <- ensure_geometry_name(overlay)  
          
          append_log("Data berhasil dimuat.")
          
          # Step 2: Calculate overlay percentage (progress 20% → 80%)
          incProgress(0.1, detail = "Menghitung tumpang tindih kawasan lindung...")
          append_log("Menghitung persentase tumpang tindih dengan Kawasan Lindung...")
          
          idx_padu_kl_map <- calculate_overlay_pct(
            pu           = pu,
            overlay_area = overlay,
            title        = "protected",
            parallel     = input$parallel,
            workers      = input$workers
          ) %>%
            mutate(idx_padu_kl = protected_pct / 100)
          
          incProgress(0.6, detail = "Pemrosesan selesai...")
          append_log("Perhitungan persentase selesai.")
          
          # Step 3: Save results (progress 90%)
          incProgress(0.1, detail = "Menyimpan hasil...")
          gpkg_path <- file.path(output_dir(), "idx_padu_kl.gpkg")
          xlsx_path <- file.path(output_dir(), "idx_padu_kl.xlsx")
          dir.create(output_dir(), recursive = TRUE, showWarnings = FALSE)
          
          sf::st_write(idx_padu_kl_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          res_table <- as_tibble(sf::st_drop_geometry(idx_padu_kl_map))
          openxlsx::write.xlsx(res_table, xlsx_path)
          
          rv$gpkg_path <- gpkg_path
          rv$xlsx_path <- xlsx_path
          rv$analysis_result <- list(map = idx_padu_kl_map, table = res_table)
          
          append_log(paste("Peta disimpan →", gpkg_path))
          append_log(paste("Tabel disimpan →", xlsx_path))
          append_log("Analisis PADU-KL berhasil diselesaikan.")
          
          incProgress(0.1, detail = "Selesai!")
          showNotification(paste("Analisis selesai. Hasil disimpan ke", gpkg_path),
                           type = "message", duration = 5)
          
        }, error = function(e) {
          msg <- conditionMessage(e)
          if (is.null(msg) || msg == "") msg <- "Error tidak diketahui (lihat konsol untuk detail)"
          append_log(paste("ERROR:", msg))
          showNotification(paste("Analisis gagal:", msg), type = "error", duration = 10)
        })
        
      }) # end withProgress
    })
    
    # ── Status box ─────────────────────────────────────────────
    output$status_box <- renderUI({
      if (!is.null(rv$analysis_result)) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Analisis selesai.")
      } else if (!is.null(input$idx_serasi_file) && !is.null(input$protected_area_file)) {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Siap menjalankan analisis.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Unggah file dan klik Jalankan Analisis.")
      }
    })
    
    # ── Map output (leaflet) ──────────────────────────────────
    output$result_map <- renderLeaflet({
      req(rv$analysis_result)
      
      map_sf <- rv$analysis_result$map
      
      if (!sf::st_is_longlat(map_sf)) {
        map_sf <- sf::st_transform(map_sf, crs = 4326)
      }
      
      if (!"idx_padu_kl" %in% names(map_sf)) {
        return(leaflet::leaflet() %>% 
                 leaflet::addControl("Kolom idx_padu_kl tidak ditemukan.", position = "topright"))
      }
      
      map_sf$search_label <- paste0("ID PU: ", map_sf$id_pu, " | ", map_sf$RTRW, " | ", map_sf$RZWP3K, " | ",
                                    "Indeks PADU-KL: ", round(map_sf$idx_padu_kl, 2)) %>% lapply(htmltools::HTML)
      
      pal <- leaflet::colorNumeric(
        palette = "RdYlGn",
        domain  = map_sf$idx_padu_kl,
        na.color = "grey"
      )
      
      leaflet::leaflet(map_sf) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::addPolygons(
          group       = "padu_ki_layer",
          fillColor   = ~pal(idx_padu_kl),
          fillOpacity = 0.7,
          weight      = 1,
          color       = "black",
          label       = ~search_label,
          popup       = ~paste(
            "<b>ID PU:</b>", id_pu, "<br>",
            "<b>Indeks PADU-KL:</b>", round(idx_padu_kl, 3)
          ) %>% lapply(htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(
            weight = 3,
            color  = "red",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        leaflet.extras::addSearchFeatures(
          targetGroups = "padu_ki_layer",
          options = leaflet.extras::searchFeaturesOptions(
            propertyName = "label",    
            zoom = 15,                 
            openPopup = TRUE,           
            firstTipSubmit = TRUE,
            autoCollapse = FALSE,
            hideMarkerOnCollapse = TRUE
          )
        ) %>% leaflet.extras::addResetMapButton() %>% 
        leaflet::addLegend(
          position = "bottomright",
          pal      = pal,
          values   = ~idx_padu_kl,
          title    = "Indeks PADU-KL",
          opacity  = 0.7
        )
    })
    
    # ── Table output ───────────────────────────────────────────
    output$result_table <- DT::renderDT({
      req(rv$analysis_result)
      
      df <- rv$analysis_result$table
      df_subset <- df[, c("id_pu", "RTRW", "RZWP3K", "admin", "area_ha", "protected_ha", "idx_padu_kl")]
      
      colnames(df_subset) <- c("ID PU", "RTRW", "RZWP3K", "Administrasi", "Luas (ha)", "Kawasan Lindung (ha)", "Indeks PADU-KL")
      
      DT::datatable(
        df_subset,
        extensions = c('FixedColumns', 'FixedHeader'),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'Bfrtip',
          fixedColumns = list(
            leftColumns = 3
          ),
          fixedHeader = TRUE
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      ) %>%
        DT::formatRound(
          columns = c("Luas (ha)", "Kawasan Lindung (ha)", "Indeks PADU-KL"),  
          digits = 2
        )
    })
    
    # ── Validation log ─────────────────────────────────────────
    output$validation_log <- renderPrint({
      invalidateLater(100, session)
      cat(rv$log_messages)
    })
    
    # ── Download handlers ──────────────────────────────────────
    output$dl_gpkg <- downloadHandler(
      filename = function() "idx_padu_kl.gpkg",
      content = function(file) {
        req(rv$gpkg_path)
        file.copy(rv$gpkg_path, file, overwrite = TRUE)
      }
    )
    
    output$dl_xlsx <- downloadHandler(
      filename = function() "idx_padu_kl.xlsx",
      content = function(file) {
        req(rv$xlsx_path)
        file.copy(rv$xlsx_path, file, overwrite = TRUE)
      }
    )
    
  })
}