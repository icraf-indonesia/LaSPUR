# ui/modules/mod_padu_hs.R
# ============================================================
#  MODULE: PADU-HS (2.2)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── small UI helpers ────────────────────────────────────────────
.locked_panel <- function(msg = "Selesaikan langkah sebelumnya terlebih dahulu.") {
  div(
    class = "alert alert-secondary mb-0",
    tags$i(class = "bi bi-lock-fill me-2"), msg
  )
}

.step_nav <- function(ns, back_id = NULL, next_id = NULL, next_label = "Lanjut") {
  div(
    style = "display:flex; justify-content:space-between; margin-top:16px;",
    if (!is.null(back_id)) {
      actionButton(ns(back_id), tagList(tags$i(class = "bi bi-arrow-left me-1"), "Kembali"),
                   class = "btn-outline-secondary btn-sm")
    } else div(),
    if (!is.null(next_id)) {
      actionButton(ns(next_id), tagList(next_label, tags$i(class = "bi bi-arrow-right ms-1")),
                   class = "btn-success btn-sm")
    } else div()
  )
}

# ── UI ──────────────────────────────────────────────────────────
padu_hs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("2.2 PADU-HS (Hidrologi dan Sedimentasi)", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menilai kepaduan lingkungan berdasarkan kondisi hidrologis dan sedimentasi untuk menghasilkan nilai indeks PADU-HS.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),
    
    fluidRow(
      class = "g-3",
      
      # ── Left column: Wizard (1/3) ─────────────────────────────
      column(
        width = 4,
        card(
          card_header("Input & Parameter"),
          accordion(
            id = ns("wizard"),
            open = "step1",
            multiple = FALSE,
            
            accordion_panel(
              title = "Langkah 1 — Menyiapkan Data Utama",
              value = "step1",
              icon = tags$i(class = "bi bi-folder-fill"),
              uiOutput(ns("step1_ui"))
            ),
            
            accordion_panel(
              title = "Langkah 2 — Menganalisis Hidrologi dan Sedimentasi",
              value = "step2",
              icon = tags$i(class = "bi bi-droplet-fill"),
              uiOutput(ns("step2_ui"))
            )
          )
        )
      ),
      
      # ── Right column: Output & Hasil (2/3) ────────────────────
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
padu_hs_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Reactive values ──────────────────────────────────────────
    rv <- reactiveValues(
      unlocked = 1,                # 1 = only step1, 2 = step2 unlocked
      
      # step1 data
      idx_serasi_map = NULL,
      tss_rast = NULL,
      euc_dist_rast = NULL,
      estuari_mode = "upload_raster",
      
      # analysis results
      analysis_result = NULL,
      gpkg_path = NULL,
      xlsx_path = NULL,
      log_messages = ""
    )
    
    go_to_panel <- function(value) {
      accordion_panel_set(id = "wizard", values = value, session = session)
    }
    
    # ── Helpers for shapefile loading ──────────────────────────
    extract_shp_path <- function(file_input) {
      shp_row <- file_input[grepl("\\.shp$", file_input$name, ignore.case = TRUE), ]
      validate(need(
        nrow(shp_row) == 1,
        "Harap unggah semua komponen shapefile (.shp, .dbf, .prj, .shx)"
      ))
      stem <- tools::file_path_sans_ext(shp_row$datapath)
      for (i in seq_len(nrow(file_input))) {
        ext <- tools::file_ext(file_input$name[i])
        file.rename(file_input$datapath[i], paste0(stem, ".", ext))
      }
      paste0(stem, ".shp")
    }
    
    extract_vector_path <- function(file_input) {
      gpkg_row <- file_input[grepl("\\.gpkg$", file_input$name, ignore.case = TRUE), ]
      if (nrow(gpkg_row) == 1) return(gpkg_row$datapath)
      extract_shp_path(file_input)
    }
    
    # ── Log helper ──────────────────────────────────────────────
    append_log <- function(msg) {
      rv$log_messages <- paste0(rv$log_messages, format(Sys.time(), "[%H:%M:%S] "), msg, "\n")
    }
    
    # ── Step 1 UI ──────────────────────────────────────────────
    output$step1_ui <- renderUI({
      tagList(
        tags$p(tags$i(class = "bi bi-info-circle me-1"), "Peta Indeks SERASI (.gpkg atau .shp)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "Output dari modul 'Identifikasi Konflik Spasial' (idx_serasi.gpkg). Menerima .gpkg atau .shp."),
        fileInput(ns("idx_serasi_file"), label = NULL,
                  accept = c(".gpkg", ".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-layers me-1"), "Peta Total Suspended Solid (TSS) (.tif)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "File raster nilai TSS."),
        fileInput(ns("tss_file"), label = NULL, accept = c(".tif", ".tiff")),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-geo-alt me-1"), "Input Peta Jarak Estuari",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "Unggah raster jarak yang sudah dihitung, atau unggah shapefile estuari untuk dihitung otomatis."),
        
        radioButtons(
          ns("estuari_input_mode"),
          label = NULL,
          choices = c(
            "Unggah raster jarak yang sudah ada (.tif)" = "upload_raster",
            "Unggah shapefile estuari dan hitung otomatis" = "calculate"
          ),
          selected = "upload_raster"
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'upload_raster'", ns("estuari_input_mode")),
          fileInput(ns("euc_dist_file"), label = "Raster Jarak Estuari (.tif)", accept = c(".tif", ".tiff"))
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'calculate'", ns("estuari_input_mode")),
          fileInput(ns("estuari_file"), label = "Shapefile Estuari",
                    accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"), multiple = TRUE),
          numericInput(ns("euc_resolution"), "Resolusi Perhitungan (meter)", value = 30, min = 1),
          div(
            class = "alert alert-warning py-2 px-3",
            style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-exclamation-triangle me-1"),
            "Jarak Euclidean akan dihitung dari shapefile di atas. Proses ini dapat memakan beberapa menit untuk dataset besar. Hasilnya akan otomatis disimpan ke direktori output."
          )
        ),
        uiOutput(ns("estuari_status")),
        
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Langkah 2")
      )
    })
    
    # ── Load SERASI map, TSS, and distance raster ──────────────
    observeEvent(input$idx_serasi_file, {
      req(input$idx_serasi_file)
      tryCatch({
        path <- extract_vector_path(input$idx_serasi_file)
        sf_obj <- load_and_validate_shapefile(path)
        rv$idx_serasi_map <- ensure_geometry_name(sf_obj) 
        showNotification("Peta Indeks SERASI berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$idx_serasi_map <- NULL
        showNotification(paste("Gagal memuat peta SERASI:", e$message), type = "error")
      })
    })
    
    observeEvent(input$tss_file, {
      req(input$tss_file)
      tryCatch({
        rv$tss_rast <- load_and_validate_raster(input$tss_file$datapath)
        showNotification("Peta TSS berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$tss_rast <- NULL
        showNotification(paste("Gagal memuat TSS:", e$message), type = "error")
      })
    })
    
    # Distance raster: either uploaded or computed from shapefile
    observeEvent(input$euc_dist_file, {
      req(input$estuari_input_mode == "upload_raster", input$euc_dist_file)
      tryCatch({
        rv$euc_dist_rast <- terra::rast(input$euc_dist_file$datapath)
        showNotification("Raster jarak estuari berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$euc_dist_rast <- NULL
        showNotification(paste("Gagal memuat raster jarak:", e$message), type = "error")
      })
    })
    
    # Compute distance from shapefile when uploaded
    observeEvent(input$estuari_file, {
      req(input$estuari_input_mode == "calculate", input$estuari_file, rv$idx_serasi_map)
      tryCatch({
        showNotification("Menghitung jarak Euclidean dari shapefile estuari...", type = "message")
        estuari <- load_and_validate_shapefile(extract_shp_path(input$estuari_file))
        estuari <- ensure_geometry_name(estuari)  
        euc <- calculate_euclidean_dist(
          estuari,
          rv$idx_serasi_map,
          resolution = input$euc_resolution
        )
        out_path <- file.path(output_dir(), "estuari_euc_dist.tif")
        dir.create(output_dir(), recursive = TRUE, showWarnings = FALSE)
        terra::writeRaster(euc, out_path, overwrite = TRUE)
        rv$euc_dist_rast <- euc
        showNotification(paste("Raster jarak berhasil dihitung dan disimpan →", out_path), type = "message")
      }, error = function(e) {
        rv$euc_dist_rast <- NULL
        showNotification(paste("Gagal menghitung jarak:", e$message), type = "error")
      })
    })
    
    output$estuari_status <- renderUI({
      if (is.null(rv$euc_dist_rast)) return(NULL)
      div(class = "alert alert-success mb-0",
          tags$i(class = "bi bi-check-circle me-2"),
          "Raster jarak estuari siap digunakan.")
    })
    
    # ── Step 1 -> Step 2 ──────────────────────────────────────
    observeEvent(input$btn_next_1, {
      if (is.null(rv$idx_serasi_map) || is.null(rv$tss_rast) || is.null(rv$euc_dist_rast)) {
        showNotification("Harap lengkapi semua data utama (SERASI, TSS, dan jarak estuari) sebelum melanjutkan.",
                         type = "warning", duration = 8)
        return()
      }
      rv$unlocked <- max(rv$unlocked, 2)
      go_to_panel("step2")
    })
    
    # ── Step 2 UI ──────────────────────────────────────────────
    output$step2_ui <- renderUI({
      if (rv$unlocked < 2) return(.locked_panel())
      
      tagList(
        tags$p(tags$i(class = "bi bi-sliders me-1"), "Parameter",
               style = "font-weight: 600; margin-bottom: 4px;"),
        numericInput(ns("estuari_dist_max"),
                     "Jarak Estuari Maksimum (meter)",
                     value = 5000, min = 1),
        
        hr(),
        
        # Check output directory
        if (is.null(output_dir()) || !nzchar(output_dir())) {
          div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
              tags$i(class = "bi bi-exclamation-triangle me-1"),
              "Direktori output belum diatur. Atur terlebih dahulu di menu utama.")
        },
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_run"),
                       tagList(tags$i(class = "bi bi-play-fill me-1"),
                               "Lakukan Analisis PADU-HS"),
                       class = "btn-success btn-sm")
        ),
        
        .step_nav(ns, back_id = "btn_back_2", next_id = NULL)
      )
    })
    
    observeEvent(input$btn_back_2, {
      go_to_panel("step1")
    })
    
    # ── Run analysis (with progress) ──────────────────────────
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
      
      req(rv$idx_serasi_map, rv$tss_rast, rv$euc_dist_rast)
      
      # Reset previous results
      rv$analysis_result <- NULL
      rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL
      rv$log_messages <- ""
      
      append_log("Memulai analisis PADU-HS...")
      
      withProgress(message = "Menjalankan Analisis PADU-HS", value = 0, {
        
        tryCatch({
          incProgress(0.1, detail = "Memuat data...")
          append_log("Data berhasil dimuat.")
          
          # Step 1: Compute PADU-HS (progress 20% → 80%)
          incProgress(0.1, detail = "Mempersiapkan perhitungan...")
          append_log("Menghitung indeks PADU-HS...")
          
          # Conditional dissolve idx_serasi_map
          if ("length" %in% colnames(rv$idx_serasi_map)) {
            idx_serasi_map <- dissolve_id_pu(rv$idx_serasi_map)
          } else {
            idx_serasi_map <- rv$idx_serasi_map  
          }
          
          incProgress(0.2, detail = "Memproses jarak estuari dan TSS...")
          padu_hs <- calculate_padu_hs(
            idx_serasi_map    = idx_serasi_map,
            estuari_euc_dist  = rv$euc_dist_rast,
            tss_rast          = rv$tss_rast,
            max_dist          = input$estuari_dist_max
          )
          
          incProgress(0.3, detail = "Menggabungkan hasil...")
          idx_padu_hs_map <- padu_hs$idx_padu_hs_map  
          idx_padu_hs_table <- as_tibble(idx_padu_hs_map %>% sf::st_drop_geometry())
          append_log("Perhitungan indeks selesai.")
          
          # Step 2: Save results (progress 90%)
          incProgress(0.2, detail = "Menyimpan hasil...")
          append_log("Menyimpan hasil ke disk...")
          gpkg_path <- file.path(output_dir(), "idx_padu_hs.gpkg")
          xlsx_path <- file.path(output_dir(), "idx_padu_hs.xlsx")
          dir.create(output_dir(), recursive = TRUE, showWarnings = FALSE)
          
          sf::st_write(idx_padu_hs_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          openxlsx::write.xlsx(idx_padu_hs_table, xlsx_path)
          
          rv$gpkg_path <- gpkg_path
          rv$xlsx_path <- xlsx_path
          rv$analysis_result <- list(map = idx_padu_hs_map, table = idx_padu_hs_table)
          
          append_log(paste("Peta disimpan →", gpkg_path))
          append_log(paste("Tabel disimpan →", xlsx_path))
          append_log("Analisis PADU-HS berhasil diselesaikan.")
          
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
      } else if (rv$unlocked >= 2 && !is.null(rv$euc_dist_rast)) {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Siap menjalankan analisis.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Lengkapi langkah sebelumnya.")
      }
    })
    
    # ── Map output (leaflet) ──────────────────────────────────
    output$result_map <- renderLeaflet({
      req(rv$analysis_result)
      
      map_sf <- rv$analysis_result$map
      
      if (!sf::st_is_longlat(map_sf)) {
        map_sf <- sf::st_transform(map_sf, crs = 4326)
      }
      
      if (!"idx_padu_hs" %in% names(map_sf)) {
        return(leaflet::leaflet() %>% 
                 leaflet::addControl("Kolom idx_padu_hs tidak ditemukan.", position = "topright"))
      }
      
      pal <- leaflet::colorNumeric(
        palette = "RdYlGn",
        domain  = map_sf$idx_padu_hs,
        na.color = "grey"
      )
      
      leaflet::leaflet(map_sf) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::addPolygons(
          fillColor   = ~pal(idx_padu_hs),
          fillOpacity = 0.7,
          weight      = 1,
          color       = "black",
          label       = ~paste0(
            "<strong>Indeks PADU-HS:</strong> ", round(idx_padu_hs, 3)
          ) %>% lapply(htmltools::HTML),
          popup       = ~paste(
            "<b>ID PU:</b>", id_pu, "<br>",
            "<b>Indeks PADU-HS:</b>", round(idx_padu_hs, 3)
          ) %>% lapply(htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(
            weight = 3,
            color  = "red",
            fillOpacity = 0.9
          )
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          pal      = pal,
          values   = ~idx_padu_hs,
          title    = "Indeks PADU-HS",
          opacity  = 0.7
        )
    })
    
    # ── Table output ───────────────────────────────────────────
    output$result_table <- DT::renderDT({
      req(rv$analysis_result)
      DT::datatable(
        rv$analysis_result$table,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'Bfrtip'
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      )
    })
    
    # ── Validation log ─────────────────────────────────────────
    output$validation_log <- renderPrint({
      invalidateLater(100, session)
      cat(rv$log_messages)
    })
    
    # ── Download handlers ──────────────────────────────────────
    output$dl_gpkg <- downloadHandler(
      filename = function() "idx_padu_hs.gpkg",
      content = function(file) {
        req(rv$gpkg_path)
        file.copy(rv$gpkg_path, file, overwrite = TRUE)
      }
    )
    
    output$dl_xlsx <- downloadHandler(
      filename = function() "idx_padu_hs.xlsx",
      content = function(file) {
        req(rv$xlsx_path)
        file.copy(rv$xlsx_path, file, overwrite = TRUE)
      }
    )
    
  })
}