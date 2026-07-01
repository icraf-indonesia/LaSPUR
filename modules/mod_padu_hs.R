# ui/modules/mod_padu_hs.R
# ============================================================
#  MODULE: PADU-HS (2.2)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
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
    
    layout_column_wrap(
      width = 1/2,
      
      # ── Card A: Input & Parameter ────────────────────────
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
        
        tags$p(tags$i(class = "bi bi-layers me-1"),
               "Peta Total Suspended Solid (TSS) (.tif)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "File raster nilai TSS."
        ),
        fileInput(ns("tss_file"),
                  label  = NULL,
                  accept = c(".tif", ".tiff")),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-geo-alt me-1"),
               "Input Peta Jarak Estuari",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Unggah raster jarak yang sudah dihitung, atau unggah shapefile estuari untuk dihitung otomatis."
        ),
        
        radioButtons(
          ns("estuari_input_mode"),
          label   = NULL,
          choices = c(
            "Unggah raster jarak yang sudah ada (.tif)" = "upload_raster",
            "Unggah shapefile estuari dan hitung otomatis" = "calculate"
          ),
          selected = "upload_raster"
        ),
        
        # Mode A: unggah raster langsung
        conditionalPanel(
          condition = sprintf("input['%s'] == 'upload_raster'", ns("estuari_input_mode")),
          fileInput(ns("euc_dist_file"),
                    label  = "Raster Jarak Estuari (.tif)",
                    accept = c(".tif", ".tiff"))
        ),
        
        # Mode B: unggah shapefile → hitung
        conditionalPanel(
          condition = sprintf("input['%s'] == 'calculate'", ns("estuari_input_mode")),
          fileInput(ns("estuari_file"),
                    label    = "Shapefile Estuari",
                    accept   = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                    multiple = TRUE),
          numericInput(ns("euc_resolution"),
                       "Resolusi Perhitungan (meter)",
                       value = 30, min = 1),
          div(
            class = "alert alert-warning py-2 px-3",
            style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-exclamation-triangle me-1"),
            "Jarak Euclidean akan dihitung dari shapefile di atas. Proses ini dapat memakan beberapa menit untuk dataset besar. Hasilnya akan otomatis disimpan ke direktori output."
          )
        ),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-sliders me-1"),
               "Parameter",
               style = "font-weight: 600; margin-bottom: 4px;"),
        numericInput(ns("estuari_dist_max"),
                     "Jarak Estuari Maksimum (meter)",
                     value = 5000, min = 1),
        
        hr(),
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_run"),
                       tagList(tags$i(class = "bi bi-play-fill me-1"), "Jalankan Analisis"),
                       class = "btn-success btn-sm")
        )
      ),
      
      # ── Card B: Output & Hasil ───────────────────────────
      card(
        card_header("Output & Hasil"),
        
        uiOutput(ns("status_box")),
        
        hr(),
        
        navset_tab(
          nav_panel(
            "Peta",
            plotOutput(ns("result_map"), height = "300px")
          ),
          nav_panel(
            "Tabel",
            div(
              style = "overflow-x: auto; max-height: 300px; overflow-y: auto;",
              tableOutput(ns("result_table"))
            )
          ),
          nav_panel(
            "Log",
            div(
              style = "max-height: 300px; overflow-y: auto; background-color: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 0.9rem; white-space: pre-wrap;",
              verbatimTextOutput(ns("run_log"))
            )
          )
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────
padu_hs_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    
    analysis_result <- reactiveVal(NULL)
    log_messages    <- reactiveVal("")  
    is_running      <- reactiveVal(FALSE)
    
    # ── Rename sidecar files and return .shp path ───
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
    
    # ── Extract .gpkg or .shp path from upload ──────
    extract_vector_path <- function(file_input) {
      gpkg_row <- file_input[grepl("\\.gpkg$", file_input$name, ignore.case = TRUE), ]
      if (nrow(gpkg_row) == 1) return(gpkg_row$datapath)
      extract_shp_path(file_input)
    }
    
    # ── Log helper ───────────────────────────────────────────
    append_log <- function(msg) {
      current <- log_messages()
      log_messages(paste0(current, format(Sys.time(), "[%H:%M:%S] "), msg, "\n"))
    }
    
    # ── Reactives ────────────────────────────────────────────
    idx_serasi_map <- reactive({
      req(input$idx_serasi_file)
      path <- extract_vector_path(input$idx_serasi_file)
      load_and_validate_shapefile(path)
    })
    
    tss_rast <- reactive({
      req(input$tss_file)
      load_and_validate_raster(input$tss_file$datapath)
    })
    
    estuari_vect <- reactive({
      req(input$estuari_input_mode == "calculate")
      req(input$estuari_file)
      load_and_validate_shapefile(extract_shp_path(input$estuari_file))
    })
    
    euc_dist_rast <- reactive({
      if (input$estuari_input_mode == "upload_raster") {
        req(input$euc_dist_file)
        append_log("Memuat raster jarak estuari yang diunggah...")
        terra::rast(input$euc_dist_file$datapath)
      } else {
        req(estuari_vect(), idx_serasi_map())
        append_log("Menghitung jarak Euclidean dari shapefile estuari...")
        euc <- calculate_euclidean_dist(
          estuari_vect(),
          idx_serasi_map(),
          resolution = input$euc_resolution
        )
        out_path <- file.path(output_dir(), "estuari_euc_dist.tif")
        terra::writeRaster(euc, out_path, overwrite = TRUE)
        append_log(paste("Raster jarak disimpan otomatis →", out_path))
        euc
      }
    })
    
    # ── Run analysis with progress bar ──────────────────────
    observeEvent(input$btn_run, {
      req(!is_running())
      req(input$idx_serasi_file, input$tss_file)
      
      if (input$estuari_input_mode == "upload_raster") {
        req(input$euc_dist_file)
      } else {
        req(input$estuari_file)
      }
      
      is_running(TRUE)
      analysis_result(NULL)
      log_messages("")   # reset log
      
      withProgress(message = "Menjalankan Analisis PADU-HS", value = 0, {
        
        tryCatch({
          # Step 1: Load data (progress 10%)
          incProgress(0.1, detail = "Memuat data...")
          append_log("Memulai analisis PADU-HS...")
          
          idx_map <- idx_serasi_map()
          tss <- tss_rast()
          euc <- euc_dist_rast()
          append_log("Data berhasil dimuat.")
          
          # Step 2: Proses perhitungan PADU-HS (progress 20% → 80%)
          incProgress(0.1, detail = "Mempersiapkan perhitungan...")
          append_log("Menghitung indeks PADU-HS...")
      
          incProgress(0.2, detail = "Memproses jarak estuari dan TSS...")
          padu_hs <- calculate_padu_hs(
            idx_serasi_map    = idx_map,
            estuari_euc_dist  = euc,
            tss_rast          = tss
          )
          incProgress(0.3, detail = "Menggabungkan hasil...")
          
          idx_padu_hs_map <- padu_hs$idx_padu_hs_map  
          idx_padu_hs_table <- as_tibble(idx_padu_hs_map %>% sf::st_drop_geometry())
          append_log("Perhitungan indeks selesai.")
          
          # Step 3: Save results (progress 90%)
          incProgress(0.2, detail = "Menyimpan hasil...")
          append_log("Menyimpan hasil ke disk...")
          out_map   <- file.path(output_dir(), "idx_padu_hs.gpkg")
          out_table <- file.path(output_dir(), "idx_padu_hs.csv")
          sf::st_write(idx_padu_hs_map, out_map, delete_dsn = TRUE, quiet = TRUE)
          write.csv(idx_padu_hs_table, out_table, row.names = FALSE)
          append_log(paste("Peta disimpan →", out_map))
          append_log(paste("Tabel disimpan →", out_table))
          
          analysis_result(list(map = idx_padu_hs_map, table = idx_padu_hs_table))
          append_log("Analisis PADU-HS berhasil diselesaikan.")
          
          incProgress(0.1, detail = "Selesai!")
          showNotification("Analisis PADU-HS selesai. Hasil disimpan ke direktori output.",
                           type = "message", duration = 5)
          
        }, error = function(e) {
          msg <- conditionMessage(e)
          if (is.null(msg) || msg == "") msg <- "Error tidak diketahui (lihat konsol untuk detail)"
          append_log(paste("ERROR:", msg))
          showNotification(paste("Analisis gagal:", msg), type = "error", duration = 10)
        })
        
      }) # end withProgress
      
      is_running(FALSE)
    })
    
    # ── Status box ───────────────────────────────────────────
    output$status_box <- renderUI({
      if (is_running()) {
        div(class = "alert alert-info mb-0",
            tags$i(class = "bi bi-hourglass-split me-2"),
            "Menjalankan analisis...")
      } else if (!is.null(analysis_result())) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Analisis selesai.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Siap. Unggah file dan klik Jalankan Analisis.")
      }
    })
    
    # ── Map output ───────────────────────────────────────────
    output$result_map <- renderPlot({
      req(analysis_result())
      plot(analysis_result()$map["idx_padu_hs"], main = "Peta Indeks PADU-HS")
    })
    
    # ── Table output ─────────────────────────────────────────
    output$result_table <- renderTable({
      req(analysis_result())
      analysis_result()$table
    })
    
    # ── Log output (real-time) ──────────────────────────────
    output$run_log <- renderPrint({
      invalidateLater(100, session)   # perbarui setiap 100ms
      cat(log_messages())
    })
    
  })
}