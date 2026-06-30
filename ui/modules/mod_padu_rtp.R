# ui/modules/mod_padu_rtp.R
# ============================================================
#  MODULE: PADU-RTp (2.5 PADU-RTp)
# ============================================================

source("../R/functions.R")
source("../R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
padu_rtp_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    div(
      style = "margin-bottom: 20px;",
      h4("2.5 PADU-RTp (Risiko dan Tekanan)", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menilai kepaduan lingkungan berdasarkan potensi risiko dan tekanan untuk menghasilkan nilai indeks PADU-RTp.",
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
        
        tags$p(tags$i(class = "bi bi-building me-1"),
               "1. Peta Jarak ke Industri",
               style = "font-weight: 600; margin-bottom: 4px;"),
        radioButtons(ns("ind_input_type"), label = NULL,
                     choices = c("Unggah Vektor (hitung jarak otomatis)" = "vector",
                                 "Unggah Raster Jarak yang Sudah Ada (.tif)" = "raster"),
                     inline = FALSE),
        uiOutput(ns("ui_ind_file")),
        numericInput(ns("max_ind_dist"), "Skala Jarak Maksimum Industri (m)", value = 8000, min = 1),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-water me-1"),
               "2. Peta Jarak ke Alur Pelayaran",
               style = "font-weight: 600; margin-bottom: 4px;"),
        radioButtons(ns("pel_input_type"), label = NULL,
                     choices = c("Unggah Vektor (hitung jarak otomatis)" = "vector",
                                 "Unggah Raster Jarak yang Sudah Ada (.tif)" = "raster"),
                     inline = FALSE),
        uiOutput(ns("ui_pel_file")),
        numericInput(ns("max_pel_dist"), "Skala Jarak Maksimum Alur Pelayaran (m)", value = 5000, min = 1),
        
        hr(),
        
        numericInput(ns("calc_resolution"), "Resolusi Perhitungan Jarak Otomatis (m)", value = 30, min = 1),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Hanya digunakan jika opsi 'Unggah Vektor' dipilih di atas."
        ),
        
        hr(),
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_run"),
                       tagList(tags$i(class = "bi bi-play-fill me-1"),
                               "Jalankan Analisis"),
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
            "Log Validasi",
            div(
              style = "max-height: 300px; overflow-y: auto; background-color: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 0.9rem; white-space: pre-wrap;",
              verbatimTextOutput(ns("validation_log"))
            )
          )
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────
padu_rtp_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    
    analysis_result <- reactiveVal(NULL)
    log_messages    <- reactiveVal("")  
    is_running      <- reactiveVal(FALSE)
    
    # ── Dynamic UI for File Inputs ────────────────────────────
    output$ui_ind_file <- renderUI({
      ns <- session$ns
      if (input$ind_input_type == "vector") {
        fileInput(ns("ind_file_vect"), "Shapefile Industri",
                  accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"), multiple = TRUE)
      } else {
        fileInput(ns("ind_file_rast"), "Raster Industri (.tif)",
                  accept = c(".tif"), multiple = FALSE)
      }
    })
    
    output$ui_pel_file <- renderUI({
      ns <- session$ns
      if (input$pel_input_type == "vector") {
        fileInput(ns("pel_file_vect"), "Shapefile Alur Pelayaran",
                  accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"), multiple = TRUE)
      } else {
        fileInput(ns("pel_file_rast"), "Raster Alur Pelayaran (.tif)",
                  accept = c(".tif"), multiple = FALSE)
      }
    })
    
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
      load_and_validate_shapefile(extract_vector_path(input$idx_serasi_file))
    })
    
    # ── Run analysis with progress bar ──────────────────────
    observeEvent(input$btn_run, {
      req(!is_running())
      req(input$idx_serasi_file)
      
      if (input$ind_input_type == "vector") req(input$ind_file_vect) else req(input$ind_file_rast)
      if (input$pel_input_type == "vector") req(input$pel_file_vect) else req(input$pel_file_rast)
      
      is_running(TRUE)
      analysis_result(NULL)
      log_messages("")   # reset log
      
      withProgress(message = "Menjalankan Analisis PADU-RTp", value = 0, {
        
        tryCatch({
          # Step 1: Load SERASI map (progress 10%)
          incProgress(0.1, detail = "Memuat peta SERASI...")
          append_log("Memulai analisis PADU-RTp...")
          idx_map <- idx_serasi_map()
          append_log("Peta SERASI berhasil dimuat.")
          
          # Step 2: Process industry data (progress 20% → 45%)
          incProgress(0.1, detail = "Memproses data industri...")
          append_log("Memproses data industri...")
          if (input$ind_input_type == "vector") {
            ind_vect <- load_and_validate_shapefile(extract_shp_path(input$ind_file_vect))
            append_log("  Menghitung jarak Euclidean dari vektor industri...")
            industry_euc_dist <- calculate_euclidean_dist(
              ind_vect,
              idx_map,
              resolution = input$calc_resolution
            )
          } else {
            append_log("  Memuat raster jarak industri yang diunggah...")
            industry_euc_dist <- terra::rast(input$ind_file_rast$datapath)
          }
          incProgress(0.25, detail = "Data industri siap.")
          
          # Step 3: Process shipping lane data (progress 45% → 70%)
          incProgress(0.1, detail = "Memproses data alur pelayaran...")
          append_log("Memproses data alur pelayaran...")
          if (input$pel_input_type == "vector") {
            pel_vect <- load_and_validate_shapefile(extract_shp_path(input$pel_file_vect))
            append_log("  Menghitung jarak Euclidean dari vektor alur pelayaran...")
            pelayaran_euc_dist <- calculate_euclidean_dist(
              pel_vect,
              idx_map,
              resolution = input$calc_resolution
            )
          } else {
            append_log("  Memuat raster jarak alur pelayaran yang diunggah...")
            pelayaran_euc_dist <- terra::rast(input$pel_file_rast$datapath)
          }
          incProgress(0.25, detail = "Data alur pelayaran siap.")
          
          # Step 4: Calculate PADU-RTp (progress 70% → 90%)
          incProgress(0.1, detail = "Menghitung indeks PADU-RTp...")
          append_log("Menghitung indeks PADU-RTp...")
          padu_rtp <- calculate_padu_rtp(
            idx_serasi_map      = idx_map,
            industry_euc_dist   = industry_euc_dist,
            pelayaran_euc_dist  = pelayaran_euc_dist
          )
          
          idx_padu_rtp_map <- padu_rtp$idx_padu_rtp_map 
          append_log("Perhitungan indeks selesai.")
          incProgress(0.2, detail = "Indeks berhasil dihitung.")
          
          # Step 5: Save results (progress 90% → 100%)
          incProgress(0.1, detail = "Menyimpan hasil...")
          append_log("Menyimpan hasil ke disk...")
          out_gpkg <- file.path(output_dir(), "idx_padu_rtp.gpkg")
          sf::st_write(idx_padu_rtp_map, out_gpkg, delete_dsn = TRUE, quiet = TRUE)
          append_log(paste("Peta disimpan →", out_gpkg))
          
          result_table <- dplyr::as_tibble(sf::st_drop_geometry(idx_padu_rtp_map))
          analysis_result(list(map = idx_padu_rtp_map, table = result_table))
          append_log("Analisis PADU-RTp berhasil diselesaikan.")
          
          incProgress(0.1, detail = "Selesai!")
          showNotification("Analisis selesai! Periksa tab Peta dan Tabel.",
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
      plot(analysis_result()$map["idx_padu_rtp"], main = "Peta Indeks PADU-RTp")
    })
    
    # ── Table output ─────────────────────────────────────────
    output$result_table <- renderTable({
      req(analysis_result())
      head(analysis_result()$table, 50)
    })
    
    # ── Validation log (real-time) ──────────────────────────
    output$validation_log <- renderPrint({
      invalidateLater(100, session)   # perbarui setiap 100ms
      cat(log_messages())
    })
    
  })
}