# ui/modules/mod_padu_ki.R
# ============================================================
#  MODULE: PADU-KI (2.7 PADU-KI: Disaster Risk Analysis)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
padu_ki_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    div(
      style = "margin-bottom: 20px;",
      h4("2.7 PADU-KI (Ketahanan Iklim)", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menilai kepaduan lingkungan berdasarkan ketahanan iklim untuk menghasilkan nilai indeks PADU-KI.",
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
        
        tags$p(tags$i(class = "bi bi-exclamation-triangle me-1"),
               "Peta Risiko Bencana (.shp)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Unggah vektor Risiko Bencana."
        ),
        fileInput(ns("disaster_risk_file"),
                  label    = NULL,
                  accept   = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        
        hr(),
        
        textInput(ns("risk_col_name"), "Nama Kolom Atribut Risiko", value = "Kerawanan"),
        
        hr(),
        
        # ── Pengaturan lanjutan (collapsible, default tertutup) ──
        accordion(
          accordion_panel(
            title = "Pengaturan lanjutan",
            icon = icon("gear"),
            open = FALSE,   # default collapsed
            checkboxInput(
              ns("parallel"),
              "Aktifkan pemrosesan paralel",
              value = FALSE
            ),
            numericInput(
              ns("workers"),
              "Jumlah pekerja (cores)",
              value = 2,
              min = 1,
              step = 1
            )
          )
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
padu_ki_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    
    analysis_result <- reactiveVal(NULL)
    log_messages    <- reactiveVal("")   # log real-time
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
    
    disaster_risk_vect <- reactive({
      req(input$disaster_risk_file)
      load_and_validate_shapefile(extract_shp_path(input$disaster_risk_file))
    })
    
    # ── Run analysis with progress bar ──────────────────────
    observeEvent(input$btn_run, {
      req(!is_running(), input$idx_serasi_file, input$disaster_risk_file)
      
      is_running(TRUE)
      analysis_result(NULL)
      log_messages("")   # reset log
      
      withProgress(message = "Menjalankan Analisis PADU-KI", value = 0, {
        
        tryCatch({
          # Step 1: Load data (progress 10%)
          incProgress(0.1, detail = "Memuat data...")
          append_log("Memulai analisis PADU-KI...")
          
          idx_map <- idx_serasi_map()
          dr_vect <- disaster_risk_vect()
          risk_col <- input$risk_col_name
          append_log("Data berhasil dimuat.")
          append_log(paste("Kolom risiko yang digunakan:", risk_col))
          
          # Step 2: Calculate PADU-KI (progress 20% → 80%)
          incProgress(0.1, detail = "Mempersiapkan perhitungan...")
          append_log("Menghitung indeks PADU-KI...")
          
          padu_ki <- calculate_padu_ki(
            idx_serasi_map      = idx_map,
            disaster_risk_vect  = dr_vect,
            value_col           = risk_col,
            parallel            = input$parallel,
            workers             = input$workers
          )
          
          incProgress(0.6, detail = "Perhitungan selesai...")
          append_log("Perhitungan indeks selesai.")
          
          idx_padu_ki_map <- padu_ki$idx_padu_ki_map
          
          # Step 3: Save results (progress 90%)
          incProgress(0.1, detail = "Menyimpan hasil...")
          append_log("Menyimpan hasil ke disk...")
          out_path <- file.path(output_dir(), "idx_padu_ki.gpkg")
          sf::st_write(idx_padu_ki_map, out_path, delete_dsn = TRUE, quiet = TRUE)
          append_log(paste("Peta disimpan →", out_path))
          
          idx_padu_ki_table <- as_tibble(sf::st_drop_geometry(idx_padu_ki_map))
          analysis_result(list(map = idx_padu_ki_map, table = idx_padu_ki_table))
          append_log("Analisis PADU-KI berhasil diselesaikan.")
          
          incProgress(0.1, detail = "Selesai!")
          showNotification("Berhasil: Perhitungan PADU-KI selesai.",
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
      plot(analysis_result()$map["idx_padu_ki"], main = "Peta Indeks PADU-KI (Ketahanan Bencana)")
    })
    
    # ── Table output ─────────────────────────────────────────
    output$result_table <- renderTable({
      req(analysis_result())
      head(analysis_result()$table, 100)
    })
    
    # ── Validation log (real-time) ──────────────────────────
    output$validation_log <- renderPrint({
      invalidateLater(100, session)   # perbarui setiap 100ms
      cat(log_messages())
    })
    
  })
}