# ui/modules/mod_adjacent.R
# ============================================================
#  MODULE: Adjacent (1.2 Type 2: Adjacent)
# ============================================================

source("../R/functions.R")
source("../R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
adjacent_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    div(
      style = "margin-bottom: 20px;",
      h4("2.1 Identifikasi Area Bertetangga", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Mengidentifikasi area bertetangga secara spasial antara kawasan/zona RTRW dan RZWP3K serta menghitung indeks SERASI.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),
    
    layout_column_wrap(
      width = 1/2,
      
      # ── Card A: Input & Parameter ────────────────────────
      card(
        card_header("Input & Parameter"),
        
        tags$p(tags$i(class = "bi bi-map me-1"),
               "Peta RTRW (.shp)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Unggah semua komponen shapefile RTRW (.shp, .dbf, .prj, .shx)."
        ),
        fileInput(ns("rtrw_file"),
                  label    = NULL,
                  accept   = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        
        tags$p(tags$i(class = "bi bi-map me-1"),
               "Peta RZWP3K (.shp)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Unggah semua komponen shapefile RZWP3K (.shp, .dbf, .prj, .shx)."
        ),
        fileInput(ns("rzwp3k_file"),
                  label    = NULL,
                  accept   = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-table me-1"),
               "Tabel Prioritas RTRW (.xlsx)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        fileInput(ns("rtrw_prioritas_file"),
                  label  = NULL,
                  accept = ".xlsx"),
        
        tags$p(tags$i(class = "bi bi-table me-1"),
               "Tabel Prioritas RZWP3K (.xlsx)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        fileInput(ns("rzwp3k_prioritas_file"),
                  label  = NULL,
                  accept = ".xlsx"),
        
        tags$p(tags$i(class = "bi bi-grid-3x3 me-1"),
               "Tabel Matriks SERASI (.xlsx)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        fileInput(ns("matriks_serasi_file"),
                  label  = NULL,
                  accept = ".xlsx"),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-sliders me-1"),
               "Parameter",
               style = "font-weight: 600; margin-bottom: 4px;"),
        numericInput(ns("threshold_ha"),
                     "Ambang Batas Luas Minimum (ha)",
                     value = 156.25, min = 0),
        
        hr(),
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_generate_matrix"),
                       tagList(tags$i(class = "bi bi-file-earmark-excel me-1"),
                               "Buat Template Matriks"),
                       class = "btn-outline-primary btn-sm"),
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
adjacent_server <- function(id, output_dir) {
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
    
    # ── Reactives ────────────────────────────────────────────
    rtrw_vect <- reactive({
      req(input$rtrw_file)
      load_and_validate_shapefile(extract_shp_path(input$rtrw_file))
    })
    
    rzwp3k_vect <- reactive({
      req(input$rzwp3k_file)
      load_and_validate_shapefile(extract_shp_path(input$rzwp3k_file))
    })
    
    # ── Generate matrix template ─────────────────────────────
    observeEvent(input$btn_generate_matrix, {
      req(rtrw_vect(), rzwp3k_vect())
      tryCatch({
        template <- generate_matrix_serasi(sf_1 = rtrw_vect(), sf_2 = rzwp3k_vect())
        out_path <- file.path(output_dir(), "matriks_serasi.xlsx")
        write.xlsx(template, out_path, overwrite = TRUE)
        showNotification(paste("Template matriks dibuat →", out_path),
                         type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Gagal membuat template matriks:", e$message),
                         type = "error", duration = 8)
      })
    })
    
    # ── Run analysis with progress bar ──────────────────────
    observeEvent(input$btn_run, {
      req(!is_running())
      req(input$rtrw_file, input$rzwp3k_file,
          input$rtrw_prioritas_file,
          input$rzwp3k_prioritas_file,
          input$matriks_serasi_file)
      
      is_running(TRUE)
      analysis_result(NULL)
      log_messages("")   # reset log
      
      # Fungsi untuk menambahkan pesan ke log
      append_log <- function(msg) {
        current <- log_messages()
        log_messages(paste0(current, msg, "\n"))
      }
      
      # Bungkus seluruh proses dengan progress bar
      withProgress(message = "Menjalankan Analisis Bertetangga", value = 0, {
        
        tryCatch({
          # Step 1: Load matrices (progress 10%)
          incProgress(0.1, detail = "Memuat matriks dan prioritas...")
          append_log(">> Memuat matriks SERASI...")
          matriks_serasi   <- load_validate_matrix_table(
            input$matriks_serasi_file$datapath, title = "serasi"
          )
          append_log(">> Memuat prioritas RTRW...")
          rtrw_prioritas   <- load_and_validate_table(input$rtrw_prioritas_file$datapath)
          append_log(">> Memuat prioritas RZWP3K...")
          rzwp3k_prioritas <- load_and_validate_table(input$rzwp3k_prioritas_file$datapath)
          append_log("   Semua tabel berhasil dimuat.")
          
          # Step 2: Identify adjacent (progress 30%)
          incProgress(0.2, detail = "Mengidentifikasi area bertetangga...")
          append_log(">> Mengidentifikasi area bertetangga antara RTRW dan RZWP3K...")
          adjacent_map_raw <- identify_adjacent(
            rtrw = rtrw_vect(),
            rzwp = rzwp3k_vect(),
            min_area_ha = 0
          )
          append_log("   Area bertetangga berhasil diidentifikasi.")
          
          # Step 3: Process adjacent (progress 50%)
          incProgress(0.2, detail = "Memproses area bertetangga...")
          append_log(">> Memproses area bertetangga dengan buffer 100 m...")
          adjacent_map <- process_adjacent(
            pu_sf = adjacent_map_raw,
            buffer_m = 100
          )
          append_log("   Pemrosesan selesai.")
          
          # Step 4: Validate zone class (progress 70%)
          incProgress(0.2, detail = "Memvalidasi kesesuaian kelas zona...")
          append_log(">> Memvalidasi kesesuaian nama kelas antara peta dan prioritas...")
          valid_class <- validate_zone_class(adjacent_map, rtrw_prioritas, rzwp3k_prioritas)
          
          # Step 5: Merge and save (progress 90%)
          incProgress(0.2, detail = "Menggabungkan dan menyimpan hasil...")
          if (length(valid_class$mismatch_col3) == 0 &&
              length(valid_class$mismatch_col4) == 0) {
            
            append_log("   Semua nama kelas cocok. Menggabungkan indeks SERASI...")
            idx_serasi_map <- merge_attributes_to_map(adjacent_map, matriks_serasi) 
            idx_serasi_table <- as_tibble(idx_serasi_map %>% sf::st_drop_geometry())
            
            out_path <- file.path(output_dir(), "idx_serasi_adjacent.gpkg")
            sf::st_write(idx_serasi_map, out_path, delete_dsn = TRUE, quiet = TRUE)
            append_log(paste0("   Hasil disimpan di: ", out_path))
            
            analysis_result(list(map = idx_serasi_map, table = idx_serasi_table))
            append_log("Analisis bertetangga berhasil diselesaikan.")
            showNotification(paste("Analisis selesai. Hasil disimpan ke", out_path),
                             type = "message", duration = 5)
            
          } else {
            
            log_msg <- paste(
              "Ketidakcocokan nama kelas terdeteksi:",
              if (length(valid_class$mismatch_col3) > 0)
                paste("  Ketidakcocokan col3:",
                      paste(valid_class$mismatch_col3, collapse = ", ")),
              if (length(valid_class$mismatch_col4) > 0)
                paste("  Ketidakcocokan col4:",
                      paste(valid_class$mismatch_col4, collapse = ", ")),
              sep = "\n"
            )
            append_log(log_msg)
            showNotification("Ketidakcocokan terdeteksi. Periksa tab Log Validasi.",
                             type = "warning", duration = 8)
          }
          
          incProgress(0.1, detail = "Selesai!")
          
        }, error = function(e) {
          msg <- conditionMessage(e)
          if (is.null(msg) || msg == "") msg <- "Error tidak diketahui (lihat konsol untuk detail)"
          append_log(paste0("❌ ERROR: ", msg))
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
      plot(analysis_result()$map["idx_serasi"], main = "Peta Indeks SERASI")
    })
    
    # ── Table output ─────────────────────────────────────────
    output$result_table <- renderTable({
      req(analysis_result())
      analysis_result()$table
    })
    
    # ── Validation log (real-time) ──────────────────────────
    output$validation_log <- renderPrint({
      invalidateLater(100, session)   # perbarui setiap 100ms
      cat(log_messages())
    })
    
  })
}