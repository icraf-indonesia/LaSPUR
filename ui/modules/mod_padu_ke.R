# ui/modules/mod_padu_ke.R
# ============================================================
#  MODULE: PADU-KE (2.1 PADU-KE)
# ============================================================

source("../R/functions.R")
source("../R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
padu_ke_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    div(
      style = "margin-bottom: 20px;",
      h4("2.1 PADU-KE (Konektivitas Ekologis)", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menilai kepaduan lingkungan berdasarkan konektivitas ekologis untuk menghasilkan nilai indeks PADU-KE.",
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
        
        tags$p(tags$i(class = "bi bi-map me-1"),
               "Peta Tutupan/Penggunaan Lahan (.shp)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Layer poligon dengan kelas tutupan lahan."
        ),
        fileInput(ns("lulc_file"),
                  label    = NULL,
                  accept   = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-table me-1"),
               "Tabel Matriks PADU-KE (.xlsx)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Kolom yang diperlukan: class1, class2, adj_index."
        ),
        fileInput(ns("matriks_padu_ke_file"),
                  label  = NULL,
                  accept = ".xlsx"),
        
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
padu_ke_server <- function(id, output_dir) {
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
    
    # ── Reactives ────────────────────────────────────────────
    idx_serasi_map <- reactive({
      req(input$idx_serasi_file)
      path <- extract_vector_path(input$idx_serasi_file)
      load_and_validate_shapefile(path)
    })
    
    lulc_vect <- reactive({
      req(input$lulc_file)
      load_and_validate_shapefile(extract_shp_path(input$lulc_file))
    })
    
    lulc_ref <- reactive({
      lulc_vect() %>%
        sf::st_drop_geometry() %>%
        dplyr::distinct(ID, LC) %>%
        dplyr::arrange(ID)
    })
    
    # ── Generate matrix template ─────────────────────────────
    observeEvent(input$btn_generate_matrix, {
      tryCatch({
        template <- generate_matrix_padu_ke(lulc_ref())
        out_path <- file.path(output_dir(), "matriks_padu_ke_template.xlsx")
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
      req(input$idx_serasi_file, input$lulc_file, input$matriks_padu_ke_file)
      
      is_running(TRUE)
      analysis_result(NULL)
      log_messages("")   # reset log
      
      # Fungsi untuk menambahkan pesan ke log
      append_log <- function(msg) {
        current <- log_messages()
        log_messages(paste0(current, msg, "\n"))
      }
      
      # Bungkus seluruh proses dengan progress bar
      withProgress(message = "Menjalankan Analisis PADU-KE", value = 0, {
        
        tryCatch({
          # Step 1: Load matrix (progress 10%)
          incProgress(0.1, detail = "Memuat matriks PADU-KE...")
          append_log(">> Memuat matriks PADU-KE...")
          matriks_padu_ke <- load_validate_matrix_table(input$matriks_padu_ke_file$datapath, title = "padu_ke")
          append_log("   Matriks berhasil dimuat.")
          
          # Step 2: Load maps (progress 25%)
          incProgress(0.15, detail = "Memuat peta...")
          append_log(">> Memuat peta SERASI...")
          idx_map        <- idx_serasi_map()
          append_log(">> Memuat peta tutupan lahan...")
          lulc_vect_data <- lulc_vect()
          class_col      <- intersect(c("ID", "Class", "class", "LULC", "Kelas"), names(lulc_vect_data))[1]
          append_log(paste0("   Kolom kelas: ", class_col))
          
          # Step 3: Calculate adjacency (progress 30% → 70%)
          incProgress(0.05, detail = "Menghitung ketetanggaan...")
          append_log(">> Menghitung ketetanggaan tutupan lahan...")
          
          lulc_adjacencies <- withCallingHandlers(
            calculate_lulc_adjacency(
              lulc         = lulc_vect_data,
              admin_vector = idx_map,
              id_col       = "id_pu",
              class_col    = class_col,
              parallel     = input$parallel,
              workers      = input$workers
            ),
            message = function(m) append_log(paste0("   [INFO] ", m$message)),
            warning = function(w) append_log(paste0("   [WARN] ", w$message))
          )
          
          incProgress(0.4, detail = "Ketetanggaan selesai, memproses hasil...")
          append_log("   Ketetanggaan selesai.")
          
          # Step 4: Calculate PADU-KE (progress 80%)
          incProgress(0.1, detail = "Menghitung indeks PADU-KE...")
          append_log(">> Menghitung indeks PADU-KE...")
          padu_ke <- calculate_padu_ke(
            matriks_padu_ke   = matriks_padu_ke,
            lulc_ref          = lulc_ref(),
            lulc_adjacencies  = lulc_adjacencies,
            idx_serasi_map    = idx_map,
            normalize         = TRUE
          )
          
          idx_padu_ke <- padu_ke$idx_padu_ke %>% select(-idx_padu_ke_abs)
          idx_padu_ke_map <- padu_ke$idx_padu_ke_map
          
          # Step 5: Save results (progress 90%)
          incProgress(0.1, detail = "Menyimpan hasil...")
          append_log(">> Menyimpan hasil ke disk...")
          out_path <- file.path(output_dir(), "idx_padu_ke.gpkg")
          sf::st_write(idx_padu_ke_map, out_path, delete_dsn = TRUE, quiet = TRUE)
          append_log(paste0("   Hasil disimpan di: ", out_path))
          
          result_table <- as_tibble(idx_padu_ke_map %>% sf::st_drop_geometry())
          analysis_result(list(map = idx_padu_ke_map, table = result_table))
          append_log("Analisis PADU-KE berhasil diselesaikan.")
          
          # Progress selesai
          incProgress(0.1, detail = "Selesai!")
          showNotification("Analisis selesai! Periksa tab Peta dan Tabel.",
                           type = "message", duration = 5)
          
        }, error = function(e) {
          msg <- conditionMessage(e)
          if (is.null(msg) || msg == "") msg <- "Error tidak diketahui (lihat konsol untuk detail)"
          append_log(paste0("ERROR: ", msg))
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
      plot(analysis_result()$map["idx_padu_ke"], main = "Peta Indeks PADU-KE")
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