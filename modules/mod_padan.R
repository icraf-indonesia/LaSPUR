# ui/modules/mod_padan.R
# ============================================================
#  MODULE: PADAN Analysis (3. PADAN)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── UI ──────────────────────────────────────────────────────────
padan_ui <- function(id) {
  ns <- NS(id)
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
      
      # ── Left column: Input & Parameter (1/3) ────────────────
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
      
      # ── Right column: Output & Hasil (2/3) ──────────────────
      column(
        width = 8,
        card(
          card_header("Output & Hasil"),
          
          uiOutput(ns("status_box")),
          
          hr(),
          
          create_result_ui(ns)
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────────
padan_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Reactive values ──────────────────────────────────────────
    rv <- reactiveValues(
      analysis_result = NULL,
      gpkg_path = NULL,
      xlsx_path = NULL,
      log_messages = ""
    )
    
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
      
      req(input$idx_padu_file)
      
      # Reset previous results
      rv$analysis_result <- NULL
      rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL
      rv$log_messages <- ""
      
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
          rv$analysis_result <- list(map = idx_padan_map, table = res_table)
          
          # ─── Store result for report generation ───
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
            map      = idx_padan_map,
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
        
      }) # end withProgress
    })
    
    # ── Status box ─────────────────────────────────────────────
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
    
    # ── Result Visualization ───────────────────────────────────
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