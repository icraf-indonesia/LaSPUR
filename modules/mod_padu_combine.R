# ui/modules/mod_padu_combine.R
# ============================================================
#  MODULE: PADU Combine (2.8 PADU-Combine)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
padu_combine_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    div(
      style = "margin-bottom: 20px;",
      h4("2.8 Kombinasi Analisis PADU", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Mengombinasikan hasil analisis PADU KE, HS, KL, KH, RTp, SE & KI untuk menghasilkan nilai indeks PADU tunggal.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),
    
    layout_column_wrap(
      width = 1/2,
      
      # ── Card A: Input & Parameter ────────────────────────
      card(
        card_header("Input & Parameter"),
        
        tags$p(tags$i(class = "bi bi-info-circle me-1"),
               "Peta Indeks SERASI (.gpkg)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Output dari modul 'Identifikasi Konflik Spasial' (idx_serasi.gpkg)."
        ),
        fileInput(ns("idx_serasi_file"),
                  label  = NULL,
                  accept = ".gpkg"),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-folder2-open me-1"),
               "Direktori File Analisis PADU",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Pilih folder yang berisi file idx_padu_*.gpkg dari semua modul PADU."
        ),
        shinyDirButton(ns("btn_browse_padu"), "Pilih Folder", "Pilih folder yang berisi file idx_padu_*.gpkg",
                       icon = icon("folder-open"), style = "width: 100%; margin-bottom: 8px;"),
        uiOutput(ns("padu_dir_status")),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-table me-1"),
               "Tabel Bobot PADU (.xlsx) (Opsional)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Jika tidak diunggah, bobot seragam (1/n) akan digunakan secara otomatis."
        ),
        fileInput(ns("weight_table_file"),
                  label  = NULL,
                  accept = ".xlsx"),
        
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
padu_combine_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    
    analysis_result <- reactiveVal(NULL)
    log_messages    <- reactiveVal("") 
    is_running      <- reactiveVal(FALSE)
    
    # ── Folder Selection Logic ────────────────────────────────
    roots <- c(Home = path.expand("~"), Project = normalizePath(".."), C = "C:/")
    shinyDirChoose(input, "btn_browse_padu", roots = roots, session = session)
    
    padu_folder_path <- reactive({
      req(input$btn_browse_padu)
      path <- parseDirPath(roots, input$btn_browse_padu)
      if (length(path) == 0 || path == "") return(NULL)
      as.character(path)
    })
    
    output$padu_dir_status <- renderUI({
      path <- padu_folder_path()
      if (!is.null(path)) {
        tags$small(style = "color: #18bc9c;", icon("check-circle"), basename(path))
      } else {
        tags$small(style = "color: #e74c3c;", icon("exclamation-circle"), "Belum memilih folder")
      }
    })
    
    # ── Log helper ───────────────────────────────────────────
    append_log <- function(msg) {
      current <- log_messages()
      log_messages(paste0(current, format(Sys.time(), "[%H:%M:%S] "), msg, "\n"))
    }
    
    # ── Run analysis with progress bar ──────────────────────
    observeEvent(input$btn_run, {
      req(!is_running())
      req(padu_folder_path(), input$idx_serasi_file)
      
      is_running(TRUE)
      analysis_result(NULL)
      log_messages("")   # reset log

      withProgress(message = "Menjalankan Analisis Kombinasi PADU", value = 0, {
        
        tryCatch({
          # Step 1: Load base map (progress 10%)
          incProgress(0.1, detail = "Memuat peta SERASI...")
          append_log("Memulai analisis Kombinasi PADU...")
          idx_serasi_map <- sf::st_read(input$idx_serasi_file$datapath, quiet = TRUE) %>%
            dplyr::select(-dplyr::any_of(c("area_ha", "area_flag")))
          append_log("Peta SERASI berhasil dimuat.")
          
          # Step 2: Load PADU files (progress 30%)
          incProgress(0.2, detail = "Mencari file PADU...")
          padu_files <- list.files(padu_folder_path(), pattern = "^idx_padu_.*\\.gpkg$", full.names = TRUE)
          
          if (length(padu_files) == 0) {
            stop("Tidak ditemukan file idx_padu_*.gpkg di folder yang dipilih.")
          }
          append_log(paste("Ditemukan", length(padu_files), "file PADU."))
          
          incProgress(0.2, detail = "Membaca file PADU...")
          padu_list <- lapply(padu_files, function(f) {
            df <- sf::st_read(f, quiet = TRUE) %>% sf::st_drop_geometry()
            return(df)
          })
          append_log("Semua file PADU berhasil dibaca.")
          
          # Step 3: Load/Prepare weights (progress 60%)
          incProgress(0.1, detail = "Mempersiapkan bobot...")
          padu_idx_weight <- NULL
          
          if (!is.null(input$weight_table_file)) {
            append_log("Memuat tabel bobot dari file yang diunggah...")
            padu_idx_weight <- load_and_validate_table(input$weight_table_file$datapath)
            weight_sum <- sum(padu_idx_weight[[2]], na.rm = TRUE)
            if (abs(weight_sum - 1) > 1e-6) {
              stop(paste("Jumlah bobot indeks PADU tidak sama dengan 1. Saat ini:", weight_sum))
            }
            append_log("Tabel bobot berhasil dimuat.")
          } else {
            n <- length(padu_list)
            append_log(paste("Tabel bobot tidak diunggah. Menggunakan bobot seragam (1/", n, ") untuk setiap indeks.", sep = ""))
            sample_df <- padu_list[[1]]
            idx_cols <- grep("^idx_padu_", names(sample_df), value = TRUE)
            if (length(idx_cols) != length(padu_list)) {
              file_names <- basename(padu_files)
              labels <- gsub("^idx_padu_|\\.gpkg$", "", file_names)
            } else {
              labels <- idx_cols
            }
            padu_idx_weight <- data.frame(
              index = labels,
              weight = rep(1 / n, n)
            )
          }
          
          # Step 4: Calculate combined PADU index (progress 80%)
          incProgress(0.2, detail = "Menghitung indeks kombinasi...")
          append_log("Menghitung indeks PADU kombinasi...")
          idx_padu_map <- calculate_padu_index(
            padu_list       = padu_list,
            idx_padu_map    = idx_serasi_map,
            padu_idx_weight = padu_idx_weight
          )
          append_log("Perhitungan indeks kombinasi selesai.")
          
          # Step 5: Save results (progress 95%)
          incProgress(0.15, detail = "Menyimpan hasil...")
          out_path <- file.path(output_dir(), "idx_padu.gpkg")
          sf::st_write(idx_padu_map, out_path, delete_dsn = TRUE, quiet = TRUE)
          append_log(paste("Peta disimpan →", out_path))
          
          analysis_result(list(map = idx_padu_map, table = sf::st_drop_geometry(idx_padu_map)))
          append_log("Analisis PADU-Combine berhasil diselesaikan.")
          
          incProgress(0.05, detail = "Selesai!")
          showNotification("Berhasil: Indeks PADU kombinasi telah dibuat.", type = "message", duration = 5)
          
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
            "Menghitung indeks kombinasi...")
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
      plot(analysis_result()$map["idx_padu_final"], main = "Peta Indeks PADU kombinasi", border = 1)
    })
    
    # ── Table output ─────────────────────────────────────────
    output$result_table <- renderTable({
      req(analysis_result())
      head(analysis_result()$table, 100)
    })
    
    # ── Validation log (real-time) ──────────────────────────
    output$validation_log <- renderPrint({
      invalidateLater(100, session)  
      cat(log_messages())
    })
    
  })
}