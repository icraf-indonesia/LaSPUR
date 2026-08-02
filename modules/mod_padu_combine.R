# ui/modules/mod_padu_combine.R
# ============================================================
#  MODULE: PADU Combine (2.8 PADU-Combine)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── UI ──────────────────────────────────────────────────────────
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
    
    fluidRow(
      class = "g-3",
      
      # ── Left column: Input & Parameter (1/3) ────────────────
      column(
        width = 4,
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
          
          # Output directory warning (rendered server-side, see output$output_dir_warning)
          uiOutput(ns("output_dir_warning")),
          
          div(
            style = "display: flex; gap: 8px; flex-wrap: wrap;",
            actionButton(ns("btn_run"),
                         tagList(tags$i(class = "bi bi-play-fill me-1"),
                                 "Lakukan Penggabungan Analisis PADU"),
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
padu_combine_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Reactive values ──────────────────────────────────────────
    rv <- reactiveValues(
      analysis_result = NULL,
      gpkg_path = NULL,
      xlsx_path = NULL,
      log_messages = ""
    )
    
    # ── Folder Selection Logic ────────────────────────────────
    roots <- c(
      Home    = path.expand("~"),
      Project = normalizePath(".."),
      shinyFiles::getVolumes()()  
    )
    
    shinyDirChoose(input, "btn_browse_padu",
                   roots   = roots,
                   session = session)
    
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
    
    # ── Log helper ──────────────────────────────────────────────
    append_log <- function(msg) {
      rv$log_messages <- paste0(rv$log_messages, format(Sys.time(), "[%H:%M:%S] "), msg, "\n")
    }
    
    # ── Output directory warning ────────────────────────────────
    output$output_dir_warning <- renderUI({
      if (is.null(output_dir()) || !nzchar(output_dir())) {
        div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-exclamation-triangle me-1"),
            "Direktori output belum diatur. Atur terlebih dahulu di menu utama.")
      }
    })
    
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
      
      req(input$idx_serasi_file, padu_folder_path())
      
      # Reset previous results
      rv$analysis_result <- NULL
      rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL
      rv$log_messages <- ""
      
      append_log("Memulai analisis Kombinasi PADU...")
      
      withProgress(message = "Menjalankan Analisis Kombinasi PADU", value = 0, {
        
        tryCatch({
          # Step 1: Load base map (progress 10%)
          incProgress(0.1, detail = "Memuat peta SERASI...")
          idx_serasi_map <- sf::st_read(input$idx_serasi_file$datapath, quiet = TRUE) %>%
            dplyr::select(-dplyr::any_of("area_flag"))
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
          gpkg_path <- file.path(output_dir(), "idx_padu.gpkg")
          xlsx_path <- file.path(output_dir(), "idx_padu.xlsx")
          dir.create(output_dir(), recursive = TRUE, showWarnings = FALSE)
          
          sf::st_write(idx_padu_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          res_table <- sf::st_drop_geometry(idx_padu_map)
          openxlsx::write.xlsx(res_table, xlsx_path)
          
          rv$gpkg_path <- gpkg_path
          rv$xlsx_path <- xlsx_path
          rv$analysis_result <- list(map = idx_padu_map, table = res_table)
          
          append_log(paste("Peta disimpan →", gpkg_path))
          append_log(paste("Tabel disimpan →", xlsx_path))
          append_log("Analisis PADU-Kombinasi berhasil diselesaikan.")
          
          incProgress(0.05, detail = "Selesai!")
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
      } else if (!is.null(input$idx_serasi_file) && !is.null(padu_folder_path())) {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Siap menjalankan analisis.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Unggah peta SERASI dan pilih folder PADU.")
      }
    })
    
    # ── Config & shared result rendering ──────────────────────
    padu_combine_config <- list(
      map_color_col    = "idx_padu_final",
      map_title        = "Indeks PADU",
      map_palette      = "RdYlGn",
      map_label_cols   = c(
        "ID PU"       = "id_pu",
        "RTRW"        = "RTRW",
        "RZWP3K"      = "RZWP3K",
        "Indeks PADU" = "idx_padu_final"
      ),
      table_cols = c(
        "id_pu"         = "ID PU",
        "RTRW"          = "RTRW",
        "RZWP3K"        = "RZWP3K",
        "admin"         = "Administrasi",
        "idx_padu_ke"   = "Indeks PADU-KE",
        "idx_padu_hs"   = "Indeks PADU-HS",
        "idx_padu_kl"   = "Indeks PADU-KL",
        "idx_padu_kh"   = "Indeks PADU-KH",
        "idx_padu_rtp"  = "Indeks PADU-RTp",
        "idx_padu_se"   = "Indeks PADU-SE",
        "idx_padu_ki"   = "Indeks PADU-KI",
        "idx_padu_final" = "Indeks PADU Kombinasi"
      ),
      table_round_cols = c(
        "Indeks PADU-KE", "Indeks PADU-HS", "Indeks PADU-KL", "Indeks PADU-KH",
        "Indeks PADU-RTp", "Indeks PADU-SE", "Indeks PADU-KI", "Indeks PADU Kombinasi"
      )
    )
    
    render_result_server(input, output, session, rv, padu_combine_config)
    
  })
}