# ui/modules/mod_padu_combine.R
# ============================================================
#  MODULE: PADU Combine (2.8 PADU-Combine)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# PADU module directory map
PADU_MODULE_DIRS <- c(
  ke  = "Analisis PADU-KE",
  hs  = "Analisis PADU-HS",
  kl  = "Analisis PADU-KL",
  kh  = "Analisis PADU-KH",
  rtp = "Analisis PADU-RTp",
  se  = "Analisis PADU-SE",
  ki  = "Analisis PADU-KI"
)

# Helper to auto-discover PADU files from output dir
discover_padu_files <- function(base_dir) {
  if (is.null(base_dir) || !nzchar(base_dir) || !dir.exists(base_dir)) {
    return(list(found = character(0),
                found_keys = character(0),
                missing = names(PADU_MODULE_DIRS)))
  }
  found       <- character(0)
  found_keys  <- character(0)
  missing     <- character(0)
  
  for (key in names(PADU_MODULE_DIRS)) {
    subdir <- file.path(base_dir, PADU_MODULE_DIRS[[key]])
    if (!dir.exists(subdir)) {
      missing <- c(missing, key); next
    }
    files <- list.files(subdir, pattern = "^idx_padu_.*\\.gpkg$", full.names = TRUE)
    if (length(files) == 0) {
      missing <- c(missing, key)
    } else {
      found      <- c(found, files[1])
      found_keys <- c(found_keys, key)
    }
  }
  list(found = found, found_keys = found_keys, missing = missing)
}

# ── UI ──────────────────────────────────────────────────────────
padu_combine_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("2.8 Kombinasi analisis PADU", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Mengombinasikan hasil analisis PADU KE, HS, KL, KH, RTp, SE & KI untuk menghasilkan nilai indeks PADU tunggal.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),
    
    fluidRow(
      class = "g-3",
      
      # ── Left column: input & parameter (1/3) ────────────────
      column(
        width = 4,
        card(
          card_header("Input & parameter"),
          
          tags$p(tags$i(class = "bi bi-info-circle me-1"),
                 "Peta indeks SERASI (.gpkg)",
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
                 "Sumber file analisis PADU",
                 style = "font-weight: 600; margin-bottom: 4px;"),
          shinyDirButton(ns("btn_browse_padu"),
                         "Pilih folder manual (opsional)",
                         "Pilih folder yang berisi file idx_padu_*.gpkg",
                         icon  = icon("folder-open"),
                         style = "width: 100%; margin-bottom: 6px;"),
          actionButton(ns("btn_reset_padu_dir"),
                       tagList(tags$i(class = "bi bi-arrow-counterclockwise me-1"),
                               "Gunakan deteksi otomatis"),
                       class = "btn-sm btn-outline-secondary w-100 mb-2"),
          uiOutput(ns("padu_dir_status")),
          uiOutput(ns("padu_discovery_feedback")),
          
          hr(),
          
          tags$p(tags$i(class = "bi bi-table me-1"),
                 "Tabel bobot PADU (.xlsx) (opsional)",
                 style = "font-weight: 600; margin-bottom: 4px;"),
          tags$small(
            style = "color: #6c757d; display: block; margin-bottom: 8px;",
            "Jika tidak diunggah, bobot seragam (1/n) akan digunakan secara otomatis."
          ),
          fileInput(ns("weight_table_file"),
                    label  = NULL,
                    accept = ".xlsx"),
          
          hr(),
          
          uiOutput(ns("output_dir_warning")),
          
          div(
            style = "display: flex; gap: 8px; flex-wrap: wrap;",
            actionButton(ns("btn_run"),
                         tagList(tags$i(class = "bi bi-play-fill me-1"),
                                 "Lakukan penggabungan analisis PADU"),
                         class = "btn-success btn-sm")
          )
        )
      ),
      
      # ── Right column: output & hasil (2/3) ──────────────────
      column(
        width = 8,
        card(
          card_header("Output & hasil"),
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
    
    rv <- reactiveValues(
      analysis_result = NULL,
      gpkg_path       = NULL,
      xlsx_path       = NULL,
      log_messages    = ""
    )
    
    # ── Manual override folder picker ────────────────────────
    roots <- c(
      Home    = path.expand("~"),
      Project = normalizePath(".."),
      shinyFiles::getVolumes()()
    )
    
    manual_padu_dir <- reactiveVal(NULL)
    
    shinyDirChoose(input, "btn_browse_padu", roots = roots, session = session)
    
    observeEvent(input$btn_browse_padu, {
      path <- parseDirPath(roots, input$btn_browse_padu)
      if (length(path) > 0 && nzchar(path)) manual_padu_dir(as.character(path))
    }, ignoreInit = TRUE)
    
    observeEvent(input$btn_reset_padu_dir, {
      manual_padu_dir(NULL)
      showNotification("Kembali ke deteksi otomatis.", type = "message", duration = 3)
    })
    
    # Discovery reactive 
    padu_discovery <- reactive({
      if (!is.null(manual_padu_dir())) {
        folder <- manual_padu_dir()
        files  <- if (dir.exists(folder)) {
          list.files(folder, pattern = "^idx_padu_.*\\.gpkg$", full.names = TRUE)
        } else character(0)
        return(list(
          files        = files,
          source       = "manual",
          source_label = folder,
          missing      = character(0),
          found_keys   = character(0)
        ))
      }
      disc <- discover_padu_files(output_dir())
      list(
        files        = disc$found,
        source       = "auto",
        source_label = "",
        missing      = disc$missing,
        found_keys   = disc$found_keys
      )
    })
    
    # ── Log helper ────────────────────────────────────────────
    append_log <- function(msg) {
      rv$log_messages <- paste0(rv$log_messages,
                                format(Sys.time(), "[%H:%M:%S] "), msg, "\n")
    }
    
    # ── Output directory warning ────────────────────────────────
    output$output_dir_warning <- renderUI({
      if (is.null(output_dir()) || !nzchar(output_dir())) {
        div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-exclamation-triangle me-1"),
            "Direktori output belum diatur. Atur terlebih dahulu di menu utama.")
      }
    })
    
    # ── Dir status line ──────────────────────────────────────
    output$padu_dir_status <- renderUI({
      disc <- padu_discovery()
      
      box_style <- paste(
        "display: flex; align-items: center; gap: 8px;",
        "padding: 8px 10px; border-radius: 8px;",
        "font-size: 0.8rem; font-weight: 600; margin-top: 4px;",
        "border: 1px solid;"
      )
      
      if (disc$source == "manual") {
        tags$div(
          style = paste(box_style,
                        "background-color: #eef6fc; color: #1b75ba;",
                        "border-color: #cfe3f5;"),
          icon("hand-pointer"),
          tags$span(paste0("Folder manual (", length(disc$files), " file)"))
        )
      } else if (length(disc$files) == 0) {
        tags$div(
          style = paste(box_style,
                        "background-color: #FEF2F2; color: #b91c1c;",
                        "border-color: #FECACA;"),
          icon("exclamation-circle"),
          tags$span("Tidak ada file PADU terdeteksi otomatis")
        )
      } else {
        tags$div(
          style = paste(box_style,
                        "background-color: #ecfdf5; color: #106665;",
                        "border-color: #bbf7d0;"),
          icon("check-circle"),
          tags$span(paste0("Terdeteksi otomatis: ",
                           length(disc$files), "/",
                           length(PADU_MODULE_DIRS), " modul PADU"))
        )
      }
    })
    
    output$padu_discovery_feedback <- renderUI({
      disc <- padu_discovery()
      if (length(disc$files) == 0 && length(disc$missing) == 0) return(NULL)
      
      found_items <- if (length(disc$files) > 0) {
        tags$ul(
          style = "margin: 4px 0 4px 0; padding-left: 18px; font-size: 0.78rem;",
          lapply(disc$files, function(f) {
            tags$li(style = "color: #106665;",
                    icon("circle-check", style = "margin-right:4px;"),
                    paste0(basename(dirname(f)), " → ", basename(f)))
          })
        )
      } else NULL
      
      missing_items <- if (length(disc$missing) > 0) {
        tags$ul(
          style = "margin: 4px 0 0 0; padding-left: 18px; font-size: 0.78rem;",
          lapply(disc$missing, function(k) {
            tags$li(style = "color: #b45309;",
                    icon("triangle-exclamation", style = "margin-right:4px;"),
                    paste0("PADU-", toupper(k), " belum tersedia"))
          })
        )
      } else NULL
      
      header_label <- if (nzchar(disc$source_label)) {
        paste0("Modul PADU terdeteksi (", disc$source_label, ")")
      } else {
        "Modul PADU terdeteksi"
      }
      
      tags$div(
        style = paste("background-color: #F8FAFC; border: 1px solid #E2E8F0;",
                      "border-radius: 8px; padding: 8px 10px; margin-top: 6px;"),
        tags$div(style = paste("font-size: 0.72rem; font-weight: 700; color: #475569;",
                               "letter-spacing: 0.3px; margin-bottom: 4px;"),
                 header_label),
        found_items,
        missing_items
      )
    })
    
    # ── Run analysis ─────────────────────────────────────────
    observeEvent(input$btn_run, {
      
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification(
          "Direktori output belum diatur. Harap atur direktori output terlebih dahulu.",
          type = "error", duration = 5
        )
        return()
      }
      
      disc <- tryCatch(
        padu_discovery(),
        error = function(e) {
          list(files = character(0), source = "auto", source_label = "",
               missing = character(0), found_keys = character(0))
        }
      )
      
      missing_inputs <- character(0)
      if (is.null(input$idx_serasi_file)) missing_inputs <- c(missing_inputs, "peta SERASI")
      if (length(disc$files) == 0)        missing_inputs <- c(missing_inputs, "file PADU")
      
      if (length(missing_inputs) > 0) {
        showNotification(
          paste0("Harap unggah ", paste(missing_inputs, collapse = " dan "),
                 " sebelum melanjutkan."),
          type = "warning", duration = 6
        )
        return()
      }
      
      if (length(disc$missing) > 0) {
        showNotification(
          paste0("Modul PADU belum tersedia: ",
                 paste(toupper(disc$missing), collapse = ", "),
                 ". Analisis akan dilanjutkan tanpa modul tersebut."),
          type = "warning", duration = 8
        )
      }
      
      rv$analysis_result <- NULL
      rv$gpkg_path       <- NULL
      rv$xlsx_path       <- NULL
      rv$log_messages    <- ""
      
      append_log("Memulai analisis kombinasi PADU...")
      append_log(paste0("Sumber file PADU: ", disc$source_label))
      if (length(disc$missing) > 0)
        append_log(paste0("Modul tidak tersedia: ", paste(disc$missing, collapse = ", ")))
      
      withProgress(message = "Menjalankan analisis kombinasi PADU", value = 0, {
        
        tryCatch({
          # Step 1: Load base SERASI map
          incProgress(0.1, detail = "Memuat peta SERASI...")
          idx_serasi_map <- sf::st_read(input$idx_serasi_file$datapath, quiet = TRUE) %>%
            dplyr::select(-dplyr::any_of("area_flag"))
          append_log("Peta SERASI berhasil dimuat.")
          
          # Step 2: Resolve PADU file list (from discovery)
          incProgress(0.2, detail = "Menyiapkan file PADU...")
          padu_files <- disc$files
          append_log(paste("Menggunakan", length(padu_files), "file PADU."))
          
          incProgress(0.2, detail = "Membaca file PADU...")
          padu_list <- lapply(padu_files, function(f) {
            sf::st_read(f, quiet = TRUE) %>% sf::st_drop_geometry()
          })
          append_log("Semua file PADU berhasil dibaca.")
          
          # Step 3: Load/Prepare weights
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
            append_log(paste0("Tabel bobot tidak diunggah. Menggunakan bobot seragam (1/", n,
                              ") untuk setiap indeks."))
            sample_df <- padu_list[[1]]
            idx_cols  <- grep("^idx_padu_", names(sample_df), value = TRUE)
            if (length(idx_cols) != length(padu_list)) {
              file_names <- basename(padu_files)
              labels     <- gsub("^idx_padu_|\\.gpkg$", "", file_names)
            } else {
              labels <- idx_cols
            }
            padu_idx_weight <- data.frame(index = labels, weight = rep(1 / n, n))
          }
          
          # Step 4: Compute combined PADU index
          incProgress(0.2, detail = "Menghitung indeks kombinasi...")
          append_log("Menghitung indeks PADU kombinasi...")
          idx_padu_map <- calculate_padu_index(
            padu_list       = padu_list,
            idx_padu_map    = idx_serasi_map,
            padu_idx_weight = padu_idx_weight
          )
          append_log("Perhitungan indeks kombinasi selesai.")
          
          # Conditional dissolve idx_serasi_map
          if ("length" %in% colnames(idx_padu_map)) {
            idx_padu_map_viz <- dissolve_id_pu(idx_padu_map)
          } else {
            idx_padu_map_viz <- idx_padu_map  
          }
          
          # Step 5: Save
          incProgress(0.15, detail = "Menyimpan hasil...")
          
          padu_combine_dir <- file.path(output_dir(), "Analisis PADU-Kombinasi")
          if (!dir.exists(padu_combine_dir)) {
            dir.create(padu_combine_dir, recursive = TRUE, showWarnings = FALSE)
          }
          if (!dir.exists(padu_combine_dir)) {
            stop("Tidak dapat membuat atau mengakses direktori: ", padu_combine_dir)
          }
          
          gpkg_path <- file.path(padu_combine_dir, "idx_padu_combine.gpkg")
          xlsx_path <- file.path(padu_combine_dir, "idx_padu_combine.xlsx")
          
          sf::st_write(idx_padu_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          res_table <- sf::st_drop_geometry(idx_padu_map)
          openxlsx::write.xlsx(res_table, xlsx_path)
          
          rv$gpkg_path       <- gpkg_path
          rv$xlsx_path       <- xlsx_path
          rv$analysis_result <- list(map = idx_padu_map_viz, table = sf::st_drop_geometry(idx_padu_map_viz))
          
          out <- list(
            inputs = list(
              start_time        = Sys.time(),
              idx_serasi_path   = input$idx_serasi_file$datapath,
              padu_source       = disc$source,
              padu_source_label = disc$source_label,
              padu_folder_path  = if (disc$source == "manual") disc$source_label else output_dir(),
              missing_modules   = disc$missing,
              weight_table_path = if (!is.null(input$weight_table_file))
                input$weight_table_file$datapath else NULL,
              output_dir        = output_dir(),
              n_files           = length(padu_files),
              padu_files        = basename(padu_files)
            ),
            result = list(
              idx_serasi_map = idx_serasi_map,
              idx_padu_map   = idx_padu_map,
              idx_padu_table = res_table,
              weight_table   = padu_idx_weight,
              padu_files     = padu_files
            )
          )
          
          log_dir <- file.path(padu_combine_dir, "log")
          if (!dir.exists(log_dir)) {
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
          }
          log_path <- file.path(log_dir, "idx_padu_combine_log.rda")
          if (dir.exists(log_dir)) {
            tryCatch({
              inputs <- out$inputs
              save(inputs, file = log_path)
            }, error = function(e) warning("Gagal menulis file log: ", e$message))
          }
          
          session$userData$module_results$padu_combine <- out
          
          idx_padu_combine_viz <- plot_continuous_map(
            map      = idx_padu_map,
            column   = "idx_padu_final",
            title    = "Peta indeks PADU gabungan",
            legend   = "Indeks PADU",
            low      = "red",
            high     = "lightgreen",
            filepath = file.path(log_dir, "idx_padu_combine.png")
          )
          
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
        
      })
    })
    
    # ── Status box ────────────────────────────────────────────
    output$status_box <- renderUI({
      disc <- padu_discovery()
      if (!is.null(rv$analysis_result)) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Analisis selesai.")
      } else if (!is.null(input$idx_serasi_file) && length(disc$files) > 0) {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Siap menjalankan analisis.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Unggah peta SERASI dan pastikan file PADU tersedia.")
      }
    })
    
    # ── Config & shared result rendering ─────────────────────
    padu_combine_config <- list(
      map_color_col  = "idx_padu_final",
      map_title      = "Indeks PADU",
      map_palette    = "RdYlGn",
      map_label_cols = c(
        "ID PU"       = "id_pu",
        "RTRW"        = "RTRW",
        "RZWP3K"      = "RZWP3K",
        "Indeks PADU" = "idx_padu_final"
      ),
      table_cols = c(
        "id_pu"          = "ID PU",
        "RTRW"           = "RTRW",
        "RZWP3K"         = "RZWP3K",
        "admin"          = "Administrasi",
        "idx_padu_ke"    = "Indeks PADU-KE",
        "idx_padu_hs"    = "Indeks PADU-HS",
        "idx_padu_kl"    = "Indeks PADU-KL",
        "idx_padu_kh"    = "Indeks PADU-KH",
        "idx_padu_rtp"   = "Indeks PADU-RTp",
        "idx_padu_se"    = "Indeks PADU-SE",
        "idx_padu_ki"    = "Indeks PADU-KI",
        "idx_padu_final" = "Indeks PADU kombinasi"
      ),
      table_round_cols = c(
        "Indeks PADU-KE", "Indeks PADU-HS", "Indeks PADU-KL", "Indeks PADU-KH",
        "Indeks PADU-RTp", "Indeks PADU-SE", "Indeks PADU-KI", "Indeks PADU kombinasi"
      )
    )
    
    render_result_server(input, output, session, rv, padu_combine_config)
    
  })
}