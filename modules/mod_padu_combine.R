# ui/modules/mod_padu_combine.R
# ============================================================
#  MODULE: PADU Combine (2.8 PADU-Combine)
# ============================================================

source("R/functions.R")
source("R/helpers.R")
source("R/shared_inputs.R")

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

PADU_MODULE_DIRS <- c(
  ke  = "Analisis PADU-KE",
  hs  = "Analisis PADU-HS",
  kl  = "Analisis PADU-KL",
  kh  = "Analisis PADU-KH",
  rtp = "Analisis PADU-RTp",
  se  = "Analisis PADU-SE",
  ki  = "Analisis PADU-KI"
)

PADU_LABELS <- c(
  ke  = "PADU-KE",
  hs  = "PADU-HS",
  kl  = "PADU-KL",
  kh  = "PADU-KH",
  rtp = "PADU-RTp",
  se  = "PADU-SE",
  ki  = "PADU-KI"
)

discover_padu_files <- function(base_dir) {
  if (is.null(base_dir) || !nzchar(base_dir) || !dir.exists(base_dir)) {
    return(list(found = character(0), found_keys = character(0),
                missing = names(PADU_MODULE_DIRS)))
  }
  found <- character(0); found_keys <- character(0); missing <- character(0)
  for (key in names(PADU_MODULE_DIRS)) {
    subdir <- file.path(base_dir, PADU_MODULE_DIRS[[key]])
    if (!dir.exists(subdir)) { missing <- c(missing, key); next }
    files <- list.files(subdir, pattern = "^idx_padu_.*\\.gpkg$", full.names = TRUE)
    if (length(files) == 0) {
      missing <- c(missing, key)
    } else {
      found <- c(found, files[1]); found_keys <- c(found_keys, key)
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
      
      column(
        width = 4,
        card(
          card_header("Input & parameter"),
          
          tags$p(tags$i(class = "bi bi-info-circle me-1"),
                 "Peta indeks SERASI (.gpkg)",
                 style = "font-weight: 600; margin-bottom: 4px;"),
          div(class = "laspur-fileinput-with-bar",
              uiOutput(ns("serasi_ui_container"))),
          
          hr(),
          
          tags$p(tags$i(class = "bi bi-folder2-open me-1"),
                 "Sumber file analisis PADU",
                 style = "font-weight: 600; margin-bottom: 4px;"),
          
          div(
            class = "laspur-fileinput-with-bar",
            shinyDirButton(
              ns("btn_browse_padu"),
              "Pilih folder manual (opsional)",
              "Pilih folder yang berisi file idx_padu_*.gpkg",
              icon  = icon("folder-open"),
              class = "btn-light w-100",
              style = paste("background-color: #FFFFFF;",
                            "border: 1px solid #E2E8F0;",
                            "color: #475569;", "font-weight: 600;",
                            "border-radius: 8px;", "text-align: left;",
                            "box-shadow: 0 1px 2px rgba(0,0,0,0.05);")
            ),
            uiOutput(ns("padu_source_bar"))
          ),
          uiOutput(ns("padu_discovery_feedback")),
          uiOutput(ns("serasi_consistency_banner")),
          
          hr(),
          
          tags$p(tags$i(class = "bi bi-table me-1"),
                 "Tabel bobot PADU (.xlsx) (opsional)",
                 style = "font-weight: 600; margin-bottom: 4px;"),
          tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                     "Jika tidak diunggah, bobot seragam (1/n) akan digunakan secara otomatis."),
          fileInput(ns("weight_table_file"), label = NULL, accept = ".xlsx"),
          
          hr(),
          uiOutput(ns("output_dir_warning")),
          
          div(style = "display: flex; gap: 8px; flex-wrap: wrap;",
              actionButton(ns("btn_run"),
                           tagList(tags$i(class = "bi bi-play-fill me-1"),
                                   "Lakukan penggabungan analisis PADU"),
                           class = "btn-success btn-sm"))
        )
      ),
      
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
      log_messages    = "",
      excluded_padus  = character(0)
    )
    
    serasi_in <- serasi_input(input, output, session, output_dir,
                              input_id = "idx_serasi_file",
                              label    = "Peta indeks SERASI (.gpkg)")
    
    output$serasi_ui_container <- renderUI({ serasi_in$ui_block() })
    
    # ── PADU folder discovery ───────────────────────────────
    roots <- c(Home = path.expand("~"),
               Project = normalizePath(".."),
               shinyFiles::getVolumes()())
    manual_padu_dir <- reactiveVal(NULL)
    shinyDirChoose(input, "btn_browse_padu", roots = roots, session = session)
    observeEvent(input$btn_browse_padu, {
      path <- parseDirPath(roots, input$btn_browse_padu)
      if (length(path) > 0 && nzchar(path)) manual_padu_dir(as.character(path))
    }, ignoreInit = TRUE)
    
    padu_discovery <- reactive({
      if (!is.null(manual_padu_dir())) {
        folder <- manual_padu_dir()
        files  <- if (dir.exists(folder)) {
          list.files(folder, pattern = "^idx_padu_.*\\.gpkg$", full.names = TRUE)
        } else character(0)
        return(list(files = files, source = "manual",
                    source_label = folder, missing = character(0),
                    found_keys = character(0)))
      }
      disc <- discover_padu_files(output_dir())
      list(files = disc$found, source = "auto", source_label = "",
           missing = disc$missing, found_keys = disc$found_keys)
    })
    
    output$padu_source_bar <- renderUI({
      disc <- padu_discovery()
      if (length(disc$files) == 0) return(NULL)
      folder_display <- if (disc$source == "manual") {
        disc$source_label
      } else {
        out <- output_dir()
        if (is.null(out) || !nzchar(out)) "" else
          normalizePath(out, winslash = "/", mustWork = FALSE)
      }
      if (!nzchar(folder_display)) return(NULL)
      tags$div(
        class = "laspur-loaded-bar",
        style = paste("height: 20px;", "border-radius: 4px;",
                      "overflow: hidden;", "background-color: #eef2f6;",
                      "width: 100%;", "box-sizing: border-box;"),
        tags$div(
          class = "laspur-loaded-bar-fill",
          style = paste(
            "width: 100%;", "height: 20px;",
            "background: linear-gradient(90deg, #1b75ba 0%, #3b92d1 100%);",
            "color: #ffffff;", "font-size: 0.72rem;", "font-weight: 600;",
            "letter-spacing: 0.2px;", "line-height: 20px;",
            "text-align: center;", "white-space: nowrap;",
            "overflow: hidden;", "text-overflow: ellipsis;",
            "padding: 0 8px;", "box-sizing: border-box;"),
          folder_display
        )
      )
    })
    
    output$padu_discovery_feedback <- renderUI({
      disc <- padu_discovery()
      if (length(disc$files) == 0 && length(disc$missing) == 0) return(NULL)
      
      found_items <- if (length(disc$files) > 0) {
        tags$ul(style = "margin: 2px 0 2px 0; padding-left: 16px; font-size: 0.78rem;",
                lapply(disc$files, function(f) {
                  tags$li(style = "color: #106665; line-height: 1.5;",
                          icon("circle-check", style = "margin-right: 4px;"),
                          paste0(basename(dirname(f)), " \u2192 ", basename(f)))
                }))
      } else NULL
      
      missing_items <- if (length(disc$missing) > 0) {
        tags$ul(style = "margin: 2px 0 0 0; padding-left: 16px; font-size: 0.78rem;",
                lapply(disc$missing, function(k) {
                  tags$li(style = "color: #b45309; line-height: 1.5;",
                          icon("triangle-exclamation", style = "margin-right: 4px;"),
                          paste0("PADU-", toupper(k), " belum tersedia"))
                }))
      } else NULL
      
      header_label <- if (nzchar(disc$source_label)) {
        paste0("Modul PADU terdeteksi (", disc$source_label, ")")
      } else "Modul PADU terdeteksi"
      
      tags$div(
        style = paste("background-color: #F8FAFC; border: 1px solid #E2E8F0;",
                      "border-radius: 8px; padding: 6px 10px; margin-top: 4px;"),
        tags$div(style = paste("font-size: 0.72rem; font-weight: 700; color: #475569;",
                               "letter-spacing: 0.3px; margin-bottom: 2px;"),
                 header_label),
        found_items, missing_items
      )
    })
    
    # ── Consistency checker ─────────────────────────────────
    fingerprint_padu <- function(key) {
      dir_name <- PADU_MODULE_DIRS[[key]]
      gpkg <- file.path(output_dir(), dir_name, sprintf("idx_padu_%s.gpkg", key))
      log_path <- file.path(output_dir(), dir_name, "log",
                            sprintf("idx_padu_%s_log.rda", key))
      
      res <- tryCatch(
        session$userData$module_results[[paste0("padu_", key)]],
        error = function(e) NULL)
      
      src_name <- NULL; src_hash <- NULL
      
      if (!is.null(res) && !is.null(res$inputs)) {
        src_name <- res$inputs$serasi_source_name
        src_hash <- res$inputs$serasi_source_hash
      }
      
      if ((is.null(src_hash) || is.na(src_hash)) && file.exists(log_path)) {
        tryCatch({
          env <- new.env(parent = emptyenv())
          load(log_path, envir = env)
          if (exists("inputs", envir = env, inherits = FALSE)) {
            inp <- get("inputs", envir = env, inherits = FALSE)
            if (is.null(src_name) || is.na(src_name))
              src_name <- inp$serasi_source_name
            if (is.null(src_hash) || is.na(src_hash))
              src_hash <- inp$serasi_source_hash
          }
        }, error = function(e) NULL)
      }
      
      # Legacy fallback: hash the id_pu set from the PADU GPKG
      if ((is.null(src_hash) || is.na(src_hash)) && file.exists(gpkg)) {
        tryCatch({
          m <- sf::st_read(gpkg, quiet = TRUE)
          if ("id_pu" %in% names(m)) {
            ids <- sort(unique(as.character(m$id_pu)))
            src_hash <- digest::digest(paste(ids, collapse = "|"),
                                       algo = "xxhash64")
          }
        }, error = function(e) NULL)
      }
      
      if (is.null(src_hash) || is.na(src_hash)) return(NULL)
      if (is.null(src_name) || is.na(src_name) || !nzchar(src_name))
        src_name <- "(hash-only)"
      
      list(key = key, name = src_name, hash = src_hash)
    }
    
    padu_fingerprints <- reactive({
      out <- list()
      for (k in names(PADU_MODULE_DIRS)) {
        fp <- fingerprint_padu(k)
        if (!is.null(fp)) out[[k]] <- fp
      }
      combine_hash <- serasi_in$hash()
      if (!is.null(combine_hash) &&
          length(combine_hash) == 1 &&
          !is.na(combine_hash)) {
        out[["combine"]] <- list(
          key  = "combine",
          name = serasi_in$filename(),
          hash = combine_hash
        )
      }
      out
    })
    
    consistency <- reactive({
      fps <- padu_fingerprints()
      if (length(fps) <= 1) {
        return(list(mismatch = FALSE, n_versions = length(fps),
                    majority_hash = NULL, rows = fps))
      }
      hashes <- sapply(fps, function(x) x$hash)
      tbl <- sort(table(hashes), decreasing = TRUE)
      majority <- names(tbl)[1]
      list(
        mismatch       = length(tbl) > 1,
        n_versions     = length(tbl),
        majority_hash  = majority,
        rows           = fps
      )
    })
    
    combine_mismatch <- reactive({
      cs <- consistency()
      if (is.null(cs$rows[["combine"]])) return(FALSE)
      if (is.null(cs$majority_hash)) return(FALSE)
      !identical(cs$rows[["combine"]]$hash, cs$majority_hash)
    })
    
    row_label <- function(k) {
      if (identical(k, "combine")) "PADU-Kombinasi"
      else (PADU_LABELS[[k]] %||% k)
    }
    
    output$serasi_consistency_banner <- renderUI({
      cs <- consistency()
      if (length(cs$rows) == 0) return(NULL)
      
      if (!cs$mismatch) {
        return(tags$div(
          style = paste("margin-top: 6px; padding: 8px 10px;",
                        "background: #ecfdf5; border: 1px solid #bbf7d0;",
                        "border-radius: 8px; font-size: 0.78rem; color: #106665;"),
          tags$i(class = "bi bi-check-circle-fill me-1"),
          "Semua modul konsisten menggunakan SERASI: ",
          tags$strong(cs$rows[[1]]$name)
        ))
      }
      
      items <- lapply(names(cs$rows), function(k) {
        fp <- cs$rows[[k]]
        is_minority <- !identical(fp$hash, cs$majority_hash)
        is_combine  <- identical(k, "combine")
        
        tags$div(
          style = sprintf(
            paste("display: grid;",
                  "grid-template-columns: 92px 1fr;",
                  "column-gap: 8px;",
                  "padding: 3px 0;",
                  "align-items: start;",
                  "color: %s;",
                  "%s"),
            if (is_minority) "#b45309" else "#475569",
            if (is_combine)
              "margin-top: 6px; padding-top: 6px; border-top: 1px dashed #F59E0B;"
            else ""
          ),
          tags$span(
            style = "font-weight: 700; font-size: 0.7rem; white-space: nowrap;",
            row_label(k)
          ),
          tags$span(
            style = paste("font-family: monospace;",
                          "font-size: 0.66rem;",
                          "text-align: right;",
                          "word-break: break-all;",
                          "line-height: 1.3;"),
            fp$name,
            if (is_minority) " \u26A0" else ""
          )
        )
      })
      
      tags$div(
        style = paste("margin-top: 6px; padding: 8px 10px;",
                      "background: #FEF3C7; border: 1px solid #FDE68A;",
                      "border-radius: 8px; font-size: 0.78rem; color: #92400E;"),
        tags$div(style = "font-weight: 700; margin-bottom: 4px;",
                 tags$i(class = "bi bi-exclamation-triangle-fill me-1"),
                 sprintf("Ditemukan %d versi SERASI berbeda", cs$n_versions)),
        tags$div(style = "color: #78350f;", items)
      )
    })
    
    # ── Run analysis ────────────────────────────────────────
    do_run_combine <- function(selected_keys) {
      disc <- tryCatch(padu_discovery(),
                       error = function(e) list(
                         files = character(0), source = "auto",
                         source_label = "", missing = character(0),
                         found_keys = character(0)))
      
      idx_serasi_map <- serasi_in$idx_serasi_map()

      if (is.null(idx_serasi_map)) {
        showNotification(
          "Peta SERASI belum tersedia. Jalankan modul 1.1/1.2 terlebih dahulu atau unggah berkas.",
          type = "warning", duration = 6)
        return()
      }
      if (length(disc$files) == 0) {
        showNotification("File PADU tidak ditemukan.",
                         type = "warning", duration = 6)
        return()
      }
      
      files_to_use <- disc$files
      if (!is.null(selected_keys)) {
        keep <- basename(dirname(files_to_use)) %in% PADU_MODULE_DIRS[selected_keys]
        files_to_use <- files_to_use[keep]
      }
      
      if (length(files_to_use) == 0) {
        showNotification("Tidak ada PADU yang tersisa setelah pengecualian.",
                         type = "warning", duration = 6)
        return()
      }
      
      rv$analysis_result <- NULL
      rv$gpkg_path       <- NULL
      rv$xlsx_path       <- NULL
      rv$log_messages    <- ""
      
      append_log <- function(msg) {
        rv$log_messages <- paste0(rv$log_messages,
                                  format(Sys.time(), "[%H:%M:%S] "), msg, "\n")
      }
      append_log("Memulai analisis kombinasi PADU...")
      append_log(sprintf("Menggunakan %d file PADU.", length(files_to_use)))
      if (!is.null(selected_keys) &&
          length(selected_keys) < length(disc$found_keys)) {
        excluded <- setdiff(disc$found_keys, selected_keys)
        append_log(paste0("PADU dikecualikan: ",
                          paste(toupper(excluded), collapse = ", ")))
      }
      
      withProgress(message = "Menjalankan analisis kombinasi PADU", value = 0, {
        tryCatch({
          incProgress(0.1, detail = "Memuat peta SERASI...")
          idx_serasi_map <- idx_serasi_map %>%
            dplyr::select(-dplyr::any_of("area_flag")) %>%
            normalize_legacy_ids()
          append_log("Peta SERASI berhasil dimuat.")
          
          incProgress(0.2, detail = "Membaca file PADU...")
          padu_list <- lapply(files_to_use, function(f) {
            sf::st_read(f, quiet = TRUE) %>% sf::st_drop_geometry()
          })
          append_log("Semua file PADU berhasil dibaca.")
          
          incProgress(0.1, detail = "Mempersiapkan bobot...")
          padu_idx_weight <- NULL
          
          if (!is.null(input$weight_table_file)) {
            append_log("Memuat tabel bobot dari file yang diunggah...")
            padu_idx_weight <- load_and_validate_table(input$weight_table_file$datapath)
            weight_sum <- sum(padu_idx_weight[[2]], na.rm = TRUE)
            if (abs(weight_sum - 1) > 1e-6) {
              stop(paste("Jumlah bobot indeks PADU tidak sama dengan 1. Saat ini:",
                         weight_sum))
            }
            append_log("Tabel bobot berhasil dimuat.")
          } else {
            n <- length(padu_list)
            append_log(paste0("Tabel bobot tidak diunggah. ",
                              "Menggunakan bobot seragam (1/", n, ")."))
            sample_df <- padu_list[[1]]
            idx_cols  <- grep("^idx_padu_", names(sample_df), value = TRUE)
            if (length(idx_cols) != length(padu_list)) {
              labels <- gsub("^idx_padu_|\\.gpkg$", "", basename(files_to_use))
            } else {
              labels <- idx_cols
            }
            padu_idx_weight <- data.frame(index = labels, weight = rep(1 / n, n))
          }
          
          incProgress(0.2, detail = "Menghitung indeks kombinasi...")
          append_log("Menghitung indeks PADU kombinasi...")
          idx_padu_map <- calculate_padu_index(
            padu_list       = padu_list,
            idx_padu_map    = idx_serasi_map,
            padu_idx_weight = padu_idx_weight
          )
          append_log("Perhitungan indeks kombinasi selesai.")
          
          if ("length" %in% colnames(idx_padu_map)) {
            idx_padu_map_viz <- dissolve_id_pu(idx_padu_map)
          } else {
            idx_padu_map_viz <- idx_padu_map
          }
          
          incProgress(0.15, detail = "Menyimpan hasil...")
          padu_combine_dir <- file.path(output_dir(), "Analisis PADU-Kombinasi")
          if (!dir.exists(padu_combine_dir)) {
            dir.create(padu_combine_dir, recursive = TRUE, showWarnings = FALSE)
          }
          gpkg_path <- file.path(padu_combine_dir, "idx_padu_combine.gpkg")
          xlsx_path <- file.path(padu_combine_dir, "idx_padu_combine.xlsx")
          sf::st_write(idx_padu_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          res_table <- sf::st_drop_geometry(idx_padu_map)
          openxlsx::write.xlsx(res_table, xlsx_path)
          
          rv$gpkg_path       <- gpkg_path
          rv$xlsx_path       <- xlsx_path
          rv$analysis_result <- list(map = idx_padu_map_viz,
                                     table = sf::st_drop_geometry(idx_padu_map_viz))
          
          out <- list(
            inputs = list(
              start_time             = Sys.time(),
              idx_serasi_path        = serasi_in$filename(),
              idx_serasi_source      = serasi_in$source(),
              idx_serasi_hash        = serasi_in$hash(),
              serasi_source_name     = serasi_in$filename(),
              serasi_source_hash     = serasi_in$hash(),
              padu_source            = disc$source,
              padu_source_label      = disc$source_label,
              padu_folder_path       = if (disc$source == "manual")
                disc$source_label else output_dir(),
              missing_modules        = disc$missing,
              excluded_modules       = setdiff(disc$found_keys,
                                               selected_keys %||% disc$found_keys),
              weight_table_path      = if (!is.null(input$weight_table_file))
                input$weight_table_file$datapath else NULL,
              output_dir             = output_dir(),
              n_files                = length(files_to_use),
              padu_files             = basename(files_to_use)
            ),
            result = list(
              idx_serasi_map = idx_serasi_map,
              idx_padu_map   = idx_padu_map,
              idx_padu_table = res_table,
              weight_table   = padu_idx_weight,
              padu_files     = files_to_use
            )
          )
          
          log_dir <- file.path(padu_combine_dir, "log")
          if (!dir.exists(log_dir)) {
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
          }
          tryCatch({
            inputs <- out$inputs
            save(inputs, file = file.path(log_dir, "idx_padu_combine_log.rda"))
          }, error = function(e) warning("Gagal menulis file log: ", e$message))
          
          session$userData$module_results$padu_combine <- out
          
          plot_continuous_map(
            map      = idx_padu_map,
            column   = "idx_padu_final",
            title    = "Peta indeks PADU gabungan",
            legend   = "Indeks PADU",
            low      = "red",
            high     = "lightgreen",
            filepath = file.path(log_dir, "idx_padu_combine.png")
          )
          
          append_log(paste("Peta disimpan \u2192", gpkg_path))
          append_log(paste("Tabel disimpan \u2192", xlsx_path))
          append_log("Analisis PADU-Kombinasi berhasil diselesaikan.")
          
          incProgress(0.05, detail = "Selesai!")
          showNotification(paste("Analisis selesai. Hasil disimpan ke", gpkg_path),
                           type = "message", duration = 5)
          
        }, error = function(e) {
          msg <- conditionMessage(e)
          if (is.null(msg) || msg == "") msg <- "Error tidak diketahui"
          append_log(paste("ERROR:", msg))
          showNotification(paste("Analisis gagal:", msg),
                           type = "error", duration = 10)
        })
      })
    }
    
    # ── Click handler with consistency gate ─────────────────
    observeEvent(input$btn_run, {
      if (is.null(output_dir()) || !nzchar(output_dir()) ||
          !validate_output_dir(output_dir())) {
        showNotification("Direktori output belum diatur.",
                         type = "error", duration = 5)
        return()
      }
      cs <- consistency()
      if (isTRUE(cs$mismatch)) {
        rows_ui <- lapply(names(cs$rows), function(k) {
          fp <- cs$rows[[k]]
          is_minority <- !identical(fp$hash, cs$majority_hash)
          is_combine  <- identical(k, "combine")
          
          tags$tr(
            style = if (is_combine)
              "border-top: 1px dashed #F59E0B;"
            else NULL,
            tags$td(style = paste("padding: 6px 8px; font-weight: 700;",
                                  "white-space: nowrap; vertical-align: top;",
                                  "width: 100px; font-size: 0.72rem;"),
                    row_label(k)),
            tags$td(style = paste("padding: 6px 8px; font-family: monospace;",
                                  "font-size: 0.68rem; word-break: break-all;",
                                  "vertical-align: top; text-align: right;"),
                    fp$name),
            tags$td(style = sprintf(paste("padding: 6px 8px; text-align: right;",
                                          "white-space: nowrap; vertical-align: top;",
                                          "font-size: 0.7rem; font-weight: 600;",
                                          "color: %s;"),
                                    if (is_minority) "#b45309" else "#106665"),
                    if (is_minority) "Berbeda" else "Seragam")
          )
        })
        
        blocked_by_combine <- isTRUE(combine_mismatch())
        
        disclaimer <- if (blocked_by_combine) {
          tags$div(
            style = paste("margin-top: 12px; padding: 8px 10px;",
                          "background:#FEF3C7; border:1px solid #FDE68A;",
                          "border-radius:8px; font-size:0.8rem; color:#92400E;"),
            tags$div(
              tags$i(class = "bi bi-lock-fill me-1"),
              tags$strong("Peta indeks SERASI pada modul ini berbeda dari versi seragam.")
            ),
            tags$div(style = "margin-top: 4px;",
                     "Ubah input Peta indeks SERASI agar sesuai dengan versi seragam, ",
                     "lalu buka kembali dialog ini. Tombol Lanjutkan dinonaktifkan sampai konsisten.")
          )
        } else {
          tags$div(
            style = paste("margin-top: 12px; padding: 8px 10px;",
                          "background:#FEF3C7; border:1px solid #FDE68A;",
                          "border-radius:8px; font-size:0.8rem; color:#92400E;"),
            tags$i(class = "bi bi-info-circle-fill me-1"),
            tags$strong("PADU dengan versi SERASI berbeda akan otomatis dikecualikan"),
            " dari proses penggabungan. PADU yang mengikuti versi seragam akan tetap digunakan."
          )
        }
        
        proceed_btn <- if (blocked_by_combine) {
          tags$button(
            type = "button",
            class = "btn btn-warning",
            style = paste("font-weight: 600; color: #1e293b;",
                          "opacity: 0.55; cursor: not-allowed;"),
            disabled = "disabled",
            tags$i(class = "bi bi-play-fill me-1"),
            "Lanjutkan"
          )
        } else {
          actionButton(ns("btn_mismatch_proceed"),
                       tagList(tags$i(class = "bi bi-play-fill me-1"),
                               "Lanjutkan"),
                       class = "btn-warning",
                       style = "font-weight: 600; color: #1e293b;")
        }
        
        showModal(modalDialog(
          title = tagList(tags$i(class = "bi bi-exclamation-triangle-fill me-2",
                                 style = "color:#b45309;"),
                          "Konsistensi versi SERASI tidak terpenuhi"),
          tags$p(style = "color:#475569; font-size:0.9rem; margin-bottom:12px;",
                 "Modul yang akan digabungkan dibangun terhadap versi indeks SERASI yang berbeda. ",
                 "Menggabungkan tanpa penyelarasan dapat menghasilkan indeks PADU yang tidak konsisten."),
          
          tags$div(
            style = "max-height: 260px; overflow-y: auto;",
            tags$table(
              style = "width: 100%; border-collapse: collapse;",
              tags$thead(
                tags$tr(
                  tags$th(style = paste("text-align: left; padding: 6px 8px;",
                                        "border-bottom: 1px solid #E2E8F0;",
                                        "font-weight: 700; color: #475569;",
                                        "font-size: 0.78rem;"),
                          "Modul"),
                  tags$th(style = paste("text-align: right; padding: 6px 8px;",
                                        "border-bottom: 1px solid #E2E8F0;",
                                        "font-weight: 700; color: #475569;",
                                        "font-size: 0.78rem;"),
                          "Berkas SERASI"),
                  tags$th(style = paste("text-align: right; padding: 6px 8px;",
                                        "border-bottom: 1px solid #E2E8F0;",
                                        "font-weight: 700; color: #475569;",
                                        "font-size: 0.78rem; white-space: nowrap;"),
                          "Status")
                )
              ),
              tags$tbody(rows_ui)
            )
          ),
          
          disclaimer,
          
          tags$div(style = paste("margin-top: 8px; padding: 8px 10px;",
                                 "background:#F8FAFC; border:1px solid #E2E8F0;",
                                 "border-radius:8px; font-size:0.8rem; color:#475569;"),
                   tags$i(class = "bi bi-check-circle me-1"),
                   sprintf("Seragam menggunakan: %s (%d dari %d modul)",
                           cs$rows[[which(sapply(cs$rows, function(x)
                             identical(x$hash, cs$majority_hash)))[1]]]$name,
                           sum(sapply(cs$rows, function(x)
                             identical(x$hash, cs$majority_hash))),
                           length(cs$rows))),
          
          easyClose = FALSE,
          footer = div(
            style = paste("display: flex; justify-content: flex-end;",
                          "align-items: center; gap: 8px; width: 100%;"),
            modalButton("Batalkan"),
            proceed_btn
          )
        ))
        return()
      }
      do_run_combine(NULL)
    })
    
    observeEvent(input$btn_mismatch_proceed, {
      removeModal()
      cs <- consistency()
      majority_keys <- names(cs$rows)[
        sapply(cs$rows, function(x) identical(x$hash, cs$majority_hash))]
      majority_keys <- setdiff(majority_keys, "combine")
      do_run_combine(majority_keys)
    })
    
    output$output_dir_warning <- renderUI({
      if (is.null(output_dir()) || !nzchar(output_dir())) {
        div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-exclamation-triangle me-1"),
            "Direktori output belum diatur. Atur terlebih dahulu di menu utama.")
      }
    })
    
    # ── Status box ──────────────────────────────────────────
    output$status_box <- renderUI({
      disc <- padu_discovery()
      if (!is.null(rv$analysis_result)) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Analisis selesai.")
      } else if (!is.null(serasi_in$idx_serasi_map()) && length(disc$files) > 0) {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Siap menjalankan analisis.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Unggah peta SERASI dan pastikan file PADU tersedia.")
      }
    })
    
    # ── Shared result rendering ─────────────────────────────
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
        "Indeks PADU-RTp", "Indeks PADU-SE", "Indeks PADU-KI",
        "Indeks PADU kombinasi"
      )
    )
    
    render_result_server(input, output, session, rv, padu_combine_config)
  })
}