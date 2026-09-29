# ui/modules/mod_reconcile.R
# ============================================================

source("R/functions.R")
source("R/helpers.R")

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.RECON_MODULE_FOLDER <- "Rekonsiliasi"
.RECON_RDA           <- "log/idx_reconcile_log.rda"
.RECON_PNG_DIR       <- "log"
.RECON_PAGE_SIZE     <- 25L

.locked_panel <- function(msg = "Selesaikan langkah sebelumnya terlebih dahulu.") {
  div(class = "alert alert-secondary mb-0",
      tags$i(class = "bi bi-lock-fill me-2"), msg)
}

.step_nav <- function(ns, back_id = NULL, next_id = NULL, next_label = "Lanjut") {
  div(
    style = "display:flex; justify-content:space-between; margin-top:16px;",
    if (!is.null(back_id)) {
      actionButton(ns(back_id),
                   tagList(tags$i(class = "bi bi-arrow-left me-1"), "Kembali"),
                   class = "btn-outline-secondary btn-sm")
    } else div(),
    if (!is.null(next_id)) {
      actionButton(ns(next_id),
                   tagList(next_label, tags$i(class = "bi bi-arrow-right ms-1")),
                   class = "btn-success btn-sm")
    } else div()
  )
}

.read_spatial_input <- function(file_df) {
  if (is.null(file_df)) return(NULL)
  if (nrow(file_df) == 1 && grepl("\\.gpkg$", file_df$name[1], ignore.case = TRUE)) {
    return(sf::st_read(file_df$datapath[1], quiet = TRUE))
  }
  temp_dir <- tempdir()
  for (i in seq_len(nrow(file_df))) {
    file.copy(file_df$datapath[i], file.path(temp_dir, file_df$name[i]),
              overwrite = TRUE)
  }
  shp_file <- file_df$name[grepl("\\.shp$", file_df$name, ignore.case = TRUE)]
  if (length(shp_file) == 0)
    stop("Komponen file .shp tidak ditemukan. Pilih .shp, .shx, .dbf, dan .prj sekaligus.")
  sf::st_read(file.path(temp_dir, shp_file[1]), quiet = TRUE)
}

.resolve_alpha_from_recommendation <- function(step, output_dir, session,
                                               default_alpha = 0.5) {
  if (identical(as.integer(step), 1L)) {
    mem_key  <- "recommendation"
    log_file <- "idx_alternatives_overlaps.rda"
  } else if (identical(as.integer(step), 2L)) {
    mem_key  <- "recommendation"
    log_file <- "idx_alternatives_adjacent.rda"
  } else {
    return(list(alpha = default_alpha, source = "default (step unknown)"))
  }
  rec <- tryCatch(session$userData$module_results[[mem_key]],
                  error = function(e) NULL)
  if (!is.null(rec) && !is.null(rec$inputs) && !is.null(rec$inputs$alpha)) {
    a <- suppressWarnings(as.numeric(rec$inputs$alpha))
    if (is.finite(a)) return(list(alpha = a, source = "sesi (memori)"))
  }
  if (!is.null(output_dir) && nzchar(output_dir)) {
    log_path <- file.path(output_dir, "Penyusunan Alternatif", "log", log_file)
    if (file.exists(log_path)) {
      res <- tryCatch({
        env <- new.env(parent = emptyenv())
        load(log_path, envir = env)
        if (exists("inputs", envir = env, inherits = FALSE)) {
          inp <- get("inputs", envir = env, inherits = FALSE)
          a <- suppressWarnings(as.numeric(inp$alpha))
          if (is.finite(a)) list(alpha = a, source = "file log") else NULL
        } else NULL
      }, error = function(e) NULL)
      if (!is.null(res)) return(res)
    }
  }
  list(alpha = default_alpha, source = "default (tidak ditemukan)")
}

reconcile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("5. Rekonsiliasi Integrasi Tata Ruang",
         style = "margin: 0; font-weight: 700;"),
      tags$p("Sinkronisasi peta RTRW dan RZWP3K secara otomatis berdasarkan matriks keputusan prioritas wilayah darat dan laut.",
             style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;")
    ),
    
    div(
      id = ns("setup_view"),
      fluidRow(
        class = "g-3",
        column(
          width = 4,
          card(
            card_header("Input & Parameter"),
            accordion(
              id = ns("wizard"), open = "step1", multiple = FALSE,
              accordion_panel(
                "Langkah 1 — Menyiapkan Keputusan Rekonsiliasi",
                value = "step1",
                icon = tags$i(class = "bi bi-file-earmark-spreadsheet-fill"),
                div(
                  class = "laspur-fileinput-with-bar",
                  fileInput(ns("recon_map_file"),
                            label = "Peta Rekomendasi (.gpkg)",
                            accept = ".gpkg"),
                  uiOutput(ns("loaded_file_bar"))
                ),
                fileInput(ns("rtrw_file"), "Peta RTRW (.shp/.gpkg)",
                          accept = c(".gpkg", ".shp", ".shx", ".dbf", ".prj"),
                          multiple = TRUE),
                fileInput(ns("rzwp3k_file"), "Peta RZWP3K (.shp/.gpkg)",
                          accept = c(".gpkg", ".shp", ".shx", ".dbf", ".prj"),
                          multiple = TRUE),
                fileInput(ns("rtrw_priority_file"), "Tabel Acuan Pola RTRW (.xlsx)",
                          accept = ".xlsx"),
                fileInput(ns("rzwp3k_priority_file"), "Tabel Acuan Pola RZWP3K (.xlsx)",
                          accept = ".xlsx"),
                fileInput(ns("serasi_matrix_file"), "Matriks SERASI (.xlsx)",
                          accept = ".xlsx"),
                .step_nav(ns, back_id = NULL, next_id = "btn_next_1",
                          next_label = "Lanjut ke Langkah 2")
              ),
              accordion_panel(
                "Langkah 2 — Menentukan Keputusan Rekonsiliasi",
                value = "step2",
                icon = tags$i(class = "bi bi-check2-circle"),
                uiOutput(ns("step2_ui"))
              )
            )
          )
        ),
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
    ),
    
    div(
      id = ns("workspace_view"),
      style = "display:none;",
      tags$style(HTML("
        .inapp-toolbar-row .form-group,
        .inapp-toolbar-row .shiny-input-container { margin-bottom: 0 !important; }
        .inapp-toolbar-row { display: flex; gap: 8px; align-items: stretch; margin-bottom: 8px; flex-wrap: wrap; }
        .inapp-toolbar-row .inapp-dd  { flex: 1 1 auto; min-width: 0; }
        .inapp-toolbar-row .inapp-btn { flex: 0 0 auto; display: flex; align-items: stretch; }
        .inapp-toolbar-row .inapp-btn .btn { height: 100%; }
        .inapp-table-host { padding-bottom: 0 !important; margin-bottom: 0 !important; }
        .inapp-table-host .html-widget { margin-bottom: 0 !important; }

        .inapp-table-host .rt-table { font-size: 0.72rem; }
        .inapp-table-host .rt-th,
        .inapp-table-host .rt-td {
          font-size: 0.72rem !important;
          padding: 3px 5px !important;
          line-height: 1.2 !important;
          vertical-align: middle;
        }
        .inapp-table-host .rt-th { font-weight: 600; }

        .laspur-recon-green  > .rt-tr, .laspur-recon-green  { background-color: #D4EDDA !important; }
        .laspur-recon-orange > .rt-tr, .laspur-recon-orange { background-color: #FFF3CD !important; }
        .laspur-recon-red    > .rt-tr, .laspur-recon-red    { background-color: #F8D7DA !important; }

        .laspur-recon-green:hover  { background-color: #c3e6cb !important; }
        .laspur-recon-orange:hover { background-color: #ffe8a1 !important; }
        .laspur-recon-red:hover    { background-color: #f1b0b7 !important; }

        .laspur-cell-select {
          width: 100%; max-width: 100%;
          padding: 1px 3px;
          border: 1px solid #CBD5E1;
          border-radius: 4px;
          font-size: 0.7rem;
          background-color: #FFFFFF;
          color: #1E293B;
          box-sizing: border-box;
        }
        .laspur-cell-select:disabled {
          background-color: #F1F5F9;
          color: #94A3B8;
          cursor: not-allowed;
        }
        .laspur-recon-red .laspur-cell-select    { border-color: #DC3545 !important; border-width: 2px !important; }
        .laspur-recon-orange .laspur-cell-select { border-color: #FFC107 !important; }
        .laspur-recon-green .laspur-cell-select  { border-color: #28A745 !important; }

        .laspur-row-check {
          width: 16px; height: 16px; cursor: pointer;
          accent-color: #106665;
        }
        .laspur-row-check:disabled { cursor: not-allowed; opacity: 0.5; }
      ")),
      fluidRow(
        class = "g-3",
        column(
          width = 8,
          card(
            card_header(
              div(
                class = "d-flex justify-content-between align-items-center",
                tagList(tags$i(class = "bi bi-table me-1"), "Keputusan Rekonsiliasi"),
                uiOutput(ns("ws_group_label_short"), inline = TRUE)
              )
            ),
            div(
              class = "inapp-toolbar-row",
              div(class = "inapp-dd",
                  selectInput(ns("ws_group_jump"), NULL,
                              choices = c("Semua grup" = ""),
                              width = "100%")),
              div(class = "inapp-btn",
                  actionButton(ns("ws_finalize_all_btn"),
                               tagList(tags$i(class = "bi bi-check2-square me-1"),
                                       "Finalisasi Semua"),
                               class = "btn-outline-success btn-sm")),
              div(class = "inapp-btn",
                  actionButton(ns("ws_reset_all"),
                               tagList(tags$i(class = "bi bi-arrow-counterclockwise me-1"),
                                       "Reset Keputusan"),
                               class = "btn-danger btn-sm"))
            ),
            uiOutput(ns("ws_filter_chip_ui")),
            uiOutput(ns("ws_summary_ui")),
            div(class = "inapp-table-host",
                reactable::reactableOutput(ns("ws_table"))),
            uiOutput(ns("ws_pagination_ui")),
            div(
              style = paste("display: flex; justify-content: space-between;",
                            "align-items: center; margin-top: 8px; gap: 8px; flex-wrap: wrap;"),
              div(
                style = "display: flex; gap: 8px; flex-wrap: wrap;",
                actionButton(ns("ws_save"),
                             tagList(tags$i(class = "bi bi-save me-1"), "Simpan Keputusan"),
                             class = "btn-primary btn-sm"),
                downloadButton(ns("ws_dl_draft"),
                               label = "Simpan sebagai Draf",
                               class = "btn-outline-secondary btn-sm"),
                uiOutput(ns("ws_run_reconcile_ui"), inline = TRUE)
              ),
              div(
                style = "display: flex; gap: 8px; flex-wrap: wrap;",
                actionButton(ns("btn_ws_back_to_setup"),
                             tagList(tags$i(class = "bi bi-x-lg me-1"), "Tutup Halaman Kerja"),
                             class = "btn-outline-primary btn-sm")
              )
            )
          )
        ),
        column(
          width = 4,
          card(
            card_header(tagList(tags$i(class = "bi bi-map me-1"), "Peta")),
            leaflet::leafletOutput(ns("ws_map"), height = "600px")
          )
        )
      )
    )
  )
}

reconcile_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shinyjs::useShinyjs()
    
    rv <- reactiveValues(
      unlocked = 1,
      detected_step = NULL,
      recon_map = NULL,
      recon_map_source = NULL,
      rtrw_vect = NULL,
      rzwp3k_vect = NULL,
      rtrw_prioritas = NULL,
      rzwp3k_prioritas = NULL,
      serasi_matrix = NULL,
      template_path = NULL,
      resolved_rtrw = NULL,
      resolved_rzwp3k = NULL,
      resolved_integrated = NULL,
      analysis_result = NULL,
      gpkg_path = NULL,
      xlsx_path = NULL,
      log_messages = "",
      final_log = NULL,
      
      workspace_open  = FALSE,
      current_panel   = "step1",
      dec_df          = NULL,
      dec_committed   = NULL,
      dec_saved       = NULL,
      group_order     = integer(0),
      group_selected  = NA_integer_,
      page            = 1L,
      selected_pu     = integer(0),
      sel_source      = "none",
      node_filter     = NULL,
      view_filter     = "unresolved",
      table_nonce     = 0L,
      data_nonce      = 0L,
      fit_nonce       = 0L,
      map_latch       = FALSE,
      map_ready       = FALSE,
      map_nonce       = 0L,
      upload_loaded   = FALSE
    )
    
    go_to_panel <- function(value) {
      rv$workspace_open <- FALSE
      rv$current_panel  <- value
      bslib::accordion_panel_set(id = "wizard", values = value, session = session)
    }
    
    is_workspace_mode <- reactive({
      identical(rv$current_panel, "step2") && isTRUE(rv$workspace_open)
    })
    
    has_unsaved <- reactive({
      if (is.null(rv$dec_df) || is.null(rv$dec_committed)) return(FALSE)
      !identical(rv$dec_df, rv$dec_committed)
    })
    
    map_trigger <- shiny::debounce(reactive({
      rv$map_nonce
      rv$group_selected
      rv$selected_pu
    }), millis = 350)
    
    bump_table <- function() rv$table_nonce <- isolate(rv$table_nonce) + 1L
    
    clear_selection <- function() {
      rv$node_filter <- NULL
      rv$selected_pu <- integer(0)
      rv$sel_source  <- "none"
    }
    
    isTRUE_vec <- function(x) {
      x <- as.logical(x)
      x[is.na(x)] <- FALSE
      x
    }
    
    active_path_val <- reactive({
      ap <- tryCatch(session$userData$active_path, error = function(e) NULL)
      if (is.null(ap)) return("")
      if (is.function(ap)) return(tryCatch(ap(), error = function(e) ""))
      as.character(ap)
    })
    expected_step <- reactive({
      switch(active_path_val(), "overlap" = 1L, "adjacent" = 2L, NULL)
    })
    
    .detect_step_from_sf <- function(map_data) {
      cols <- names(map_data)
      if (any(c("stat_pu", "id_rtrw", "id_rzwp3k") %in% cols)) {
        if (!"length" %in% cols) return(1L)
      }
      if ("length" %in% cols) return(2L)
      NA_integer_
    }
    
    discovered_recon_key <- reactive({
      step <- expected_step()
      if (is.null(step)) return("none")
      expected_case <- if (identical(step, 1L)) "overlaps" else "adjacent"
      rec_res <- tryCatch(session$userData$module_results$recommendation,
                          error = function(e) NULL)
      if (!is.null(rec_res) && !is.null(rec_res$result)) {
        case_in_session <- rec_res$inputs$case %||% ""
        if (!nzchar(case_in_session) || identical(case_in_session, expected_case)) {
          m <- rec_res$result[[if (identical(step, 1L))
            "idx_alternative_overlaps_map" else "idx_alternative_adjacent_map"]]
          if (!is.null(m) && inherits(m, "sf")) return("session")
        }
      }
      if (!is.null(output_dir()) && nzchar(output_dir())) {
        folder <- file.path(output_dir(), "Penyusunan Alternatif")
        expected_file <- if (identical(step, 1L))
          "idx_alternatives_overlaps.gpkg" else "idx_alternatives_adjacent.gpkg"
        f <- file.path(folder, expected_file)
        if (file.exists(f)) return(paste0("file|", expected_file, "|",
                                          as.numeric(file.mtime(f))))
      }
      "none"
    })
    
    .load_recon_from_discovery <- function() {
      key <- discovered_recon_key()
      if (identical(key, "none")) {
        rv$recon_map <- NULL; rv$recon_map_source <- NULL; rv$detected_step <- NULL
        return(invisible(NULL))
      }
      map_data <- NULL; source <- NULL
      if (identical(key, "session")) {
        rec_res <- session$userData$module_results$recommendation
        step    <- expected_step()
        map_data <- if (identical(step, 1L))
          rec_res$result$idx_alternative_overlaps_map
        else
          rec_res$result$idx_alternative_adjacent_map
        map_data <- normalize_legacy_ids(map_data)
        source <- "session"
      } else {
        parts <- strsplit(key, "\\|")[[1]]
        fname <- parts[2]
        f <- file.path(output_dir(), "Penyusunan Alternatif", fname)
        map_data <- tryCatch(sf::st_read(f, quiet = TRUE), error = function(e) NULL)
        if (!is.null(map_data)) map_data <- normalize_legacy_ids(map_data)
        source <- "file"
      }
      if (is.null(map_data)) {
        rv$recon_map <- NULL; rv$recon_map_source <- NULL; rv$detected_step <- NULL
        return(invisible(NULL))
      }
      step <- .detect_step_from_sf(map_data)
      if (is.na(step)) {
        rv$recon_map <- NULL; rv$recon_map_source <- NULL; rv$detected_step <- NULL
        return(invisible(NULL))
      }
      rv$recon_map        <- map_data
      rv$recon_map_source <- source
      rv$detected_step    <- step
      rv$unlocked         <- 1
    }
    
    observeEvent(discovered_recon_key(), {
      if (identical(rv$recon_map_source, "manual")) return()
      .load_recon_from_discovery()
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    output$loaded_file_bar <- renderUI({
      detected_name <- if (identical(rv$detected_step, 1L)) {
        "idx_alternatives_overlaps.gpkg"
      } else if (identical(rv$detected_step, 2L)) {
        "idx_alternatives_adjacent.gpkg"
      } else {
        "idx_alternatives.gpkg"
      }
      render_loaded_file_bar(
        state    = rv$recon_map_source,
        input_id = ns("recon_map_file"),
        filename = detected_name
      )
    })
    
    observeEvent(input$recon_map_file, {
      req(input$recon_map_file)
      rv$template_path <- NULL
      rv$unlocked <- 1
      rv$dec_df <- NULL; rv$dec_committed <- NULL; rv$dec_saved <- NULL
      tryCatch({
        map_data <- sf::st_read(input$recon_map_file$datapath, quiet = TRUE)
        map_data <- normalize_legacy_ids(map_data)
        step <- .detect_step_from_sf(map_data)
        if (is.na(step)) stop("Kolom penanda struktural tidak ditemukan.")
        exp_step <- expected_step()
        if (!is.null(exp_step) && step != exp_step) {
          rv$recon_map <- NULL; rv$recon_map_source <- NULL; rv$detected_step <- NULL
          showNotification(sprintf(
            "Berkas yang Anda unggah adalah STEP %d, jalur aktif STEP %d.",
            step, exp_step), type = "error", duration = 10)
          return()
        }
        rv$recon_map <- map_data
        rv$recon_map_source <- "manual"
        rv$detected_step <- step
        
        fname <- input$recon_map_file$name
        fname <- if (length(fname) > 1) sprintf("%d files", length(fname)) else fname[1]
        session$sendCustomMessage("set_fileinput_text", list(
          input_id = ns("recon_map_file"), filename = fname))
        showNotification(sprintf(
          "Peta Rekomendasi (manual) terdeteksi sebagai STEP %d (%s).",
          step, if (step == 1) "Overlaps" else "Adjacent"),
          type = "message")
      }, error = function(e) {
        rv$recon_map <- NULL; rv$recon_map_source <- NULL; rv$detected_step <- NULL
        showNotification(paste("Gagal Memvalidasi Berkas:", e$message),
                         type = "error", duration = NULL)
      })
    })
    
    observeEvent(input$rtrw_file, {
      req(input$rtrw_file)
      rv$rtrw_vect <- .read_spatial_input(input$rtrw_file)
    })
    observeEvent(input$rzwp3k_file, {
      req(input$rzwp3k_file)
      rv$rzwp3k_vect <- .read_spatial_input(input$rzwp3k_file)
    })
    observeEvent(input$rtrw_priority_file, {
      req(input$rtrw_priority_file)
      rv$rtrw_prioritas <- openxlsx::read.xlsx(input$rtrw_priority_file$datapath)
    })
    observeEvent(input$rzwp3k_priority_file, {
      req(input$rzwp3k_priority_file)
      rv$rzwp3k_prioritas <- openxlsx::read.xlsx(input$rzwp3k_priority_file$datapath)
    })
    observeEvent(input$serasi_matrix_file, {
      req(input$serasi_matrix_file)
      rv$serasi_matrix <- load_validate_matrix_table(
        input$serasi_matrix_file$datapath, title = "serasi")
    })
    
    observeEvent(input$recon_table_filled_file, {
      req(input$recon_table_filled_file)
      tryCatch({
        tbl  <- load_and_validate_table(input$recon_table_filled_file$datapath)
        step <- rv$detected_step
        
        if (identical(step, 2L)) {
          is_dissolved <- all(c("id_rtrw", "id_rzwp3k") %in% names(tbl)) &&
            !"id" %in% names(tbl)
          df <- if (is_dissolved) {
            as.data.frame(tbl, stringsAsFactors = FALSE)
          } else {
            as.data.frame(dissolve_adjacent_pairs(tbl),
                          stringsAsFactors = FALSE)
          }
          if (!"user_decision_rtrw"   %in% names(df)) df$user_decision_rtrw   <- df$RTRW
          if (!"user_decision_rzwp3k" %in% names(df)) df$user_decision_rzwp3k <- df$RZWP3K
        } else {
          if (!"user_decision" %in% names(tbl))
            stop("Kolom 'user_decision' tidak ditemukan pada berkas.")
          df <- as.data.frame(tbl, stringsAsFactors = FALSE)
        }
        
        if (!"finalized" %in% names(df)) df$finalized <- FALSE
        if (!"locked" %in% names(df)) {
          use_rec <- if ("use_recommendation" %in% names(df))
            as.character(df$use_recommendation) else rep("Tidak", nrow(df))
          df$locked <- !is.na(use_rec) & use_rec == "Ya"
        }
        df$finalized <- as.logical(df$finalized)
        df$finalized[is.na(df$finalized)] <- FALSE
        df$locked    <- as.logical(df$locked)
        df$locked[is.na(df$locked)] <- FALSE
        
        rv$dec_df        <- df
        rv$dec_committed <- df
        rv$dec_saved     <- NULL
        rv$upload_loaded <- TRUE
        
        if (identical(step, 2L) && "id_group" %in% names(df)) {
          rv$group_order    <- sort(unique(as.integer(df$id_group)))
          rv$group_selected <- rv$group_order[1]
        } else {
          rv$group_order    <- integer(0)
          rv$group_selected <- NA_integer_
        }
        rv$page         <- 1L
        rv$view_filter  <- "unresolved"
        clear_selection()
        rv$data_nonce   <- isolate(rv$data_nonce) + 1L
        
        showNotification(
          sprintf("Templat dimuat: %d baris. Klik 'Buka Halaman Kerja' untuk meninjau.",
                  nrow(df)),
          type = "message", duration = 5)
      }, error = function(e) {
        rv$upload_loaded <- FALSE
        rv$dec_df        <- NULL
        rv$dec_committed <- NULL
        showNotification(paste("Gagal memuat templat:", e$message),
                         type = "error", duration = 8)
      })
    })
    
    observeEvent(input$btn_make_template, {
      if (is.null(output_dir()) || !nzchar(output_dir()) ||
          !validate_output_dir(output_dir())) {
        showNotification("Direktori output belum diatur.", type = "error", duration = 6)
        return()
      }
      missing_items <- character(0)
      if (is.null(rv$recon_map))        missing_items <- c(missing_items, "Peta Rekomendasi (.gpkg)")
      if (is.null(rv$detected_step))    missing_items <- c(missing_items, "Deteksi tahap (Step 1/2)")
      if (is.null(rv$rtrw_prioritas))   missing_items <- c(missing_items, "Tabel Acuan Pola RTRW (.xlsx)")
      if (is.null(rv$rzwp3k_prioritas)) missing_items <- c(missing_items, "Tabel Acuan Pola RZWP3K (.xlsx)")
      if (length(missing_items) > 0) {
        showNotification(tagList(
          tags$strong("Templat belum dapat dibuat. Input belum diunggah:"),
          tags$ul(style = "margin: 6px 0 0 0; padding-left: 20px;",
                  lapply(missing_items, function(x) tags$li(x)))
        ), type = "warning", duration = 10)
        return()
      }
      rv$template_path <- NULL
      withProgress(message = "Membuat Templat Rekonsiliasi", value = 0, {
        tryCatch({
          incProgress(0.2, detail = "Menyiapkan direktori modul...")
          module_dir <- file.path(output_dir(), .RECON_MODULE_FOLDER)
          dir.create(module_dir, recursive = TRUE, showWarnings = FALSE)
          file_name <- if (rv$detected_step == 1) "overlaps_reconcilliation_table.xlsx"
          else                        "adjacent_reconcilliation_table.xlsx"
          incProgress(0.5, detail = "Menjalankan pembuat templat...")
          generate_reconciliation_excel(
            recon_map        = rv$recon_map,
            rtrw_prioritas   = rv$rtrw_prioritas,
            rzwp3k_prioritas = rv$rzwp3k_prioritas,
            output_dir       = module_dir,
            step             = rv$detected_step,
            file_name        = file_name)
          incProgress(0.8, detail = "Verifikasi berkas templat...")
          generated_path <- file.path(module_dir, file_name)
          if (!file.exists(generated_path)) stop("File templat gagal dibuat.")
          rv$template_path <- generated_path
          showNotification("Templat Rekonsiliasi Berhasil Dibuat.", type = "message")
          incProgress(1.0, detail = "Selesai!")
        }, error = function(e) {
          showNotification(paste("Gagal membuat templat:", e$message),
                           type = "error", duration = 10)
        })
      })
    })
    
    output$template_status_ui <- renderUI({
      req(rv$template_path)
      div(class = "alert alert-success mb-0 mt-2",
          tags$i(class = "bi bi-check-circle me-2"),
          sprintf("Templat Siap (%s): %s",
                  paste0("Step ", rv$detected_step), basename(rv$template_path)))
    })
    
    output$dl_template <- downloadHandler(
      filename = function() {
        if (!is.null(rv$template_path)) basename(rv$template_path)
        else "reconcilliation_table.xlsx"
      },
      content = function(file) {
        req(rv$template_path)
        file.copy(rv$template_path, file, overwrite = TRUE)
      }
    )
    
    observeEvent(input$btn_next_1, {
      if (is.null(rv$recon_map)) {
        showNotification("Harap unggah Peta Rekomendasi terlebih dahulu.", type = "warning")
        return()
      }
      if (is.null(rv$rtrw_vect) || is.null(rv$rzwp3k_vect)) {
        showNotification("Harap unggah Peta RTRW dan RZWP3K terlebih dahulu.", type = "warning")
        return()
      }
      if (is.null(rv$rtrw_prioritas) || is.null(rv$rzwp3k_prioritas)) {
        showNotification("Harap unggah tabel acuan pola RTRW dan RZWP3K.", type = "warning")
        return()
      }
      if (is.null(rv$serasi_matrix)) {
        showNotification("Harap unggah Matriks SERASI (.xlsx).", type = "warning")
        return()
      }
      rv$unlocked <- max(rv$unlocked, 2)
      go_to_panel("step2")
    })
    
    resolved_alpha_info <- reactive({
      req(rv$detected_step)
      .resolve_alpha_from_recommendation(
        step = rv$detected_step, output_dir = output_dir(), session = session)
    })
    
    output$step2_ui <- renderUI({
      alpha_info  <- resolved_alpha_info()
      alpha_txt   <- sprintf("Alpha (\u03B1) dari modul Penyusunan Alternatif: %.2f  [%s]",
                             alpha_info$alpha, alpha_info$source)
      alpha_style <- if (grepl("default", alpha_info$source)) {
        "background-color: #FEF3C7; border-color: #FDE68A; color: #92400E;"
      } else {
        "background-color: #eef6fc; border-color: #cfe3f5; color: #1b75ba;"
      }
      tagList(
        radioButtons(ns("decision_mode"), "Mode Penentuan Keputusan",
                     choices = c("Gunakan Halaman Kerja" = "inapp",
                                 "Gunakan Templat"       = "manual"),
                     selected = "inapp", inline = FALSE),
        
        conditionalPanel(
          condition = paste0("input['", ns("decision_mode"), "'] == 'manual'"),
          tags$h6("Tabel Keputusan Rekonsiliasi (Legacy)",
                  style = "font-weight: 600; margin: 0 0 8px 0;"),
          div(style = "display: flex; gap: 8px; flex-wrap: wrap; margin-bottom: 10px;",
              actionButton(ns("btn_make_template"),
                           tagList(tags$i(class = "bi bi-file-earmark-spreadsheet me-1"),
                                   "Buat Templat"),
                           class = "btn-outline-primary btn-sm"),
              downloadButton(ns("dl_template"), "Unduh Templat",
                             class = "btn-outline-success btn-sm")),
          uiOutput(ns("template_status_ui")),
          hr(),
          fileInput(ns("recon_table_filled_file"),
                    "Unggah Tabel Keputusan Rekonsiliasi (.xlsx)",
                    accept = ".xlsx")
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("decision_mode"), "'] == 'inapp'"),
          actionButton(ns("btn_prepare_decisions"),
                       tagList(tags$i(class = "bi bi-play-fill me-1"),
                               "Siapkan Keputusan"),
                       class = "btn-primary btn-sm",
                       style = "width: 100%; font-weight: 600;")
        ),
        
        uiOutput(ns("inapp_ready_hint_ui")),
        
        div(class = "alert",
            style = paste("font-size: 0.85rem; padding: 8px 12px; margin-top: 8px;",
                          alpha_style),
            tags$i(class = "bi bi-info-circle me-1"), alpha_txt),
        
        if (is.null(output_dir()) || !nzchar(output_dir()) ||
            !validate_output_dir(output_dir())) {
          div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
              tags$i(class = "bi bi-exclamation-triangle me-1"),
              "Direktori output belum diatur.")
        },
        
        div(style = "margin-top: 10px;",
            actionButton(ns("btn_run_reconcile"),
                         tagList(tags$i(class = "bi bi-play-fill me-1"),
                                 "Lakukan Rekonsiliasi"),
                         class = "btn-success btn-sm")),
        .step_nav(ns, back_id = "btn_back_2", next_id = NULL)
      )
    })
    
    output$inapp_ready_hint_ui <- renderUI({
      mode    <- input$decision_mode %||% "inapp"
      has_dec <- !is.null(rv$dec_df)
      
      if (identical(mode, "inapp") && has_dec) {
        div(class = "alert alert-info mb-2 mt-2", style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-check-circle me-1"),
            sprintf("Keputusan siap: %d baris. ", nrow(rv$dec_df)),
            actionButton(ns("btn_reopen_workspace"), "Buka Halaman Kerja",
                         class = "btn-outline-primary btn-sm ms-2"))
      } else if (identical(mode, "manual") && has_dec && isTRUE(rv$upload_loaded)) {
        div(class = "alert alert-info mb-2 mt-2", style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-check-circle me-1"),
            sprintf("Templat dimuat: %d baris. ", nrow(rv$dec_df)),
            actionButton(ns("btn_reopen_workspace"),
                         "Buka Halaman Kerja untuk Meninjau",
                         class = "btn-outline-primary btn-sm ms-2"))
      } else if (identical(mode, "inapp") && !has_dec) {
        div(class = "alert alert-secondary mb-2 mt-2", style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-info-circle me-1"),
            "Klik 'Siapkan Keputusan' untuk memuat opsi.")
      } else if (identical(mode, "manual") && !has_dec) {
        div(class = "alert alert-secondary mb-2 mt-2", style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-info-circle me-1"),
            "Unggah tabel keputusan yang sudah diisi untuk meninjau di Halaman Kerja.")
      } else {
        NULL
      }
    })
    
    outputOptions(output, "inapp_ready_hint_ui", suspendWhenHidden = FALSE)
    
    observeEvent(input$btn_reopen_workspace, { rv$workspace_open <- TRUE })
    
    observeEvent(input$btn_back_2, go_to_panel("step1"))
    
    observeEvent(input$btn_prepare_decisions, {
      if (is.null(output_dir()) || !nzchar(output_dir()) ||
          !validate_output_dir(output_dir())) {
        showNotification("Direktori output belum diatur.", type = "error", duration = 5)
        return()
      }
      if (is.null(rv$recon_map) || is.null(rv$detected_step)) {
        showNotification("Peta rekomendasi belum tersedia.", type = "warning")
        return()
      }
      if (is.null(rv$rtrw_prioritas) || is.null(rv$rzwp3k_prioritas)) {
        showNotification("Tabel acuan pola RTRW/RZWP3K belum diunggah.",
                         type = "warning")
        return()
      }
      
      btn_sel <- sprintf("#%s", ns("btn_prepare_decisions"))
      shinyjs::runjs(sprintf("$('%s').addClass('laspur-btn-loading').prop('disabled', true);",
                             btn_sel))
      on.exit({
        shinyjs::runjs(sprintf("$('%s').removeClass('laspur-btn-loading').prop('disabled', false);",
                               btn_sel))
      }, add = TRUE)
      
      withProgress(message = "Menyiapkan Keputusan", value = 0, {
        tryCatch({
          incProgress(0.3, detail = "Membangun dataframe keputusan...")
          step <- rv$detected_step
          
          if (step == 2L) {
            incProgress(0.1, detail = "Mengubah ke bentuk terpisah (1 baris per pasangan)...")
            dissolved <- tryCatch(
              dissolve_adjacent_pairs(rv$recon_map),
              error = function(e)
                stop("Gagal dissolve adjacent pairs: ", e$message))
            df <- as.data.frame(dissolved, stringsAsFactors = FALSE)
            
            use_rec <- if ("use_recommendation" %in% names(df))
              as.character(df$use_recommendation) else rep("Tidak", nrow(df))
            
            default_rtrw <- if ("alt_RTRW" %in% names(df))
              as.character(df$alt_RTRW) else as.character(df$RTRW)
            default_rz   <- if ("alt_RZWP3K" %in% names(df))
              as.character(df$alt_RZWP3K) else as.character(df$RZWP3K)
            
            fill_r <- is.na(default_rtrw) | !nzchar(default_rtrw)
            fill_z <- is.na(default_rz)   | !nzchar(default_rz)
            default_rtrw[fill_r] <- as.character(df$RTRW)[fill_r]
            default_rz[fill_z]   <- as.character(df$RZWP3K)[fill_z]
            
            is_locked <- !is.na(use_rec) & use_rec == "Ya"
            
            df$user_decision_rtrw   <- default_rtrw
            df$user_decision_rzwp3k <- default_rz
            df$finalized            <- FALSE
            df$locked               <- is_locked
          } else {
            df <- sf::st_drop_geometry(rv$recon_map)
            df <- as.data.frame(df, stringsAsFactors = FALSE)
            
            use_rec <- if ("use_recommendation" %in% names(df))
              as.character(df$use_recommendation) else rep("Tidak", nrow(df))
            rec <- if ("recommendation" %in% names(df))
              as.character(df$recommendation) else rep(NA_character_, nrow(df))
            
            alt_r <- if ("alt_RTRW" %in% names(df))
              as.character(df$alt_RTRW) else as.character(df$RTRW)
            alt_z <- if ("alt_RZWP3K" %in% names(df))
              as.character(df$alt_RZWP3K) else as.character(df$RZWP3K)
            
            default_decision <- dplyr::case_when(
              !is.na(use_rec) & use_rec == "Ya" & !is.na(rec) & rec == "Ubah RTRW" ~ alt_r,
              !is.na(use_rec) & use_rec == "Ya" & !is.na(rec) & rec == "Ubah RZ"   ~ alt_z,
              TRUE ~ as.character(df$RTRW)
            )
            fill_d <- is.na(default_decision) | !nzchar(default_decision)
            default_decision[fill_d] <- as.character(df$RTRW)[fill_d]
            
            is_locked <- !is.na(use_rec) & use_rec == "Ya"
            
            df$user_decision <- default_decision
            df$finalized     <- FALSE
            df$locked        <- is_locked
          }
          
          rv$dec_df        <- df
          rv$dec_committed <- df
          rv$dec_saved     <- NULL
          
          grp_col <- if (step == 2L && "id_group" %in% names(df)) df$id_group else NULL
          if (!is.null(grp_col)) {
            rv$group_order <- sort(unique(as.integer(grp_col)))
            rv$group_selected <- rv$group_order[1]
          } else {
            rv$group_order <- integer(0)
            rv$group_selected <- NA_integer_
          }
          rv$page <- 1L
          clear_selection()
          rv$view_filter <- "unresolved"
          
          rv$data_nonce <- isolate(rv$data_nonce) + 1L
          rv$workspace_open <- TRUE
          
          incProgress(1.0, detail = "Selesai!")
          showNotification(
            sprintf("Keputusan siap: %d baris. Membuka Halaman Kerja...",
                    nrow(df)),
            type = "message", duration = 4)
        }, error = function(e) {
          showNotification(paste("Gagal menyiapkan keputusan:", e$message),
                           type = "error", duration = 10)
        })
      })
    })
    
    observeEvent(rv$workspace_open, {
      if (isTRUE(rv$workspace_open)) {
        shinyjs::runjs(sprintf("$('#%s').hide(); $('#%s').show();",
                               ns("setup_view"), ns("workspace_view")))
        if (!isTRUE(rv$map_latch)) rv$map_latch <- TRUE
        shinyjs::delay(450, {
          rv$table_nonce <- isolate(rv$table_nonce) + 1L
          rv$data_nonce  <- isolate(rv$data_nonce)  + 1L
          rv$fit_nonce   <- isolate(rv$fit_nonce)   + 1L
          shinyjs::runjs(sprintf("
            try { var rt = HTMLWidgets.find('#%s'); if (rt && rt.resize) rt.resize(); } catch(e) {}
            try { var lf = HTMLWidgets.find('#%s'); if (lf && lf.resize) lf.resize(); } catch(e) {}
            setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 60);
          ", ns("ws_table"), ns("ws_map")))
        })
      } else {
        shinyjs::runjs(sprintf("$('#%s').show(); $('#%s').hide();",
                               ns("setup_view"), ns("workspace_view")))
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$btn_ws_back_to_setup, { rv$workspace_open <- FALSE })
    
    row_states <- reactive({
      rv$data_nonce
      df <- rv$dec_df
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      step <- rv$detected_step
      n <- nrow(df)
      conflict <- rep(FALSE, n)
      
      if (identical(step, 2L) &&
          all(c("id_rtrw", "id_rzwp3k", "user_decision_rtrw",
                "user_decision_rzwp3k") %in% names(df))) {
        rtrw_groups <- split(seq_len(n), as.character(df$id_rtrw))
        for (idx in rtrw_groups) {
          if (length(idx) < 2) next
          vals <- unique(stats::na.omit(df$user_decision_rtrw[idx]))
          if (length(vals) > 1) conflict[idx] <- TRUE
        }
        rz_groups <- split(seq_len(n), as.character(df$id_rzwp3k))
        for (idx in rz_groups) {
          if (length(idx) < 2) next
          vals <- unique(stats::na.omit(df$user_decision_rzwp3k[idx]))
          if (length(vals) > 1) conflict[idx] <- TRUE
        }
      }
      
      state <- ifelse(conflict, "red",
                      ifelse(isTRUE_vec(df$finalized), "green", "orange"))
      state
    })
    
    set_group <- function(new_grp) {
      rv$group_selected <- new_grp
      rv$page <- 1L
      clear_selection()
    }
    
    group_choices <- function() {
      df <- isolate(rv$dec_df)
      if (is.null(df) || !"id_group" %in% names(df)) {
        return(c("Semua grup" = ""))
      }
      order <- isolate(rv$group_order)
      if (length(order) == 0) order <- sort(unique(as.integer(df$id_group)))
      order <- sort(unique(as.integer(order)))
      counts <- table(df$id_group)
      labels <- sprintf("Grup %d  \u00b7  %d pasang",
                        order, as.integer(counts[as.character(order)]))
      c("Semua grup" = "", stats::setNames(as.character(order), labels))
    }
    
    output$ws_group_label_short <- renderUI({
      rv$data_nonce
      df <- isolate(rv$dec_df)
      if (is.null(df)) return(NULL)
      cur <- rv$group_selected
      n <- if (is.na(cur) || !"id_group" %in% names(df)) nrow(df)
      else sum(as.integer(df$id_group) == cur, na.rm = TRUE)
      tags$small(style = "color:#6c757d;", sprintf("%d baris", n))
    })
    
    observeEvent(input$ws_group_jump, {
      val <- input$ws_group_jump
      new_grp <- if (is.null(val) || !nzchar(val)) NA_integer_ else as.integer(val)
      if (!identical(rv$group_selected, new_grp)) set_group(new_grp)
    }, ignoreInit = TRUE)
    
    observeEvent(rv$dec_df, {
      req(rv$dec_df)
      updateSelectInput(
        session, "ws_group_jump",
        choices  = group_choices(),
        selected = {
          g <- rv$group_selected
          if (is.null(g) || is.na(g)) "" else as.character(g)
        }
      )
    }, ignoreInit = TRUE)
    
    ws_filtered_idx <- reactive({
      rv$data_nonce
      df <- isolate(rv$dec_df)
      if (is.null(df)) return(integer(0))
      keep <- rep(TRUE, nrow(df))
      cur <- rv$group_selected
      if (!is.na(cur) && "id_group" %in% names(df)) {
        keep <- keep & as.integer(df$id_group) == cur
      }
      if (identical(rv$view_filter, "unresolved")) {
        states <- isolate(row_states())
        if (!is.null(states) && length(states) == nrow(df)) {
          keep <- keep & states %in% c("orange", "red")
        }
      }
      nf <- rv$node_filter
      if (!is.null(nf)) keep <- keep & df$id_pu %in% nf$pu
      which(keep)
    })
    
    ws_paged <- reactive({
      rv$table_nonce
      idx <- ws_filtered_idx()
      df <- isolate(rv$dec_df)
      if (is.null(df) || length(idx) == 0) return(df[0, , drop = FALSE])
      total_pages <- max(1L, ceiling(length(idx) / .RECON_PAGE_SIZE))
      page <- min(max(1L, rv$page), total_pages)
      rows <- idx[seq.int((page - 1L) * .RECON_PAGE_SIZE + 1L,
                          min(page * .RECON_PAGE_SIZE, length(idx)))]
      df[rows, , drop = FALSE]
    })
    
    output$ws_pagination_ui <- renderUI({
      n <- length(ws_filtered_idx())
      total_pages <- max(1L, ceiling(n / .RECON_PAGE_SIZE))
      page <- min(max(1L, rv$page), total_pages)
      div(style = "display:flex; justify-content:space-between; align-items:center; margin-top:4px;",
          actionButton(ns("ws_prev"), "\u2039 Prev", class = "btn-outline-secondary btn-sm"),
          tags$span(sprintf("Halaman %d / %d (%d baris)", page, total_pages, n),
                    style = "font-size:0.8rem; color:#495057;"),
          actionButton(ns("ws_next"), "Next \u203a", class = "btn-outline-secondary btn-sm"))
    })
    observeEvent(input$ws_prev, { rv$page <- max(1L, rv$page - 1L) })
    observeEvent(input$ws_next, {
      total_pages <- max(1L, ceiling(length(ws_filtered_idx()) / .RECON_PAGE_SIZE))
      rv$page <- min(total_pages, rv$page + 1L)
    })
    
    output$ws_filter_chip_ui <- renderUI({
      rv$data_nonce; rv$table_nonce
      states <- row_states()
      if (is.null(states)) return(NULL)
      n_or <- sum(states == "orange"); n_rd <- sum(states == "red")
      nf   <- rv$node_filter
      tagList(
        div(class = "d-flex align-items-center gap-2 mb-2",
            style = "flex-wrap: wrap;",
            if (identical(rv$view_filter, "unresolved")) {
              tags$span(class = "badge",
                        style = "background-color:#FFF3CD; color:#856404; border:1px solid #FFC107; padding:4px 10px; font-weight:600; font-size:0.78rem;",
                        tags$i(class = "bi bi-funnel-fill me-1"),
                        sprintf("Hanya belum terekonsiliasi (%d oranye, %d merah)", n_or, n_rd))
            } else {
              tags$span(class = "badge",
                        style = "background-color:#E2E8F0; color:#475569; padding:4px 10px; font-weight:600; font-size:0.78rem;",
                        tags$i(class = "bi bi-list-ul me-1"),
                        "Tampilkan semua")
            },
            actionButton(ns("ws_toggle_filter"),
                         if (identical(rv$view_filter, "unresolved"))
                           "Tampilkan semua" else "Sembunyikan yang terekonsiliasi",
                         class = "btn-outline-secondary btn-sm py-0")),
        if (!is.null(nf)) {
          div(class = "alert alert-warning py-1 px-2 mb-2 d-flex justify-content-between align-items-center",
              style = "font-size: 0.8rem;",
              tags$span(tags$i(class = "bi bi-geo-alt-fill me-1"),
                        sprintf("Filter fitur %s (%s) \u2014 %d pasang",
                                nf$label, nf$side, length(nf$pu))),
              actionButton(ns("ws_clear_node_filter"), "Hapus filter",
                           class = "btn-outline-secondary btn-sm py-0"))
        }
      )
    })
    
    observeEvent(input$ws_toggle_filter, {
      rv$view_filter <- if (identical(rv$view_filter, "unresolved"))
        "all" else "unresolved"
      rv$page <- 1L
    })
    
    observeEvent(input$ws_clear_node_filter, {
      rv$page <- 1L
      rv$node_filter <- NULL
      rv$selected_pu <- integer(0)
      rv$sel_source  <- "none"
    })
    
    output$ws_summary_ui <- renderUI({
      rv$data_nonce; rv$table_nonce
      df <- isolate(rv$dec_df); req(df)
      states <- row_states()
      n_total  <- nrow(df)
      n_green  <- sum(states == "green",  na.rm = TRUE)
      n_orange <- sum(states == "orange", na.rm = TRUE)
      n_red    <- sum(states == "red",    na.rm = TRUE)
      n_shown  <- length(ws_filtered_idx())
      div(
        class = "alert alert-info mb-2",
        style = paste("font-size: 0.8rem; padding: 6px 10px;",
                      "display: flex; justify-content: space-between;",
                      "align-items: center; gap: 12px; flex-wrap: wrap;"),
        tags$div(
          style = "font-weight: 600; white-space: nowrap;",
          sprintf("Total: %d \u00b7 Ditampilkan: %d", n_total, n_shown)
        ),
        tags$div(
          style = "white-space: nowrap;",
          tags$span(style = "color:#28A745; font-weight:600;",
                    sprintf("\u25CF Terekonsiliasi: %d", n_green)),
          tags$span(style = "color:#FFC107; font-weight:600; margin-left:10px;",
                    sprintf("\u25CF Belum Terekonsiliasi: %d", n_orange)),
          tags$span(style = "color:#DC3545; font-weight:600; margin-left:10px;",
                    sprintf("\u25CF Konflik: %d", n_red))
        )
      )
    })
    
    output$ws_table <- reactable::renderReactable({
      req(is_workspace_mode())
      df <- ws_paged()
      if (is.null(df) || nrow(df) == 0) {
        states_all  <- isolate(row_states())
        dec_all     <- isolate(rv$dec_df)
        cur_grp     <- isolate(rv$group_selected)
        empty_msg   <- "Tidak ada baris untuk filter ini"
        if (!is.null(states_all) && !is.null(dec_all) && nrow(dec_all) > 0) {
          in_group <- if (is.na(cur_grp) || !"id_group" %in% names(dec_all)) {
            rep(TRUE, nrow(dec_all))
          } else {
            as.integer(dec_all$id_group) == cur_grp
          }
          if (any(in_group) &&
              all(states_all[in_group] == "green", na.rm = TRUE)) {
            empty_msg <- "Semua pasangan telah terekonsiliasi"
          }
        }
        return(reactable::reactable(
          data.frame(Info = empty_msg),
          outlined = TRUE, compact = TRUE, bordered = TRUE))
      }
      
      states_all <- isolate(row_states())
      row_idx    <- match(as.integer(df$id_pu),
                          as.integer(isolate(rv$dec_df)$id_pu))
      row_state  <- states_all[row_idx]
      
      rtrw_pool <- unique(as.character(isolate(rv$rtrw_prioritas)$RTRW))
      rz_pool   <- unique(as.character(isolate(rv$rzwp3k_prioritas)$RZWP3K))
      rtrw_pool <- rtrw_pool[!is.na(rtrw_pool) & nzchar(rtrw_pool)]
      rz_pool   <- rz_pool[!is.na(rz_pool)     & nzchar(rz_pool)]
      combined_pool <- unique(c(rtrw_pool, rz_pool))
      
      esc <- function(x) htmltools::htmlEscape(as.character(x), attribute = TRUE)
      
      make_select_html <- function(id_pu, col, current, opts, locked) {
        cur <- if (is.null(current) || length(current) == 0 || is.na(current)) ""
        else as.character(current)
        opts <- as.character(opts)
        if (nzchar(cur) && !cur %in% opts) opts <- c(cur, opts)
        opts <- unique(opts)
        if (length(opts) == 0) opts <- cur
        opt_tags <- vapply(opts, function(v) {
          sprintf('<option value="%s"%s>%s</option>',
                  esc(v), if (identical(v, cur)) ' selected' else '', esc(v))
        }, character(1))
        disabled <- if (isTRUE(locked)) ' disabled' else ''
        input_id <- ns("ws_cell_change")
        pu_int   <- suppressWarnings(as.integer(id_pu))
        if (is.na(pu_int)) pu_int <- -1L
        onchange_js <- sprintf(
          "Shiny.setInputValue('%s', {id_pu: %d, col: '%s', value: this.value, nonce: Math.random()}, {priority: 'event'});",
          input_id, pu_int, col)
        sprintf(
          '<select class="laspur-cell-select" data-id_pu="%d" data-col="%s"%s onchange="%s">%s</select>',
          pu_int, col, disabled, onchange_js, paste0(opt_tags, collapse = ""))
      }
      
      make_checkbox_html <- function(id_pu, checked, disabled) {
        pu_int <- suppressWarnings(as.integer(id_pu))
        if (is.na(pu_int)) pu_int <- -1L
        onchange_js <- sprintf(
          "Shiny.setInputValue('%s', {id_pu: %d, checked: this.checked, nonce: Math.random()}, {priority: 'event'});",
          ns("ws_finalize_change"), pu_int)
        sprintf(
          '<input type="checkbox" class="laspur-row-check" data-id_pu="%d"%s%s onchange="%s">',
          pu_int,
          if (isTRUE(checked)) ' checked' else '',
          if (isTRUE(disabled)) ' disabled' else '',
          onchange_js)
      }
      
      step <- isolate(rv$detected_step)
      locked_vec <- isTRUE_vec(df$locked)
      locked_vec[row_state == "red"] <- FALSE
      
      display <- df
      if (step == 2L) {
        display$user_decision_rtrw <- vapply(seq_len(nrow(df)), function(i) {
          make_select_html(df$id_pu[i], "user_decision_rtrw",
                           df$user_decision_rtrw[i], rtrw_pool,
                           locked_vec[i])
        }, character(1))
        display$user_decision_rzwp3k <- vapply(seq_len(nrow(df)), function(i) {
          make_select_html(df$id_pu[i], "user_decision_rzwp3k",
                           df$user_decision_rzwp3k[i], rz_pool,
                           locked_vec[i])
        }, character(1))
      } else {
        display$user_decision <- vapply(seq_len(nrow(df)), function(i) {
          make_select_html(df$id_pu[i], "user_decision",
                           df$user_decision[i], combined_pool,
                           locked_vec[i])
        }, character(1))
      }
      
      display$finalized <- vapply(seq_len(nrow(df)), function(i) {
        make_checkbox_html(df$id_pu[i], df$finalized[i], FALSE)
      }, character(1))
      
      helper_numerics <- c(
        "length", "area_ha_rtrw", "area_ha_rzwp3k",
        "idx_serasi", "idx_serasi_new",
        "idx_padu_final",
        "idx_padan", "idx_padan_new",
        "idx_padu_ke", "idx_padu_hs", "idx_padu_kl", "idx_padu_kh",
        "idx_padu_rtp", "idx_padu_se", "idx_padu_ki")
      for (col in helper_numerics) {
        if (!col %in% names(display)) display[[col]] <- NA_real_
        display[[col]] <- suppressWarnings(as.numeric(display[[col]]))
      }
      
      display$.row_state <- row_state
      
      if (step == 2L) {
        desired_order <- c(
          "id_rtrw", "id_rzwp3k", "recommendation",
          "user_decision_rtrw", "user_decision_rzwp3k", "finalized",
          "id_pu", "RTRW", "RZWP3K", "use_recommendation",
          "admin", "length",
          "area_ha_rtrw", "area_ha_rzwp3k",
          "idx_serasi", "idx_serasi_new",
          "idx_padu_final",
          "idx_padan", "idx_padan_new",
          "idx_padu_ke", "idx_padu_hs", "idx_padu_kl", "idx_padu_kh",
          "idx_padu_rtp", "idx_padu_se", "idx_padu_ki",
          ".row_state")
      } else {
        desired_order <- c(
          "id_rtrw", "id_rzwp3k", "recommendation",
          "user_decision", "finalized",
          "id_pu", "RTRW", "RZWP3K", "use_recommendation",
          "admin", "length",
          "area_ha_rtrw", "area_ha_rzwp3k",
          "idx_serasi", "idx_serasi_new",
          "idx_padu_final",
          "idx_padan", "idx_padan_new",
          "idx_padu_ke", "idx_padu_hs", "idx_padu_kl", "idx_padu_kh",
          "idx_padu_rtp", "idx_padu_se", "idx_padu_ki",
          ".row_state")
      }
      final_order <- intersect(desired_order, names(display))
      display <- display[, final_order, drop = FALSE]
      
      num2_fmt <- reactable::colFormat(digits = 2)
      
      col_defs <- list(
        id_rtrw             = reactable::colDef(name = "ID RTRW", minWidth = 85),
        id_rzwp3k           = reactable::colDef(name = "ID RZWP3K", minWidth = 85),
        recommendation      = reactable::colDef(name = "Rekomendasi", minWidth = 130),
        user_decision_rtrw  = reactable::colDef(name = "Keputusan RTRW",
                                                html = TRUE, minWidth = 200),
        user_decision_rzwp3k = reactable::colDef(name = "Keputusan RZWP3K",
                                                 html = TRUE, minWidth = 200),
        user_decision       = reactable::colDef(name = "Keputusan",
                                                html = TRUE, minWidth = 220),
        finalized           = reactable::colDef(
          name = "Finalisasi",
          html = TRUE,
          minWidth = 90,
          header = htmltools::tags$div(
            style = paste("display: flex; align-items: center; gap: 6px;",
                          "justify-content: center; cursor: pointer;"),
            htmltools::tags$input(
              type = "checkbox",
              class = "laspur-header-check",
              title = "Tandai/batalkan finalisasi untuk semua baris di grup ini",
              style = paste("width: 16px; height: 16px; cursor: pointer;",
                            "accent-color: #106665;"),
              onclick = sprintf(
                "event.stopPropagation(); Shiny.setInputValue('%s', {checked: this.checked, nonce: Math.random()}, {priority: 'event'});",
                ns("ws_finalize_all"))
            ),
            "Finalisasi"
          )
        ),
        id_pu               = reactable::colDef(name = "ID PU", minWidth = 70),
        RTRW                = reactable::colDef(name = "RTRW Aktual", minWidth = 180),
        RZWP3K              = reactable::colDef(name = "RZWP3K Aktual", minWidth = 180),
        use_recommendation  = reactable::colDef(name = "Kunci Opsi?", minWidth = 90),
        admin               = reactable::colDef(name = "Wilayah Administratif", minWidth = 140),
        length              = reactable::colDef(name = "Panjang Segmen (m)",
                                                minWidth = 110, format = num2_fmt),
        area_ha_rtrw        = reactable::colDef(name = "Luas Area RTRW (ha)",
                                                minWidth = 110, format = num2_fmt),
        area_ha_rzwp3k      = reactable::colDef(name = "Luas Area RZWP3K (ha)",
                                                minWidth = 120, format = num2_fmt),
        idx_serasi          = reactable::colDef(name = "Indeks SERASI Aktual",
                                                minWidth = 120, format = num2_fmt),
        idx_serasi_new      = reactable::colDef(name = "Indeks SERASI Baru",
                                                minWidth = 120, format = num2_fmt),
        idx_padu_final      = reactable::colDef(name = "Indeks PADU",
                                                minWidth = 100, format = num2_fmt),
        idx_padan           = reactable::colDef(name = "Indeks PADAN Aktual",
                                                minWidth = 120, format = num2_fmt),
        idx_padan_new       = reactable::colDef(name = "Indeks PADAN Baru",
                                                minWidth = 120, format = num2_fmt),
        idx_padu_ke         = reactable::colDef(name = "Indeks PADU-KE",
                                                minWidth = 110, format = num2_fmt),
        idx_padu_hs         = reactable::colDef(name = "Indeks PADU-HS",
                                                minWidth = 110, format = num2_fmt),
        idx_padu_kl         = reactable::colDef(name = "Indeks PADU-KL",
                                                minWidth = 110, format = num2_fmt),
        idx_padu_kh         = reactable::colDef(name = "Indeks PADU-KH",
                                                minWidth = 110, format = num2_fmt),
        idx_padu_rtp        = reactable::colDef(name = "Indeks PADU-RTp",
                                                minWidth = 110, format = num2_fmt),
        idx_padu_se         = reactable::colDef(name = "Indeks PADU-SE",
                                                minWidth = 110, format = num2_fmt),
        idx_padu_ki         = reactable::colDef(name = "Indeks PADU-KI",
                                                minWidth = 110, format = num2_fmt),
        .row_state          = reactable::colDef(show = FALSE)
      )
      
      reactable::reactable(
        display,
        columns       = col_defs,
        pagination    = FALSE,
        outlined      = TRUE,
        bordered      = FALSE,
        compact       = TRUE,
        striped       = FALSE,
        highlight     = FALSE,
        defaultColDef = reactable::colDef(vAlign = "center", headerVAlign = "center"),
        rowClass = reactable::JS(
          "function(rowInfo) {
             var s = rowInfo.row['.row_state'];
             return 'laspur-recon-row laspur-recon-' + (s || 'orange');
           }"),
        onClick = reactable::JS(sprintf(
          "function(rowInfo, colInfo, evt) {
             var t = evt && evt.target;
             if (t && (t.tagName === 'SELECT' || t.tagName === 'INPUT' ||
                       (t.parentNode && (t.parentNode.tagName === 'SELECT' || t.parentNode.tagName === 'INPUT')))) return;
             var pu = rowInfo.row['id_pu'];
             if (pu === null || pu === undefined) return;
             Shiny.setInputValue('%s', {id_pu: pu, nonce: Math.random()}, {priority: 'event'});
           }", ns("ws_row_pick")))
      )
    })
    
    observeEvent(input$ws_cell_change, {
      info <- input$ws_cell_change
      req(info, rv$dec_df)
      pu  <- suppressWarnings(as.integer(info$id_pu))
      col <- as.character(info$col)
      val <- as.character(info$value)
      req(!is.na(pu), nzchar(col))
      idx <- match(pu, as.integer(rv$dec_df$id_pu))
      req(!is.na(idx))
      if (col %in% names(rv$dec_df)) {
        rv$dec_df[[col]][idx] <- val
        rv$data_nonce  <- isolate(rv$data_nonce) + 1L
        bump_table()
        rv$map_nonce   <- isolate(rv$map_nonce) + 1L
      }
    })
    
    observeEvent(input$ws_finalize_change, {
      info <- input$ws_finalize_change
      req(info, rv$dec_df)
      pu  <- suppressWarnings(as.integer(info$id_pu))
      req(!is.na(pu))
      idx <- match(pu, as.integer(rv$dec_df$id_pu))
      req(!is.na(idx))
      rv$dec_df$finalized[idx] <- isTRUE(info$checked)
      rv$data_nonce <- isolate(rv$data_nonce) + 1L
      bump_table()
      rv$map_nonce  <- isolate(rv$map_nonce) + 1L
    })
    
    observeEvent(input$ws_row_pick, {
      pu <- suppressWarnings(as.integer(input$ws_row_pick$id_pu))
      req(!is.na(pu))
      nf <- rv$node_filter
      if (!is.null(nf) && !(pu %in% nf$pu)) {
        rv$node_filter <- NULL
        rv$page        <- 1L
      }
      rv$selected_pu <- pu
      rv$sel_source  <- "table"
    })
    
    observeEvent(input$ws_reset_all, {
      req(rv$dec_committed)
      rv$dec_df <- rv$dec_committed
      rv$page <- 1L
      rv$data_nonce <- isolate(rv$data_nonce) + 1L
      bump_table()
      showNotification("Keputusan dikembalikan ke nilai awal.",
                       type = "message", duration = 3)
    })
    
    do_finalize_all <- function(checked) {
      req(rv$dec_df)
      df  <- rv$dec_df
      cur <- rv$group_selected
      if (!is.na(cur) && "id_group" %in% names(df)) {
        tgt <- which(as.integer(df$id_group) == cur)
      } else {
        tgt <- seq_len(nrow(df))
      }
      if (length(tgt) == 0) {
        showNotification("Tidak ada baris yang dapat difinalisasi di grup ini.",
                         type = "message", duration = 2)
        return(invisible())
      }
      df$finalized[tgt] <- isTRUE(checked)
      rv$dec_df <- df
      rv$data_nonce <- isolate(rv$data_nonce) + 1L
      bump_table()
      showNotification(
        sprintf("%s finalisasi untuk %d baris.",
                if (isTRUE(checked)) "Menandai" else "Membatalkan",
                length(tgt)),
        type = "message", duration = 2)
    }
    
    observeEvent(input$ws_finalize_all_btn, { do_finalize_all(TRUE) })
    
    observeEvent(input$ws_finalize_all, {
      info <- input$ws_finalize_all
      req(info)
      do_finalize_all(isTRUE(info$checked))
    })
    
    output$ws_run_reconcile_ui <- renderUI({
      rv$data_nonce; rv$table_nonce
      states <- row_states()
      if (is.null(states) || length(states) == 0) return(NULL)
      if (!all(states == "green", na.rm = TRUE)) return(NULL)
      actionButton(
        ns("ws_run_reconcile"),
        tagList(tags$i(class = "bi bi-check-all me-1"),
                "Semua Selesai \u2014 Lakukan Rekonsiliasi"),
        class = "btn-success btn-sm")
    })
    
    observeEvent(input$ws_run_reconcile, {
      req(rv$dec_df)
      rv$dec_committed <- rv$dec_df
      rv$dec_saved     <- rv$dec_df
      
      rv$workspace_open <- FALSE
      shinyjs::delay(300, {
        shinyjs::click(ns("btn_run_reconcile"))
      })
    })
    
    observeEvent(input$ws_save, {
      req(rv$dec_df)
      states <- row_states()
      n_or <- sum(states == "orange", na.rm = TRUE)
      n_rd <- sum(states == "red",    na.rm = TRUE)
      if (n_or > 0 || n_rd > 0) {
        showModal(modalDialog(
          title = "Simpan Keputusan?",
          tagList(
            tags$p("Masih terdapat keputusan yang belum selesai:"),
            tags$ul(
              tags$li(sprintf("%d baris belum terekonsiliasi (oranye)", n_or)),
              tags$li(sprintf("%d baris memiliki konflik keputusan (merah)", n_rd))
            ),
            tags$p(style = "font-size: 0.9rem; color:#6c757d;",
                   "Anda dapat memakai Keputusan Mayoritas untuk menyelesaikan ",
                   "konflik otomatis (nilai terbanyak per fitur bersama akan ",
                   "diterapkan ke semua baris fitur tersebut), atau Simpan Saja ",
                   "untuk mempertahankan nilai apa adanya.")
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Batal"),
            actionButton(ns("btn_ws_save_majority"),
                         tagList(tags$i(class = "bi bi-people-fill me-1"),
                                 "Gunakan Keputusan Mayoritas & Simpan"),
                         class = "btn-warning"),
            actionButton(ns("btn_ws_save_as_is"),
                         tagList(tags$i(class = "bi bi-save me-1"), "Simpan Saja"),
                         class = "btn-primary")
          )
        ))
      } else {
        commit_decisions()
      }
    })
    
    apply_majority_vote <- function(df, step) {
      if (identical(step, 2L)) {
        df <- df %>%
          dplyr::group_by(id_rtrw) %>%
          dplyr::mutate(user_decision_rtrw = {
            vals <- user_decision_rtrw[!is.na(user_decision_rtrw)]
            if (length(vals) == 0) user_decision_rtrw
            else {
              tb <- sort(table(vals), decreasing = TRUE)
              rep(names(tb)[1], dplyr::n())
            }
          }) %>%
          dplyr::ungroup()
        df <- df %>%
          dplyr::group_by(id_rzwp3k) %>%
          dplyr::mutate(user_decision_rzwp3k = {
            vals <- user_decision_rzwp3k[!is.na(user_decision_rzwp3k)]
            if (length(vals) == 0) user_decision_rzwp3k
            else {
              tb <- sort(table(vals), decreasing = TRUE)
              rep(names(tb)[1], dplyr::n())
            }
          }) %>%
          dplyr::ungroup()
      }
      as.data.frame(df)
    }
    
    commit_decisions <- function() {
      req(rv$dec_df)
      rv$dec_committed <- rv$dec_df
      rv$dec_saved     <- rv$dec_df
      bump_table()
      showNotification(
        sprintf("Keputusan tersimpan: %d baris.", nrow(rv$dec_df)),
        type = "message", duration = 4)
      TRUE
    }
    
    observeEvent(input$btn_ws_save_majority, {
      removeModal()
      req(rv$dec_df)
      step <- rv$detected_step
      if (identical(step, 2L)) {
        rv$dec_df <- apply_majority_vote(rv$dec_df, step)
      }
      rv$data_nonce <- isolate(rv$data_nonce) + 1L
      commit_decisions()
      showNotification("Konflik diselesaikan dengan keputusan mayoritas.",
                       type = "message", duration = 4)
    })
    
    observeEvent(input$btn_ws_save_as_is, {
      removeModal()
      commit_decisions()
    })
    
    output$ws_dl_draft <- downloadHandler(
      filename = function() {
        ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
        sprintf("Draf Rekonsiliasi %s.xlsx", ts)
      },
      content = function(file) {
        df <- rv$dec_df; req(df)
        step <- rv$detected_step
        if (identical(step, 2L)) {
          keep <- intersect(
            c("id_pu", "id_group", "id_rtrw", "id_rzwp3k",
              "RTRW", "RZWP3K", "recommendation", "use_recommendation",
              "user_decision_rtrw", "user_decision_rzwp3k", "finalized"),
            names(df))
          openxlsx::write.xlsx(as.data.frame(df)[, keep, drop = FALSE], file)
        } else {
          keep <- intersect(
            c("id_pu", "id_rtrw", "id_rzwp3k", "RTRW", "RZWP3K",
              "recommendation", "use_recommendation",
              "user_decision", "finalized"),
            names(df))
          openxlsx::write.xlsx(as.data.frame(df)[, keep, drop = FALSE], file)
        }
      }
    )
    
    recon_map_4326 <- reactive({
      m <- rv$recon_map
      req(inherits(m, "sf"))
      if (is.na(sf::st_crs(m))) sf::st_crs(m) <- 4326
      else m <- sf::st_transform(m, 4326)
      m
    })
    
    output$ws_map <- leaflet::renderLeaflet({
      req(rv$map_latch)
      leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
        leaflet::addProviderTiles("Esri.WorldGrayCanvas", group = "Peta Dasar") %>%
        leaflet::addProviderTiles("Esri.WorldImagery",    group = "Citra Satelit") %>%
        leaflet::addLayersControl(baseGroups = c("Peta Dasar", "Citra Satelit"),
                                  options = leaflet::layersControlOptions(collapsed = TRUE)) %>%
        leaflet::addLegend(
          position = "bottomright", opacity = 0.9,
          title = "Tipe Zona",
          colors = c("#1565C0", "#2E7D32"),
          labels = c("RTRW", "RZWP3K")) %>%
        leaflet::addLegend(
          position = "bottomleft", opacity = 0.9,
          title = "Status Rekonsiliasi",
          colors = c("#28A745", "#FFC107", "#DC3545"),
          labels = c("Terekonsiliasi", "Belum Terekonsiliasi", "Konflik")) %>%
        leaflet::setView(lng = 118, lat = -2, zoom = 5)
    })
    
    observeEvent(input$ws_map_bounds, {
      if (!isTRUE(rv$map_ready)) rv$map_ready <- TRUE
    })
    
    fit_map_to <- function(x) {
      if (is.null(x) || nrow(x) == 0) return(invisible(NULL))
      bb <- sf::st_bbox(x)
      if (anyNA(bb)) return(invisible(NULL))
      leaflet::leafletProxy("ws_map", session = session) %>%
        leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]],
                           bb[["xmax"]], bb[["ymax"]],
                           options = list(maxZoom = 16))
      invisible(NULL)
    }
    
    observe({
      map_trigger()
      rv$map_ready
      
      cur <- isolate(rv$group_selected)
      sel <- isolate(rv$selected_pu)
      df  <- isolate(rv$dec_df)
      
      if (!isTRUE(isolate(rv$workspace_open))) return()
      if (!isTRUE(rv$map_ready)) return()
      if (is.null(df) || nrow(df) == 0) return()
      
      states <- isolate(row_states())
      
      proxy <- leaflet::leafletProxy("ws_map", session = session) %>%
        leaflet::clearGroup("Konteks grup") %>%
        leaflet::clearGroup("Terpilih")
      
      tryCatch(
        proxy <- proxy %>% leaflet.extras::removeSearchFeatures(),
        error = function(e) NULL)
      
      tryCatch({
        m <- recon_map_4326()
        
        idx <- match(as.integer(m$id_pu), as.integer(df$id_pu))
        m$row_state <- states[idx]
        m$row_state[is.na(m$row_state)] <- "orange"
        
        state_color <- c(green = "#28A745", orange = "#FFC107", red = "#DC3545")
        status_label <- c(
          green  = "Terekonsiliasi",
          orange = "Belum Terekonsiliasi",
          red    = "Konflik"
        )
        
        m$status_text <- unname(status_label[m$row_state])
        m$status_text[is.na(m$status_text)] <- "Belum Terekonsiliasi"
        
        m$state_col <- unname(state_color[m$row_state])
        m$state_col[is.na(m$state_col)] <- "#FFC107"
        
        is_rtrw <- !is.na(m$RTRW)
        m$side_col <- ifelse(is_rtrw, "#1565C0", "#2E7D32")
        
        zone_type <- ifelse(is_rtrw, "RTRW", "RZWP3K")
        zone_name <- ifelse(is_rtrw,
                            as.character(m$RTRW),
                            as.character(m$RZWP3K))
        
        zone_id_col <- if ("id_rtrw" %in% names(m) && "id_rzwp3k" %in% names(m)) {
          ifelse(is_rtrw,
                 as.character(m$id_rtrw),
                 as.character(m$id_rzwp3k))
        } else if ("id" %in% names(m)) {
          as.character(m$id)
        } else {
          rep(NA_character_, nrow(m))
        }
        
        area_val <- if ("area_ha_rtrw" %in% names(m) &&
                        "area_ha_rzwp3k" %in% names(m)) {
          ifelse(is_rtrw,
                 suppressWarnings(as.numeric(m$area_ha_rtrw)),
                 suppressWarnings(as.numeric(m$area_ha_rzwp3k)))
        } else if ("area_ha" %in% names(m)) {
          suppressWarnings(as.numeric(m$area_ha))
        } else {
          rep(NA_real_, nrow(m))
        }
        len_val <- if ("length" %in% names(m))
          suppressWarnings(as.numeric(m$length)) else rep(NA_real_, nrow(m))
        
        fmt_num <- function(x) ifelse(is.na(x), "-",
                                      formatC(x, format = "f", digits = 2))
        .dash <- function(x) ifelse(is.na(x) | !nzchar(as.character(x)),
                                    "-", as.character(x))
        
        m$label <- sprintf(
          "%s | %s | %s | %s | id_pu %s",
          .dash(zone_id_col),
          .dash(zone_name),
          m$status_text,
          zone_type,
          m$id_pu
        )
        
        popup_txt <- sprintf(
          "<b>%s</b><br/>ID: %s<br/>Zona: %s<br/>Status: %s<br/>id_pu: %s<br/>Luas: %s ha<br/>Panjang Segmen: %s m",
          zone_type,
          .dash(zone_id_col),
          .dash(zone_name),
          m$status_text,
          m$id_pu,
          fmt_num(area_val),
          fmt_num(len_val)
        )
        m$popup <- lapply(popup_txt, htmltools::HTML)
        
        if (!is.na(cur) && "id_group" %in% names(m)) {
          m <- m[as.integer(m$id_group) == cur, ]
        }
        if (nrow(m) == 0) return(invisible())
        
        m_sel_flag <- as.integer(m$id_pu) %in% sel
        ctx <- m[!m_sel_flag, , drop = FALSE]
        slc <- m[m_sel_flag,  , drop = FALSE]
        
        build_polys <- function(x, id_prefix, weight, opacity, fill_op) {
          if (is.null(x) || nrow(x) == 0) return(NULL)
          is_r <- !is.na(x$RTRW)
          list(
            data    = x,
            layerId = paste0(id_prefix, "_", x$id_pu, "_",
                             ifelse(is_r, "R", "Z")),
            fillColor   = x$state_col,
            color       = x$side_col,
            weight      = weight,
            opacity     = opacity,
            fillOpacity = fill_op,
            label       = x$label,
            popup       = x$popup
          )
        }
        
        ctx_p <- build_polys(ctx, "ctx", 1.5, 0.85, 0.20)
        slc_p <- build_polys(slc, "sel", 3.5, 1.00, 0.55)
        
        if (!is.null(ctx_p)) {
          proxy <- proxy %>% leaflet::addPolygons(
            data = ctx_p$data, layerId = ctx_p$layerId,
            color = ctx_p$color, weight = ctx_p$weight,
            opacity = ctx_p$opacity, fillColor = ctx_p$fillColor,
            fillOpacity = ctx_p$fillOpacity,
            label = ctx_p$label, popup = ctx_p$popup,
            group = "Konteks grup",
            highlightOptions = leaflet::highlightOptions(
              weight = 3, bringToFront = TRUE))
        }
        if (!is.null(slc_p)) {
          proxy <- proxy %>% leaflet::addPolygons(
            data = slc_p$data, layerId = slc_p$layerId,
            color = slc_p$color, weight = slc_p$weight,
            opacity = slc_p$opacity, fillColor = slc_p$fillColor,
            fillOpacity = slc_p$fillOpacity,
            label = slc_p$label, popup = slc_p$popup,
            group = "Terpilih")
        }
        
        if (!is.null(ctx_p) || !is.null(slc_p)) {
          proxy %>% leaflet.extras::addSearchFeatures(
            targetGroups = c("Konteks grup", "Terpilih"),
            options = leaflet.extras::searchFeaturesOptions(
              propertyName         = "label",
              zoom                 = 15,
              openPopup            = TRUE,
              firstTipSubmit       = TRUE,
              autoCollapse         = FALSE,
              hideMarkerOnCollapse = TRUE
            )
          )
        }
      }, error = function(e) {
        message("[reconcile] map proxy error: ", conditionMessage(e))
      })
    })
    
    observeEvent(input$ws_map_shape_click, {
      id <- input$ws_map_shape_click$id
      req(id, grepl("^(ctx|sel)_[0-9]+_[RZ]$", id))
      
      pu   <- as.integer(sub("^(ctx|sel)_([0-9]+)_([RZ])$", "\\2", id))
      side <-                sub("^(ctx|sel)_([0-9]+)_([RZ])$", "\\3", id)
      
      m <- rv$recon_map
      req(inherits(m, "sf"), "id_pu" %in% names(m))
      
      feature_col <- if (identical(side, "R")) "id_rtrw" else "id_rzwp3k"
      if (!feature_col %in% names(m)) {
        if ("id" %in% names(m)) feature_col <- "id" else return()
      }
      
      row_match <- as.integer(m$id_pu) == pu
      if (identical(side, "R")) {
        row_match <- row_match & !is.na(m$RTRW)
      } else {
        row_match <- row_match & !is.na(m$RZWP3K)
      }
      
      candidate_vals <- as.character(m[[feature_col]][row_match])
      candidate_vals <- candidate_vals[!is.na(candidate_vals) &
                                         nzchar(candidate_vals)]
      if (length(candidate_vals) == 0) return()
      feature_id <- candidate_vals[1]
      
      all_pairs <- unique(as.integer(
        m$id_pu[!is.na(m[[feature_col]]) &
                  as.character(m[[feature_col]]) == feature_id]
      ))
      if (length(all_pairs) == 0) return()
      
      nf <- rv$node_filter
      if (!is.null(nf) &&
          identical(as.character(nf$node), feature_id) &&
          identical(as.character(nf$side), side)) return()
      
      rv$node_filter <- list(node  = feature_id,
                             side  = side,
                             label = feature_id,
                             pu    = all_pairs)
      rv$page        <- 1L
      rv$selected_pu <- integer(0)
      rv$sel_source  <- "map"
    })
    
    observeEvent(
      list(rv$group_selected, rv$fit_nonce, rv$map_ready),
      {
        req(isTRUE(rv$workspace_open), isTRUE(rv$map_ready))
        tryCatch({
          m <- recon_map_4326()
          cur <- rv$group_selected
          if (!is.na(cur) && "id_group" %in% names(m)) {
            m <- m[as.integer(m$id_group) == cur, ]
          }
          fit_map_to(m)
        }, error = function(e) message("[reconcile] fit error: ", conditionMessage(e)))
      }, ignoreInit = TRUE)
    
    observeEvent(rv$selected_pu, {
      sel <- rv$selected_pu
      req(length(sel) > 0,
          identical(rv$sel_source, "table"),
          isTRUE(rv$workspace_open), isTRUE(rv$map_ready))
      tryCatch({
        m <- recon_map_4326()
        fit_map_to(m[m$id_pu %in% sel, ])
      }, error = function(e) message("[reconcile] fit selection error: ", conditionMessage(e)))
    }, ignoreInit = TRUE)
    
    run_reconciliation <- function() {
      rv$resolved_rtrw <- NULL; rv$resolved_rzwp3k <- NULL
      rv$resolved_integrated <- NULL
      rv$analysis_result <- NULL; rv$gpkg_path <- NULL; rv$xlsx_path <- NULL
      rv$log_messages <- ""
      log_lines <- character(0)
      
      showNotification("Menjalankan proses rekonsiliasi...",
                       type = "message", id = "recon_progress", duration = 10)
      
      withProgress(message = "Menjalankan Rekonsiliasi Spasial", value = 0, {
        tryCatch({
          module_dir <- file.path(output_dir(), .RECON_MODULE_FOLDER)
          log_dir    <- file.path(module_dir, .RECON_PNG_DIR)
          dir.create(module_dir, recursive = TRUE, showWarnings = FALSE)
          dir.create(log_dir,    recursive = TRUE, showWarnings = FALSE)
          
          alpha_info <- .resolve_alpha_from_recommendation(
            step = rv$detected_step, output_dir = output_dir(), session = session)
          alpha_val <- alpha_info$alpha
          log_lines <- c(log_lines,
                         sprintf("Alpha (\u03B1): %.2f  [%s]",
                                 alpha_val, alpha_info$source))
          
          if (rv$detected_step == 1) {
            incProgress(0.1, detail = "Mempersiapkan tabel keputusan...")
            recon_table <- rv$dec_committed[, c("id_pu", "user_decision"), drop = FALSE]
            
            incProgress(0.3, detail = "Menggabungkan dengan peta rekomendasi...")
            overlaps_map <- rv$recon_map %>%
              dplyr::left_join(recon_table, by = "id_pu") %>%
              dplyr::mutate(user_decision = user_decision)
            incProgress(0.5, detail = "Menjalankan rekonsiliasi Overlaps...")
            result <- reconciliation_step1(
              rtrw_base = rv$rtrw_vect, rzwp3k_base = rv$rzwp3k_vect,
              overlaps_map = overlaps_map,
              rtrw_priority = rv$rtrw_prioritas,
              rzwp3k_priority = rv$rzwp3k_prioritas,
              matriks_serasi = rv$serasi_matrix,
              alpha = alpha_val)
            rv$resolved_rtrw <- result$rtrw
            rv$resolved_rzwp3k <- result$rzwp3k
            log_lines <- c(log_lines,
                           "--- LOG REKONSILIASI STEP 1 (OVERLAPS) ---",
                           sprintf("RTRW Features: %d", nrow(result$rtrw)),
                           sprintf("RZWP3K Features: %d", nrow(result$rzwp3k)))
            rtrw_combined   <- result$rtrw   %>% dplyr::mutate(Source = "RTRW")
            rzwp3k_combined <- result$rzwp3k %>% dplyr::mutate(Source = "RZWP3K")
            combined <- dplyr::bind_rows(rtrw_combined, rzwp3k_combined)
          } else {
            incProgress(0.1, detail = "Mempersiapkan tabel keputusan...")
            recon_table_filled <- undissolve_adjacent_pairs(
              rv$dec_committed,
              decisions_rtrw_col   = "user_decision_rtrw",
              decisions_rzwp3k_col = "user_decision_rzwp3k")
            incProgress(0.3, detail = "Menjalankan rekonsiliasi Bertetangga...")
            result <- reconcilliation_step2(
              recon_table_path   = recon_table_filled,
              adjacent_recom_map = rv$recon_map,
              rtrw_vect          = rv$rtrw_vect,
              rzwp3k_vect        = rv$rzwp3k_vect,
              matriks_serasi     = rv$serasi_matrix,
              alpha              = alpha_val)
            rv$resolved_integrated <- result
            log_lines <- c(log_lines,
                           "--- LOG REKONSILIASI STEP 2 (ADJACENT) ---",
                           sprintf("Feature Hasil Integrasi: %d", nrow(result)))
            combined <- result
          }
          
          incProgress(0.6, detail = "Menyiapkan hasil...")
          if (!"Source" %in% names(combined)) combined$Source <- "Integrated"
          combined <- combined %>%
            dplyr::mutate(
              original_id_pu = as.character(id_pu),
              id_pu          = paste0(Source, "_", dplyr::row_number()))
          combined <- tryCatch(sf::st_make_valid(combined), error = function(e) combined)
          
          incProgress(0.75, detail = "Menyimpan hasil...")
          case_suffix <- if (rv$detected_step == 1) "overlaps" else "adjacent"
          base_name   <- sprintf("rtrwp_terintegrasi_%s", case_suffix)
          
          stale_case <- if (rv$detected_step == 1) "adjacent" else "overlaps"
          stale_files <- file.path(module_dir,
                                   sprintf("rtrwp_terintegrasi_%s.%s",
                                           stale_case, c("gpkg", "xlsx")))
          stale_files <- stale_files[file.exists(stale_files)]
          if (length(stale_files) > 0) file.remove(stale_files)
          
          out_gpkg <- file.path(module_dir, paste0(base_name, ".gpkg"))
          sf::st_write(combined, out_gpkg, delete_dsn = TRUE, quiet = TRUE)
          out_xlsx <- file.path(module_dir, paste0(base_name, ".xlsx"))
          openxlsx::write.xlsx(sf::st_drop_geometry(combined), out_xlsx)
          
          tryCatch({
            plot_categorical_map(
              map = combined, title = "Peta Status Rekonsiliasi",
              column = "Reconcile", legend = "Status Rekonsiliasi",
              filepath = file.path(log_dir, sprintf("reconcile_map_%s.png", case_suffix)))
          }, error = function(e) warning("Gagal membuat PNG: ", e$message))
          
          incProgress(0.9, detail = "Menyimpan log...")
          rv$final_log <- paste(log_lines, collapse = "\n")
          rv$log_messages <- rv$final_log
          
          out <- list(
            inputs = list(
              start_time         = Sys.time(),
              recon_step         = rv$detected_step,
              recon_map_source   = rv$recon_map_source,
              decision_mode      = input$decision_mode %||% "inapp",
              recon_table_filled = if (!is.null(input$recon_table_filled_file))
                input$recon_table_filled_file$name else NULL,
              recon_map_file     = if (!is.null(input$recon_map_file))
                input$recon_map_file$name else NULL,
              alpha              = alpha_val,
              alpha_source       = alpha_info$source,
              output_dir         = output_dir()),
            result = list(
              idx_reconcile_map   = combined,
              idx_reconcile_table = sf::st_drop_geometry(combined),
              resolved_rtrw       = rv$resolved_rtrw,
              resolved_rzwp3k     = rv$resolved_rzwp3k,
              resolved_integrated = rv$resolved_integrated))
          
          log_path <- file.path(module_dir, .RECON_RDA)
          tryCatch({
            inputs <- out$inputs
            save(inputs, file = log_path)
          }, error = function(e) warning("Gagal menulis log: ", e$message))
          
          session$userData$module_results$reconcile <- out
          rv$analysis_result <- list(map = combined,
                                     table = sf::st_drop_geometry(combined))
          rv$gpkg_path <- out_gpkg
          rv$xlsx_path <- out_xlsx
          
          showNotification("Proses Rekonsiliasi Selesai.", type = "message")
          incProgress(1.0, detail = "Selesai!")
        }, error = function(e) {
          rv$final_log    <- paste0("Error Runtime Execution:\n", e$message)
          rv$log_messages <- rv$final_log
          showNotification(paste("Gagal melakukan rekonsiliasi:", e$message),
                           type = "error", duration = NULL)
        })
      })
    }
    
    observeEvent(input$btn_run_reconcile, {
      if (is.null(output_dir()) || !nzchar(output_dir()) ||
          !validate_output_dir(output_dir())) {
        showNotification("Direktori output belum diatur.",
                         type = "error", duration = 5)
        return()
      }
      req(rv$recon_map, rv$rtrw_vect, rv$rzwp3k_vect)
      req(rv$rtrw_prioritas, rv$rzwp3k_prioritas, rv$serasi_matrix)
      
      if (is.null(rv$dec_committed)) {
        showNotification(
          "Keputusan belum tersedia. Siapkan keputusan (in-app atau unggah templat) terlebih dahulu.",
          type = "warning", duration = 8)
        return()
      }
      
      if (isTRUE(has_unsaved())) {
        showModal(modalDialog(
          title = "Ada Perubahan yang Belum Disimpan",
          tagList(
            tags$p("Keputusan di Halaman Kerja telah berubah sejak penyimpanan terakhir."),
            tags$p(style = "font-size: 0.9rem; color: #6c757d;",
                   "Simpan terlebih dahulu agar perubahan tersebut dipakai pada ",
                   "proses rekonsiliasi, atau batalkan untuk kembali ke Halaman Kerja ",
                   "dan meninjau kembali keputusan Anda.")
          ),
          easyClose = FALSE,
          footer = tagList(
            actionButton(ns("btn_recon_cancel"), "Batal",
                         class = "btn-outline-secondary"),
            actionButton(ns("btn_recon_save_run"),
                         tagList(tags$i(class = "bi bi-save me-1"),
                                 "Simpan & Lanjutkan"),
                         class = "btn-primary")
          )
        ))
        return()
      }
      
      run_reconciliation()
    })
    
    observeEvent(input$btn_recon_cancel, {
      removeModal()
      rv$workspace_open <- TRUE
    })
    
    observeEvent(input$btn_recon_save_run, {
      removeModal()
      req(rv$dec_df)
      rv$dec_committed <- rv$dec_df
      rv$dec_saved     <- rv$dec_df
      bump_table()
      showNotification(
        sprintf("Keputusan tersimpan: %d baris.", nrow(rv$dec_df)),
        type = "message", duration = 3)
      run_reconciliation()
    })
    
    output$status_box <- renderUI({
      if (!is.null(rv$analysis_result)) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Peta berhasil direkonsiliasi! Silakan periksa peta hasil dan unduh gpkg.")
      } else if (!is.null(rv$final_log) && grepl("^Error", rv$final_log)) {
        div(class = "alert alert-danger mb-0",
            tags$i(class = "bi bi-exclamation-triangle-fill me-2"),
            "Gagal mengeksekusi rekonsiliasi. Lihat log kesalahan.")
      } else {
        div(class = "alert alert-secondary mb-0", "Menunggu hasil rekonsiliasi...")
      }
    })
    
    reconcile_config <- list(
      map_color_col  = "Reconcile",
      map_title      = "Status Rekonsiliasi",
      map_palette    = c("#BDBDBD", "#2B8CBE"),
      map_label_cols = list(
        "ID PU" = "original_id_pu", "Sumber" = "Source",
        "Kelas Lama" = "Zoning_Old", "Kelas Baru" = "Zoning_New",
        "Rekonsiliasi" = "Reconcile", "Indeks SERASI" = "idx_serasi",
        "Indeks PADAN" = "idx_padan", "Selisih PADAN" = "delta_idx_padan"),
      table_cols = c(
        "original_id_pu"  = "ID PU",
        "Source"          = "Sumber",
        "Zoning_Old"      = "Kelas Lama",
        "Zoning_New"      = "Kelas Baru",
        "Reconcile"       = "Rekonsiliasi?",
        "Overlap"         = "Tumpang Tindih?",
        "Overlap_Pair"    = "Pasangan Tumpang Tindih",
        "Adjacent"        = "Bertetangga?",
        "idx_serasi"      = "Indeks SERASI",
        "idx_padu_final"  = "Indeks PADU",
        "idx_padan"       = "Indeks PADAN",
        "idx_serasi_new"  = "Indeks SERASI Baru",
        "idx_padan_new"   = "Indeks PADAN Baru",
        "delta_idx_padan" = "Selisih Indeks PADAN"),
      table_round_cols = c(
        "Indeks SERASI", "Indeks PADU", "Indeks PADAN",
        "Indeks SERASI Baru", "Indeks PADAN Baru", "Selisih Indeks PADAN")
    )
    
    render_result_server(input, output, session, rv, reconcile_config)
  })
}