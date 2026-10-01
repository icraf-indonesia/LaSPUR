# ui/modules/mod_recommendation_overlaps.R
# ============================================================

source("R/functions.R")
source("R/helpers.R")

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

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

.validate_alt_table_overlaps <- function(alt_table, matriks_serasi) {
  required_cols <- c("id_pu", "id_rtrw", "id_rzwp3k", "alt_RTRW", "alt_RZWP3K")
  missing_cols <- setdiff(required_cols, names(alt_table))
  if (length(missing_cols) > 0) {
    return(list(ok = FALSE, msg = sprintf(
      "Kolom wajib tidak ditemukan pada file yang diunggah: %s",
      paste(missing_cols, collapse = ", ")
    )))
  }
  valid_rtrw <- unique(as.character(matriks_serasi$class1))
  valid_rz   <- unique(as.character(matriks_serasi$class2))
  bad_rtrw <- unique(as.character(alt_table$alt_RTRW[
    !is.na(alt_table$alt_RTRW) & !as.character(alt_table$alt_RTRW) %in% valid_rtrw
  ]))
  bad_rz <- unique(as.character(alt_table$alt_RZWP3K[
    !is.na(alt_table$alt_RZWP3K) & !as.character(alt_table$alt_RZWP3K) %in% valid_rz
  ]))
  if (length(bad_rtrw) > 0 || length(bad_rz) > 0) {
    msg <- "Ditemukan nilai zona alternatif yang tidak dikenali pada Matriks SERASI."
    if (length(bad_rtrw) > 0) msg <- paste0(msg, sprintf("\n- alt_RTRW tidak valid: %s", paste(bad_rtrw, collapse = ", ")))
    if (length(bad_rz)   > 0) msg <- paste0(msg, sprintf("\n- alt_RZWP3K tidak valid: %s", paste(bad_rz, collapse = ", ")))
    return(list(ok = FALSE, msg = msg))
  }
  list(ok = TRUE, msg = "Validasi berhasil.")
}

.validate_priority_table <- function(tbl, zone_col) {
  required_cols <- c(zone_col, "Prioritas")
  missing_cols <- setdiff(required_cols, names(tbl))
  if (length(missing_cols) > 0) {
    return(list(ok = FALSE, msg = sprintf(
      "Kolom wajib tidak ditemukan pada tabel acuan pola %s: %s",
      zone_col, paste(missing_cols, collapse = ", ")
    )))
  }
  list(ok = TRUE, msg = "Validasi berhasil.")
}

.get_serasi <- function(class1, class2, matriks_serasi) {
  if (is.na(class1) || is.na(class2)) return(NA_real_)
  class1 <- trimws(as.character(class1))
  class2 <- trimws(as.character(class2))
  mat <- matriks_serasi
  mat$class1 <- trimws(as.character(mat$class1))
  mat$class2 <- trimws(as.character(mat$class2))
  idx <- which(mat$class1 == class1 & mat$class2 == class2)
  if (length(idx) == 0) return(NA_real_)
  mat$idx_serasi[idx[1]]
}

recommendation_overlaps_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("4. Analisis Penyusunan Alternatif Area Tumpang Tindih",
         style = "margin: 0; font-weight: 700;"),
      tags$p("Menentukan opsi penyelesaian konflik tumpang tindih dalam integrasi tata ruang darat-laut.",
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
                "Langkah 1 — Menyaring Kasus",
                value = "step1",
                icon = tags$i(class = "bi bi-funnel-fill"),
                div(
                  class = "laspur-fileinput-with-bar",
                  fileInput(ns("idx_padan_file"),
                            label = "Peta PADAN (.gpkg)",
                            accept = ".gpkg"),
                  uiOutput(ns("loaded_file_bar"))
                ),
                uiOutput(ns("step1_body_ui"))
              ),
              accordion_panel("Langkah 2 — Menentukan Opsi Alternatif", value = "step2",
                              icon = tags$i(class = "bi bi-signpost-split-fill"),
                              uiOutput(ns("step2_ui"))),
              accordion_panel("Langkah 3 — Menentukan Alternatif", value = "step3",
                              icon = tags$i(class = "bi bi-check2-circle"),
                              uiOutput(ns("step3_ui")))
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
        .inapp-table-host { --inapp-table-max: 620px; }
        .inapp-table-host .reactable { height: auto !important; }
        .inapp-table-host .rt-table { max-height: var(--inapp-table-max); overflow: auto; }

        .inapp-table-host .rt-table { font-size: 0.72rem; }
        .inapp-table-host .rt-th,
        .inapp-table-host .rt-td {
          font-size: 0.72rem !important;
          padding: 3px 5px !important;
          line-height: 1.2 !important;
          vertical-align: middle;
        }
        .inapp-table-host .rt-th { font-weight: 600; }
        .inapp-table-host .rt-thead { position: sticky; top: 0; z-index: 5; }
        .inapp-table-host .rt-thead .rt-th {
          background: #F8FAFC !important;
          border-bottom: 2px solid #E2E8F0;
        }

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

        .laspur-summary-row {
          display: flex;
          gap: 8px;
          margin-bottom: 8px;
          flex-wrap: wrap;
        }
        .laspur-summary-row > .laspur-summary-box {
          flex: 1 1 0;
          min-width: 180px;
          border-radius: 8px;
          padding: 8px 12px;
          border: 1px solid;
          box-sizing: border-box;
        }
        .laspur-summary-box .laspur-summary-title {
          font-size: 0.75rem;
          font-weight: 700;
          margin-bottom: 2px;
          opacity: 0.75;
        }
        .laspur-summary-box.box-outcome {
          flex: 2 1 0;
        }
        .laspur-summary-box .laspur-summary-value {
          font-size: 0.85rem;
          font-weight: 600;
          line-height: 1.35;
        }
        .laspur-summary-box.box-total {
          background-color: #eef6fc;
          border-color: #cfe3f5;
          color: #1b75ba;
        }
        .laspur-summary-box.box-usage {
          background-color: #F8FAFC;
          border-color: #E2E8F0;
          color: #334155;
        }
        .laspur-summary-box.box-outcome {
          background-color: #e6f2f2;
          border-color: #c9e4e4;
          color: #106665;
        }
      ")),
      fluidRow(
        class = "g-3",
        style = "align-items: flex-start;",
        column(
          width = 8,
          card(
            card_header(
              tags$div(
                class = "d-flex align-items-center",
                style = "gap: 6px;",
                tags$i(class = "bi bi-table"),
                "Keputusan Alternatif"
              )
            ),
            div(
              class = "inapp-toolbar-row",
              div(class = "inapp-dd",
                  tags$small(style = "color:#6c757d; font-size:0.78rem;",
                             "Kolom alternatif hanya aktif pada sisi yang direkomendasikan untuk diubah.")),
              div(class = "inapp-btn",
                  actionButton(ns("inapp_reset_all"),
                               tagList(tags$i(class = "bi bi-arrow-counterclockwise me-1"), "Reset Keputusan"),
                               class = "btn-danger btn-sm"))
            ),
            uiOutput(ns("inapp_filter_chip_ui")),
            uiOutput(ns("inapp_summary_ui")),
            div(class = "inapp-table-host",
                reactable::reactableOutput(ns("inapp_table"))),
            div(
              style = paste("display: flex; justify-content: space-between;",
                            "align-items: center; margin-top: 8px; gap: 8px; flex-wrap: wrap;"),
              div(
                style = "display: flex; gap: 8px; flex-wrap: wrap;",
                actionButton(ns("inapp_save"),
                             tagList(tags$i(class = "bi bi-save me-1"), "Simpan Keputusan"),
                             class = "btn-primary btn-sm"),
                downloadButton(ns("inapp_dl_draft"),
                               label = "Simpan sebagai Draf",
                               class = "btn-outline-secondary btn-sm")
              ),
              div(
                style = "display: flex; gap: 8px; flex-wrap: wrap;",
                actionButton(ns("btn_back_to_setup"),
                             tagList(tags$i(class = "bi bi-x-lg me-1"), "Tutup Halaman Kerja"),
                             class = "btn-outline-primary btn-sm"),
                actionButton(ns("btn_workspace_to_step3"),
                             tagList("Lanjut ke Step 3", tags$i(class = "bi bi-arrow-right ms-1")),
                             class = "btn-success btn-sm")
              )
            )
          )
        ),
        column(
          width = 4,
          card(
            card_header(
              tags$div(
                class = "d-flex align-items-center",
                style = "gap: 6px;",
                tags$i(class = "bi bi-map"),
                "Peta"
              )
            ),
            leaflet::leafletOutput(ns("group_map"), height = "600px")
          )
        )
      )
    )
  )
}

recommendation_overlaps_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shinyjs::useShinyjs()
    
    rv <- reactiveValues(
      unlocked = 1L,
      idx_padan_map        = NULL,
      idx_padan_source     = NULL,
      filter_snapshot      = NULL,
      idx_padan_map_filter = NULL,
      count_before         = NULL,
      count_after          = NULL,
      matriks_serasi       = NULL,
      alt_template_path    = NULL,
      alt_table_snapshot   = NULL,
      idx_padan_map_alt    = NULL,
      alt_status           = NULL,
      final_result         = NULL,
      final_log            = NULL,
      analysis_result      = NULL,
      gpkg_path            = NULL,
      xlsx_path            = NULL,
      log_messages         = "",
      
      priority_rtrw      = NULL,
      priority_rzwp3k    = NULL,
      alpha              = 0.5,
      threshold_serasi   = 0.6,
      threshold_padu     = 0.65,
      
      inapp_decisions  = NULL,
      inapp_committed  = NULL,
      inapp_saved      = NULL,
      inapp_options_df = NULL,
      workspace_open   = FALSE,
      current_panel    = "step1",
      upload_loaded    = FALSE,
      selected_pu      = integer(0),
      sel_source       = "none",
      node_filter      = NULL,
      table_nonce      = 0L,
      data_nonce       = 0L,
      fit_nonce        = 0L,
      map_latch        = FALSE,
      map_ready        = FALSE,
      map_nonce        = 0L
    )
    
    go_to_panel <- function(value) {
      rv$workspace_open <- FALSE
      rv$current_panel  <- value
      bslib::accordion_panel_set(id = "wizard", values = value, session = session)
    }
    
    is_workspace_mode <- reactive({
      identical(rv$current_panel, "step2") && isTRUE(rv$workspace_open)
    })
    
    bump_table <- function() rv$table_nonce <- isolate(rv$table_nonce) + 1L
    
    clear_selection <- function() {
      rv$selected_pu <- integer(0)
      rv$sel_source  <- "none"
    }
    
    map_trigger <- shiny::debounce(reactive({
      rv$map_nonce
      rv$selected_pu
    }), millis = 350)
    
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
          ", ns("inapp_table"), ns("group_map")))
        })
      } else {
        shinyjs::runjs(sprintf("$('#%s').show(); $('#%s').hide();",
                               ns("setup_view"), ns("workspace_view")))
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$btn_back_to_setup, { rv$workspace_open <- FALSE })
    
    discovered_padan_key <- reactive({
      padan_res <- tryCatch(session$userData$module_results$padan,
                            error = function(e) NULL)
      if (!is.null(padan_res) &&
          !is.null(padan_res$result$idx_padan_map) &&
          inherits(padan_res$result$idx_padan_map, "sf")) {
        return("session")
      }
      if (!is.null(output_dir()) && nzchar(output_dir())) {
        f <- file.path(output_dir(), "Analisis PADAN", "idx_padan.gpkg")
        if (file.exists(f)) return(paste0("file|", as.numeric(file.mtime(f))))
      }
      "none"
    })
    
    .load_padan_from_discovery <- function() {
      key <- discovered_padan_key()
      if (identical(key, "none")) {
        rv$idx_padan_map    <- NULL
        rv$idx_padan_source <- NULL
        return(invisible(NULL))
      }
      if (identical(key, "session")) {
        rv$idx_padan_map    <- session$userData$module_results$padan$result$idx_padan_map
        rv$idx_padan_source <- "session"
      } else {
        f <- file.path(output_dir(), "Analisis PADAN", "idx_padan.gpkg")
        map <- tryCatch(sf::st_read(f, quiet = TRUE), error = function(e) NULL)
        if (!is.null(map)) {
          rv$idx_padan_map    <- map
          rv$idx_padan_source <- "file"
        } else {
          rv$idx_padan_map    <- NULL
          rv$idx_padan_source <- NULL
        }
      }
      if (!is.null(rv$idx_padan_map)) {
        rv$idx_padan_map_filter <- NULL
        rv$filter_snapshot      <- NULL
        rv$count_before         <- NULL
        rv$count_after          <- NULL
        rv$inapp_decisions      <- NULL
        rv$inapp_committed      <- NULL
        rv$inapp_saved          <- NULL
        rv$inapp_options_df     <- NULL
        rv$idx_padan_map_alt    <- NULL
        rv$alt_template_path    <- NULL
        rv$alt_status           <- NULL
        rv$final_result         <- NULL
        rv$final_log            <- NULL
        rv$workspace_open       <- FALSE
        rv$upload_loaded        <- FALSE
        rv$unlocked             <- 1L
      }
      invisible(NULL)
    }
    
    observeEvent(discovered_padan_key(), {
      if (identical(rv$idx_padan_source, "manual")) return()
      .load_padan_from_discovery()
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    output$loaded_file_bar <- renderUI({
      render_loaded_file_bar(
        state    = rv$idx_padan_source,
        input_id = ns("idx_padan_file"),
        filename = "idx_padan.gpkg"
      )
    })
    
    # ── Step 1: filter ────────────────────────────────────
    output$step1_body_ui <- renderUI({
      if (is.null(rv$idx_padan_map)) {
        return(tagList(
          tags$p(style = "color: #6c757d; margin-top: 8px;",
                 "Unggah peta PADAN untuk mengaktifkan filter."),
          .step_nav(ns, back_id = NULL, next_id = "btn_next_1",
                    next_label = "Lanjut ke Step 2")
        ))
      }
      tagList(
        layout_column_wrap(
          width = 1/2,
          div(
            checkboxInput(ns("apply_area"), "Aktifkan Filter Luas Area", value = TRUE),
            conditionalPanel(
              condition = paste0("input['", ns("apply_area"), "']"),
              numericInput(ns("area_filter"), "Ambang Luas (ha)", value = 10, min = 0, step = 1)
            )
          ),
          div(
            checkboxInput(ns("apply_idx"), "Aktifkan Filter Indeks PADAN", value = TRUE),
            conditionalPanel(
              condition = paste0("input['", ns("apply_idx"), "']"),
              numericInput(ns("idx_filter"), "Ambang Indeks PADAN (<)",
                           value = 0.75, min = 0, max = 1, step = 0.05)
            )
          )
        ),
        uiOutput(ns("filter_count_ui")),
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1",
                  next_label = "Lanjut ke Step 2")
      )
    })
    
    observeEvent(input$idx_padan_file, {
      req(input$idx_padan_file)
      showNotification("Memuat peta PADAN...", type = "message", duration = 2)
      tryCatch({
        rv$idx_padan_map    <- sf::st_read(input$idx_padan_file$datapath, quiet = TRUE)
        rv$idx_padan_source <- "manual"
        rv$idx_padan_map_filter <- NULL
        rv$filter_snapshot <- NULL
        rv$count_before <- NULL
        rv$count_after <- NULL
        
        rv$inapp_decisions   <- NULL
        rv$inapp_committed   <- NULL
        rv$inapp_saved       <- NULL
        rv$inapp_options_df  <- NULL
        rv$idx_padan_map_alt <- NULL
        rv$alt_template_path <- NULL
        rv$alt_status        <- NULL
        rv$final_result      <- NULL
        rv$final_log         <- NULL
        rv$workspace_open    <- FALSE
        rv$upload_loaded     <- FALSE
        rv$unlocked          <- 1L
        
        fname <- input$idx_padan_file$name
        fname <- if (length(fname) > 1) sprintf("%d files", length(fname)) else fname[1]
        session$sendCustomMessage("set_fileinput_text", list(
          input_id = ns("idx_padan_file"),
          filename = fname
        ))
        
        showNotification("Peta PADAN berhasil dimuat.", type = "message", duration = 5)
      }, error = function(e) {
        rv$idx_padan_map    <- NULL
        rv$idx_padan_source <- NULL
        showNotification(paste("Gagal membaca file:", e$message), type = "error", duration = 10)
      })
    })
    
    filtered_data <- reactive({
      req(rv$idx_padan_map)
      map <- rv$idx_padan_map
      keep <- rep(TRUE, nrow(map))
      if (isTRUE(input$apply_area)) keep <- keep & (as.numeric(map$area_ha) > input$area_filter)
      if (isTRUE(input$apply_idx))    keep <- keep & (map$idx_padan < input$idx_filter)
      list(filtered = map[keep, ], before = nrow(map), after = nrow(map[keep, ]))
    })
    
    observeEvent(filtered_data(), {
      res <- filtered_data()
      rv$idx_padan_map_filter <- res$filtered
      rv$count_before <- res$before
      rv$count_after  <- res$after
      if (!is.null(rv$filter_snapshot)) {
        current <- list(
          apply_area = input$apply_area, area_filter = input$area_filter,
          apply_idx = input$apply_idx,   idx_filter = input$idx_filter
        )
        if (!identical(current, rv$filter_snapshot)) {
          rv$inapp_decisions   <- NULL
          rv$inapp_committed   <- NULL
          rv$inapp_saved       <- NULL
          rv$inapp_options_df  <- NULL
          rv$idx_padan_map_alt <- NULL
          rv$alt_template_path <- NULL
          rv$alt_status        <- NULL
          rv$final_result      <- NULL
          rv$final_log         <- NULL
          rv$upload_loaded     <- FALSE
          rv$unlocked          <- min(rv$unlocked, 2L)
        }
      }
    })
    
    output$filter_count_ui <- renderUI({
      req(!is.null(rv$count_before))
      removed <- rv$count_before - rv$count_after
      pct <- if (rv$count_before > 0) (1 - rv$count_after / rv$count_before) * 100 else 0
      div(class = "alert alert-info", style = "margin-top: 12px;",
          tags$div(sprintf("Sebelum filter: %d kasus", rv$count_before)),
          tags$div(sprintf("Setelah filter: %d kasus", rv$count_after)),
          tags$div(sprintf("Terhapus: %d kasus (%.1f%%)", removed, pct)))
    })
    
    observeEvent(input$btn_next_1, {
      rv$filter_snapshot <- list(
        apply_area = input$apply_area, area_filter = input$area_filter,
        apply_idx = input$apply_idx,   idx_filter = input$idx_filter
      )
      rv$unlocked <- max(rv$unlocked, 2L)
      go_to_panel("step2")
    })
    
    # ── Step 2: identify options ──────────────────────────
    output$step2_ui <- renderUI({
      tagList(
        fileInput(ns("matrix_file"), "Pilih Matriks Serasi (.xlsx)", accept = ".xlsx"),
        hr(),
        h6("Parameter Rekomendasi Sistem", style = "font-weight: 700;"),
        tags$p(style = "color:#6c757d; font-size:0.82rem; margin: 0 0 8px 0;",
               "Rekomendasi sisi yang berubah (RTRW / RZWP3K) dihitung dari ",
               "prioritas pola dan ambang SERASI/PADU."),
        bslib::layout_column_wrap(
          width = 1/2,
          fileInput(ns("rtrw_priority_file"),
                    "Tabel Acuan Pola RTRW (.xlsx)", accept = ".xlsx"),
          fileInput(ns("rzwp3k_priority_file"),
                    "Tabel Acuan Pola RZWP3K (.xlsx)", accept = ".xlsx")
        ),
        bslib::layout_column_wrap(
          width = 1/2,
          numericInput(ns("threshold_serasi"), "Ambang SERASI",
                       value = 0.6, min = 0, max = 1, step = 0.05),
          numericInput(ns("threshold_padu"), "Ambang PADU",
                       value = 0.65, min = 0, max = 1, step = 0.05)
        ),
        sliderInput(ns("alpha_val"), "Proporsi Alpha (\u03B1)",
                    min = 0, max = 1, value = 0.5, step = 0.1),
        hr(),
        if (is.null(output_dir()) || !nzchar(output_dir())) {
          div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
              tags$i(class = "bi bi-exclamation-triangle me-1"),
              "Direktori output belum diatur. Atur terlebih dahulu di menu utama.")
        },
        actionButton(
          ns("btn_prepare_options"),
          tagList(tags$i(class = "bi bi-play-fill me-1"), "Identifikasi Alternatif"),
          class = "btn-primary",
          style = "width: 100%; font-weight: 600; padding: 10px 16px; border-radius: 8px;"
        ),
        hr(),
        radioButtons(
          ns("decision_mode"), "Mode Penentuan Alternatif",
          choices = c(
            "Gunakan Halaman Kerja" = "inapp",
            "Gunakan Templat"       = "manual"
          ),
          selected = "inapp", inline = FALSE
        ),
        conditionalPanel(
          condition = paste0("input['", ns("decision_mode"), "'] == 'manual'"),
          uiOutput(ns("template_status_ui")),
          div(style = "margin-bottom: 8px;",
              downloadButton(ns("dl_template"),
                             label = "Unduh Templat",
                             class = "btn-outline-success btn-sm")),
          fileInput(ns("alt_upload"),
                    "Unggah Templat yang Sudah Diisi (.xlsx)", accept = ".xlsx"),
          uiOutput(ns("alt_validation_ui"))
        ),
        uiOutput(ns("inapp_ready_hint_ui")),
        .step_nav(ns, back_id = "btn_back_2", next_id = "btn_next_2",
                  next_label = "Lanjut ke Step 3")
      )
    })
    
    output$inapp_ready_hint_ui <- renderUI({
      mode    <- input$decision_mode %||% "inapp"
      has_dec <- !is.null(rv$inapp_decisions)
      
      show_btn <- FALSE
      if (identical(mode, "inapp") && has_dec) {
        show_btn <- TRUE
      } else if (identical(mode, "manual") && has_dec && isTRUE(rv$upload_loaded)) {
        show_btn <- TRUE
      }
      
      if (show_btn) {
        div(class = "alert alert-info mb-2 mt-2", style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-check-circle me-1"),
            sprintf("Keputusan siap: %d pasang. ", nrow(rv$inapp_decisions)),
            actionButton(ns("btn_reopen_workspace"), "Buka Halaman Kerja",
                         class = "btn-outline-primary btn-sm ms-2"))
      } else if (identical(mode, "inapp") && !has_dec) {
        div(class = "alert alert-secondary mb-2 mt-2", style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-info-circle me-1"),
            "Klik 'Identifikasi Alternatif' untuk memuat opsi.")
      } else {
        NULL
      }
    })
    outputOptions(output, "inapp_ready_hint_ui", suspendWhenHidden = FALSE)
    
    observeEvent(input$btn_reopen_workspace, { rv$workspace_open <- TRUE })
    
    observeEvent(
      list(input$alpha_val, input$threshold_serasi, input$threshold_padu,
           input$rtrw_priority_file, input$rzwp3k_priority_file,
           input$matrix_file),
      {
        if (!is.null(rv$alt_template_path) || !is.null(rv$inapp_decisions)) {
          rv$alt_template_path    <- NULL
          rv$idx_padan_map_alt    <- NULL
          rv$alt_status           <- NULL
          rv$alt_table_snapshot   <- NULL
          rv$inapp_decisions      <- NULL
          rv$inapp_committed      <- NULL
          rv$inapp_saved          <- NULL
          rv$inapp_options_df     <- NULL
          rv$workspace_open       <- FALSE
          rv$upload_loaded        <- FALSE
          rv$final_result         <- NULL
          rv$final_log            <- NULL
          showNotification(
            "Parameter rekomendasi berubah. Harap klik 'Identifikasi Alternatif' ulang.",
            type = "warning", duration = 8)
        }
      },
      ignoreInit = TRUE
    )
    
    observeEvent(input$matrix_file, {
      req(input$matrix_file)
      tryCatch({
        rv$matriks_serasi <- load_validate_matrix_table(input$matrix_file$datapath, title = "serasi")
        showNotification("Matriks Serasi berhasil dimuat.", type = "message", duration = 3)
      }, error = function(e) {
        rv$matriks_serasi <- NULL
        showNotification(paste("Gagal memuat Matriks Serasi:", e$message), type = "error", duration = 8)
      })
    })
    
    observeEvent(input$btn_prepare_options, {
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification("Direktori output belum diatur.", type = "error", duration = 5)
        return()
      }
      req(rv$idx_padan_map_filter, input$matrix_file,
          input$rtrw_priority_file, input$rzwp3k_priority_file)
      
      btn_sel <- sprintf("#%s", ns("btn_prepare_options"))
      shinyjs::runjs(sprintf("$('%s').addClass('laspur-btn-loading').prop('disabled', true);",
                             btn_sel))
      on.exit({
        shinyjs::runjs(sprintf("$('%s').removeClass('laspur-btn-loading').prop('disabled', false);",
                               btn_sel))
      }, add = TRUE)
      
      withProgress(message = "Mengidentifikasi Alternatif", value = 0, {
        tryCatch({
          incProgress(0.1, detail = "Memuat matriks serasi...")
          rv$matriks_serasi <- load_validate_matrix_table(
            input$matrix_file$datapath, title = "serasi")
          
          incProgress(0.2, detail = "Memuat tabel prioritas...")
          rtrw_prior_tbl <- load_and_validate_table(input$rtrw_priority_file$datapath)
          rz_prior_tbl   <- load_and_validate_table(input$rzwp3k_priority_file$datapath)
          
          chk_rtrw <- .validate_priority_table(rtrw_prior_tbl, "RTRW")
          if (!chk_rtrw$ok) stop(chk_rtrw$msg)
          chk_rz <- .validate_priority_table(rz_prior_tbl, "RZWP3K")
          if (!chk_rz$ok) stop(chk_rz$msg)
          
          priority_rtrw_vec <- rtrw_prior_tbl$RTRW[rtrw_prior_tbl$Prioritas == 1]
          priority_rz_vec   <- rz_prior_tbl$RZWP3K[rz_prior_tbl$Prioritas == 1]
          
          rv$priority_rtrw   <- priority_rtrw_vec
          rv$priority_rzwp3k <- priority_rz_vec
          rv$alpha           <- input$alpha_val
          rv$threshold_serasi <- input$threshold_serasi
          rv$threshold_padu   <- input$threshold_padu
          
          out_dir_step2 <- file.path(output_dir(), "Penyusunan Alternatif")
          dir.create(out_dir_step2, recursive = TRUE, showWarnings = FALSE)
          
          incProgress(0.5, detail = "Menghitung opsi alternatif...")
          result <- determine_alternative_zones(
            idx_padan_map_filter = rv$idx_padan_map_filter,
            serasi_matrix        = rv$matriks_serasi,
            step                 = "step1",
            n_alt                = 5L,
            output_dir           = out_dir_step2
          )
          
          incProgress(0.8, detail = "Menyiapkan tabel keputusan...")
          generated <- file.path(out_dir_step2, "overlaps_alternative_zones_selections.xlsx")
          if (!file.exists(generated))
            stop("Template dibuat tetapi file .xlsx tidak ditemukan di folder output.")
          rv$alt_template_path <- generated
          
          df <- result$data
          if (is.null(df) || nrow(df) == 0) stop("Generator tidak mengembalikan data opsi.")
          rv$inapp_options_df <- df
          
          extract_by_id_pu <- function(map, col) {
            if (is.null(map) || !inherits(map, "sf") || !col %in% names(map)) {
              return(setNames(numeric(0), character(0)))
            }
            gdf <- sf::st_drop_geometry(map)
            if (!"id_pu" %in% names(gdf)) return(setNames(numeric(0), character(0)))
            gdf <- gdf[!is.na(gdf[[col]]), c("id_pu", col), drop = FALSE]
            if (nrow(gdf) == 0) return(setNames(numeric(0), character(0)))
            gdf <- gdf[!duplicated(gdf$id_pu), ]
            setNames(as.numeric(gdf[[col]]), as.character(gdf$id_pu))
          }
          idx_padu_lookup  <- extract_by_id_pu(rv$idx_padan_map_filter, "idx_padu_final")
          idx_padan_lookup <- extract_by_id_pu(rv$idx_padan_map_filter, "idx_padan")
          area_ha_lookup   <- extract_by_id_pu(rv$idx_padan_map_filter, "area_ha")
          
          df$idx_serasi     <- suppressWarnings(as.numeric(df$idx_serasi))
          df$idx_padu_final <- idx_padu_lookup[as.character(df$id_pu)]
          df$idx_padan      <- idx_padan_lookup[as.character(df$id_pu)]
          
          df$recommendation <- dplyr::case_when(
            is.na(df$RTRW) | is.na(df$RZWP3K) | is.na(df$idx_serasi) |
              is.na(df$idx_padu_final) ~ NA_character_,
            df$RTRW   %in% priority_rtrw_vec ~ "Tetap/Koordinasi",
            df$RZWP3K %in% priority_rz_vec   ~ "Ubah RTRW",
            df$idx_serasi     >= input$threshold_serasi ~ "Koordinasi",
            df$idx_padu_final >= input$threshold_padu   ~ "Ubah RZWP3K",
            df$idx_serasi < input$threshold_serasi &
              df$idx_padu_final < input$threshold_padu ~ "Ubah RTRW",
            TRUE ~ NA_character_
          )
          
          top1_rtrw <- if ("alt_RTRW_1"   %in% names(df)) as.character(df$alt_RTRW_1)   else rep(NA_character_, nrow(df))
          top1_rz   <- if ("alt_RZWP3K_1" %in% names(df)) as.character(df$alt_RZWP3K_1) else rep(NA_character_, nrow(df))
          top1_rtrw[is.na(top1_rtrw) | top1_rtrw %in% c("", "No alternative")] <- NA_character_
          top1_rz[is.na(top1_rz)     | top1_rz   %in% c("", "No alternative")] <- NA_character_
          
          init <- data.frame(
            id_pu     = as.integer(df$id_pu),
            id_rtrw   = if ("id_rtrw"   %in% names(df)) as.character(df$id_rtrw)   else NA_character_,
            id_rzwp3k = if ("id_rzwp3k" %in% names(df)) as.character(df$id_rzwp3k) else NA_character_,
            RTRW      = as.character(df$RTRW),
            RZWP3K    = as.character(df$RZWP3K),
            admin     = if ("admin" %in% names(df)) as.character(df$admin) else NA_character_,
            length    = if ("length" %in% names(df)) as.numeric(df$length) else NA_real_,
            area_ha        = unname(area_ha_lookup[as.character(df$id_pu)]),
            idx_serasi     = df$idx_serasi,
            idx_padu_final = df$idx_padu_final,
            idx_padan      = df$idx_padan,
            recommendation = df$recommendation,
            stringsAsFactors = FALSE
          )
          
          sub_padu_cols <- c("idx_padu_ke", "idx_padu_hs", "idx_padu_kl",
                             "idx_padu_kh", "idx_padu_rtp", "idx_padu_se",
                             "idx_padu_ki")
          for (col in sub_padu_cols) {
            if (col %in% names(rv$idx_padan_map_filter)) {
              lk <- extract_by_id_pu(rv$idx_padan_map_filter, col)
              init[[col]] <- if (length(lk) > 0)
                unname(lk[as.character(init$id_pu)]) else NA_real_
            } else {
              init[[col]] <- NA_real_
            }
          }
          
          init$alt_RTRW   <- ifelse(
            !is.na(init$recommendation) & init$recommendation == "Ubah RTRW" & !is.na(top1_rtrw),
            top1_rtrw, init$RTRW)
          init$alt_RZWP3K <- ifelse(
            !is.na(init$recommendation) & init$recommendation == "Ubah RZWP3K" & !is.na(top1_rz),
            top1_rz, init$RZWP3K)
          init$use_recommendation <- "Ya"
          
          rv$inapp_decisions <- init
          rv$inapp_committed <- init
          rv$data_nonce      <- isolate(rv$data_nonce) + 1L
          rv$upload_loaded   <- FALSE
          
          rv$workspace_open <- FALSE
          rv$current_panel  <- "step2"
          
          showNotification(
            "Alternatif berhasil diidentifikasi. Klik 'Buka Halaman Kerja' untuk meninjau.",
            type = "message", duration = 5)
          incProgress(1.0, detail = "Selesai!")
        }, error = function(e) {
          rv$alt_template_path <- NULL
          showNotification(paste("Gagal mengidentifikasi alternatif:", e$message),
                           type = "error", duration = 10)
        })
      })
    })
    
    output$template_status_ui <- renderUI({
      req(rv$alt_template_path)
      div(class = "alert alert-success mb-2", style = "font-size: 0.8rem;",
          tags$i(class = "bi bi-check-circle me-2"),
          sprintf("Templat siap: %s", basename(rv$alt_template_path)))
    })
    
    output$dl_template <- downloadHandler(
      filename = function() {
        if (!is.null(rv$alt_template_path)) basename(rv$alt_template_path)
        else "overlaps_alternative_zones_selections.xlsx"
      },
      content = function(file) {
        req(rv$alt_template_path)
        file.copy(rv$alt_template_path, file, overwrite = TRUE)
      }
    )
    
    # ── Workspace helpers ────────────────────────────────
    inapp_filtered_idx <- reactive({
      rv$data_nonce
      dec <- isolate(rv$inapp_decisions)
      req(dec)
      keep <- rep(TRUE, nrow(dec))
      nf <- rv$node_filter
      if (!is.null(nf)) keep <- keep & dec$id_pu %in% nf$pu
      which(keep)
    })
    
    inapp_paged <- reactive({
      rv$table_nonce
      idx <- inapp_filtered_idx()
      dec <- isolate(rv$inapp_decisions)
      req(dec)
      if (length(idx) == 0) return(dec[0, , drop = FALSE])
      dec[idx, , drop = FALSE]
    })
    
    output$inapp_summary_ui <- renderUI({
      rv$data_nonce; rv$table_nonce
      dec <- isolate(rv$inapp_decisions); req(dec)
      n_total <- nrow(dec)
      n_ya    <- sum(dec$use_recommendation == "Ya",    na.rm = TRUE)
      n_tidak <- sum(dec$use_recommendation == "Tidak", na.rm = TRUE)
      n_ubah_r <- sum(dec$recommendation == "Ubah RTRW"   & dec$use_recommendation == "Ya", na.rm = TRUE)
      n_ubah_z <- sum(dec$recommendation == "Ubah RZWP3K" & dec$use_recommendation == "Ya", na.rm = TRUE)
      n_koord  <- sum(dec$recommendation %in% c("Tetap/Koordinasi", "Koordinasi") |
                        is.na(dec$recommendation), na.rm = TRUE)
      
      div(
        class = "laspur-summary-row",
        div(
          class = "laspur-summary-box box-total",
          tags$div(class = "laspur-summary-title", "Total kasus"),
          tags$div(class = "laspur-summary-value", sprintf("%d pasang", n_total))
        ),
        div(
          class = "laspur-summary-box box-usage",
          tags$div(class = "laspur-summary-title", "Opsi yang dikunci"),
          tags$div(class = "laspur-summary-value",
                   sprintf("Ya: %d \u00b7 Tidak: %d", n_ya, n_tidak))
        ),
        div(
          class = "laspur-summary-box box-outcome",
          tags$div(class = "laspur-summary-title", "Keputusan alternatif"),
          tags$div(class = "laspur-summary-value",
                   sprintf("Ubah ke RTRW: %d \u00b7 Ubah ke RZWP3K: %d \u00b7 Tetap: %d",
                           n_ubah_r, n_ubah_z, n_koord))
        )
      )
    })
    
    output$inapp_filter_chip_ui <- renderUI({
      nf <- rv$node_filter
      if (is.null(nf)) return(NULL)
      div(class = "alert alert-warning py-1 px-2 mb-2 d-flex justify-content-between align-items-center",
          style = "font-size: 0.8rem;",
          tags$span(tags$i(class = "bi bi-funnel-fill me-1"),
                    sprintf("Filter fitur %s \u2014 %d pasang", nf$label, length(nf$pu))),
          actionButton(ns("inapp_clear_node_filter"), "Hapus filter",
                       class = "btn-outline-secondary btn-sm py-0"))
    })
    
    observeEvent(input$inapp_clear_node_filter, {
      rv$node_filter <- NULL
      clear_selection()
    })
    
    # ── Workspace: table ─────────────────────────────────
    output$inapp_table <- reactable::renderReactable({
      req(is_workspace_mode())
      df <- inapp_paged()
      
      if (is.null(df) || nrow(df) == 0) {
        return(reactable::reactable(
          data.frame(Info = "Tidak ada baris"),
          outlined = TRUE, compact = TRUE, bordered = TRUE))
      }
      
      matriks_serasi <- isolate(rv$matriks_serasi)
      alpha_val      <- isolate(rv$alpha)
      if (is.null(alpha_val) || !is.finite(alpha_val)) alpha_val <- 0.5
      
      rec_v   <- as.character(df$recommendation)
      alt_r_v <- as.character(df$alt_RTRW)
      alt_z_v <- as.character(df$alt_RZWP3K)
      rtrw_v  <- as.character(df$RTRW)
      rz_v    <- as.character(df$RZWP3K)
      idx_padu_v <- suppressWarnings(as.numeric(df$idx_padu_final))
      
      r_new <- ifelse(!is.na(rec_v) & rec_v == "Ubah RTRW" &
                        !is.na(alt_r_v) & nzchar(alt_r_v),
                      alt_r_v, rtrw_v)
      z_new <- ifelse(!is.na(rec_v) & rec_v == "Ubah RZWP3K" &
                        !is.na(alt_z_v) & nzchar(alt_z_v),
                      alt_z_v, rz_v)
      
      serasi_lookup <- function(r, z) {
        if (is.na(r) || is.na(z) || is.null(matriks_serasi)) return(NA_real_)
        m <- matriks_serasi$idx_serasi[matriks_serasi$class1 == r &
                                         matriks_serasi$class2 == z]
        if (length(m) == 0) NA_real_ else as.numeric(m[1])
      }
      df$idx_serasi_new <- mapply(serasi_lookup, r_new, z_new, USE.NAMES = FALSE)
      df$idx_padan_new  <- alpha_val * df$idx_serasi_new +
        (1 - alpha_val) * idx_padu_v
      
      valid_rtrw <- if (!is.null(matriks_serasi)) unique(as.character(matriks_serasi$class1)) else character(0)
      valid_rz   <- if (!is.null(matriks_serasi)) unique(as.character(matriks_serasi$class2)) else character(0)
      valid_rtrw <- valid_rtrw[!is.na(valid_rtrw) & nzchar(valid_rtrw)]
      valid_rz   <- valid_rz[!is.na(valid_rz)     & nzchar(valid_rz)]
      
      opts_df    <- isolate(rv$inapp_options_df)
      opts_by_pu <- if (!is.null(opts_df) && nrow(opts_df) > 0)
        split(opts_df, as.character(opts_df$id_pu)) else list()
      
      build_opts_for_row <- function(pu, side, actual) {
        key <- as.character(pu)
        orow <- opts_by_pu[[key]]
        vals <- character(0)
        if (!is.null(orow) && nrow(orow) > 0) {
          prefix <- if (identical(side, "RTRW")) "^alt_RTRW_[0-9]+$"
          else                          "^alt_RZWP3K_[0-9]+$"
          cols <- grep(prefix, names(orow), value = TRUE)
          if (length(cols) > 0) {
            v <- unlist(orow[1, cols, drop = FALSE], use.names = FALSE)
            v <- v[!is.na(v) & v != "" & v != "No alternative"]
            v <- unique(as.character(v))
            if (length(v) > 0) vals <- v
          }
        }
        if (length(vals) == 0) {
          vals <- if (identical(side, "RTRW")) valid_rtrw else valid_rz
        }
        act <- if (is.null(actual) || length(actual) == 0 || is.na(actual)) "" else as.character(actual)
        if (nzchar(act)) {
          vals <- c(act, setdiff(vals, act))
        }
        vals
      }
      
      esc <- function(x) htmltools::htmlEscape(as.character(x), attribute = TRUE)
      
      make_select_html <- function(id_pu, side, current, opts, locked) {
        cur  <- if (is.null(current) || length(current) == 0 || is.na(current)) "" else as.character(current)
        opts <- as.character(opts)
        if (nzchar(cur) && !cur %in% opts) opts <- c(cur, opts)
        opts <- unique(opts)
        if (length(opts) == 0) opts <- cur
        
        opt_tags <- vapply(opts, function(v) {
          sprintf('<option value="%s"%s>%s</option>',
                  esc(v),
                  if (identical(v, cur)) ' selected' else '',
                  esc(v))
        }, character(1))
        
        disabled <- if (isTRUE(locked)) ' disabled' else ''
        input_id <- ns("inapp_cell_change")
        pu_int   <- suppressWarnings(as.integer(id_pu))
        if (is.na(pu_int)) pu_int <- -1L
        
        onchange_js <- sprintf(
          "Shiny.setInputValue('%s', {id_pu: %d, side: '%s', value: this.value, nonce: Math.random()}, {priority: 'event'});",
          input_id, pu_int, side
        )
        
        sprintf(
          '<select class="laspur-cell-select" data-id_pu="%d" data-side="%s"%s onchange="%s">%s</select>',
          pu_int, side, disabled, onchange_js,
          paste0(opt_tags, collapse = "")
        )
      }
      
      display_alt <- vapply(seq_len(nrow(df)), function(i) {
        rec <- rec_v[i]
        if (is.na(rec)) rec <- ""
        
        if (identical(rec, "Ubah RTRW")) {
          opts <- build_opts_for_row(df$id_pu[i], "RTRW", df$RTRW[i])
          make_select_html(df$id_pu[i], "RTRW", df$alt_RTRW[i], opts, FALSE)
        } else if (identical(rec, "Ubah RZWP3K")) {
          opts <- build_opts_for_row(df$id_pu[i], "RZWP3K", df$RZWP3K[i])
          make_select_html(df$id_pu[i], "RZWP3K", df$alt_RZWP3K[i], opts, FALSE)
        } else {
          locked_val <- df$RTRW[i]
          if (is.na(locked_val) || !nzchar(locked_val)) locked_val <- "\u2014"
          sprintf('<span style="display:inline-block;padding:2px 6px;background:#F1F5F9;border:1px solid #E2E8F0;border-radius:4px;color:#94A3B8;font-size:0.7rem;">%s</span>',
                  esc(locked_val))
        }
      }, character(1))
      
      display_use_rec <- vapply(seq_len(nrow(df)), function(i) {
        cur <- as.character(df$use_recommendation[i])
        if (is.na(cur) || !nzchar(cur)) cur <- "Ya"
        make_select_html(df$id_pu[i], "use_recommendation", cur,
                         c("Ya", "Tidak"), FALSE)
      }, character(1))
      
      display <- df[, intersect(
        c("id_pu", "recommendation",
          "RTRW", "RZWP3K", "id_rtrw", "id_rzwp3k", "admin",
          "area_ha",
          "idx_serasi", "idx_serasi_new", "idx_padu_final",
          "idx_padan", "idx_padan_new",
          "idx_padu_ke", "idx_padu_hs", "idx_padu_kl", "idx_padu_kh",
          "idx_padu_rtp", "idx_padu_se", "idx_padu_ki"),
        names(df)), drop = FALSE]
      
      ensure_num <- function(d, col) {
        if (!col %in% names(d)) d[[col]] <- NA_real_
        d[[col]] <- suppressWarnings(as.numeric(d[[col]]))
        d
      }
      for (col in c("area_ha",
                    "idx_serasi", "idx_serasi_new", "idx_padu_final",
                    "idx_padan", "idx_padan_new",
                    "idx_padu_ke", "idx_padu_hs", "idx_padu_kl", "idx_padu_kh",
                    "idx_padu_rtp", "idx_padu_se", "idx_padu_ki")) {
        display <- ensure_num(display, col)
      }
      
      display$.alt_zone_ <- display_alt
      display$.use_rec   <- display_use_rec
      
      final_order <- c(
        "id_pu",
        "recommendation",
        ".alt_zone_",
        "RTRW",
        "RZWP3K",
        ".use_rec",
        "id_rtrw",
        "id_rzwp3k",
        "admin",
        "area_ha",
        "idx_serasi",
        "idx_serasi_new",
        "idx_padu_final",
        "idx_padan",
        "idx_padan_new",
        "idx_padu_ke",
        "idx_padu_hs",
        "idx_padu_kl",
        "idx_padu_kh",
        "idx_padu_rtp",
        "idx_padu_se",
        "idx_padu_ki"
      )
      final_order <- intersect(final_order, names(display))
      display <- display[, final_order, drop = FALSE]
      
      padu_sub_cols <- c("idx_padu_ke", "idx_padu_hs", "idx_padu_kl",
                         "idx_padu_kh", "idx_padu_rtp", "idx_padu_se",
                         "idx_padu_ki")
      padu_sub_cols <- intersect(padu_sub_cols, names(display))
      
      apply_padu_row_scale <- function(d, cols) {
        n <- nrow(d)
        if (n == 0 || length(cols) == 0) return(d)
        mat <- as.matrix(d[, cols, drop = FALSE]); storage.mode(mat) <- "double"
        rmin <- apply(mat, 1, function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else min(x) })
        rmax <- apply(mat, 1, function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else max(x) })
        ramp <- grDevices::colorRamp(c("#EAF2FB", "#08519C"))
        for (j in seq_along(cols)) {
          col <- cols[j]; vals <- mat[, j]; out <- character(n)
          for (i in seq_len(n)) {
            v <- vals[i]
            if (!is.finite(v)) { out[i] <- ""; next }
            mn <- rmin[i]; mx <- rmax[i]
            nv <- if (!is.finite(mn) || !is.finite(mx) || mx == mn) 0.5
            else (v - mn) / (mx - mn)
            nv <- max(0, min(1, nv))
            rgb <- ramp(nv)
            bg  <- grDevices::rgb(rgb[1,1], rgb[1,2], rgb[1,3], maxColorValue = 255)
            tx  <- if (nv > 0.65) "#FFFFFF" else "#1E293B"
            out[i] <- sprintf('<span style="display:block;margin:-3px -5px;padding:3px 5px;background:%s;color:%s;">%s</span>',
                              bg, tx, formatC(v, format = "f", digits = 2))
          }
          d[[col]] <- out
        }
        d
      }
      display <- apply_padu_row_scale(display, padu_sub_cols)
      
      num2_fmt <- reactable::colFormat(digits = 2)
      
      col_defs <- list(
        id_pu           = reactable::colDef(name = "ID PU",                    minWidth = 70),
        recommendation  = reactable::colDef(name = "Rekomendasi",              minWidth = 140),
        .alt_zone_      = reactable::colDef(name = "Opsi Alternatif",          html = TRUE, minWidth = 190),
        RTRW            = reactable::colDef(name = "RTRW Aktual",              minWidth = 180),
        RZWP3K          = reactable::colDef(name = "RZWP3K Aktual",            minWidth = 180),
        .use_rec        = reactable::colDef(name = "Kunci Opsi?",              html = TRUE, minWidth = 110),
        id_rtrw         = reactable::colDef(name = "ID RTRW",                  minWidth = 90),
        id_rzwp3k       = reactable::colDef(name = "ID RZWP3K",                minWidth = 90),
        admin           = reactable::colDef(name = "Wilayah Administratif",    minWidth = 140),
        area_ha         = reactable::colDef(name = "Luas (ha)",                minWidth = 130, format = num2_fmt),
        idx_serasi      = reactable::colDef(name = "Indeks SERASI Aktual",     minWidth = 130, format = num2_fmt),
        idx_serasi_new  = reactable::colDef(name = "Potensi Indeks SERASI Baru", minWidth = 140, format = num2_fmt),
        idx_padu_final  = reactable::colDef(name = "Indeks PADU Kombinasi",    minWidth = 130, format = num2_fmt),
        idx_padan       = reactable::colDef(name = "Indeks PADAN Aktual",      minWidth = 130, format = num2_fmt),
        idx_padan_new   = reactable::colDef(name = "Potensi Indeks PADAN Baru", minWidth = 140, format = num2_fmt),
        idx_padu_ke     = reactable::colDef(name = "Indeks PADU-KE",  html = TRUE, minWidth = 110),
        idx_padu_hs     = reactable::colDef(name = "Indeks PADU-HS",  html = TRUE, minWidth = 110),
        idx_padu_kl     = reactable::colDef(name = "Indeks PADU-KL",  html = TRUE, minWidth = 110),
        idx_padu_kh     = reactable::colDef(name = "Indeks PADU-KH",  html = TRUE, minWidth = 110),
        idx_padu_rtp    = reactable::colDef(name = "Indeks PADU-RTp", html = TRUE, minWidth = 110),
        idx_padu_se     = reactable::colDef(name = "Indeks PADU-SE",  html = TRUE, minWidth = 110),
        idx_padu_ki     = reactable::colDef(name = "Indeks PADU-KI",  html = TRUE, minWidth = 110)
      )
      
      reactable::reactable(
        display,
        columns       = col_defs,
        pagination    = FALSE,
        height        = "auto",
        outlined      = TRUE,
        bordered      = TRUE,
        compact       = TRUE,
        striped       = TRUE,
        highlight     = TRUE,
        rownames      = FALSE,
        defaultColDef = reactable::colDef(vAlign = "center", headerVAlign = "center"),
        onClick = reactable::JS(sprintf(
          "function(rowInfo, colInfo, evt) {
             var t = evt && evt.target;
             if (t && (t.tagName === 'SELECT' || (t.parentNode && t.parentNode.tagName === 'SELECT'))) return;
             var pu = rowInfo.row['id_pu'];
             if (pu === null || pu === undefined) return;
             Shiny.setInputValue('%s', {id_pu: pu, nonce: Math.random()}, {priority: 'event'});
           }", ns("inapp_row_pick")))
      )
    })
    
    observeEvent(input$inapp_cell_change, {
      info <- input$inapp_cell_change
      req(info, rv$inapp_decisions)
      
      pu   <- suppressWarnings(as.integer(info$id_pu))
      side <- as.character(info$side)
      val  <- as.character(info$value)
      req(!is.na(pu), nzchar(side))
      
      idx <- match(pu, rv$inapp_decisions$id_pu)
      req(!is.na(idx))
      
      if (identical(side, "RTRW")) {
        rv$inapp_decisions$alt_RTRW[idx] <- val
      } else if (identical(side, "RZWP3K")) {
        rv$inapp_decisions$alt_RZWP3K[idx] <- val
      } else if (identical(side, "use_recommendation")) {
        rv$inapp_decisions$use_recommendation[idx] <- val
      }
      
      rv$data_nonce  <- isolate(rv$data_nonce) + 1L
      rv$table_nonce <- isolate(rv$table_nonce) + 1L
      rv$map_nonce   <- isolate(rv$map_nonce)  + 1L
    })
    
    observeEvent(input$inapp_row_pick, {
      pu <- suppressWarnings(as.integer(input$inapp_row_pick$id_pu))
      req(!is.na(pu))
      if (identical(as.integer(rv$selected_pu), pu)) return()
      if (!is.null(rv$node_filter) && !(pu %in% rv$node_filter$pu)) {
        rv$node_filter <- NULL
      }
      rv$selected_pu <- pu
      rv$sel_source  <- "table"
    })
    
    observeEvent(input$inapp_reset_all, {
      req(rv$inapp_committed)
      rv$inapp_decisions <- rv$inapp_committed
      bump_table()
      showNotification("Keputusan dikembalikan ke nilai awal.",
                       type = "message", duration = 3)
    })
    
    # ── Map ──────────────────────────────────────────────
    padan_map_4326 <- reactive({
      m <- rv$idx_padan_map_filter
      req(inherits(m, "sf"))
      if (!"id" %in% names(m)) m$id <- NA_character_
      if (!"area_ha" %in% names(m)) m$area_ha <- NA_real_
      keep <- intersect(c("id", "id_pu", "id_rtrw", "id_rzwp3k",
                          "RTRW", "RZWP3K", "area_ha"),
                        names(m))
      m <- m[, keep]
      if (is.na(sf::st_crs(m))) sf::st_crs(m) <- 4326
      else m <- sf::st_transform(m, 4326)
      m
    })
    
    output$group_map <- leaflet::renderLeaflet({
      req(rv$map_latch)
      leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
        leaflet::addProviderTiles("Esri.WorldGrayCanvas", group = "Peta Dasar") %>%
        leaflet::addProviderTiles("Esri.WorldImagery",    group = "Citra Satelit") %>%
        leaflet::addLayersControl(baseGroups = c("Peta Dasar", "Citra Satelit"),
                                  options = leaflet::layersControlOptions(collapsed = TRUE)) %>%
        leaflet::addLegend(
          position = "bottomright", opacity = 0.9, title = "Polygon",
          colors = c("#1565C0", "#2E7D32"), labels = c("RTRW", "RZWP3K")) %>%
        leaflet::setView(lng = 118, lat = -2, zoom = 5)
    })
    
    observeEvent(input$group_map_bounds, {
      if (!isTRUE(rv$map_ready)) rv$map_ready <- TRUE
    })
    
    fit_map_to <- function(x) {
      if (is.null(x) || nrow(x) == 0) return(invisible(NULL))
      bb <- sf::st_bbox(x)
      if (anyNA(bb)) return(invisible(NULL))
      leaflet::leafletProxy("group_map", session = session) %>%
        leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]],
                           options = list(maxZoom = 16))
      invisible(NULL)
    }
    
    observe({
      map_trigger()
      rv$map_ready
      
      sel <- isolate(rv$selected_pu)
      dec <- isolate(rv$inapp_decisions)
      
      if (!isTRUE(isolate(rv$workspace_open))) return()
      if (!isTRUE(rv$map_ready)) return()
      if (is.null(dec) || nrow(dec) == 0) return()
      
      proxy <- leaflet::leafletProxy("group_map", session = session) %>%
        leaflet::clearGroup("Konteks") %>%
        leaflet::clearGroup("Terpilih")
      
      tryCatch(proxy <- proxy %>% leaflet.extras::removeSearchFeatures(),
               error = function(e) NULL)
      
      tryCatch({
        m <- padan_map_4326()
        m$recommendation <- dec$recommendation[match(m$id_pu, dec$id_pu)]
        
        .dash <- function(x) ifelse(is.na(x) | !nzchar(as.character(x)), "-", as.character(x))
        
        build_polys <- function(x, id_prefix, weight, fill_op, opacity) {
          if (is.null(x) || nrow(x) == 0) return(NULL)
          is_r <- !is.na(x$RTRW)
          id_r_v <- if ("id_rtrw"   %in% names(x)) .dash(x$id_rtrw)   else rep("-", nrow(x))
          id_z_v <- if ("id_rzwp3k" %in% names(x)) .dash(x$id_rzwp3k) else rep("-", nrow(x))
          zone_id <- ifelse(is_r, id_r_v, id_z_v)
          lbl <- sprintf("%s | %s | %s | %s | id_pu %s",
                         zone_id,
                         ifelse(is_r, .dash(x$RTRW), .dash(x$RZWP3K)),
                         .dash(x$recommendation),
                         ifelse(is_r, "RTRW", "RZWP3K"),
                         x$id_pu)
          list(
            data    = x,
            layerId = paste0(id_prefix, "_", x$id_pu, "_", ifelse(is_r, "R", "Z")),
            color   = ifelse(is_r, "#1565C0", "#2E7D32"),
            weight  = weight, fill = fill_op, opacity = opacity,
            label   = lbl,
            popup   = sprintf("<b>%s</b><br/>ID: %s<br/>Zona: %s<br/>Rekomendasi: %s<br/>id_pu: %s",
                              ifelse(is_r, "RTRW", "RZWP3K"), zone_id,
                              ifelse(is_r, .dash(x$RTRW), .dash(x$RZWP3K)),
                              .dash(x$recommendation), x$id_pu)
          )
        }
        
        ctx_pu <- setdiff(m$id_pu, sel)
        ctx <- build_polys(m[m$id_pu %in% ctx_pu, ], "ctx", 1, 0.12, 0.6)
        slc <- build_polys(m[m$id_pu %in% sel, ],    "sel", 3, 0.55, 1)
        
        if (!is.null(ctx)) {
          proxy <- proxy %>% leaflet::addPolygons(
            data = ctx$data, layerId = ctx$layerId, color = ctx$color,
            weight = ctx$weight, opacity = ctx$opacity, fillColor = ctx$color,
            fillOpacity = ctx$fill, label = ctx$label, popup = ctx$popup,
            group = "Konteks",
            highlightOptions = leaflet::highlightOptions(weight = 3, bringToFront = TRUE))
        }
        if (!is.null(slc)) {
          proxy <- proxy %>% leaflet::addPolygons(
            data = slc$data, layerId = slc$layerId, color = slc$color,
            weight = slc$weight, opacity = slc$opacity, fillColor = slc$color,
            fillOpacity = slc$fill, label = slc$label, popup = slc$popup,
            group = "Terpilih")
        }
        
        if (!is.null(ctx) || !is.null(slc)) {
          proxy %>% leaflet.extras::addSearchFeatures(
            targetGroups = c("Konteks", "Terpilih"),
            options = leaflet.extras::searchFeaturesOptions(
              propertyName = "label", zoom = 15,
              openPopup = TRUE, firstTipSubmit = TRUE,
              autoCollapse = FALSE, hideMarkerOnCollapse = TRUE))
        }
      }, error = function(e) {
        message("[recom_overlaps] map proxy error: ", conditionMessage(e))
      })
    })
    
    observeEvent(
      list(rv$fit_nonce, rv$map_ready),
      {
        req(isTRUE(rv$workspace_open), isTRUE(rv$map_ready))
        tryCatch(fit_map_to(padan_map_4326()),
                 error = function(e) message("[recom_overlaps] fit error: ", conditionMessage(e)))
      }, ignoreInit = TRUE)
    
    observeEvent(rv$selected_pu, {
      req(length(rv$selected_pu) > 0,
          identical(rv$sel_source, "table"),
          isTRUE(rv$workspace_open), isTRUE(rv$map_ready))
      tryCatch({
        m <- padan_map_4326()
        fit_map_to(m[m$id_pu %in% rv$selected_pu, ])
      }, error = function(e) message("[recom_overlaps] fit-sel error: ", conditionMessage(e)))
    }, ignoreInit = TRUE)
    
    observeEvent(input$group_map_shape_click, {
      id <- input$group_map_shape_click$id
      req(id, grepl("^(ctx|sel)_[0-9]+_[RZ]$", id))
      pu <- as.integer(sub("^(ctx|sel)_([0-9]+)_([RZ])$", "\\2", id))
      if (identical(as.integer(rv$selected_pu), pu)) return()
      rv$selected_pu <- pu
      rv$sel_source  <- "map"
    })
    
    # ── Commit decisions ─────────────────────────────────
    commit_decisions <- function() {
      if (is.null(rv$inapp_decisions) || is.null(rv$idx_padan_map_filter)) return(FALSE)
      
      df <- rv$inapp_decisions
      
      if (!is.null(rv$matriks_serasi)) {
        valid_rtrw <- unique(as.character(rv$matriks_serasi$class1))
        valid_rz   <- unique(as.character(rv$matriks_serasi$class2))
        
        changed_r <- !is.na(df$recommendation) & df$recommendation == "Ubah RTRW"
        changed_z <- !is.na(df$recommendation) & df$recommendation == "Ubah RZWP3K"
        bad_r <- unique(df$alt_RTRW[changed_r   & !df$alt_RTRW   %in% valid_rtrw])
        bad_z <- unique(df$alt_RZWP3K[changed_z & !df$alt_RZWP3K %in% valid_rz])
        bad_r <- bad_r[!is.na(bad_r)]
        bad_z <- bad_z[!is.na(bad_z)]
        
        if (length(bad_r) > 0 || length(bad_z) > 0) {
          showNotification(
            paste0(
              "Nilai alternatif tidak valid: ",
              if (length(bad_r) > 0) paste0("alt_RTRW: ",   paste(bad_r, collapse = ", "), ". ") else "",
              if (length(bad_z) > 0) paste0("alt_RZWP3K: ", paste(bad_z, collapse = ", "), ". ") else "",
              "Pastikan nilainya ada pada Matriks SERASI."),
            type = "error", duration = 10)
          return(FALSE)
        }
      }
      
      alt_per_pu <- df %>%
        dplyr::transmute(
          id_pu      = as.integer(id_pu),
          alt_RTRW   = ifelse(!is.na(recommendation) & recommendation == "Ubah RTRW",
                              alt_RTRW, RTRW),
          alt_RZWP3K = ifelse(!is.na(recommendation) & recommendation == "Ubah RZWP3K",
                              alt_RZWP3K, RZWP3K),
          recommendation,
          use_recommendation
        )
      
      base_filtered <- rv$idx_padan_map_filter
      drop_cols <- intersect(c("alt_RTRW", "alt_RZWP3K",
                               "recommendation", "use_recommendation"),
                             names(base_filtered))
      if (length(drop_cols) > 0)
        base_filtered <- base_filtered[, setdiff(names(base_filtered), drop_cols)]
      
      idx_padan_map_alt <- dplyr::left_join(base_filtered, alt_per_pu, by = "id_pu")
      if (!"recommendation" %in% names(idx_padan_map_alt) ||
          !"use_recommendation" %in% names(idx_padan_map_alt)) {
        showNotification("Penggabungan gagal: kolom penting hilang.",
                         type = "error", duration = 8)
        return(FALSE)
      }
      
      rv$idx_padan_map_alt  <- idx_padan_map_alt
      rv$alt_table_snapshot <- alt_per_pu
      rv$inapp_saved        <- rv$inapp_decisions
      showNotification(sprintf("Keputusan tersimpan: %d pasang.", nrow(alt_per_pu)),
                       type = "message", duration = 4)
      TRUE
    }
    
    observeEvent(input$inapp_save, { commit_decisions() })
    
    is_decisions_saved <- function() {
      !is.null(rv$idx_padan_map_alt) &&
        !is.null(rv$inapp_saved) &&
        identical(rv$inapp_decisions, rv$inapp_saved)
    }
    
    go_to_step3 <- function() {
      rv$unlocked <- max(rv$unlocked, 3L)
      go_to_panel("step3")
    }
    
    observeEvent(input$btn_workspace_to_step3, {
      req(rv$inapp_decisions)
      if (is_decisions_saved()) { go_to_step3(); return() }
      showModal(modalDialog(
        title = "Simpan keputusan?",
        "Keputusan pada halaman kerja belum disimpan atau ada perubahan sejak ",
        "penyimpanan terakhir. Simpan keputusan dan lanjut ke Langkah 3?",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Batal"),
          actionButton(ns("btn_confirm_save_step3"),
                       tagList(tags$i(class = "bi bi-save me-1"), "Simpan & Lanjut"),
                       class = "btn-success")
        )
      ))
    })
    
    observeEvent(input$btn_confirm_save_step3, {
      removeModal()
      if (isTRUE(commit_decisions())) go_to_step3()
    })
    
    output$inapp_dl_draft <- downloadHandler(
      filename = function() {
        ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
        sprintf("Draf Alternatif Tumpang Tindih %s.xlsx", ts)
      },
      content = function(file) {
        df <- rv$inapp_decisions
        req(df)
        cols <- c("id_pu", "id_rtrw", "id_rzwp3k",
                  "RTRW", "RZWP3K", "admin", "length",
                  "area_ha",
                  "idx_serasi", "idx_padu_final", "idx_padan",
                  "recommendation", "alt_RTRW", "alt_RZWP3K", "use_recommendation")
        cols <- intersect(cols, names(df))
        openxlsx::write.xlsx(as.data.frame(df)[, cols, drop = FALSE], file)
      }
    )
    
    # ── Manual template upload ───────────────────────────
    observe({
      req(input$alt_upload)
      if (is.null(rv$matriks_serasi) || is.null(rv$idx_padan_map_filter)) {
        missing <- c(
          if (is.null(rv$matriks_serasi))       "Matriks Serasi (.xlsx)",
          if (is.null(rv$idx_padan_map_filter)) "Peta PADAN yang sudah difilter"
        )
        rv$alt_status <- list(
          ok  = FALSE,
          msg = paste0("File template telah diunggah. Menunggu input berikut sebelum dapat divalidasi:\n- ",
                       paste(missing, collapse = "\n- ")),
          preview = NULL
        )
        return()
      }
      tryCatch({
        alt_table <- load_and_validate_table(input$alt_upload$datapath)
        
        required_core <- c("id_pu", "recommendation", "alt_RTRW", "alt_RZWP3K",
                           "use_recommendation")
        missing_core <- setdiff(required_core, names(alt_table))
        if (length(missing_core) > 0) {
          rv$idx_padan_map_alt <- NULL
          rv$alt_status <- list(
            ok = FALSE,
            msg = paste("Kolom wajib tidak ditemukan:",
                        paste(missing_core, collapse = ", ")),
            preview = NULL)
          return()
        }
        
        check <- .validate_alt_table_overlaps(alt_table, rv$matriks_serasi)
        if (!check$ok) {
          rv$idx_padan_map_alt <- NULL
          rv$alt_status <- list(ok = FALSE, msg = check$msg, preview = NULL)
          return()
        }
        
        bad_use <- unique(alt_table$use_recommendation[
          !is.na(alt_table$use_recommendation) &
            !as.character(alt_table$use_recommendation) %in% c("Ya", "Tidak")])
        if (length(bad_use) > 0) {
          rv$idx_padan_map_alt <- NULL
          rv$alt_status <- list(
            ok = FALSE,
            msg = paste0("Nilai 'use_recommendation' tidak valid: ",
                         paste(bad_use, collapse = ", ")),
            preview = NULL)
          return()
        }
        
        df <- as.data.frame(alt_table, stringsAsFactors = FALSE)
        extract_by_id_pu <- function(map, col) {
          if (is.null(map) || !inherits(map, "sf") || !col %in% names(map)) return(numeric(0))
          gdf <- sf::st_drop_geometry(map)
          if (!"id_pu" %in% names(gdf)) return(numeric(0))
          gdf <- gdf[!is.na(gdf[[col]]), c("id_pu", col), drop = FALSE]
          if (nrow(gdf) == 0) return(numeric(0))
          gdf <- gdf[!duplicated(gdf$id_pu), ]
          setNames(as.numeric(gdf[[col]]), as.character(gdf$id_pu))
        }
        idx_padu_lk  <- extract_by_id_pu(rv$idx_padan_map_filter, "idx_padu_final")
        idx_padan_lk <- extract_by_id_pu(rv$idx_padan_map_filter, "idx_padan")
        area_ha_lk   <- extract_by_id_pu(rv$idx_padan_map_filter, "area_ha")
        
        if (!"idx_serasi"     %in% names(df)) df$idx_serasi     <- NA_real_
        if (!"idx_padu_final" %in% names(df)) df$idx_padu_final <- idx_padu_lk[as.character(df$id_pu)]
        if (!"idx_padan"      %in% names(df)) df$idx_padan      <- idx_padan_lk[as.character(df$id_pu)]
        if (!"admin"          %in% names(df)) df$admin          <- NA_character_
        if (!"length"         %in% names(df)) df$length         <- NA_real_
        if (!"area_ha"        %in% names(df)) df$area_ha        <- unname(area_ha_lk[as.character(df$id_pu)])
        
        sub_padu_cols <- c("idx_padu_ke", "idx_padu_hs", "idx_padu_kl",
                           "idx_padu_kh", "idx_padu_rtp", "idx_padu_se",
                           "idx_padu_ki")
        for (col in sub_padu_cols) {
          if (col %in% names(rv$idx_padan_map_filter)) {
            lk <- extract_by_id_pu(rv$idx_padan_map_filter, col)
            df[[col]] <- if (length(lk) > 0) unname(lk[as.character(df$id_pu)]) else NA_real_
          } else {
            df[[col]] <- NA_real_
          }
        }
        
        df$id_pu <- as.integer(df$id_pu)
        
        new_snapshot <- df[, intersect(
          c("id_pu", "id_rtrw", "id_rzwp3k", "alt_RTRW", "alt_RZWP3K",
            "use_recommendation", "recommendation"), names(df))]
        changed <- is.null(rv$alt_table_snapshot) ||
          !identical(new_snapshot, rv$alt_table_snapshot)
        if (changed) { rv$final_result <- NULL; rv$final_log <- NULL }
        
        rv$alt_table_snapshot <- new_snapshot
        rv$inapp_decisions    <- df
        rv$inapp_committed    <- df
        rv$inapp_saved        <- df
        rv$upload_loaded      <- TRUE
        
        alt_per_pu <- df %>%
          dplyr::transmute(
            id_pu      = as.integer(id_pu),
            alt_RTRW   = ifelse(!is.na(recommendation) & recommendation == "Ubah RTRW",
                                alt_RTRW, RTRW),
            alt_RZWP3K = ifelse(!is.na(recommendation) & recommendation == "Ubah RZWP3K",
                                alt_RZWP3K, RZWP3K),
            recommendation,
            use_recommendation
          )
        base_filtered <- rv$idx_padan_map_filter
        drop_cols <- intersect(c("alt_RTRW", "alt_RZWP3K",
                                 "recommendation", "use_recommendation"),
                               names(base_filtered))
        if (length(drop_cols) > 0)
          base_filtered <- base_filtered[, setdiff(names(base_filtered), drop_cols)]
        rv$idx_padan_map_alt <- dplyr::left_join(base_filtered, alt_per_pu, by = "id_pu")
        
        rv$alt_status <- list(
          ok = TRUE,
          msg = sprintf("Templat/Draf dimuat: %d pasang. Klik 'Buka Halaman Kerja' untuk meninjau.",
                        nrow(df)),
          preview = NULL)
        showNotification(
          "Berkas berhasil dimuat. Klik 'Buka Halaman Kerja' untuk melanjutkan.",
          type = "message", duration = 6)
      }, error = function(e) {
        rv$idx_padan_map_alt <- NULL
        rv$alt_status <- list(ok = FALSE,
                              msg = paste("Gagal membaca file:", e$message),
                              preview = NULL)
      })
    })
    
    output$alt_validation_ui <- renderUI({
      req(rv$alt_status)
      if (!rv$alt_status$ok) {
        div(class = "alert alert-danger", style = "white-space: pre-wrap;",
            tags$i(class = "bi bi-exclamation-triangle-fill me-2"), rv$alt_status$msg)
      } else {
        div(class = "alert alert-success mb-2",
            tags$i(class = "bi bi-check-circle me-2"), rv$alt_status$msg)
      }
    })
    
    observeEvent(input$btn_back_2, go_to_panel("step1"))
    observeEvent(input$btn_next_2, {
      if (is.null(rv$idx_padan_map_alt)) {
        showNotification(
          "Belum ada keputusan tersimpan. Simpan keputusan di Halaman Kerja atau unggah templat.",
          type = "warning", duration = 8)
        return()
      }
      rv$unlocked <- max(rv$unlocked, 3L)
      go_to_panel("step3")
    })
    
    # ── Step 3 ───────────────────────────────────────────
    output$step3_ui <- renderUI({
      tagList(
        tags$p(style = "color:#6c757d; font-size:0.85rem; margin-bottom: 8px;",
               "Parameter (alpha, threshold, prioritas pola) telah diatur pada Langkah 2. ",
               "Klik tombol di bawah untuk menghitung keputusan akhir dan menyimpan hasil."),
        tags$div(class = "alert alert-info mb-2", style = "font-size: 0.85rem;",
                 tags$div(sprintf("Alpha (\u03B1): %.2f", rv$alpha %||% 0.5)),
                 tags$div(sprintf("Ambang SERASI: %.2f", rv$threshold_serasi %||% 0.6)),
                 tags$div(sprintf("Ambang PADU: %.2f", rv$threshold_padu %||% 0.65))),
        if (is.null(output_dir()) || !nzchar(output_dir())) {
          div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
              tags$i(class = "bi bi-exclamation-triangle me-1"),
              "Direktori output belum diatur. Atur terlebih dahulu di menu utama.")
        },
        div(style = "display: flex; gap: 8px; flex-wrap: wrap;",
            actionButton(ns("btn_run_final"),
                         tagList(tags$i(class = "bi bi-play-fill me-1"),
                                 "Jalankan Analisis Alternatif"),
                         class = "btn-success btn-sm")),
        .step_nav(ns, back_id = "btn_back_3", next_id = NULL)
      )
    })
    
    observeEvent(input$btn_run_final, {
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification("Direktori output belum diatur.", type = "error", duration = 5)
        return()
      }
      req(rv$idx_padan_map_alt, rv$priority_rtrw, rv$priority_rzwp3k, rv$matriks_serasi)
      
      rv$final_result <- NULL
      withProgress(message = "Membuat Alternatif Tumpang Tindih", value = 0, {
        tryCatch({
          incProgress(0.2, detail = "Menyiapkan parameter...")
          priority_rtrw   <- rv$priority_rtrw
          priority_rzwp3k <- rv$priority_rzwp3k
          alpha            <- rv$alpha %||% input$alpha_val
          threshold_serasi <- rv$threshold_serasi %||% input$threshold_serasi
          threshold_padu   <- rv$threshold_padu   %||% input$threshold_padu
          
          incProgress(0.4, detail = "Menghitung indeks padan alternatif...")
          df <- rv$idx_padan_map_alt
          if (!all(c("alt_RTRW", "alt_RZWP3K", "recommendation",
                     "use_recommendation") %in% names(df))) {
            stop("Kolom alternatif tidak lengkap. Harap siapkan opsi ulang pada Langkah 2.")
          }
          df <- df %>% dplyr::mutate(
            idx_serasi_rtrw_alt   = mapply(function(r, z)
              .get_serasi(r, z, rv$matriks_serasi),
              alt_RTRW, RZWP3K),
            idx_serasi_rzwp3k_alt = mapply(function(r, z)
              .get_serasi(r, z, rv$matriks_serasi),
              RTRW, alt_RZWP3K)
          )
          df <- df %>% dplyr::mutate(
            idx_padan_rtrw_alt   = alpha * idx_serasi_rtrw_alt   + (1 - alpha) * idx_padu_final,
            idx_padan_rzwp3k_alt = alpha * idx_serasi_rzwp3k_alt + (1 - alpha) * idx_padu_final
          )
          
          incProgress(0.6, detail = "Membuat keputusan akhir...")
          df <- df %>% dplyr::mutate(
            decision = dplyr::case_when(
              is.na(recommendation) | is.na(idx_padan_rzwp3k_alt) | is.na(idx_padan) |
                is.na(alt_RZWP3K) | is.na(idx_padan_rtrw_alt) | is.na(alt_RTRW) ~ NA_character_,
              use_recommendation != "Ya" ~ "Tetap/Koordinasi",
              recommendation == "Ubah RZWP3K" & idx_padan_rzwp3k_alt >  idx_padan ~ paste("Ubah RZWP3K ke", alt_RZWP3K),
              recommendation == "Ubah RZWP3K" & idx_padan_rzwp3k_alt <= idx_padan ~ "Tetap/Koordinasi",
              recommendation == "Ubah RTRW"   & idx_padan_rtrw_alt   >  idx_padan ~ paste("Ubah RTRW ke",   alt_RTRW),
              recommendation == "Ubah RTRW"   & idx_padan_rtrw_alt   <= idx_padan ~ "Tetap/Koordinasi",
              TRUE ~ "Tetap/Koordinasi"
            )
          )
          df <- df %>% dplyr::mutate(
            idx_padan_final = dplyr::case_when(
              grepl("Ubah RTRW",   decision, fixed = TRUE) ~ idx_padan_rtrw_alt,
              grepl("Ubah RZWP3K", decision, fixed = TRUE) ~ idx_padan_rzwp3k_alt,
              grepl("Tetap/Koordinasi", decision, fixed = TRUE) ~ idx_padan,
              TRUE ~ NA_real_
            )
          )
          
          incProgress(0.8, detail = "Menyimpan hasil...")
          recom_overlaps_dir <- file.path(output_dir(), "Penyusunan Alternatif")
          if (!dir.exists(recom_overlaps_dir))
            dir.create(recom_overlaps_dir, recursive = TRUE, showWarnings = FALSE)
          out_gpkg <- file.path(recom_overlaps_dir, "idx_alternatives_overlaps.gpkg")
          out_xlsx <- file.path(recom_overlaps_dir, "idx_alternatives_overlaps.xlsx")
          sf::st_write(df, out_gpkg, delete_dsn = TRUE, quiet = TRUE)
          openxlsx::write.xlsx(sf::st_drop_geometry(df), out_xlsx)
          
          log_lines <- c(
            "Ringkasan alternatif:",
            capture.output(print(table(df$recommendation, useNA = "ifany"))),
            "",
            "Ringkasan keputusan akhir:",
            capture.output(print(table(df$decision, useNA = "ifany")))
          )
          rv$final_result <- list(map = df, table = sf::st_drop_geometry(df),
                                  gpkg_path = out_gpkg, xlsx_path = out_xlsx)
          rv$final_log <- paste(log_lines, collapse = "\n")
          
          out <- list(
            inputs = list(
              start_time           = Sys.time(),
              case                 = "overlaps",
              idx_padan_file       = input$idx_padan_file$name,
              idx_padan_source     = rv$idx_padan_source,
              rtrw_priority_file   = input$rtrw_priority_file$name,
              rzwp3k_priority_file = input$rzwp3k_priority_file$name,
              alpha                = alpha,
              matriks_serasi       = rv$matriks_serasi,
              threshold_serasi     = threshold_serasi,
              threshold_padu       = threshold_padu,
              decision_mode        = input$decision_mode %||% "inapp",
              output_dir           = output_dir()
            ),
            result = list(
              idx_alternative_overlaps_map   = df,
              idx_alternative_overlaps_table = sf::st_drop_geometry(df)
            )
          )
          
          log_dir <- file.path(recom_overlaps_dir, "log")
          if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
          tryCatch({
            inputs <- out$inputs
            save(inputs, file = file.path(log_dir, "idx_alternatives_overlaps.rda"))
          }, error = function(e) warning("Gagal menulis file log: ", e$message))
          
          session$userData$module_results$recommendation <- out
          plot_categorical_map(
            map = df, title = "Peta Opsi Alternatif Kasus Tumpang Tindih",
            column = "recommendation", legend = "Opsi Alternatif",
            filepath = file.path(log_dir, "peta_opsi_alternatif_tumpang_tindih.png")
          )
          showNotification("Berhasil! Hasil analisis alternatif telah disimpan.",
                           type = "message")
          incProgress(1.0, detail = "Selesai!")
        }, error = function(e) {
          call_txt <- if (!is.null(conditionCall(e)))
            paste0("\n(pada pemanggilan: ", paste(deparse(conditionCall(e)), collapse = " "), ")") else ""
          rv$final_log <- paste0("Error: ", conditionMessage(e), call_txt)
          showNotification(paste("Gagal:", conditionMessage(e)),
                           type = "error", duration = NULL)
        })
      })
    })
    
    observeEvent(input$btn_back_3, go_to_panel("step2"))
    
    output$status_box <- renderUI({
      if (!is.null(rv$final_result)) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Selesai. Silakan lanjut ke ", tags$strong("Langkah Rekonsiliasi"), ".")
      } else if (!is.null(rv$final_log) && grepl("^Error", rv$final_log)) {
        div(class = "alert alert-danger mb-0",
            tags$i(class = "bi bi-exclamation-triangle-fill me-2"),
            "Terjadi kesalahan, lihat Log Validasi.")
      } else if (rv$unlocked >= 3) {
        div(class = "alert alert-secondary mb-0", "Siap dijalankan.")
      } else {
        div(class = "alert alert-secondary mb-0", "Lengkapi langkah sebelumnya.")
      }
    })
    
    observe({
      if (!is.null(rv$final_result)) {
        rv$analysis_result <- list(map = rv$final_result$map, table = rv$final_result$table)
        rv$gpkg_path    <- rv$final_result$gpkg_path
        rv$xlsx_path    <- rv$final_result$xlsx_path
        rv$log_messages <- if (!is.null(rv$final_log)) rv$final_log else ""
      } else {
        rv$analysis_result <- NULL
        rv$gpkg_path       <- NULL
        rv$xlsx_path       <- NULL
        rv$log_messages    <- if (!is.null(rv$final_log)) rv$final_log else "Siap untuk penyusunan alternatif."
      }
    })
    
    recom_overlaps_config <- list(
      map_color_col  = "recommendation",
      map_title      = "Alternatif Awal",
      map_palette    = c("blue", "green", "orange", "red", "purple"),
      map_label_cols = list(
        "ID PU"      = "id_pu", "RTRW" = "RTRW", "RZWP3K" = "RZWP3K",
        "Alternatif" = "recommendation", "Keputusan" = "decision"
      ),
      table_cols = c(
        "id_pu"                 = "ID PU",
        "RTRW"                  = "RTRW",
        "RZWP3K"                = "RZWP3K",
        "area_ha"               = "Luas (ha)",
        "admin"                 = "Administrasi",
        "idx_serasi"            = "Indeks SERASI Awal",
        "idx_padu_final"        = "Indeks PADU Kombinasi",
        "alt_RTRW"              = "RTRW Alternatif",
        "alt_RZWP3K"            = "RZWP3K Alternatif",
        "idx_serasi_rtrw_alt"   = "Indeks SERASI RTRW Alternatif",
        "idx_serasi_rzwp3k_alt" = "Indeks SERASI RZWP3K Alternatif",
        "idx_padan_rtrw_alt"    = "Indeks PADAN RTRW Alternatif",
        "idx_padan_rzwp3k_alt"  = "Indeks PADAN RZWP3K Alternatif",
        "recommendation"        = "Opsi Alternatif",
        "use_recommendation"    = "Kunci Opsi?",
        "decision"              = "Keputusan Alternatif",
        "idx_padan_final"       = "Indeks PADAN Akhir"
      ),
      table_round_cols = c(
        "Luas (ha)", "Indeks SERASI Awal", "Indeks PADU Kombinasi",
        "Indeks SERASI RTRW Alternatif", "Indeks SERASI RZWP3K Alternatif",
        "Indeks PADAN RTRW Alternatif", "Indeks PADAN RZWP3K Alternatif",
        "Indeks PADAN Akhir"
      )
    )
    
    render_result_server(input, output, session, rv, recom_overlaps_config)
  })
}