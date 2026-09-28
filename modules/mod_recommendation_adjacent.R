# ui/modules/mod_recommendation_adjacent.R
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
      actionButton(ns(back_id), tagList(tags$i(class = "bi bi-arrow-left me-1"), "Kembali"),
                   class = "btn-outline-secondary btn-sm")
    } else div(),
    if (!is.null(next_id)) {
      actionButton(ns(next_id), tagList(next_label, tags$i(class = "bi bi-arrow-right ms-1")),
                   class = "btn-success btn-sm")
    } else div()
  )
}

.validate_alt_table <- function(alt_table, matriks_serasi) {
  has_id  <- "id" %in% names(alt_table)
  has_ids <- all(c("id_rtrw", "id_rzwp3k") %in% names(alt_table))
  if (!has_id && !has_ids) {
    return(list(ok = FALSE, msg = paste0(
      "Kolom 'id' atau pasangan 'id_rtrw'/'id_rzwp3k' tidak ditemukan ",
      "pada file yang diunggah.")))
  }
  required_cols <- c("id_pu", "alt_RTRW", "alt_RZWP3K")
  missing_cols <- setdiff(required_cols, names(alt_table))
  if (length(missing_cols) > 0) {
    return(list(ok = FALSE, msg = sprintf(
      "Kolom wajib tidak ditemukan pada file yang diunggah: %s",
      paste(missing_cols, collapse = ", "))))
  }
  valid_rtrw <- unique(as.character(matriks_serasi$class1))
  valid_rz   <- unique(as.character(matriks_serasi$class2))
  bad_rtrw <- unique(as.character(alt_table$alt_RTRW[
    !is.na(alt_table$alt_RTRW) & !as.character(alt_table$alt_RTRW) %in% valid_rtrw]))
  bad_rz <- unique(as.character(alt_table$alt_RZWP3K[
    !is.na(alt_table$alt_RZWP3K) & !as.character(alt_table$alt_RZWP3K) %in% valid_rz]))
  if (length(bad_rtrw) > 0 || length(bad_rz) > 0) {
    msg <- "Ditemukan nilai zona alternatif yang tidak dikenali pada Matriks SERASI."
    if (length(bad_rtrw) > 0)
      msg <- paste0(msg, sprintf("\n- alt_RTRW tidak valid: %s", paste(bad_rtrw, collapse = ", ")))
    if (length(bad_rz) > 0)
      msg <- paste0(msg, sprintf("\n- alt_RZWP3K tidak valid: %s", paste(bad_rz, collapse = ", ")))
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
      zone_col, paste(missing_cols, collapse = ", "))))
  }
  list(ok = TRUE, msg = "Validasi berhasil.")
}

.classify_integrasi <- function(score, th_high, th_med, th_low) {
  dplyr::case_when(
    score >= th_high ~ "Sangat Terintegrasi",
    score >= th_med  ~ "Terintegrasi",
    score >= th_low  ~ "Kurang Terintegrasi",
    TRUE             ~ "Tidak Terintegrasi"
  )
}

recommendation_adjacent_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("4. Analisis Penyusunan Alternatif Area Bertetangga", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menentukan opsi penyelesaian konflik dalam integrasi tata ruang darat-laut.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
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
              accordion_panel("Langkah 3 — Menghitung Nilai Ekonomi (Opsional)", value = "step3",
                              icon = tags$i(class = "bi bi-cash-coin"),
                              uiOutput(ns("step3_ui"))),
              accordion_panel("Langkah 4 — Menentukan Keputusan Alternatif", value = "step4",
                              icon = tags$i(class = "bi bi-check2-circle"),
                              uiOutput(ns("step4_ui")))
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
        .inapp-toolbar-row { display: flex; gap: 8px; align-items: stretch; margin-bottom: 8px; }
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

        .laspur-cell-select {
          width: 100%;
          max-width: 100%;
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
      ")),
      fluidRow(
        class = "g-3",
        column(
          width = 8,
          card(
            card_header(
              div(
                class = "d-flex justify-content-between align-items-center",
                tagList(tags$i(class = "bi bi-table me-1"), "Keputusan Pasangan "),
                uiOutput(ns("inapp_group_label_short"), inline = TRUE)
              )
            ),
            div(
              class = "inapp-toolbar-row",
              div(class = "inapp-dd",
                  selectInput(ns("inapp_group_jump"), NULL,
                              choices = c("Semua grup" = ""),
                              width = "100%")),
              div(class = "inapp-btn",
                  actionButton(ns("inapp_reset_all"),
                               tagList(tags$i(class = "bi bi-arrow-counterclockwise me-1"), "Reset Keputusan"),
                               class = "btn-danger btn-sm"))
            ),
            uiOutput(ns("inapp_filter_chip_ui")),
            uiOutput(ns("inapp_summary_ui")),
            div(class = "inapp-table-host",
                reactable::reactableOutput(ns("inapp_table"))),
            uiOutput(ns("inapp_pagination_ui")),
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
            card_header(tagList(tags$i(class = "bi bi-map me-1"), "Peta")),
            leaflet::leafletOutput(ns("group_map"), height = "600px")
          )
        )
      )
    )
  )
}

recommendation_adjacent_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shinyjs::useShinyjs()
    
    rv <- reactiveValues(
      unlocked = 1,
      idx_padan_map        = NULL,
      idx_padan_source     = NULL,
      filter_snapshot       = NULL,
      idx_padan_map_filter  = NULL,
      count_before          = NULL,
      count_after           = NULL,
      matriks_serasi        = NULL,
      alt_template_path     = NULL,
      alt_table_snapshot    = NULL,
      idx_padan_map_alt     = NULL,
      alt_status            = NULL,
      adjacent_economy_map  = NULL,
      npv_status            = NULL,
      final_result          = NULL,
      final_log             = NULL,
      analysis_result = NULL,
      gpkg_path       = NULL,
      xlsx_path       = NULL,
      log_messages    = "",
      priority_rtrw           = NULL,
      priority_rzwp3k         = NULL,
      alpha                   = 0.5,
      recommendation_snapshot = NULL,
      inapp_decisions        = NULL,
      inapp_committed        = NULL,
      inapp_options_df       = NULL,
      inapp_page             = 1L,
      inapp_group_selected   = NA_integer_,
      inapp_group_order      = integer(0),
      current_panel     = "step1",
      workspace_open    = FALSE,
      upload_loaded     = FALSE,
      selected_pu       = integer(0),
      sel_source        = "none",
      node_filter       = NULL,
      table_nonce       = 0L,
      data_nonce        = 0L,
      fit_nonce         = 0L,
      map_latch         = FALSE,
      map_ready         = FALSE,
      inapp_saved       = NULL
    )

    go_to_panel <- function(value) {
      rv$workspace_open <- FALSE
      rv$current_panel  <- value
      bslib::accordion_panel_set(id = "wizard", values = value, session = session)
    }

    is_workspace_mode <- reactive({
      identical(rv$current_panel, "step2") && isTRUE(rv$workspace_open)
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
          ", ns("inapp_table"), ns("group_map")))
        })
      } else {
        shinyjs::runjs(sprintf("$('#%s').show(); $('#%s').hide();",
                               ns("setup_view"), ns("workspace_view")))
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$btn_back_to_setup, {
      rv$workspace_open <- FALSE
    })
    
    reset_from_step2 <- function() {
      rv$idx_padan_map_alt  <- NULL
      rv$alt_table_snapshot <- NULL
      rv$alt_status         <- NULL
      rv$adjacent_economy_map <- NULL
      rv$npv_status         <- NULL
      rv$final_result       <- NULL
      rv$final_log          <- NULL
      rv$inapp_decisions    <- NULL
      rv$inapp_committed    <- NULL
      rv$inapp_saved        <- NULL
      rv$inapp_options_df   <- NULL
      rv$inapp_page         <- 1L
      rv$inapp_group_selected <- NA_integer_
      rv$inapp_group_order  <- integer(0)
      rv$workspace_open     <- FALSE
      rv$node_filter        <- NULL
      rv$selected_pu        <- integer(0)
      rv$sel_source         <- "none"
      rv$upload_loaded      <- FALSE
      rv$unlocked           <- min(rv$unlocked, 2)
    }
    reset_from_step3 <- function() {
      rv$adjacent_economy_map <- NULL
      rv$npv_status   <- NULL
      rv$final_result <- NULL
      rv$final_log    <- NULL
      rv$unlocked     <- min(rv$unlocked, 3)
    }
    .clear_downstream_alt <- function() {
      rv$idx_padan_map_alt    <- NULL
      rv$alt_table_snapshot   <- NULL
      rv$adjacent_economy_map <- NULL
      rv$npv_status           <- NULL
      rv$final_result         <- NULL
      rv$final_log            <- NULL
      rv$upload_loaded        <- FALSE
      rv$unlocked             <- min(rv$unlocked, 2)
    }
    
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
          rv$idx_padan_map    <- normalize_legacy_ids(map)
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
        reset_from_step2()
        rv$unlocked             <- 1
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
    
    output$step1_body_ui <- renderUI({
      if (is.null(rv$idx_padan_map)) {
        return(tagList(
          tags$p(style = "color: #6c757d; margin-top: 8px;",
                 "Unggah peta PADAN untuk mengaktifkan filter."),
          .step_nav(ns, back_id = NULL, next_id = "btn_next_1",
                    next_label = "Lanjut ke Step 2")))
      }
      tagList(
        bslib::layout_column_wrap(
          width = 1/2,
          div(
            checkboxInput(ns("apply_length"), "Aktifkan Filter Panjang Segmen", value = TRUE),
            conditionalPanel(
              condition = paste0("input['", ns("apply_length"), "']"),
              numericInput(ns("length_filter"), "Ambang Panjang (meter)", value = 1500, min = 0, step = 50))
          ),
          div(
            checkboxInput(ns("apply_idx"), "Aktifkan Filter Indeks PADAN", value = TRUE),
            conditionalPanel(
              condition = paste0("input['", ns("apply_idx"), "']"),
              numericInput(ns("idx_filter"), "Ambang Indeks PADAN (<)", value = 0.75, min = 0, max = 1, step = 0.05))
          )
        ),
        uiOutput(ns("filter_count_ui")),
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Step 2")
      )
    })
    
    observeEvent(input$idx_padan_file, {
      req(input$idx_padan_file)
      showNotification("Memuat peta PADAN...", type = "message", duration = 2)
      tryCatch({
        loaded_map <- sf::st_read(input$idx_padan_file$datapath, quiet = TRUE)
        if ("id_group.x" %in% names(loaded_map) || "id_group.y" %in% names(loaded_map)) {
          gx <- if ("id_group.x" %in% names(loaded_map)) loaded_map$id_group.x else NA
          gy <- if ("id_group.y" %in% names(loaded_map)) loaded_map$id_group.y else NA
          loaded_map$id_group <- dplyr::coalesce(gx, gy)
          loaded_map$id_group.x <- NULL
          loaded_map$id_group.y <- NULL
        }
        if (!"id_group" %in% names(loaded_map))
          stop("Peta PADAN harus memiliki kolom 'id_group' untuk kasus bertetangga.")
        loaded_map <- normalize_legacy_ids(loaded_map)
        rv$idx_padan_map    <- loaded_map
        rv$idx_padan_source <- "manual"
        rv$idx_padan_map_filter <- NULL
        rv$filter_snapshot <- NULL
        rv$count_before <- NULL
        rv$count_after <- NULL
        reset_from_step2()
        rv$unlocked <- 1
        
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
      if (isTRUE(input$apply_length)) keep <- keep & (as.numeric(map$length) > input$length_filter)
      if (isTRUE(input$apply_idx))    keep <- keep & (map$idx_padan < input$idx_filter)
      list(filtered = map[keep, ], before = nrow(map) / 2, after = nrow(map[keep, ]) / 2)
    })
    
    observeEvent(filtered_data(), {
      res <- filtered_data()
      rv$idx_padan_map_filter <- res$filtered
      rv$count_before <- res$before
      rv$count_after  <- res$after
      if (!is.null(rv$filter_snapshot)) {
        current <- list(
          apply_length = input$apply_length, length_filter = input$length_filter,
          apply_idx = input$apply_idx,       idx_filter = input$idx_filter)
        if (!identical(current, rv$filter_snapshot)) reset_from_step2()
      }
    })
    
    output$filter_count_ui <- renderUI({
      req(!is.null(rv$count_before))
      removed <- rv$count_before - rv$count_after
      pct <- if (rv$count_before > 0) (1 - rv$count_after / rv$count_before) * 100 else 0
      div(class = "alert alert-info", style = "margin-top: 12px;",
          tags$div(sprintf("Sebelum filter: %d pasang", rv$count_before)),
          tags$div(sprintf("Setelah filter: %d pasang", rv$count_after)),
          tags$div(sprintf("Terhapus: %d pasang (%.1f%%)", removed, pct)))
    })
    
    observeEvent(input$btn_next_1, {
      rv$filter_snapshot <- list(
        apply_length = input$apply_length, length_filter = input$length_filter,
        apply_idx = input$apply_idx,       idx_filter = input$idx_filter)
      rv$unlocked <- max(rv$unlocked, 2)
      go_to_panel("step2")
    })
    
    # Step 2 setup UI
    output$step2_ui <- renderUI({
      tagList(
        fileInput(ns("matrix_file"), "Pilih Matriks Serasi (.xlsx)", accept = ".xlsx"),
        hr(),
        h6("Parameter Rekomendasi Sistem", style = "font-weight: 700;"),
        tags$p(style = "color:#6c757d; font-size:0.82rem; margin: 0 0 8px 0;",
               "Rekomendasi sisi yang berubah (RTRW / RZWP3K) dihitung dari ",
               "alternatif terbaik pada masing-masing sisi."),
        bslib::layout_column_wrap(
          width = 1/2,
          fileInput(ns("rtrw_priority_file"),
                    "Tabel Acuan Pola RTRW (.xlsx)", accept = ".xlsx"),
          fileInput(ns("rzwp3k_priority_file"),
                    "Tabel Acuan Pola RZWP3K (.xlsx)", accept = ".xlsx")
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
    
    observeEvent(input$btn_reopen_workspace, {
      rv$workspace_open <- TRUE
    })
    
    observeEvent(
      list(input$alpha_val, input$rtrw_priority_file, input$rzwp3k_priority_file,
           input$matrix_file),
      {
        if (!is.null(rv$alt_template_path) || !is.null(rv$inapp_decisions)) {
          rv$alt_template_path    <- NULL
          rv$idx_padan_map_alt    <- NULL
          rv$alt_status           <- NULL
          rv$alt_table_snapshot   <- NULL
          rv$adjacent_economy_map <- NULL
          rv$npv_status           <- NULL
          rv$final_result         <- NULL
          rv$final_log            <- NULL
          rv$inapp_decisions      <- NULL
          rv$inapp_committed      <- NULL
          rv$inapp_saved          <- NULL
          rv$inapp_options_df     <- NULL
          rv$inapp_page           <- 1L
          rv$inapp_group_selected <- NA_integer_
          rv$inapp_group_order    <- integer(0)
          rv$workspace_open       <- FALSE
          rv$upload_loaded        <- FALSE
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
          
          out_dir_step2 <- file.path(output_dir(), "Penyusunan Alternatif")
          dir.create(out_dir_step2, recursive = TRUE, showWarnings = FALSE)
          
          incProgress(0.5, detail = "Menghitung opsi alternatif + rekomendasi...")
          result <- determine_alternative_zones(
            idx_padan_map_filter   = rv$idx_padan_map_filter,
            serasi_matrix          = rv$matriks_serasi,
            n_alt                  = 5L,
            step                   = "step2",
            output_dir             = out_dir_step2,
            compute_recommendation = TRUE,
            priority_rtrw          = priority_rtrw_vec,
            priority_rzwp3k        = priority_rz_vec,
            alpha                  = input$alpha_val
          )
          
          incProgress(0.8, detail = "Menyiapkan tabel keputusan...")
          generated <- file.path(out_dir_step2, "adjacent_alternative_zones_selections.xlsx")
          if (!file.exists(generated))
            stop("Template dibuat tetapi file .xlsx tidak ditemukan di folder output.")
          rv$alt_template_path <- generated
          rv$recommendation_snapshot <- list(
            priority_rtrw   = priority_rtrw_vec,
            priority_rzwp3k = priority_rz_vec,
            alpha           = input$alpha_val
          )
          
          df <- result$data
          rv$inapp_options_df <- df
          
          top1_rtrw <- if ("alt_RTRW_1"   %in% names(df)) as.character(df$alt_RTRW_1)   else rep(NA_character_, nrow(df))
          top1_rz   <- if ("alt_RZWP3K_1" %in% names(df)) as.character(df$alt_RZWP3K_1) else rep(NA_character_, nrow(df))
          top1_rtrw[is.na(top1_rtrw) | top1_rtrw %in% c("", "No alternative")] <- NA_character_
          top1_rz[is.na(top1_rz)     | top1_rz   %in% c("", "No alternative")] <- NA_character_
          
          if (!"recommendation" %in% names(df)) {
            stop("Output determine_alternative_zones tidak memiliki kolom 'recommendation'. ",
                 "Periksa apakah compute_recommendation = TRUE.")
          }
          
          init <- data.frame(
            id_pu          = as.integer(df$id_pu),
            id_group       = as.integer(df$id_group),
            id_rtrw        = if ("id_rtrw"   %in% names(df)) as.character(df$id_rtrw)   else NA_character_,
            id_rzwp3k      = if ("id_rzwp3k" %in% names(df)) as.character(df$id_rzwp3k) else NA_character_,
            RTRW           = as.character(df$RTRW),
            RZWP3K         = as.character(df$RZWP3K),
            admin          = if ("admin" %in% names(df)) as.character(df$admin) else NA_character_,
            length         = as.numeric(df$length),
            area_ha_rtrw   = if ("area_ha_rtrw"   %in% names(df)) as.numeric(df$area_ha_rtrw)   else NA_real_,
            area_ha_rzwp3k = if ("area_ha_rzwp3k" %in% names(df)) as.numeric(df$area_ha_rzwp3k) else NA_real_,
            idx_serasi     = if ("idx_serasi"     %in% names(df)) as.numeric(df$idx_serasi)     else NA_real_,
            recommendation = as.character(df$recommendation),
            stringsAsFactors = FALSE
          )
          
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
          idx_padan_lookup <- extract_by_id_pu(rv$idx_padan_map_filter, "idx_padan")
          idx_padu_lookup  <- extract_by_id_pu(rv$idx_padan_map_filter, "idx_padu_final")
          
          init$idx_padan      <- idx_padan_lookup[as.character(init$id_pu)]
          init$idx_padu_final <- idx_padu_lookup[as.character(init$id_pu)]
          
          init$area_ha_total <- ifelse(
            is.na(init$area_ha_rtrw) & is.na(init$area_ha_rzwp3k), NA_real_,
            rowSums(cbind(init$area_ha_rtrw, init$area_ha_rzwp3k), na.rm = TRUE)
          )
          
          init$alt_RTRW <- ifelse(
            init$recommendation == "Ubah RTRW" & !is.na(top1_rtrw),
            top1_rtrw, init$RTRW
          )
          init$alt_RZWP3K <- ifelse(
            init$recommendation == "Ubah RZ" & !is.na(top1_rz),
            top1_rz, init$RZWP3K
          )
          init$use_recommendation <- "Ya"
          
          rv$inapp_decisions <- init
          rv$inapp_committed <- init
          rv$inapp_page      <- 1L
          rv$node_filter     <- NULL
          rv$selected_pu     <- integer(0)
          rv$sel_source      <- "none"
          rv$data_nonce      <- isolate(rv$data_nonce) + 1L
          rv$upload_loaded   <- FALSE
          
          grp_order <- sort(unique(init$id_group))
          rv$inapp_group_order    <- grp_order
          rv$inapp_group_selected <- grp_order[1]
          
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
        if (!is.null(rv$alt_template_path)) basename(rv$alt_template_path) else "template_alternatif.xlsx"
      },
      content = function(file) {
        req(rv$alt_template_path)
        file.copy(rv$alt_template_path, file, overwrite = TRUE)
      }
    )
    
    # Workspace page
    PAGE_SIZE <- 25L
    
    bump_table <- function() rv$table_nonce <- isolate(rv$table_nonce) + 1L
    
    clear_selection <- function() {
      rv$node_filter <- NULL
      rv$selected_pu <- integer(0)
      rv$sel_source  <- "none"
    }
    
    set_group <- function(new_grp) {
      rv$inapp_group_selected <- new_grp
      rv$inapp_page <- 1L
      clear_selection()
    }
    
    select_pair <- function(pu, source) {
      nf <- rv$node_filter
      if (!is.null(nf) && !(pu %in% nf$pu)) {
        rv$node_filter <- NULL
        rv$inapp_page  <- 1L
      }
      rv$selected_pu <- as.integer(pu)
      rv$sel_source  <- source
    }
    
    .na_dash <- function(x) ifelse(is.na(x) | x == "", "-", as.character(x))
    
    group_choices <- function() {
      dec   <- isolate(rv$inapp_decisions)
      order <- isolate(rv$inapp_group_order)
      if (is.null(dec)) return(c("Semua grup" = ""))
      if (length(order) == 0) order <- sort(unique(dec$id_group))
      order <- sort(unique(as.integer(order)))
      counts <- table(dec$id_group)
      labels <- sprintf("Grup %d  \u00b7  %d pasang",
                        order, as.integer(counts[as.character(order)]))
      c("Semua grup" = "", stats::setNames(as.character(order), labels))
    }
    
    validation_pools <- reactive({
      xlsx <- rv$alt_template_path
      if (is.null(xlsx) || !file.exists(xlsx)) return(NULL)
      tryCatch({
        sheets <- openxlsx::getSheetNames(xlsx)
        if (!all(c("Data", "Validation_Lists") %in% sheets)) return(NULL)
        vl <- openxlsx::read.xlsx(xlsx, sheet = "Validation_Lists")
        ds <- openxlsx::read.xlsx(xlsx, sheet = "Data")
        if (is.null(vl) || nrow(vl) == 0) return(NULL)
        if (is.null(ds) || !"id_pu" %in% names(ds)) return(NULL)
        n  <- min(nrow(vl), nrow(ds))
        vl <- vl[seq_len(n), , drop = FALSE]
        vl$id_pu <- ds$id_pu[seq_len(n)]
        vl
      }, error = function(e) {
        message("[recom_adjacent] Failed to read validation pools: ", e$message)
        NULL
      })
    })
    
    observeEvent(input$inapp_group_jump, {
      val <- input$inapp_group_jump
      new_grp <- if (is.null(val) || !nzchar(val)) NA_integer_ else as.integer(val)
      if (!identical(rv$inapp_group_selected, new_grp)) set_group(new_grp)
    }, ignoreInit = TRUE)
    
    observeEvent(rv$inapp_decisions, {
      req(rv$inapp_decisions)
      updateSelectInput(
        session, "inapp_group_jump",
        choices  = group_choices(),
        selected = {
          g <- rv$inapp_group_selected
          if (is.null(g) || is.na(g)) "" else as.character(g)
        }
      )
    }, ignoreInit = TRUE)
    
    output$inapp_group_label_short <- renderUI({
      rv$data_nonce
      dec <- isolate(rv$inapp_decisions)
      req(dec)
      cur <- rv$inapp_group_selected
      n <- if (is.na(cur)) nrow(dec) else sum(dec$id_group == cur)
      tags$small(style = "color:#6c757d;", sprintf("%d pasang", n))
    })
    
    inapp_filtered_idx <- reactive({
      rv$data_nonce
      dec <- isolate(rv$inapp_decisions)
      req(dec)
      keep <- rep(TRUE, nrow(dec))
      cur <- rv$inapp_group_selected
      if (!is.na(cur)) keep <- keep & dec$id_group == cur
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
      total_pages <- max(1L, ceiling(length(idx) / PAGE_SIZE))
      page <- min(max(1L, rv$inapp_page), total_pages)
      rows <- idx[seq.int((page - 1L) * PAGE_SIZE + 1L, min(page * PAGE_SIZE, length(idx)))]
      dec[rows, , drop = FALSE]
    })
    
    output$inapp_table <- reactable::renderReactable({
      req(is_workspace_mode())
      df <- inapp_paged()
      
      if (is.null(df) || nrow(df) == 0) {
        return(
          reactable::reactable(
            data.frame(Info = "Tidak ada baris untuk grup ini"),
            outlined = TRUE, compact = TRUE, bordered = TRUE
          )
        )
      }
      
      keep_cols <- c(
        "id_rtrw", "id_rzwp3k", "recommendation", "alt_RTRW", "alt_RZWP3K",
        "use_recommendation", "RTRW", "RZWP3K", "id_pu", "admin",
        "length", "area_ha_rtrw", "area_ha_rzwp3k",
        "idx_serasi", "idx_padu_final", "idx_padan"
      )
      keep_cols <- intersect(keep_cols, names(df))
      display <- df[, keep_cols, drop = FALSE]
      
      if (!"admin" %in% names(display)) display$admin <- NA_character_
      if (!"idx_padu_final" %in% names(display)) display$idx_padu_final <- NA_real_
      if (!"idx_padan" %in% names(display)) display$idx_padan <- NA_real_
      
      display$area_ha_total <- ifelse(
        is.na(display$area_ha_rtrw) & is.na(display$area_ha_rzwp3k), NA_real_,
        rowSums(cbind(display$area_ha_rtrw, display$area_ha_rzwp3k), na.rm = TRUE)
      )
      
      pools   <- isolate(validation_pools())
      opts_df <- isolate(rv$inapp_options_df)
      
      pools_by_pu <- if (!is.null(pools) && nrow(pools) > 0)
        split(pools, as.character(pools$id_pu)) else list()
      opts_by_pu  <- if (!is.null(opts_df) && nrow(opts_df) > 0)
        split(opts_df, as.character(opts_df$id_pu)) else list()
      
      ms <- isolate(rv$matriks_serasi)
      fallback_rtrw <- if (!is.null(ms)) unique(as.character(ms$class1)) else character(0)
      fallback_rz   <- if (!is.null(ms)) unique(as.character(ms$class2)) else character(0)
      
      build_row_source <- function(pu, side) {
        key <- as.character(pu)
        row <- pools_by_pu[[key]]
        if (!is.null(row) && nrow(row) > 0) {
          prefix <- if (identical(side, "RTRW")) "^RTRW_" else "^RZWP3K_"
          cols <- grep(prefix, names(row), value = TRUE)
          if (length(cols) > 0) {
            vals <- unlist(row[, cols, drop = FALSE], use.names = FALSE)
            vals <- vals[!is.na(vals) & vals != "" & vals != "No alternative"]
            vals <- unique(as.character(vals))
            if (length(vals) > 0) return(vals)
          }
        }
        orow <- opts_by_pu[[key]]
        if (!is.null(orow) && nrow(orow) > 0) {
          prefix <- if (identical(side, "RTRW")) "^alt_RTRW_[0-9]+$"
          else                          "^alt_RZWP3K_[0-9]+$"
          cols <- grep(prefix, names(orow), value = TRUE)
          if (length(cols) > 0) {
            vals <- unlist(orow[, cols, drop = FALSE], use.names = FALSE)
            vals <- vals[!is.na(vals) & vals != "" & vals != "No alternative"]
            vals <- unique(as.character(vals))
            if (length(vals) > 0) return(vals)
          }
        }
        if (identical(side, "RTRW")) fallback_rtrw else fallback_rz
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
      
      rec_vec <- as.character(display$recommendation)
      
      display$alt_RTRW <- vapply(seq_len(nrow(display)), function(i) {
        locked <- !identical(rec_vec[i], "Ubah RTRW")
        cur    <- display$alt_RTRW[i]
        opts   <- if (locked) cur else {
          unique(c(cur, build_row_source(display$id_pu[i], "RTRW")))
        }
        make_select_html(display$id_pu[i], "RTRW", cur, opts, locked)
      }, character(1))
      
      display$alt_RZWP3K <- vapply(seq_len(nrow(display)), function(i) {
        locked <- !identical(rec_vec[i], "Ubah RZ")
        cur    <- display$alt_RZWP3K[i]
        opts   <- if (locked) cur else {
          unique(c(cur, build_row_source(display$id_pu[i], "RZWP3K")))
        }
        make_select_html(display$id_pu[i], "RZWP3K", cur, opts, locked)
      }, character(1))
      
      display$use_recommendation <- vapply(seq_len(nrow(display)), function(i) {
        cur <- display$use_recommendation[i]
        make_select_html(display$id_pu[i], "use_recommendation", cur,
                         c("Ya", "Tidak"), FALSE)
      }, character(1))
      
      final_order <- c("id_rtrw", "id_rzwp3k", "recommendation",
                       "alt_RTRW", "alt_RZWP3K", "use_recommendation",
                       "RTRW", "RZWP3K", "id_pu", "admin",
                       "length", "area_ha_total",
                       "idx_serasi", "idx_padu_final", "idx_padan")
      final_order <- intersect(final_order, names(display))
      display <- display[, final_order, drop = FALSE]
      
      num2_fmt <- reactable::colFormat(digits = 2)
      
      col_defs <- list(
        id_rtrw            = reactable::colDef(name = "ID RTRW",               minWidth = 90),
        id_rzwp3k          = reactable::colDef(name = "ID RZWP3K",             minWidth = 90),
        recommendation     = reactable::colDef(name = "Rekomendasi",           minWidth = 130),
        alt_RTRW           = reactable::colDef(name = "RTRW Alternatif",       html = TRUE, minWidth = 190),
        alt_RZWP3K         = reactable::colDef(name = "RZWP3K Alternatif",     html = TRUE, minWidth = 190),
        use_recommendation = reactable::colDef(name = "Kunci Opsi?",           html = TRUE, minWidth = 110),
        RTRW               = reactable::colDef(name = "RTRW Aktual",           minWidth = 190),
        RZWP3K             = reactable::colDef(name = "RZWP3K Aktual",         minWidth = 190),
        id_pu              = reactable::colDef(name = "ID PU",                 minWidth = 60),
        admin              = reactable::colDef(name = "Wilayah Administratif", minWidth = 140),
        length             = reactable::colDef(name = "Panjang Segmen (m)",    minWidth = 110, format = num2_fmt),
        area_ha_total      = reactable::colDef(name = "Luas (ha)",             minWidth = 90,  format = num2_fmt),
        idx_serasi         = reactable::colDef(name = "Indeks SERASI Aktual",  minWidth = 120, format = num2_fmt),
        idx_padu_final     = reactable::colDef(name = "Indeks PADU",           minWidth = 100, format = num2_fmt),
        idx_padan          = reactable::colDef(name = "Indeks PADAN Aktual",   minWidth = 120, format = num2_fmt)
      )
      
      reactable::reactable(
        display,
        columns       = col_defs,
        outlined      = TRUE,
        bordered      = TRUE,
        compact       = TRUE,
        striped       = TRUE,
        highlight     = TRUE,
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
      rv$inapp_page <- 1L
      clear_selection()
    })
    
    output$inapp_summary_ui <- renderUI({
      req(rv$inapp_decisions)
      n_total   <- nrow(rv$inapp_decisions)
      n_visible <- length(inapp_filtered_idx())
      n_ya    <- sum(rv$inapp_decisions$use_recommendation == "Ya",    na.rm = TRUE)
      n_tidak <- sum(rv$inapp_decisions$use_recommendation == "Tidak", na.rm = TRUE)
      n_ubah_r <- sum(rv$inapp_decisions$recommendation == "Ubah RTRW" &
                        rv$inapp_decisions$use_recommendation == "Ya", na.rm = TRUE)
      n_ubah_z <- sum(rv$inapp_decisions$recommendation == "Ubah RZ" &
                        rv$inapp_decisions$use_recommendation == "Ya", na.rm = TRUE)
      
      div(class = "alert alert-info mb-2", style = "font-size: 0.8rem; padding: 6px 10px;",
          tags$div(sprintf("Total: %d \u00b7 Ditampilkan: %d", n_total, n_visible)),
          tags$div(sprintf("Ya: %d \u00b7 Tidak: %d", n_ya, n_tidak)),
          tags$div(sprintf("Akan Ubah RTRW: %d \u00b7 Akan Ubah RZWP3K: %d",
                           n_ubah_r, n_ubah_z)))
    })
    
    output$inapp_pagination_ui <- renderUI({
      n <- length(inapp_filtered_idx())
      total_pages <- max(1L, ceiling(n / PAGE_SIZE))
      page <- min(max(1L, rv$inapp_page), total_pages)
      div(
        style = "display:flex; justify-content:space-between; align-items:center; margin-top:4px;",
        actionButton(ns("inapp_prev"), "\u2039 Prev", class = "btn-outline-secondary btn-sm"),
        tags$span(sprintf("Halaman %d / %d (%d baris)", page, total_pages, n),
                  style = "font-size:0.8rem; color:#495057;"),
        actionButton(ns("inapp_next"), "Next \u203a", class = "btn-outline-secondary btn-sm")
      )
    })
    observeEvent(input$inapp_prev, {
      rv$inapp_page <- max(1L, rv$inapp_page - 1L)
    })
    observeEvent(input$inapp_next, {
      total_pages <- max(1L, ceiling(length(inapp_filtered_idx()) / PAGE_SIZE))
      rv$inapp_page <- min(total_pages, rv$inapp_page + 1L)
    })
    
    observeEvent(input$inapp_reset_all, {
      req(rv$inapp_committed)
      rv$inapp_decisions <- rv$inapp_committed
      rv$inapp_page      <- 1L
      bump_table()
      showNotification("Keputusan dikembalikan ke nilai awal.",
                       type = "message", duration = 3)
    })
    
    padan_map_4326 <- reactive({
      m <- rv$idx_padan_map_filter
      req(inherits(m, "sf"))
      if (!"id" %in% names(m)) m$id <- NA_character_
      if (!"area_ha" %in% names(m)) m$area_ha <- NA_real_
      m <- m[, intersect(c("id", "id_pu", "RTRW", "RZWP3K", "area_ha"), names(m))]
      if (is.na(sf::st_crs(m))) {
        sf::st_crs(m) <- 4326
      } else {
        m <- sf::st_transform(m, 4326)
      }
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
      rv$data_nonce
      rv$map_ready
      cur <- rv$inapp_group_selected
      sel <- rv$selected_pu
      dec <- isolate(rv$inapp_decisions)
      
      if (!isTRUE(isolate(rv$workspace_open))) return()
      if (!isTRUE(rv$map_ready)) return()
      if (is.null(dec) || nrow(dec) == 0) return()
      
      proxy <- leaflet::leafletProxy("group_map", session = session) %>%
        leaflet::clearGroup("Konteks grup") %>%
        leaflet::clearGroup("Terpilih")
      
      tryCatch(
        proxy <- proxy %>% leaflet.extras::removeSearchFeatures(),
        error = function(e) NULL)
      
      tryCatch({
        m <- padan_map_4326()
        m$recommendation <- dec$recommendation[match(m$id_pu, dec$id_pu)]
        
        build_polys <- function(x, id_prefix, weight, fill, opacity) {
          if (is.null(x) || nrow(x) == 0) return(NULL)
          is_r <- !is.na(x$RTRW)
          search_lbl <- sprintf(
            "%s | %s | %s | %s | id_pu %s",
            .na_dash(x$id),
            ifelse(is_r, .na_dash(x$RTRW), .na_dash(x$RZWP3K)),
            .na_dash(x$recommendation),
            ifelse(is_r, "RTRW", "RZWP3K"),
            x$id_pu
          )
          list(
            data    = x,
            layerId = paste0(id_prefix, "_", x$id_pu, "_", ifelse(is_r, "R", "Z")),
            color   = ifelse(is_r, "#1565C0", "#2E7D32"),
            weight  = weight, fill = fill, opacity = opacity,
            label   = search_lbl,
            popup   = sprintf(
              "<b>%s</b><br/>ID: %s<br/>Zona: %s<br/>Rekomendasi: %s<br/>id_pu: %s<br/>Luas: %s ha",
              ifelse(is_r, "RTRW", "RZWP3K"), .na_dash(x$id),
              ifelse(is_r, .na_dash(x$RTRW), .na_dash(x$RZWP3K)),
              .na_dash(x$recommendation),
              x$id_pu, ifelse(is.na(x$area_ha), "-", format(round(x$area_ha, 2), big.mark = ",")))
          )
        }
        
        ctx_pu <- if (is.na(cur)) integer(0) else setdiff(dec$id_pu[dec$id_group == cur], sel)
        ctx <- build_polys(m[m$id_pu %in% ctx_pu, ], "ctx", 1, 0.12, 0.6)
        slc <- build_polys(m[m$id_pu %in% sel, ],    "sel", 3, 0.55, 1)
        
        if (!is.null(ctx)) {
          proxy <- proxy %>% leaflet::addPolygons(
            data = ctx$data, layerId = ctx$layerId, color = ctx$color,
            weight = ctx$weight, opacity = ctx$opacity, fillColor = ctx$color,
            fillOpacity = ctx$fill, label = ctx$label, popup = ctx$popup,
            group = "Konteks grup",
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
        message("[recom_adjacent] map proxy error: ", conditionMessage(e))
      })
    })
    
    observeEvent(
      list(rv$inapp_group_selected, rv$fit_nonce, rv$map_ready),
      {
        req(isTRUE(rv$workspace_open), isTRUE(rv$map_ready), rv$inapp_decisions)
        tryCatch({
          m   <- padan_map_4326()
          dec <- rv$inapp_decisions
          cur <- rv$inapp_group_selected
          tgt <- if (is.null(cur) || is.na(cur)) m
          else m[m$id_pu %in% dec$id_pu[dec$id_group == cur], ]
          fit_map_to(tgt)
        }, error = function(e) {
          message("[recom_adjacent] fit-to-group error: ", conditionMessage(e))
        })
      }, ignoreInit = TRUE)
    
    observeEvent(rv$selected_pu, {
      sel <- rv$selected_pu
      req(length(sel) > 0,
          identical(rv$sel_source, "table"),
          isTRUE(rv$workspace_open), isTRUE(rv$map_ready))
      tryCatch({
        m <- padan_map_4326()
        fit_map_to(m[m$id_pu %in% sel, ])
      }, error = function(e) {
        message("[recom_adjacent] fit-to-selection error: ", conditionMessage(e))
      })
    }, ignoreInit = TRUE)
    
    observeEvent(input$group_map_shape_click, {
      id <- input$group_map_shape_click$id
      req(id, grepl("^(ctx|sel)_[0-9]+_[RZ]$", id))
      
      pu   <- as.integer(sub("^(ctx|sel)_([0-9]+)_([RZ])$", "\\2", id))
      side <-                sub("^(ctx|sel)_([0-9]+)_([RZ])$", "\\3", id)
      
      m <- rv$idx_padan_map_filter
      req(inherits(m, "sf"), "id_pu" %in% names(m), "id" %in% names(m))
      
      feature_id <- if (identical(side, "R")) {
        m$id[!is.na(m$RTRW)   & m$id_pu == pu][1]
      } else {
        m$id[!is.na(m$RZWP3K) & m$id_pu == pu][1]
      }
      req(!is.na(feature_id), nzchar(feature_id))
      
      all_pairs <- unique(m$id_pu[m$id == feature_id])
      if (length(all_pairs) == 0) return()
      
      nf <- rv$node_filter
      if (!is.null(nf) &&
          identical(as.character(nf$node), as.character(feature_id))) return()
      
      rv$node_filter <- list(node  = feature_id,
                             label = feature_id,
                             pu    = all_pairs)
      rv$inapp_page  <- 1L
      rv$selected_pu <- integer(0)
      rv$sel_source  <- "map"
    })
    
    observeEvent(input$inapp_row_pick, {
      pu <- suppressWarnings(as.integer(input$inapp_row_pick$id_pu))
      req(!is.na(pu))
      if (identical(as.integer(rv$selected_pu), pu)) return()
      select_pair(pu, "table")
    })

    commit_decisions <- function() {
      if (is.null(rv$inapp_decisions) || is.null(rv$idx_padan_map_filter)) return(FALSE)
      
      df <- rv$inapp_decisions
      
      if (!is.null(rv$matriks_serasi)) {
        valid_rtrw <- unique(as.character(rv$matriks_serasi$class1))
        valid_rz   <- unique(as.character(rv$matriks_serasi$class2))
        
        changed_r <- df$recommendation == "Ubah RTRW"
        changed_z <- df$recommendation == "Ubah RZ"
        bad_r <- unique(df$alt_RTRW[changed_r & !df$alt_RTRW %in% valid_rtrw])
        bad_z <- unique(df$alt_RZWP3K[changed_z & !df$alt_RZWP3K %in% valid_rz])
        bad_r <- bad_r[!is.na(bad_r)]
        bad_z <- bad_z[!is.na(bad_z)]
        
        if (length(bad_r) > 0 || length(bad_z) > 0) {
          showNotification(
            paste0(
              "Nilai alternatif tidak valid: ",
              if (length(bad_r) > 0) paste0("alt_RTRW: ", paste(bad_r, collapse = ", "), ". ") else "",
              if (length(bad_z) > 0) paste0("alt_RZWP3K: ", paste(bad_z, collapse = ", "), ". ") else "",
              "Pastikan nilainya ada pada Matriks SERASI."),
            type = "error", duration = 10)
          return(FALSE)
        }
      }
      
      alt_per_pu <- df %>%
        dplyr::transmute(
          id_pu      = as.integer(id_pu),
          alt_RTRW   = ifelse(recommendation == "Ubah RTRW", alt_RTRW,   RTRW),
          alt_RZWP3K = ifelse(recommendation == "Ubah RZ",   alt_RZWP3K, RZWP3K),
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
    
    observeEvent(input$inapp_save, {
      commit_decisions()
    })
    
    is_decisions_saved <- function() {
      !is.null(rv$idx_padan_map_alt) &&
        !is.null(rv$inapp_saved) &&
        identical(rv$inapp_decisions, rv$inapp_saved)
    }
    
    go_to_step3 <- function() {
      rv$unlocked <- max(rv$unlocked, 3)
      go_to_panel("step3")
    }
    
    
    output$inapp_dl_draft <- downloadHandler(
      filename = function() {
        ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
        sprintf("Draf Alternatif Bertetangga %s.xlsx", ts)
      },
      content = function(file) {
        df <- rv$inapp_decisions
        req(df)
        cols <- c("id_pu", "id_group", "id_rtrw", "id_rzwp3k",
                  "RTRW", "RZWP3K", "admin", "length",
                  "area_ha_rtrw", "area_ha_rzwp3k", "area_ha_total",
                  "idx_serasi", "idx_padu_final", "idx_padan",
                  "recommendation", "alt_RTRW", "alt_RZWP3K", "use_recommendation")
        cols <- intersect(cols, names(df))
        openxlsx::write.xlsx(as.data.frame(df)[, cols, drop = FALSE], file)
      }
    )
    
    observeEvent(input$btn_workspace_to_step3, {
      req(rv$inapp_decisions)
      if (is_decisions_saved()) {
        go_to_step3()
        return()
      }
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

    observe({
      req(input$alt_upload)
      
      if (is.null(rv$idx_padan_map_filter)) {
        rv$alt_status <- list(
          ok  = FALSE,
          msg = "Unggah Peta PADAN yang sudah difilter terlebih dahulu.",
          preview = NULL)
        return()
      }
      
      tryCatch({
        uploaded <- load_and_validate_table(input$alt_upload$datapath)
        
        required_core <- c("id_pu", "recommendation", "alt_RTRW", "alt_RZWP3K",
                           "use_recommendation")
        missing_core <- setdiff(required_core, names(uploaded))
        if (length(missing_core) > 0) {
          .clear_downstream_alt()
          rv$alt_status <- list(
            ok = FALSE,
            msg = paste("Kolom wajib tidak ditemukan:",
                        paste(missing_core, collapse = ", ")),
            preview = NULL)
          return()
        }
        
        bad_use <- unique(uploaded$use_recommendation[
          !is.na(uploaded$use_recommendation) &
            !as.character(uploaded$use_recommendation) %in% c("Ya", "Tidak")])
        if (length(bad_use) > 0) {
          .clear_downstream_alt()
          rv$alt_status <- list(
            ok = FALSE,
            msg = paste0("Nilai 'use_recommendation' tidak valid: ",
                         paste(bad_use, collapse = ", ")),
            preview = NULL)
          return()
        }
        
        is_dissolved <- all(c("id_rtrw", "id_rzwp3k") %in% names(uploaded))
        
        if (is_dissolved) {
          needed <- c("id_pu", "id_group", "id_rtrw", "id_rzwp3k",
                      "RTRW", "RZWP3K", "admin", "length",
                      "area_ha_rtrw", "area_ha_rzwp3k", "area_ha_total",
                      "idx_serasi", "idx_padu_final", "idx_padan",
                      "recommendation", "alt_RTRW", "alt_RZWP3K", "use_recommendation")
          for (col in needed) {
            if (!col %in% names(uploaded)) uploaded[[col]] <- NA
          }
          
          uploaded$id_pu    <- as.integer(uploaded$id_pu)
          uploaded$id_group <- as.integer(uploaded$id_group)
          
          if (all(is.na(uploaded$area_ha_total))) {
            uploaded$area_ha_total <- ifelse(
              is.na(uploaded$area_ha_rtrw) & is.na(uploaded$area_ha_rzwp3k), NA_real_,
              rowSums(cbind(uploaded$area_ha_rtrw, uploaded$area_ha_rzwp3k), na.rm = TRUE)
            )
          }
          
          uploaded <- as.data.frame(uploaded, stringsAsFactors = FALSE)
          rv$inapp_decisions  <- uploaded
          rv$inapp_committed  <- uploaded
          rv$inapp_saved      <- uploaded
          rv$inapp_options_df <- NULL
          rv$inapp_page       <- 1L
          rv$inapp_group_order    <- sort(unique(uploaded$id_group))
          rv$inapp_group_selected <- rv$inapp_group_order[1]
          rv$data_nonce <- isolate(rv$data_nonce) + 1L
          rv$workspace_open <- FALSE
          rv$upload_loaded  <- TRUE
          
          alt_per_pu <- uploaded %>%
            dplyr::transmute(
              id_pu      = as.integer(id_pu),
              alt_RTRW   = ifelse(recommendation == "Ubah RTRW", alt_RTRW,   RTRW),
              alt_RZWP3K = ifelse(recommendation == "Ubah RZ",   alt_RZWP3K, RZWP3K),
              recommendation,
              use_recommendation
            )
          
          base_filtered <- rv$idx_padan_map_filter
          drop_cols <- intersect(
            c("alt_RTRW", "alt_RZWP3K", "recommendation", "use_recommendation"),
            names(base_filtered))
          if (length(drop_cols) > 0)
            base_filtered <- base_filtered[, setdiff(names(base_filtered), drop_cols)]
          
          rv$idx_padan_map_alt <- dplyr::left_join(base_filtered, alt_per_pu, by = "id_pu")
          
          rv$alt_status <- list(
            ok = TRUE,
            msg = sprintf("Templat/Draf dimuat: %d pasang. Klik 'Buka Halaman Kerja' untuk meninjau.",
                          nrow(uploaded)),
            preview = NULL)
          showNotification(
            "Berkas berhasil dimuat. Klik 'Buka Halaman Kerja' untuk melanjutkan.",
            type = "message", duration = 6)
        } else {
          if (!"id" %in% names(uploaded)) {
            .clear_downstream_alt()
            rv$alt_status <- list(
              ok = FALSE,
              msg = "Kolom 'id' tidak ditemukan. Format berkas tidak dikenali.",
              preview = NULL)
            return()
          }
          
          needed <- c("id", "id_pu", "alt_RTRW", "alt_RZWP3K",
                      "recommendation", "use_recommendation")
          missing_needed <- setdiff(needed, names(uploaded))
          if (length(missing_needed) > 0) {
            .clear_downstream_alt()
            rv$alt_status <- list(
              ok = FALSE,
              msg = paste("Kolom tidak ditemukan:",
                          paste(missing_needed, collapse = ", ")),
              preview = NULL)
            return()
          }
          
          alt_table_feature <- uploaded[, needed]
          idx_padan_map_alt <- dplyr::left_join(
            rv$idx_padan_map_filter, alt_table_feature, by = c("id", "id_pu"))
          
          if (!"recommendation" %in% names(idx_padan_map_alt)) {
            .clear_downstream_alt()
            rv$alt_status <- list(
              ok = FALSE,
              msg = "Kolom 'recommendation' hilang setelah penggabungan.",
              preview = NULL)
            return()
          }
          
          bad_locked_rtrw <- idx_padan_map_alt %>%
            dplyr::filter(!is.na(RTRW),
                          recommendation != "Ubah RTRW",
                          as.character(alt_RTRW) != as.character(RTRW))
          bad_locked_rz <- idx_padan_map_alt %>%
            dplyr::filter(!is.na(RZWP3K),
                          recommendation != "Ubah RZ",
                          as.character(alt_RZWP3K) != as.character(RZWP3K))
          
          if (nrow(bad_locked_rtrw) > 0 || nrow(bad_locked_rz) > 0) {
            .clear_downstream_alt()
            rv$alt_status <- list(
              ok = FALSE,
              msg = paste0("Kolom yang terkunci tidak boleh diubah.\n",
                           "- Baris RTRW terkunci diubah: ",  nrow(bad_locked_rtrw), "\n",
                           "- Baris RZWP3K terkunci diubah: ", nrow(bad_locked_rz)),
              preview = NULL)
            return()
          }
          
          new_snapshot <- alt_table_feature
          changed <- is.null(rv$alt_table_snapshot) ||
            !identical(new_snapshot, rv$alt_table_snapshot)
          if (changed) reset_from_step3()
          rv$alt_table_snapshot <- new_snapshot
          rv$idx_padan_map_alt  <- idx_padan_map_alt
          rv$upload_loaded      <- TRUE
          rv$alt_status <- list(ok = TRUE, msg = "Validasi berhasil.", preview = NULL)
        }
      }, error = function(e) {
        .clear_downstream_alt()
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
          "Belum ada keputusan tersimpan. Klik 'Simpan Keputusan' atau unggah templat.",
          type = "warning", duration = 8)
        return()
      }
      rv$unlocked <- max(rv$unlocked, 3)
      go_to_panel("step3")
    })
    
    output$step3_ui <- renderUI({
      tagList(
        checkboxInput(ns("npv_enable"), "Hitung Nilai Ekonomi (NPV)?", value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("npv_enable"), "']"),
          fileInput(ns("npv_lulc_file"), "Tabel Acuan NPV Penutupan Lahan (.xlsx)", accept = ".xlsx"),
          fileInput(ns("land_dist_rtrw_file"), "Matriks Distribusi Lahan RTRW (.xlsx)", accept = ".xlsx"),
          fileInput(ns("land_dist_rzwp3k_file"), "Matriks Distribusi Lahan RZWP3K (.xlsx)", accept = ".xlsx"),
          actionButton(ns("btn_calc_npv"),
                       tagList(tags$i(class = "bi bi-calculator me-1"), "Hitung NPV"),
                       class = "btn-outline-primary btn-sm")
        ),
        uiOutput(ns("npv_status_ui")),
        .step_nav(ns, back_id = "btn_back_3", next_id = "btn_next_3", next_label = "Lanjut ke Step 4")
      )
    })
    
    observeEvent(input$btn_calc_npv, {
      req(rv$idx_padan_map_alt, input$npv_lulc_file,
          input$land_dist_rtrw_file, input$land_dist_rzwp3k_file)
      tryCatch({
        npv_lulc <- load_and_validate_table(input$npv_lulc_file$datapath)
        matriks_land_distribution_rtrw   <- load_validate_matrix_table(input$land_dist_rtrw_file$datapath, title = "distribusi lahan RTRW")
        matriks_land_distribution_rzwp3k <- load_validate_matrix_table(input$land_dist_rzwp3k_file$datapath, title = "distribusi lahan RZWP3K")
        adjacent_economy_map <- calculate_economic_npv(
          alt_map_with_decision = rv$idx_padan_map_alt,
          npv_lulc = npv_lulc,
          matriks_land_distribution_rtrw = matriks_land_distribution_rtrw,
          matriks_land_distribution_rzwp3k = matriks_land_distribution_rzwp3k)
        
        if (!"recommendation" %in% names(adjacent_economy_map)) {
          stop("Kolom 'recommendation' hilang setelah perhitungan NPV.")
        }
        rv$adjacent_economy_map <- adjacent_economy_map
        rv$npv_status <- list(ok = TRUE, msg = "Perhitungan NPV berhasil.")
        rv$final_result <- NULL
        rv$final_log <- NULL
      }, error = function(e) {
        rv$adjacent_economy_map <- NULL
        rv$npv_status <- list(ok = FALSE, msg = paste("Gagal menghitung NPV:", e$message))
      })
    })
    
    output$npv_status_ui <- renderUI({
      req(rv$npv_status)
      cls <- if (rv$npv_status$ok) "alert alert-success mb-0" else "alert alert-danger mb-0"
      icn <- if (rv$npv_status$ok) "bi bi-check-circle me-2" else "bi bi-exclamation-triangle-fill me-2"
      div(class = cls, tags$i(class = icn), rv$npv_status$msg)
    })
    
    observeEvent(input$btn_back_3, go_to_panel("step2"))
    
    observeEvent(input$btn_next_3, {
      if (isTRUE(input$npv_enable)) {
        if (is.null(rv$adjacent_economy_map)) {
          showNotification("Klik 'Hitung NPV' terlebih dahulu.",
                           type = "warning", duration = 6)
          return()
        }
      } else {
        base_map <- rv$idx_padan_map_alt
        if (is.null(base_map)) {
          rv$adjacent_economy_map <- NULL
          showNotification("Peta alternatif belum tersedia.",
                           type = "warning", duration = 6)
          return()
        }
        if (!"recommendation" %in% names(base_map)) {
          rv$adjacent_economy_map <- NULL
          showNotification("Harap siapkan opsi ulang pada Langkah 2.",
                           type = "warning", duration = 8)
          return()
        }
        rv$adjacent_economy_map <- base_map %>%
          dplyr::mutate(econ_rtrw_delta = NA_real_, econ_rzwp3k_delta = NA_real_)
      }
      rv$unlocked <- max(rv$unlocked, 4)
      go_to_panel("step4")
    })

    output$step4_ui <- renderUI({
      tagList(
        tags$p(style = "color:#6c757d; font-size:0.85rem; margin-bottom: 8px;",
               "Ambang batas berikut hanya dipakai untuk mengklasifikasikan ",
               "tingkat integrasi (aktual vs. hasil alternatif)."),
        bslib::layout_column_wrap(
          width = 1/3,
          numericInput(ns("th_high"), "Ambang Tinggi",
                       value = 0.8, min = 0, max = 1, step = 0.05),
          numericInput(ns("th_med"),  "Ambang Sedang",
                       value = 0.5, min = 0, max = 1, step = 0.05),
          numericInput(ns("th_low"),  "Ambang Rendah",
                       value = 0.25, min = 0, max = 1, step = 0.05)
        ),
        hr(),
        if (is.null(output_dir()) || !nzchar(output_dir())) {
          div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
              tags$i(class = "bi bi-exclamation-triangle me-1"),
              "Direktori output belum diatur.")
        },
        div(style = "display: flex; gap: 8px; flex-wrap: wrap;",
            actionButton(ns("btn_run_final"),
                         tagList(tags$i(class = "bi bi-play-fill me-1"),
                                 "Jalankan Analisis Alternatif"),
                         class = "btn-success btn-sm")),
        .step_nav(ns, back_id = "btn_back_4", next_id = NULL)
      )
    })
    
    observeEvent(input$btn_run_final, {
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification("Direktori output belum diatur.",
                         type = "error", duration = 5)
        return()
      }
      req(rv$adjacent_economy_map)
      
      rv$final_result <- NULL
      log_lines <- character(0)
      
      withProgress(message = "Menyusun alternatif kasus bertetangga", value = 0, {
        tryCatch({
          th_high <- input$th_high
          th_med  <- input$th_med
          th_low  <- input$th_low
          alpha   <- rv$alpha
          npv_enabled    <- isTRUE(input$npv_enable)
          matriks_serasi <- rv$matriks_serasi
          adjacent_economy_map <- rv$adjacent_economy_map
          
          if (!"recommendation" %in% names(adjacent_economy_map))
            stop("Kolom 'recommendation' tidak ditemukan pada peta alternatif.\n")
          if (!"use_recommendation" %in% names(adjacent_economy_map))
            stop("Kolom 'use_recommendation' tidak ditemukan pada peta alternatif.\n")
          
          get_compat <- function(x, y) {
            if (is.na(x) || is.na(y)) return(NA_real_)
            val <- matriks_serasi %>%
              dplyr::filter(class1 == x, class2 == y) %>%
              dplyr::pull(idx_serasi)
            if (length(val) == 0) NA_real_ else val
          }
          
          incProgress(0.4, detail = "Memisahkan layer...")
          rtrw_rows <- adjacent_economy_map %>%
            dplyr::filter(!is.na(RTRW)) %>%
            dplyr::select(id_pu, id_group, RTRW, alt_RTRW,
                          recommendation, use_recommendation,
                          idx_padu_final, econ_rtrw_delta) %>%
            sf::st_drop_geometry() %>% dplyr::as_tibble()
          rz_rows <- adjacent_economy_map %>%
            dplyr::filter(!is.na(RZWP3K)) %>%
            dplyr::select(id_pu, RZWP3K, alt_RZWP3K, econ_rzwp3k_delta) %>%
            sf::st_drop_geometry() %>% dplyr::as_tibble()
          common_cols <- adjacent_economy_map %>%
            dplyr::select(id_pu, idx_padan, area_buffer_ha) %>%
            sf::st_drop_geometry() %>%
            dplyr::distinct(id_pu, .keep_all = TRUE)
          
          split_rtrw_rzwp3k <- rtrw_rows %>%
            dplyr::full_join(rz_rows, by = "id_pu") %>%
            dplyr::left_join(common_cols, by = "id_pu")
          
          stopifnot(all(!is.na(split_rtrw_rzwp3k$RTRW) &
                          !is.na(split_rtrw_rzwp3k$RZWP3K)))
          
          incProgress(0.6, detail = "Menyusun keputusan")
          recommendation_decision <- split_rtrw_rzwp3k %>%
            dplyr::mutate(
              RTRW_new = dplyr::case_when(
                recommendation == "Ubah RTRW" ~ as.character(alt_RTRW),
                TRUE                          ~ as.character(RTRW)),
              RZWP3K_new = dplyr::case_when(
                recommendation == "Ubah RZ"   ~ as.character(alt_RZWP3K),
                TRUE                          ~ as.character(RZWP3K)),
              idx_serasi_new  = purrr::map2_dbl(RTRW_new, RZWP3K_new, get_compat),
              idx_padan_new   = alpha * idx_serasi_new + (1 - alpha) * idx_padu_final,
              actual_integration = .classify_integrasi(idx_padan,     th_high, th_med, th_low),
              recom_integration  = .classify_integrasi(idx_padan_new, th_high, th_med, th_low),
              econ_delta = dplyr::case_when(
                !npv_enabled                  ~ NA_real_,
                recommendation == "Ubah RTRW" ~ econ_rtrw_delta,
                recommendation == "Ubah RZ"   ~ econ_rzwp3k_delta,
                TRUE                          ~ 0),
              area_change_ha  = dplyr::if_else(recommendation == "Tetap/Koordinasi",
                                               0, area_buffer_ha),
              idx_padan_delta = idx_padan_new - idx_padan
            )
          
          recommendation_decision_filter <- recommendation_decision %>%
            dplyr::select(id_pu, RTRW_new, RZWP3K_new,
                          idx_serasi_new, idx_padan_new, actual_integration,
                          recom_integration, econ_delta, area_change_ha,
                          idx_padan_delta) %>%
            sf::st_drop_geometry()
          
          adjacent_recom_map <- adjacent_economy_map %>%
            dplyr::left_join(recommendation_decision_filter, by = "id_pu")
          
          incProgress(0.8, detail = "Menyimpan hasil...")
          recom_adjacent_dir <- file.path(output_dir(), "Penyusunan Alternatif")
          if (!dir.exists(recom_adjacent_dir))
            dir.create(recom_adjacent_dir, recursive = TRUE, showWarnings = FALSE)
          out_gpkg <- file.path(recom_adjacent_dir, "idx_alternatives_adjacent.gpkg")
          out_xlsx <- file.path(recom_adjacent_dir, "idx_alternatives_adjacent.xlsx")
          sf::st_write(adjacent_recom_map, out_gpkg, delete_dsn = TRUE, quiet = TRUE)
          openxlsx::write.xlsx(sf::st_drop_geometry(adjacent_recom_map), out_xlsx)
          
          log_lines <- c(
            log_lines,
            "Ringkasan alternatif:",
            capture.output(print(table(adjacent_recom_map$recommendation))),
            "",
            "Ringkasan integrasi (aktual vs alternatif):",
            capture.output(print(
              adjacent_recom_map %>%
                sf::st_drop_geometry() %>%
                dplyr::count(actual_integration, recom_integration) %>%
                dplyr::arrange(actual_integration, recom_integration))))
          
          adjacent_recom_map_viz <- tryCatch({
            viz <- dissolve_id_pu(adjacent_recom_map)
            if (any(!sf::st_is_valid(viz))) viz <- sf::st_make_valid(viz)
            viz
          }, error = function(e) {
            warning("dissolve_id_pu failed: ", conditionMessage(e))
            sf::st_make_valid(adjacent_recom_map)
          })
          
          rv$final_result <- list(map = adjacent_recom_map_viz,
                                  table = sf::st_drop_geometry(adjacent_recom_map_viz),
                                  gpkg_path = out_gpkg, xlsx_path = out_xlsx)
          rv$final_log <- paste(log_lines, collapse = "\n")
          
          out <- list(
            inputs = list(
              start_time           = Sys.time(),
              case                 = "adjacent",
              idx_padan_file       = input$idx_padan_file$name,
              idx_padan_source     = rv$idx_padan_source,
              alpha                = rv$alpha,
              priority_rtrw        = rv$priority_rtrw,
              priority_rzwp3k      = rv$priority_rzwp3k,
              th_high              = input$th_high,
              th_med               = input$th_med,
              th_low               = input$th_low,
              npv_enabled          = isTRUE(input$npv_enable),
              decision_mode        = input$decision_mode,
              output_dir           = output_dir()
            ),
            result = list(
              idx_alternative_adjacent_map   = adjacent_recom_map,
              idx_alternative_adjacent_table = sf::st_drop_geometry(adjacent_recom_map)
            )
          )
          
          log_dir <- file.path(recom_adjacent_dir, "log")
          if (!dir.exists(log_dir))
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
          tryCatch({
            inputs <- out$inputs
            save(inputs, file = file.path(log_dir, "idx_alternatives_adjacent.rda"))
          }, error = function(e) warning("Gagal menulis file log: ", e$message))
          
          session$userData$module_results$recommendation <- out
          
          plot_categorical_map(
            map = adjacent_recom_map,
            title = "Peta Opsi Alternatif Kasus Bertetangga",
            column = "recommendation", legend = "Opsi Alternatif",
            filepath = file.path(log_dir, "peta_opsi_alternatif_bertetangga.png"))
          
          showNotification("Berhasil! Analisis alternatif telah disimpan.",
                           type = "message")
          incProgress(1.0, detail = "Selesai!")
        }, error = function(e) {
          call_txt <- if (!is.null(conditionCall(e)))
            paste0("\n(pada pemanggilan: ",
                   paste(deparse(conditionCall(e)), collapse = " "), ")") else ""
          rv$final_log <- paste0("Error: ", conditionMessage(e), call_txt)
          showNotification(paste("Gagal:", conditionMessage(e)),
                           type = "error", duration = NULL)
        })
      })
    })
    
    observeEvent(input$btn_back_4, go_to_panel("step3"))
    
    output$status_box <- renderUI({
      if (!is.null(rv$final_result)) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Selesai. Silakan lanjut ke ", tags$strong("Langkah Rekonsiliasi"), ".")
      } else if (!is.null(rv$final_log) && grepl("^Error", rv$final_log)) {
        div(class = "alert alert-danger mb-0",
            tags$i(class = "bi bi-exclamation-triangle-fill me-2"),
            "Terjadi kesalahan, lihat Log Validasi.")
      } else if (rv$unlocked >= 4) {
        div(class = "alert alert-secondary mb-0", "Siap dijalankan.")
      } else {
        div(class = "alert alert-secondary mb-0", "Lengkapi langkah sebelumnya.")
      }
    })
    
    observe({
      if (!is.null(rv$final_result)) {
        rv$analysis_result <- list(map = rv$final_result$map, table = rv$final_result$table)
        rv$gpkg_path   <- rv$final_result$gpkg_path
        rv$xlsx_path   <- rv$final_result$xlsx_path
        rv$log_messages <- if (!is.null(rv$final_log)) rv$final_log else ""
      } else {
        rv$analysis_result <- NULL
        rv$gpkg_path       <- NULL
        rv$xlsx_path       <- NULL
        rv$log_messages    <- if (!is.null(rv$final_log)) rv$final_log else "Siap untuk penyusunan alternatif."
      }
    })
    
    recom_adjacent_config <- list(
      map_color_col  = "recommendation",
      map_title      = "Alternatif",
      map_palette    = c("blue", "orange", "red", "purple", "green"),
      map_label_cols = list(
        "ID PU" = "id_pu", "ID Grup" = "id_group",
        "RTRW" = "RTRW", "RZWP3K" = "RZWP3K",
        "Alternatif" = "recommendation",
        "RTRW Baru" = "RTRW_new", "RZWP3K Baru" = "RZWP3K_new"),
      table_cols = c(
        "id_pu"              = "ID PU",
        "id_group"           = "ID Grup",
        "RTRW"               = "RTRW Awal",
        "RZWP3K"             = "RZWP3K Awal",
        "area_ha"            = "Luas (ha)",
        "length"             = "Panjang Segmen (m)",
        "idx_padan"          = "PADAN Awal",
        "recommendation"     = "Alternatif",
        "use_recommendation" = "Gunakan Alternatif?",
        "alt_RTRW"           = "RTRW Alternatif",
        "alt_RZWP3K"         = "RZWP3K Alternatif",
        "RTRW_new"           = "RTRW Baru",
        "RZWP3K_new"         = "RZWP3K Baru",
        "idx_padan_new"      = "PADAN Baru",
        "idx_padan_delta"    = "Δ PADAN",
        "actual_integration" = "Integrasi Aktual",
        "recom_integration"  = "Integrasi Alternatif",
        "econ_rtrw_delta"    = "Δ Ekonomi RTRW",
        "econ_rzwp3k_delta"  = "Δ Ekonomi RZWP3K",
        "econ_delta"         = "Δ Ekonomi Akhir"
      ),
      table_round_cols = c(
        "Luas (ha)", "Panjang Segmen (m)",
        "PADAN Awal", "PADAN Baru", "Δ PADAN"
      ),
      table_optional_cols = c(
        "econ_rtrw_delta", "econ_rzwp3k_delta", "econ_delta"
      )
    )
    
    render_result_server(input, output, session, rv, recom_adjacent_config)
  })
}