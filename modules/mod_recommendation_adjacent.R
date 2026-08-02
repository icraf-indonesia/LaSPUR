# ui/modules/mod_recommendation_adjacent.R
# ============================================================
#  MODULE: Recommendation (4. Analisis Rekomendasi)
#  Wizard flow (accordion in left panel)
#  Outputs (map, table accordion, log) in right panel.
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── small UI helpers ────────────────────────────────────────
.locked_panel <- function(msg = "Selesaikan langkah sebelumnya terlebih dahulu.") {
  div(
    class = "alert alert-secondary mb-0",
    tags$i(class = "bi bi-lock-fill me-2"), msg
  )
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

# ── validation helpers  ──────────────────────────
.validate_alt_table <- function(alt_table, matriks_serasi) {
  required_cols <- c("id", "id_pu", "alt_RTRW", "alt_RZWP3K")
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

.classify_integrasi <- function(score, th_high, th_med, th_low) {
  case_when(
    score >= th_high ~ "Sangat Terintegrasi",
    score >= th_med  ~ "Terintegrasi",
    score >= th_low  ~ "Kurang Terintegrasi",
    TRUE             ~ "Tidak Terintegrasi"
  )
}

# ── UI (two‑column layout) ──────────────────────────────────
recommendation_adjacent_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("4. Analisis Rekomendasi Area Bertetangga", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menentukan opsi penyelesaian konflik dalam integrasi tata ruang darat-laut.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),
    
    fluidRow(
      class = "g-3",
      
      # ── Left column: Input & Parameter (1/3) ──────────────
      column(
        width = 4,
        card(
          card_header("Input & Parameter"),
          accordion(
            id = ns("wizard"),
            open = "step1",
            multiple = FALSE,
            
            accordion_panel(
              title = "Langkah 1 — Menyaring Kasus",
              value = "step1",
              icon = tags$i(class = "bi bi-funnel-fill"),
              uiOutput(ns("step1_ui"))
            ),
            
            accordion_panel(
              title = "Langkah 2 — Menentukan Kawasan Alternatif",
              value = "step2",
              icon = tags$i(class = "bi bi-signpost-split-fill"),
              uiOutput(ns("step2_ui"))
            ),
            
            accordion_panel(
              title = "Langkah 3 — Menghitung Nilai Ekonomi (Opsional)",
              value = "step3",
              icon = tags$i(class = "bi bi-cash-coin"),
              uiOutput(ns("step3_ui"))
            ),
            
            accordion_panel(
              title = "Langkah 4 — Menentukan Rekomendasi",
              value = "step4",
              icon = tags$i(class = "bi bi-check2-circle"),
              uiOutput(ns("step4_ui"))
            )
          )
        )
      ),
      
      # ── Right column: Output & Hasil (2/3) ────────────────
      column(
        width = 8,
        card(
          card_header("Output & Hasil"),
          
          uiOutput(ns("status_box")),
          
          hr(),
          
          navset_tab(
            nav_panel(
              "Visualisasi Hasil",
              leafletOutput(ns("recommendation_map"), height = "450px"),
              hr(style = "margin: 15px 0; border-top: 1px solid #dee2e6;"),
              div(
                style = "max-height: 500px; overflow: auto;",
                uiOutput(ns("table_accordion"))
              )
            ),
            nav_panel(
              "Log",
              div(
                style = "max-height: 300px; overflow-y: auto; background-color: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 0.9rem; white-space: pre-wrap;",
                verbatimTextOutput(ns("validation_log"))
              )
            )
          ),
          
          div(
            style = "display: flex; gap: 8px; flex-wrap: wrap; margin-top: 12px;",
            downloadButton(ns("dl_gpkg"), "Unduh GPKG", class = "btn-outline-secondary btn-sm"),
            downloadButton(ns("dl_xlsx"), "Unduh XLSX", class = "btn-outline-secondary btn-sm")
          )
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────
recommendation_adjacent_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      unlocked = 1,
      
      # step 1
      idx_padan_map        = NULL,
      filter_snapshot       = NULL,
      idx_padan_map_filter  = NULL,
      count_before          = NULL,
      count_after           = NULL,
      
      # step 2
      matriks_serasi        = NULL,
      alt_template_path     = NULL,
      alt_table_snapshot    = NULL,
      idx_padan_map_alt     = NULL,
      alt_status            = NULL,
      
      # step 3 
      adjacent_economy_map  = NULL,
      npv_status            = NULL,
      
      # step 4
      final_result          = NULL,
      final_log             = NULL
    )
    
    go_to_panel <- function(value) accordion_panel_set(id = "wizard", values = value, session = session)
    
    reset_from_step2 <- function() {
      rv$idx_padan_map_alt  <- NULL
      rv$alt_table_snapshot <- NULL
      rv$alt_status         <- NULL
      rv$adjacent_economy_map <- NULL
      rv$npv_status         <- NULL
      rv$final_result       <- NULL
      rv$final_log          <- NULL
      rv$unlocked           <- min(rv$unlocked, 2)
    }
    
    reset_from_step3 <- function() {
      rv$adjacent_economy_map <- NULL
      rv$npv_status   <- NULL
      rv$final_result <- NULL
      rv$final_log    <- NULL
      rv$unlocked     <- min(rv$unlocked, 3)
    }
    
    # ── Step 1 UI ─────────────────────────────────────────────
    output$step1_ui <- renderUI({
      file_input <- fileInput(ns("idx_padan_file"), "Pilih Peta Hasil Analisis PADAN (.gpkg)", accept = ".gpkg")

      if (is.null(rv$idx_padan_map)) {
        return(tagList(
          file_input,
          tags$p(style = "color: #6c757d; margin-top: 8px;", "Unggah peta PADAN untuk mengaktifkan filter."),
          .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Step 2")
        ))
      }

      tagList(
        file_input,
        layout_column_wrap(
          width = 1/2,
          div(
            checkboxInput(ns("apply_length"), "Aktifkan Filter Panjang Segmen", value = TRUE),
            conditionalPanel(
              condition = paste0("input['", ns("apply_length"), "']"),
              numericInput(ns("length_filter"), "Ambang Panjang (meter)", value = 1500, min = 0, step = 50)
            )
          ),
          div(
            checkboxInput(ns("apply_idx"), "Aktifkan Filter Indeks PADAN", value = TRUE),
            conditionalPanel(
              condition = paste0("input['", ns("apply_idx"), "']"),
              numericInput(ns("idx_filter"), "Ambang Indeks PADAN (<)", value = 0.75, min = 0, max = 1, step = 0.05)
            )
          )
        ),
        uiOutput(ns("filter_count_ui")),
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Step 2")
      )
    })
    
    # ── Step 1: load map ────────────────────────────────────
    observeEvent(input$idx_padan_file, {
      req(input$idx_padan_file)
      showNotification("Memuat peta PADAN...", type = "message", duration = 2)
      tryCatch({
        rv$idx_padan_map <- sf::st_read(input$idx_padan_file$datapath, quiet = TRUE)
        rv$idx_padan_map_filter <- NULL
        rv$filter_snapshot <- NULL
        rv$count_before <- NULL
        rv$count_after <- NULL
        reset_from_step2()
        rv$unlocked <- 1
        showNotification("Peta PADAN berhasil dimuat.", type = "message", duration = 5)
      }, error = function(e) {
        rv$idx_padan_map <- NULL
        showNotification(paste("Gagal membaca file:", e$message), type = "error", duration = 10)
      })
    })
    
    # ── Reactive filter  ──────────────────────────
    filtered_data <- reactive({
      req(rv$idx_padan_map)
      
      map <- rv$idx_padan_map
      keep <- rep(TRUE, nrow(map))
      if (isTRUE(input$apply_length)) keep <- keep & (as.numeric(map$length) > input$length_filter)
      if (isTRUE(input$apply_idx))    keep <- keep & (map$idx_padan < input$idx_filter)
      
      list(
        filtered = map[keep, ],
        before = nrow(map) / 2,
        after  = nrow(map[keep, ]) / 2
      )
    })
    
    # Update rv when filter changes
    observeEvent(filtered_data(), {
      res <- filtered_data()
      rv$idx_padan_map_filter <- res$filtered
      rv$count_before <- res$before
      rv$count_after  <- res$after

      if (!is.null(rv$filter_snapshot)) {
        current <- list(
          apply_length = input$apply_length,
          length_filter = input$length_filter,
          apply_idx = input$apply_idx,
          idx_filter = input$idx_filter
        )
        if (!identical(current, rv$filter_snapshot)) {
          reset_from_step2()
        }
      }
    })
    
    output$filter_count_ui <- renderUI({
      req(!is.null(rv$count_before))
      removed <- rv$count_before - rv$count_after
      pct <- if (rv$count_before > 0) (1 - rv$count_after / rv$count_before) * 100 else 0
      div(
        class = "alert alert-info", style = "margin-top: 12px;",
        tags$div(sprintf("Sebelum filter: %d pasang", rv$count_before)),
        tags$div(sprintf("Setelah filter: %d pasang", rv$count_after)),
        tags$div(sprintf("Terhapus: %d pasang (%.1f%%)", removed, pct))
      )
    })
    
    # ── Step 1 -> Step 2 ────────────────────────────────────
    observeEvent(input$btn_next_1, {
      if (is.null(rv$idx_padan_map_filter)) {
        showNotification("Terapkan filter terlebih dahulu.", type = "warning")
        return()
      }
      rv$filter_snapshot <- list(
        apply_length = input$apply_length,
        length_filter = input$length_filter,
        apply_idx = input$apply_idx,
        idx_filter = input$idx_filter
      )
      rv$unlocked <- max(rv$unlocked, 2)
      go_to_panel("step2")
    })
    
    # ── Step 2 UI ────────────────────────────────────────────
    output$step2_ui <- renderUI({
      if (rv$unlocked < 2) return(.locked_panel())
      tagList(
        fileInput(ns("matrix_file"), "Pilih Matriks Serasi (.xlsx)", accept = ".xlsx"),
        numericInput(ns("n_alt"), "Jumlah Opsi Alternatif per Kasus", value = 5, min = 1, max = 10, step = 1),
        
        # Check output directory
        if (is.null(output_dir()) || !nzchar(output_dir())) {
          div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
              tags$i(class = "bi bi-exclamation-triangle me-1"),
              "Direktori output belum diatur. Atur terlebih dahulu di menu utama.")
        },
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_make_template"),
                       tagList(tags$i(class = "bi bi-file-earmark-spreadsheet me-1"), "Buat Template"),
                       class = "btn-outline-primary btn-sm"),
          downloadButton(ns("dl_template"), "Unduh Template", class = "btn-outline-success btn-sm")
        ),
        uiOutput(ns("template_status_ui")),
        hr(),
        fileInput(ns("alt_upload"), "Unggah Template yang Sudah Diisi (.xlsx)", accept = ".xlsx"),
        uiOutput(ns("alt_validation_ui")),
        .step_nav(ns, back_id = "btn_back_2", next_id = "btn_next_2", next_label = "Lanjut ke Step 3")
      )
    })
    
    # Step 2: load matrix on upload 
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

    # ── Step 2: make template ───────────────────────────────
    observeEvent(input$btn_make_template, {
      
      # Check output directory 
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification(
          "Direktori output belum diatur. Harap atur direktori output terlebih dahulu.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      req(rv$idx_padan_map_filter, input$matrix_file)
      withProgress(message = "Membuat Template Opsi Alternatif", value = 0, {
        tryCatch({
          incProgress(0.2, detail = "Memuat matriks serasi...")
          rv$matriks_serasi <- load_validate_matrix_table(input$matrix_file$datapath, title = "serasi")
          
          out_dir_step2 <- file.path(output_dir(), "step2")
          dir.create(out_dir_step2, recursive = TRUE, showWarnings = FALSE)
          
          incProgress(0.5, detail = "Memproses opsi alternatif...")
          determine_alternative_zones(
            idx_padan_map_filter = rv$idx_padan_map_filter,
            serasi_matrix = rv$matriks_serasi,
            n_alt = input$n_alt,
            step = "step2",
            output_dir = out_dir_step2
          )
          
          incProgress(0.8, detail = "Menyimpan file template...")
          generated <- list.files(out_dir_step2, pattern = "\\.xlsx$", full.names = TRUE)
          if (length(generated) == 0) {
            stop("Template dibuat tetapi file .xlsx tidak ditemukan di folder output.")
          }
          rv$alt_template_path <- generated[order(file.info(generated)$mtime, decreasing = TRUE)][1]
          
          showNotification("Template alternatif zona berhasil dibuat.", type = "message")
          incProgress(1.0, detail = "Selesai!")
        }, error = function(e) {
          rv$alt_template_path <- NULL
          showNotification(paste("Gagal membuat template:", e$message), type = "error", duration = 10)
        })
      })
    })
    
    output$template_status_ui <- renderUI({
      req(rv$alt_template_path)
      div(class = "alert alert-success mb-0",
          tags$i(class = "bi bi-check-circle me-2"),
          sprintf("Template siap: %s", basename(rv$alt_template_path)))
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

    observe({
      req(input$alt_upload)
      
      if (is.null(rv$matriks_serasi) || is.null(rv$idx_padan_map_filter)) {
        missing <- c(
          if (is.null(rv$matriks_serasi))       "Matriks Serasi (.xlsx)",
          if (is.null(rv$idx_padan_map_filter)) "Peta PADAN yang sudah difilter"
        )
        rv$alt_status <- list(
          ok  = FALSE,
          msg = paste0(
            "File template telah diunggah. Menunggu input berikut sebelum dapat divalidasi:\n- ",
            paste(missing, collapse = "\n- ")
          ),
          preview = NULL
        )
        return()
      }
      
      tryCatch({
        alt_table <- load_and_validate_table(input$alt_upload$datapath)
        check <- .validate_alt_table(alt_table, rv$matriks_serasi)
        
        if (!check$ok) {
          rv$idx_padan_map_alt <- NULL
          rv$alt_status <- list(ok = FALSE, msg = check$msg, preview = NULL)
          return()
        }
        
        idx_padan_map_alt <- dplyr::left_join(
          rv$idx_padan_map_filter,
          alt_table[, c("id", "id_pu", "alt_RTRW", "alt_RZWP3K")],
          by = c("id", "id_pu")
        )
        
        new_snapshot <- alt_table[, c("id", "id_pu", "alt_RTRW", "alt_RZWP3K")]
        changed <- is.null(rv$alt_table_snapshot) || !identical(new_snapshot, rv$alt_table_snapshot)
        if (changed) reset_from_step3()
        rv$alt_table_snapshot <- new_snapshot
        
        rv$idx_padan_map_alt <- idx_padan_map_alt
        rv$alt_status <- list(
          ok = TRUE, msg = check$msg,
          preview = head(sf::st_drop_geometry(idx_padan_map_alt)[, intersect(
            c("id", "id_pu", "RTRW", "RZWP3K", "alt_RTRW", "alt_RZWP3K"),
            names(idx_padan_map_alt)
          )], 10)
        )
      }, error = function(e) {
        rv$idx_padan_map_alt <- NULL
        rv$alt_status <- list(ok = FALSE, msg = paste("Gagal membaca file:", e$message), preview = NULL)
      })
    })
    
    output$alt_validation_ui <- renderUI({
      req(rv$alt_status)
      if (!rv$alt_status$ok) {
        div(class = "alert alert-danger", style = "white-space: pre-wrap;",
            tags$i(class = "bi bi-exclamation-triangle-fill me-2"), rv$alt_status$msg,
            tags$div(style = "margin-top: 6px; font-weight: 600;", "Silakan unggah ulang file yang sudah diperbaiki."))
      } else {
        div(class = "alert alert-success mb-2",
            tags$i(class = "bi bi-check-circle me-2"), rv$alt_status$msg)
      }
    })
    
    observeEvent(input$btn_back_2, go_to_panel("step1"))
    
    observeEvent(input$btn_next_2, {
      if (is.null(rv$idx_padan_map_alt)) {
        showNotification("Unggah dan validasi template alternatif terlebih dahulu.", type = "warning")
        return()
      }
      rv$unlocked <- max(rv$unlocked, 3)
      go_to_panel("step3")
    })
    
    # ── Step 3 UI ────────────────────────────────────────────
    output$step3_ui <- renderUI({
      if (rv$unlocked < 3) return(.locked_panel())
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
      req(rv$idx_padan_map_alt, input$npv_lulc_file, input$land_dist_rtrw_file, input$land_dist_rzwp3k_file)
      tryCatch({
        npv_lulc <- load_and_validate_table(input$npv_lulc_file$datapath)
        matriks_land_distribution_rtrw   <- load_validate_matrix_table(input$land_dist_rtrw_file$datapath, title = "distribusi lahan RTRW")
        matriks_land_distribution_rzwp3k <- load_validate_matrix_table(input$land_dist_rzwp3k_file$datapath, title = "distribusi lahan RZWP3K")
        
        adjacent_economy_map <- calculate_economic_npv(
          alt_map_with_decision = rv$idx_padan_map_alt,
          npv_lulc = npv_lulc,
          matriks_land_distribution_rtrw = matriks_land_distribution_rtrw,
          matriks_land_distribution_rzwp3k = matriks_land_distribution_rzwp3k
        )
        
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
      req(rv$idx_padan_map_alt)
      
      if (isTRUE(input$npv_enable)) {
        if (is.null(rv$adjacent_economy_map)) {
          showNotification("Hitung NPV terlebih dahulu, atau nonaktifkan opsi Nilai Ekonomi.", type = "warning")
          return()
        }
      } else {
        base_map <- rv$idx_padan_map_alt
        has_econ_rtrw <- "econ_rtrw_delta" %in% names(base_map)
        has_econ_rz   <- "econ_rzwp3k_delta" %in% names(base_map)
        rv$adjacent_economy_map <- base_map %>%
          dplyr::mutate(
            econ_rtrw_delta   = if (has_econ_rtrw) econ_rtrw_delta   else 0,
            econ_rzwp3k_delta = if (has_econ_rz)   econ_rzwp3k_delta else 0
          )
      }
      
      rv$unlocked <- max(rv$unlocked, 4)
      go_to_panel("step4")
    })
    
    # ── Step 4 UI ────────────────────────────────────────────
    output$step4_ui <- renderUI({
      if (rv$unlocked < 4) return(.locked_panel())
      tagList(
        layout_column_wrap(
          width = 1/2,
          fileInput(ns("rtrw_priority_file"), "Tabel Acuan Pola RTRW (.xlsx)", accept = ".xlsx"),
          fileInput(ns("rzwp3k_priority_file"), "Tabel Acuan Pola RZWP3K (.xlsx)", accept = ".xlsx")
        ),
        hr(),
        layout_column_wrap(
          width = 1/3,
          numericInput(ns("th_high"), "Ambang Tinggi", value = 0.8, min = 0, max = 1, step = 0.05),
          numericInput(ns("th_med"), "Ambang Sedang", value = 0.5, min = 0, max = 1, step = 0.05),
          numericInput(ns("th_low"), "Ambang Rendah", value = 0.25, min = 0, max = 1, step = 0.05)
        ),
        sliderInput(ns("alpha_val"), "Proporsi Alpha (\u03B1)", min = 0, max = 1, value = 0.5, step = 0.1),
        hr(),
        
        # Check output directory
        if (is.null(output_dir()) || !nzchar(output_dir())) {
          div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
              tags$i(class = "bi bi-exclamation-triangle me-1"),
              "Direktori output belum diatur. Atur terlebih dahulu di menu utama.")
        },
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_run_final"),
                       tagList(tags$i(class = "bi bi-lightning-charge-fill me-1"), "Buat Rekomendasi"),
                       class = "btn-success btn-sm")
        ),
        .step_nav(ns, back_id = "btn_back_4", next_id = NULL)
      )
    })
    
    # ── Step 4: Final calculation ────────────────────────────
    observeEvent(input$btn_run_final, {
      
      # Check output directory 
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification(
          "Direktori output belum diatur. Harap atur direktori output terlebih dahulu.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      req(rv$adjacent_economy_map, input$rtrw_priority_file, input$rzwp3k_priority_file)
      
      rv$final_result <- NULL
      log_lines <- character(0)
      
      withProgress(message = "Membuat Rekomendasi Bertetangga", value = 0, {
        tryCatch({
          incProgress(0.2, detail = "Memuat tabel acuan pola...")
          rtrw_prioritas   <- load_and_validate_table(input$rtrw_priority_file$datapath)
          rzwp3k_prioritas <- load_and_validate_table(input$rzwp3k_priority_file$datapath)
          
          chk_rtrw <- .validate_priority_table(rtrw_prioritas, "RTRW")
          if (!chk_rtrw$ok) stop(chk_rtrw$msg)
          chk_rz <- .validate_priority_table(rzwp3k_prioritas, "RZWP3K")
          if (!chk_rz$ok) stop(chk_rz$msg)
          
          priority_rtrw   <- rtrw_prioritas$RTRW[rtrw_prioritas$Prioritas == 1]
          priority_rzwp3k <- rzwp3k_prioritas$RZWP3K[rzwp3k_prioritas$Prioritas == 1]
          
          alpha   <- input$alpha_val
          th_high <- input$th_high
          th_med  <- input$th_med
          th_low  <- input$th_low
          
          matriks_serasi <- rv$matriks_serasi
          adjacent_economy_map <- rv$adjacent_economy_map
          
          get_compat <- function(x, y) {
            if (is.na(x) || is.na(y)) return(NA_real_)
            val <- matriks_serasi %>%
              dplyr::filter(class1 == x, class2 == y) %>%
              dplyr::pull(idx_serasi)
            if (length(val) == 0) NA_real_ else val
          }
          
          incProgress(0.4, detail = "Memisahkan layer RTRW dan RZWP3K...")
          # Split RTRW / RZWP3K sides and re-join
          rtrw_rows <- adjacent_economy_map %>%
            dplyr::filter(!is.na(RTRW)) %>%
            dplyr::select(id_pu, RTRW, alt_RTRW, idx_padu_final, econ_rtrw_delta) %>%
            sf::st_drop_geometry() %>%
            dplyr::as_tibble()
          
          rz_rows <- adjacent_economy_map %>%
            dplyr::filter(!is.na(RZWP3K)) %>%
            dplyr::select(id_pu, RZWP3K, alt_RZWP3K, econ_rzwp3k_delta) %>%
            sf::st_drop_geometry() %>%
            dplyr::as_tibble()
          
          common_cols <- adjacent_economy_map %>%
            dplyr::select(id_pu, idx_padan, area_buffer_ha) %>%
            sf::st_drop_geometry() %>%
            dplyr::distinct(id_pu, .keep_all = TRUE)
          
          split_rtrw_rzwp3k <- rtrw_rows %>%
            dplyr::full_join(rz_rows, by = "id_pu") %>%
            dplyr::left_join(common_cols, by = "id_pu")
          
          stopifnot(all(!is.na(split_rtrw_rzwp3k$RTRW) & !is.na(split_rtrw_rzwp3k$RZWP3K)))
          
          incProgress(0.6, detail = "Menghitung keputusan dan rekomendasi...")
          prep_recomendation <- split_rtrw_rzwp3k %>%
            dplyr::mutate(
              priority_class = dplyr::case_when(
                RTRW %in% priority_rtrw & RZWP3K %in% priority_rzwp3k ~ "Both",
                RTRW %in% priority_rtrw ~ "Yes",
                RZWP3K %in% priority_rzwp3k ~ "Yes",
                TRUE ~ "None"
              ),
              comp_if_rtrw = purrr::map2_dbl(alt_RTRW, RZWP3K, get_compat),
              comp_if_rz   = purrr::map2_dbl(RTRW, alt_RZWP3K, get_compat),
              N_if_rtrw = alpha * comp_if_rtrw + (1 - alpha) * idx_padu_final,
              N_if_rz   = alpha * comp_if_rz   + (1 - alpha) * idx_padu_final,
              dR = N_if_rtrw - idx_padan,
              dZ = N_if_rz   - idx_padan
            )
          
          recommendation_decision <- prep_recomendation %>%
            dplyr::mutate(
              recommendation = dplyr::case_when(
                RTRW %in% priority_rtrw & RZWP3K %in% priority_rzwp3k ~ dplyr::case_when(
                  dZ > dR ~ "Ubah RZ",
                  dR > dZ ~ "Ubah RTRW",
                  TRUE    ~ "Tetap/Koordinasi"
                ),
                RTRW %in% priority_rtrw & dZ > 0 ~ "Ubah RZ",
                RZWP3K %in% priority_rzwp3k & dR > 0 ~ "Ubah RTRW",
                dR > 0 | dZ > 0 ~ dplyr::if_else(dR >= dZ, "Ubah RTRW", "Ubah RZ"),
                TRUE ~ "Tetap/Koordinasi"
              ),
              RTRW_new = dplyr::if_else(recommendation == "Ubah RTRW", alt_RTRW, RTRW),
              RZWP3K_new = dplyr::if_else(recommendation == "Ubah RZ", alt_RZWP3K, RZWP3K),
              idx_serasi_new = purrr::map2_dbl(RTRW_new, RZWP3K_new, get_compat),
              idx_padan_new = alpha * idx_serasi_new + (1 - alpha) * idx_padu_final,
              actual_integration = .classify_integrasi(idx_padan, th_high, th_med, th_low),
              recom_integration = .classify_integrasi(idx_padan_new, th_high, th_med, th_low),
              econ_delta = dplyr::case_when(
                recommendation == "Ubah RTRW" ~ econ_rtrw_delta,
                recommendation == "Ubah RZ"   ~ econ_rzwp3k_delta,
                TRUE ~ 0
              ),
              area_change_ha = dplyr::if_else(recommendation == "Tetap/Koordinasi", 0, area_buffer_ha),
              idx_padan_delta = idx_padan_new - idx_padan
            )
          
          # Select only new columns to avoid duplication
          recommendation_decision_filter <- recommendation_decision %>%
            dplyr::select(
              id_pu,
              recommendation,
              RTRW_new,
              RZWP3K_new,
              idx_serasi_new,
              idx_padan_new,
              actual_integration,
              recom_integration,
              econ_delta,
              area_change_ha,
              idx_padan_delta
            ) %>%
            sf::st_drop_geometry()
          
          adjacent_recom_map <- adjacent_economy_map %>%
            dplyr::left_join(recommendation_decision_filter, by = "id_pu")
          
          incProgress(0.8, detail = "Menyimpan hasil ke disk...")
          out_gpkg <- file.path(output_dir(), "idx_padan_recommendation.gpkg")
          out_xlsx <- file.path(output_dir(), "idx_padan_recommendation.xlsx")
          
          sf::st_write(adjacent_recom_map, out_gpkg, delete_dsn = TRUE, quiet = TRUE)
          openxlsx::write.xlsx(sf::st_drop_geometry(adjacent_recom_map), out_xlsx)
          
          log_lines <- c(
            log_lines,
            "Ringkasan rekomendasi:",
            capture.output(print(table(adjacent_recom_map$recommendation))),
            "",
            "Ringkasan integrasi (aktual vs rekomendasi):",
            capture.output(print(
              adjacent_recom_map %>%
                sf::st_drop_geometry() %>%
                dplyr::count(actual_integration, recom_integration) %>%
                dplyr::arrange(actual_integration, recom_integration)
            ))
          )
          
          rv$final_result <- list(
            map = adjacent_recom_map,
            table = sf::st_drop_geometry(adjacent_recom_map),
            gpkg_path = out_gpkg,
            xlsx_path = out_xlsx
          )
          rv$final_log <- paste(log_lines, collapse = "\n")
          showNotification("Berhasil! File rekomendasi telah disimpan.", type = "message")
          incProgress(1.0, detail = "Selesai!")
          
        }, error = function(e) {
          call_txt <- if (!is.null(conditionCall(e))) paste0("\n(pada pemanggilan: ", paste(deparse(conditionCall(e)), collapse = " "), ")") else ""
          rv$final_log <- paste0("Error: ", conditionMessage(e), call_txt)
          showNotification(paste("Gagal:", conditionMessage(e)), type = "error", duration = NULL)
        })
      })
    })
    
    observeEvent(input$btn_back_4, go_to_panel("step3"))
    
    # ── Outputs for right column ─────────────────────────────
    # Overall status
    output$status_box <- renderUI({
      if (!is.null(rv$final_result)) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Selesai. Silakan lanjut ke ", tags$strong("Langkah Rekonsiliasi"), ".")
      } else if (!is.null(rv$final_log) && grepl("^Error", rv$final_log)) {
        div(class = "alert alert-danger mb-0",
            tags$i(class = "bi bi-exclamation-triangle-fill me-2"),
            "Terjadi kesalahan, lihat Visual Log.")
      } else if (rv$unlocked >= 4) {
        div(class = "alert alert-secondary mb-0", "Siap dijalankan.")
      } else {
        div(class = "alert alert-secondary mb-0", "Lengkapi langkah sebelumnya.")
      }
    })
    
    # ── Table Accordion (collapsible panels) ─────────────────
    output$table_accordion <- renderUI({
      panels <- list()
      
      if (!is.null(rv$alt_status) && rv$alt_status$ok) {
        panels <- c(panels, list(
          accordion_panel(
            title = "Alternatif Zona (Pratinjau)",
            value = "preview",
            icon = tags$i(class = "bi bi-eye"),
            DT::DTOutput(ns("alt_preview_table"))  
          )
        ))
      }
      
      if (!is.null(rv$final_result)) {
        panels <- c(panels, list(
          accordion_panel(
            title = "Rekomendasi Akhir",
            value = "final",
            icon = tags$i(class = "bi bi-check2-circle"),
            div(style = "max-height: 400px; overflow: auto;",
                DT::DTOutput(ns("final_table")))   
          )
        ))
      }
      
      if (length(panels) == 0) {
        return(tags$p("Belum ada tabel untuk ditampilkan."))
      }
      
      accordion(
        id = ns("table_accordion_widget"),
        multiple = TRUE,
        !!!panels
      )
    })
    
    output$alt_preview_table <- DT::renderDT({
      req(rv$alt_status, rv$alt_status$ok)
      df_preview <- rv$alt_status$preview
      
      desired_cols <- c("id_pu", "RTRW", "RZWP3K", "area_ha", "length", "idx_serasi", "alt_RTRW", "alt_RZWP3K")
      cols_present <- intersect(desired_cols, names(df_preview))
      
      if (length(cols_present) == 0) {
        df_subset <- df_preview
      } else {
        df_subset <- df_preview[, cols_present, drop = FALSE]
      }

      label_map <- c(
        "id_pu"       = "ID PU",
        "RTRW"        = "RTRW",
        "RZWP3K"      = "RZWP3K",
        "area_ha"     = "Luas (ha)",
        "length"      = "Panjang Segmen (meter)",
        "idx_serasi"  = "Indeks SERASI",
        "alt_RTRW"    = "RTRW Alternatif Terpilih",
        "alt_RZWP3K"  = "RZWP3K Alternatif Terpilih"
      )
      
      new_names <- label_map[names(df_subset)]
      new_names[is.na(new_names)] <- names(df_subset)[is.na(new_names)]
      names(new_names) <- names(df_subset)
      colnames(df_subset) <- unname(new_names)
      
      # Round numeric columns
      numeric_cols <- names(df_subset)[sapply(df_subset, is.numeric)]
      exclude_round <- c("ID PU", "id_pu")
      round_cols <- setdiff(numeric_cols, exclude_round)
      
      DT::datatable(
        df_subset,
        extensions = c('FixedColumns', 'FixedHeader'),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'Bfrtip',
          fixedColumns = list(leftColumns = 1),
          fixedHeader = TRUE
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      ) %>%
        DT::formatRound(columns = round_cols, digits = 2)
    })
    
    output$final_table <- DT::renderDT({
      req(rv$final_result)
      df_final <- rv$final_result$table
      
      base_cols <- c(
        "id_pu", "RTRW", "RZWP3K", "area_ha", "length", "idx_padu_final",
        "alt_RTRW", "alt_RZWP3K", "recommendation", "RTRW_new", "RZWP3K_new",
        "idx_serasi", "idx_serasi_new", "idx_padan", "idx_padan_new", "idx_padan_delta",
        "actual_integration", "recom_integration"
      )

      extra_cols <- c()
      if (isTRUE(rv$npv_status$ok)) {
        econ_candidates <- grep("^(npv_ha_|econ_)", names(df_final), value = TRUE)
        extra_cols <- intersect(econ_candidates, names(df_final))
      }
      
      # Combine and keep only those that exist
      cols_to_show <- intersect(c(base_cols, extra_cols), names(df_final))
      
      if (length(cols_to_show) == 0) {
        df_subset <- df_final
      } else {
        df_subset <- df_final[, cols_to_show, drop = FALSE]
      }

      label_map <- c(
        "id_pu"               = "ID PU",
        "RTRW"                = "RTRW Awal",
        "RZWP3K"              = "RZWP3K Awal",
        "area_ha"             = "Luas (ha)",
        "length"              = "Panjang Segmen Bertetangga (meter)",
        "idx_padu_final"      = "Indeks PADU Kombinasi",
        "alt_RTRW"            = "RTRW Alternatif",
        "alt_RZWP3K"          = "RZWP3K Alternatif",
        "recommendation"      = "Rekomendasi",
        "RTRW_new"            = "RTRW Baru",
        "RZWP3K_new"          = "RZWP3K Baru",
        "idx_serasi"          = "Indeks SERASI Awal",
        "idx_serasi_new"      = "Indeks SERASI Baru",
        "idx_padan"           = "Indeks PADAN Awal",
        "idx_padan_new"       = "Indeks PADAN Baru",
        "idx_padan_delta"     = "Selisih Indeks PADAN",
        "actual_integration"  = "Integrasi Aktual",
        "recom_integration"   = "Integrasi Hasil Rekomendasi"
      )
      
      econ_label_map <- c(
        "npv_ha_actual_rtrw"   = "NPV per ha (Aktual RTRW)",
        "npv_ha_actual_rzwp3k" = "NPV per ha (Aktual RZWP3K)",
        "npv_ha_recom_rtrw"    = "NPV per ha (Rekomendasi RTRW)",
        "npv_ha_recom_rzwp3k"  = "NPV per ha (Rekomendasi RZWP3K)",
        "econ_rtrw_actual"     = "Nilai Ekonomi RTRW (Aktual)",
        "econ_rtrw_recom"      = "Nilai Ekonomi RTRW (Rekomendasi)",
        "econ_rtrw_delta"      = "Selisih Nilai Ekonomi RTRW",
        "econ_rzwp3k_actual"   = "Nilai Ekonomi RZWP3K (Aktual)",
        "econ_rzwp3k_recom"    = "Nilai Ekonomi RZWP3K (Rekomendasi)",
        "econ_rzwp3k_delta"    = "Selisih Nilai Ekonomi RZWP3K",
        "econ_delta"           = "Selisih Nilai Ekonomi Akhir"
      )
      
      full_map <- c(label_map, econ_label_map)
      new_names <- full_map[names(df_subset)]
      new_names[is.na(new_names)] <- names(df_subset)[is.na(new_names)]
      names(new_names) <- names(df_subset)
      colnames(df_subset) <- unname(new_names)
      
      # Round numeric columns 
      numeric_cols <- names(df_subset)[sapply(df_subset, is.numeric)]
      exclude_round <- c("ID_PU", "id_pu")
      round_cols <- setdiff(numeric_cols, exclude_round)
      
      DT::datatable(
        df_subset,
        extensions = c('FixedColumns', 'FixedHeader'),
        selection = "single",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'Bfrtip',
          fixedColumns = list(leftColumns = 3),
          fixedHeader = TRUE
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      ) %>%
        DT::formatRound(columns = round_cols, digits = 2)
    })
    
    # ── Interactive map ──────────────────────────────────────
    output$recommendation_map <- renderLeaflet({
      req(rv$final_result)
      map_sf <- rv$final_result$map
      
      if (nrow(map_sf) == 0) {
        return(leaflet::leaflet() %>% 
                 leaflet::addControl("Tidak ada data untuk ditampilkan.", position = "topright"))
      }
      
      if (!sf::st_is_longlat(map_sf)) {
        map_sf <- sf::st_transform(map_sf, crs = 4326)
      }
      
      required_cols <- c("recommendation", "actual_integration", "recom_integration",
                         "idx_padan", "idx_padan_new", "RTRW_new", "RZWP3K_new", "id_pu",
                         "RTRW", "RZWP3K")
      missing <- setdiff(required_cols, names(map_sf))
      if (length(missing) > 0) {
        rv$final_log <- paste0("Map error: missing columns: ", paste(missing, collapse = ", "))
        return(leaflet::leaflet() %>% 
                 leaflet::addControl("Kolom yang diperlukan tidak ditemukan. Periksa Log.", position = "topright"))
      }
      
      map_sf$search_label <- paste0("ID PU: ", map_sf$id_pu, " | ", map_sf$RTRW, " | ", map_sf$RZWP3K, " | Rekomendasi: ", map_sf$recommendation)
      
      pal <- leaflet::colorFactor(
        palette = c("blue", "green", "orange", "red", "purple"),
        domain = unique(map_sf$recommendation),
        na.color = "grey"
      )
      
      leaflet::leaflet(map_sf) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::addPolygons(
          group = "recommendation_layer",
          fillColor = ~pal(recommendation),
          fillOpacity = 0.7,
          weight = 1,
          color = "black",
          stroke = FALSE,
          label = ~search_label,
          popup = ~paste(
            "<b>ID PU:</b>", id_pu, "<br>",
            "<b>RTRW asal:</b>", RTRW, "<br>",
            "<b>RZWP3K asal:</b>", RZWP3K, "<br>",
            "<b>Rekomendasi:</b>", recommendation, "<br>",
            "<b>RTRW baru:</b>", RTRW_new, "<br>",
            "<b>RZWP3K baru:</b>", RZWP3K_new, "<br>",
            "<b>Indeks integrasi aktual:</b>", round(idx_padan, 3), "<br>",
            "<b>Indeks integrasi baru:</b>", round(idx_padan_new, 3)
          ) %>% lapply(htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(
            weight = 3,
            color = "red",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        leaflet.extras::addSearchFeatures(
          targetGroups = "recommendation_layer",
          options = leaflet.extras::searchFeaturesOptions(
            propertyName = "label",    
            zoom = 15,                 
            openPopup = TRUE,           
            firstTipSubmit = TRUE,
            autoCollapse = FALSE,
            hideMarkerOnCollapse = TRUE
          )
        ) %>% leaflet.extras::addResetMapButton() %>% 
        leaflet::addLegend(
          position = "bottomright",
          pal = pal,
          values = ~recommendation,
          title = "Rekomendasi",
          opacity = 0.7
        )
    })
    
    # ── Sync rv$final_result → standard rv fields for render_result_server ──
    observe({
      if (!is.null(rv$final_result)) {
        rv$analysis_result <- list(
          map   = rv$final_result$map,
          table = rv$final_result$table
        )
        rv$gpkg_path   <- rv$final_result$gpkg_path
        rv$xlsx_path   <- rv$final_result$xlsx_path
        rv$log_messages <- if (!is.null(rv$final_log)) rv$final_log else ""
      } else {
        rv$analysis_result <- NULL
        rv$gpkg_path       <- NULL
        rv$xlsx_path       <- NULL
        rv$log_messages    <- if (!is.null(rv$final_log)) rv$final_log else "Siap untuk analisis rekomendasi."
      }
    })
    
    # ── Row-click: zoom & highlight on recommendation_map ───
    observeEvent(input$final_table_rows_selected, {
      req(rv$final_result)
      
      selected_idx <- input$final_table_rows_selected
      df_table <- rv$final_result$table
      
      if (!"id_pu" %in% colnames(df_table)) return()
      selected_id_pu <- df_table$id_pu[selected_idx]
      
      map_sf <- rv$final_result$map
      if (!sf::st_is_longlat(map_sf)) {
        map_sf <- sf::st_transform(map_sf, crs = 4326)
      }
      
      selected_polygon <- map_sf[map_sf$id_pu == selected_id_pu, ]
      req(nrow(selected_polygon) > 0)
      
      centroid_coord <- sf::st_coordinates(sf::st_centroid(selected_polygon))
      
      popup_text <- paste0(
        "<b>ID PU:</b> ", selected_polygon$id_pu[1], "<br>",
        "<b>Rekomendasi:</b> ", selected_polygon$recommendation[1], "<br>",
        "<b>RTRW baru:</b> ", selected_polygon$RTRW_new[1], "<br>",
        "<b>RZWP3K baru:</b> ", selected_polygon$RZWP3K_new[1]
      )
      
      leaflet::leafletProxy("recommendation_map", session = session) %>%
        leaflet::clearGroup("row_highlight") %>%
        leaflet::setView(lng = centroid_coord[1], lat = centroid_coord[2], zoom = 13) %>%
        leaflet::addPolygons(
          data        = selected_polygon,
          color       = "#FF4136",
          weight      = 5,
          fillColor   = "#FFDC00",
          fillOpacity = 0.5,
          stroke      = TRUE,
          group       = "row_highlight",
          popup       = lapply(popup_text, htmltools::HTML)
        )
    })
    
    # Clear highlight when no row selected
    observe({
      if (is.null(input$final_table_rows_selected)) {
        leaflet::leafletProxy("recommendation_map", session = session) %>%
          leaflet::clearGroup("row_highlight")
      }
    })
    
    # ── Shared result server: wires validation_log, dl_gpkg, dl_xlsx ──
    recom_adjacent_config <- list(
      map_color_col    = "recommendation",
      map_title        = "Rekomendasi",
      map_palette      = c("blue", "green", "orange", "red", "purple"),
      map_label_cols   = list(
        "ID PU"        = "id_pu",
        "RTRW"         = "RTRW",
        "RZWP3K"       = "RZWP3K",
        "Rekomendasi"  = "recommendation",
        "RTRW Baru"    = "RTRW_new",
        "RZWP3K Baru"  = "RZWP3K_new"
      ),
      table_cols       = c(
        "id_pu"              = "ID PU",
        "RTRW"               = "RTRW Awal",
        "RZWP3K"             = "RZWP3K Awal",
        "recommendation"     = "Rekomendasi",
        "RTRW_new"           = "RTRW Baru",
        "RZWP3K_new"         = "RZWP3K Baru",
        "idx_padan"          = "Indeks PADAN Awal",
        "idx_padan_new"      = "Indeks PADAN Baru",
        "idx_padan_delta"    = "Selisih Indeks PADAN",
        "actual_integration" = "Integrasi Aktual",
        "recom_integration"  = "Integrasi Hasil Rekomendasi"
      ),
      table_round_cols = c(
        "Indeks PADAN Awal", "Indeks PADAN Baru", "Selisih Indeks PADAN"
      )
    )
    
    render_result_server(input, output, session, rv, recom_adjacent_config)
  })
}