# ui/modules/mod_recommendation_overlaps.R
# ============================================================
#  MODULE: Recommendation (Overlaps)
#  Wizard flow (accordion in left panel)
#  Outputs (map, table accordion, log) in right panel.
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── small UI helpers ────────────────────────────────────────────
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

# ── validation helpers ──────────────────────────────────────────
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

# Helper to compute serasi index from matrix
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

# ── UI ──────────────────────────────────────────────────────────
recommendation_overlaps_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("4. Analisis Penyusunan Alternatif Area Tumpang Tindih", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menentukan opsi penyelesaian konflik tumpang tindih dalam integrasi tata ruang darat-laut.",
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
              title = "Langkah 3 — Menentukan Rekomendasi",
              value = "step3",
              icon = tags$i(class = "bi bi-check2-circle"),
              uiOutput(ns("step3_ui"))
            )
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

# ── Server ──────────────────────────────────────────────────────
recommendation_overlaps_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      unlocked = 1,
      
      # step 1
      idx_padan_map        = NULL,
      filter_snapshot      = NULL,
      idx_padan_map_filter = NULL,
      count_before         = NULL,
      count_after          = NULL,
      
      # step 2
      matriks_serasi        = NULL,
      alt_template_path     = NULL,
      alt_table_snapshot    = NULL,
      idx_padan_map_alt     = NULL,
      alt_status            = NULL,
      
      # step 3
      final_result          = NULL,
      final_log             = NULL
    )
    
    go_to_panel <- function(value) accordion_panel_set(id = "wizard", values = value, session = session)
    
    reset_from_step2 <- function() {
      rv$idx_padan_map_alt  <- NULL
      rv$alt_table_snapshot <- NULL
      rv$alt_status         <- NULL
      rv$final_result       <- NULL
      rv$final_log          <- NULL
      rv$unlocked           <- min(rv$unlocked, 2)
    }
    
    reset_from_step3 <- function() {
      rv$final_result <- NULL
      rv$final_log    <- NULL
      rv$unlocked     <- min(rv$unlocked, 3)
    }
    
    # ── Step 1 UI ──────────────────────────────────────────────
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
              numericInput(ns("idx_filter"), "Ambang Indeks PADAN (<)", value = 0.75, min = 0, max = 1, step = 0.05)
            )
          )
        ),
        uiOutput(ns("filter_count_ui")),
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Step 2")
      )
    })
    
    # Load map
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
    
    # Reactive filter
    filtered_data <- reactive({
      req(rv$idx_padan_map)
      
      map <- rv$idx_padan_map
      keep <- rep(TRUE, nrow(map))
      if (isTRUE(input$apply_area)) keep <- keep & (as.numeric(map$area_ha) > input$area_filter)
      if (isTRUE(input$apply_idx))    keep <- keep & (map$idx_padan < input$idx_filter)
      
      list(
        filtered = map[keep, ],
        before = nrow(map),
        after  = nrow(map[keep, ])
      )
    })
    
    observeEvent(filtered_data(), {
      res <- filtered_data()
      rv$idx_padan_map_filter <- res$filtered
      rv$count_before <- res$before
      rv$count_after  <- res$after
      
      if (!is.null(rv$filter_snapshot)) {
        current <- list(
          apply_area = input$apply_area,
          area_filter = input$area_filter,
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
        tags$div(sprintf("Sebelum filter: %d kasus", rv$count_before)),
        tags$div(sprintf("Setelah filter: %d kasus", rv$count_after)),
        tags$div(sprintf("Terhapus: %d kasus (%.1f%%)", removed, pct))
      )
    })
    
    # Step 1 -> Step 2
    observeEvent(input$btn_next_1, {
      if (is.null(rv$idx_padan_map_filter)) {
        showNotification("Terapkan filter terlebih dahulu.", type = "warning")
        return()
      }
      rv$filter_snapshot <- list(
        apply_area = input$apply_area,
        area_filter = input$area_filter,
        apply_idx = input$apply_idx,
        idx_filter = input$idx_filter
      )
      rv$unlocked <- max(rv$unlocked, 2)
      go_to_panel("step2")
    })
    
    # ── Step 2 UI ──────────────────────────────────────────────
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

    # ── Make template using step = "step1" ─────────────────────
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
      
      # Clear previous status
      rv$alt_template_path <- NULL
      
      withProgress(message = "Membuat Template Opsi Alternatif", value = 0, {
        tryCatch({
          incProgress(0.2, detail = "Memuat matriks serasi...")
          rv$matriks_serasi <- load_validate_matrix_table(input$matrix_file$datapath, title = "serasi")
          
          out_dir_step2 <- file.path(output_dir(), "step2")
          dir.create(out_dir_step2, recursive = TRUE, showWarnings = FALSE)
          
          incProgress(0.4, detail = "Memproses opsi alternatif...")
          result <- determine_alternative_zones(
            idx_padan_map_filter = rv$idx_padan_map_filter,
            serasi_matrix = rv$matriks_serasi,
            step = "step1",
            n_alt = input$n_alt,
            output_dir = out_dir_step2
          )
          
          incProgress(0.8, detail = "Menyimpan file template...")
          output_path <- file.path(out_dir_step2, "overlaps_alternative_zones_selections.xlsx")
          if (!file.exists(output_path)) {
            stop("File template tidak ditemukan setelah pembuatan.")
          }
          
          rv$alt_template_path <- output_path
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
        if (!is.null(rv$alt_template_path)) basename(rv$alt_template_path) else "overlaps_alternative_zones_selections.xlsx"
      },
      content = function(file) {
        req(rv$alt_template_path)
        file.copy(rv$alt_template_path, file, overwrite = TRUE)
      }
    )
    
    observe({
      req(input$alt_upload)
      
      # Dependencies not yet ready — show a pending hint and wait.
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
        check <- .validate_alt_table_overlaps(alt_table, rv$matriks_serasi)
        
        if (!check$ok) {
          rv$idx_padan_map_alt <- NULL
          rv$alt_status <- list(ok = FALSE, msg = check$msg, preview = NULL)
          return()
        }
        
        # Join alternative zones to the filtered map
        idx_padan_map_alt <- dplyr::left_join(
          rv$idx_padan_map_filter,
          alt_table[, c("id_pu", "id_rtrw", "id_rzwp3k", "alt_RTRW", "alt_RZWP3K")],
          by = c("id_pu", "id_rtrw", "id_rzwp3k")
        )
        
        # Compute alternative serasi indices
        idx_padan_map_alt <- idx_padan_map_alt %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            idx_serasi_rtrw_alt = .get_serasi(alt_RTRW, RZWP3K, rv$matriks_serasi),
            idx_serasi_rzwp3k_alt = .get_serasi(RTRW, alt_RZWP3K, rv$matriks_serasi)
          ) %>%
          dplyr::ungroup()
        
        new_snapshot <- alt_table[, c("id_pu", "id_rtrw", "id_rzwp3k", "alt_RTRW", "alt_RZWP3K")]
        changed <- is.null(rv$alt_table_snapshot) || !identical(new_snapshot, rv$alt_table_snapshot)
        if (changed) reset_from_step3()
        rv$alt_table_snapshot <- new_snapshot
        
        rv$idx_padan_map_alt <- idx_padan_map_alt
        rv$alt_status <- list(
          ok = TRUE, msg = check$msg,
          preview = head(sf::st_drop_geometry(idx_padan_map_alt)[, intersect(
            c("id_pu", "id_rtrw", "id_rzwp3k", "RTRW", "RZWP3K", 
              "area_ha", "admin", "idx_serasi",         
              "alt_RTRW", "alt_RZWP3K"),
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
    
    # ── Step 3 UI (final recommendation) ──────────────────────
    output$step3_ui <- renderUI({
      if (rv$unlocked < 3) return(.locked_panel())
      tagList(
        layout_column_wrap(
          width = 1/2,
          fileInput(ns("rtrw_priority_file"), "Tabel Acuan Pola RTRW (.xlsx)", accept = ".xlsx"),
          fileInput(ns("rzwp3k_priority_file"), "Tabel Acuan Pola RZWP3K (.xlsx)", accept = ".xlsx")
        ),
        hr(),
        layout_column_wrap(
          width = 1/3,
          numericInput(ns("threshold_serasi"), "Ambang SERASI", value = 0.6, min = 0, max = 1, step = 0.05),
          numericInput(ns("threshold_padu"), "Ambang PADU", value = 0.65, min = 0, max = 1, step = 0.05)
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
        .step_nav(ns, back_id = "btn_back_3", next_id = NULL)
      )
    })
    
    # ── Final calculation ──────────────────────────────────────
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
      
      req(rv$idx_padan_map_alt, input$rtrw_priority_file, input$rzwp3k_priority_file)
      
      rv$final_result <- NULL
      withProgress(message = "Membuat Rekomendasi Tumpang Tindih", value = 0, {
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
          
          alpha            <- input$alpha_val
          threshold_serasi <- input$threshold_serasi
          threshold_padu   <- input$threshold_padu
          
          incProgress(0.4, detail = "Menghitung indeks padan alternatif...")
          df <- rv$idx_padan_map_alt
          
          # Compute alternative padan indices
          df <- df %>%
            dplyr::mutate(
              idx_padan_rtrw_alt = alpha * idx_serasi_rtrw_alt + (1 - alpha) * idx_padu_final,
              idx_padan_rzwp3k_alt = alpha * idx_serasi_rzwp3k_alt + (1 - alpha) * idx_padu_final
            )
          
          # First-pass recommendation
          df <- df %>%
            dplyr::mutate(
              recommendation = dplyr::case_when(
                is.na(RTRW) | is.na(RZWP3K) | is.na(idx_serasi) | is.na(idx_padu_final) ~ NA_character_,
                RTRW %in% priority_rtrw ~ "",
                RZWP3K %in% priority_rzwp3k ~ "Ubah_RTRW",
                idx_serasi >= threshold_serasi ~ "Koordinasi",
                idx_padu_final >= threshold_padu ~ "Ubah_RZWP3K",
                idx_serasi < threshold_serasi & idx_padu_final < threshold_padu ~ "Ubah_RTRW"
              )
            )
          
          incProgress(0.6, detail = "Membuat keputusan akhir...")
          # Final decision
          df <- df %>%
            dplyr::mutate(
              decision = dplyr::case_when(
                is.na(recommendation) | is.na(idx_padan_rzwp3k_alt) | is.na(idx_padan) | 
                  is.na(alt_RZWP3K) | is.na(idx_padan_rtrw_alt) | is.na(alt_RTRW) ~ NA_character_,
                recommendation == "Ubah_RZWP3K" & idx_padan_rzwp3k_alt > idx_padan ~ 
                  paste("Ubah RZWP3K ke", alt_RZWP3K),
                recommendation == "Ubah_RZWP3K" & idx_padan_rzwp3k_alt <= idx_padan ~ "Tetap/Koordinasi",
                recommendation == "Ubah_RTRW" & idx_padan_rtrw_alt > idx_padan ~ 
                  paste("Ubah RTRW ke", alt_RTRW),
                recommendation == "Ubah_RTRW" & idx_padan_rtrw_alt <= idx_padan ~ "Tetap/Koordinasi",
                TRUE ~ "Tetap/Koordinasi"
              )
            )
          
          # Final padan index
          df <- df %>%
            dplyr::mutate(
              idx_padan_final = dplyr::case_when(
                grepl("Ubah RTRW", decision, fixed = TRUE) ~ idx_padan_rtrw_alt,
                grepl("Ubah RZWP3K", decision, fixed = TRUE) ~ idx_padan_rzwp3k_alt,
                grepl("Tetap/Koordinasi", decision, fixed = TRUE) ~ idx_padan,
                TRUE ~ NA_real_
              )
            )
          
          incProgress(0.8, detail = "Menyimpan hasil ke disk...")
          out_gpkg <- file.path(output_dir(), "idx_padan_overlaps_recommendation.gpkg")
          out_xlsx <- file.path(output_dir(), "idx_padan_overlaps_recommendation.xlsx")
          
          sf::st_write(df, out_gpkg, delete_dsn = TRUE, quiet = TRUE)
          openxlsx::write.xlsx(sf::st_drop_geometry(df), out_xlsx)
          
          log_lines <- c(
            character(0),
            "Ringkasan rekomendasi (langkah pertama):",
            capture.output(print(table(df$recommendation, useNA = "ifany"))),
            "",
            "Ringkasan keputusan akhir:",
            capture.output(print(table(df$decision, useNA = "ifany")))
          )
          
          rv$final_result <- list(
            map = df,
            table = sf::st_drop_geometry(df),
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
    
    observeEvent(input$btn_back_3, go_to_panel("step2"))
    
    # ── Outputs for right column ──────────────────────────────
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
    
    # Table accordion
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
      df_zone_alt <- rv$alt_status$preview
      
      df_zone_alt_subset <- df_zone_alt[, c("id_pu", "RTRW", "RZWP3K", "area_ha", "admin", "idx_serasi", "alt_RTRW", "alt_RZWP3K")]
      colnames(df_zone_alt_subset) <- c("ID_PU", "RTRW", "RZWP3K", "Luas (ha)", "Administrasi", "Indeks SERASI", "RTRW Alternatif Terpilih", "RZWP3K Alternatif Terpilih")
      
      DT::datatable(
        df_zone_alt_subset,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'Bfrtip'
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      ) %>%
        DT::formatRound(
          columns = c("Luas (ha)", "Indeks SERASI"),  
          digits = 2
        )
    })
    
    output$final_table <- DT::renderDT({
      req(rv$final_result)
      df_final <- rv$final_result$table
      df_final_subset <- df_final[, c("id_pu", "RTRW", "RZWP3K", "area_ha", "admin", "idx_serasi", "idx_padu_final", "alt_RTRW", "alt_RZWP3K", "idx_serasi_rtrw_alt", "idx_serasi_rzwp3k_alt", "idx_padan_rtrw_alt", "idx_padan_rzwp3k_alt", "recommendation", "decision", "idx_padan_final")]
      colnames(df_final_subset) <- c("ID_PU", "RTRW", "RZWP3K", "Luas (ha)", "Administrasi", "Indeks SERASI Awal", "Indeks PADU Kombinasi", "RTRW Alternatif", "RZWP3K Alternatif", "Indeks SERASI RTRW Alternatif", "Indeks SERASI RZWP3K Alternatif", "Indeks PADAN RTRW Alternatif", "Indeks PADAN RZWP3K Alternatif", "Opsi Rekomendasi", "Rekomendasi Keputusan", "Indeks PADAN Akhir")
      
      DT::datatable(
        df_final_subset,
        extensions = c('FixedColumns', 'FixedHeader'),
        selection = "single",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'Bfrtip',
          fixedColumns = list(
            leftColumns = 3
          ),
          fixedHeader = TRUE
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      ) %>%
        DT::formatRound(
          columns = c("Luas (ha)", "Indeks SERASI Awal", "Indeks PADU Kombinasi", "Indeks SERASI RTRW Alternatif", "Indeks SERASI RZWP3K Alternatif", "Indeks PADAN RTRW Alternatif", "Indeks PADAN RZWP3K Alternatif", "Indeks PADAN Akhir"),  
          digits = 2
        )
    })
    
    
    # ── Interactive map ────────────────────────────────────────
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
      
      required_cols <- c("recommendation", "decision", "idx_padan", "idx_padan_final",
                         "RTRW", "RZWP3K", "alt_RTRW", "alt_RZWP3K", "id_pu")
      missing <- setdiff(required_cols, names(map_sf))
      if (length(missing) > 0) {
        rv$final_log <- paste0("Map error: missing columns: ", paste(missing, collapse = ", "))
        return(leaflet::leaflet() %>% 
                 leaflet::addControl("Kolom yang diperlukan tidak ditemukan. Periksa Log.", position = "topright"))
      }
      
      map_sf$search_label <- paste0("ID PU: ", map_sf$id_pu, " | ", map_sf$RTRW, " | ", map_sf$RZWP3K, " | Rekomendasi: ", map_sf$recommendation)
      
      
      pal <- leaflet::colorFactor(
        palette = c("blue", "green", "orange", "red", "purple", "grey"),
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
            "<b>Alt RTRW:</b>", alt_RTRW, "<br>",
            "<b>Alt RZWP3K:</b>", alt_RZWP3K, "<br>",
            "<b>Keputusan:</b>", decision, "<br>",
            "<b>Indeks PADAN asal:</b>", round(idx_padan, 3), "<br>",
            "<b>Indeks PADAN akhir:</b>", round(idx_padan_final, 3)
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
          title = "Rekomendasi Awal",
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
        rv$gpkg_path    <- rv$final_result$gpkg_path
        rv$xlsx_path    <- rv$final_result$xlsx_path
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
        "<b>RTRW asal:</b> ", selected_polygon$RTRW[1], "<br>",
        "<b>RZWP3K asal:</b> ", selected_polygon$RZWP3K[1], "<br>",
        "<b>Rekomendasi:</b> ", selected_polygon$recommendation[1], "<br>",
        "<b>Keputusan:</b> ", selected_polygon$decision[1]
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
    recom_overlaps_config <- list(
      map_color_col  = "recommendation",
      map_title      = "Rekomendasi Awal",
      map_palette    = c("blue", "green", "orange", "red", "purple", "grey"),
      map_label_cols = list(
        "ID PU"        = "id_pu",
        "RTRW"         = "RTRW",
        "RZWP3K"       = "RZWP3K",
        "Rekomendasi"  = "recommendation",
        "Keputusan"    = "decision"
      ),
      table_cols     = c(
        "id_pu"             = "ID PU",
        "RTRW"              = "RTRW",
        "RZWP3K"            = "RZWP3K",
        "area_ha"           = "Luas (ha)",
        "admin"             = "Administrasi",
        "idx_serasi"        = "Indeks SERASI Awal",
        "idx_padu_final"    = "Indeks PADU Kombinasi",
        "alt_RTRW"          = "RTRW Alternatif",
        "alt_RZWP3K"        = "RZWP3K Alternatif",
        "recommendation"    = "Opsi Rekomendasi",
        "decision"          = "Rekomendasi Keputusan",
        "idx_padan_final"   = "Indeks PADAN Akhir"
      ),
      table_round_cols = c(
        "Luas (ha)", "Indeks SERASI Awal", "Indeks PADU Kombinasi",
        "Indeks PADAN Akhir"
      )
    )
    
    render_result_server(input, output, session, rv, recom_overlaps_config)
  })
}