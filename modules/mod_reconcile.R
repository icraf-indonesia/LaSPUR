# ============================================================
#  MODULE: Reconcile (RTRW & RZWP3K)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── Small UI Helpers ────────────────────────────────────────────
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

# ── Shapefile / GPKG Layer Reader Helper ────────────────────────
.read_spatial_input <- function(file_df) {
  if (is.null(file_df)) return(NULL)
  
  if (nrow(file_df) == 1 && grepl("\\.gpkg$", file_df$name[1], ignore.case = TRUE)) {
    return(sf::st_read(file_df$datapath[1], quiet = TRUE))
  }
  
  temp_dir <- tempdir()
  for (i in 1:nrow(file_df)) {
    file.copy(file_df$datapath[i], file.path(temp_dir, file_df$name[i]), overwrite = TRUE)
  }
  
  shp_file <- file_df$name[grepl("\\.shp$", file_df$name, ignore.case = TRUE)]
  if (length(shp_file) == 0) {
    stop("Komponen file .shp tidak ditemukan. Pastikan Anda memilih file .shp, .shx, .dbf, dan .prj sekaligus.")
  }
  
  sf::st_read(file.path(temp_dir, shp_file[1]), quiet = TRUE)
}

# ── Safe Display-Layer Builder ──────────────────────────────────
.prepare_map_display <- function(sf_obj, layer_name = NULL, category_col = NULL, keep_cols = character(0)) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) return(NULL)
  
  if (!is.null(category_col) && category_col %in% names(sf_obj)) {
    sf_obj$origin_layer <- sf_obj[[category_col]]
  } else {
    sf_obj$origin_layer <- if (!is.null(layer_name)) layer_name else "Layer"
  }
  
  sf_obj <- sf::st_make_valid(sf_obj)
  
  sf_obj <- tryCatch(
    sf::st_collection_extract(sf_obj, "POLYGON", warn = FALSE),
    error = function(e) sf_obj
  )
  
  sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
  if (nrow(sf_obj) == 0) return(NULL)
  
  cols_keep <- intersect(c(keep_cols, "origin_layer", "geometry"), names(sf_obj))
  sf_obj[, cols_keep]
}

.to_leaflet_crs <- function(sf_obj) {
  crs <- sf::st_crs(sf_obj)
  if (is.na(crs)) {
    sf::st_crs(sf_obj) <- 4326
    return(sf_obj)
  }
  is_ll <- tryCatch(sf::st_is_longlat(sf_obj), error = function(e) NA)
  if (is.na(is_ll) || !isTRUE(is_ll)) {
    sf_obj <- tryCatch(
      sf::st_transform(sf_obj, crs = 4326),
      error = function(e) {
        sf::st_crs(sf_obj) <- 4326
        sf_obj
      }
    )
  }
  sf_obj
}

# ── UI ──────────────────────────────────────────────────────────
reconcile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("5. Rekonsiliasi Integrasi Tata Ruang", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Sinkronisasi peta RTRW dan RZWP3K secara otomatis berdasarkan matriks keputusan prioritas wilayah darat dan laut.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),
    
    fluidRow(
      class = "g-3",
      
      column(
        width = 4,
        card(
          card_header("Input & Parameter"),
          accordion(
            id = ns("wizard"),
            open = "step1",
            multiple = FALSE,
            
            accordion_panel(
              title = "Langkah 1 — Menyiapkan Keputusan Rekonsiliasi",
              value = "step1",
              icon = tags$i(class = "bi bi-file-earmark-spreadsheet-fill"),
              uiOutput(ns("step1_ui"))
            ),
            
            accordion_panel(
              title = "Langkah 2 — Menentukan Keputusan Rekonsiliasi",
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
          navset_tab(
            nav_panel(
              "Peta",
              leafletOutput(ns("reconcile_map_view"), height = "500px")
            ),
            nav_panel(
              "Tabel Hasil",
              div(
                style = "height: 500px; overflow: auto;",
                uiOutput(ns("table_view_panel"))
              )
            ),
            nav_panel(
              "Log & Validasi",
              div(
                style = "max-height: 300px; overflow-y: auto; background-color: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 0.9rem; white-space: pre-wrap;",
                verbatimTextOutput(ns("validation_log"))
              )
            )
          ),
          div(
            style = "display: flex; gap: 8px; flex-wrap: wrap; margin-top: 12px;",
            uiOutput(ns("download_buttons_container"))
          )
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────────
reconcile_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      unlocked = 1,
      detected_step = NULL,
      
      recon_map = NULL,
      rtrw_vect = NULL,
      rzwp3k_vect = NULL,
      rtrw_prioritas = NULL,
      rzwp3k_prioritas = NULL,
      serasi_matrix = NULL,
      
      template_path = NULL,
      
      resolved_rtrw = NULL,
      resolved_rzwp3k = NULL,
      resolved_integrated = NULL,
      
      display_rtrw = NULL,
      display_rzwp3k = NULL,
      display_integrated = NULL,
      
      final_log = NULL
    )
    
    go_to_panel <- function(value) accordion_panel_set(id = "wizard", values = value, session = session)
    
    have_results <- reactive({
      !is.null(rv$resolved_rtrw) || !is.null(rv$resolved_rzwp3k) || !is.null(rv$resolved_integrated)
    })
    
    # ── Step 1 UI ──────────────────────────────────────────────
    output$step1_ui <- renderUI({
      tagList(
        fileInput(ns("recon_map_file"), "Pilih Peta Rekomendasi (.gpkg)", accept = ".gpkg"),
        fileInput(ns("rtrw_file"), "Pilih Peta RTRW (.shp/.gpkg)", accept = c(".gpkg", ".shp", ".shx", ".dbf", ".prj"), multiple = TRUE),
        fileInput(ns("rzwp3k_file"), "Pilih Peta RZWP3K (.shp/.gpkg)", accept = c(".gpkg", ".shp", ".shx", ".dbf", ".prj"), multiple = TRUE),
        fileInput(ns("rtrw_priority_file"), "Tabel Acuan Pola RTRW (.xlsx)", accept = ".xlsx"),
        fileInput(ns("rzwp3k_priority_file"), "Tabel Acuan Pola RZWP3K (.xlsx)", accept = ".xlsx"),
        fileInput(ns("serasi_matrix_file"), "Matriks SERASI (.xlsx)", accept = ".xlsx"),
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap; margin-top: 10px;",
          actionButton(ns("btn_make_template"),
                       tagList(tags$i(class = "bi bi-file-earmark-spreadsheet me-1"), "Buat Templat"),
                       class = "btn-outline-primary btn-sm"),
          downloadButton(ns("dl_template"), "Unduh Templat", class = "btn-outline-success btn-sm")
        ),
        uiOutput(ns("template_status_ui")),
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Langkah 2")
      )
    })
    
    # Detect step
    observeEvent(input$recon_map_file, {
      req(input$recon_map_file)
      rv$template_path <- NULL
      rv$detected_step <- NULL
      rv$unlocked <- 1
      
      tryCatch({
        map_data <- sf::st_read(input$recon_map_file$datapath, quiet = TRUE)
        cols <- names(map_data)
        
        if (any(c("stat_pu", "id_rtrw", "id_rzwp3k") %in% cols)) {
          rv$detected_step <- 1
          rv$recon_map <- map_data
          showNotification("Peta Rekomendasi terdeteksi sebagai STEP 1 (Overlaps).", type = "message")
        } else if ("length" %in% cols) {
          rv$detected_step <- 2
          rv$recon_map <- map_data
          showNotification("Peta Rekomendasi terdeteksi sebagai STEP 2 (Adjacent).", type = "message")
        } else {
          rv$recon_map <- NULL
          stop("Kolom penanda struktural tidak ditemukan.")
        }
      }, error = function(e) {
        rv$recon_map <- NULL
        rv$detected_step <- NULL
        showNotification(paste("Gagal Memvalidasi Berkas:", e$message), type = "error", duration = NULL)
      })
    })
    
    # Load inputs
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
      rv$serasi_matrix <- load_validate_matrix_table(input$serasi_matrix_file$datapath, title = "serasi")
    })
    
    # Template Generation
    observeEvent(input$btn_make_template, {
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification("Direktori output belum diatur.", type = "error", duration = 5)
        return()
      }
      
      req(rv$recon_map, rv$detected_step, rv$rtrw_prioritas, rv$rzwp3k_prioritas)
      rv$template_path <- NULL
      
      withProgress(message = "Membuat Templat Rekonsiliasi", value = 0, {
        tryCatch({
          incProgress(0.2, detail = "Memuat tabel acuan pola...")
          out_dir_step <- file.path(output_dir(), paste0("step", rv$detected_step))
          dir.create(out_dir_step, recursive = TRUE, showWarnings = FALSE)
          
          file_name <- if (rv$detected_step == 1) "overlaps_reconcilliation_table.xlsx" else "adjacent_reconcilliation_table.xlsx"
          
          incProgress(0.5, detail = "Menjalankan pembuat templat...")
          generate_reconciliation_excel(
            recon_map        = rv$recon_map,
            rtrw_prioritas   = rv$rtrw_prioritas,
            rzwp3k_prioritas = rv$rzwp3k_prioritas,
            output_dir       = out_dir_step,
            step             = rv$detected_step,
            file_name        = file_name
          )
          
          incProgress(0.8, detail = "Verifikasi berkas templat...")
          generated_path <- file.path(out_dir_step, file_name)
          if (!file.exists(generated_path)) stop("File templat gagal dibuat.")
          
          rv$template_path <- generated_path
          showNotification("Templat Rekonsiliasi Berhasil Dibuat.", type = "message")
          incProgress(1.0, detail = "Selesai!")
        }, error = function(e) {
          showNotification(paste("Gagal membuat templat:", e$message), type = "error", duration = 10)
        })
      })
    })
    
    output$template_status_ui <- renderUI({
      req(rv$template_path)
      div(class = "alert alert-success mb-0 mt-2",
          tags$i(class = "bi bi-check-circle me-2"),
          sprintf("Templat Siap (%s): %s", paste0("Step ", rv$detected_step), basename(rv$template_path)))
    })
    
    output$dl_template <- downloadHandler(
      filename = function() {
        if (!is.null(rv$template_path)) basename(rv$template_path) else "reconcilliation_table.xlsx"
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
        showNotification("Harap unggah Peta RTRW dan RZWP3K (raw shapefile) terlebih dahulu.", type = "warning")
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
      if (is.null(rv$template_path)) {
        showNotification("Templat belum dibuat. Jika sudah memiliki tabel rekonsiliasi yang sudah diisi, Anda tetap dapat melanjutkan.", type = "message", duration = 5)
      }
      rv$unlocked <- max(rv$unlocked, 2)
      go_to_panel("step2")
    })
    
    # ── Step 2 UI ──────────────────────────────────────────────
    output$step2_ui <- renderUI({
      if (rv$unlocked < 2) return(.locked_panel())
      tagList(
        fileInput(ns("recon_table_filled_file"), "Unggah Tabel Keputusan Rekonsiliasi Berisi (.xlsx)", accept = ".xlsx"),
        if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
          div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
              tags$i(class = "bi bi-exclamation-triangle me-1"),
              "Direktori output belum diatur. Atur terlebih dahulu di menu utama.")
        },
        div(
          style = "margin-top: 10px;",
          actionButton(ns("btn_run_reconcile"),
                       tagList(tags$i(class = "bi bi-lightning-charge-fill me-1"), "Lakukan Rekonsiliasi"),
                       class = "btn-success btn-sm")
        ),
        .step_nav(ns, back_id = "btn_back_2", next_id = NULL)
      )
    })
    
    observeEvent(input$btn_back_2, go_to_panel("step1"))
    
    # ── Core Reconciliation Execution ──────────────────────────
    observeEvent(input$btn_run_reconcile, {
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification("Direktori output belum diatur.", type = "error", duration = 5)
        return()
      }
      req(input$recon_table_filled_file)
      req(rv$recon_map, rv$rtrw_vect, rv$rzwp3k_vect)
      req(rv$rtrw_prioritas, rv$rzwp3k_prioritas, rv$serasi_matrix)
      
      rv$resolved_rtrw <- NULL
      rv$resolved_rzwp3k <- NULL
      rv$resolved_integrated <- NULL
      rv$display_rtrw <- NULL
      rv$display_rzwp3k <- NULL
      rv$display_integrated <- NULL
      log_lines <- character(0)
      
      showNotification("Menjalankan proses rekonsiliasi...", type = "message", id = "recon_progress", duration = 10)
      
      withProgress(message = "Menjalankan Rekonsiliasi Spasial", value = 0, {
        tryCatch({
          if (rv$detected_step == 1) {
            # --- Step 1 ---
            incProgress(0.1, detail = "Membaca tabel keputusan...")
            recon_table <- load_and_validate_table(input$recon_table_filled_file$datapath) %>%
              select(id_pu, user_decision)
            
            incProgress(0.2, detail = "Menggabungkan dengan peta rekomendasi...")
            overlaps_map <- rv$recon_map %>%
              left_join(recon_table, by = "id_pu") %>%
              mutate(user_decision = user_decision)
            
            incProgress(0.4, detail = "Menjalankan rekonsiliasi Overlaps...")
            result <- reconciliation_step1(
              rtrw_base = rv$rtrw_vect,
              rzwp3k_base = rv$rzwp3k_vect,
              overlaps_map = overlaps_map,
              rtrw_priority = rv$rtrw_prioritas,
              rzwp3k_priority = rv$rzwp3k_prioritas,
              matriks_serasi = rv$serasi_matrix,
              alpha = 0.5
            )
            
            rv$resolved_rtrw <- result$rtrw
            rv$resolved_rzwp3k <- result$rzwp3k
            rv$resolved_integrated <- NULL
            
            rv$display_rtrw <- .prepare_map_display(
              result$rtrw, 
              layer_name = "RTRW", 
              category_col = "Reconcile",
              keep_cols = names(result$rtrw)
            )
            rv$display_rzwp3k <- .prepare_map_display(
              result$rzwp3k, 
              layer_name = "RZWP3K", 
              category_col = "Reconcile",
              keep_cols = names(result$rzwp3k)
            )
            rv$display_integrated <- NULL
            
            log_lines <- c(
              log_lines,
              "--- LOG REKONSILIASI KASUS STEP 1 (OVERLAPS) ---",
              sprintf("Jumlah Feature RTRW Hasil Resolusi: %d", nrow(result$rtrw)),
              sprintf("Jumlah Feature RZWP3K Hasil Resolusi: %d", nrow(result$rzwp3k)),
              sprintf("RTRW Reconcile=Yes: %d, No: %d", 
                      sum(result$rtrw$Reconcile == "Yes", na.rm = TRUE),
                      sum(result$rtrw$Reconcile == "No", na.rm = TRUE)),
              sprintf("RZWP3K Reconcile=Yes: %d, No: %d",
                      sum(result$rzwp3k$Reconcile == "Yes", na.rm = TRUE),
                      sum(result$rzwp3k$Reconcile == "No", na.rm = TRUE))
            )
            
          } else if (rv$detected_step == 2) {
            # --- Step 2 ---
            incProgress(0.2, detail = "Menjalankan rekonsiliasi Bertetangga...")
            result <- reconcilliation_step2(
              recon_table_path = input$recon_table_filled_file$datapath,
              adjacent_recom_map = rv$recon_map,
              rtrw_vect = rv$rtrw_vect,
              rzwp3k_vect = rv$rzwp3k_vect,
              matriks_serasi = rv$serasi_matrix,
              alpha = 0.5
            )
            
            rv$resolved_integrated <- result
            rv$resolved_rtrw <- NULL
            rv$resolved_rzwp3k <- NULL
            
            rv$display_integrated <- .prepare_map_display(
              result, 
              layer_name = "Integrated", 
              category_col = "Reconcile",
              keep_cols = names(result)
            )
            rv$display_rtrw <- NULL
            rv$display_rzwp3k <- NULL
            
            log_lines <- c(
              log_lines,
              "--- LOG REKONSILIASI KASUS STEP 2 (ADJACENT) ---",
              sprintf("Jumlah Feature Hasil Integrasi: %d", nrow(result)),
              sprintf("Reconcile=Yes: %d, No: %d",
                      sum(result$Reconcile == "Yes", na.rm = TRUE),
                      sum(result$Reconcile == "No", na.rm = TRUE))
            )
          } else {
            stop("Step tidak dikenali.")
          }
          
          incProgress(0.9, detail = "Menyelesaikan log...")
          rv$final_log <- paste(log_lines, collapse = "\n")
          showNotification("Proses Penyelesaian Konflik Peta Selesai.", type = "message")
          incProgress(1.0, detail = "Selesai!")
          
        }, error = function(e) {
          rv$final_log <- paste0("Error Runtime Execution:\n", e$message)
          showNotification(paste("Gagal melakukan rekonsiliasi:", e$message), type = "error", duration = NULL)
        })
      })
    })
    
    # ── Right Panel ─────────────────────────────────────────────
    output$status_box <- renderUI({
      if (have_results()) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Peta berhasil diperbaiki! Silakan periksa hasil visual spasial dan unduh gpkg.")
      } else if (!is.null(rv$final_log) && grepl("^Error", rv$final_log)) {
        div(class = "alert alert-danger mb-0",
            tags$i(class = "bi bi-exclamation-triangle-fill me-2"),
            "Gagal mengeksekusi rekonsiliasi. Lihat rincian log kesalahan.")
      } else {
        div(class = "alert alert-secondary mb-0", "Menunggu hasil rekonsiliasi...")
      }
    })
    
    output$validation_log <- renderText({
      if (!is.null(rv$final_log)) rv$final_log else "Sistem siap melakukan rekonsiliasi spasial."
    })
    
    # ── Table Results ───────────────────────────────────────────
    output$table_view_panel <- renderUI({
      if (!have_results()) {
        return(tags$p("Belum ada ringkasan spasial untuk dievaluasi."))
      }
      
      if (!is.null(rv$resolved_integrated)) {
        accordion(
          id = ns("table_summary_accordion"),
          multiple = TRUE,
          accordion_panel(
            title = "Pratinjau Data Hasil Integrasi (Step 2)",
            value = "integrated_tab",
            icon = tags$i(class = "bi bi-table"),
            div(style = "overflow-x: auto;", DT::DTOutput(ns("integrated_preview_render")))
          )
        )
      } else {
        accordion(
          id = ns("table_summary_accordion"),
          multiple = TRUE,
          accordion_panel(
            title = "Pratinjau Data Geometri RTRW Rekonsiliasi",
            value = "rtrw_tab",
            icon = tags$i(class = "bi bi-table"),
            div(style = "overflow-x: auto;", DT::DTOutput(ns("rtrw_preview_render")))
          ),
          accordion_panel(
            title = "Pratinjau Data Geometri RZWP3K Rekonsiliasi",
            value = "rzwp3k_tab",
            icon = tags$i(class = "bi bi-table"),
            div(style = "overflow-x: auto;", DT::DTOutput(ns("rzwp3k_preview_render")))
          )
        )
      }
    })
    
    # ── Helper to format and render reconciliation tables ──────────────
    format_reconcile_table <- function(df, type = c("rtrw", "rzwp3k", "integrated")) {
      type <- match.arg(type)
      
      base_map <- c(
        "fid"             = "FID",
        "id_pu"           = "ID PU",
        "Zoning_Old"      = "Kelas Lama",
        "Zoning_New"      = "Kelas Baru",
        "Reconcile"       = "Rekonsiliasi?",
        "idx_serasi"      = "Indeks SERASI",
        "idx_padu_final"  = "Indeks PADU",
        "idx_padan"       = "Indeks PADAN",
        "idx_serasi_new"  = "Indeks SERASI Baru",
        "idx_padan_new"   = "Indeks PADAN Baru",
        "delta_idx_padan" = "Selisih Indeks PADAN"
      )
      
      if (type %in% c("rtrw", "rzwp3k")) {
        extra_map <- c(
          "Overlap_Pair" = "Pasangan Tumpang Tindih",
          "Overlap"      = "Tumpang Tindih?"
        )
      } else {
        extra_map <- c(
          "Source"   = "Sumber",
          "Adjacent" = "Bertetangga?"
        )
      }
      
      col_map <- c(base_map, extra_map)
      existing <- intersect(names(df), names(col_map))
      names(df)[match(existing, names(df))] <- col_map[existing]
      
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      # Exclude ID columns from rounding
      numeric_cols <- setdiff(numeric_cols, c("FID", "fid", "id_pu"))
      
      DT::datatable(
        df,
        extensions = c('FixedColumns', 'FixedHeader'),
        options = list(
          pageLength   = 10,
          scrollX      = TRUE,
          scrollY      = "400px",
          dom          = 'Bfrtip',
          fixedColumns = list(leftColumns = 4),
          fixedHeader  = TRUE
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      ) %>%
        DT::formatRound(columns = numeric_cols, digits = 2)
    }
    
    # ── Table renderers ─────────────────────────
    
    output$rtrw_preview_render <- DT::renderDT({
      req(rv$resolved_rtrw)
      df <- sf::st_drop_geometry(rv$resolved_rtrw)
      format_reconcile_table(df, "rtrw")
    })
    
    output$rzwp3k_preview_render <- DT::renderDT({
      req(rv$resolved_rzwp3k)
      df <- sf::st_drop_geometry(rv$resolved_rzwp3k)
      format_reconcile_table(df, "rzwp3k")
    })
    
    output$integrated_preview_render <- DT::renderDT({
      req(rv$resolved_integrated)
      df <- sf::st_drop_geometry(rv$resolved_integrated)
      format_reconcile_table(df, "integrated")
    })
    
    # ── Leaflet Map ─────────────────────────────────────────────
    output$reconcile_map_view <- renderLeaflet({
      
      # Helper to dynamically build comprehensive popup from all columns
      make_popup <- function(sf_obj) {
        if (is.null(sf_obj) || nrow(sf_obj) == 0) return(NULL)
        df <- sf::st_drop_geometry(sf_obj)
        popups <- sapply(1:nrow(df), function(i) {
          row_data <- df[i, , drop = FALSE]
          lines <- sapply(names(row_data), function(col) {
            if (col %in% c("origin_layer", "search_label")) return(NULL)
            val <- row_data[[1, col]]
            if (length(val) == 0 || is.na(val) || is.null(val)) val <- "N/A"
            paste0("<b>", col, ":</b> ", as.character(val))
          })
          lines <- unlist(lines)
          paste(lines, collapse = "<br/>")
        })
        lapply(popups, htmltools::HTML)
      }
      
      make_label <- function(sf_obj) {
        if (is.null(sf_obj) || nrow(sf_obj) == 0) return(NULL)
        df <- sf::st_drop_geometry(sf_obj)
        z_old <- if ("Zoning_Old" %in% names(df)) ifelse(is.na(df$Zoning_Old), "N/A", df$Zoning_Old) else "N/A"
        z_new <- if ("Zoning_New" %in% names(df)) ifelse(is.na(df$Zoning_New), "N/A", df$Zoning_New) else "N/A"
        labels <- paste0("<strong>", z_old, " ➜ ", z_new, "</strong>")
        lapply(labels, htmltools::HTML)
      }
      
      # Helper to prepare search label column
      add_search_col <- function(sf_obj) {
        if (is.null(sf_obj) || nrow(sf_obj) == 0) return(sf_obj)
        df <- sf::st_drop_geometry(sf_obj)
        id_val <- if ("id_pu" %in% names(df)) df$id_pu else ifelse("fid" %in% names(df), df$fid, "N/A")
        z_old  <- if ("Zoning_Old" %in% names(df)) ifelse(is.na(df$Zoning_Old), "N/A", df$Zoning_Old) else "N/A"
        z_new  <- if ("Zoning_New" %in% names(df)) ifelse(is.na(df$Zoning_New), "N/A", df$Zoning_New) else "N/A"
        
        sf_obj$search_label <- paste0("ID PU: ", id_val, " | ", z_old, " ➜ ", z_new)
        sf_obj
      }
      
      # Color palette: "No" maps to Gray, "Yes" maps to Blue 
      pal_reconcile <- colorFactor(
        palette = c("#BDBDBD", "#2B8CBE"), 
        domain = c("No", "Yes"),
        na.color = "#808080"
      )
      
      # Step 1: Overlaps processing
      if (!is.null(rv$display_rtrw) && nrow(rv$display_rtrw) > 0 ||
          !is.null(rv$display_rzwp3k) && nrow(rv$display_rzwp3k) > 0) {
        
        m <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
        available_groups <- character(0)
        
        if (!is.null(rv$display_rtrw) && nrow(rv$display_rtrw) > 0) {
          rtrw_map <- add_search_col(.to_leaflet_crs(rv$display_rtrw))
          popup_rtrw <- make_popup(rtrw_map)
          label_rtrw <- make_label(rtrw_map)
          m <- m %>% addPolygons(
            data = rtrw_map,
            group = "RTRW Reconciled",
            fillColor = ~pal_reconcile(Reconcile),
            fillOpacity = 0.7,
            weight = 1,
            color = "#333333",
            label = ~search_label,
            popup = popup_rtrw,
            highlightOptions = highlightOptions(weight = 3, color = "#0056b3", fillOpacity = 0.8, bringToFront = TRUE)
          )
          available_groups <- c(available_groups, "RTRW Reconciled")
        }
        
        if (!is.null(rv$display_rzwp3k) && nrow(rv$display_rzwp3k) > 0) {
          rzwp3k_map <- add_search_col(.to_leaflet_crs(rv$display_rzwp3k))
          popup_rzwp3k <- make_popup(rzwp3k_map)
          label_rzwp3k <- make_label(rzwp3k_map)
          m <- m %>% addPolygons(
            data = rzwp3k_map,
            group = "RZWP3K Reconciled",
            fillColor = ~pal_reconcile(Reconcile),
            fillOpacity = 0.7,
            weight = 1,
            color = "#333333",
            label = ~search_label,
            popup = popup_rzwp3k,
            highlightOptions = highlightOptions(weight = 3, color = "#0056b3", fillOpacity = 0.8, bringToFront = TRUE)
          )
          available_groups <- c(available_groups, "RZWP3K Reconciled")
        }
        
        if (length(available_groups) == 0) {
          return(leaflet() %>% addControl("Tidak ada data untuk ditampilkan.", position = "topright"))
        }
        
        m %>%
          addLayersControl(
            overlayGroups = available_groups,
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          leaflet.extras::addSearchFeatures(
            targetGroups = available_groups,
            options = leaflet.extras::searchFeaturesOptions(
              propertyName = "label",
              zoom = 15,
              openPopup = TRUE,
              position = "topleft",
              firstTipSubmit = TRUE,
              autoCollapse = FALSE,
              hideMarkerOnCollapse = TRUE
            )
          ) %>% leaflet.extras::addResetMapButton() %>% 
          addLegend(
            position = "bottomright",
            colors = c("#2B8CBE", "#BDBDBD"),
            labels = c("Reconcile = Yes", "Reconcile = No"),
            title = "Status Rekonsiliasi",
            opacity = 0.7
          )
        
      } else if (!is.null(rv$display_integrated) && nrow(rv$display_integrated) > 0) {
        # Step 2: Adjacent processing
        map_sf <- add_search_col(.to_leaflet_crs(rv$display_integrated))
        popup_int <- make_popup(map_sf)
        label_int <- make_label(map_sf)
        
        leaflet(map_sf) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(
            group = "Integrated Reconciled",
            fillColor = ~pal_reconcile(Reconcile),
            fillOpacity = 0.7,
            weight = 1,
            color = "#333333",
            label = ~search_label,
            popup = popup_int,
            highlightOptions = highlightOptions(weight = 3, color = "#0056b3", fillOpacity = 0.8, bringToFront = TRUE)
          ) %>%
          leaflet.extras::addSearchFeatures(
            targetGroups = "Integrated Reconciled",
            options = leaflet.extras::searchFeaturesOptions(
              propertyName = "label",
              zoom = 15,
              openPopup = TRUE,
              position = "topleft",
              firstTipSubmit = TRUE,
              autoCollapse = FALSE,
              hideMarkerOnCollapse = TRUE
            )
          ) %>% leaflet.extras::addResetMapButton() %>% 
          addLegend(
            position = "bottomright",
            colors = c("#2B8CBE", "#BDBDBD"), 
            labels = c("Reconcile = Yes", "Reconcile = No"),
            title = "Status Rekonsiliasi",
            opacity = 0.7
          )
        
      } else {
        leaflet() %>% addControl("Belum ada hasil rekonsiliasi atau data kosong.", position = "topright")
      }
    })
    
    # ── Download Buttons ────────────────────────────────────────
    output$download_buttons_container <- renderUI({
      if (!have_results()) return(NULL)
      
      if (!is.null(rv$resolved_integrated)) {
        # Step 2: one button
        downloadButton(ns("dl_integrated_final"), "Unduh Hasil Integrasi (.gpkg)", class = "btn-outline-primary btn-sm")
      } else {
        # Step 1: two buttons
        tagList(
          downloadButton(ns("dl_rtrw_final"), "Unduh Hasil RTRW (.gpkg)", class = "btn-outline-primary btn-sm"),
          downloadButton(ns("dl_rzwp3k_final"), "Unduh Hasil RZWP3K (.gpkg)", class = "btn-outline-info btn-sm")
        )
      }
    })
    
    output$dl_rtrw_final <- downloadHandler(
      filename = function() { "ST_RTRW_Resolved_Overlaps.gpkg" },
      content = function(file) {
        req(rv$resolved_rtrw)
        sf::st_write(rv$resolved_rtrw, file, delete_dsn = TRUE, quiet = TRUE)
      }
    )
    output$dl_rzwp3k_final <- downloadHandler(
      filename = function() { "ST_RZWP3K_Resolved_Overlaps.gpkg" },
      content = function(file) {
        req(rv$resolved_rzwp3k)
        sf::st_write(rv$resolved_rzwp3k, file, delete_dsn = TRUE, quiet = TRUE)
      }
    )
    output$dl_integrated_final <- downloadHandler(
      filename = function() { "ST_Integrated_Resolved_Adjacent.gpkg" },
      content = function(file) {
        req(rv$resolved_integrated)
        sf::st_write(rv$resolved_integrated, file, delete_dsn = TRUE, quiet = TRUE)
      }
    )
  })
}