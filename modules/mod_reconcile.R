# ui/modules/mod_reconcile.R
# ============================================================
#  MODULE: Reconcile (RTRW & RZWP3K)
#  Uses the standardized create_result_ui() / render_result_server().
#  Exports GPKG, XLSX, template, PNG map, and RDA log to the
#  module's folder under output_dir ("Rekonsiliasi").
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# Config
.RECON_MODULE_FOLDER <- "Rekonsiliasi"
.RECON_RDA           <- "log/idx_reconcile_log.rda"
.RECON_PNG_DIR       <- "log"

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
          create_result_ui(ns)
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
      
      # inputs
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
      
      analysis_result = NULL,
      gpkg_path       = NULL,
      xlsx_path       = NULL,
      log_messages    = "",
      
      final_log = NULL
    )
    
    go_to_panel <- function(value) accordion_panel_set(id = "wizard", values = value, session = session)
    
    have_results <- reactive({
      !is.null(rv$analysis_result)
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
    
    # Detect step from uploaded GPKG
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
    
    # Load other inputs
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
          incProgress(0.2, detail = "Menyiapkan direktori modul...")
          module_dir <- file.path(output_dir(), .RECON_MODULE_FOLDER)
          dir.create(module_dir, recursive = TRUE, showWarnings = FALSE)
          
          file_name <- if (rv$detected_step == 1) {
            "overlaps_reconcilliation_table.xlsx"
          } else {
            "adjacent_reconcilliation_table.xlsx"
          }
          
          incProgress(0.5, detail = "Menjalankan pembuat templat...")
          generate_reconciliation_excel(
            recon_map        = rv$recon_map,
            rtrw_prioritas   = rv$rtrw_prioritas,
            rzwp3k_prioritas = rv$rzwp3k_prioritas,
            output_dir       = module_dir,
            step             = rv$detected_step,
            file_name        = file_name
          )
          
          incProgress(0.8, detail = "Verifikasi berkas templat...")
          generated_path <- file.path(module_dir, file_name)
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
        showNotification(
          "Templat belum dibuat. Jika sudah memiliki tabel rekonsiliasi yang sudah diisi, Anda tetap dapat melanjutkan.",
          type = "message", duration = 5
        )
      }
      rv$unlocked <- max(rv$unlocked, 2)
      go_to_panel("step2")
    })
    
    # ── Step 2 UI ──────────────────────────────────────────────
    output$step2_ui <- renderUI({
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
      rv$analysis_result <- NULL
      rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL
      rv$log_messages <- ""
      log_lines <- character(0)
      
      showNotification("Menjalankan proses rekonsiliasi...", type = "message", id = "recon_progress", duration = 10)
      
      withProgress(message = "Menjalankan Rekonsiliasi Spasial", value = 0, {
        tryCatch({
          module_dir <- file.path(output_dir(), .RECON_MODULE_FOLDER)
          log_dir    <- file.path(module_dir, .RECON_PNG_DIR)
          dir.create(module_dir, recursive = TRUE, showWarnings = FALSE)
          dir.create(log_dir,    recursive = TRUE, showWarnings = FALSE)
          
          if (rv$detected_step == 1) {
            # ── Step 1 (Overlaps) ─────────────────────────────
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
            
            rtrw_combined   <- result$rtrw   %>% dplyr::mutate(Source = "RTRW")
            rzwp3k_combined <- result$rzwp3k %>% dplyr::mutate(Source = "RZWP3K")
            combined <- dplyr::bind_rows(rtrw_combined, rzwp3k_combined)
            
          } else if (rv$detected_step == 2) {
            # ── Step 2 (Adjacent) ─────────────────────────────
            incProgress(0.1, detail = "Membaca tabel keputusan...")
            recon_table_filled <- load_and_validate_table(input$recon_table_filled_file$datapath)
            
            is_dissolved <- all(c("id_rtrw", "id_rzwp3k") %in% names(recon_table_filled)) &&
              !"id" %in% names(recon_table_filled)
            
            if (is_dissolved) {
              incProgress(0.15, detail = "Mengubah tabel ke bentuk fitur...")
              recon_table_filled <- undissolve_adjacent_pairs(
                recon_table_filled,
                decisions_rtrw_col   = "user_decision_rtrw",
                decisions_rzwp3k_col = "user_decision_rzwp3k"
              )
            }
            
            incProgress(0.2, detail = "Menjalankan rekonsiliasi Bertetangga...")
            result <- reconcilliation_step2(
              recon_table_path   = recon_table_filled,  
              adjacent_recom_map = rv$recon_map,
              rtrw_vect          = rv$rtrw_vect,
              rzwp3k_vect        = rv$rzwp3k_vect,
              matriks_serasi     = rv$serasi_matrix,
              alpha              = 0.5
            )
            
            rv$resolved_integrated <- result
            rv$resolved_rtrw <- NULL
            rv$resolved_rzwp3k <- NULL
            
            log_lines <- c(
              log_lines,
              "--- LOG REKONSILIASI KASUS STEP 2 (ADJACENT) ---",
              sprintf("Jumlah Feature Hasil Integrasi: %d", nrow(result)),
              sprintf("Reconcile=Yes: %d, No: %d",
                      sum(result$Reconcile == "Yes", na.rm = TRUE),
                      sum(result$Reconcile == "No", na.rm = TRUE))
            )
            
            combined <- result
          } else {
            stop("Step tidak dikenali.")
          }
          
          incProgress(0.6, detail = "Menyiapkan hasil untuk ditampilkan...")
          
          if (!"Source" %in% names(combined)) {
            combined$Source <- "Integrated"
          }
          
          # Unique display id
          combined <- combined %>%
            dplyr::mutate(
              original_id_pu = as.character(id_pu),
              id_pu          = paste0(Source, "_", dplyr::row_number())
            )
          
          combined <- tryCatch(sf::st_make_valid(combined), error = function(e) combined)
          
          incProgress(0.7, detail = "Menyimpan hasil ke disk...")
          
          case_suffix <- if (rv$detected_step == 1) "overlaps" else "adjacent"
          base_name   <- sprintf("rtrwp_terintegrasi_%s", case_suffix)
          
          stale_case <- if (rv$detected_step == 1) "adjacent" else "overlaps"
          stale_files <- file.path(
            module_dir,
            sprintf("rtrwp_terintegrasi_%s.%s", stale_case, c("gpkg", "xlsx"))
          )
          stale_files <- stale_files[file.exists(stale_files)]
          if (length(stale_files) > 0) file.remove(stale_files)
          
          out_gpkg <- file.path(module_dir, paste0(base_name, ".gpkg"))
          sf::st_write(combined, out_gpkg, delete_dsn = TRUE, quiet = TRUE)
          
          out_xlsx <- file.path(module_dir, paste0(base_name, ".xlsx"))
          openxlsx::write.xlsx(sf::st_drop_geometry(combined), out_xlsx)
          
          tryCatch({
            plot_categorical_map(
              map      = combined,
              title    = "Peta Status Rekonsiliasi",
              column   = "Reconcile",
              legend   = "Status Rekonsiliasi",
              filepath = file.path(log_dir, sprintf("reconcile_map_%s.png", case_suffix))
            )
          }, error = function(e) {
            warning("Gagal membuat PNG peta rekonsiliasi: ", e$message)
          })
          
          incProgress(0.9, detail = "Menyimpan log...")
          rv$final_log    <- paste(log_lines, collapse = "\n")
          rv$log_messages <- rv$final_log
          
          out <- list(
            inputs = list(
              start_time         = Sys.time(),
              recon_step         = rv$detected_step,
              recon_table_filled = input$recon_table_filled_file$name,
              recon_map_file     = input$recon_map_file$name,
              output_dir         = output_dir()
            ),
            result = list(
              idx_reconcile_map   = combined,
              idx_reconcile_table = sf::st_drop_geometry(combined),
              resolved_rtrw        = rv$resolved_rtrw,
              resolved_rzwp3k      = rv$resolved_rzwp3k,
              resolved_integrated  = rv$resolved_integrated
            )
          )
          
          log_path <- file.path(module_dir, .RECON_RDA)
          tryCatch({
            inputs <- out$inputs
            save(inputs, file = log_path)
          }, error = function(e) warning("Gagal menulis file log: ", e$message))
          
          session$userData$module_results$reconcile <- out
          
          rv$analysis_result <- list(
            map   = combined,
            table = sf::st_drop_geometry(combined)
          )
          rv$gpkg_path <- out_gpkg
          rv$xlsx_path <- out_xlsx
          
          showNotification("Proses Penyelesaian Konflik Peta Selesai.", type = "message")
          incProgress(1.0, detail = "Selesai!")
          
        }, error = function(e) {
          rv$final_log    <- paste0("Error Runtime Execution:\n", e$message)
          rv$log_messages <- rv$final_log
          showNotification(paste("Gagal melakukan rekonsiliasi:", e$message), type = "error", duration = NULL)
        })
      })
    })
    
    # ── Status box ─────────────────────────────────────────────
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
    
    # ── Shared result server ────────────────────────────────────
    reconcile_config <- list(
      map_color_col  = "Reconcile",
      map_title      = "Status Rekonsiliasi",
      map_palette    = c("#BDBDBD", "#2B8CBE"),
      map_label_cols = list(
        "ID PU"         = "original_id_pu",
        "Sumber"        = "Source",
        "Kelas Lama"    = "Zoning_Old",
        "Kelas Baru"    = "Zoning_New",
        "Rekonsiliasi"  = "Reconcile",
        "Indeks SERASI" = "idx_serasi",
        "Indeks PADAN"  = "idx_padan",
        "Selisih PADAN" = "delta_idx_padan"
      ),
      table_cols = c(
        "original_id_pu"   = "ID PU",
        "Source"           = "Sumber",
        "Zoning_Old"       = "Kelas Lama",
        "Zoning_New"       = "Kelas Baru",
        "Reconcile"        = "Rekonsiliasi?",
        "Overlap"          = "Tumpang Tindih?",
        "Overlap_Pair"     = "Pasangan Tumpang Tindih",
        "Adjacent"         = "Bertetangga?",
        "idx_serasi"       = "Indeks SERASI",
        "idx_padu_final"   = "Indeks PADU",
        "idx_padan"        = "Indeks PADAN",
        "idx_serasi_new"   = "Indeks SERASI Baru",
        "idx_padan_new"    = "Indeks PADAN Baru",
        "delta_idx_padan"  = "Selisih Indeks PADAN"
      ),
      table_round_cols = c(
        "Indeks SERASI",
        "Indeks PADU",
        "Indeks PADAN",
        "Indeks SERASI Baru",
        "Indeks PADAN Baru",
        "Selisih Indeks PADAN"
      )
    )
    
    render_result_server(input, output, session, rv, reconcile_config)
  })
}