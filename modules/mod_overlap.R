# ui/modules/mod_overlap.R
# ============================================================
#  MODULE: Overlap (1.1 Type 1: Overlap)
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

# ── UI ──────────────────────────────────────────────────────────
overlap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("1.1 Analisis SERASI Area Tumpang Tindih", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Mengidentifikasi kasus area tumpang tindih secara spasial antara kawasan/zona peta RTRW dan RZWP3K serta menghitung indeks SERASI.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),
    
    fluidRow(
      class = "g-3",
      
      # ── Left column: Wizard (1/3) ─────────────────────────────
      column(
        width = 4,
        card(
          card_header("Input & Parameter"),
          accordion(
            id = ns("wizard"),
            open = "step1",
            multiple = FALSE,
            
            accordion_panel(
              title = "Langkah 1 — Menyiapkan Data Utama",
              value = "step1",
              icon = tags$i(class = "bi bi-folder-fill"),
              uiOutput(ns("step1_ui"))
            ),
            
            accordion_panel(
              title = "Langkah 2 — Menentukan Kompabilitas",
              value = "step2",
              icon = tags$i(class = "bi bi-diagram-3-fill"),
              uiOutput(ns("step2_ui"))
            )
          )
        )
      ),
      
      # ── Right column: Output & Hasil (2/3) ────────────────────
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
overlap_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Reactive values ──────────────────────────────────────────
    rv <- reactiveValues(
      unlocked = 1,                # 1 = only step1, 2 = step2 unlocked
      
      # step1 data
      rtrw_vect = NULL,
      rzwp3k_vect = NULL,
      rtrw_prioritas = NULL,
      rzwp3k_prioritas = NULL,
      
      # NEW: administrative data
      admin_vect = NULL,
      admin_col = NULL,
      
      # step2 data
      matriks_serasi = NULL,
      threshold_ha = 156.25,
      
      # analysis results
      analysis_result = NULL,
      gpkg_path = NULL,
      xlsx_path = NULL,
      log_messages = ""
    )
    
    go_to_panel <- function(value) {
      accordion_panel_set(id = "wizard", values = value, session = session)
    }
    
    # ── Helpers for shapefile loading ──────────────────────────
    extract_shp_path <- function(file_input) {
      shp_row <- file_input[grepl("\\.shp$", file_input$name, ignore.case = TRUE), ]
      validate(need(
        nrow(shp_row) == 1,
        "Harap unggah semua komponen shapefile (.shp, .dbf, .prj, .shx)"
      ))
      stem <- tools::file_path_sans_ext(shp_row$datapath)
      for (i in seq_len(nrow(file_input))) {
        ext <- tools::file_ext(file_input$name[i])
        file.rename(file_input$datapath[i], paste0(stem, ".", ext))
      }
      paste0(stem, ".shp")
    }
    
    # ── Step 1 UI ──────────────────────────────────────────────
    output$step1_ui <- renderUI({
      tagList(
        tags$p(tags$i(class = "bi bi-map me-1"), "Peta RTRW (.shp)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "Unggah semua komponen shapefile RTRW (.shp, .dbf, .prj, .shx)."),
        fileInput(ns("rtrw_file"), label = NULL,
                  accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        
        tags$p(tags$i(class = "bi bi-map me-1"), "Peta RZWP3K (.shp)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "Unggah semua komponen shapefile RZWP3K (.shp, .dbf, .prj, .shx)."),
        fileInput(ns("rzwp3k_file"), label = NULL,
                  accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        
        # NEW: Administrative map input (optional)
        tags$p(tags$i(class = "bi bi-map me-1"), "Peta Administratif (.shp) (Opsional)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "Unggah shapefile batas administratif untuk menggabungkan hasil analisis per wilayah. (Kosongkan jika tidak diperlukan)"),
        fileInput(ns("admin_file"), label = NULL,
                  accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        
        # Dynamic dropdown for admin column
        uiOutput(ns("admin_field_ui")),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-table me-1"), "Tabel Acuan Pola RTRW (.xlsx)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        fileInput(ns("rtrw_prioritas_file"), label = NULL, accept = ".xlsx"),
        
        tags$p(tags$i(class = "bi bi-table me-1"), "Tabel Acuan Pola RZWP3K (.xlsx)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        fileInput(ns("rzwp3k_prioritas_file"), label = NULL, accept = ".xlsx"),
        
        hr(),
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_generate_matrix"),
                       tagList(tags$i(class = "bi bi-file-earmark-excel me-1"),
                               "Buat Templat Matriks SERASI"),
                       class = "btn-outline-primary btn-sm"),
          downloadButton(ns("dl_matrix_template"), "Unduh Matriks",
                         class = "btn-outline-success btn-sm")
        ),
        uiOutput(ns("matrix_template_status")),
        
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Langkah 2")
      )
    })
    
    # ── Load shapefiles and tables ─────────────────────────────
    observeEvent(input$rtrw_file, {
      req(input$rtrw_file)
      tryCatch({
        rv$rtrw_vect <- load_and_validate_shapefile(extract_shp_path(input$rtrw_file))
        showNotification("Peta RTRW berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$rtrw_vect <- NULL
        showNotification(paste("Gagal memuat RTRW:", e$message), type = "error")
      })
    })
    
    observeEvent(input$rzwp3k_file, {
      req(input$rzwp3k_file)
      tryCatch({
        rv$rzwp3k_vect <- load_and_validate_shapefile(extract_shp_path(input$rzwp3k_file))
        showNotification("Peta RZWP3K berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$rzwp3k_vect <- NULL
        showNotification(paste("Gagal memuat RZWP3K:", e$message), type = "error")
      })
    })
    
    # Load administrative shapefile and populate field dropdown
    observeEvent(input$admin_file, {
      req(input$admin_file)
      tryCatch({
        shp_path <- extract_shp_path(input$admin_file)
        admin_sf <- load_and_validate_shapefile(shp_path)
        admin_sf <- ensure_geometry_name(admin_sf)   
        rv$admin_vect <- admin_sf
        
        # Extract column names (drop geometry)
        col_names <- names(admin_sf)
        col_names <- col_names[!col_names %in% c("geometry", "geom")]
        
        # Render the dropdown for admin field selection
        output$admin_field_ui <- renderUI({
          req(rv$admin_vect)
          selectInput(
            ns("admin_field"),
            label = "Pilih kolom identitas wilayah administratif",
            choices = col_names,
            selected = if (!is.null(rv$admin_col) && rv$admin_col %in% col_names) rv$admin_col else col_names[1]
          )
        })
        
        showNotification("Peta Administratif berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$admin_vect <- NULL
        output$admin_field_ui <- renderUI(NULL)
        showNotification(paste("Gagal memuat Peta Administratif:", e$message), type = "error")
      })
    })
    
    # Store selected admin column
    observeEvent(input$admin_field, {
      rv$admin_col <- input$admin_field
    })
    
    observeEvent(input$rtrw_prioritas_file, {
      req(input$rtrw_prioritas_file)
      tryCatch({
        rv$rtrw_prioritas <- load_and_validate_table(input$rtrw_prioritas_file$datapath)
        showNotification("Prioritas RTRW berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$rtrw_prioritas <- NULL
        showNotification(paste("Gagal memuat prioritas RTRW:", e$message), type = "error")
      })
    })
    
    observeEvent(input$rzwp3k_prioritas_file, {
      req(input$rzwp3k_prioritas_file)
      tryCatch({
        rv$rzwp3k_prioritas <- load_and_validate_table(input$rzwp3k_prioritas_file$datapath)
        showNotification("Prioritas RZWP3K berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$rzwp3k_prioritas <- NULL
        showNotification(paste("Gagal memuat prioritas RZWP3K:", e$message), type = "error")
      })
    })
    
    # ── Generate matrix template ───────────────────────────────
    matrix_template_path <- reactiveVal(NULL)
    
    observeEvent(input$btn_generate_matrix, {
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification("Direktori output belum diatur...", type = "error", duration = 5)
        return()
      }
      
      req(rv$rtrw_vect, rv$rzwp3k_vect)
      tryCatch({
        out_path <- file.path(output_dir(), "matriks_serasi.xlsx")
        dir.create(output_dir(), recursive = TRUE, showWarnings = FALSE)
        
        # Writes the styled file and returns invisibly
        generate_matrix_serasi(rv$rtrw_vect, rv$rzwp3k_vect, file_path = out_path)
        
        matrix_template_path(out_path)
        showNotification(paste("Template matriks dibuat →", out_path), type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Gagal membuat template matriks:", e$message), type = "error", duration = 8)
      })
    })
    
    output$matrix_template_status <- renderUI({
      req(matrix_template_path())
      div(class = "alert alert-success mb-0",
          tags$i(class = "bi bi-check-circle me-2"),
          "Template siap diunduh.")
    })
    
    output$dl_matrix_template <- downloadHandler(
      filename = function() "matriks_serasi.xlsx",
      content = function(file) {
        req(matrix_template_path())
        file.copy(matrix_template_path(), file, overwrite = TRUE)
      }
    )
    
    # ── Step 1 -> Step 2 ──────────────────────────────────────
    observeEvent(input$btn_next_1, {
      # Validate that all required files are uploaded
      if (is.null(rv$rtrw_vect) || is.null(rv$rzwp3k_vect) ||
          is.null(rv$rtrw_prioritas) || is.null(rv$rzwp3k_prioritas)) {
        showNotification("Harap unggah semua data utama (peta dan prioritas) sebelum melanjutkan.",
                         type = "warning", duration = 8)
        return()
      }
      rv$unlocked <- max(rv$unlocked, 2)
      go_to_panel("step2")
    })
    
    # ── Step 2 UI ──────────────────────────────────────────────
    output$step2_ui <- renderUI({
      if (rv$unlocked < 2) return(.locked_panel())
      
      tagList(
        tags$p(tags$i(class = "bi bi-table me-1"), "Tabel Matriks SERASI (.xlsx)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        fileInput(ns("matriks_serasi_file"), label = NULL, accept = ".xlsx"),
        uiOutput(ns("matriks_serasi_status")),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-sliders me-1"), "Parameter",
               style = "font-weight: 600; margin-bottom: 4px;"),
        numericInput(ns("threshold_ha"),
                     "Ambang Batas Luas Minimum (ha)",
                     value = 156.25, min = 0),
        
        hr(),
        
        # Check output directory
        if (is.null(output_dir()) || !nzchar(output_dir())) {
          div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
              tags$i(class = "bi bi-exclamation-triangle me-1"),
              "Direktori output belum diatur. Atur terlebih dahulu di menu utama.")
        },
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_run"),
                       tagList(tags$i(class = "bi bi-play-fill me-1"),
                               "Jalankan Analisis"),
                       class = "btn-success btn-sm")
        ),
        
        .step_nav(ns, back_id = "btn_back_2", next_id = NULL)
      )
    })
    
    # ── Load matrix in step2 ──────────────────────────────────
    observeEvent(input$matriks_serasi_file, {
      req(input$matriks_serasi_file)
      tryCatch({
        rv$matriks_serasi <- load_validate_matrix_table(
          input$matriks_serasi_file$datapath, title = "serasi"
        )
        showNotification("Matriks SERASI berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$matriks_serasi <- NULL
        showNotification(paste("Gagal memuat matriks:", e$message), type = "error")
      })
    })
    
    output$matriks_serasi_status <- renderUI({
      if (is.null(rv$matriks_serasi)) return(NULL)
      div(class = "alert alert-success mb-0",
          tags$i(class = "bi bi-check-circle me-2"),
          "Matriks SERASI berhasil divalidasi.")
    })
    
    observeEvent(input$btn_back_2, {
      go_to_panel("step1")
    })
    
    # ── Run analysis (with progress) ──────────────────────────
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
      
      req(rv$rtrw_vect, rv$rzwp3k_vect,
          rv$rtrw_prioritas, rv$rzwp3k_prioritas,
          rv$matriks_serasi)
      
      # Reset previous results
      rv$analysis_result <- NULL
      rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL
      rv$log_messages <- ""
      
      append_log <- function(msg) {
        rv$log_messages <- paste0(rv$log_messages, msg, "\n")
      }
      
      withProgress(message = "Menjalankan Analisis Overlap", value = 0, {
        
        tryCatch({
          
          # Step 1: load already loaded, skip
          incProgress(0.1, detail = "Memulai analisis...")
          append_log(">> Memulai analisis overlap...")
          
          # Step 2: Identify overlaps (progress 30%)
          incProgress(0.2, detail = "Mengidentifikasi tumpang tindih...")
          append_log(">> Mengidentifikasi tumpang tindih antara RTRW dan RZWP3K...")
          union_sf <- identify_overlaps(rv$rtrw_vect, rv$rzwp3k_vect)
          append_log("   Tumpang tindih berhasil diidentifikasi.")
          
          # Step 3: Filter by threshold (progress 50%)
          incProgress(0.2, detail = "Menyaring berdasarkan luas minimum...")
          threshold <- input$threshold_ha
          append_log(paste0(">> Menyaring poligon dengan luas >= ", threshold, " ha..."))
          filtered_union_sf <- filter_overlaps(union_sf, threshold)
          append_log(paste0("   ", nrow(filtered_union_sf), " poligon tersisa setelah penyaringan."))
          
          # Step 4: Validate zone class (progress 70%)
          incProgress(0.2, detail = "Memvalidasi kesesuaian kelas zona...")
          append_log(">> Memvalidasi kesesuaian nama kelas antara peta dan prioritas...")
          valid_class <- validate_zone_class(
            filtered_union_sf, rv$rtrw_prioritas, rv$rzwp3k_prioritas
          )
          
          # Step 5: Merge and save (progress 90%)
          incProgress(0.2, detail = "Menggabungkan dan menyimpan hasil...")
          if (length(valid_class$mismatch_col3) == 0 &&
              length(valid_class$mismatch_col4) == 0) {
            
            append_log("   Semua nama kelas cocok. Menggabungkan indeks SERASI...")
            idx_serasi_map <- merge_attributes_to_map(filtered_union_sf, rv$matriks_serasi)
            
            # Merge with administrative map if provided ──
            if (!is.null(rv$admin_vect) && !is.null(rv$admin_col) && nzchar(rv$admin_col)) {
              append_log(">> Menggabungkan hasil dengan peta administratif...")
              admin_sf <- rv$admin_vect
              if (sf::st_crs(admin_sf) != sf::st_crs(idx_serasi_map)) {
                admin_sf <- sf::st_transform(admin_sf, sf::st_crs(idx_serasi_map))
              }
              idx_serasi_map <- sf::st_join(
                idx_serasi_map,
                admin_sf[, rv$admin_col, drop = FALSE],
                join = sf::st_intersects,
                largest = TRUE
              )
              names(idx_serasi_map)[names(idx_serasi_map) == rv$admin_col] <- "admin"
              append_log("   Penggabungan administratif selesai.")
            }
            
            # Prepare table and save
            idx_serasi_table <- as_tibble(idx_serasi_map %>% sf::st_drop_geometry())
            
            gpkg_path <- file.path(output_dir(), "idx_serasi_overlaps.gpkg")
            xlsx_path <- file.path(output_dir(), "idx_serasi_overlaps.xlsx")
            dir.create(output_dir(), recursive = TRUE, showWarnings = FALSE)
            
            sf::st_write(idx_serasi_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
            openxlsx::write.xlsx(idx_serasi_table, xlsx_path)
            
            rv$gpkg_path <- gpkg_path
            rv$xlsx_path <- xlsx_path
            rv$analysis_result <- list(map = idx_serasi_map, table = idx_serasi_table)
            
            # ─── Store result for report generation ───
            out <- list(
              inputs = list(
                rtrw_path = input$rtrw_file,
                rzwp3k_path = input$rzwp3k_file,
                admin_path = input$admin_file,
                rtrw_prioritas_path = input$rtrw_prioritas_file,
                rzwp3k_prioritas_path = input$rzwp3k_prioritas_file,
                matriks_serasi_path = input$matriks_serasi_file,
                output_dir = output_dir()
              ),
              result = list(
                rtrw_vect = rv$rtrw_vect,
                rzwp3k_vect = rv$rzwp3k_vect,
                matriks_serasi = rv$matriks_serasi,
                rtrw_prioritas = rv$rtrw_prioritas,
                rzwp3k_prioritas = rv$rzwp3k_prioritas,
                idx_serasi_map = idx_serasi_map,
                idx_serasi_table = idx_serasi_table
              )
            )
            
            # Store in shared environment
            session$userData$module_results$overlap <- out
            
            append_log(paste0("   Hasil disimpan di: ", gpkg_path))
            append_log("Analisis overlap berhasil diselesaikan.")
            showNotification(paste("Analisis selesai. Hasil disimpan ke", gpkg_path),
                             type = "message", duration = 5)
            
          } else {
            log_msg <- paste(
              "Ketidakcocokan nama kelas terdeteksi:",
              if (length(valid_class$mismatch_col3) > 0)
                paste("  Ketidakcocokan col3:",
                      paste(valid_class$mismatch_col3, collapse = ", ")),
              if (length(valid_class$mismatch_col4) > 0)
                paste("  Ketidakcocokan col4:",
                      paste(valid_class$mismatch_col4, collapse = ", ")),
              sep = "\n"
            )
            append_log(log_msg)
            showNotification("Ketidakcocokan terdeteksi. Periksa tab Log Validasi.",
                             type = "warning", duration = 8)
          }
          
          incProgress(0.1, detail = "Selesai!")
          
        }, error = function(e) {
          msg <- conditionMessage(e)
          if (is.null(msg) || msg == "") msg <- "Error tidak diketahui (lihat konsol untuk detail)"
          append_log(paste0("ERROR: ", msg))
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
      } else if (rv$unlocked >= 2 && !is.null(rv$matriks_serasi)) {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Siap menjalankan analisis.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Lengkapi langkah sebelumnya.")
      }
    })
    
    # ── Shared result UI wiring ────────────────────────────────
    overlap_config <- list(
      map_color_col    = "idx_serasi",
      map_title        = "Indeks SERASI",
      map_palette      = "RdYlGn",
      map_label_cols   = c(
        "ID PU"         = "id_pu",
        "Status"        = "stat_pu",
        "RTRW"          = "RTRW",
        "RZWP3K"        = "RZWP3K",
        "Luas (ha)"     = "area_ha",
        "Indeks SERASI" = "idx_serasi"
      ),
      table_cols = c(
        "id_pu"     = "ID PU",
        "RTRW"      = "RTRW",
        "RZWP3K"    = "RZWP3K",
        "admin"     = "Administrasi",
        "area_ha"   = "Luas (ha)",
        "idx_serasi" = "Indeks SERASI"
      ),
      table_round_cols = c("Luas (ha)", "Indeks SERASI")
    )
    
    render_result_server(input, output, session, rv, overlap_config)
    
  })
}