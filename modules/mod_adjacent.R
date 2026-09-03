# ui/modules/mod_adjacent.R
# ============================================================
#  MODULE: Adjacent (1.2 Type 2: Adjacent)
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
adjacent_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("2.1 Analisis SERASI Area Bertetangga", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Mengidentifikasi area bertetangga secara spasial antara kawasan/zona RTRW dan RZWP3K serta menghitung indeks SERASI.",
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
adjacent_server <- function(id, output_dir) {
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
      
      # administrative data (NEW)
      admin_vect = NULL,
      admin_col = NULL,        
      
      # step2 data
      matriks_serasi = NULL,
      threshold_ha = 0,
      
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
        
        hr(),
        
        # ── NEW: Administrative map input ──────────────────────
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
        
        tags$p(tags$i(class = "bi bi-table me-1"), "Tabel Acual Pola RZWP3K (.xlsx)",
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
    
    # ── Admin shapefile and field selection ────────────────────
    observeEvent(input$admin_file, {
      req(input$admin_file)
      tryCatch({
        shp_path <- extract_shp_path(input$admin_file)
        admin_sf <- load_and_validate_shapefile(shp_path)
        admin_sf <- ensure_geometry_name(admin_sf)  
        rv$admin_vect <- admin_sf
        
        # Extract column names (drop geometry and maybe others)
        col_names <- names(admin_sf)
        col_names <- col_names[!col_names %in% c("geometry", "geom")]
        
        # Update select input choices via UI output
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
    
    # ── Load other shapefiles and tables ──────────────────────
    observeEvent(input$rtrw_file, {
      req(input$rtrw_file)
      tryCatch({
        sf <- load_and_validate_shapefile(extract_shp_path(input$rtrw_file))
        rv$rtrw_vect <- ensure_geometry_name(sf)   
        showNotification("Peta RTRW berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$rtrw_vect <- NULL
        showNotification(paste("Gagal memuat RTRW:", e$message), type = "error")
      })
    })
    
    observeEvent(input$rzwp3k_file, {
      req(input$rzwp3k_file)
      tryCatch({
        sf <- load_and_validate_shapefile(extract_shp_path(input$rzwp3k_file))
        rv$rzwp3k_vect <- ensure_geometry_name(sf)  
        showNotification("Peta RZWP3K berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$rzwp3k_vect <- NULL
        showNotification(paste("Gagal memuat RZWP3K:", e$message), type = "error")
      })
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
      filename = function() "matriks_serasi_adjacent.xlsx",
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
        
        # ── Pengaturan Lanjutan (accordion) ──────────────────────
        accordion_panel(
          title = "Pengaturan Lanjutan",
          icon = icon("gear"),
          open = FALSE,
          
          # Input presisi numerik
          numericInput(
            ns("m_precision"),
            "Galat Geometri (meter)",
            value = 1, min = 0.001, step = 1
          ),
          tags$small(
            "Menentukan ukuran grid terkecil untuk pembulatan koordinat. 
          Semakin kecil nilainya (misal 0.1), semakin presisi bentuk geometri, 
          tetapi bisa memunculkan celah kecil atau tumpang tindih yang tidak diinginkan. 
          Semakin besar (misal 10), koordinat akan lebih kasar, 
          yang dapat menyederhanakan data tetapi berisiko menghilangkan segmen batas bersama 
          yang seharusnya terdeteksi. Nilai 1 meter umumnya aman untuk sebagian besar kasus.",
            style = "color: #6c757d; display: block; margin-top: -5px; margin-bottom: 12px; font-size: 0.85em;"
          ),
          
          # Input toleransi snap
          numericInput(
            ns("snap_tolerance"),
            "Snapping Distance (meter)",
            value = 0.5, min = 0, step = 0.1
          ),
          tags$small(
            "Mengoreksi celah kecil antara batas RZWP3K dan RTRW dengan menarik garis batas RZWP3K 
          mendekati RTRW sebelum menghitung panjang segmen bersama. 
          Nilai 0.5 meter cukup untuk mengatasi kesalahan digitasi umum. 
          Naikkan (misal 1–2 meter) jika sering muncul hasil panjang = 0 meskipun secara visual 
          kedua poligon bersentuhan. Jangan terlalu besar agar tidak menjepret batas yang sebenarnya tidak bersentuhan.",
            style = "color: #6c757d; display: block; margin-top: -5px; margin-bottom: 0; font-size: 0.85em;"
          ),
          
          # Input parallel processing
          checkboxInput(ns("parallel"), "Aktifkan pemrosesan paralel", value = FALSE),
          numericInput(ns("workers"), "Jumlah kanal komputasi (cores)", value = 2, min = 1, step = 1)
        ),
        
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
    
    # ── Run analysis  ──────────────────────────
    observeEvent(input$btn_run, {
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
      
      withProgress(message = "Menjalankan Analisis Bertetangga", value = 0, {
        
        tryCatch({
          
          incProgress(0.1, detail = "Memulai analisis...")
          append_log(">> Memulai analisis area bertetangga...")
          
          incProgress(0.2, detail = "Mengidentifikasi area bertetangga...")
          append_log(">> Mengidentifikasi area bertetangga antara RTRW dan RZWP3K...")
          adjacent_map_raw <- identify_adjacent(
            rtrw = rv$rtrw_vect,
            rzwp = rv$rzwp3k_vect,
            min_area_ha = 1,
            m_precision = input$m_precision 
          )
          
          # Identify group
          adjacent_map_raw_update <- identify_adjacent_group(
            adjacent_map_raw
          )
          
          append_log("   Area bertetangga berhasil diidentifikasi.")
          
          incProgress(0.2, detail = "Memproses area bertetangga...")
          append_log(">> Memproses area bertetangga dengan buffer 100 m...")
          adjacent_map <- process_adjacent(
            pu_sf = adjacent_map_raw_update,
            buffer_m = 100,
            m_precision = input$m_precision,   
            snap_tolerance = input$snap_tolerance,
            parallel = input$parallel, 
            workers = input$workers
          )
          append_log("   Pemrosesan selesai.")
          
          incProgress(0.2, detail = "Memvalidasi kesesuaian kelas zona...")
          append_log(">> Memvalidasi kesesuaian nama kelas antara peta dan prioritas...")
          valid_class <- validate_zone_class(adjacent_map, rv$rtrw_prioritas, rv$rzwp3k_prioritas)
          
          incProgress(0.2, detail = "Menggabungkan dan menyimpan hasil...")
          if (length(valid_class$mismatch_col3) == 0 &&
              length(valid_class$mismatch_col4) == 0) {
            
            append_log("   Semua nama kelas cocok. Menggabungkan indeks SERASI...")
            idx_serasi_map <- merge_attributes_to_map(adjacent_map, rv$matriks_serasi) %>%
              filter(idx_serasi != 1)
            idx_serasi_table <- as_tibble(idx_serasi_map %>% sf::st_drop_geometry())
            
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
            
            serasi_dir <- file.path(output_dir(), "Analisis SERASI")
            if (!dir.exists(serasi_dir)) {
              dir.create(serasi_dir, recursive = TRUE, showWarnings = FALSE)
            }
            
            if (!dir.exists(serasi_dir)) {
              stop("Tidak dapat membuat atau mengakses direktori: ", serasi_dir)
            }
            
            gpkg_path <- file.path(serasi_dir, "idx_serasi_adjacent.gpkg")
            xlsx_path <- file.path(serasi_dir, "idx_serasi_adjacent.xlsx")
            
            sf::st_write(idx_serasi_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
            openxlsx::write.xlsx(idx_serasi_table, xlsx_path)
            
            rv$gpkg_path <- gpkg_path
            rv$xlsx_path <- xlsx_path
            
            idx_serasi_map_viz <- dissolve_id_pu(idx_serasi_map)
            rv$analysis_result <- list(map = idx_serasi_map_viz, table = as_tibble(sf::st_drop_geometry(idx_serasi_map_viz)))
            
            # ─── Store result for report generation ───
            out <- list(
              inputs = list(
                start_time = Sys.time(),
                case = "adjacent",
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
            
            # Export log
            log_dir <- file.path(serasi_dir, "log")
            if (!dir.exists(log_dir)) {
              dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
            }
            log_path <- file.path(log_dir, "idx_serasi_log.txt")
            if (dir.exists(log_dir)) {
              tryCatch({
                dput(out$inputs, file = log_path)
              }, error = function(e) {
                warning("Gagal menulis file log: ", e$message)
              })
            } else {
              warning("Direktori log tidak tersedia, lewati penulisan log.")
            }
            
            # Store in shared environment
            session$userData$module_results$serasi <- out
            
            # Export static maps 
            idx_serasi_viz <- plot_continuous_map(
              map      = idx_serasi_map,
              column   = "idx_serasi",         
              title    = "Peta Indeks SERASI Kasus Bertetangga",
              legend   = "Indeks SERASI",
              low      = "red",
              high     = "lightgreen",
              filepath = file.path(log_dir, "idx_serasi.png")
            )
            
            rtrw_viz <- plot_categorical_map(
              map      = rv$rtrw_vect,
              title    = "Peta RTRW Kasus Bertetangga",
              column   = "RTRW",
              legend   = "Kelas RTRW",
              legend_ncol = 1,
              filepath = file.path(log_dir, "rtrw.png")
            )
            
            rzwp3k_viz <- plot_categorical_map(
              map      = rv$rzwp3k_vect,
              title    = "Peta RZWP3K Kasus Bertetangga",
              column   = "RZWP3K",
              legend   = "Kelas RZWP3K",
              legend_ncol = 1,
              filepath = file.path(log_dir, "rzwp3k.png")
            )
            
            append_log(paste0("   Hasil disimpan di: ", gpkg_path))
            append_log("Analisis bertetangga berhasil diselesaikan.")
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
        
      }) 
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
    adjacent_config <- list(
      map_color_col    = "idx_serasi",
      map_title        = "Indeks SERASI",
      map_palette      = "RdYlGn",
      map_label_cols   = c(
        "ID PU"          = "id_pu",
        "ID Group"       = "id_group",
        "RTRW"           = "RTRW",
        "RZWP3K"         = "RZWP3K",
        "Indeks SERASI"  = "idx_serasi"
      ),
      table_cols = c(
        "id"       = "ID",
        "id_pu"    = "ID PU",
        "id_group" = "ID Group",
        "RTRW"     = "RTRW",
        "RZWP3K"   = "RZWP3K",
        "admin"    = "Administrasi",
        "area_ha"  = "Luas (ha)",
        "length"   = "Panjang Segmen Ketetanggaan (m)",
        "n_pairs"  = "Jumlah pasangan tetangga",
        "idx_serasi" = "Indeks SERASI"
      ),
      table_round_cols = c("Luas (ha)", "Panjang Segmen Ketetanggaan (m)", "Indeks SERASI")
    )
    
    render_result_server(input, output, session, rv, adjacent_config)
    
  })
}