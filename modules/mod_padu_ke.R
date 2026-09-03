# ui/modules/mod_padu_ke.R
# ============================================================
#  MODULE: PADU-KE (2.1 PADU-KE)
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
padu_ke_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("2.1 PADU-KE (Keterpaduan Penggunaan Lahan dan Lautan)", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menilai kepaduan lingkungan berdasarkan keterpaduan penggunaan lahan dan lautan untuk menghasilkan nilai indeks PADU-KE.",
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
              title = "Langkah 2 — Menganalisis Konektivitas Ekologis",
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
padu_ke_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Reactive values ──────────────────────────────────────────
    rv <- reactiveValues(
      unlocked = 1,                # 1 = only step1, 2 = step2 unlocked
      
      # step1 data
      idx_serasi_map = NULL,
      lulc_vect = NULL,
      lulc_id_col = NULL,      
      lulc_class_col = NULL,   
      lulc_ref = NULL,           
      
      # step2 data
      matriks_padu_ke = NULL,
      parallel = FALSE,
      workers = 2,
      
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
    
    extract_vector_path <- function(file_input) {
      gpkg_row <- file_input[grepl("\\.gpkg$", file_input$name, ignore.case = TRUE), ]
      if (nrow(gpkg_row) == 1) return(gpkg_row$datapath)
      extract_shp_path(file_input)
    }
    
    # ── Step 1 UI ──────────────────────────────────────────────
    output$step1_ui <- renderUI({
      tagList(
        tags$p(tags$i(class = "bi bi-info-circle me-1"), "Peta Indeks SERASI (.gpkg atau .shp)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "Output dari modul 'Identifikasi Konflik Spasial' (idx_serasi.gpkg). Menerima .gpkg atau .shp."),
        fileInput(ns("idx_serasi_file"), label = NULL,
                  accept = c(".gpkg", ".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        
        tags$p(tags$i(class = "bi bi-map me-1"), "Peta Tutupan/Penggunaan Lahan (.shp)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "Layer poligon dengan kelas tutupan lahan."),
        fileInput(ns("lulc_file"), label = NULL,
                  accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        
        # Dynamic dropdowns for LULC columns 
        uiOutput(ns("lulc_column_selectors")),
        
        hr(),
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_generate_matrix"),
                       tagList(tags$i(class = "bi bi-file-earmark-excel me-1"),
                               "Buat Templat Matriks PADU-KE"),
                       class = "btn-outline-primary btn-sm"),
          downloadButton(ns("dl_matrix_template"), "Unduh Matriks",
                         class = "btn-outline-success btn-sm")
        ),
        uiOutput(ns("matrix_template_status")),
        
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Langkah 2")
      )
    })
    
    # ── Load SERASI map ───────────────────────────────────────
    observeEvent(input$idx_serasi_file, {
      req(input$idx_serasi_file)
      tryCatch({
        path <- extract_vector_path(input$idx_serasi_file)
        sf_obj <- load_and_validate_shapefile(path)
        rv$idx_serasi_map <- ensure_geometry_name(sf_obj)  
        showNotification("Peta Indeks SERASI berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$idx_serasi_map <- NULL
        showNotification(paste("Gagal memuat peta SERASI:", e$message), type = "error")
      })
    })
    
    # ── Load LULC and populate column dropdowns ──────────────
    observeEvent(input$lulc_file, {
      req(input$lulc_file)
      tryCatch({
        sf_obj <- load_and_validate_shapefile(extract_shp_path(input$lulc_file))
        rv$lulc_vect <- ensure_geometry_name(sf_obj)
        
        col_names <- names(rv$lulc_vect)
        col_names <- col_names[!col_names %in% c("geometry", "geom")]
        
        # Render the column selectors
        output$lulc_column_selectors <- renderUI({
          req(rv$lulc_vect)
          tagList(
            selectInput(
              ns("lulc_id_col"),
              label = "Pilih kolom ID (numeric, untuk kode kelas)",
              choices = col_names,
              selected = if (!is.null(rv$lulc_id_col) && rv$lulc_id_col %in% col_names) rv$lulc_id_col else col_names[1]
            ),
            selectInput(
              ns("lulc_class_col"),
              label = "Pilih kolom Nama Kelas (teks)",
              choices = col_names,
              selected = if (!is.null(rv$lulc_class_col) && rv$lulc_class_col %in% col_names) rv$lulc_class_col else col_names[1]
            )
          )
        })
        
        showNotification("Peta tutupan lahan berhasil dimuat. Silakan pilih kolom ID dan Nama Kelas.", type = "message")
      }, error = function(e) {
        rv$lulc_vect <- NULL
        output$lulc_column_selectors <- renderUI(NULL)
        showNotification(paste("Gagal memuat peta tutupan lahan:", e$message), type = "error")
      })
    })
    
    # ── React to column selections ────────────────────────────
    observeEvent(c(input$lulc_id_col, input$lulc_class_col), {
      req(rv$lulc_vect, input$lulc_id_col, input$lulc_class_col)
      
      # Store selections
      rv$lulc_id_col <- input$lulc_id_col
      rv$lulc_class_col <- input$lulc_class_col
      
      # Validate that ID column is numeric
      col_data <- rv$lulc_vect[[rv$lulc_id_col]]
      if (!is.numeric(col_data)) {
        showNotification(
          paste("Kolom", rv$lulc_id_col, "harus bertipe numerik. Pilih kolom lain."),
          type = "warning", duration = 8
        )
        rv$lulc_ref <- NULL
        return()
      }
      
      # Build lulc_ref
      rv$lulc_ref <- rv$lulc_vect %>%
        sf::st_drop_geometry() %>%
        dplyr::select(ID = !!sym(rv$lulc_id_col), LC = !!sym(rv$lulc_class_col)) %>%
        dplyr::distinct(ID, LC) %>%
        dplyr::arrange(ID)
      
      showNotification("Kolom LULC diperbarui. Analisis ulang jika perlu.", type = "message")
    })
    
    # ── Generate matrix template ───────────────────────────────
    matrix_template_path <- reactiveVal(NULL)
    
    observeEvent(input$btn_generate_matrix, {
      
      # Check output directory 
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification(
          "Direktori output belum diatur. Harap atur direktori output terlebih dahulu.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      req(rv$lulc_ref)
      
      if (!requireNamespace("openxlsx2", quietly = TRUE)) {
        showNotification(
          "Paket 'openxlsx2' diperlukan untuk membuat template dengan instruksi. Harap instal.",
          type = "error",
          duration = 8
        )
        return()
      }
      
      tryCatch({
        out_path <- file.path(output_dir(), "matriks_padu_ke_template.xlsx")
        dir.create(output_dir(), recursive = TRUE, showWarnings = FALSE)
        
        generate_matrix_padu_ke(rv$lulc_ref, file_path = out_path)
        
        matrix_template_path(out_path)
        showNotification(paste("Template matriks PADU-KE dibuat →", out_path),
                         type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Gagal membuat template matriks:", e$message),
                         type = "error", duration = 8)
      })
    })
    
    output$matrix_template_status <- renderUI({
      req(matrix_template_path())
      div(class = "alert alert-success mb-0",
          tags$i(class = "bi bi-check-circle me-2"),
          "Template siap diunduh.")
    })
    
    output$dl_matrix_template <- downloadHandler(
      filename = function() "matriks_padu_ke_template.xlsx",
      content = function(file) {
        req(matrix_template_path())
        file.copy(matrix_template_path(), file, overwrite = TRUE)
      }
    )
    
    # ── Step 1 -> Step 2 ──────────────────────────────────────
    observeEvent(input$btn_next_1, {
      
      if (is.null(rv$idx_serasi_map) || is.null(rv$lulc_vect) || is.null(rv$lulc_ref)) {
        showNotification("Harap unggah peta SERASI, peta tutupan lahan, dan pilih kolom ID & kelas sebelum melanjutkan.",
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
        tags$p(tags$i(class = "bi bi-table me-1"), "Tabel Matriks PADU-KE (.xlsx)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "Kolom yang diperlukan: class1, class2, adj_index."),
        fileInput(ns("matriks_padu_ke_file"), label = NULL, accept = ".xlsx"),
        uiOutput(ns("matriks_padu_ke_status")),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-gear me-1"), "Pengaturan Lanjutan",
               style = "font-weight: 600; margin-bottom: 4px;"),
        accordion(
          accordion_panel(
            title = "Pengaturan lanjutan",
            icon = icon("gear"),
            open = FALSE,
            checkboxInput(ns("parallel"), "Aktifkan pemrosesan paralel", value = FALSE),
            numericInput(ns("workers"), "Jumlah kanal komputasi (cores)", value = 2, min = 1, step = 1)
          )
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
                               "Lakukan Analisis PADU-KE"),
                       class = "btn-success btn-sm")
        ),
        
        .step_nav(ns, back_id = "btn_back_2", next_id = NULL)
      )
    })
    
    # ── Load matrix in step2 ──────────────────────────────────
    observeEvent(input$matriks_padu_ke_file, {
      req(input$matriks_padu_ke_file)
      tryCatch({
        rv$matriks_padu_ke <- load_validate_matrix_table(
          input$matriks_padu_ke_file$datapath, title = "padu_ke"
        )
        showNotification("Matriks PADU-KE berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$matriks_padu_ke <- NULL
        showNotification(paste("Gagal memuat matriks:", e$message), type = "error")
      })
    })
    
    output$matriks_padu_ke_status <- renderUI({
      if (is.null(rv$matriks_padu_ke)) return(NULL)
      div(class = "alert alert-success mb-0",
          tags$i(class = "bi bi-check-circle me-2"),
          "Matriks PADU-KE berhasil divalidasi.")
    })
    
    observeEvent(input$btn_back_2, {
      go_to_panel("step1")
    })
    
    # ── Run analysis ──────────────────────────────────────────
    observeEvent(input$btn_run, {
      req(rv$idx_serasi_map, rv$lulc_vect, rv$matriks_padu_ke, rv$lulc_id_col)
      
      # Store advanced settings
      rv$parallel <- input$parallel
      rv$workers <- input$workers
      
      # Reset previous results
      rv$analysis_result <- NULL
      rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL
      rv$log_messages <- ""
      
      append_log <- function(msg) {
        rv$log_messages <- paste0(rv$log_messages, msg, "\n")
      }
      
      withProgress(message = "Menjalankan Analisis PADU-KE", value = 0, {
        
        tryCatch({
          
          incProgress(0.1, detail = "Memulai analisis...")
          append_log(">> Memulai analisis PADU-KE...")
          
          # Step 1: Prepare data
          if ("length" %in% colnames(rv$idx_serasi_map)) {
            idx_map <- dissolve_id_pu(rv$idx_serasi_map)
          } else {
            idx_map <- rv$idx_serasi_map  
          }
          
          lulc_vect_data <- rv$lulc_vect
          class_col <- rv$lulc_id_col 
          append_log(paste0("   Kolom kelas (ID): ", class_col))
          
          # Step 2: Calculate adjacency (progress 30% → 70%)
          incProgress(0.2, detail = "Menghitung ketetanggaan tutupan lahan...")
          append_log(">> Menghitung ketetanggaan tutupan lahan...")
          
          lulc_adjacencies <- withCallingHandlers(
            calculate_lulc_adjacency(
              lulc         = lulc_vect_data,
              admin_vector = idx_map,
              id_col       = "id_pu",
              class_col    = class_col,
              parallel     = rv$parallel,
              workers      = rv$workers
            ),
            message = function(m) append_log(paste0("   [INFO] ", m$message)),
            warning = function(w) append_log(paste0("   [WARN] ", w$message))
          )
          
          incProgress(0.4, detail = "Ketetanggaan selesai, memproses hasil...")
          append_log("   Ketetanggaan selesai.")
          
          # Step 3: Calculate PADU-KE (progress 80%)
          incProgress(0.1, detail = "Menghitung indeks PADU-KE...")
          append_log(">> Menghitung indeks PADU-KE...")
          padu_ke <- calculate_padu_ke(
            matriks_padu_ke   = rv$matriks_padu_ke,
            lulc_ref          = rv$lulc_ref,
            lulc_adjacencies  = lulc_adjacencies,
            idx_serasi_map    = idx_map,
            normalize         = TRUE
          )
          
          idx_padu_ke <- padu_ke$idx_padu_ke %>% dplyr::select(-abs_idx_padu_ke)
          idx_padu_ke_map <- padu_ke$idx_padu_ke_map
          
          # Step 4: Save results (progress 90%)
          incProgress(0.1, detail = "Menyimpan hasil...")
          append_log(">> Menyimpan hasil ke direktori...")
          
          padu_ke_dir <- file.path(output_dir(), "Analisis PADU-KE")
          if (!dir.exists(padu_ke_dir)) {
            dir.create(padu_ke_dir, recursive = TRUE, showWarnings = FALSE)
          }
          
          if (!dir.exists(padu_ke_dir)) {
            stop("Tidak dapat membuat atau mengakses direktori: ", padu_ke_dir)
          }
          
          gpkg_path <- file.path(padu_ke_dir, "idx_padu_ke.gpkg")
          xlsx_path <- file.path(padu_ke_dir, "idx_padu_ke.xlsx")
          
          sf::st_write(idx_padu_ke_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          result_table <- as_tibble(idx_padu_ke_map %>% sf::st_drop_geometry())
          openxlsx::write.xlsx(result_table, xlsx_path)
          
          rv$gpkg_path <- gpkg_path
          rv$xlsx_path <- xlsx_path
          rv$analysis_result <- list(map = idx_padu_ke_map, table = result_table)
          
          # ─── Store result for report generation ───
          out <- list(
            inputs = list(
              start_time = Sys.time(),
              idx_serasi_path = input$idx_serasi_file,
              lulc_map_path = input$lulc_file,
              lulc_id_col = rv$lulc_id_col,
              lulc_class_col = rv$lulc_class_col,
              matriks_padu_ke_path = input$matriks_padu_ke_file,
              output_dir = output_dir()
            ),
            result = list(
              idx_serasi_map = idx_map,
              lulc_map = lulc_vect_data,
              lulc_ref = rv$lulc_ref,
              matriks_padu_ke = rv$matriks_padu_ke,
              lulc_adjacencies  = lulc_adjacencies,
              idx_padu_ke_map = idx_padu_ke_map,
              idx_padu_ke_table = result_table
            )
          )
          
          # Export log
          log_dir <- file.path(padu_ke_dir, "log")
          if (!dir.exists(log_dir)) {
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
          }
          log_path <- file.path(log_dir, "idx_padu_ke_log.txt")
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
          session$userData$module_results$padu_ke <- out
          
          # Export static maps 
          idx_padu_ke_viz <- plot_continuous_map(
            map      = idx_padu_ke_map,
            column   = "idx_padu_ke",         
            title    = "Peta Indeks PADU-KE",
            legend   = "Indeks PADU-KE",
            low      = "red",
            high     = "lightgreen",
            filepath = file.path(log_dir, "idx_padu_ke.png")
          )
          
          lulc_viz <- plot_categorical_map(
            map      = lulc_vect_data,
            title    = "Peta Tutupan/Penggunaan Lahan",
            column   = rv$lulc_class_col,  
            legend   = "Kelas Penutup Lahan",
            legend_ncol = 1,
            filepath = file.path(log_dir, "penutup_lahan.png")
          )
          
          append_log(paste0("   Hasil disimpan di: ", gpkg_path))
          append_log("Analisis PADU-KE berhasil diselesaikan.")
          showNotification(paste("Analisis selesai. Hasil disimpan ke", gpkg_path),
                           type = "message", duration = 5)
          
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
      } else if (rv$unlocked >= 2 && !is.null(rv$matriks_padu_ke)) {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Siap menjalankan analisis.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Lengkapi langkah sebelumnya.")
      }
    })
    
    # ── Result Visualization ───────────────────────────────────
    padu_ke_config <- list(
      map_color_col = "idx_padu_ke",
      map_title = "Indeks PADU-KE",
      map_palette = "RdYlGn",
      map_label_cols = c(
        "ID PU" = "id_pu",
        "RTRW" = "RTRW",
        "RZWP3K" = "RZWP3K",
        "Indeks PADU-KE" = "idx_padu_ke"
      ),
      table_cols = c(
        "id_pu" = "ID PU",
        "RTRW" = "RTRW",
        "RZWP3K" = "RZWP3K",
        "admin" = "Administrasi",
        "area_ha" = "Luas (ha)",
        "idx_padu_ke" = "Indeks PADU-KE"
      ),
      table_round_cols = c("Luas (ha)", "Indeks PADU-KE")
    )
    
    render_result_server(input, output, session, rv, padu_ke_config)
    
  })
}