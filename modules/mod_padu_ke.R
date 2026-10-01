# ui/modules/mod_padu_ke.R
# ============================================================
#  MODULE: PADU-KE (2.1 PADU-KE)
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

padu_ke_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("2.1 PADU-KE (Keterpaduan Penggunaan Lahan dan Lautan)",
         style = "margin: 0; font-weight: 700;"),
      tags$p("Menilai kepaduan lingkungan berdasarkan keterpaduan penggunaan lahan dan lautan untuk menghasilkan nilai indeks PADU-KE.",
             style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;")
    ),
    fluidRow(
      class = "g-3",
      column(
        width = 4,
        card(
          card_header("Input & Parameter"),
          accordion(
            id = ns("wizard"), open = "step1", multiple = FALSE,
            accordion_panel("Langkah 1 — Menyiapkan Data Utama", value = "step1",
                            icon = tags$i(class = "bi bi-folder-fill"),
                            uiOutput(ns("step1_ui"))),
            accordion_panel("Langkah 2 — Menganalisis Konektivitas Ekologis", value = "step2",
                            icon = tags$i(class = "bi bi-diagram-3-fill"),
                            uiOutput(ns("step2_ui")))
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

padu_ke_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Shared SERASI input ────────────────────────────────
    serasi_in <- serasi_input(input, output, session, output_dir)
    
    rv <- reactiveValues(
      unlocked = 1,
      lulc_vect = NULL,
      lulc_id_col = NULL,
      lulc_class_col = NULL,
      lulc_ref = NULL,
      matriks_padu_ke = NULL,
      parallel = FALSE,
      workers = 2,
      analysis_result = NULL,
      gpkg_path = NULL,
      xlsx_path = NULL,
      log_messages = ""
    )
    
    go_to_panel <- function(value) {
      accordion_panel_set(id = "wizard", values = value, session = session)
    }
    
    extract_shp_path <- function(file_input) {
      shp_row <- file_input[grepl("\\.shp$", file_input$name, ignore.case = TRUE), ]
      validate(need(nrow(shp_row) == 1,
                    "Harap unggah semua komponen shapefile (.shp, .dbf, .prj, .shx)"))
      stem <- tools::file_path_sans_ext(shp_row$datapath)
      for (i in seq_len(nrow(file_input))) {
        ext <- tools::file_ext(file_input$name[i])
        file.rename(file_input$datapath[i], paste0(stem, ".", ext))
      }
      paste0(stem, ".shp")
    }
    
    # ── Step 1 UI ──────────────────────────────────────────
    output$step1_ui <- renderUI({
      tagList(
        serasi_in$ui_block(),
        
        tags$p(tags$i(class = "bi bi-map me-1"), "Peta Tutupan/Penggunaan Lahan (.shp)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "Layer poligon dengan kelas tutupan lahan."),
        fileInput(ns("lulc_file"), label = NULL,
                  accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        uiOutput(ns("lulc_column_selectors")),
        hr(),
        div(style = "display: flex; gap: 8px; flex-wrap: wrap;",
            actionButton(ns("btn_generate_matrix"),
                         tagList(tags$i(class = "bi bi-file-earmark-excel me-1"),
                                 "Buat Templat Matriks PADU-KE"),
                         class = "btn-outline-primary btn-sm"),
            downloadButton(ns("dl_matrix_template"), "Unduh Matriks",
                           class = "btn-outline-success btn-sm")),
        uiOutput(ns("matrix_template_status")),
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1",
                  next_label = "Lanjut ke Langkah 2")
      )
    })
    
    # ── LULC ───────────────────────────────────────────────
    observeEvent(input$lulc_file, {
      req(input$lulc_file)
      tryCatch({
        sf_obj <- load_and_validate_shapefile(extract_shp_path(input$lulc_file))
        rv$lulc_vect <- ensure_geometry_name(sf_obj)
        col_names <- names(rv$lulc_vect)
        col_names <- col_names[!col_names %in% c("geometry", "geom")]
        
        output$lulc_column_selectors <- renderUI({
          req(rv$lulc_vect)
          tagList(
            selectInput(ns("lulc_id_col"),
                        label = "Pilih kolom ID (numeric, untuk kode kelas)",
                        choices = col_names,
                        selected = if (!is.null(rv$lulc_id_col) && rv$lulc_id_col %in% col_names)
                          rv$lulc_id_col else col_names[1]),
            selectInput(ns("lulc_class_col"),
                        label = "Pilih kolom Nama Kelas (teks)",
                        choices = col_names,
                        selected = if (!is.null(rv$lulc_class_col) && rv$lulc_class_col %in% col_names)
                          rv$lulc_class_col else col_names[1])
          )
        })
        showNotification("Peta tutupan lahan berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$lulc_vect <- NULL
        output$lulc_column_selectors <- renderUI(NULL)
        showNotification(paste("Gagal memuat peta tutupan lahan:", e$message),
                         type = "error")
      })
    })
    
    observeEvent(c(input$lulc_id_col, input$lulc_class_col), {
      req(rv$lulc_vect, input$lulc_id_col, input$lulc_class_col)
      rv$lulc_id_col <- input$lulc_id_col
      rv$lulc_class_col <- input$lulc_class_col
      col_data <- rv$lulc_vect[[rv$lulc_id_col]]
      if (!is.numeric(col_data)) {
        showNotification(paste("Kolom", rv$lulc_id_col, "harus bertipe numerik."),
                         type = "warning", duration = 8)
        rv$lulc_ref <- NULL
        return()
      }
      rv$lulc_ref <- rv$lulc_vect %>%
        sf::st_drop_geometry() %>%
        dplyr::select(ID = !!sym(rv$lulc_id_col), LC = !!sym(rv$lulc_class_col)) %>%
        dplyr::distinct(ID, LC) %>% dplyr::arrange(ID)
      showNotification("Kolom LULC diperbarui.", type = "message")
    })
    
    # ── Matrix template (unchanged) ────────────────────────
    matrix_template_path <- reactiveVal(NULL)
    
    observeEvent(input$btn_generate_matrix, {
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification("Direktori output belum diatur.", type = "error", duration = 5)
        return()
      }
      req(rv$lulc_ref)
      withProgress(message = "Membuat Templat Matriks PADU-KE", value = 0, {
        tryCatch({
          padu_ke_dir <- file.path(output_dir(), "Analisis PADU-KE")
          dir.create(padu_ke_dir, recursive = TRUE, showWarnings = FALSE)
          out_path <- file.path(padu_ke_dir, "matriks_padu_ke_template.xlsx")
          generate_matrix_padu_ke(rv$lulc_ref, file_path = out_path)
          matrix_template_path(out_path)
          showNotification(paste("Template dibuat \u2192", out_path),
                           type = "message", duration = 5)
        }, error = function(e) {
          showNotification(paste("Gagal membuat template:", e$message),
                           type = "error", duration = 8)
        })
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
        req(matrix_template_path()); file.copy(matrix_template_path(), file, overwrite = TRUE)
      }
    )
    
    observeEvent(input$btn_next_1, {
      rv$unlocked <- max(rv$unlocked, 2)
      go_to_panel("step2")
    })
    
    # ── Step 2 UI ──────────────────────────────────────────
    output$step2_ui <- renderUI({
      tagList(
        tags$p(tags$i(class = "bi bi-table me-1"), "Tabel Matriks PADU-KE (.xlsx)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "Kolom yang diperlukan: class1, class2, adj_index."),
        fileInput(ns("matriks_padu_ke_file"), label = NULL, accept = ".xlsx"),
        uiOutput(ns("matriks_padu_ke_status")),
        hr(),
        accordion(accordion_panel("Pengaturan lanjutan", icon = icon("gear"),
                                  open = FALSE,
                                  checkboxInput(ns("parallel"), "Aktifkan pemrosesan paralel", value = FALSE),
                                  numericInput(ns("workers"), "Jumlah kanal komputasi (cores)",
                                               value = 2, min = 1, step = 1))),
        hr(),
        if (is.null(output_dir()) || !nzchar(output_dir())) {
          div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
              tags$i(class = "bi bi-exclamation-triangle me-1"),
              "Direktori output belum diatur.")
        },
        div(style = "display: flex; gap: 8px; flex-wrap: wrap;",
            actionButton(ns("btn_run"),
                         tagList(tags$i(class = "bi bi-play-fill me-1"),
                                 "Lakukan Analisis PADU-KE"),
                         class = "btn-success btn-sm")),
        .step_nav(ns, back_id = "btn_back_2", next_id = NULL)
      )
    })
    
    observeEvent(input$matriks_padu_ke_file, {
      req(input$matriks_padu_ke_file)
      tryCatch({
        rv$matriks_padu_ke <- load_validate_matrix_table(
          input$matriks_padu_ke_file$datapath, title = "padu_ke")
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
    
    observeEvent(input$btn_back_2, go_to_panel("step1"))
    
    # ── Run analysis ───────────────────────────────────────
    observeEvent(input$btn_run, {
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification("Direktori output belum diatur.", type = "error", duration = 5)
        return()
      }
      
      idx_serasi_map <- serasi_in$idx_serasi_map()
      req(idx_serasi_map, rv$lulc_vect, rv$matriks_padu_ke, rv$lulc_id_col)
      
      rv$parallel <- input$parallel
      rv$workers <- input$workers
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
          
          if ("length" %in% colnames(idx_serasi_map)) {
            idx_map <- dissolve_id_pu(idx_serasi_map)
          } else {
            idx_map <- idx_serasi_map
          }
          
          lulc_vect_data <- rv$lulc_vect
          class_col <- rv$lulc_id_col
          append_log(paste0("   Kolom kelas (ID): ", class_col))
          
          incProgress(0.2, detail = "Menghitung ketetanggaan tutupan lahan...")
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
          
          incProgress(0.4, detail = "Menghitung indeks PADU-KE...")
          padu_ke <- calculate_padu_ke(
            matriks_padu_ke   = rv$matriks_padu_ke,
            lulc_ref          = rv$lulc_ref,
            lulc_adjacencies  = lulc_adjacencies,
            idx_serasi_map    = idx_map,
            normalize         = TRUE
          )
          idx_padu_ke_map <- padu_ke$idx_padu_ke_map
          
          incProgress(0.1, detail = "Menyimpan hasil...")
          padu_ke_dir <- file.path(output_dir(), "Analisis PADU-KE")
          if (!dir.exists(padu_ke_dir))
            dir.create(padu_ke_dir, recursive = TRUE, showWarnings = FALSE)
          log_dir <- file.path(padu_ke_dir, "log")
          if (!dir.exists(log_dir))
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
          
          gpkg_path <- file.path(padu_ke_dir, "idx_padu_ke.gpkg")
          xlsx_path <- file.path(padu_ke_dir, "idx_padu_ke.xlsx")
          sf::st_write(idx_padu_ke_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          result_table <- as_tibble(idx_padu_ke_map %>% sf::st_drop_geometry())
          openxlsx::write.xlsx(result_table, xlsx_path)
          
          matriks_filled_name <- "matriks_padu_ke_filled.xlsx"
          matriks_filled_xlsx <- file.path(log_dir, matriks_filled_name)
          tryCatch({
            file.copy(input$matriks_padu_ke_file$datapath,
                      matriks_filled_xlsx, overwrite = TRUE)
          }, error = function(e) {
            warning("Gagal menyalin matriks PADU-KE: ", e$message)
          })
          
          rv$gpkg_path <- gpkg_path
          rv$xlsx_path <- xlsx_path
          rv$analysis_result <- list(map = idx_padu_ke_map, table = result_table)
          
          out <- list(
            inputs = list(
              start_time             = Sys.time(),
              idx_serasi_path        = serasi_in$filename(),
              idx_serasi_source      = serasi_in$source(),
              serasi_source_name     = serasi_in$filename(),
              serasi_source_hash     = serasi_in$hash(),
              lulc_map_path          = input$lulc_file,
              lulc_id_col            = rv$lulc_id_col,
              lulc_class_col         = rv$lulc_class_col,
              matriks_padu_ke_path   = input$matriks_padu_ke_file,
              matriks_padu_ke_uploaded = matriks_filled_name,
              output_dir             = output_dir()
            ),
            result = list(
              idx_serasi_map     = idx_map,
              lulc_map           = lulc_vect_data,
              lulc_ref           = rv$lulc_ref,
              matriks_padu_ke    = rv$matriks_padu_ke,
              lulc_adjacencies   = lulc_adjacencies,
              idx_padu_ke_map    = idx_padu_ke_map,
              idx_padu_ke_table  = result_table
            )
          )
          
          tryCatch({
            inputs <- out$inputs
            save(inputs, file = file.path(log_dir, "idx_padu_ke_log.rda"))
          }, error = function(e) warning("Gagal menulis log: ", e$message))
          
          session$userData$module_results$padu_ke <- out
          
          plot_continuous_map(
            map = idx_padu_ke_map, column = "idx_padu_ke",
            title = "Peta Indeks PADU-KE", legend = "Indeks PADU-KE",
            low = "red", high = "lightgreen",
            filepath = file.path(log_dir, "idx_padu_ke.png"))
          plot_categorical_map(
            map = lulc_vect_data, title = "Peta Tutupan/Penggunaan Lahan",
            column = rv$lulc_class_col, legend = "Kelas Penutup Lahan",
            legend_ncol = 1,
            filepath = file.path(log_dir, "penutup_lahan.png"))
          
          append_log(paste0("   Hasil disimpan di: ", gpkg_path))
          append_log("Analisis PADU-KE berhasil diselesaikan.")
          incProgress(0.1, detail = "Selesai!")
          showNotification(paste("Analisis selesai. Hasil disimpan ke", gpkg_path),
                           type = "message", duration = 5)
        }, error = function(e) {
          msg <- conditionMessage(e)
          if (is.null(msg) || msg == "") msg <- "Error tidak diketahui"
          append_log(paste0("ERROR: ", msg))
          showNotification(paste("Analisis gagal:", msg), type = "error", duration = 10)
        })
      })
    })
    
    output$status_box <- renderUI({
      if (!is.null(rv$analysis_result)) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"), "Analisis selesai.")
      } else if (rv$unlocked >= 2 && !is.null(rv$matriks_padu_ke)) {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"), "Siap menjalankan analisis.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"), "Lengkapi langkah sebelumnya.")
      }
    })
    
    padu_ke_config <- list(
      map_color_col = "idx_padu_ke",
      map_title     = "Indeks PADU-KE",
      map_palette   = "RdYlGn",
      map_label_cols = c("ID PU" = "id_pu", "RTRW" = "RTRW",
                         "RZWP3K" = "RZWP3K", "Indeks PADU-KE" = "idx_padu_ke"),
      table_cols = c(
        "id_pu" = "ID PU", "RTRW" = "RTRW", "RZWP3K" = "RZWP3K",
        "admin" = "Administrasi", "area_ha" = "Luas (ha)",
        "idx_padu_ke" = "Indeks PADU-KE"),
      table_round_cols = c("Luas (ha)", "Indeks PADU-KE")
    )
    
    render_result_server(input, output, session, rv, padu_ke_config)
  })
}