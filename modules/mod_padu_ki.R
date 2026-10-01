# ui/modules/mod_padu_ki.R
# ============================================================
#  MODULE: PADU-KI (2.7 PADU-KI: Disaster Risk Analysis)
# ============================================================

source("R/functions.R")
source("R/helpers.R")
source("R/shared_inputs.R")

padu_ki_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("2.7 PADU-KI (Ketahanan Iklim)", style = "margin: 0; font-weight: 700;"),
      tags$p("Menilai kepaduan lingkungan berdasarkan ketahanan iklim untuk menghasilkan nilai indeks PADU-KI.",
             style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;")
    ),
    fluidRow(
      class = "g-3",
      column(
        width = 4,
        card(
          card_header("Input & Parameter"),
          uiOutput(ns("serasi_ui_container")),
          tags$p(tags$i(class = "bi bi-exclamation-triangle me-1"),
                 "Peta Risiko Bencana (.shp)",
                 style = "font-weight: 600; margin-bottom: 4px;"),
          tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                     "Unggah vektor Risiko Bencana."),
          fileInput(ns("disaster_risk_file"), label = NULL,
                    accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"), multiple = TRUE),
          uiOutput(ns("risk_col_ui")),
          hr(),
          accordion(accordion_panel("Pengaturan lanjutan", icon = icon("gear"), open = FALSE,
                                    checkboxInput(ns("parallel"), "Aktifkan pemrosesan paralel", value = FALSE),
                                    numericInput(ns("workers"), "Jumlah kanal komputasi (cores)",
                                                 value = 2, min = 1, step = 1))),
          hr(),
          uiOutput(ns("output_dir_warning")),
          div(style = "display: flex; gap: 8px; flex-wrap: wrap;",
              actionButton(ns("btn_run"),
                           tagList(tags$i(class = "bi bi-play-fill me-1"),
                                   "Lakukan Analisis Ketahanan Iklim"),
                           class = "btn-success btn-sm"))
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

padu_ki_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    serasi_in <- serasi_input(input, output, session, output_dir)
    
    rv <- reactiveValues(
      analysis_result = NULL, gpkg_path = NULL,
      xlsx_path = NULL, log_messages = "",
      dr_vect = NULL, risk_col = NULL
    )
    
    extract_shp_path <- function(file_input) {
      shp_row <- file_input[grepl("\\.shp$", file_input$name, ignore.case = TRUE), ]
      validate(need(nrow(shp_row) == 1,
                    "Harap unggah semua komponen shapefile (.shp, .dbf, .prj, .shx)"))
      base_name <- tools::file_path_sans_ext(shp_row$name)
      temp_dir <- file.path(tempdir(), paste0("shp_", sample(1e9, 1)))
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
      for (i in seq_len(nrow(file_input))) {
        ext <- tools::file_ext(file_input$name[i])
        file.copy(file_input$datapath[i],
                  file.path(temp_dir, paste0(base_name, ".", ext)),
                  overwrite = TRUE)
      }
      file.path(temp_dir, paste0(base_name, ".shp"))
    }
    
    append_log <- function(msg) {
      rv$log_messages <- paste0(rv$log_messages, format(Sys.time(), "[%H:%M:%S] "), msg, "\n")
    }
    
    output$serasi_ui_container <- renderUI({ serasi_in$ui_block() })
    
    output$output_dir_warning <- renderUI({
      if (is.null(output_dir()) || !nzchar(output_dir())) {
        div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-exclamation-triangle me-1"),
            "Direktori output belum diatur. Atur terlebih dahulu di menu utama.")
      }
    })
    
    observeEvent(input$disaster_risk_file, {
      req(input$disaster_risk_file)
      tryCatch({
        shp_path <- extract_shp_path(input$disaster_risk_file)
        dr_sf <- load_and_validate_shapefile(shp_path)
        dr_sf <- ensure_geometry_name(dr_sf)
        rv$dr_vect <- dr_sf
        
        col_names <- names(dr_sf)
        col_names <- col_names[!col_names %in% c("geometry", "geom")]
        
        output$risk_col_ui <- renderUI({
          req(rv$dr_vect)
          selectInput(
            ns("risk_col_select"),
            label = "Pilih kolom atribut risiko bencana",
            choices = col_names,
            selected = if (!is.null(rv$risk_col) && rv$risk_col %in% col_names)
              rv$risk_col else col_names[1]
          )
        })
        showNotification("Peta Risiko Bencana berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$dr_vect <- NULL
        output$risk_col_ui <- renderUI(NULL)
        showNotification(paste("Gagal memuat Peta Risiko Bencana:", e$message), type = "error")
      })
    })
    
    observeEvent(input$risk_col_select, { rv$risk_col <- input$risk_col_select })
    
    observeEvent(input$btn_run, {
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification("Direktori output belum diatur.", type = "error", duration = 5); return()
      }
      
      raw_serasi <- serasi_in$idx_serasi_map()
      req(raw_serasi, input$disaster_risk_file, rv$risk_col)
      
      rv$analysis_result <- NULL; rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL; rv$log_messages <- ""
      
      append_log("Memulai analisis PADU-KI...")
      
      withProgress(message = "Menjalankan Analisis PADU-KI", value = 0, {
        tryCatch({
          incProgress(0.1, detail = "Memuat data...")
          if ("length" %in% colnames(raw_serasi)) {
            idx_map <- dissolve_id_pu(raw_serasi)
          } else {
            idx_map <- raw_serasi
          }
          dr_vect <- if (!is.null(rv$dr_vect)) rv$dr_vect else {
            tmp <- load_and_validate_shapefile(extract_shp_path(input$disaster_risk_file))
            ensure_geometry_name(tmp)
          }
          risk_col <- rv$risk_col
          append_log("Data berhasil dimuat.")
          append_log(paste("Kolom risiko yang digunakan:", risk_col))
          
          incProgress(0.1, detail = "Mempersiapkan perhitungan...")
          append_log("Menghitung indeks PADU-KI...")
          
          padu_ki <- calculate_padu_ki(
            idx_serasi_map     = idx_map,
            disaster_risk_vect = dr_vect,
            value_col          = risk_col,
            parallel           = input$parallel,
            workers            = input$workers
          )
          incProgress(0.6, detail = "Perhitungan selesai...")
          append_log("Perhitungan indeks selesai.")
          
          idx_padu_ki_map <- padu_ki$idx_padu_ki_map
          
          incProgress(0.1, detail = "Menyimpan hasil...")
          padu_ki_dir <- file.path(output_dir(), "Analisis PADU-KI")
          if (!dir.exists(padu_ki_dir))
            dir.create(padu_ki_dir, recursive = TRUE, showWarnings = FALSE)
          
          gpkg_path <- file.path(padu_ki_dir, "idx_padu_ki.gpkg")
          xlsx_path <- file.path(padu_ki_dir, "idx_padu_ki.xlsx")
          sf::st_write(idx_padu_ki_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          res_table <- as_tibble(sf::st_drop_geometry(idx_padu_ki_map))
          openxlsx::write.xlsx(res_table, xlsx_path)
          
          rv$gpkg_path <- gpkg_path; rv$xlsx_path <- xlsx_path
          rv$analysis_result <- list(map = idx_padu_ki_map, table = res_table)
          
          out <- list(
            inputs = list(
              start_time             = Sys.time(),
              idx_serasi_path        = serasi_in$filename(),
              idx_serasi_source      = serasi_in$source(),
              serasi_source_name     = serasi_in$filename(),
              serasi_source_hash     = serasi_in$hash(),
              disaster_risk_path     = input$disaster_risk_file,
              output_dir             = output_dir()
            ),
            result = list(
              idx_serasi_map      = idx_map,
              disaster_risk_vect  = dr_vect,
              idx_padu_ki_map     = idx_padu_ki_map,
              idx_padu_ki_table   = res_table
            )
          )
          
          log_dir <- file.path(padu_ki_dir, "log")
          if (!dir.exists(log_dir))
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
          tryCatch({
            inputs <- out$inputs
            save(inputs, file = file.path(log_dir, "idx_padu_ki_log.rda"))
          }, error = function(e) warning("Gagal menulis log: ", e$message))
          
          session$userData$module_results$padu_ki <- out
          
          plot_continuous_map(map = idx_padu_ki_map, column = "idx_padu_ki",
                              title = "Peta Indeks PADU-KI", legend = "Indeks PADU-KI",
                              low = "red", high = "lightgreen",
                              filepath = file.path(log_dir, "idx_padu_ki.png"))
          plot_categorical_map(map = dr_vect, title = "Peta Kerawanan Bencana",
                               column = risk_col, legend = "Tingkat Kerawanan",
                               filepath = file.path(log_dir, "kerawanan_bencana.png"))
          
          append_log(paste("Peta disimpan \u2192", gpkg_path))
          append_log(paste("Tabel disimpan \u2192", xlsx_path))
          append_log("Analisis PADU-KI berhasil diselesaikan.")
          
          incProgress(0.1, detail = "Selesai!")
          showNotification(paste("Analisis selesai. Hasil disimpan ke", gpkg_path),
                           type = "message", duration = 5)
        }, error = function(e) {
          msg <- conditionMessage(e)
          if (is.null(msg) || msg == "") msg <- "Error tidak diketahui"
          append_log(paste("ERROR:", msg))
          showNotification(paste("Analisis gagal:", msg), type = "error", duration = 10)
        })
      })
    })
    
    output$status_box <- renderUI({
      if (!is.null(rv$analysis_result)) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"), "Analisis selesai.")
      } else if (!is.null(serasi_in$idx_serasi_map()) && !is.null(input$disaster_risk_file)) {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"), "Siap menjalankan analisis.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"), "Unggah file dan klik Jalankan Analisis.")
      }
    })
    
    padu_ki_config <- list(
      map_color_col = "idx_padu_ki",
      map_title     = "Indeks PADU-KI",
      map_palette   = "RdYlGn",
      map_label_cols = c("ID PU" = "id_pu", "RTRW" = "RTRW",
                         "RZWP3K" = "RZWP3K", "Indeks PADU-KI" = "idx_padu_ki"),
      table_cols = c(
        "id_pu" = "ID PU", "RTRW" = "RTRW", "RZWP3K" = "RZWP3K",
        "admin" = "Administrasi", "area_ha" = "Luas (ha)",
        "idx_padu_ki" = "Indeks PADU-KI"),
      table_round_cols = c("Luas (ha)", "Indeks PADU-KI")
    )
    
    render_result_server(input, output, session, rv, padu_ki_config)
  })
}