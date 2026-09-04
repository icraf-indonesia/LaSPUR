# ui/modules/mod_padu_rtp.R
# ============================================================
#  MODULE: PADU-RTp (2.5 PADU-RTp)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── small UI helpers ──────────────────────────────────────────────────────────
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

# ── UI ──────────────────────────────────────────────────────────────────────
padu_rtp_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("2.5 PADU-RTp (Risiko dan Tekanan)", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menilai kepaduan lingkungan berdasarkan potensi risiko dan tekanan untuk menghasilkan nilai indeks PADU-RTp.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),
    
    fluidRow(
      class = "g-3",
      
      # ── Left column: Wizard (1/3) ────────────────────────────────────────
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
              title = "Langkah 2 — Menganalisis Risiko dan Tekanan",
              value = "step2",
              icon = tags$i(class = "bi bi-exclamation-triangle-fill"),
              uiOutput(ns("step2_ui"))
            )
          )
        )
      ),
      
      # ── Right column: Output & Hasil (2/3) ──────────────────────────────
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

# ── Server ──────────────────────────────────────────────────────────────────
padu_rtp_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Reactive values ────────────────────────────────────────────────────
    rv <- reactiveValues(
      unlocked = 1,
      
      # step1 data
      idx_serasi_map = NULL,
      ind_vect = NULL,
      pel_vect = NULL,
      industry_euc_dist = NULL,
      pelayaran_euc_dist = NULL,
      
      # analysis results
      analysis_result = NULL,
      gpkg_path = NULL,
      xlsx_path = NULL,
      log_messages = ""
    )
    
    go_to_panel <- function(value) {
      accordion_panel_set(id = "wizard", values = value, session = session)
    }
    
    # ── Robust helper to extract shapefile path ──────────────────────────
    extract_shp_path <- function(file_input) {
      shp_row <- file_input[grepl("\\.shp$", file_input$name, ignore.case = TRUE), ]
      validate(need(
        nrow(shp_row) == 1,
        "Harap unggah semua komponen shapefile (.shp, .dbf, .prj, .shx)"
      ))
      base_name <- tools::file_path_sans_ext(shp_row$name)
      temp_dir <- file.path(tempdir(), paste0("shp_", sample(1e9, 1)))
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
      for (i in seq_len(nrow(file_input))) {
        ext <- tools::file_ext(file_input$name[i])
        new_path <- file.path(temp_dir, paste0(base_name, ".", ext))
        file.copy(file_input$datapath[i], new_path, overwrite = TRUE)
      }
      file.path(temp_dir, paste0(base_name, ".shp"))
    }
    
    # ── Helper to extract vector path (gpkg or shp) ──────────────────────
    extract_vector_path <- function(file_input) {
      gpkg_row <- file_input[grepl("\\.gpkg$", file_input$name, ignore.case = TRUE), ]
      if (nrow(gpkg_row) == 1) return(gpkg_row$datapath)
      extract_shp_path(file_input)
    }
    
    # ── Log helper ────────────────────────────────────────────────────────
    append_log <- function(msg) {
      rv$log_messages <- paste0(rv$log_messages, format(Sys.time(), "[%H:%M:%S] "), msg, "\n")
    }
    
    # ── Dynamic UI for Step 1 ─────────────────────────────────────────────
    output$step1_ui <- renderUI({
      tagList(
        tags$p(tags$i(class = "bi bi-info-circle me-1"), "Peta Indeks SERASI (.gpkg atau .shp)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "Output dari modul 'Identifikasi Konflik Spasial' (idx_serasi.gpkg). Menerima .gpkg atau .shp."),
        fileInput(ns("idx_serasi_file"), label = NULL,
                  accept = c(".gpkg", ".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-building me-1"), "1. Peta Jarak ke Industri",
               style = "font-weight: 600; margin-bottom: 4px;"),
        radioButtons(ns("ind_input_type"), label = NULL,
                     choices = c("Unggah Vektor (hitung jarak otomatis)" = "vector",
                                 "Unggah Raster Jarak yang Sudah Ada (.tif)" = "raster"),
                     selected = "raster",
                     inline = FALSE),
        uiOutput(ns("ui_ind_file")),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-water me-1"), "2. Peta Jarak ke Alur Pelayaran",
               style = "font-weight: 600; margin-bottom: 4px;"),
        radioButtons(ns("pel_input_type"), label = NULL,
                     choices = c("Unggah Vektor (hitung jarak otomatis)" = "vector",
                                 "Unggah Raster Jarak yang Sudah Ada (.tif)" = "raster"),
                     selected = "raster",
                     inline = FALSE),
        uiOutput(ns("ui_pel_file")),
        
        hr(),
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Langkah 2")
      )
    })
    
    output$ui_ind_file <- renderUI({
      ns <- session$ns
      if (input$ind_input_type == "vector") {
        tagList(
          fileInput(ns("ind_file_vect"), "Shapefile Industri",
                    accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"), multiple = TRUE),
          numericInput(ns("ind_calc_resolution"), "Resolusi Perhitungan Jarak (m)", value = 100, min = 1),
          actionButton(ns("btn_calc_ind_dist"),
                       tagList(tags$i(class = "bi bi-calculator me-1"), "Buat Peta Jarak Industri"),
                       class = "btn-outline-primary btn-sm"),
          uiOutput(ns("ind_dist_status"))
        )
      } else {
        fileInput(ns("ind_file_rast"), "Raster Industri (.tif)",
                  accept = c(".tif"), multiple = FALSE)
      }
    })
    
    output$ui_pel_file <- renderUI({
      ns <- session$ns
      if (input$pel_input_type == "vector") {
        tagList(
          fileInput(ns("pel_file_vect"), "Shapefile Alur Pelayaran",
                    accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"), multiple = TRUE),
          numericInput(ns("pel_calc_resolution"), "Resolusi Perhitungan Jarak (m)", value = 100, min = 1),
          actionButton(ns("btn_calc_pel_dist"),
                       tagList(tags$i(class = "bi bi-calculator me-1"), "Buat Peta Jarak Alur Pelayaran"),
                       class = "btn-outline-primary btn-sm"),
          uiOutput(ns("pel_dist_status"))
        )
      } else {
        fileInput(ns("pel_file_rast"), "Raster Alur Pelayaran (.tif)",
                  accept = c(".tif"), multiple = FALSE)
      }
    })
    
    # ── Load SERASI map ──────────────────────────────────────────────────
    observeEvent(input$idx_serasi_file, {
      req(input$idx_serasi_file)
      tryCatch({
        path <- extract_vector_path(input$idx_serasi_file)
        sf_obj <- load_and_validate_shapefile(path)
        sf_obj <- ensure_geometry_name(sf_obj)  
        rv$idx_serasi <- sf_obj
        
        # Conditional dissolve idx_serasi_map
        if ("length" %in% colnames(rv$idx_serasi)) {
          rv$idx_serasi_map <- dissolve_id_pu(rv$idx_serasi)
        } else {
          rv$idx_serasi_map <- rv$idx_serasi 
        }
        
        showNotification("Peta Indeks SERASI berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$idx_serasi_map <- NULL
        showNotification(paste("Gagal memuat peta SERASI:", e$message), type = "error")
      })
    })
    
    # ── Process industry data ─────────────────────────────────────────────
    # Store loaded vector in rv so button handler can access it
    observeEvent(input$ind_file_vect, {
      req(input$ind_file_vect)
      tryCatch({
        ind_vect <- load_and_validate_shapefile(extract_shp_path(input$ind_file_vect))
        rv$ind_vect <- ensure_geometry_name(ind_vect)
        showNotification("Shapefile industri berhasil dimuat. Klik 'Buat Peta Jarak Industri' untuk menghitung.", type = "message")
      }, error = function(e) {
        rv$ind_vect <- NULL
        showNotification(paste("Gagal memuat shapefile industri:", e$message), type = "error")
      })
    })
    
    observeEvent(input$btn_calc_ind_dist, {
      if (is.null(rv$idx_serasi_map)) {
        showNotification(
          "Harap unggah Peta Indeks SERASI terlebih dahulu.",
          type = "warning", duration = 6
        )
        return()
      }
      if (is.null(rv$ind_vect)) {
        showNotification("Harap unggah shapefile industri terlebih dahulu.", type = "warning")
        return()
      }
      withProgress(message = "Menghitung jarak ke Industri...", value = 0.3, {
        tryCatch({
          append_log("Menghitung jarak Euclidean dari vektor industri...")
          rv$industry_euc_dist <- calculate_euclidean_dist(
            rv$ind_vect,
            rv$idx_serasi_map,
            resolution = input$ind_calc_resolution
          )
          
          # Save industry distance raster
          if (!is.null(rv$industry_euc_dist)) {
            industry_raster_path <- file.path(output_dir(), "industry_euc_dist.tif")
            terra::writeRaster(rv$industry_euc_dist, industry_raster_path, overwrite = TRUE)
            append_log(paste("Raster jarak industri disimpan →", industry_raster_path))
          }
          
          incProgress(1, detail = "Selesai!")
          showNotification("Raster jarak industri berhasil dihitung.", type = "message")
        }, error = function(e) {
          rv$industry_euc_dist <- NULL
          showNotification(paste("Gagal memproses industri:", e$message), type = "error")
        })
      })
    })
    
    output$ind_dist_status <- renderUI({
      if (!is.null(rv$industry_euc_dist)) {
        div(class = "alert alert-success mt-2 mb-0 py-1 px-2", style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-check-circle me-1"), "Peta jarak industri siap.")
      }
    })
    
    observeEvent(input$ind_file_rast, {
      req(input$ind_input_type == "raster", input$ind_file_rast)
      tryCatch({
        rv$industry_euc_dist <- terra::rast(input$ind_file_rast$datapath)
        showNotification("Raster jarak industri berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$industry_euc_dist <- NULL
        showNotification(paste("Gagal memuat raster industri:", e$message), type = "error")
      })
    })
    
    # ── Process shipping lane data ──────────────────────────────────────
    # Store loaded vector in rv so button handler can access it
    observeEvent(input$pel_file_vect, {
      req(input$pel_file_vect)
      tryCatch({
        pel_vect <- load_and_validate_shapefile(extract_shp_path(input$pel_file_vect))
        rv$pel_vect <- ensure_geometry_name(pel_vect)
        showNotification("Shapefile alur pelayaran berhasil dimuat. Klik 'Buat Peta Jarak Alur Pelayaran' untuk menghitung.", type = "message")
      }, error = function(e) {
        rv$pel_vect <- NULL
        showNotification(paste("Gagal memuat shapefile alur pelayaran:", e$message), type = "error")
      })
    })
    
    observeEvent(input$btn_calc_pel_dist, {
      if (is.null(rv$idx_serasi_map)) {
        showNotification(
          "Harap unggah Peta Indeks SERASI terlebih dahulu.",
          type = "warning", duration = 6
        )
        return()
      }
      if (is.null(rv$pel_vect)) {
        showNotification("Harap unggah shapefile alur pelayaran terlebih dahulu.", type = "warning")
        return()
      }
      withProgress(message = "Menghitung jarak ke Alur Pelayaran...", value = 0.3, {
        tryCatch({
          append_log("Menghitung jarak Euclidean dari vektor alur pelayaran...")
          rv$pelayaran_euc_dist <- calculate_euclidean_dist(
            rv$pel_vect,
            rv$idx_serasi_map,
            resolution = input$pel_calc_resolution
          )
          
          # Save shipping lane distance raster
          if (!is.null(rv$pelayaran_euc_dist)) {
            pelayaran_raster_path <- file.path(output_dir(), "pelayaran_euc_dist.tif")
            terra::writeRaster(rv$pelayaran_euc_dist, pelayaran_raster_path, overwrite = TRUE)
            append_log(paste("Raster jarak alur pelayaran disimpan →", pelayaran_raster_path))
          }
          
          incProgress(1, detail = "Selesai!")
          showNotification("Raster jarak alur pelayaran berhasil dihitung.", type = "message")
        }, error = function(e) {
          rv$pelayaran_euc_dist <- NULL
          showNotification(paste("Gagal memproses alur pelayaran:", e$message), type = "error")
        })
      })
    })
    
    output$pel_dist_status <- renderUI({
      if (!is.null(rv$pelayaran_euc_dist)) {
        div(class = "alert alert-success mt-2 mb-0 py-1 px-2", style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-check-circle me-1"), "Peta jarak alur pelayaran siap.")
      }
    })
    
    observeEvent(input$pel_file_rast, {
      req(input$pel_input_type == "raster", input$pel_file_rast)
      tryCatch({
        rv$pelayaran_euc_dist <- terra::rast(input$pel_file_rast$datapath)
        showNotification("Raster jarak alur pelayaran berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$pelayaran_euc_dist <- NULL
        showNotification(paste("Gagal memuat raster alur pelayaran:", e$message), type = "error")
      })
    })
    
    # ── Step 1 -> Step 2 ──────────────────────────────────────────────────
    observeEvent(input$btn_next_1, {
      # Validate SERASI map
      if (is.null(rv$idx_serasi_map)) {
        showNotification("Harap unggah peta SERASI.", type = "warning")
        return()
      }
      
      # Validate industry data
      if (input$ind_input_type == "vector") {
        if (is.null(input$ind_file_vect) || is.null(rv$industry_euc_dist)) {
          showNotification("Harap unggah shapefile industri dan pastikan jarak berhasil dihitung.", type = "warning")
          return()
        }
      } else {
        if (is.null(input$ind_file_rast) || is.null(rv$industry_euc_dist)) {
          showNotification("Harap unggah raster jarak industri.", type = "warning")
          return()
        }
      }
      
      # Validate shipping lane data
      if (input$pel_input_type == "vector") {
        if (is.null(input$pel_file_vect) || is.null(rv$pelayaran_euc_dist)) {
          showNotification("Harap unggah shapefile alur pelayaran dan pastikan jarak berhasil dihitung.", type = "warning")
          return()
        }
      } else {
        if (is.null(input$pel_file_rast) || is.null(rv$pelayaran_euc_dist)) {
          showNotification("Harap unggah raster jarak alur pelayaran.", type = "warning")
          return()
        }
      }
      
      rv$unlocked <- max(rv$unlocked, 2)
      go_to_panel("step2")
    })
    
    # ── Step 2 UI ──────────────────────────────────────────────────────────
    output$step2_ui <- renderUI({
      if (rv$unlocked < 2) return(.locked_panel())
      
      tagList(
        tags$p(tags$i(class = "bi bi-sliders me-1"), "Parameter",
               style = "font-weight: 600; margin-bottom: 4px;"),
        numericInput(ns("max_ind_dist"), "Skala Jarak Maksimum Industri (m)", value = 8000, min = 1),
        numericInput(ns("max_pel_dist"), "Skala Jarak Maksimum Alur Pelayaran (m)", value = 5000, min = 1),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Catatan: Parameter ini disediakan untuk referensi; fungsi perhitungan saat ini menggunakan nilai bawaan."
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
                               "Jalankan Analisis PADU-RTp"),
                       class = "btn-success btn-sm")
        ),
        
        .step_nav(ns, back_id = "btn_back_2", next_id = NULL)
      )
    })
    
    observeEvent(input$btn_back_2, {
      go_to_panel("step1")
    })
    
    # ── Run analysis (with progress) ──────────────────────────────────────
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
      
      req(rv$idx_serasi_map, rv$industry_euc_dist, rv$pelayaran_euc_dist)
      
      # Reset previous results
      rv$analysis_result <- NULL
      rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL
      rv$log_messages <- ""
      
      append_log("Memulai analisis PADU-RTp...")
      
      withProgress(message = "Menjalankan Analisis PADU-RTp", value = 0, {
        
        tryCatch({
          incProgress(0.1, detail = "Memuat data...")
          idx_map <- rv$idx_serasi_map
          append_log("Peta SERASI berhasil dimuat.")
          
          # Step 2: Calculate PADU-RTp (progress 20% → 80%)
          incProgress(0.1, detail = "Mempersiapkan perhitungan...")
          append_log("Menghitung indeks PADU-RTp...")
          
          padu_rtp <- calculate_padu_rtp(
            idx_serasi_map      = idx_map,
            industry_euc_dist   = rv$industry_euc_dist,
            pelayaran_euc_dist  = rv$pelayaran_euc_dist,
            industry_max_dist   = input$max_ind_dist,
            pelayaran_max_dist  = input$max_pel_dist,
          )
          
          incProgress(0.6, detail = "Perhitungan selesai...")
          append_log("Perhitungan indeks selesai.")
          
          idx_padu_rtp_map <- padu_rtp$idx_padu_rtp_map
          
          # Step 3: Save results (progress 90%)
          incProgress(0.1, detail = "Menyimpan hasil...")
          
          padu_rtp_dir <- file.path(output_dir(), "Analisis PADU-RTp")
          if (!dir.exists(padu_rtp_dir)) {
            dir.create(padu_rtp_dir, recursive = TRUE, showWarnings = FALSE)
          }
          
          if (!dir.exists(padu_rtp_dir)) {
            stop("Tidak dapat membuat atau mengakses direktori: ", padu_rtp_dir)
          }
          
          gpkg_path <- file.path(padu_rtp_dir, "idx_padu_rtp.gpkg")
          xlsx_path <- file.path(padu_rtp_dir, "idx_padu_rtp.xlsx")
          
          sf::st_write(idx_padu_rtp_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          res_table <- as_tibble(sf::st_drop_geometry(idx_padu_rtp_map))
          openxlsx::write.xlsx(res_table, xlsx_path)
          
          rv$gpkg_path <- gpkg_path
          rv$xlsx_path <- xlsx_path
          rv$analysis_result <- list(map = idx_padu_rtp_map, table = res_table)
          
          # ─── Store result for report generation ───
          out <- list(
            inputs = list(
              start_time = Sys.time(),
              idx_serasi_path = input$idx_serasi_file,
              ind_shp_path = input$ind_file_vect,
              ind_tif_path = input$ind_file_rast,
              pel_shp_path = input$pel_file_vect,
              pel_tif_path =input$pel_file_rast,
              max_ind_dist = input$max_ind_dist,
              max_pel_dist = input$max_pel_dist,
              output_dir = output_dir()
            ),
            result = list(
              idx_serasi_map = idx_map,
              industry_euc_dist   = rv$industry_euc_dist,
              pelayaran_euc_dist  = rv$pelayaran_euc_dist,
              idx_padu_rtp_map = idx_padu_rtp_map,
              idx_padu_rtp_table = res_table
            )
          )
          
          # Export log
          log_dir <- file.path(padu_rtp_dir, "log")
          if (!dir.exists(log_dir)) {
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
          }
          log_path <- file.path(log_dir, "idx_padu_rtp_log.rda")
          if (dir.exists(log_dir)) {
            tryCatch({
              inputs <- out$inputs
              save(inputs, file = log_path)
            }, error = function(e) {
              warning("Gagal menulis file log: ", e$message)
            })
          } else {
            warning("Direktori log tidak tersedia, lewati penulisan log.")
          }
          
          # Store in shared environment
          session$userData$module_results$padu_rtp <- out
          
          # Export static maps 
          idx_padu_rtp_viz <- plot_continuous_map(
            map      = idx_padu_rtp_map,
            column   = "idx_padu_rtp",         
            title    = "Peta Indeks PADU-RTp",
            legend   = "Indeks PADU-RTp",
            low      = "red",
            high     = "lightgreen",
            filepath = file.path(log_dir, "idx_padu_rtp.png")
          )
          
          euc_industry_viz <- plot_continuous_map(
            map      = rv$industry_euc_dist,
            column   = NA,        
            title    = "Peta Jarak ke Industri",
            legend   = "Meter",
            low      = "darkblue",
            high     = "yellow",
            filepath = file.path(log_dir, "jarak_ke_industri.png")
          )
          
          euc_alur_pelayaran_viz <- plot_continuous_map(
            map      = rv$pelayaran_euc_dist,
            column   = NA,        
            title    = "Peta Jarak ke Alur Pelayaran",
            legend   = "Meter",
            low      = "darkblue",
            high     = "yellow",
            filepath = file.path(log_dir, "jarak_ke_alur_pelayaran.png")
          )
          
          append_log(paste("Peta disimpan →", gpkg_path))
          append_log(paste("Tabel disimpan →", xlsx_path))
          append_log("Analisis PADU-RTp berhasil diselesaikan.")
          
          incProgress(0.1, detail = "Selesai!")
          showNotification(paste("Analisis selesai. Hasil disimpan ke", gpkg_path),
                           type = "message", duration = 5)
          
        }, error = function(e) {
          msg <- conditionMessage(e)
          if (is.null(msg) || msg == "") msg <- "Error tidak diketahui (lihat konsol untuk detail)"
          append_log(paste("ERROR:", msg))
          showNotification(paste("Analisis gagal:", msg), type = "error", duration = 10)
        })
        
      }) # end withProgress
    })
    
    # ── Status box ─────────────────────────────────────────────────────────
    output$status_box <- renderUI({
      if (!is.null(rv$analysis_result)) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Analisis selesai.")
      } else if (rv$unlocked >= 2) {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Siap menjalankan analisis.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Lengkapi langkah sebelumnya.")
      }
    })
    
    # ── Result Visualization ──────────────────────────────────────────────
    padu_rtp_config <- list(
      map_color_col = "idx_padu_rtp",
      map_title = "Indeks PADU-RTp",
      map_palette = "RdYlGn",
      map_label_cols = c(
        "ID PU" = "id_pu",
        "RTRW" = "RTRW",
        "RZWP3K" = "RZWP3K",
        "Indeks PADU-RTp" = "idx_padu_rtp"
      ),
      table_cols = c(
        "id_pu" = "ID PU",
        "RTRW" = "RTRW",
        "RZWP3K" = "RZWP3K",
        "admin" = "Administrasi",
        "area_ha" = "Luas (ha)",
        "pelayaran_dist_mean" = "Jarak ke Alur Pelayaran (m)",
        "industry_dist_mean" = "Jarak ke Area Industri (m)",
        "idx_padu_rtp" = "Indeks PADU-RTp"
      ),
      table_round_cols = c(
        "Luas (ha)",
        "Jarak ke Alur Pelayaran (m)",
        "Jarak ke Area Industri (m)",
        "Indeks PADU-RTp"
      )
    )

    render_result_server(input, output, session, rv, padu_rtp_config)
    
  })
}