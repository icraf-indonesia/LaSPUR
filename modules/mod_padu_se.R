# ui/modules/mod_padu_se.R
# ============================================================
#  MODULE: PADU-SE (2.6 Social Economy Analysis)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── UI ──────────────────────────────────────────────────────────
padu_se_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("2.6 PADU-SE (Sosial & Ekonomi)", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menilai kepaduan wilayah berdasarkan aktivitas sosial dan ekonomi untuk menghasilkan nilai indeks PADU-SE.",
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
          
          tags$p(tags$i(class = "bi bi-info-circle me-1"),
                 "Peta Indeks SERASI (.gpkg atau .shp)",
                 style = "font-weight: 600; margin-bottom: 4px;"),
          tags$small(
            style = "color: #6c757d; display: block; margin-bottom: 8px;",
            "Output dari modul 'Identifikasi Konflik Spasial' (idx_serasi.gpkg). Menerima .gpkg atau .shp."
          ),
          fileInput(ns("idx_serasi_file"),
                    label    = NULL,
                    accept   = c(".gpkg", ".shp", ".dbf", ".prj", ".shx", ".cpg"),
                    multiple = TRUE),
          
          hr(),
          
          tags$p(tags$i(class = "bi bi-shield-check me-1"),
                 "Peta Cahaya Malam (Night Time Light) (.tif)",
                 style = "font-weight: 600; margin-bottom: 4px;"),
          tags$small(
            style = "color: #6c757d; display: block; margin-bottom: 8px;",
            "Layer raster cahaya malam yang akan ditumpangtindihkan dengan unit perencanaan."
          ),
          fileInput(ns("ntl_area_file"),
                    label    = NULL,
                    accept   = c(".tif", ".tiff", ".geotiff"),
                    multiple = FALSE),
          
          tags$p(tags$i(class = "bi bi-shield-check me-1"),
                 "Peta Kepadatan Populasi Penduduk (.tif)",
                 style = "font-weight: 600; margin-bottom: 4px;"),
          tags$small(
            style = "color: #6c757d; display: block; margin-bottom: 8px;",
            "Layer raster populasi penduduk per satuan luas yang akan ditumpangtindihkan dengan unit perencanaan."
          ),
          fileInput(ns("popdens_area_file"),
                    label    = NULL,
                    accept   = c(".tif", ".tiff", ".geotiff"),
                    multiple = FALSE),
          
          hr(),
          
          div(
            style = "display: flex; gap: 8px; flex-wrap: wrap;",
            actionButton(ns("btn_run"),
                         tagList(tags$i(class = "bi bi-play-fill me-1"),
                                 "Lakukan Analisis PADU-SE"),
                         class = "btn-success btn-sm")
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
          
          create_result_ui(ns)
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────────
padu_se_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Reactive values ──────────────────────────────────────────
    rv <- reactiveValues(
      analysis_result = NULL,
      gpkg_path = NULL,
      xlsx_path = NULL,
      log_messages = ""
    )
    
    # ── Robust helper to extract shapefile path ────────────────
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
    
    # ── Helper to extract vector path (gpkg or shp) ─────────────
    extract_vector_path <- function(file_input) {
      gpkg_row <- file_input[grepl("\\.gpkg$", file_input$name, ignore.case = TRUE), ]
      if (nrow(gpkg_row) == 1) return(gpkg_row$datapath)
      extract_shp_path(file_input)
    }
    
    # ── Log helper ──────────────────────────────────────────────
    append_log <- function(msg) {
      rv$log_messages <- paste0(rv$log_messages, format(Sys.time(), "[%H:%M:%S] "), msg, "\n")
    }
    
    # ── Run analysis ──────────────────────────────────────────
    observeEvent(input$btn_run, {
      
      # Ensure output_dir is a single path 
      out_dir <- output_dir()
      if (length(out_dir) > 1) {
        out_dir <- out_dir[1]
      }
      
      # Check output directory 
      if (is.null(out_dir) || !nzchar(out_dir) || !validate_output_dir(out_dir)) {
        showNotification(
          "Direktori output belum diatur atau tidak valid. Harap atur direktori output terlebih dahulu.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      req(input$idx_serasi_file, input$ntl_area_file, input$popdens_area_file)
      
      # Reset previous results
      rv$analysis_result <- NULL
      rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL
      rv$log_messages <- ""
      
      append_log("Memulai analisis PADU-SE...")
      
      withProgress(message = "Menjalankan Analisis PADU-SE", value = 0, {
        
        tryCatch({
          # Step 1: Load data (progress 10%)
          incProgress(0.1, detail = "Memuat data...")
          pu_raw <- load_and_validate_shapefile(extract_vector_path(input$idx_serasi_file))
          pu_raw <- ensure_geometry_name(pu_raw)  
          
          # Conditional dissolve idx_serasi_map
          if ("length" %in% colnames(pu_raw)) {
            pu <- dissolve_id_pu(pu_raw)
          } else {
            pu <- pu_raw  
          }
          
          ntl_map <- load_and_validate_raster(input$ntl_area_file$datapath)
          popdens_map <- load_and_validate_raster(input$popdens_area_file$datapath)
          
          id_col = "id_pu"
          append_log("Data berhasil dimuat.")
          
          # Step 2: Extract data (progress 20% → 70%)
          incProgress(0.1, detail = "Mengekstrak nilai...")
          append_log("Menghitung nilai agregat NTL dan kepadatan populasi yang berada dalam area tumpang tindih...")
          
          # Extract raster to pu
          ntl_extracted <- extract_raster_to_sf(
            pu = pu,
            rast = ntl_map,
            id_col = id_col,
            new_col = "ntl"
          )
          
          popdens_extracted <- extract_raster_to_sf(
            pu = pu,
            rast = popdens_map,
            id_col = id_col,
            new_col = "popdens"
          )
          
          incProgress(0.5, detail = "Pemrosesan selesai...")
          
          # Step 3: Calculate index (progress 70%-80%)
          incProgress(0.1, detail = "Mengekstrak nilai...")
          ntl_norm <- ntl_extracted %>%
            mutate(ntl_norm = (ntl - min(ntl)) / (max(ntl) - min(ntl)))
          
          popdens_norm <- popdens_extracted %>%
            mutate(popdens_norm = (popdens - min(popdens)) / (max(popdens) - min(popdens)))
          
          # Join the data
          popdens_subset <- popdens_norm %>%
            dplyr::select(id_pu, popdens, popdens_norm) %>% 
            sf::st_drop_geometry()
          
          idx_padu_se_map <- ntl_norm %>% 
            dplyr::left_join(popdens_subset, by = "id_pu") %>% 
            dplyr::mutate(
              idx_padu_se = (ntl_norm + popdens_norm) / 2
            )
          
          incProgress(0.5, detail = "Pemrosesan selesai...")
          
          # Step 4: Save results (progress 90%)
          incProgress(0.1, detail = "Menyimpan hasil...")
          padu_se_dir <- file.path(output_dir(), "Analisis PADU-SE")
          if (!dir.exists(padu_se_dir)) {
            dir.create(padu_se_dir, recursive = TRUE, showWarnings = FALSE)
          }
          
          if (!dir.exists(padu_se_dir)) {
            stop("Tidak dapat membuat atau mengakses direktori: ", padu_se_dir)
          }
          
          gpkg_path <- file.path(padu_se_dir, "idx_padu_se.gpkg")
          xlsx_path <- file.path(padu_se_dir, "idx_padu_se.xlsx")
          
          sf::st_write(idx_padu_se_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          res_table <- as_tibble(sf::st_drop_geometry(idx_padu_se_map))
          openxlsx::write.xlsx(res_table, xlsx_path)
          
          rv$gpkg_path <- gpkg_path
          rv$xlsx_path <- xlsx_path
          rv$analysis_result <- list(map = idx_padu_se_map, table = res_table)
          
          # ─── Store result for report generation ───
          out <- list(
            inputs = list(
              start_time = Sys.time(),
              idx_serasi_path = input$idx_serasi_file,
              ntl_path = input$ntl_area_file, 
              popdens_area_path = input$popdens_area_file,
              output_dir = output_dir()
            ),
            result = list(
              idx_serasi_map = pu,
              ntl_map = ntl_map,
              popdens_map = popdens_map,
              idx_padu_se_map = idx_padu_se_map,
              idx_padu_se_table = res_table
            )
          )
          
          # Export log
          log_dir <- file.path(padu_se_dir, "log")
          if (!dir.exists(log_dir)) {
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
          }
          log_path <- file.path(log_dir, "idx_padu_se_log.rda")
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
          session$userData$module_results$padu_se <- out
          
          # Export static maps 
          idx_padu_se_viz <- plot_continuous_map(
            map      = idx_padu_se_map,
            column   = "idx_padu_se",         
            title    = "Peta Indeks PADU-SE",
            legend   = "Indeks PADU-SE",
            low      = "red",
            high     = "lightgreen",
            filepath = file.path(log_dir, "idx_padu_se.png")
          )
          
          ntl_viz <- plot_continuous_map(
            map      = ntl_map,
            column   = NA,        
            title    = "Peta Cahaya Malam (Nigth time light)",
            legend   = "nanoWatts/sr/cm²",
            low      = "darkblue",
            high     = "yellow",
            filepath = file.path(log_dir, "cahaya_malam.png")
          )
          
          popdens_viz <- plot_continuous_map(
            map      = popdens_map,
            column   = NA,        
            title    = "Peta Kepadatan Populasi Penduduk",
            legend   = "Penduduk (jiwa/ha)",
            low      = "darkblue",
            high     = "yellow",
            filepath = file.path(log_dir, "kepadatan_penduduk.png")
          )

          append_log(paste("Peta disimpan →", gpkg_path))
          append_log(paste("Tabel disimpan →", xlsx_path))
          append_log("Analisis PADU-SE berhasil diselesaikan.")
          
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
    
    # ── Status box ─────────────────────────────────────────────
    output$status_box <- renderUI({
      if (!is.null(rv$analysis_result)) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Analisis selesai.")
      } else if (!is.null(input$idx_serasi_file) && !is.null(input$popdens_area_file)) {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Siap menjalankan analisis.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Unggah file dan klik Jalankan Analisis.")
      }
    })
    
    # ── Result Visualization ───────────────────────────────────
    padu_se_config <- list(
      map_color_col = "idx_padu_se",
      map_title = "Indeks PADU-SE",
      map_palette = "RdYlGn",
      map_label_cols = c(
        "ID PU" = "id_pu",
        "RTRW" = "RTRW",
        "RZWP3K" = "RZWP3K",
        "Indeks PADU-SE" = "idx_padu_se"
      ),
      table_cols = c(
        "id_pu" = "ID PU",
        "RTRW" = "RTRW",
        "RZWP3K" = "RZWP3K",
        "admin" = "Administrasi",
        "area_ha" = "Luas (ha)",
        "ntl" = "Cahaya Malam (nanoWatts/sr/cm²)",
        "popdens" = "Kepadatan Penduduk (jiwa/ha)",
        "idx_padu_se" = "Indeks PADU-SE"
      ),
      table_round_cols = c(
        "Luas (ha)", 
        "Cahaya Malam (nanoWatts/sr/cm²)", 
        "Kepadatan Penduduk (jiwa/ha)", 
        "Indeks PADU-SE"
      )
    )
    
    render_result_server(input, output, session, rv, padu_se_config)
    
  })
}