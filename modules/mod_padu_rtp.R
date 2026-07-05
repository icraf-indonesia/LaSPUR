# ui/modules/mod_padu_rtp.R
# ============================================================
#  MODULE: PADU-RTp (2.5 PADU-RTp)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── small UI helpers ────────────────────────────────────────────
.locked_panel <- function(msg = "Selesaikan tahap sebelumnya terlebih dahulu.") {
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
              title = "Tahap 1 — Menyiapkan Data Utama",
              value = "step1",
              icon = tags$i(class = "bi bi-folder-fill"),
              uiOutput(ns("step1_ui"))
            ),
            
            accordion_panel(
              title = "Tahap 2 — Menganalisis Risiko dan Tekanan",
              value = "step2",
              icon = tags$i(class = "bi bi-exclamation-triangle-fill"),
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
          
          navset_tab(
            nav_panel(
              "Peta",
              leafletOutput(ns("result_map"), height = "500px")
            ),
            nav_panel(
              "Tabel",
              div(
                style = "height: 500px; overflow: auto;",
                DT::DTOutput(ns("result_table"))
              )
            ),
            nav_panel(
              "Log Validasi",
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
padu_rtp_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Reactive values ──────────────────────────────────────────
    rv <- reactiveValues(
      unlocked = 1,
      
      # step1 data
      idx_serasi_map = NULL,
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
    
    # ── Dynamic UI for Step 1 ─────────────────────────────────
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
                     inline = FALSE),
        uiOutput(ns("ui_ind_file")),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-water me-1"), "2. Peta Jarak ke Alur Pelayaran",
               style = "font-weight: 600; margin-bottom: 4px;"),
        radioButtons(ns("pel_input_type"), label = NULL,
                     choices = c("Unggah Vektor (hitung jarak otomatis)" = "vector",
                                 "Unggah Raster Jarak yang Sudah Ada (.tif)" = "raster"),
                     inline = FALSE),
        uiOutput(ns("ui_pel_file")),
        
        hr(),
        
        numericInput(ns("calc_resolution"), "Resolusi Perhitungan Jarak Otomatis (m)", value = 30, min = 1),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Hanya digunakan jika opsi 'Unggah Vektor' dipilih di atas."
        ),
        
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Tahap 2")
      )
    })
    
    output$ui_ind_file <- renderUI({
      ns <- session$ns
      if (input$ind_input_type == "vector") {
        fileInput(ns("ind_file_vect"), "Shapefile Industri",
                  accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"), multiple = TRUE)
      } else {
        fileInput(ns("ind_file_rast"), "Raster Industri (.tif)",
                  accept = c(".tif"), multiple = FALSE)
      }
    })
    
    output$ui_pel_file <- renderUI({
      ns <- session$ns
      if (input$pel_input_type == "vector") {
        fileInput(ns("pel_file_vect"), "Shapefile Alur Pelayaran",
                  accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"), multiple = TRUE)
      } else {
        fileInput(ns("pel_file_rast"), "Raster Alur Pelayaran (.tif)",
                  accept = c(".tif"), multiple = FALSE)
      }
    })
    
    # ── Load SERASI map ─────────────────────────────────────────
    observeEvent(input$idx_serasi_file, {
      req(input$idx_serasi_file)
      tryCatch({
        path <- extract_vector_path(input$idx_serasi_file)
        rv$idx_serasi_map <- load_and_validate_shapefile(path)
        showNotification("Peta Indeks SERASI berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$idx_serasi_map <- NULL
        showNotification(paste("Gagal memuat peta SERASI:", e$message), type = "error")
      })
    })
    
    # ── Process industry data ──────────────────────────────────
    observeEvent(input$ind_file_vect, {
      req(input$ind_input_type == "vector", input$ind_file_vect, rv$idx_serasi_map)
      tryCatch({
        ind_vect <- load_and_validate_shapefile(extract_shp_path(input$ind_file_vect))
        append_log("Menghitung jarak Euclidean dari vektor industri...")
        rv$industry_euc_dist <- calculate_euclidean_dist(
          ind_vect,
          rv$idx_serasi_map,
          resolution = input$calc_resolution
        )
        showNotification("Raster jarak industri berhasil dihitung.", type = "message")
      }, error = function(e) {
        rv$industry_euc_dist <- NULL
        showNotification(paste("Gagal memproses industri:", e$message), type = "error")
      })
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
    
    # ── Process shipping lane data ─────────────────────────────
    observeEvent(input$pel_file_vect, {
      req(input$pel_input_type == "vector", input$pel_file_vect, rv$idx_serasi_map)
      tryCatch({
        pel_vect <- load_and_validate_shapefile(extract_shp_path(input$pel_file_vect))
        append_log("Menghitung jarak Euclidean dari vektor alur pelayaran...")
        rv$pelayaran_euc_dist <- calculate_euclidean_dist(
          pel_vect,
          rv$idx_serasi_map,
          resolution = input$calc_resolution
        )
        showNotification("Raster jarak alur pelayaran berhasil dihitung.", type = "message")
      }, error = function(e) {
        rv$pelayaran_euc_dist <- NULL
        showNotification(paste("Gagal memproses alur pelayaran:", e$message), type = "error")
      })
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
    
    # ── Step 1 -> Step 2 ──────────────────────────────────────
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
    
    # ── Step 2 UI ──────────────────────────────────────────────
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
    
    # ── Run analysis (with progress) ──────────────────────────
    observeEvent(input$btn_run, {
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
          
          # FIXED: Removed extra arguments that were causing the error.
          padu_rtp <- calculate_padu_rtp(
            idx_serasi_map      = idx_map,
            industry_euc_dist   = rv$industry_euc_dist,
            pelayaran_euc_dist  = rv$pelayaran_euc_dist
          )
          
          incProgress(0.6, detail = "Perhitungan selesai...")
          append_log("Perhitungan indeks selesai.")
          
          idx_padu_rtp_map <- padu_rtp$idx_padu_rtp_map
          
          # Step 3: Save results (progress 90%)
          incProgress(0.1, detail = "Menyimpan hasil...")
          gpkg_path <- file.path(output_dir(), "idx_padu_rtp.gpkg")
          xlsx_path <- file.path(output_dir(), "idx_padu_rtp.xlsx")
          dir.create(output_dir(), recursive = TRUE, showWarnings = FALSE)
          
          sf::st_write(idx_padu_rtp_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          res_table <- as_tibble(sf::st_drop_geometry(idx_padu_rtp_map))
          openxlsx::write.xlsx(res_table, xlsx_path)
          
          rv$gpkg_path <- gpkg_path
          rv$xlsx_path <- xlsx_path
          rv$analysis_result <- list(map = idx_padu_rtp_map, table = res_table)
          
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
    
    # ── Status box ─────────────────────────────────────────────
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
            "Lengkapi tahap sebelumnya.")
      }
    })
    
    # ── Map output (leaflet) ──────────────────────────────────
    output$result_map <- renderLeaflet({
      req(rv$analysis_result)
      
      map_sf <- rv$analysis_result$map
      
      if (!sf::st_is_longlat(map_sf)) {
        map_sf <- sf::st_transform(map_sf, crs = 4326)
      }
      
      if (!"idx_padu_rtp" %in% names(map_sf)) {
        return(leaflet::leaflet() %>% 
                 leaflet::addControl("Kolom idx_padu_rtp tidak ditemukan.", position = "topright"))
      }
      
      pal <- leaflet::colorNumeric(
        palette = "RdYlGn",
        domain  = map_sf$idx_padu_rtp,
        na.color = "grey"
      )
      
      leaflet::leaflet(map_sf) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::addPolygons(
          fillColor   = ~pal(idx_padu_rtp),
          fillOpacity = 0.7,
          weight      = 1,
          color       = "black",
          label       = ~paste0(
            "<strong>Indeks PADU-RTp:</strong> ", round(idx_padu_rtp, 3)
          ) %>% lapply(htmltools::HTML),
          popup       = ~paste(
            "<b>ID PU:</b>", id_pu, "<br>",
            "<b>Indeks PADU-RTp:</b>", round(idx_padu_rtp, 3)
          ) %>% lapply(htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(
            weight = 3,
            color  = "red",
            fillOpacity = 0.9
          )
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          pal      = pal,
          values   = ~idx_padu_rtp,
          title    = "Indeks PADU-RTp",
          opacity  = 0.7
        )
    })
    
    # ── Table output ───────────────────────────────────────────
    output$result_table <- DT::renderDT({
      req(rv$analysis_result)
      DT::datatable(
        rv$analysis_result$table,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'Bfrtip'
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      )
    })
    
    # ── Validation log ─────────────────────────────────────────
    output$validation_log <- renderPrint({
      invalidateLater(100, session)
      cat(rv$log_messages)
    })
    
    # ── Download handlers ──────────────────────────────────────
    output$dl_gpkg <- downloadHandler(
      filename = function() "idx_padu_rtp.gpkg",
      content = function(file) {
        req(rv$gpkg_path)
        file.copy(rv$gpkg_path, file, overwrite = TRUE)
      }
    )
    
    output$dl_xlsx <- downloadHandler(
      filename = function() "idx_padu_rtp.xlsx",
      content = function(file) {
        req(rv$xlsx_path)
        file.copy(rv$xlsx_path, file, overwrite = TRUE)
      }
    )
    
  })
}