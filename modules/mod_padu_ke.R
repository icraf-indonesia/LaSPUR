# ui/modules/mod_padu_ke.R
# ============================================================
#  MODULE: PADU-KE (2.1 PADU-KE)
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
padu_ke_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("2.1 PADU-KE (Konektivitas Ekologis)", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menilai kepaduan lingkungan berdasarkan konektivitas ekologis untuk menghasilkan nilai indeks PADU-KE.",
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
              title = "Tahap 2 — Menganalisis Konektivitas Ekologis",
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
padu_ke_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Reactive values ──────────────────────────────────────────
    rv <- reactiveValues(
      unlocked = 1,                # 1 = only step1, 2 = step2 unlocked
      
      # step1 data
      idx_serasi_map = NULL,
      lulc_vect = NULL,
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
        
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Tahap 2")
      )
    })
    
    # ── Load SERASI map and LULC ──────────────────────────────
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
    
    observeEvent(input$lulc_file, {
      req(input$lulc_file)
      tryCatch({
        rv$lulc_vect <- load_and_validate_shapefile(extract_shp_path(input$lulc_file))
        # Extract LULC reference table
        rv$lulc_ref <- rv$lulc_vect %>%
          sf::st_drop_geometry() %>%
          dplyr::distinct(ID, LC) %>%
          dplyr::arrange(ID)
        showNotification("Peta tutupan lahan berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$lulc_vect <- NULL
        rv$lulc_ref <- NULL
        showNotification(paste("Gagal memuat peta tutupan lahan:", e$message), type = "error")
      })
    })
    
    # ── Generate matrix template ───────────────────────────────
    matrix_template_path <- reactiveVal(NULL)
    
    observeEvent(input$btn_generate_matrix, {
      req(rv$lulc_ref)
      tryCatch({
        template <- generate_matrix_padu_ke(rv$lulc_ref)
        out_path <- file.path(output_dir(), "matriks_padu_ke_template.xlsx")
        dir.create(output_dir(), recursive = TRUE, showWarnings = FALSE)
        write.xlsx(template, out_path, overwrite = TRUE)
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
      if (is.null(rv$idx_serasi_map) || is.null(rv$lulc_vect)) {
        showNotification("Harap unggah peta SERASI dan peta tutupan lahan sebelum melanjutkan.",
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
            numericInput(ns("workers"), "Jumlah pekerja (cores)", value = 2, min = 1, step = 1)
          )
        ),
        
        hr(),
        
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
    
    # ── Run analysis (with progress) ──────────────────────────
    observeEvent(input$btn_run, {
      req(rv$idx_serasi_map, rv$lulc_vect, rv$matriks_padu_ke)
      
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
          
          # Step 1: Prepare data (already loaded)
          idx_map <- rv$idx_serasi_map
          lulc_vect_data <- rv$lulc_vect
          class_col <- intersect(c("ID", "Class", "class", "LULC", "Kelas"), names(lulc_vect_data))[1]
          append_log(paste0("   Kolom kelas: ", class_col))
          
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
          
          idx_padu_ke <- padu_ke$idx_padu_ke %>% dplyr::select(-idx_padu_ke_abs)
          idx_padu_ke_map <- padu_ke$idx_padu_ke_map
          
          # Step 4: Save results (progress 90%)
          incProgress(0.1, detail = "Menyimpan hasil...")
          append_log(">> Menyimpan hasil ke disk...")
          gpkg_path <- file.path(output_dir(), "idx_padu_ke.gpkg")
          xlsx_path <- file.path(output_dir(), "idx_padu_ke.xlsx")
          dir.create(output_dir(), recursive = TRUE, showWarnings = FALSE)
          
          sf::st_write(idx_padu_ke_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          result_table <- as_tibble(idx_padu_ke_map %>% sf::st_drop_geometry())
          openxlsx::write.xlsx(result_table, xlsx_path)
          
          rv$gpkg_path <- gpkg_path
          rv$xlsx_path <- xlsx_path
          rv$analysis_result <- list(map = idx_padu_ke_map, table = result_table)
          
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
        
      }) # end withProgress
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
      
      # Check if idx_padu_ke column exists
      if (!"idx_padu_ke" %in% names(map_sf)) {
        return(leaflet::leaflet() %>% 
                 leaflet::addControl("Kolom idx_padu_ke tidak ditemukan.", position = "topright"))
      }
      
      pal <- leaflet::colorNumeric(
        palette = "RdYlGn",
        domain  = map_sf$idx_padu_ke,
        na.color = "grey"
      )
      
      leaflet::leaflet(map_sf) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::addPolygons(
          fillColor   = ~pal(idx_padu_ke),
          fillOpacity = 0.7,
          weight      = 1,
          color       = "black",
          label       = ~paste0(
            "<strong>Indeks PADU-KE:</strong> ", round(idx_padu_ke, 3)
          ) %>% lapply(htmltools::HTML),
          popup       = ~paste(
            "<b>ID PU:</b>", id_pu, "<br>",
            "<b>Indeks PADU-KE:</b>", round(idx_padu_ke, 3)
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
          values   = ~idx_padu_ke,
          title    = "Indeks PADU-KE",
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
      filename = function() "idx_padu_ke.gpkg",
      content = function(file) {
        req(rv$gpkg_path)
        file.copy(rv$gpkg_path, file, overwrite = TRUE)
      }
    )
    
    output$dl_xlsx <- downloadHandler(
      filename = function() "idx_padu_ke.xlsx",
      content = function(file) {
        req(rv$xlsx_path)
        file.copy(rv$xlsx_path, file, overwrite = TRUE)
      }
    )
    
  })
}