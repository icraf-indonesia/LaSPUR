# ui/modules/mod_padu_kh.R
# ============================================================
#  MODULE: PADU-KH (2.4 PADU-KH: Habitat Presence/Quality)
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
padu_kh_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("2.4 PADU-KH (Komposisi Habitat)", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menilai kepaduan lingkungan berdasarkan komposisi habitat pada bentang lahan darat dan laut untuk menghasilkan nilai indeks PADU-KH.",
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
              title = "Tahap 2 — Menganalisis Komposisi Habitat",
              value = "step2",
              icon = tags$i(class = "bi bi-pie-chart-fill"),
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
padu_kh_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Reactive values ──────────────────────────────────────────
    rv <- reactiveValues(
      unlocked = 1,
      
      # step1 data
      idx_serasi_map = NULL,
      habitat_source = "lulc",
      
      # LULC source
      lulc_vect = NULL,
      habitat_ids = NULL,
      
      # Manual source – fixed slots (max 10)
      active_count = 0,
      max_entries = 10,
      entry_names = rep("", 10),
      entry_paths = vector("list", 10),
      entry_last_datapath = vector("list", 10),   # to avoid reprocessing
      
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
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-tree me-1"), "Sumber Peta Habitat",
               style = "font-weight: 600; margin-bottom: 4px;"),
        radioButtons(ns("habitat_source"), label = NULL,
                     choices = c("Ekstrak dari Tutupan Lahan (LULC)" = "lulc",
                                 "Unggah File Habitat Terpisah"       = "manual"),
                     inline = TRUE),
        
        # LULC source UI
        conditionalPanel(
          condition = sprintf("input['%s'] == 'lulc'", ns("habitat_source")),
          fileInput(ns("lulc_file"), "Peta Tutupan/Penggunaan Lahan (.shp)",
                    accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"), multiple = TRUE),
          textInput(ns("habitat_ids"), "ID Kelas yang menunjukkan habitat (pisahkan dengan koma)",
                    value = "5, 6, 24, 25"),
          tags$small(class = "text-muted", "Default: 5,6 (Mangrove), 24 (Terumbu Karang), 25 (Lamun)")
        ),
        
        # Manual source UI – fixed slots with conditionalPanel
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("habitat_source")),
          div(
            style = "margin-bottom: 8px;",
            actionButton(ns("btn_add_habitat"), 
                         tagList(tags$i(class = "bi bi-plus-circle me-1"), "Tambahkan Peta Habitat +"),
                         class = "btn-outline-primary btn-sm"),
            actionButton(ns("btn_remove_habitat"),
                         tagList(tags$i(class = "bi bi-dash-circle me-1"), "Hapus Terakhir"),
                         class = "btn-outline-danger btn-sm")
          ),
          # Hidden numeric input for active count
          numericInput(ns("active_count"), label = NULL, value = 0, min = 0, max = 10, step = 1),
          # Generate 10 slots, each wrapped in conditionalPanel
          lapply(1:10, function(i) {
            conditionalPanel(
              condition = sprintf("input['%s'] >= %d", ns("active_count"), i),
              div(
                style = "border: 1px solid #dee2e6; padding: 12px; margin-bottom: 12px; border-radius: 4px;",
                textInput(ns(paste0("habitat_name_", i)), 
                          label = "Nama Habitat", 
                          value = "",
                          placeholder = "Misal: Mangrove, Coral, Seagrass"),
                fileInput(ns(paste0("habitat_file_", i)), 
                          label = "Peta Habitat (.shp)",
                          accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                          multiple = TRUE)
              )
            )
          }),
          uiOutput(ns("habitat_status"))
        ),
        
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Tahap 2")
      )
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
    
    # ── LULC source ─────────────────────────────────────────────
    observeEvent(input$lulc_file, {
      req(input$habitat_source == "lulc", input$lulc_file)
      tryCatch({
        rv$lulc_vect <- sf::st_read(extract_shp_path(input$lulc_file), quiet = TRUE)
        showNotification("Peta LULC berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$lulc_vect <- NULL
        showNotification(paste("Gagal memuat LULC:", e$message), type = "error")
      })
    })
    
    observeEvent(input$habitat_ids, {
      rv$habitat_ids <- input$habitat_ids
    })
    
    # ── Manual source – slot management ────────────────────────
    # Add slot
    observeEvent(input$btn_add_habitat, {
      if (rv$active_count < rv$max_entries) {
        rv$active_count <- rv$active_count + 1
        updateNumericInput(session, "active_count", value = rv$active_count)
      } else {
        showNotification("Maksimal 10 peta habitat.", type = "warning")
      }
    })
    
    # Remove last slot
    observeEvent(input$btn_remove_habitat, {
      if (rv$active_count > 0) {
        # Clear data for the removed slot
        idx <- rv$active_count
        rv$entry_names[idx] <- ""
        rv$entry_paths[[idx]] <- NULL
        rv$entry_last_datapath[[idx]] <- NULL
        rv$active_count <- rv$active_count - 1
        updateNumericInput(session, "active_count", value = rv$active_count)
      }
    })
    
    # Observe name changes
    observe({
      for (i in 1:rv$max_entries) {
        local({
          idx <- i
          name_input <- input[[paste0("habitat_name_", idx)]]
          if (!is.null(name_input) && idx <= rv$active_count) {
            rv$entry_names[idx] <- name_input
          }
        })
      }
    })
    
    # Observe file uploads – one observer per slot
    lapply(1:10, function(i) {
      observeEvent(input[[paste0("habitat_file_", i)]], {
        file_input <- input[[paste0("habitat_file_", i)]]
        if (is.null(file_input) || nrow(file_input) == 0) return()
        if (i > rv$active_count) return()  # ignore inactive slots
        
        # Avoid reprocessing if datapath hasn't changed
        current_datapath <- file_input$datapath[1]
        if (!is.null(rv$entry_last_datapath[[i]]) &&
            identical(current_datapath, rv$entry_last_datapath[[i]])) {
          return()
        }
        
        tryCatch({
          path <- extract_shp_path(file_input)
          rv$entry_paths[[i]] <- path
          rv$entry_last_datapath[[i]] <- current_datapath
          if (nchar(rv$entry_names[i]) > 0) {
            showNotification(paste("Peta habitat", rv$entry_names[i], "berhasil dimuat."), type = "message")
          } else {
            showNotification("Peta habitat berhasil dimuat.", type = "message")
          }
        }, error = function(e) {
          rv$entry_paths[[i]] <- NULL
          rv$entry_last_datapath[[i]] <- NULL
          showNotification(paste("Gagal memuat slot", i, ":", e$message), type = "error")
        })
      })
    })
    
    # Status for manual habitat
    output$habitat_status <- renderUI({
      if (rv$active_count == 0) return(NULL)
      all_ready <- all(sapply(1:rv$active_count, function(i) {
        nchar(rv$entry_names[i]) > 0 && !is.null(rv$entry_paths[[i]])
      }))
      if (all_ready) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Semua peta habitat siap.")
      } else {
        div(class = "alert alert-warning mb-0",
            tags$i(class = "bi bi-exclamation-triangle me-2"),
            "Pastikan setiap habitat memiliki nama dan file yang diunggah.")
      }
    })
    
    # ── Step 1 -> Step 2 ──────────────────────────────────────
    observeEvent(input$btn_next_1, {
      if (is.null(rv$idx_serasi_map)) {
        showNotification("Harap unggah peta SERASI.", type = "warning")
        return()
      }
      
      if (input$habitat_source == "lulc") {
        if (is.null(rv$lulc_vect) || is.null(rv$habitat_ids) || nchar(rv$habitat_ids) == 0) {
          showNotification("Harap unggah peta LULC dan tentukan ID habitat.", type = "warning")
          return()
        }
      } else {
        if (rv$active_count == 0) {
          showNotification("Tambahkan minimal satu peta habitat.", type = "warning")
          return()
        }
        for (i in 1:rv$active_count) {
          if (nchar(rv$entry_names[i]) == 0) {
            showNotification(paste("Isi nama untuk habitat slot", i), type = "warning")
            return()
          }
          if (is.null(rv$entry_paths[[i]])) {
            showNotification(paste("Unggah file untuk habitat", rv$entry_names[i]), type = "warning")
            return()
          }
        }
      }
      
      rv$unlocked <- max(rv$unlocked, 2)
      go_to_panel("step2")
    })
    
    # ── Step 2 UI ──────────────────────────────────────────────
    output$step2_ui <- renderUI({
      if (rv$unlocked < 2) return(.locked_panel())
      
      tagList(
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
                               "Lakukan Analisis PADU-KH"),
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
      req(rv$idx_serasi_map)
      
      rv$analysis_result <- NULL
      rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL
      rv$log_messages <- ""
      
      append_log("Memulai analisis PADU-KH...")
      
      withProgress(message = "Menjalankan Analisis PADU-KH", value = 0, {
        
        tryCatch({
          incProgress(0.1, detail = "Memuat data...")
          pu <- rv$idx_serasi_map
          append_log("Peta SERASI berhasil dimuat.")
          
          incProgress(0.1, detail = "Mempersiapkan data habitat...")
          coastal_habitat <- NULL
          
          if (input$habitat_source == "lulc") {
            append_log("Menggunakan tutupan lahan sebagai sumber habitat...")
            ids <- as.numeric(unlist(strsplit(rv$habitat_ids, ",")))
            id_col <- intersect(c("ID", "id", "Id"), names(rv$lulc_vect))[1]
            if (is.na(id_col)) stop("Tidak ditemukan kolom ID pada peta LULC.")
            coastal_habitat <- rv$lulc_vect[rv$lulc_vect[[id_col]] %in% ids, ]
            append_log(paste("  Filter kelas habitat ID:", paste(ids, collapse = ", ")))
            if (nrow(coastal_habitat) == 0) {
              stop("Tidak ada poligon yang cocok dengan ID habitat yang diberikan.")
            }
          } else {
            append_log("Menggunakan file habitat terpisah...")
            habitat_list <- list()
            for (i in 1:rv$active_count) {
              name <- rv$entry_names[i]
              path <- rv$entry_paths[[i]]
              shp <- load_and_validate_shapefile(path)
              shp <- sf::st_transform(shp, sf::st_crs(pu))
              habitat_list[[name]] <- shp
              append_log(paste("  Dimuat:", name))
            }
            combined <- dplyr::bind_rows(habitat_list)
            sf::sf_use_s2(FALSE)
            coastal_habitat <- combined %>%
              sf::st_make_valid() %>%
              sf::st_combine() %>%
              sf::st_make_valid() %>%
              sf::st_as_sf()
            sf::sf_use_s2(TRUE)
            append_log("  Semua habitat digabung menjadi satu layer.")
          }
          
          incProgress(0.2, detail = "Habitat siap...")
          
          incProgress(0.1, detail = "Menghitung tumpang tindih habitat...")
          append_log("Menghitung persentase tumpang tindih habitat dalam unit perencanaan...")
          res_map <- calculate_overlay_pct(
            pu           = pu,
            overlay_area = coastal_habitat,
            title        = "coastal_habitat",
            parallel     = input$parallel,
            workers      = input$workers
          )
          incProgress(0.3, detail = "Perhitungan selesai...")
          
          incProgress(0.1, detail = "Menghitung indeks PADU-KH...")
          append_log("Menghitung indeks akhir PADU-KH...")
          res_map <- res_map %>%
            mutate(idx_padu_kh = coastal_habitat_pct / 100)
          append_log("Perhitungan indeks selesai.")
          
          incProgress(0.1, detail = "Menyimpan hasil...")
          gpkg_path <- file.path(output_dir(), "idx_padu_kh.gpkg")
          xlsx_path <- file.path(output_dir(), "idx_padu_kh.xlsx")
          dir.create(output_dir(), recursive = TRUE, showWarnings = FALSE)
          
          sf::st_write(res_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          res_table <- sf::st_drop_geometry(res_map)
          openxlsx::write.xlsx(res_table, xlsx_path)
          
          rv$gpkg_path <- gpkg_path
          rv$xlsx_path <- xlsx_path
          rv$analysis_result <- list(map = res_map, table = res_table)
          
          append_log(paste("Peta disimpan →", gpkg_path))
          append_log(paste("Tabel disimpan →", xlsx_path))
          append_log("Analisis PADU-KH berhasil diselesaikan.")
          
          incProgress(0.1, detail = "Selesai!")
          showNotification(paste("Analisis selesai. Hasil disimpan ke", gpkg_path),
                           type = "message", duration = 5)
          
        }, error = function(e) {
          msg <- conditionMessage(e)
          if (is.null(msg) || msg == "") msg <- "Error tidak diketahui (lihat konsol untuk detail)"
          append_log(paste("ERROR:", msg))
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
      
      if (!"idx_padu_kh" %in% names(map_sf)) {
        return(leaflet::leaflet() %>% 
                 leaflet::addControl("Kolom idx_padu_kh tidak ditemukan.", position = "topright"))
      }
      
      pal <- leaflet::colorNumeric(
        palette = "RdYlGn",
        domain  = map_sf$idx_padu_kh,
        na.color = "grey"
      )
      
      leaflet::leaflet(map_sf) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::addPolygons(
          fillColor   = ~pal(idx_padu_kh),
          fillOpacity = 0.7,
          weight      = 1,
          color       = "black",
          label       = ~paste0(
            "<strong>Indeks PADU-KH:</strong> ", round(idx_padu_kh, 3)
          ) %>% lapply(htmltools::HTML),
          popup       = ~paste(
            "<b>ID PU:</b>", id_pu, "<br>",
            "<b>Indeks PADU-KH:</b>", round(idx_padu_kh, 3)
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
          values   = ~idx_padu_kh,
          title    = "Indeks PADU-KH",
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
      filename = function() "idx_padu_kh.gpkg",
      content = function(file) {
        req(rv$gpkg_path)
        file.copy(rv$gpkg_path, file, overwrite = TRUE)
      }
    )
    
    output$dl_xlsx <- downloadHandler(
      filename = function() "idx_padu_kh.xlsx",
      content = function(file) {
        req(rv$xlsx_path)
        file.copy(rv$xlsx_path, file, overwrite = TRUE)
      }
    )
    
  })
}