# ============================================================
#  MODULE: Reconcile (RTRW & RZWP3K) – CORRECTED VERSION
#  Fixes: Step 1 GEOMETRYCOLLECTION, Step 2 CRS NA, DT tables
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── Small UI Helpers ────────────────────────────────────────────
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

# ── Shapefile / GPKG Layer Reader Helper ────────────────────────
.read_spatial_input <- function(file_df) {
  if (is.null(file_df)) return(NULL)
  
  # Check if Geopackage
  if (nrow(file_df) == 1 && grepl("\\.gpkg$", file_df$name[1], ignore.case = TRUE)) {
    return(sf::st_read(file_df$datapath[1], quiet = TRUE))
  }
  
  # Handle Shapefile Multi-file Upload (.shp, .shx, .dbf, .prj)
  temp_dir <- tempdir()
  for (i in 1:nrow(file_df)) {
    file.copy(file_df$datapath[i], file.path(temp_dir, file_df$name[i]), overwrite = TRUE)
  }
  
  shp_file <- file_df$name[grepl("\\.shp$", file_df$name, ignore.case = TRUE)]
  if (length(shp_file) == 0) {
    stop("Komponen file .shp tidak ditemukan. Pastikan Anda memilih file .shp, .shx, .dbf, dan .prj sekaligus.")
  }
  
  sf::st_read(file.path(temp_dir, shp_file[1]), quiet = TRUE)
}

# ── Safe Display-Layer Builder (used by BOTH Step 1 and Step 2) ──
# This is the single place that prepares any sf object for Leaflet: repairs
# geometry, flattens collections, drops empties. It does NOT rbind anything -
# Step 1 shows one dataset (final_map) colored by its own category column,
# and Step 2 keeps RTRW/RZWP3K as two independent layers, so each is prepared
# on its own with a single pass through this function (no split+rbind).
#
# Use `category_col` when the sf object already carries the category you want
# to color by (e.g. Step 1's `stat_pu_final`). Use `layer_name` when you just
# want every row tagged with one fixed label (e.g. Step 2's separate layers).
.prepare_map_display <- function(sf_obj, layer_name = NULL, category_col = NULL, keep_cols = character(0)) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) return(NULL)
  
  if (!is.null(category_col) && category_col %in% names(sf_obj)) {
    sf_obj$origin_layer <- sf_obj[[category_col]]
  } else {
    sf_obj$origin_layer <- if (!is.null(layer_name)) layer_name else "Layer"
  }
  
  # Repair invalid geometries (self-intersections etc.) that can survive
  # earlier overlay operations (union/intersection/difference) upstream.
  sf_obj <- sf::st_make_valid(sf_obj)
  
  # Flatten any GEOMETRYCOLLECTION down to its POLYGON/MULTIPOLYGON parts.
  # IMPORTANT: this must run on the whole sf object at once, never looped
  # per-feature/per-geometry - looping causes R to dispatch to
  # st_collection_extract.sfg on a single sfg, which is exactly what throws:
  # "Don't know how to get polygon data from object of class
  #  XY,GEOMETRYCOLLECTION,sfg"
  sf_obj <- tryCatch(
    sf::st_collection_extract(sf_obj, "POLYGON", warn = FALSE),
    error = function(e) sf_obj
  )
  
  # Drop empty/degenerate geometries left behind by st_make_valid() /
  # st_collection_extract() or by upstream overlay ops. Empty geometries are
  # a common hidden cause of Leaflet erroring downstream with
  # "missing value where TRUE/FALSE needed".
  sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
  if (nrow(sf_obj) == 0) return(NULL)
  
  cols_keep <- intersect(c(keep_cols, "origin_layer", "geometry"), names(sf_obj))
  sf_obj[, cols_keep]
}

# ── Safe CRS-to-WGS84 Helper (used by the Leaflet renderer) ──────
# st_is_longlat() can return NA (not just TRUE/FALSE) for a malformed CRS
# (common with .shp uploads missing/broken .prj), which is what caused the
# Step 2 "missing value where TRUE/FALSE needed" crash. NA is handled
# explicitly here instead of relying on `!is_longlat`.
.to_leaflet_crs <- function(sf_obj) {
  crs <- sf::st_crs(sf_obj)
  if (is.na(crs)) {
    sf::st_crs(sf_obj) <- 4326
    return(sf_obj)
  }
  is_ll <- tryCatch(sf::st_is_longlat(sf_obj), error = function(e) NA)
  if (is.na(is_ll) || !isTRUE(is_ll)) {
    sf_obj <- tryCatch(
      sf::st_transform(sf_obj, crs = 4326),
      error = function(e) {
        # CRS too broken to reproject - fall back to treating the existing
        # coordinates as EPSG:4326 rather than crashing the map render.
        sf::st_crs(sf_obj) <- 4326
        sf_obj
      }
    )
  }
  sf_obj
}

# ── UI ──────────────────────────────────────────────────────────
reconcile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("5. Rekonsiliasi Integrasi Tata Ruang", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Sinkronisasi peta RTRW dan RZWP3K secara otomatis berdasarkan matriks keputusan prioritas wilayah darat dan laut.",
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
          accordion(
            id = ns("wizard"),
            open = "step1",
            multiple = FALSE,
            
            accordion_panel(
              title = "Langkah 1 — Menyiapkan Keputusan Rekonsiliasi",
              value = "step1",
              icon = tags$i(class = "bi bi-file-earmark-spreadsheet-fill"),
              uiOutput(ns("step1_ui"))
            ),
            
            accordion_panel(
              title = "Langkah 2 — Menentukan Keputusan Rekonsiliasi",
              value = "step2",
              icon = tags$i(class = "bi bi-check2-circle"),
              uiOutput(ns("step2_ui"))
            )
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
          
          navset_tab(
            nav_panel(
              "Peta",
              leafletOutput(ns("reconcile_map_view"), height = "500px")
            ),
            nav_panel(
              "Tabel Hasil",
              div(
                style = "height: 500px; overflow: auto;",
                uiOutput(ns("table_view_panel"))
              )
            ),
            nav_panel(
              "Log & Validasi",
              div(
                style = "max-height: 300px; overflow-y: auto; background-color: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 0.9rem; white-space: pre-wrap;",
                verbatimTextOutput(ns("validation_log"))
              )
            )
          ),
          
          div(
            style = "display: flex; gap: 8px; flex-wrap: wrap; margin-top: 12px;",
            uiOutput(ns("download_buttons_container"))
          )
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────────
reconcile_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive states
    rv <- reactiveValues(
      unlocked = 1,
      detected_step = NULL,
      
      # Objects loaded in Step 1
      recon_map = NULL,
      rtrw_vect = NULL,
      rzwp3k_vect = NULL,
      rtrw_prioritas = NULL,
      rzwp3k_prioritas = NULL,
      
      # Generated template track
      template_path = NULL,
      
      # Final resolved maps (used for tables/downloads in both steps)
      resolved_rtrw = NULL,
      resolved_rzwp3k = NULL,
      
      # Map display layers - kept separate per step on purpose:
      # Step 1 -> one dataset (final_map), colored by its own category column
      # Step 2 -> two independent layers (never rbound together)
      display_step1 = NULL,
      display_rtrw = NULL,
      display_rzwp3k = NULL,
      
      final_log = NULL
    )
    
    go_to_panel <- function(value) accordion_panel_set(id = "wizard", values = value, session = session)
    
    # Any output available yet? (used to gate status/table/download UI,
    # regardless of which step produced it or how the map layers are stored)
    have_results <- reactive({
      !is.null(rv$resolved_rtrw) || !is.null(rv$resolved_rzwp3k)
    })
    
    # ── Step 1 UI ──────────────────────────────────────────────
    output$step1_ui <- renderUI({
      tagList(
        fileInput(ns("recon_map_file"), "Pilih Peta Rekomendasi (.gpkg)", accept = ".gpkg"),
        fileInput(ns("rtrw_file"), "Pilih Peta RTRW (.shp/.gpkg)", accept = c(".gpkg", ".shp", ".shx", ".dbf", ".prj"), multiple = TRUE),
        fileInput(ns("rzwp3k_file"), "Pilih Peta RZWP3K (.shp/.gpkg)", accept = c(".gpkg", ".shp", ".shx", ".dbf", ".prj"), multiple = TRUE),
        fileInput(ns("rtrw_priority_file"), "Tabel Prioritas RTRW (.xlsx)", accept = ".xlsx"),
        fileInput(ns("rzwp3k_priority_file"), "Tabel Prioritas RZWP3K (.xlsx)", accept = ".xlsx"),
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap; margin-top: 10px;",
          actionButton(ns("btn_make_template"),
                       tagList(tags$i(class = "bi bi-file-earmark-spreadsheet me-1"), "Buat Templat"),
                       class = "btn-outline-primary btn-sm"),
          downloadButton(ns("dl_template"), "Unduh Templat", class = "btn-outline-success btn-sm")
        ),
        uiOutput(ns("template_status_ui")),
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Langkah 2")
      )
    })
    
    # Detect step level dynamically
    observeEvent(input$recon_map_file, {
      req(input$recon_map_file)
      rv$template_path <- NULL
      rv$detected_step <- NULL
      rv$unlocked <- 1
      
      tryCatch({
        map_data <- sf::st_read(input$recon_map_file$datapath, quiet = TRUE)
        cols <- names(map_data)
        
        if (any(c("stat_pu", "id_rtrw", "id_rzwp3k") %in% cols)) {
          rv$detected_step <- 1
          rv$recon_map <- map_data
          showNotification("Peta Rekomendasi terdeteksi sebagai STEP 1 (Overlaps).", type = "message")
        } else if ("length" %in% cols) {
          rv$detected_step <- 2
          rv$recon_map <- map_data
          showNotification("Peta Rekomendasi terdeteksi sebagai STEP 2 (Adjacent).", type = "message")
        } else {
          rv$recon_map <- NULL
          stop("Kolom penanda struktural (stat_pu/id_rtrw/id_rzwp3k untuk Step 1 atau length untuk Step 2) tidak ditemukan pada berkas.")
        }
      }, error = function(e) {
        rv$recon_map <- NULL
        rv$detected_step <- NULL
        showNotification(paste("Gagal Memvalidasi Berkas:", e$message), type = "error", duration = NULL)
      })
    })
    
    # Template Generation Worker
    observeEvent(input$btn_make_template, {
      
      # Check output directory 
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification(
          "Direktori output belum diatur. Harap atur direktori output terlebih dahulu.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      req(rv$recon_map, rv$detected_step, input$rtrw_priority_file, input$rzwp3k_priority_file)
      rv$template_path <- NULL
      
      tryCatch({
        rtrw_prioritas <- openxlsx::read.xlsx(input$rtrw_priority_file$datapath)
        rzwp3k_prioritas <- openxlsx::read.xlsx(input$rzwp3k_priority_file$datapath)
        
        out_dir_step <- file.path(output_dir(), paste0("step", rv$detected_step))
        dir.create(out_dir_step, recursive = TRUE, showWarnings = FALSE)
        
        file_name <- if (rv$detected_step == 1) "overlaps_reconcilliation_table.xlsx" else "adjacent_reconcilliation_table.xlsx"
        
        generate_reconciliation_excel(
          recon_map        = rv$recon_map,
          rtrw_prioritas   = rtrw_prioritas,
          rzwp3k_prioritas = rzwp3k_prioritas,
          output_dir       = out_dir_step,
          step             = rv$detected_step,
          file_name        = file_name
        )
        
        generated_path <- file.path(out_dir_step, file_name)
        if (!file.exists(generated_path)) stop("File templat gagal dibuat oleh sistem core script.")
        
        rv$template_path <- generated_path
        rv$rtrw_prioritas <- rtrw_prioritas
        rv$rzwp3k_prioritas <- rzwp3k_prioritas
        
        showNotification("Templat Rekonsiliasi Berhasil Dibuat.", type = "message")
      }, error = function(e) {
        showNotification(paste("Gagal membuat templat:", e$message), type = "error", duration = 10)
      })
    })
    
    output$template_status_ui <- renderUI({
      req(rv$template_path)
      div(class = "alert alert-success mb-0 mt-2",
          tags$i(class = "bi bi-check-circle me-2"),
          sprintf("Templat Siap (%s): %s", paste0("Step ", rv$detected_step), basename(rv$template_path)))
    })
    
    output$dl_template <- downloadHandler(
      filename = function() {
        if (!is.null(rv$template_path)) basename(rv$template_path) else "reconcilliation_table.xlsx"
      },
      content = function(file) {
        req(rv$template_path)
        file.copy(rv$template_path, file, overwrite = TRUE)
      }
    )
    
    observeEvent(input$btn_next_1, {
      if (is.null(rv$template_path)) {
        showNotification("Silakan buat dan simpan templat keputusan rekonsiliasi terlebih dahulu.", type = "warning")
        return()
      }
      rv$unlocked <- max(rv$unlocked, 2)
      go_to_panel("step2")
    })
    
    # ── Step 2 UI ──────────────────────────────────────────────
    output$step2_ui <- renderUI({
      if (rv$unlocked < 2) return(.locked_panel())
      tagList(
        fileInput(ns("recon_table_filled_file"), "Unggah Tabel Keputusan Rekonsiliasi Berisi (.xlsx)", accept = ".xlsx"),
        
        # Check output directory
        if (is.null(output_dir()) || !nzchar(output_dir())) {
          div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
              tags$i(class = "bi bi-exclamation-triangle me-1"),
              "Direktori output belum diatur. Atur terlebih dahulu di menu utama.")
        },
        
        div(
          style = "margin-top: 10px;",
          actionButton(ns("btn_run_reconcile"),
                       tagList(tags$i(class = "bi bi-lightning-charge-fill me-1"), "Lakukan Rekonsiliasi"),
                       class = "btn-success btn-sm")
        ),
        .step_nav(ns, back_id = "btn_back_2", next_id = NULL)
      )
    })
    
    observeEvent(input$btn_back_2, go_to_panel("step1"))
    
    # Core Reconciliation Execution Engine
    observeEvent(input$btn_run_reconcile, {
      
      # Check output directory 
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification(
          "Direktori output belum diatur. Harap atur direktori output terlebih dahulu.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      req(input$recon_table_filled_file, input$rtrw_file, input$rzwp3k_file)
      
      rv$resolved_rtrw <- NULL
      rv$resolved_rzwp3k <- NULL
      rv$display_step1 <- NULL
      rv$display_rtrw <- NULL
      rv$display_rzwp3k <- NULL
      log_lines <- character(0)
      
      showNotification("Menjalankan proses rekonstruksi spasial rekonsiliasi...", type = "message", id = "recon_progress", duration = NULL)
      
      tryCatch({
        # Context Parsing
        rtrw_vect <- .read_spatial_input(input$rtrw_file)
        rzwp3k_vect <- .read_spatial_input(input$rzwp3k_file)
        recon_table_path <- input$recon_table_filled_file$datapath
        
        out_base_dir <- file.path(output_dir(), paste0("step", rv$detected_step))
        dir.create(out_base_dir, recursive = TRUE, showWarnings = FALSE)
        
        if (rv$detected_step == 1) {
          # STEP 1 Execution Flow
          recon_table <- load_and_validate_table(recon_table_path)
          
          rtrw <- rtrw_vect %>% dplyr::mutate(id_rtrw = dplyr::row_number())
          rzwp3k <- rzwp3k_vect %>% dplyr::mutate(id_rzwp3k = dplyr::row_number())
          
          union_layer <- identify_overlaps_union(rtrw, rzwp3k)
          final_map <- reconcile_map(
            step             = 1,
            union            = union_layer,
            recon_table      = recon_table,
            rtrw_prioritas   = rv$rtrw_prioritas,
            rzwp3k_prioritas = rv$rzwp3k_prioritas
          )
          
          rtrw_final <- final_map[final_map$stat_pu_final == "RTRW", ]
          rzwp3k_final <- final_map[final_map$stat_pu_final == "RZWP3K", ]
          
          rv$resolved_rtrw <- rtrw_final
          rv$resolved_rzwp3k <- rzwp3k_final
          
          # Display final_map as-is: one pass through the safe-display
          # pipeline, colored by its own stat_pu_final column. No need to
          # split it into RTRW/RZWP3K and rbind them back together just to
          # render it - that was pure wasted computation on ~6k features.
          rv$display_step1 <- .prepare_map_display(final_map, category_col = "stat_pu_final")
          
          log_lines <- c(
            log_lines,
            "--- LOG REKONSILIASI KASUS STEP 1 (OVERLAPS) ---",
            sprintf("Jumlah Feature RTRW Hasil Resolusi: %d", nrow(rtrw_final)),
            sprintf("Jumlah Feature RZWP3K Hasil Resolusi: %d", nrow(rzwp3k_final))
          )
          
        } else {
          # STEP 2 Execution Flow
          rtrw <- rtrw_vect %>% dplyr::mutate(id = dplyr::row_number())
          rzwp3k <- rzwp3k_vect %>% dplyr::mutate(id = dplyr::row_number())
          
          # The function reads the Excel file internally, pass the path
          rtrw_resolved <- reconcile_map(step = 2, sf_obj = rtrw,   recon_table = recon_table_path, class_type = "RTRW")
          rzwp3k_resolved <- reconcile_map(step = 2, sf_obj = rzwp3k, recon_table = recon_table_path, class_type = "RZWP3K")
          
          rv$resolved_rtrw <- rtrw_resolved
          rv$resolved_rzwp3k <- rzwp3k_resolved
          
          # Keep RTRW and RZWP3K as two independent map layers instead of
          # rbinding them into one object. rbinding two ~6k-feature layers
          # was expensive and unnecessary - Leaflet can draw two polygon
          # layers on the same canvas (with a toggle) without merging the
          # underlying data, and each still goes through the same safe-
          # display pipeline (geometry repair/flatten/empty-drop) that fixed
          # the earlier "missing value where TRUE/FALSE needed" crash.
          rv$display_rtrw <- .prepare_map_display(rtrw_resolved, layer_name = "RTRW Adjacent Resolved")
          rv$display_rzwp3k <- .prepare_map_display(rzwp3k_resolved, layer_name = "RZWP3K Adjacent Resolved")
          
          log_lines <- c(
            log_lines,
            "--- LOG REKONSILIASI KASUS STEP 2 (ADJACENT) ---",
            sprintf("Jumlah Feature RTRW Berhasil Diselaraskan: %d", nrow(rtrw_resolved)),
            sprintf("Jumlah Feature RZWP3K Berhasil Diselaraskan: %d", nrow(rzwp3k_resolved))
          )
        }
        
        rv$final_log <- paste(log_lines, collapse = "\n")
        removeNotification("recon_progress")
        showNotification("Proses Penyelesaian Konflik Peta Selesai.", type = "message")
        
      }, error = function(e) {
        removeNotification("recon_progress")
        rv$final_log <- paste0("Error Runtime Execution:\n", e$message)
        showNotification(paste("Gagal melakukan rekonsiliasi:", e$message), type = "error", duration = NULL)
      })
    })
    
    # ── Right Panel Context Displays ───────────────────────────
    output$status_box <- renderUI({
      if (have_results()) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Peta berhasil diperbaiki! Silakan periksa hasil visual spasial dan unduh gpkg.")
      } else if (!is.null(rv$final_log) && grepl("^Error", rv$final_log)) {
        div(class = "alert alert-danger mb-0",
            tags$i(class = "bi bi-exclamation-triangle-fill me-2"),
            "Gagal mengeksekusi rekonsiliasi. Lihat rincian log kesalahan.")
      } else {
        div(class = "alert alert-secondary mb-0", "Menunggu eksekusi parameter komparasi peta...")
      }
    })
    
    output$validation_log <- renderText({
      if (!is.null(rv$final_log)) rv$final_log else "Sistem siap melakukan rekonsiliasi spasial."
    })
    
    # ── Table Results (now with DT) ───────────────────────────
    output$table_view_panel <- renderUI({
      if (!have_results()) {
        return(tags$p("Belum ada ringkasan spasial untuk dievaluasi."))
      }
      
      accordion(
        id = ns("table_summary_accordion"),
        multiple = TRUE,
        accordion_panel(
          title = "Pratinjau Data Geometri RTRW Rekonsiliasi",
          value = "rtrw_tab",
          icon = tags$i(class = "bi bi-table"),
          div(style = "overflow-x: auto;", DT::DTOutput(ns("rtrw_preview_render")))
        ),
        accordion_panel(
          title = "Pratinjau Data Geometri RZWP3K Rekonsiliasi",
          value = "rzwp3k_tab",
          icon = tags$i(class = "bi bi-table"),
          div(style = "overflow-x: auto;", DT::DTOutput(ns("rzwp3k_preview_render")))
        )
      )
    })
    
    output$rtrw_preview_render <- DT::renderDT({
      req(rv$resolved_rtrw)
      DT::datatable(
        sf::st_drop_geometry(rv$resolved_rtrw),
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    output$rzwp3k_preview_render <- DT::renderDT({
      req(rv$resolved_rzwp3k)
      DT::datatable(
        sf::st_drop_geometry(rv$resolved_rzwp3k),
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    # ── Leaflet Map ─────────────────────────────────────────────
    # Step 1: renders final_map directly, colored by stat_pu_final.
    # Step 2: renders RTRW and RZWP3K as two separate, toggleable layers on
    # the same canvas (never rbound into one object - see .prepare_map_display
    # call sites above).
    output$reconcile_map_view <- renderLeaflet({
      
      # ── STEP 1: single dataset ──
      if (identical(rv$detected_step, 1)) {
        req(rv$display_step1)
        map_sf <- rv$display_step1
        if (nrow(map_sf) == 0) {
          return(leaflet() %>% addControl("Data kosong setelah filter dilakukan.", position = "topright"))
        }
        
        map_sf <- .to_leaflet_crs(map_sf)
        
        pal <- colorFactor(
          palette = c("#2b8cbe", "#a6bddb", "#fdae61", "#66c2a5"),
          domain = unique(map_sf$origin_layer),
          na.color = "#808080"
        )
        
        return(
          leaflet(map_sf) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(
              fillColor = ~pal(origin_layer),
              fillOpacity = 0.6,
              weight = 1,
              color = "#333333",
              label = ~paste("<b>Status:</b>", origin_layer) %>% lapply(htmltools::HTML),
              highlightOptions = highlightOptions(
                weight = 3, color = "#ff0000", fillOpacity = 0.8
              )
            ) %>%
            addLegend(
              position = "bottomright",
              pal = pal,
              values = ~origin_layer,
              title = "Status Rekonsiliasi (Final Map)",
              opacity = 0.7
            )
        )
      }
      
      # ── STEP 2: two independent layers, same canvas ──
      req(!is.null(rv$display_rtrw) || !is.null(rv$display_rzwp3k))
      
      m <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
      layer_colors <- c("RTRW Adjacent Resolved" = "#2b8cbe", "RZWP3K Adjacent Resolved" = "#a6bddb")
      available_groups <- character(0)
      
      if (!is.null(rv$display_rtrw) && nrow(rv$display_rtrw) > 0) {
        rtrw_map <- .to_leaflet_crs(rv$display_rtrw)
        m <- m %>% addPolygons(
          data = rtrw_map,
          group = "RTRW Adjacent Resolved",
          fillColor = layer_colors[["RTRW Adjacent Resolved"]],
          fillOpacity = 0.6,
          weight = 1,
          color = "#333333",
          label = ~paste("<b>Layer Sumber:</b>", origin_layer) %>% lapply(htmltools::HTML),
          highlightOptions = highlightOptions(weight = 3, color = "#ff0000", fillOpacity = 0.8)
        )
        available_groups <- c(available_groups, "RTRW Adjacent Resolved")
      }
      
      if (!is.null(rv$display_rzwp3k) && nrow(rv$display_rzwp3k) > 0) {
        rzwp3k_map <- .to_leaflet_crs(rv$display_rzwp3k)
        m <- m %>% addPolygons(
          data = rzwp3k_map,
          group = "RZWP3K Adjacent Resolved",
          fillColor = layer_colors[["RZWP3K Adjacent Resolved"]],
          fillOpacity = 0.6,
          weight = 1,
          color = "#333333",
          label = ~paste("<b>Layer Sumber:</b>", origin_layer) %>% lapply(htmltools::HTML),
          highlightOptions = highlightOptions(weight = 3, color = "#ff0000", fillOpacity = 0.8)
        )
        available_groups <- c(available_groups, "RZWP3K Adjacent Resolved")
      }
      
      if (length(available_groups) == 0) {
        return(leaflet() %>% addControl("Data kosong setelah filter dilakukan.", position = "topright"))
      }
      
      m %>%
        addLayersControl(
          overlayGroups = available_groups,
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addLegend(
          position = "bottomright",
          colors = unname(layer_colors[available_groups]),
          labels = available_groups,
          title = "Layer Rekonsiliasi (Step 2)",
          opacity = 0.7
        )
    })
    
    # ── Downloader Generators ──────────────────────────────────
    output$download_buttons_container <- renderUI({
      req(have_results())
      tagList(
        downloadButton(ns("dl_rtrw_final"), "Unduh Hasil RTRW (.gpkg)", class = "btn-outline-primary btn-sm"),
        downloadButton(ns("dl_rzwp3k_final"), "Unduh Hasil RZWP3K (.gpkg)", class = "btn-outline-info btn-sm")
      )
    })
    
    output$dl_rtrw_final <- downloadHandler(
      filename = function() {
        suffix <- if (rv$detected_step == 1) "Resolved_Overlaps.gpkg" else "Resolved_Adjacent.gpkg"
        paste0("ST_RTRW_", suffix)
      },
      content = function(file) {
        req(rv$resolved_rtrw)
        sf::st_write(rv$resolved_rtrw, file, delete_dsn = TRUE, quiet = TRUE)
      }
    )
    
    output$dl_rzwp3k_final <- downloadHandler(
      filename = function() {
        suffix <- if (rv$detected_step == 1) "Resolved_Overlaps.gpkg" else "Resolved_Adjacent.gpkg"
        paste0("ST_RZWP3K_", suffix)
      },
      content = function(file) {
        req(rv$resolved_rzwp3k)
        sf::st_write(rv$resolved_rzwp3k, file, delete_dsn = TRUE, quiet = TRUE)
      }
    )
  })
}