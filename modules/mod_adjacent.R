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
.parse_id_spec <- function(spec) {
  if (is.null(spec) || !nzchar(trimws(spec))) return(numeric(0))
  spec <- gsub("\\s", "", spec)
  parts <- strsplit(spec, ",", fixed = TRUE)[[1]]
  out <- numeric(0)
  for (p in parts) {
    if (nzchar(p) && grepl("-", p, fixed = TRUE)) {
      rng <- strsplit(p, "-", fixed = TRUE)[[1]]
      if (length(rng) == 2) {
        a <- suppressWarnings(as.numeric(rng[1]))
        b <- suppressWarnings(as.numeric(rng[2]))
        if (!is.na(a) && !is.na(b) && a <= b) out <- c(out, seq(a, b))
      }
    } else {
      v <- suppressWarnings(as.numeric(p))
      if (!is.na(v)) out <- c(out, v)
    }
  }
  unique(out)
}

# Format a meter length for display
.fmt_m <- function(m) {
  if (is.na(m) || !is.finite(m)) return("—")
  if (m >= 1000) sprintf("%.2f km", m / 1000)
  else           sprintf("%.1f m", m)
}

# Format a hectare area for display
.fmt_ha <- function(x) {
  if (!is.finite(x)) return("—")
  if (x >= 1000) sprintf("%.0f ha", x)
  else           sprintf("%.2f ha", x)
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
            ),
            
            # ── Step 3 filter panel ──────────────────────────────
            accordion_panel(
              title = "Langkah 3 — Filter Hasil (Opsional)",
              value = "step3",
              icon = tags$i(class = "bi bi-funnel-fill"),
              uiOutput(ns("step3_ui"))
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
      unlocked = 1,
      
      rtrw_vect = NULL,
      rzwp3k_vect = NULL,
      rtrw_prioritas = NULL,
      rzwp3k_prioritas = NULL,
      
      admin_vect = NULL,
      admin_col = NULL,
      
      matriks_serasi = NULL,
      threshold_ha = 0,
      
      analysis_result = NULL,
      analysis_result_original = NULL,
      gpkg_path = NULL,
      xlsx_path = NULL,
      gpkg_path_original = NULL,
      xlsx_path_original = NULL,
      log_messages = "",
      
      # pre-dissolved map + per-pair attrs
      idx_serasi_map_raw = NULL,
      pair_attrs = NULL
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
        
        tags$p(tags$i(class = "bi bi-map me-1"), "Peta Administratif (.shp) (Opsional)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "Unggah shapefile batas administratif untuk menggabungkan hasil analisis per wilayah. (Kosongkan jika tidak diperlukan)"),
        fileInput(ns("admin_file"), label = NULL,
                  accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        
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
    
    # Admin shapefile and field selection
    observeEvent(input$admin_file, {
      req(input$admin_file)
      tryCatch({
        shp_path <- extract_shp_path(input$admin_file)
        admin_sf <- load_and_validate_shapefile(shp_path)
        admin_sf <- ensure_geometry_name(admin_sf)
        rv$admin_vect <- admin_sf
        
        col_names <- names(admin_sf)
        col_names <- col_names[!col_names %in% c("geometry", "geom")]
        
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
    
    observeEvent(input$admin_field, {
      rv$admin_col <- input$admin_field
    })
    
    # Load other shapefiles and tables
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
        
        accordion_panel(
          title = "Pengaturan Lanjutan",
          icon = icon("gear"),
          open = FALSE,
          
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
          kedua poligon bersentuhan.",
            style = "color: #6c757d; display: block; margin-top: -5px; margin-bottom: 0; font-size: 0.85em;"
          ),
          
          checkboxInput(ns("parallel"), "Aktifkan pemrosesan paralel", value = FALSE),
          numericInput(ns("workers"), "Jumlah kanal komputasi (cores)", value = 2, min = 1, step = 1)
        ),
        
        hr(),
        
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
    
    # ── Run analysis ───────────────────────────────────────────
    observeEvent(input$btn_run, {
      req(rv$rtrw_vect, rv$rzwp3k_vect,
          rv$rtrw_prioritas, rv$rzwp3k_prioritas,
          rv$matriks_serasi)
      
      rv$analysis_result <- NULL
      rv$analysis_result_original <- NULL
      rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL
      rv$gpkg_path_original <- NULL
      rv$xlsx_path_original <- NULL
      rv$idx_serasi_map_raw <- NULL
      rv$pair_attrs <- NULL
      rv$log_messages <- ""
      rv$unlocked <- 2
      
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
            
            matriks_xlsx <- file.path(serasi_dir, "matriks_serasi_input.xlsx")
            tryCatch({
              openxlsx::write.xlsx(rv$matriks_serasi, matriks_xlsx)
            }, error = function(e) {
              warning("Gagal menyimpan matriks SERASI: ", e$message)
            })
            
            rv$gpkg_path <- gpkg_path
            rv$xlsx_path <- xlsx_path
            rv$gpkg_path_original <- gpkg_path
            rv$xlsx_path_original <- xlsx_path
            
            rv$idx_serasi_map_raw <- idx_serasi_map
            
            raw_df <- sf::st_drop_geometry(idx_serasi_map)
            rv$pair_attrs <- raw_df %>%
              dplyr::group_by(id_pu) %>%
              dplyr::summarise(
                idx_serasi  = dplyr::first(idx_serasi),
                luas_rtrw   = dplyr::first(area_ha[!is.na(RTRW)]),
                luas_rzwp3k = dplyr::first(area_ha[!is.na(RZWP3K)]),
                length      = dplyr::first(length),
                id_group    = dplyr::first(id_group),
                .groups = "drop"
              ) %>%
              dplyr::mutate(
                rasio_segmen = length / sum(length, na.rm = TRUE)
              )
            
            idx_serasi_map_viz <- tryCatch({
              dissolve_id_pu(idx_serasi_map)
            }, error = function(e) {
              warning("dissolve_id_pu failed, using raw map: ", conditionMessage(e))
              idx_serasi_map
            })
            
            rv$analysis_result <- list(
              map = idx_serasi_map_viz,
              table = as_tibble(sf::st_drop_geometry(idx_serasi_map_viz))
            )
            rv$analysis_result_original <- rv$analysis_result
            
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
            
            log_dir <- file.path(serasi_dir, "log")
            if (!dir.exists(log_dir)) {
              dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
            }
            log_path <- file.path(log_dir, "idx_serasi_log.rda")
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
            
            session$userData$module_results$serasi <- out
            
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
            
            rv$unlocked <- max(rv$unlocked, 3)
            
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
    
    # ── Step 3 UI ──────────────────────────────────────────────
    
    output$step3_ui <- renderUI({
      if (rv$unlocked < 3 || is.null(rv$analysis_result_original)) {
        return(.locked_panel("Jalankan analisis terlebih dahulu untuk mengaktifkan filter."))
      }
      tagList(
        checkboxInput(
          ns("enable_filter"),
          tagList(tags$i(class = "bi bi-funnel me-1"), "Aktifkan Filter Hasil"),
          value = FALSE
        ),
        
        tags$div(
          style = paste(
            "margin: 8px 0 4px 0; padding: 12px 14px;",
            "background-color: #F8FAFC; border-left: 3px solid #1b75ba;",
            "border-radius: 6px; font-size: 0.82rem; color: #475569; line-height: 1.55;"
          ),
          tags$p(
            style = "margin: 0 0 8px 0;",
            tags$strong("Apa yang dilakukan filter ini: "),
            "Mempersempit hasil analisis area bertetangga berdasarkan ",
            tags$em("Indeks SERASI"), ", ",
            tags$em("luas kawasan RTRW"), ", ",
            tags$em("luas kawasan RZWP3K"), ", ",
            tags$em("rasio panjang segmen batas"), ", dan/atau ",
            tags$em("ID tertentu (id_pu / id_group)")
          ),
          tags$p(
            style = "margin: 0 0 8px 0;",
            "Hasil yang lolos filter akan ditampilkan di ",
            tags$em("Visualisasi Hasil"),
            " dan diekspor sebagai file GPKG/XLSX tambahan di folder ",
            tags$code("Analisis SERASI")
          ),
          tags$div(
            style = paste(
              "padding: 8px 10px; background-color: #FEF3C7;",
              "border-left: 3px solid #D97706; border-radius: 4px;",
              "color: #92400E; font-size: 0.8rem;"
            ),
            tags$div(
              tags$i(class = "bi bi-info-circle-fill me-1"),
              tags$strong("Pengecualian otomatis sebelum filter manual:")
            ),
            tags$div(
              style = "margin-top: 4px;",
              "Sistem sudah mengeluarkan secara otomatis dari hasil analisis: ",
              tags$strong("(1)"), " pasangan dengan ",
              tags$strong("Indeks SERASI = 1"),
              " — artinya kedua pola/zona sudah sepenuhnya sesuai (tidak ada konflik); dan ",
              tags$strong("(2)"), " pasangan dengan ",
              tags$strong("luas salah satu sisi < 1 ha"),
              ". Jika ID yang Anda masukkan tidak muncul pada pratinjau, kemungkinan pasangan ",
              "tersebut sudah tersaring otomatis."
            )
          )
        ),
        
        uiOutput(ns("filter_controls_ui"))
      )
    })
    
    output$filter_controls_ui <- renderUI({
      if (!isTRUE(input$enable_filter)) return(NULL)
      req(rv$pair_attrs)
      
      pa <- rv$pair_attrs
      
      # ── Detected ranges ──
      min_serasi <- suppressWarnings(min(pa$idx_serasi,  na.rm = TRUE))
      max_serasi <- suppressWarnings(max(pa$idx_serasi,  na.rm = TRUE))
      min_lrtrw  <- suppressWarnings(min(pa$luas_rtrw,   na.rm = TRUE))
      max_lrtrw  <- suppressWarnings(max(pa$luas_rtrw,   na.rm = TRUE))
      min_lrzwp  <- suppressWarnings(min(pa$luas_rzwp3k, na.rm = TRUE))
      max_lrzwp  <- suppressWarnings(max(pa$luas_rzwp3k, na.rm = TRUE))
      min_rasio  <- suppressWarnings(min(pa$rasio_segmen, na.rm = TRUE)) * 100
      max_rasio  <- suppressWarnings(max(pa$rasio_segmen, na.rm = TRUE)) * 100
      
      if (!is.finite(min_serasi)) min_serasi <- 0
      if (!is.finite(max_serasi)) max_serasi <- 1
      if (!is.finite(min_lrtrw))  min_lrtrw  <- 0
      if (!is.finite(max_lrtrw))  max_lrtrw  <- 1
      if (!is.finite(min_lrzwp))  min_lrzwp  <- 0
      if (!is.finite(max_lrzwp))  max_lrzwp  <- 1
      if (!is.finite(min_rasio))  min_rasio  <- 0
      if (!is.finite(max_rasio))  max_rasio  <- 100
      
      default_max_lrtrw <- ceiling(max_lrtrw)
      default_max_lrzwp <- ceiling(max_lrzwp)
      default_max_rasio <- ceiling(max_rasio * 10) / 10
      default_max_serasi <- ceiling(max_serasi * 100) / 100
      
      tagList(
        hr(),
        
        # ── 1. Indeks SERASI ─────────────────────────────────
        checkboxInput(ns("use_idx_serasi"),
                      tagList(tags$strong("1. Indeks SERASI")),
                      value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("use_idx_serasi"), "']"),
          layout_column_wrap(
            width = 1/2,
            numericInput(ns("f_serasi_min"), "Min",
                         value = 0, min = 0, max = 1, step = 0.05),
            numericInput(ns("f_serasi_max"), "Max",
                         value = default_max_serasi, min = 0, max = 1, step = 0.05)
          ),
          tags$small(
            sprintf("Rentang tersedia: %.2f – %.2f (nilai 1,00 = zona sudah sepenuhnya sesuai dan telah dikeluarkan otomatis)",
                    min_serasi, max_serasi),
            style = "color: #6c757d; display:block; margin:-6px 0 12px 0; font-size: 0.78em;"
          )
        ),
        
        # ── 2. Luas RTRW ─────────────────────────────────────
        checkboxInput(ns("use_lrtrw"),
                      tagList(tags$strong("2. Luas RTRW (ha)")),
                      value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("use_lrtrw"), "']"),
          layout_column_wrap(
            width = 1/2,
            numericInput(ns("f_lrtrw_min"), "Min",
                         value = floor(min_lrtrw), min = 0, step = 10),
            numericInput(ns("f_lrtrw_max"), "Max",
                         value = default_max_lrtrw, min = 0, step = 10)
          ),
          tags$small(
            sprintf("Rentang tersedia: %s – %s",
                    .fmt_ha(min_lrtrw), .fmt_ha(max_lrtrw)),
            style = "color: #6c757d; display:block; margin:-6px 0 12px 0; font-size: 0.78em;"
          )
        ),
        
        # ── 3. Luas RZWP3K ───────────────────────────────────
        checkboxInput(ns("use_lrzwp"),
                      tagList(tags$strong("3. Luas RZWP3K (ha)")),
                      value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("use_lrzwp"), "']"),
          layout_column_wrap(
            width = 1/2,
            numericInput(ns("f_lrzwp_min"), "Min",
                         value = floor(min_lrzwp), min = 0, step = 10),
            numericInput(ns("f_lrzwp_max"), "Max",
                         value = default_max_lrzwp, min = 0, step = 10)
          ),
          tags$small(
            sprintf("Rentang tersedia: %s – %s",
                    .fmt_ha(min_lrzwp), .fmt_ha(max_lrzwp)),
            style = "color: #6c757d; display:block; margin:-6px 0 12px 0; font-size: 0.78em;"
          )
        ),
        tags$small(
          "Jika kedua komponen luas aktif, pasangan harus lolos rentang RTRW DAN rentang RZWP3K.",
          style = "color: #6c757d; display:block; margin-bottom: 10px; font-size: 0.8em;"
        ),
        
        # ── 4. Rasio segmen (persen) ─────────────────────────
        checkboxInput(ns("use_rasio"),
                      tagList(tags$strong("4. Rasio Panjang Segmen (%)")),
                      value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("use_rasio"), "']"),
          layout_column_wrap(
            width = 1/2,
            tags$div(
              numericInput(ns("f_rasio_min"), "Min (%)",
                           value = floor(min_rasio),
                           min = 0, max = 100, step = 1),
              uiOutput(ns("rasio_min_hint"))
            ),
            tags$div(
              numericInput(ns("f_rasio_max"), "Max (%)",
                           value = default_max_rasio,
                           min = 0, max = 100, step = 1),
              uiOutput(ns("rasio_max_hint"))
            )
          ),
          tags$small(
            sprintf("Rentang tersedia: %.2f%% – %.2f%%", min_rasio, max_rasio),
            style = "color: #6c757d; display:block; margin:4px 0 4px 0; font-size: 0.78em;"
          ),
          tags$small(
            "Rasio = panjang segmen / total panjang seluruh segmen (0–100%).",
            style = "color: #6c757d; display:block; margin-bottom: 10px; font-size: 0.8em;"
          )
        ),
        
        # ── 5. ID filter ─────────────────────────────────────
        checkboxInput(ns("use_id"),
                      tagList(tags$strong("5. Filter ID")),
                      value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("use_id"), "']"),
          selectInput(ns("f_id_type"), "Tipe ID",
                      choices = c("id_pu", "id_group"), selected = "id_pu"),
          textInput(ns("f_id_spec"), "Daftar ID (contoh: 1-10, 15, 20-25)",
                    placeholder = "1-10, 15, 20-25"),
          uiOutput(ns("id_hint_ui"))
        ),
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap; margin-top: 8px;",
          actionButton(ns("btn_apply_filter"),
                       tagList(tags$i(class = "bi bi-funnel-fill me-1"), "Lakukan Filter"),
                       class = "btn-primary btn-sm"),
          actionButton(ns("btn_reset_filter"),
                       tagList(tags$i(class = "bi bi-arrow-counterclockwise me-1"), "Reset"),
                       class = "btn-outline-secondary btn-sm")
        ),
        uiOutput(ns("filter_preview_ui"))
      )
    })
    
    # ── Live meter-length hint under rasio Min (%) ──
    output$rasio_min_hint <- renderUI({
      req(rv$pair_attrs)
      total_len <- sum(rv$pair_attrs$length, na.rm = TRUE)
      if (!is.finite(total_len) || total_len <= 0) return(NULL)
      mn <- input$f_rasio_min
      if (is.null(mn) || is.na(mn)) return(NULL)
      tags$small(
        sprintf("≈ %s", .fmt_m(total_len * mn / 100)),
        style = "color: #1b75ba; font-weight: 600; display:block; margin-top:-6px; margin-bottom:6px; font-size:0.78em;"
      )
    })
    
    # ── Live meter-length hint under rasio Max (%) ──
    output$rasio_max_hint <- renderUI({
      req(rv$pair_attrs)
      total_len <- sum(rv$pair_attrs$length, na.rm = TRUE)
      if (!is.finite(total_len) || total_len <= 0) return(NULL)
      mx <- input$f_rasio_max
      if (is.null(mx) || is.na(mx)) return(NULL)
      tags$small(
        sprintf("≈ %s", .fmt_m(total_len * mx / 100)),
        style = "color: #1b75ba; font-weight: 600; display:block; margin-top:-6px; margin-bottom:6px; font-size:0.78em;"
      )
    })
    
    # ── Status box ─────────────────────────────────────────────
    output$id_hint_ui <- renderUI({
      req(rv$pair_attrs)
      pa <- rv$pair_attrs
      id_type <- input$f_id_type
      if (is.null(id_type) || !nzchar(id_type)) id_type <- "id_pu"
      
      available_all <- unique(as.numeric(pa[[id_type]]))
      n_avail   <- length(available_all)
      max_avail <- suppressWarnings(max(available_all, na.rm = TRUE))
      if (!is.finite(max_avail)) max_avail <- NA_real_
      max_txt <- if (is.na(max_avail)) "—" else format(max_avail, scientific = FALSE, trim = TRUE)
      
      spec <- input$f_id_spec
      
      if (is.null(spec) || !nzchar(trimws(spec))) {
        return(tags$small(
          sprintf(
            "Tersedia: %d %s, dengan nilai maksimum %s. Pasangan dengan Indeks SERASI = 1 atau luas < 1 ha tidak disertakan.",
            n_avail, id_type, max_txt
          ),
          style = "color: #6c757d; display:block; margin-top:-6px; font-size: 0.78em;"
        ))
      }
      
      entered <- .parse_id_spec(spec)
      
      if (length(entered) == 0) {
        return(tags$small(
          sprintf("Tersedia: %d %s, dengan nilai maksimum %s.",
                  n_avail, id_type, max_txt),
          style = "color: #6c757d; display:block; margin-top:-6px; font-size: 0.78em;"
        ))
      }
      
      matched <- intersect(entered, available_all)
      missing <- setdiff(entered, available_all)
      
      if (length(missing) == 0) {
        return(tags$div(
          class = "alert alert-success",
          style = "margin-top:6px; font-size:0.78rem; padding:6px 10px;",
          tags$i(class = "bi bi-check-circle me-1"),
          sprintf("%d dari %d ID dikenali (nilai maksimum tersedia: %s).",
                  length(matched), length(entered), max_txt)
        ))
      }
      
      missing_str <- paste(head(missing, 15), collapse = ", ")
      if (length(missing) > 15) {
        missing_str <- paste0(
          missing_str,
          sprintf(" … (+%d lainnya)", length(missing) - 15)
        )
      }
      
      if (length(matched) == 0) {
        return(tags$div(
          class = "alert alert-warning",
          style = "margin-top:6px; font-size:0.8rem; padding:8px 10px;",
          tags$div(
            tags$i(class = "bi bi-exclamation-triangle-fill me-1"),
            tags$strong("Tidak ada ID yang cocok.")
          ),
          tags$div(
            style = "margin-top:4px;",
            sprintf("ID yang tidak ditemukan: %s", missing_str)
          ),
          tags$div(
            style = "margin-top:4px;",
            sprintf("Nilai maksimum %s yang tersedia: %s.", id_type, max_txt)
          ),
          tags$div(
            style = "margin-top:4px; color:#92400E; font-size:0.75rem;",
            "Kemungkinan tersaring otomatis (Indeks SERASI = 1 atau luas salah satu sisi < 1 ha)."
          )
        ))
      }
      
      tags$div(
        class = "alert alert-secondary",
        style = "margin-top:6px; font-size:0.78rem; padding:8px 10px;",
        tags$div(
          tags$i(class = "bi bi-info-circle me-1"),
          sprintf("%d dari %d ID dikenali.", length(matched), length(entered))
        ),
        tags$div(
          style = "margin-top:4px;",
          sprintf("ID tidak ditemukan: %s", missing_str)
        ),
        tags$div(
          style = "margin-top:4px;",
          sprintf("Nilai maksimum %s yang tersedia: %s.", id_type, max_txt)
        ),
        tags$div(
          style = "margin-top:4px; color:#64748B; font-size:0.75rem;",
          "Kemungkinan tersaring otomatis (Indeks SERASI = 1 atau luas salah satu sisi < 1 ha)."
        )
      )
    })
    
    filtered_ids <- reactive({
      req(rv$pair_attrs)
      pa <- rv$pair_attrs
      
      keep <- rep(TRUE, nrow(pa))
      
      # ── 1. Indeks SERASI ──
      if (isTRUE(input$use_idx_serasi)) {
        kis <- rep(TRUE, nrow(pa))
        mn <- input$f_serasi_min
        mx <- input$f_serasi_max
        if (!is.null(mn) && !is.na(mn)) kis <- kis & (pa$idx_serasi >= mn)
        if (!is.null(mx) && !is.na(mx)) kis <- kis & (pa$idx_serasi <= mx)
        kis[is.na(kis)] <- FALSE
        keep <- keep & kis
      }
      
      # ── 2. Luas RTRW ──
      if (isTRUE(input$use_lrtrw)) {
        kr <- rep(TRUE, nrow(pa))
        mn <- input$f_lrtrw_min
        mx <- input$f_lrtrw_max
        if (!is.null(mn) && !is.na(mn)) kr <- kr & (pa$luas_rtrw >= mn)
        if (!is.null(mx) && !is.na(mx)) kr <- kr & (pa$luas_rtrw <= mx)
        kr[is.na(kr)] <- FALSE
        keep <- keep & kr
      }
      
      # ── 3. Luas RZWP3K ──
      if (isTRUE(input$use_lrzwp)) {
        kz <- rep(TRUE, nrow(pa))
        mn <- input$f_lrzwp_min
        mx <- input$f_lrzwp_max
        if (!is.null(mn) && !is.na(mn)) kz <- kz & (pa$luas_rzwp3k >= mn)
        if (!is.null(mx) && !is.na(mx)) kz <- kz & (pa$luas_rzwp3k <= mx)
        kz[is.na(kz)] <- FALSE
        keep <- keep & kz
      }
      
      # ── 4. Rasio segmen (input %, internal 0–1) ──
      if (isTRUE(input$use_rasio)) {
        krs <- rep(TRUE, nrow(pa))
        mn <- input$f_rasio_min
        mx <- input$f_rasio_max
        if (!is.null(mn) && !is.na(mn)) krs <- krs & (pa$rasio_segmen >= mn / 100)
        if (!is.null(mx) && !is.na(mx)) krs <- krs & (pa$rasio_segmen <= mx / 100)
        krs[is.na(krs)] <- FALSE
        keep <- keep & krs
      }
      
      # ── 5. ID filter ──
      if (isTRUE(input$use_id)) {
        id_type <- input$f_id_type
        if (is.null(id_type) || !nzchar(id_type)) id_type <- "id_pu"
        ids_parsed <- .parse_id_spec(input$f_id_spec)
        if (length(ids_parsed) > 0) {
          keep <- keep & (as.numeric(pa[[id_type]]) %in% ids_parsed)
        }
      }
      
      pa$id_pu[keep]
    })
    
    # ── Realtime preview ──
    output$filter_preview_ui <- renderUI({
      if (!isTRUE(input$enable_filter)) return(NULL)
      req(rv$pair_attrs)
      
      any_enabled <- isTRUE(input$use_idx_serasi) ||
        isTRUE(input$use_lrtrw)      ||
        isTRUE(input$use_lrzwp)      ||
        isTRUE(input$use_rasio)      ||
        isTRUE(input$use_id)
      
      if (!any_enabled) {
        return(div(
          class = "alert alert-warning",
          style = "margin-top: 12px; font-size: 0.9rem;",
          tags$i(class = "bi bi-exclamation-triangle me-1"),
          "Aktifkan minimal satu komponen filter untuk melihat pratinjau."
        ))
      }
      
      ids <- tryCatch(filtered_ids(), error = function(e) NULL)
      if (is.null(ids)) return(NULL)
      
      n_before <- nrow(rv$pair_attrs)
      n_after  <- length(ids)
      removed  <- n_before - n_after
      pct      <- if (n_before > 0) (removed / n_before) * 100 else 0
      
      div(
        class = "alert alert-info", style = "margin-top: 12px; font-size: 0.9rem;",
        tags$div(sprintf("Sebelum filter: %d pasangan", n_before)),
        tags$div(sprintf("Setelah filter: %d pasangan", n_after)),
        tags$div(sprintf("Terhapus: %d pasangan (%.1f%%)", removed, pct))
      )
    })
    
    # ── Apply filter ──
    observeEvent(input$btn_apply_filter, {
      req(rv$idx_serasi_map_raw, rv$pair_attrs, output_dir())
      
      any_enabled <- isTRUE(input$use_idx_serasi) ||
        isTRUE(input$use_lrtrw)      ||
        isTRUE(input$use_lrzwp)      ||
        isTRUE(input$use_rasio)      ||
        isTRUE(input$use_id)
      
      if (!any_enabled) {
        showNotification("Aktifkan minimal satu komponen filter sebelum menjalankan filter.",
                         type = "warning", duration = 5)
        return()
      }
      
      ids_to_keep <- filtered_ids()
      if (length(ids_to_keep) == 0) {
        showNotification("Filter menghasilkan 0 pasangan. Tidak ada yang diekspor.",
                         type = "warning", duration = 6)
        return()
      }
      
      filtered_raw <- rv$idx_serasi_map_raw %>%
        dplyr::filter(id_pu %in% ids_to_keep)
      
      if (nrow(filtered_raw) == 0) {
        showNotification("Tidak ada data setelah filter.", type = "warning")
        return()
      }
      
      # ── Build suffix — only include enabled components ───
      parts <- character(0)
      build_range <- function(mn, mx, nm, digits = 0) {
        has_mn <- !is.null(mn) && !is.na(mn)
        has_mx <- !is.null(mx) && !is.na(mx)
        if (!has_mn && !has_mx) return(NULL)
        mn_s <- if (has_mn) format(mn, nsmall = digits, trim = TRUE) else "0"
        mx_s <- if (has_mx) format(mx, nsmall = digits, trim = TRUE) else "inf"
        sprintf("%s%s-%s", nm, mn_s, mx_s)
      }
      
      if (isTRUE(input$use_idx_serasi)) {
        r <- build_range(input$f_serasi_min, input$f_serasi_max, "idx_serasi", 2)
        if (!is.null(r)) parts <- c(parts, r)
      }
      if (isTRUE(input$use_lrtrw)) {
        r <- build_range(input$f_lrtrw_min, input$f_lrtrw_max, "luas_rtrw", 0)
        if (!is.null(r)) parts <- c(parts, r)
      }
      if (isTRUE(input$use_lrzwp)) {
        r <- build_range(input$f_lrzwp_min, input$f_lrzwp_max, "luas_rzwp3k", 0)
        if (!is.null(r)) parts <- c(parts, r)
      }
      if (isTRUE(input$use_rasio)) {
        r <- build_range(input$f_rasio_min, input$f_rasio_max, "rasio_segmen_pct", 0)
        if (!is.null(r)) parts <- c(parts, r)
      }
      if (isTRUE(input$use_id) &&
          !is.null(input$f_id_spec) && nzchar(trimws(input$f_id_spec))) {
        spec_clean <- gsub("\\s", "", input$f_id_spec)
        id_type <- input$f_id_type
        if (is.null(id_type) || !nzchar(id_type)) id_type <- "id_pu"
        parts <- c(parts, sprintf("%s%s", id_type, spec_clean))
      }
      
      suffix <- if (length(parts) > 0)
        paste0("_filtered_", paste(parts, collapse = "_"))
      else
        "_filtered"
      
      serasi_dir <- file.path(output_dir(), "Analisis SERASI")
      if (!dir.exists(serasi_dir)) {
        dir.create(serasi_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      gpkg_path <- file.path(serasi_dir,
                             sprintf("idx_serasi_adjacent%s_%s.gpkg", suffix, ts))
      xlsx_path <- file.path(serasi_dir,
                             sprintf("idx_serasi_adjacent%s_%s.xlsx", suffix, ts))
      
      tryCatch({
        sf::st_write(filtered_raw, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
        openxlsx::write.xlsx(sf::st_drop_geometry(filtered_raw), xlsx_path)
      }, error = function(e) {
        showNotification(paste("Gagal mengekspor hasil filter:", e$message),
                         type = "error", duration = 8)
        return()
      })
      
      rv$gpkg_path <- gpkg_path
      rv$xlsx_path <- xlsx_path
      
      filtered_viz <- tryCatch({
        dissolve_id_pu(filtered_raw)
      }, error = function(e) {
        warning("dissolve_id_pu failed for filtered data: ", conditionMessage(e))
        filtered_raw
      })
      
      rv$analysis_result <- list(
        map   = filtered_viz,
        table = as_tibble(sf::st_drop_geometry(filtered_viz))
      )
      
      rv$log_messages <- paste0(
        rv$log_messages,
        sprintf(">> Filter diterapkan: %d pasangan tersisa (dari %d).\n",
                length(ids_to_keep), nrow(rv$pair_attrs)),
        sprintf("   Ekspor: %s\n", basename(gpkg_path))
      )
      
      showNotification(
        sprintf("Filter diterapkan. %d pasangan tersisa. File: %s",
                length(ids_to_keep), basename(gpkg_path)),
        type = "message", duration = 6
      )
    })
    
    # ── Reset to original ──
    observeEvent(input$btn_reset_filter, {
      req(rv$analysis_result_original)
      rv$analysis_result <- rv$analysis_result_original
      rv$gpkg_path <- rv$gpkg_path_original
      rv$xlsx_path <- rv$xlsx_path_original
      showNotification("Hasil dikembalikan ke versi awal (tanpa filter).",
                       type = "message", duration = 4)
    })
    
    observeEvent(input$enable_filter, {
      if (!isTRUE(input$enable_filter)) {
        if (!is.null(rv$analysis_result_original)) {
          rv$analysis_result <- rv$analysis_result_original
          rv$gpkg_path <- rv$gpkg_path_original
          rv$xlsx_path <- rv$xlsx_path_original
        }
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    output$status_box <- renderUI({
      if (!is.null(rv$analysis_result) && !is.null(rv$analysis_result_original) &&
          !identical(rv$analysis_result, rv$analysis_result_original)) {
        div(class = "alert alert-info mb-0",
            tags$i(class = "bi bi-funnel-fill me-2"),
            "Analisis selesai. Filter sedang aktif.")
      } else if (!is.null(rv$analysis_result)) {
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