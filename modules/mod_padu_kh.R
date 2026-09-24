# ui/modules/mod_padu_kh.R
# ============================================================
#  MODULE: PADU-KH (2.4 PADU-KH: Habitat Presence/Quality)
# ============================================================

source("R/functions.R")
source("R/helpers.R")
source("R/shared_inputs.R")

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

padu_kh_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("2.4 PADU-KH (Komposisi Habitat)", style = "margin: 0; font-weight: 700;"),
      tags$p("Menilai kepaduan lingkungan berdasarkan komposisi habitat pada bentang lahan darat dan laut untuk menghasilkan nilai indeks PADU-KH.",
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
            accordion_panel("Langkah 2 — Menganalisis Komposisi Habitat", value = "step2",
                            icon = tags$i(class = "bi bi-pie-chart-fill"),
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

padu_kh_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    serasi_in <- serasi_input(input, output, session, output_dir)
    
    rv <- reactiveValues(
      unlocked = 1,
      habitat_source = "lulc",
      lulc_vect = NULL,
      habitat_ids = NULL,
      active_count = 0,
      max_entries = 10,
      entry_names = rep("", 10),
      entry_paths = vector("list", 10),
      entry_last_datapath = vector("list", 10),
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
    
    output$step1_ui <- renderUI({
      tagList(
        serasi_in$ui_block(),
        
        tags$p(tags$i(class = "bi bi-tree me-1"), "Sumber Peta Habitat",
               style = "font-weight: 600; margin-bottom: 4px;"),
        radioButtons(ns("habitat_source"), label = NULL,
                     choices = c("Ekstrak dari Tutupan Lahan (LULC)" = "lulc",
                                 "Unggah File Habitat Terpisah"       = "manual"),
                     inline = TRUE),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'lulc'", ns("habitat_source")),
          fileInput(ns("lulc_file"), "Peta Tutupan/Penggunaan Lahan (.shp)",
                    accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"), multiple = TRUE),
          textInput(ns("habitat_ids"), "ID Kelas yang menunjukkan habitat (pisahkan dengan koma)",
                    placeholder = "5, 6, 24, 25"),
          tags$small(class = "text-muted",
                     "Contoh: 1 (Hutan Mangrove), 14 (Terumbu Karang), dll.")
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("habitat_source")),
          div(style = "margin-bottom: 8px;",
              actionButton(ns("btn_add_habitat"),
                           tagList(tags$i(class = "bi bi-plus-circle me-1"), "Tambahkan Peta (+)"),
                           class = "btn-outline-primary btn-sm"),
              actionButton(ns("btn_remove_habitat"),
                           tagList(tags$i(class = "bi bi-dash-circle me-1"), "Hapus Peta (-)"),
                           class = "btn-outline-danger btn-sm")),
          div(style = "display: none;",
              numericInput(ns("active_count"), label = NULL, value = 0,
                           min = 0, max = 10, step = 1)),
          lapply(1:10, function(i) {
            conditionalPanel(
              condition = sprintf("input['%s'] >= %d", ns("active_count"), i),
              div(style = "border: 1px solid #dee2e6; padding: 12px; margin-bottom: 12px; border-radius: 4px;",
                  textInput(ns(paste0("habitat_name_", i)),
                            label = "Nama Habitat", value = "",
                            placeholder = "Contoh: Hutan Mangrove, Terumbu Karang, dll."),
                  fileInput(ns(paste0("habitat_file_", i)),
                            label = "Peta Habitat (.shp)",
                            accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                            multiple = TRUE))
            )
          }),
          uiOutput(ns("habitat_status"))
        ),
        
        .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Langkah 2")
      )
    })
    
    observeEvent(input$lulc_file, {
      req(input$habitat_source == "lulc", input$lulc_file)
      tryCatch({
        lulc <- sf::st_read(extract_shp_path(input$lulc_file), quiet = TRUE)
        rv$lulc_vect <- ensure_geometry_name(lulc)
        showNotification("Peta LULC berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$lulc_vect <- NULL
        showNotification(paste("Gagal memuat LULC:", e$message), type = "error")
      })
    })
    
    observeEvent(input$habitat_ids, { rv$habitat_ids <- input$habitat_ids })
    
    observeEvent(input$btn_add_habitat, {
      if (rv$active_count < rv$max_entries) {
        rv$active_count <- rv$active_count + 1
        updateNumericInput(session, "active_count", value = rv$active_count)
      } else {
        showNotification("Maksimal 10 peta habitat.", type = "warning")
      }
    })
    
    observeEvent(input$btn_remove_habitat, {
      if (rv$active_count > 0) {
        idx <- rv$active_count
        rv$entry_names[idx] <- ""
        rv$entry_paths[[idx]] <- NULL
        rv$entry_last_datapath[[idx]] <- NULL
        rv$active_count <- rv$active_count - 1
        updateNumericInput(session, "active_count", value = rv$active_count)
      }
    })
    
    observe({
      for (i in 1:rv$max_entries) {
        local({
          idx <- i
          name_input <- input[[paste0("habitat_name_", idx)]]
          if (!is.null(name_input) && idx <= rv$active_count)
            rv$entry_names[idx] <- name_input
        })
      }
    })
    
    lapply(1:10, function(i) {
      observeEvent(input[[paste0("habitat_file_", i)]], {
        file_input <- input[[paste0("habitat_file_", i)]]
        if (is.null(file_input) || nrow(file_input) == 0) return()
        if (i > rv$active_count) return()
        current_datapath <- file_input$datapath[1]
        if (!is.null(rv$entry_last_datapath[[i]]) &&
            identical(current_datapath, rv$entry_last_datapath[[i]])) return()
        tryCatch({
          path <- extract_shp_path(file_input)
          rv$entry_paths[[i]] <- path
          rv$entry_last_datapath[[i]] <- current_datapath
          showNotification(if (nchar(rv$entry_names[i]) > 0)
            paste("Peta habitat", rv$entry_names[i], "berhasil dimuat.")
            else "Peta habitat berhasil dimuat.",
            type = "message")
        }, error = function(e) {
          rv$entry_paths[[i]] <- NULL
          rv$entry_last_datapath[[i]] <- NULL
          showNotification(paste("Gagal memuat slot", i, ":", e$message), type = "error")
        })
      })
    })
    
    output$habitat_status <- renderUI({
      if (rv$active_count == 0) return(NULL)
      all_ready <- all(sapply(1:rv$active_count, function(i) {
        nchar(rv$entry_names[i]) > 0 && !is.null(rv$entry_paths[[i]])
      }))
      if (all_ready) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"), "Semua peta habitat siap.")
      } else {
        div(class = "alert alert-warning mb-0",
            tags$i(class = "bi bi-exclamation-triangle me-2"),
            "Pastikan setiap habitat memiliki nama dan file yang diunggah.")
      }
    })
    
    observeEvent(input$btn_next_1, {
      if (is.null(serasi_in$idx_serasi_map())) {
        showNotification("Harap siapkan peta SERASI.", type = "warning")
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
            showNotification(paste("Isi nama untuk habitat", i), type = "warning"); return()
          }
          if (is.null(rv$entry_paths[[i]])) {
            showNotification(paste("Unggah file untuk habitat", rv$entry_names[i]), type = "warning"); return()
          }
        }
      }
      rv$unlocked <- max(rv$unlocked, 2)
      go_to_panel("step2")
    })
    
    output$step2_ui <- renderUI({
      tagList(
        accordion(accordion_panel("Pengaturan lanjutan", icon = icon("gear"), open = FALSE,
                                  checkboxInput(ns("parallel"), "Aktifkan pemrosesan paralel", value = FALSE),
                                  numericInput(ns("workers"), "Jumlah kanal komputasi (cores)",
                                               value = 2, min = 1, step = 1))),
        hr(),
        if (is.null(output_dir()) || !nzchar(output_dir())) {
          div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
              tags$i(class = "bi bi-exclamation-triangle me-1"),
              "Direktori output belum diatur. Atur terlebih dahulu di menu utama.")
        },
        div(style = "display: flex; gap: 8px; flex-wrap: wrap;",
            actionButton(ns("btn_run"),
                         tagList(tags$i(class = "bi bi-play-fill me-1"),
                                 "Lakukan Analisis PADU-KH"),
                         class = "btn-success btn-sm")),
        .step_nav(ns, back_id = "btn_back_2", next_id = NULL)
      )
    })
    
    observeEvent(input$btn_back_2, go_to_panel("step1"))
    
    observeEvent(input$btn_run, {
      if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
        showNotification("Direktori output belum diatur. Harap atur direktori output terlebih dahulu.",
                         type = "error", duration = 5)
        return()
      }
      
      raw_serasi <- serasi_in$idx_serasi_map()
      req(raw_serasi)
      
      rv$analysis_result <- NULL
      rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL
      rv$log_messages <- ""
      
      append_log("Memulai analisis PADU-KH...")
      
      withProgress(message = "Menjalankan Analisis PADU-KH", value = 0, {
        tryCatch({
          incProgress(0.1, detail = "Memuat data...")
          if ("length" %in% colnames(raw_serasi)) {
            pu <- dissolve_id_pu(raw_serasi)
          } else {
            pu <- raw_serasi
          }
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
            if (nrow(coastal_habitat) == 0)
              stop("Tidak ada poligon yang cocok dengan ID habitat yang diberikan.")
          } else {
            append_log("Menggunakan file habitat terpisah...")
            habitat_list <- list()
            for (i in 1:rv$active_count) {
              name <- rv$entry_names[i]
              path <- rv$entry_paths[[i]]
              shp <- load_and_validate_shapefile(path)
              shp <- ensure_geometry_name(shp)
              shp <- sf::st_transform(shp, sf::st_crs(pu))
              habitat_list[[name]] <- shp
              append_log(paste("  Dimuat:", name))
            }
            combined <- dplyr::bind_rows(habitat_list)
            sf::sf_use_s2(FALSE)
            coastal_habitat <- combined %>%
              sf::st_make_valid() %>% sf::st_combine() %>%
              sf::st_make_valid() %>% sf::st_as_sf()
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
          res_map <- res_map %>% mutate(idx_padu_kh = coastal_habitat_pct / 100)
          append_log("Perhitungan indeks selesai.")
          
          incProgress(0.1, detail = "Menyimpan hasil...")
          padu_kh_dir <- file.path(output_dir(), "Analisis PADU-KH")
          if (!dir.exists(padu_kh_dir))
            dir.create(padu_kh_dir, recursive = TRUE, showWarnings = FALSE)
          
          gpkg_path <- file.path(padu_kh_dir, "idx_padu_kh.gpkg")
          xlsx_path <- file.path(padu_kh_dir, "idx_padu_kh.xlsx")
          sf::st_write(res_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          res_table <- sf::st_drop_geometry(res_map)
          openxlsx::write.xlsx(res_table, xlsx_path)
          
          rv$gpkg_path <- gpkg_path
          rv$xlsx_path <- xlsx_path
          rv$analysis_result <- list(map = res_map, table = res_table)
          
          habitat_input <- if (input$habitat_source == "lulc") {
            list(source = "lulc", lulc_file = input$lulc_file$datapath[1],
                 habitat_ids = rv$habitat_ids)
          } else {
            list(source = "manual",
                 entries = lapply(1:rv$active_count, function(i)
                   list(name = rv$entry_names[i], path = rv$entry_paths[[i]])))
          }
          
          out <- list(
            inputs = list(
              start_time             = Sys.time(),
              idx_serasi_path        = serasi_in$filename(),
              idx_serasi_source      = serasi_in$source(),
              serasi_source_name     = serasi_in$filename(),
              serasi_source_hash     = serasi_in$hash(),
              habitat_source         = input$habitat_source,
              habitat_input          = habitat_input,
              output_dir             = output_dir()
            ),
            result = list(
              idx_serasi_map     = pu,
              coastal_habitat    = coastal_habitat,
              idx_padu_kh_map    = res_map,
              idx_padu_kh_table  = res_table
            )
          )
          
          log_dir <- file.path(padu_kh_dir, "log")
          if (!dir.exists(log_dir))
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
          tryCatch({
            inputs <- out$inputs
            save(inputs, file = file.path(log_dir, "idx_padu_kh_log.rda"))
          }, error = function(e) warning("Gagal menulis log: ", e$message))
          
          session$userData$module_results$padu_kh <- out
          
          plot_continuous_map(map = res_map, column = "idx_padu_kh",
                              title = "Peta Indeks PADU-KH", legend = "Indeks PADU-KH",
                              low = "red", high = "lightgreen",
                              filepath = file.path(log_dir, "idx_padu_kh.png"))
          plot_categorical_map(map = coastal_habitat,
                               title = "Peta Habitat Pesisir", column = NA,
                               filepath = file.path(log_dir, "habitat_pesisir.png"))
          
          append_log(paste("Peta disimpan \u2192", gpkg_path))
          append_log(paste("Tabel disimpan \u2192", xlsx_path))
          append_log("Analisis PADU-KH berhasil diselesaikan.")
          
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
      } else if (rv$unlocked >= 2) {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"), "Siap menjalankan analisis.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"), "Lengkapi langkah sebelumnya.")
      }
    })
    
    padu_kh_config <- list(
      map_color_col = "idx_padu_kh",
      map_title     = "Indeks PADU-KH",
      map_palette   = "RdYlGn",
      map_label_cols = c("ID PU" = "id_pu", "RTRW" = "RTRW",
                         "RZWP3K" = "RZWP3K", "Indeks PADU-KH" = "idx_padu_kh"),
      table_cols = c(
        "id_pu" = "ID PU", "RTRW" = "RTRW", "RZWP3K" = "RZWP3K",
        "admin" = "Administrasi", "area_ha" = "Luas (ha)",
        "coastal_habitat_ha" = "Habitat Pesisir (ha)",
        "idx_padu_kh" = "Indeks PADU-KH"),
      table_round_cols = c("Luas (ha)", "Habitat Pesisir (ha)", "Indeks PADU-KH")
    )
    
    render_result_server(input, output, session, rv, padu_kh_config)
  })
}