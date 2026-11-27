# ui/modules/mod_padu_se.R
# ============================================================
#  MODULE: PADU-SE (2.6 Social Economy Analysis)
# ============================================================

source("R/functions.R")
source("R/helpers.R")
source("R/shared_inputs.R")

padu_se_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("2.6 PADU-SE (Sosial & Ekonomi)", style = "margin: 0; font-weight: 700;"),
      tags$p("Menilai kepaduan wilayah berdasarkan keterjangkauan infrastruktur pendukung kegiatan sosial dan ekonomi untuk menghasilkan nilai indeks PADU-SE.",
             style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;")
    ),
    fluidRow(
      class = "g-3",
      column(
        width = 4,
        card(
          card_header("Input & Parameter"),
          uiOutput(ns("serasi_ui_container")),
          hr(),
          div(
            id = ns("facility_section"),
            style = "margin-bottom: 12px;",
            tags$span(
              tagList(tags$i(class = "bi bi-buildings me-1"), "Peta Infrastruktur Pendukung Kegiatan Sosial & Ekonomi"),
              style = "font-weight: 600; font-size: 0.95rem; display: block; margin-bottom: 6px;"
            ),
            tags$small(
              style = "color: #6c757d; display: block; margin-bottom: 10px;",
              "Tambahkan minimal satu layer peta jarak ke infrastruktur (fasilitas umum, fasilitas sosial, jalan, dll) yang menunjang kegiatan sosial & ekonomi. Unggah peta dalam format vektor (titik/garis/poligon) atau raster (jarak)."
            ),
            div(
              class = "row align-items-center g-2 mb-2",
              div(
                class = "col-7",
                uiOutput(ns("facility_summary_ui"))
              ),
              div(
                class = "col-5 text-end",
                actionButton(
                  ns("btn_add_facility"),
                  tagList(tags$i(class = "bi bi-plus-circle me-1"), "Tambah Peta"),
                  class = "btn-outline-primary btn-sm w-100"
                )
              )
            ),
            div(id = ns("facility_container"))
          ),
          hr(),
          div(style = "display: flex; gap: 8px; flex-wrap: wrap;",
              actionButton(ns("btn_run"),
                           tagList(tags$i(class = "bi bi-play-fill me-1"),
                                   "Lakukan Analisis PADU-SE"),
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

padu_se_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    serasi_in <- serasi_input(input, output, session, output_dir)
    
    fac_ids <- reactiveVal(character(0))
    fac_counter <- reactiveVal(0)
    
    rv <- reactiveValues(
      analysis_result = NULL, gpkg_path = NULL,
      xlsx_path = NULL, log_messages = ""
    )
    
    append_log <- function(msg) {
      rv$log_messages <- paste0(rv$log_messages, format(Sys.time(), "[%H:%M:%S] "), msg, "\n")
    }
    
    output$serasi_ui_container <- renderUI({ serasi_in$ui_block() })
    
    observeEvent(input$btn_add_facility, {
      cnt <- fac_counter() + 1
      fac_counter(cnt)
      fac_id <- paste0("fac_", cnt)
      current_ids <- fac_ids()
      fac_ids(c(current_ids, fac_id))
      
      insertUI(
        selector = paste0("#", ns("facility_container")),
        where = "beforeEnd",
        ui = div(
          id = ns(paste0("box_", fac_id)),
          class = "card p-3 mb-2 bg-light border",
          style = "position: relative; border-radius: 6px;",
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
            tags$span(
              tags$strong(sprintf("Peta #%d", length(current_ids) + 1)),
              style = "font-size: 0.88rem; color: #334155;"
            ),
            actionButton(
              ns(paste0("btn_del_", fac_id)),
              tagList(tags$i(class = "bi bi-trash me-1"), "Hapus"),
              class = "btn-outline-danger btn-sm py-0 px-2",
              style = "font-size: 0.78rem;"
            )
          ),
          textInput(
            ns(paste0("name_", fac_id)),
            label = tags$span("Nama Peta", style = "font-size: 0.85rem; font-weight: 600;"),
            value = sprintf("Peta %d", length(current_ids) + 1),
            placeholder = "Contoh: Sekolah, Rumah Sakit, Pasar, Jalan"
          ),
          radioButtons(
            ns(paste0("type_", fac_id)),
            label = tags$span("Format Data", style = "font-size: 0.85rem; font-weight: 600;"),
            choices = c(
              "Vektor (Titik/Garis/Poligon) (.shp / .gpkg)" = "vector",
              "Raster Jarak (.tif)" = "raster"
            ),
            selected = "vector",
            inline = TRUE
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'vector'", ns(paste0("type_", fac_id))),
            fileInput(
              ns(paste0("file_vec_", fac_id)),
              label = tags$span("Unggah File Vektor (.shp, .gpkg)", style = "font-size: 0.85rem; font-weight: 600;"),
              accept = c(".shp", ".shx", ".dbf", ".prj", ".gpkg", ".cpg"),
              multiple = TRUE
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'raster'", ns(paste0("type_", fac_id))),
            fileInput(
              ns(paste0("file_rast_", fac_id)),
              label = tags$span("Unggah File Raster Jarak (.tif)", style = "font-size: 0.85rem; font-weight: 600;"),
              accept = c(".tif", ".tiff", ".geotiff"),
              multiple = FALSE
            )
          ),
          numericInput(
            ns(paste0("dmax_", fac_id)),
            label = tags$span("Ambang Batas Keterjangkauan (m)", style = "font-size: 0.85rem; font-weight: 600;"),
            value = 5000,
            min = 1,
            step = 100
          )
        )
      )
      
      observeEvent(input[[paste0("btn_del_", fac_id)]], {
        removeUI(selector = paste0("#", ns(paste0("box_", fac_id))))
        fac_ids(setdiff(fac_ids(), fac_id))
      }, ignoreInit = TRUE, once = TRUE)
    })
    
    output$facility_summary_ui <- renderUI({
      ids <- fac_ids()
      if (length(ids) == 0) {
        div(
          class = "alert alert-secondary py-1 px-2 mb-0",
          style = "font-size: 0.82rem;",
          tags$i(class = "bi bi-info-circle me-1"),
          "Belum ada peta ditambahkan."
        )
      } else {
        div(
          class = "alert alert-info py-1 px-2 mb-0",
          style = "font-size: 0.82rem;",
          tags$i(class = "bi bi-layers me-1"),
          sprintf("%d peta ditambahkan.", length(ids))
        )
      }
    })
    
    observeEvent(input$btn_run, {
      out_dir <- output_dir()
      if (length(out_dir) > 1) out_dir <- out_dir[1]
      if (is.null(out_dir) || !nzchar(out_dir) || !validate_output_dir(out_dir)) {
        showNotification("Direktori output belum diatur atau tidak valid.", type = "error", duration = 5)
        return()
      }
      
      pu_raw <- serasi_in$idx_serasi_map()
      if (is.null(pu_raw)) {
        showNotification("Peta unit perencanaan (SERASI) belum diatur.", type = "error", duration = 5)
        return()
      }

      ids <- fac_ids()
      if (length(ids) == 0) {
        showNotification("Harap tambahkan minimal satu peta infrastruktur pendukung terlebih dahulu.",
                         type = "error", duration = 6)
        return()
      }

      for (id_item in ids) {
        fac_type <- input[[paste0("type_", id_item)]] %||% "vector"
        fac_file <- if (fac_type == "vector") input[[paste0("file_vec_", id_item)]] else input[[paste0("file_rast_", id_item)]]
        fac_name <- trimws(input[[paste0("name_", id_item)]] %||% "")
        if (!nzchar(fac_name)) fac_name <- id_item
        if (is.null(fac_file) || nrow(fac_file) == 0) {
          showNotification(
            sprintf("File untuk peta '%s' belum diunggah. Silakan unggah file atau hapus baris peta tersebut.", fac_name),
            type = "error", duration = 8
          )
          return()
        }
        fac_dmax <- input[[paste0("dmax_", id_item)]]
        if (is.null(fac_dmax) || is.na(fac_dmax) || !is.numeric(fac_dmax) || !is.finite(fac_dmax) || fac_dmax <= 0) {
          showNotification(
            sprintf("Nilai Ambang Batas Keterjangkauan untuk peta '%s' harus berupa angka positif (meter).", fac_name),
            type = "error", duration = 8
          )
          return()
        }
      }

      rv$analysis_result <- NULL; rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL; rv$log_messages <- ""

      append_log("Memulai analisis PADU-SE...")

      withProgress(message = "Menjalankan Analisis PADU-SE", value = 0, {
        tryCatch({
          incProgress(0.1, detail = "Memuat data...")
          if ("length" %in% colnames(pu_raw) || ("id_pu" %in% colnames(pu_raw) && any(duplicated(pu_raw$id_pu)))) {
            pu <- tryCatch(dissolve_id_pu(pu_raw), error = function(e) pu_raw)
          } else {
            pu <- pu_raw
          }
          append_log("Data SERASI berhasil dimuat.")

          facility_list <- list()
          append_log(sprintf("Memproses %d layer peta pendukung...", length(ids)))
          for (j in seq_along(ids)) {
            id_item <- ids[j]
            fac_name <- trimws(input[[paste0("name_", id_item)]] %||% "")
            if (!nzchar(fac_name)) fac_name <- paste0("peta_", j)
            fac_type <- input[[paste0("type_", id_item)]] %||% "vector"
            fac_file <- if (fac_type == "vector") input[[paste0("file_vec_", id_item)]] else input[[paste0("file_rast_", id_item)]]
            fac_dmax <- as.numeric(input[[paste0("dmax_", id_item)]] %||% 5000)

            incProgress(0.3 / length(ids), detail = sprintf("Memproses peta '%s'...", fac_name))
            append_log(sprintf("Memproses peta [%d/%d]: %s (%s, ambang batas=%s m)...",
                               j, length(ids), fac_name, fac_type, fac_dmax))

            fac_layer <- build_facility_layer(
              name       = fac_name,
              file_input = fac_file,
              fac_type   = fac_type,
              pu         = pu,
              max_dist   = fac_dmax,
              resolution = 100
            )
            facility_list[[length(facility_list) + 1]] <- fac_layer
          }
          append_log("Seluruh layer peta pendukung berhasil diproses menjadi raster jarak.")

          incProgress(0.2, detail = "Menghitung indikator & indeks PADU-SE...")
          append_log("Menghitung keterjangkauan infrastruktur pendukung sosial & ekonomi...")

          calc_res <- calculate_padu_se(
            idx_serasi_map     = pu,
            facilities         = facility_list,
            id_col             = "id_pu",
            keep_intermediates = TRUE
          )

          idx_padu_se_map <- calc_res$idx_padu_se_map
          res_table       <- calc_res$idx_padu_se_table

          incProgress(0.3, detail = "Menyimpan hasil...")
          padu_se_dir <- file.path(output_dir(), "Analisis PADU-SE")
          if (!dir.exists(padu_se_dir))
            dir.create(padu_se_dir, recursive = TRUE, showWarnings = FALSE)

          gpkg_path <- file.path(padu_se_dir, "idx_padu_se.gpkg")
          xlsx_path <- file.path(padu_se_dir, "idx_padu_se.xlsx")
          sf::st_write(idx_padu_se_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          openxlsx::write.xlsx(res_table, xlsx_path)

          rv$gpkg_path <- gpkg_path; rv$xlsx_path <- xlsx_path
          rv$analysis_result <- list(map = idx_padu_se_map, table = res_table)

          out <- list(
            inputs = list(
              start_time             = Sys.time(),
              idx_serasi_path        = serasi_in$filename(),
              idx_serasi_source      = serasi_in$source(),
              serasi_source_name     = serasi_in$filename(),
              serasi_source_hash     = serasi_in$hash(),
              facilities             = lapply(facility_list, function(f) list(name = f$name, max_dist = f$max_dist)),
              output_dir             = output_dir()
            ),
            result = list(
              idx_serasi_map     = pu,
              facilities         = facility_list,
              idx_padu_se_map    = idx_padu_se_map,
              idx_padu_se_table  = res_table
            )
          )

          log_dir <- file.path(padu_se_dir, "log")
          if (!dir.exists(log_dir))
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
          tryCatch({
            inputs <- out$inputs
            save(inputs, file = file.path(log_dir, "idx_padu_se_log.rda"))
          }, error = function(e) warning("Gagal menulis log: ", e$message))

          session$userData$module_results$padu_se <- out

          tryCatch({
            plot_continuous_map(map = idx_padu_se_map, column = "idx_padu_se",
                                title = "Peta Indeks PADU-SE", legend = "Indeks PADU-SE",
                                low = "red", high = "lightgreen",
                                filepath = file.path(log_dir, "idx_padu_se.png"))
          }, error = function(e) {
            append_log(paste("Peringatan saat membuat plot peta log:", conditionMessage(e)))
          })

          append_log(paste("Peta disimpan \u2192", gpkg_path))
          append_log(paste("Tabel disimpan \u2192", xlsx_path))
          append_log("Analisis PADU-SE berhasil diselesaikan.")

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
      } else if (!is.null(serasi_in$idx_serasi_map()) && length(fac_ids()) > 0) {
        n_fac <- length(fac_ids())
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            sprintf("Siap menjalankan analisis (%d peta infrastruktur disiapkan).", n_fac))
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Siapkan peta SERASI dan tambahkan minimal 1 peta infrastruktur.")
      }
    })

    padu_se_config <- list(
      map_color_col = "idx_padu_se",
      map_title     = "Indeks PADU-SE",
      map_palette   = "RdYlGn",
      map_label_cols = c("ID PU" = "id_pu", "RTRW" = "RTRW",
                         "RZWP3K" = "RZWP3K", "Indeks PADU-SE" = "idx_padu_se"),
      table_cols = c(
        "id_pu" = "ID PU", "RTRW" = "RTRW", "RZWP3K" = "RZWP3K",
        "admin" = "Administrasi", "area_ha" = "Luas (ha)",
        "idx_padu_se" = "Indeks PADU-SE"),
      table_round_cols = c("Luas (ha)", "Indeks PADU-SE"),
      include_dynamic_cols = TRUE
    )
    
    render_result_server(input, output, session, rv, padu_se_config)
  })
}
