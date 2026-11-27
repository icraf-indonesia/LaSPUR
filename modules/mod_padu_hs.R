# ui/modules/mod_padu_hs.R
# ============================================================
#  MODULE: PADU-HS (2.2)
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

.plot_river_buffer_overlay <- function(idx_padu_hs_map, river_vect, buffer_radius, filepath,
                                       width = 7, height = 5, dpi = 300) {
  status_map <- idx_padu_hs_map[, "river_index", drop = FALSE]
  status_map <- sf::st_zm(status_map, drop = TRUE, what = "ZM")
  status_map <- sf::st_make_valid(status_map)
  status_map <- status_map[!sf::st_is_empty(status_map), , drop = FALSE]
  status_map$river_status <- factor(
    ifelse(!is.na(status_map$river_index) & status_map$river_index == 1,
           "Terkualifikasi (Buffer)", "Di Luar Buffer"),
    levels = c("Terkualifikasi (Buffer)", "Di Luar Buffer")
  )

  river_clean <- sf::st_zm(river_vect, drop = TRUE, what = "ZM")
  river_clean <- sf::st_make_valid(river_clean)
  river_clean <- river_clean[!sf::st_is_empty(river_clean), , drop = FALSE]

  status_3857 <- .reproject_to_3857(status_map)
  river_3857  <- .reproject_to_3857(river_clean)
  bm <- .get_esri_gray_basemap(status_3857)
  bb <- sf::st_bbox(status_3857)

  p <- ggplot2::ggplot()
  if (!is.null(bm)) p <- p + bm
  p <- p +
    ggplot2::geom_sf(
      data = status_3857,
      ggplot2::aes(fill = .data$river_status),
      color = "#bdbdbd",
      linewidth = 0.15,
      alpha = 0.75
    ) +
    ggplot2::scale_fill_manual(
      values = c("Terkualifikasi (Buffer)" = "#2b8cbe", "Di Luar Buffer" = "#f0f0f0"),
      drop   = FALSE,
      name   = "Status Kualifikasi"
    ) +
    ggplot2::geom_sf(
      data = river_3857,
      color = "#08306b",
      fill = "#08306b",
      linewidth = 0.5,
      alpha = 0.85
    ) +
    ggplot2::coord_sf(
      xlim = c(bb["xmin"], bb["xmax"]),
      ylim = c(bb["ymin"], bb["ymax"]),
      expand = FALSE
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = sprintf("Peta Kualifikasi Buffer Sungai (%s m)", format(buffer_radius, big.mark = ","))
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 8),
      axis.text.y = ggplot2::element_text(size = 8),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0),
      legend.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 10),
      legend.position = "right",
      legend.justification = c(0, 0.5),
      legend.box.spacing = grid::unit(0.5, "cm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 2, l = 2)
    )

  ggplot2::ggsave(filename = filepath, plot = p, width = width, height = height, dpi = dpi)
  invisible(p)
}

padu_hs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("2.2 PADU-HS (Hidrologi dan Sedimentasi)", style = "margin: 0; font-weight: 700;"),
      tags$p("Menilai kepaduan lingkungan berdasarkan kualifikasi buffer sungai dan konsentrasi Total Suspended Solids (TSS) untuk menghasilkan nilai indeks PADU-HS.",
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
            accordion_panel("Langkah 2 — Menganalisis Hidrologi dan Sedimentasi", value = "step2",
                            icon = tags$i(class = "bi bi-droplet-fill"),
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

padu_hs_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    serasi_in <- serasi_input(input, output, session, output_dir)

    rv <- reactiveValues(
      unlocked = 1,
      tss_rast = NULL,
      river_vect = NULL,
      analysis_result = NULL,
      gpkg_path = NULL,
      xlsx_path = NULL,
      log_messages = ""
    )

    go_to_panel <- function(value) {
      accordion_panel_set(id = "wizard", values = value, session = session)
    }

    load_river_vector <- function(file_input) {
      gpkg_row <- file_input[grepl("\\.gpkg$", file_input$name, ignore.case = TRUE), ]
      shp_row <- file_input[grepl("\\.shp$", file_input$name, ignore.case = TRUE), ]

      if (nrow(gpkg_row) > 0 && nrow(shp_row) > 0) {
        stop("Harap unggah file GeoPackage (.gpkg) ATAU Shapefile (.shp), bukan keduanya sekaligus.")
      }

      if (nrow(gpkg_row) > 0) {
        if (nrow(gpkg_row) != 1 || nrow(file_input) != 1) {
          stop("Harap unggah tepat satu file GeoPackage (.gpkg).")
        }
        temp_dir <- file.path(tempdir(), paste0("river_gpkg_", sample(1e9, 1)))
        dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
        dest_gpkg <- file.path(temp_dir, basename(gpkg_row$name[1]))
        file.copy(gpkg_row$datapath[1], dest_gpkg, overwrite = TRUE)
        return(load_and_validate_shapefile(dest_gpkg))
      }

      if (nrow(shp_row) != 1) {
        stop("Harap unggah file GeoPackage (.gpkg) atau seluruh komponen Shapefile (.shp, .dbf, .prj, .shx)")
      }
      base_name <- tools::file_path_sans_ext(basename(shp_row$name[1]))
      temp_dir <- file.path(tempdir(), paste0("river_shp_", sample(1e9, 1)))
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
      for (i in seq_len(nrow(file_input))) {
        ext <- tolower(tools::file_ext(file_input$name[i]))
        file.copy(file_input$datapath[i],
                  file.path(temp_dir, paste0(base_name, ".", ext)),
                  overwrite = TRUE)
      }
      load_and_validate_shapefile(file.path(temp_dir, paste0(base_name, ".shp")))
    }

    append_log <- function(msg) {
      rv$log_messages <- paste0(rv$log_messages, format(Sys.time(), "[%H:%M:%S] "), msg, "\n")
    }

    output$step1_ui <- renderUI({
      tagList(
        serasi_in$ui_block(),

        tags$p(tags$i(class = "bi bi-layers me-1"), "Peta Total Suspended Solid (TSS) (.tif)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "File raster konsentrasi TSS (mg/L)."),
        fileInput(ns("tss_file"), label = NULL, accept = c(".tif", ".tiff", ".geotiff")),
        uiOutput(ns("tss_status")),

        hr(),

        tags$p(tags$i(class = "bi bi-water me-1"), "Data Vektor Sungai (.shp / .gpkg)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(style = "color: #6c757d; display: block; margin-bottom: 8px;",
                   "Unggah seluruh komponen Shapefile (.shp, .dbf, .prj, .shx) atau satu file GeoPackage (.gpkg) jaringan/badan sungai."),
        fileInput(ns("river_file"), label = NULL,
                  accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg", ".gpkg"),
                  multiple = TRUE),
        uiOutput(ns("river_status")),

        .step_nav(ns, back_id = NULL, next_id = "btn_next_1", next_label = "Lanjut ke Langkah 2")
      )
    })

    observeEvent(input$tss_file, {
      req(input$tss_file)
      tryCatch({
        r <- load_and_validate_raster(input$tss_file$datapath)
        r_crs <- terra::crs(r)
        if (is.na(r_crs) || !nzchar(trimws(r_crs))) {
          stop("Raster TSS tidak memiliki sistem koordinat (CRS) terdefinisi.")
        }
        rv$tss_rast <- r
        showNotification("Peta TSS berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$tss_rast <- NULL
        showNotification(paste("Gagal memuat TSS:", e$message), type = "error")
      })
    })

    output$tss_status <- renderUI({
      if (is.null(rv$tss_rast)) return(NULL)
      div(class = "alert alert-success mt-2 mb-0 py-1 px-2", style = "font-size: 0.85rem;",
          tags$i(class = "bi bi-check-circle me-1"), "Peta TSS siap digunakan.")
    })

    observeEvent(input$river_file, {
      req(input$river_file)
      tryCatch({
        river <- load_river_vector(input$river_file)
        if (is.na(sf::st_crs(river))) {
          stop("Data vektor sungai harus memiliki sistem koordinat (CRS) terdefinisi.")
        }
        river <- sf::st_zm(river, drop = TRUE, what = "ZM")
        river <- river[!sf::st_is_empty(river), , drop = FALSE]
        if (nrow(river) == 0) {
          stop("Data vektor sungai tidak memiliki geometri yang valid.")
        }
        geom_types <- unique(as.character(sf::st_geometry_type(river, by_geometry = TRUE)))
        allowed_types <- c("LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")
        if (!all(geom_types %in% allowed_types)) {
          stop("Data vektor sungai harus bertipe garis (LINESTRING/MULTILINESTRING) atau poligon (POLYGON/MULTIPOLYGON).")
        }
        rv$river_vect <- ensure_geometry_name(river)
        showNotification("Data vektor sungai berhasil dimuat.", type = "message")
      }, error = function(e) {
        rv$river_vect <- NULL
        showNotification(paste("Gagal memuat vektor sungai:", e$message), type = "error")
      })
    })

    output$river_status <- renderUI({
      if (is.null(rv$river_vect)) return(NULL)
      div(class = "alert alert-success mt-2 mb-0 py-1 px-2", style = "font-size: 0.85rem;",
          tags$i(class = "bi bi-check-circle me-1"), "Data vektor sungai siap digunakan.")
    })

    observeEvent(input$btn_next_1, {
      if (is.null(serasi_in$idx_serasi_map())) {
        showNotification("Harap siapkan Peta Indeks SERASI terlebih dahulu.", type = "warning")
        return()
      }
      if (is.null(rv$tss_rast)) {
        showNotification("Harap unggah Peta TSS (.tif) terlebih dahulu.", type = "warning")
        return()
      }
      if (is.null(rv$river_vect)) {
        showNotification("Harap unggah Data Vektor Sungai (.shp / .gpkg) terlebih dahulu.", type = "warning")
        return()
      }
      rv$unlocked <- max(rv$unlocked, 2)
      go_to_panel("step2")
    })

    output$step2_ui <- renderUI({
      cur_radius <- isolate(input$buffer_radius)
      if (is.null(cur_radius) || !is.numeric(cur_radius) || length(cur_radius) != 1 || is.na(cur_radius)) {
        cur_radius <- 5000
      }
      tagList(
        tags$p(tags$i(class = "bi bi-sliders me-1"), "Parameter",
               style = "font-weight: 600; margin-bottom: 4px;"),
        numericInput(ns("buffer_radius"), "Radius Buffer Sungai (meter)", value = cur_radius, min = 1),
        hr(),
        if (is.null(output_dir()) || !nzchar(output_dir())) {
          div(class = "alert alert-warning py-2 px-3 mb-2", style = "font-size: 0.85rem;",
              tags$i(class = "bi bi-exclamation-triangle me-1"),
              "Direktori output belum diatur. Atur terlebih dahulu di menu utama.")
        },
        div(style = "display: flex; gap: 8px; flex-wrap: wrap;",
            actionButton(ns("btn_run"),
                         tagList(tags$i(class = "bi bi-play-fill me-1"),
                                 "Lakukan Analisis PADU-HS"),
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

      idx_serasi_map_raw <- serasi_in$idx_serasi_map()
      if (is.null(idx_serasi_map_raw) || is.null(rv$tss_rast) || is.null(rv$river_vect)) {
        showNotification("Harap lengkapi Peta SERASI, Peta TSS, dan Data Vektor Sungai terlebih dahulu.",
                         type = "error", duration = 5)
        return()
      }

      buf_rad <- input$buffer_radius
      if (is.null(buf_rad) || !is.numeric(buf_rad) || length(buf_rad) != 1 || is.na(buf_rad) || !is.finite(buf_rad) || buf_rad <= 0) {
        showNotification("Radius buffer sungai harus berupa angka positif.", type = "error", duration = 5)
        return()
      }

      rv$analysis_result <- NULL
      rv$gpkg_path <- NULL
      rv$xlsx_path <- NULL
      rv$log_messages <- ""

      append_log("Memulai analisis PADU-HS...")

      withProgress(message = "Menjalankan Analisis PADU-HS", value = 0, {
        tryCatch({
          incProgress(0.1, detail = "Memuat data...")
          append_log("Data berhasil dimuat.")

          incProgress(0.1, detail = "Mempersiapkan perhitungan...")
          append_log("Menghitung indeks PADU-HS...")

          if ("length" %in% colnames(idx_serasi_map_raw)) {
            idx_serasi_map <- dissolve_id_pu(idx_serasi_map_raw)
          } else {
            idx_serasi_map <- idx_serasi_map_raw
          }

          incProgress(0.2, detail = "Memproses buffer sungai dan TSS...")
          warn_msgs <- character(0)
          padu_hs <- withCallingHandlers(
            calculate_padu_hs(
              idx_serasi_map = idx_serasi_map,
              river_vect     = rv$river_vect,
              tss_rast       = rv$tss_rast,
              buffer_radius  = buf_rad
            ),
            warning = function(w) {
              msg <- conditionMessage(w)
              warn_msgs <<- c(warn_msgs, msg)
              append_log(paste("PERINGATAN:", msg))
              invokeRestart("muffleWarning")
            }
          )
          if (length(warn_msgs) > 0) {
            showNotification(paste(unique(warn_msgs), collapse = " | "),
                             type = "warning", duration = 8)
          }

          incProgress(0.3, detail = "Menggabungkan hasil...")
          idx_padu_hs_map <- padu_hs$idx_padu_hs_map
          idx_padu_hs_table <- padu_hs$idx_padu_hs
          append_log("Perhitungan indeks selesai.")

          incProgress(0.2, detail = "Menyimpan hasil...")
          append_log("Menyimpan hasil ke disk...")

          padu_hs_dir <- file.path(output_dir(), "Analisis PADU-HS")
          if (!dir.exists(padu_hs_dir))
            dir.create(padu_hs_dir, recursive = TRUE, showWarnings = FALSE)

          gpkg_path <- file.path(padu_hs_dir, "idx_padu_hs.gpkg")
          xlsx_path <- file.path(padu_hs_dir, "idx_padu_hs.xlsx")

          sf::st_write(idx_padu_hs_map, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
          openxlsx::write.xlsx(idx_padu_hs_table, xlsx_path)

          rv$gpkg_path <- gpkg_path
          rv$xlsx_path <- xlsx_path
          rv$analysis_result <- list(map = idx_padu_hs_map, table = idx_padu_hs_table)

          out <- list(
            inputs = list(
              start_time         = Sys.time(),
              idx_serasi_path    = serasi_in$filename(),
              idx_serasi_source  = serasi_in$source(),
              serasi_source_name = serasi_in$filename(),
              serasi_source_hash = serasi_in$hash(),
              tss_path           = input$tss_file,
              river_vect_path    = input$river_file,
              buffer_radius      = buf_rad,
              output_dir         = output_dir()
            ),
            result = list(
              idx_serasi_map    = idx_serasi_map,
              river_vect        = rv$river_vect,
              tss_rast          = rv$tss_rast,
              idx_padu_hs_map   = idx_padu_hs_map,
              idx_padu_hs_table = idx_padu_hs_table
            )
          )

          log_dir <- file.path(padu_hs_dir, "log")
          if (!dir.exists(log_dir))
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
          tryCatch({
            inputs <- out$inputs
            save(inputs, file = file.path(log_dir, "idx_padu_hs_log.rda"))
          }, error = function(e) warning("Gagal menulis log: ", e$message))

          session$userData$module_results$padu_hs <- out

          plot_continuous_map(map = idx_padu_hs_map, column = "idx_padu_hs",
                              title = "Peta Indeks PADU-HS", legend = "Indeks PADU-HS",
                              low = "red", high = "lightgreen",
                              filepath = file.path(log_dir, "idx_padu_hs.png"))
          .plot_river_buffer_overlay(
            idx_padu_hs_map = idx_padu_hs_map,
            river_vect      = rv$river_vect,
            buffer_radius   = buf_rad,
            filepath        = file.path(log_dir, "buffer_sungai.png")
          )
          plot_continuous_map(map = rv$tss_rast, column = NA,
                              title = "Peta Total Suspended Solid (TSS)", legend = "TSS (mg/L)",
                              low = "darkblue", high = "yellow",
                              filepath = file.path(log_dir, "tss.png"))

          append_log(paste("Peta disimpan \u2192", gpkg_path))
          append_log(paste("Tabel disimpan \u2192", xlsx_path))
          append_log("Analisis PADU-HS berhasil diselesaikan.")

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
      } else if (rv$unlocked >= 2 && !is.null(rv$river_vect) && !is.null(rv$tss_rast) &&
                 !is.null(serasi_in$idx_serasi_map())) {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"), "Siap menjalankan analisis.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"), "Lengkapi langkah sebelumnya.")
      }
    })

    padu_hs_config <- list(
      map_color_col = "idx_padu_hs",
      map_title     = "Indeks PADU-HS",
      map_palette   = "RdYlGn",
      map_label_cols = c(
        "ID PU"          = "id_pu",
        "RTRW"           = "RTRW",
        "RZWP3K"         = "RZWP3K",
        "Indeks Sungai"  = "river_index",
        "TSS (mg/L)"     = "tss_mean",
        "Indeks TSS"     = "tss_index",
        "Indeks PADU-HS" = "idx_padu_hs"
      ),
      table_cols = c(
        "id_pu"       = "ID PU",
        "RTRW"        = "RTRW",
        "RZWP3K"      = "RZWP3K",
        "admin"       = "Administrasi",
        "area_ha"     = "Luas (ha)",
        "river_index" = "Indeks Sungai",
        "tss_mean"    = "Total Suspended Solids (mg/L)",
        "tss_index"   = "Indeks TSS",
        "idx_padu_hs" = "Indeks PADU-HS"
      ),
      table_round_cols = c("Luas (ha)", "Total Suspended Solids (mg/L)",
                           "Indeks TSS", "Indeks PADU-HS")
    )

    render_result_server(input, output, session, rv, padu_hs_config)
  })
}
