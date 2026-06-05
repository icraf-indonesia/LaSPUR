# ui/modules/mod_padu_rtp.R
# ============================================================
#  MODULE: PADU-RTp (2.5 PADU-RTp)
# ============================================================

source("../R/functions.R")
source("../R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
padu_rtp_ui <- function(id) {
  ns <- NS(id)
  tagList(

    div(
      style = "margin-bottom: 20px;",
      h4("2.5 PADU-RTp: Jarak Industri & Alur Pelayaran", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menghitung indeks PADU-RTp berdasarkan jarak ke kawasan industri dan alur pelayaran.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),

    layout_column_wrap(
      width = 1/2,

      # ── Card A: Input & Parameter ────────────────────────
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

        tags$p(tags$i(class = "bi bi-building me-1"),
               "1. Input Jarak Industri",
               style = "font-weight: 600; margin-bottom: 4px;"),
        radioButtons(ns("ind_input_type"), label = NULL,
                     choices = c("Unggah Vektor (hitung jarak otomatis)" = "vector",
                                 "Unggah Raster Jarak yang Sudah Ada (.tif)" = "raster"),
                     inline = FALSE),
        uiOutput(ns("ui_ind_file")),
        numericInput(ns("max_ind_dist"), "Skala Jarak Maksimum Industri (m)", value = 8000, min = 1),

        hr(),

        tags$p(tags$i(class = "bi bi-water me-1"),
               "2. Input Jarak Alur Pelayaran",
               style = "font-weight: 600; margin-bottom: 4px;"),
        radioButtons(ns("pel_input_type"), label = NULL,
                     choices = c("Unggah Vektor (hitung jarak otomatis)" = "vector",
                                 "Unggah Raster Jarak yang Sudah Ada (.tif)" = "raster"),
                     inline = FALSE),
        uiOutput(ns("ui_pel_file")),
        numericInput(ns("max_pel_dist"), "Skala Jarak Maksimum Alur Pelayaran (m)", value = 5000, min = 1),

        hr(),

        numericInput(ns("calc_resolution"), "Resolusi Perhitungan Jarak Otomatis (m)", value = 30, min = 1),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Hanya digunakan jika opsi 'Unggah Vektor' dipilih di atas."
        ),

        hr(),

        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_run"),
                       tagList(tags$i(class = "bi bi-play-fill me-1"),
                               "Jalankan Analisis"),
                       class = "btn-success btn-sm")
        )
      ),

      # ── Card B: Output & Hasil ───────────────────────────
      card(
        card_header("Output & Hasil"),

        uiOutput(ns("status_box")),

        hr(),

        navset_tab(
          nav_panel(
            "Peta",
            plotOutput(ns("result_map"), height = "300px")
          ),
          nav_panel(
            "Tabel",
            div(
              style = "overflow-x: auto; max-height: 300px; overflow-y: auto;",
              tableOutput(ns("result_table"))
            )
          ),
          nav_panel(
            "Log Validasi",
            verbatimTextOutput(ns("validation_log"))
          )
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────
padu_rtp_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {

    analysis_result <- reactiveVal(NULL)
    analysis_log    <- reactiveVal("Belum ada analisis yang dijalankan.")
    is_running      <- reactiveVal(FALSE)

    # ── Dynamic UI for File Inputs ────────────────────────────
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

    # ── Rename sidecar files and return .shp path ───
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

    # ── Extract .gpkg or .shp path from upload ──────
    extract_vector_path <- function(file_input) {
      gpkg_row <- file_input[grepl("\\.gpkg$", file_input$name, ignore.case = TRUE), ]
      if (nrow(gpkg_row) == 1) return(gpkg_row$datapath)
      extract_shp_path(file_input)
    }

    # ── Reactives ────────────────────────────────────────────
    idx_serasi_map <- reactive({
      req(input$idx_serasi_file)
      load_and_validate_shapefile(extract_vector_path(input$idx_serasi_file))
    })

    # ── Run analysis ─────────────────────────────────────────
    observeEvent(input$btn_run, {
      req(!is_running())
      req(input$idx_serasi_file)

      if (input$ind_input_type == "vector") req(input$ind_file_vect) else req(input$ind_file_rast)
      if (input$pel_input_type == "vector") req(input$pel_file_vect) else req(input$pel_file_rast)

      is_running(TRUE)
      analysis_result(NULL)
      analysis_log("Memulai analisis PADU-RTp...")

      tryCatch({
        analysis_log("Memproses data industri...")
        if (input$ind_input_type == "vector") {
          ind_vect <- load_and_validate_shapefile(extract_shp_path(input$ind_file_vect))
          industry_euc_dist <- calculate_euclidean_dist(ind_vect, idx_serasi_map(), resolution = input$calc_resolution)
        } else {
          industry_euc_dist <- terra::rast(input$ind_file_rast$datapath)
        }

        analysis_log("Memproses data alur pelayaran...")
        if (input$pel_input_type == "vector") {
          pel_vect <- load_and_validate_shapefile(extract_shp_path(input$pel_file_vect))
          pelayaran_euc_dist <- calculate_euclidean_dist(pel_vect, idx_serasi_map(), resolution = input$calc_resolution)
        } else {
          pelayaran_euc_dist <- terra::rast(input$pel_file_rast$datapath)
        }

        analysis_log("Mengekstrak nilai jarak ke zona spasial...")
        industry_dist_extracted <- extract_raster_to_sf(
          idx_serasi_map(),
          industry_euc_dist,
          id_col  = "id_pu",
          new_col = "industry_dist_mean"
        )

        pelayaran_dist_extracted <- extract_raster_to_sf(
          idx_serasi_map(),
          pelayaran_euc_dist,
          id_col  = "id_pu",
          new_col = "pelayaran_dist_mean"
        )

        analysis_log("Menghitung indeks PADU-RTp...")
        industry_to_merge <- industry_dist_extracted %>%
          sf::st_drop_geometry() %>%
          dplyr::select(id_pu, industry_dist_mean)

        idx_padu_rtp_map <- pelayaran_dist_extracted %>%
          dplyr::left_join(industry_to_merge, by = "id_pu") %>%
          dplyr::mutate(
            industry_clean   = dplyr::if_else(is.na(industry_dist_mean), 0, pmax(industry_dist_mean, 0)),
            pelayaran_clean  = dplyr::if_else(is.na(pelayaran_dist_mean), 0, pmax(pelayaran_dist_mean, 0)),
            filter_industry  = 1 - pmin(industry_clean / input$max_ind_dist, 1),
            filter_pelayaran = 1 - pmin(pelayaran_clean / input$max_pel_dist, 1),
            idx_padu_rtp     = pmax(0, 1 - (filter_industry + filter_pelayaran) / 2)
          ) %>%
          dplyr::select(-industry_clean, -pelayaran_clean, -filter_industry, -filter_pelayaran)

        out_gpkg <- file.path(output_dir(), "idx_padu_rtp.gpkg")
        sf::st_write(idx_padu_rtp_map, out_gpkg, delete_dsn = TRUE, quiet = TRUE)

        result_table <- dplyr::as_tibble(sf::st_drop_geometry(idx_padu_rtp_map))
        analysis_result(list(map = idx_padu_rtp_map, table = result_table))
        analysis_log("Analisis PADU-RTp berhasil diselesaikan.")
        showNotification("Analisis selesai! Periksa tab Peta dan Tabel.",
                         type = "message", duration = 5)

      }, error = function(e) {
        msg <- conditionMessage(e)
        if (is.null(msg) || msg == "") msg <- "Error tidak diketahui (lihat konsol untuk detail)"
        analysis_log(paste("Error:", msg))
        showNotification(paste("Analisis gagal:", msg), type = "error", duration = 10)
      })

      is_running(FALSE)
    })

    # ── Status box ───────────────────────────────────────────
    output$status_box <- renderUI({
      if (is_running()) {
        div(class = "alert alert-info mb-0",
            tags$i(class = "bi bi-hourglass-split me-2"),
            "Menjalankan analisis...")
      } else if (!is.null(analysis_result())) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Analisis selesai.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Siap. Unggah file dan klik Jalankan Analisis.")
      }
    })

    # ── Map output ───────────────────────────────────────────
    output$result_map <- renderPlot({
      req(analysis_result())
      plot(analysis_result()$map["idx_padu_rtp"], main = "Peta Indeks PADU-RTp")
    })

    # ── Table output ─────────────────────────────────────────
    output$result_table <- renderTable({
      req(analysis_result())
      head(analysis_result()$table, 50)
    })

    # ── Validation log ───────────────────────────────────────
    output$validation_log <- renderText({
      analysis_log()
    })

  })
}
