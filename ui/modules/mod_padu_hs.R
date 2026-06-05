# ui/modules/mod_padu_hs.R
# ============================================================
#  MODULE: PADU-HS (2.2)
# ============================================================

source("../R/functions.R")
source("../R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
padu_hs_ui <- function(id) {
  ns <- NS(id)
  tagList(

    div(
      style = "margin-bottom: 20px;",
      h4("2.2 PADU-HS", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menghitung indeks PADU-HS berdasarkan jarak estuari dan nilai raster TSS.",
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

        tags$p(tags$i(class = "bi bi-layers me-1"),
               "Raster TSS (.tif)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "File raster nilai Total Suspended Solid (TSS)."
        ),
        fileInput(ns("tss_file"),
                  label  = NULL,
                  accept = c(".tif", ".tiff")),

        hr(),

        tags$p(tags$i(class = "bi bi-geo-alt me-1"),
               "Input Jarak Estuari",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Unggah raster jarak yang sudah dihitung, atau unggah shapefile estuari untuk dihitung otomatis."
        ),

        radioButtons(
          ns("estuari_input_mode"),
          label   = NULL,
          choices = c(
            "Unggah raster jarak yang sudah ada (.tif)" = "upload_raster",
            "Unggah shapefile estuari dan hitung otomatis" = "calculate"
          ),
          selected = "upload_raster"
        ),

        # Mode A: unggah raster langsung
        conditionalPanel(
          condition = sprintf("input['%s'] == 'upload_raster'", ns("estuari_input_mode")),
          fileInput(ns("euc_dist_file"),
                    label  = "Raster Jarak Estuari (.tif)",
                    accept = c(".tif", ".tiff"))
        ),

        # Mode B: unggah shapefile → hitung
        conditionalPanel(
          condition = sprintf("input['%s'] == 'calculate'", ns("estuari_input_mode")),
          fileInput(ns("estuari_file"),
                    label    = "Shapefile Estuari",
                    accept   = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                    multiple = TRUE),
          numericInput(ns("euc_resolution"),
                       "Resolusi Perhitungan (meter)",
                       value = 30, min = 1),
          div(
            class = "alert alert-warning py-2 px-3",
            style = "font-size: 0.85rem;",
            tags$i(class = "bi bi-exclamation-triangle me-1"),
            "Jarak Euclidean akan dihitung dari shapefile di atas. Proses ini dapat memakan beberapa menit untuk dataset besar. Hasilnya akan otomatis disimpan ke direktori output."
          )
        ),

        hr(),

        tags$p(tags$i(class = "bi bi-sliders me-1"),
               "Parameter",
               style = "font-weight: 600; margin-bottom: 4px;"),
        numericInput(ns("estuari_dist_max"),
                     "Jarak Estuari Maksimum (meter)",
                     value = 5000, min = 1),

        hr(),

        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_run"),
                       tagList(tags$i(class = "bi bi-play-fill me-1"), "Jalankan Analisis"),
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
            "Log",
            verbatimTextOutput(ns("run_log"))
          )
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────
padu_hs_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {

    analysis_result <- reactiveVal(NULL)
    run_log         <- reactiveVal("Belum ada analisis yang dijalankan.")
    is_running      <- reactiveVal(FALSE)

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

    # ── Log helper ───────────────────────────────────────────
    log_append <- function(msg) {
      current <- run_log()
      if (current == "Belum ada analisis yang dijalankan.") current <- ""
      run_log(paste0(current, format(Sys.time(), "[%H:%M:%S] "), msg, "\n"))
    }

    # ── Reactives ────────────────────────────────────────────
    idx_serasi_map <- reactive({
      req(input$idx_serasi_file)
      path <- extract_vector_path(input$idx_serasi_file)
      load_and_validate_shapefile(path)
    })

    tss_rast <- reactive({
      req(input$tss_file)
      load_and_validate_raster(input$tss_file$datapath)
    })

    estuari_vect <- reactive({
      req(input$estuari_input_mode == "calculate")
      req(input$estuari_file)
      load_and_validate_shapefile(extract_shp_path(input$estuari_file))
    })

    euc_dist_rast <- reactive({
      if (input$estuari_input_mode == "upload_raster") {
        req(input$euc_dist_file)
        log_append("Memuat raster jarak estuari yang diunggah...")
        terra::rast(input$euc_dist_file$datapath)
      } else {
        req(estuari_vect(), idx_serasi_map())
        log_append("Menghitung jarak Euclidean dari shapefile estuari...")
        euc <- calculate_euclidean_dist(
          estuari_vect(),
          idx_serasi_map(),
          resolution = input$euc_resolution
        )
        out_path <- file.path(output_dir(), "estuari_euc_dist.tif")
        terra::writeRaster(euc, out_path, overwrite = TRUE)
        log_append(paste("Raster jarak disimpan otomatis →", out_path))
        euc
      }
    })

    # ── Run analysis ─────────────────────────────────────────
    observeEvent(input$btn_run, {
      req(!is_running())
      req(input$idx_serasi_file, input$tss_file)

      if (input$estuari_input_mode == "upload_raster") {
        req(input$euc_dist_file)
      } else {
        req(input$estuari_file)
      }

      is_running(TRUE)
      analysis_result(NULL)
      run_log("Belum ada analisis yang dijalankan.")

      tryCatch({
        log_append("Memulai analisis PADU-HS...")

        # Langkah 1: Ekstrak jarak estuari ke area tumpang tindih
        log_append("Mengekstrak jarak estuari ke area tumpang tindih...")
        estuari_dist_extracted <- extract_raster_to_sf(
          idx_serasi_map(),
          euc_dist_rast(),
          id_col  = "id_pu",
          new_col = "estuari_dist_mean"
        )

        # Langkah 2: Ekstrak nilai TSS ke area tumpang tindih
        log_append("Mengekstrak nilai TSS ke area tumpang tindih...")
        tss_extracted <- extract_raster_to_sf(
          idx_serasi_map(),
          tss_rast(),
          id_col  = "id_pu",
          new_col = "tss_mean"
        )

        # Langkah 3: Gabungkan dan hitung indeks PADU-HS
        log_append("Menghitung indeks PADU-HS...")
        tss_to_merge <- tss_extracted %>%
          sf::st_drop_geometry() %>%
          dplyr::select(id_pu, tss_mean)

        dist_max <- input$estuari_dist_max

        idx_padu_hs_map <- estuari_dist_extracted %>%
          dplyr::left_join(tss_to_merge, by = "id_pu") %>%
          dplyr::mutate(
            filter_estuari = ifelse(
              is.na(estuari_dist_mean), NA,
              1 - pmin(estuari_dist_mean / dist_max, 1)
            ),
            idx_padu_hs = (filter_estuari + tss_mean) / 2
          ) %>%
          dplyr::select(-filter_estuari)

        idx_padu_hs_table <- as_tibble(idx_padu_hs_map %>% sf::st_drop_geometry())

        analysis_result(list(map = idx_padu_hs_map, table = idx_padu_hs_table))

        # Auto-save hasil
        out_map   <- file.path(output_dir(), "idx_padu_hs.gpkg")
        out_table <- file.path(output_dir(), "idx_padu_hs.csv")
        sf::write_sf(idx_padu_hs_map, out_map, delete_dsn = TRUE)
        write.csv(idx_padu_hs_table, out_table, row.names = FALSE)

        log_append("Analisis PADU-HS selesai.")
        log_append(paste("Peta disimpan →", out_map))
        log_append(paste("Tabel disimpan →", out_table))
        showNotification("Analisis PADU-HS selesai. Hasil disimpan ke direktori output.",
                         type = "message", duration = 5)

      }, error = function(e) {
        log_append(paste("Error:", e$message))
        showNotification(paste("Analisis gagal:", e$message), type = "error", duration = 8)
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
      plot(analysis_result()$map["idx_padu_hs"], main = "Peta Indeks PADU-HS")
    })

    # ── Table output ─────────────────────────────────────────
    output$result_table <- renderTable({
      req(analysis_result())
      analysis_result()$table
    })

    # ── Log output ───────────────────────────────────────────
    output$run_log <- renderText({
      run_log()
    })

  })
}
