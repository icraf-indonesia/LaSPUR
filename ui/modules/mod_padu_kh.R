# ui/modules/mod_padu_kh.R
# ============================================================
#  MODULE: PADU-KH (2.4 PADU-KH: Habitat Presence/Quality)
# ============================================================

source("../R/functions.R")
source("../R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
padu_kh_ui <- function(id) {
  ns <- NS(id)
  tagList(

    div(
      style = "margin-bottom: 20px;",
      h4("2.4 PADU-KH: Analisis Keberadaan Habitat", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menghitung persentase keberadaan habitat (Mangrove, Lamun, Terumbu Karang) dalam unit perencanaan.",
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

        tags$p(tags$i(class = "bi bi-tree me-1"),
               "Sumber Peta Habitat",
               style = "font-weight: 600; margin-bottom: 4px;"),
        radioButtons(ns("habitat_source"), label = NULL,
                     choices = c("Ekstrak dari Tutupan Lahan (LULC)" = "lulc",
                                 "Unggah File Habitat Terpisah"       = "manual"),
                     inline = TRUE),

        # Conditional UI untuk LULC
        conditionalPanel(
          condition = sprintf("input['%s'] == 'lulc'", ns("habitat_source")),
          fileInput(ns("lulc_file"), "Shapefile Tutupan Lahan (LULC)",
                    accept   = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                    multiple = TRUE),
          textInput(ns("habitat_ids"), "ID Kelas Habitat (pisahkan dengan koma)", value = "5, 6, 24, 25"),
          tags$small(class = "text-muted", "Default: 5,6 (Mangrove), 24 (Terumbu Karang), 25 (Lamun)")
        ),

        # Conditional UI untuk file terpisah
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("habitat_source")),
          fileInput(ns("coral_file"),    "Peta Terumbu Karang (.shp)", multiple = TRUE),
          fileInput(ns("seagrass_file"), "Peta Lamun (.shp)",          multiple = TRUE),
          fileInput(ns("mangrove_file"), "Peta Mangrove (.shp)",       multiple = TRUE)
        ),

        hr(),

        div(
          style = "display: flex; gap: 8px;",
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
            "Log Validasi",
            verbatimTextOutput(ns("validation_log"))
          )
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────
padu_kh_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {

    analysis_result <- reactiveVal(NULL)
    analysis_log    <- reactiveVal("Belum ada analisis yang dijalankan.")
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

    # ── Reactives ────────────────────────────────────────────
    idx_serasi_map <- reactive({
      req(input$idx_serasi_file)
      path <- extract_vector_path(input$idx_serasi_file)
      load_and_validate_shapefile(path)
    })

    # ── Run analysis ─────────────────────────────────────────
    observeEvent(input$btn_run, {
      req(!is_running())
      req(input$idx_serasi_file)

      is_running(TRUE)
      analysis_result(NULL)

      tryCatch({
        # Siapkan data habitat pesisir
        coastal_habitat <- NULL

        if (input$habitat_source == "lulc") {
          req(input$lulc_file)
          lulc_vect <- sf::st_read(extract_shp_path(input$lulc_file), quiet = TRUE)
          ids    <- as.numeric(unlist(strsplit(input$habitat_ids, ",")))
          id_col <- intersect(c("ID", "id"), names(lulc_vect))[1]
          coastal_habitat <- lulc_vect[lulc_vect[[id_col]] %in% ids, ]

        } else {
          req(input$coral_file, input$seagrass_file, input$mangrove_file)
          coastal_habitat <- list(
            sf::st_read(extract_shp_path(input$coral_file),    quiet = TRUE),
            sf::st_read(extract_shp_path(input$seagrass_file), quiet = TRUE),
            sf::st_read(extract_shp_path(input$mangrove_file), quiet = TRUE)
          )
        }

        # Hitung persentase tumpang tindih
        res_map <- calculate_overlay_pct(
          pu           = idx_serasi_map(),
          overlay_area = coastal_habitat,
          title        = "coastal_habitat"
        )

        # Hitung indeks akhir
        res_map <- res_map %>%
          mutate(idx_padu_kh = coastal_habitat_pct / 100)

        # Simpan hasil
        out_path <- file.path(output_dir(), "idx_padu_kh.gpkg")
        sf::st_write(res_map, out_path, delete_dsn = TRUE, quiet = TRUE)

        analysis_result(list(map = res_map, table = sf::st_drop_geometry(res_map)))
        analysis_log("Analisis PADU-KH berhasil diselesaikan.")
        showNotification("Analisis selesai!", type = "message", duration = 5)

      }, error = function(e) {
        analysis_log(paste("Error:", e$message))
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
      plot(analysis_result()$map["idx_padu_kh"], main = "Peta Indeks PADU-KH")
    })

    # ── Table output ─────────────────────────────────────────
    output$result_table <- renderTable({
      req(analysis_result())
      head(analysis_result()$table, 100)
    })

    # ── Validation log ───────────────────────────────────────
    output$validation_log <- renderText({
      analysis_log()
    })

  })
}
