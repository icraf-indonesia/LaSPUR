# ui/modules/mod_padu_ki.R
# ============================================================
#  MODULE: PADU-KI (2.7 PADU-KI: Disaster Risk Analysis)
# ============================================================

source("../R/functions.R")
source("../R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
padu_ki_ui <- function(id) {
  ns <- NS(id)
  tagList(

    div(
      style = "margin-bottom: 20px;",
      h4("2.7 PADU-KI: Analisis Risiko Bencana", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menghitung indeks PADU-KI dengan mengekstrak nilai risiko bencana ke dalam unit perencanaan.",
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

        tags$p(tags$i(class = "bi bi-exclamation-triangle me-1"),
               "Shapefile Risiko Bencana (KRB)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Unggah vektor KRB (contoh: 08_ST_KRB_clim_diss.shp)."
        ),
        fileInput(ns("disaster_risk_file"),
                  label    = NULL,
                  accept   = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),

        hr(),

        textInput(ns("risk_col_name"), "Nama Kolom Atribut Risiko", value = "Kerawanan"),

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
padu_ki_server <- function(id, output_dir) {
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

    disaster_risk_vect <- reactive({
      req(input$disaster_risk_file)
      load_and_validate_shapefile(extract_shp_path(input$disaster_risk_file))
    })

    # ── Run analysis ─────────────────────────────────────────
    observeEvent(input$btn_run, {
      req(!is_running(), input$idx_serasi_file, input$disaster_risk_file)

      is_running(TRUE)
      analysis_result(NULL)

      tryCatch({
        # Ekstrak nilai risiko
        extracted <- extract_sf_to_sf(
          pu        = idx_serasi_map(),
          value_sf  = disaster_risk_vect(),
          value_col = input$risk_col_name,
          new_col   = "disaster_risk_mean",
          pu_id     = "id_pu"
        )

        # Hitung indeks PADU-KI
        idx_padu_ki_map <- extracted %>%
          mutate(
            idx_padu_ki = if_else(is.na(disaster_risk_mean), NA_real_, 1 - disaster_risk_mean)
          ) %>%
          select(-disaster_risk_mean)

        # Simpan hasil
        out_path <- file.path(output_dir(), "idx_padu_ki.gpkg")
        sf::write_sf(idx_padu_ki_map, out_path, delete_dsn = TRUE)

        idx_padu_ki_table <- as_tibble(sf::st_drop_geometry(idx_padu_ki_map))
        analysis_result(list(map = idx_padu_ki_map, table = idx_padu_ki_table))
        analysis_log("Analisis PADU-KI selesai dan disimpan ke direktori output.")
        showNotification("Berhasil: Perhitungan PADU-KI selesai.", type = "message", duration = 5)

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
      plot(analysis_result()$map["idx_padu_ki"], main = "Peta Indeks PADU-KI (Ketahanan Bencana)")
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
