# ui/modules/mod_overlap.R
# ============================================================
#  MODULE: Overlap (1.1 Type 1: Overlap)
# ============================================================

source("../R/functions.R")
source("../R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
overlap_ui <- function(id) {
  ns <- NS(id)
  tagList(

    div(
      style = "margin-bottom: 20px;",
      h4("1.1 Identifikasi Konflik Spasial: Tumpang Tindih", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Mengidentifikasi tumpang tindih spasial antara zona RTRW dan RZWP3K serta menghitung indeks SERASI.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),

    layout_column_wrap(
      width = 1/2,

      # ── Card A: Input & Parameter ────────────────────────
      card(
        card_header("Input & Parameter"),

        tags$p(tags$i(class = "bi bi-map me-1"),
               "Shapefile Peta RTRW",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Unggah semua komponen shapefile RTRW (.shp, .dbf, .prj, .shx)."
        ),
        fileInput(ns("rtrw_file"),
                  label    = NULL,
                  accept   = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),

        tags$p(tags$i(class = "bi bi-map me-1"),
               "Shapefile Peta RZWP3K",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Unggah semua komponen shapefile RZWP3K (.shp, .dbf, .prj, .shx)."
        ),
        fileInput(ns("rzwp3k_file"),
                  label    = NULL,
                  accept   = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),

        hr(),

        tags$p(tags$i(class = "bi bi-table me-1"),
               "Tabel Prioritas RTRW (.xlsx)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        fileInput(ns("rtrw_prioritas_file"),
                  label  = NULL,
                  accept = ".xlsx"),

        tags$p(tags$i(class = "bi bi-table me-1"),
               "Tabel Prioritas RZWP3K (.xlsx)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        fileInput(ns("rzwp3k_prioritas_file"),
                  label  = NULL,
                  accept = ".xlsx"),

        tags$p(tags$i(class = "bi bi-grid-3x3 me-1"),
               "Tabel Matriks SERASI (.xlsx)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        fileInput(ns("matriks_serasi_file"),
                  label  = NULL,
                  accept = ".xlsx"),

        hr(),

        tags$p(tags$i(class = "bi bi-sliders me-1"),
               "Parameter",
               style = "font-weight: 600; margin-bottom: 4px;"),
        numericInput(ns("threshold_ha"),
                     "Ambang Batas Luas Minimum (ha)",
                     value = 156.25, min = 0),

        hr(),

        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_generate_matrix"),
                       tagList(tags$i(class = "bi bi-file-earmark-excel me-1"),
                               "Buat Template Matriks"),
                       class = "btn-outline-primary btn-sm"),
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
overlap_server <- function(id, output_dir) {
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

    # ── Reactives ────────────────────────────────────────────
    rtrw_vect <- reactive({
      req(input$rtrw_file)
      load_and_validate_shapefile(extract_shp_path(input$rtrw_file))
    })

    rzwp3k_vect <- reactive({
      req(input$rzwp3k_file)
      load_and_validate_shapefile(extract_shp_path(input$rzwp3k_file))
    })

    # ── Generate matrix template ─────────────────────────────
    observeEvent(input$btn_generate_matrix, {
      req(rtrw_vect(), rzwp3k_vect())
      tryCatch({
        template <- generate_matrix_serasi(sf_1 = rtrw_vect(), sf_2 = rzwp3k_vect())
        out_path <- file.path(output_dir(), "matriks_serasi.xlsx")
        write.xlsx(template, out_path, overwrite = TRUE)
        showNotification(paste("Template matriks dibuat →", out_path),
                         type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Gagal membuat template matriks:", e$message),
                         type = "error", duration = 8)
      })
    })

    # ── Run analysis ─────────────────────────────────────────
    observeEvent(input$btn_run, {
      req(!is_running())
      req(input$rtrw_file, input$rzwp3k_file,
          input$rtrw_prioritas_file,
          input$rzwp3k_prioritas_file,
          input$matriks_serasi_file)

      is_running(TRUE)
      analysis_result(NULL)

      tryCatch({
        matriks_serasi   <- load_validate_matrix_table(
          input$matriks_serasi_file$datapath, title = "serasi"
        )
        rtrw_prioritas   <- load_and_validate_table(input$rtrw_prioritas_file$datapath)
        rzwp3k_prioritas <- load_and_validate_table(input$rzwp3k_prioritas_file$datapath)

        union_sf          <- identify_overlaps(rtrw_vect(), rzwp3k_vect())
        filtered_union_sf <- filter_overlaps(union_sf, input$threshold_ha)

        valid_class <- validate_zone_class(
          filtered_union_sf, rtrw_prioritas, rzwp3k_prioritas
        )

        if (length(valid_class$mismatch_col3) == 0 &&
            length(valid_class$mismatch_col4) == 0) {

          idx_serasi_map   <- merge_attributes_to_map(filtered_union_sf, matriks_serasi)
          idx_serasi_table <- as_tibble(idx_serasi_map %>% sf::st_drop_geometry())

          out_path <- file.path(output_dir(), "idx_serasi.gpkg")
          write_sf(idx_serasi_map, out_path, overwrite = TRUE)

          analysis_result(list(map = idx_serasi_map, table = idx_serasi_table))
          analysis_log("Semua nama kelas cocok. Nilai kompatibilitas berhasil digabungkan.")
          showNotification(paste("Analisis selesai. Hasil disimpan ke", out_path),
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
          analysis_log(log_msg)
          showNotification("Ketidakcocokan terdeteksi. Periksa tab Log Validasi.",
                           type = "warning", duration = 8)
        }

      }, error = function(e) {
        analysis_log(paste("Error:", e$message))
        showNotification(paste("Analisis gagal:", e$message),
                         type = "error", duration = 8)
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
      plot(analysis_result()$map["idx_serasi"], main = "Peta Indeks SERASI")
    })

    # ── Table output ─────────────────────────────────────────
    output$result_table <- renderTable({
      req(analysis_result())
      analysis_result()$table
    })

    # ── Validation log ───────────────────────────────────────
    output$validation_log <- renderText({
      analysis_log()
    })

  })
}
