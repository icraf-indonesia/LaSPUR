# ui/modules/mod_padu_combine.R
# ============================================================
#  MODULE: PADU Combine (2.8 PADU-Combine)
# ============================================================

source("../R/functions.R")
source("../R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
padu_combine_ui <- function(id) {
  ns <- NS(id)
  tagList(

    div(
      style = "margin-bottom: 20px;",
      h4("2.8 PADU-Combine: Penggabungan Analisis PADU", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menggabungkan hasil analisis PADU (KE, HS, KL, KH, RTp, SE, KI) menggunakan pembobotan.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),

    layout_column_wrap(
      width = 1/2,

      # ── Card A: Input & Parameter ────────────────────────
      card(
        card_header("Input & Parameter"),

        tags$p(tags$i(class = "bi bi-info-circle me-1"),
               "Peta Indeks SERASI (.gpkg)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Output dari modul 'Identifikasi Konflik Spasial' (idx_serasi.gpkg)."
        ),
        fileInput(ns("idx_serasi_file"),
                  label  = NULL,
                  accept = ".gpkg"),

        hr(),

        tags$p(tags$i(class = "bi bi-folder2-open me-1"),
               "Direktori File Analisis PADU",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Pilih folder yang berisi file idx_padu_*.gpkg dari semua modul PADU."
        ),
        shinyDirButton(ns("btn_browse_padu"), "Pilih Folder", "Pilih folder yang berisi file idx_padu_*.gpkg",
                       icon = icon("folder-open"), style = "width: 100%; margin-bottom: 8px;"),
        uiOutput(ns("padu_dir_status")),

        hr(),

        tags$p(tags$i(class = "bi bi-table me-1"),
               "Tabel Bobot PADU (.xlsx)",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Tabel bobot untuk setiap indeks PADU. Jumlah bobot harus sama dengan 1."
        ),
        fileInput(ns("weight_table_file"),
                  label  = NULL,
                  accept = ".xlsx"),

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
padu_combine_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {

    analysis_result <- reactiveVal(NULL)
    analysis_log    <- reactiveVal("Belum ada analisis yang dijalankan.")
    is_running      <- reactiveVal(FALSE)

    # ── Folder Selection Logic ────────────────────────────────
    roots <- c(Home = path.expand("~"), Project = normalizePath(".."), C = "C:/")
    shinyDirChoose(input, "btn_browse_padu", roots = roots, session = session)

    padu_folder_path <- reactive({
      req(input$btn_browse_padu)
      path <- parseDirPath(roots, input$btn_browse_padu)
      if (length(path) == 0 || path == "") return(NULL)
      as.character(path)
    })

    output$padu_dir_status <- renderUI({
      path <- padu_folder_path()
      if (!is.null(path)) {
        tags$small(style = "color: #18bc9c;", icon("check-circle"), basename(path))
      } else {
        tags$small(style = "color: #e74c3c;", icon("exclamation-circle"), "Belum memilih folder")
      }
    })

    # ── Run analysis ─────────────────────────────────────────
    observeEvent(input$btn_run, {
      req(!is_running())
      req(padu_folder_path(), input$idx_serasi_file, input$weight_table_file)

      is_running(TRUE)
      analysis_result(NULL)

      tryCatch({
        # Muat tabel bobot
        padu_idx_weight <- load_and_validate_table(input$weight_table_file$datapath)
        weight_sum <- sum(padu_idx_weight[[2]], na.rm = TRUE)

        if (weight_sum != 1) {
          stop("Jumlah bobot indeks PADU tidak sama dengan 1. Silakan periksa file Excel Anda.")
        }

        # Muat base map (idx_serasi)
        idx_serasi_map <- sf::st_read(input$idx_serasi_file$datapath, quiet = TRUE) %>%
          dplyr::select(-dplyr::any_of(c("area_ha", "area_flag")))

        # Daftar dan muat file PADU dari direktori secara otomatis
        padu_files <- list.files(padu_folder_path(), pattern = "^idx_padu_.*\\.gpkg$", full.names = TRUE)

        if (length(padu_files) == 0) {
          stop("Tidak ditemukan file idx_padu_*.gpkg di folder yang dipilih.")
        }

        # Baca semua file gpkg menjadi list sf object
        padu_list <- lapply(padu_files, function(f) {
          df <- sf::st_read(f, quiet = TRUE) %>% sf::st_drop_geometry()
          return(df)
        })

        # Hitung indeks PADU gabungan
        idx_padu_map <- calculate_padu_index(
          padu_list       = padu_list,
          idx_padu_map    = idx_serasi_map,
          padu_idx_weight = padu_idx_weight
        )

        # Simpan hasil
        out_path <- file.path(output_dir(), "idx_padu.gpkg")
        sf::st_write(idx_padu_map, out_path, delete_dsn = TRUE, quiet = TRUE)

        analysis_result(list(map = idx_padu_map, table = sf::st_drop_geometry(idx_padu_map)))
        analysis_log("Analisis PADU-Combine berhasil diselesaikan.")
        showNotification("Berhasil: Indeks PADU gabungan telah dibuat.", type = "message")

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
            "Menghitung indeks gabungan...")
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
      plot(analysis_result()$map["idx_padu_final"], main = "Peta Indeks PADU Gabungan", border = 1)
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
