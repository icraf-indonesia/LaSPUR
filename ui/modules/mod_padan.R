# ui/modules/mod_padan.R
# ============================================================
#  MODULE: PADAN Analysis (3. PADAN)
# ============================================================

source("../R/functions.R")
source("../R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
padan_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("3. PADAN Analysis", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menghitung nilai akhir integrasi (Indeks PADAN) berdasarkan penggabungan Indeks SERASI dan Indeks PADU.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),
    
    layout_column_wrap(
      width = 1/2,
      
      # ── Card A: Input & Parameters ───────────────────────
      card(
        card_header("Input & Parameters"),
        
        # File input untuk hasil PADU Combine (idx_padu.gpkg)
        fileInput(ns("idx_padu_file"), "Pilih Hasil Analisis PADU (.gpkg)",
                  accept = ".gpkg"),
        tags$div(
          class = "form-text text-muted",
          style = "margin-top: -8px; margin-bottom: 12px; font-size: 0.8rem;",
          "Gunakan file 'idx_padu.gpkg' dari modul 2.8"
        ),
        
        hr(),
        
        # Slider untuk nilai Alpha
        sliderInput(ns("alpha_val"), 
                    label = tags$span("Proporsi Alpha (\u03B1)", 
                                      tags$i(class = "bi bi-info-circle ms-1", 
                                             title = "Alpha: Bobot untuk SERASI. (1-Alpha): Bobot untuk PADU Final")),
                    min = 0, max = 1, value = 0.5, step = 0.1),
        
        div(
          style = "background: #f8f9fa; padding: 10px; border-radius: 6px; font-size: 0.85rem;",
          tags$strong("Formula:"), br(),
          tags$code("(\u03B1 * idx_serasi) + ((1 - \u03B1) * idx_padu_final)")
        ),
        
        hr(),
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_run"),
                       tagList(tags$i(class = "bi bi-play-fill me-1"),
                               "Run PADAN Analysis"),
                       class = "btn-success btn-sm")
        )
      ),
      
      # ── Card B: Output & Results ─────────────────────────
      card(
        card_header("Output & Results"),
        
        uiOutput(ns("status_box")),
        
        hr(),
        
        navset_tab(
          nav_panel(
            "Map",
            plotOutput(ns("result_map"), height = "300px")
          ),
          nav_panel(
            "Table",
            div(
              style = "overflow-x: auto; max-height: 300px; overflow-y: auto;",
              tableOutput(ns("result_table"))
            )
          ),
          nav_panel(
            "Validation Log",
            verbatimTextOutput(ns("validation_log"))
          )
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────
padan_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    
    analysis_result <- reactiveVal(NULL)
    analysis_log    <- reactiveVal("Belum ada analisis yang dijalankan.")
    is_running      <- reactiveVal(FALSE)
    
    # ── Run Analysis ────────────────────────────────────────
    observeEvent(input$btn_run, {
      req(!is_running())
      req(input$idx_padu_file)
      
      is_running(TRUE)
      analysis_result(NULL)
      
      tryCatch({
        # Load Geopackage dari PADU Combine
        idx_padu_map <- sf::st_read(input$idx_padu_file$datapath, quiet = TRUE)
        
        # Validasi kolom yang diperlukan
        required_cols <- c("idx_serasi", "idx_padu_final")
        missing_cols <- setdiff(required_cols, names(idx_padu_map))
        
        if (length(missing_cols) > 0) {
          stop(paste("Kolom berikut tidak ditemukan dalam file:", paste(missing_cols, collapse = ", ")))
        }
        
        # Hitung Indeks PADAN
        alpha <- input$alpha_val
        
        idx_padan_map <- idx_padu_map %>%
          dplyr::mutate(
            idx_padan = (alpha * idx_serasi) + ((1 - alpha) * idx_padu_final)
          )
        
        # Simpan Hasil
        out_path <- file.path(output_dir(), "idx_padan.gpkg")
        sf::st_write(idx_padan_map, out_path, delete_dsn = TRUE, quiet = TRUE)
        
        # Update Status
        analysis_result(list(
          map = idx_padan_map, 
          table = sf::st_drop_geometry(idx_padan_map)
        ))
        
        analysis_log(paste0(
          "Analisis PADAN selesai.\n",
          "Parameter Alpha: ", alpha, "\n",
          "File tersimpan di: ", out_path
        ))
        
        showNotification("Analisis PADAN berhasil!", type = "message")
        
      }, error = function(e) {
        analysis_log(paste("Error:", e$message))
        showNotification(paste("Gagal:", e$message), type = "error", duration = 8)
      })
      
      is_running(FALSE)
    })
    
    # ── UI Outputs ───────────────────────────────────────────
    output$status_box <- renderUI({
      if (is_running()) {
        div(class = "alert alert-info mb-0", tags$i(class = "bi bi-hourglass-split me-2"), "Menghitung PADAN...")
      } else if (!is.null(analysis_result())) {
        div(class = "alert alert-success mb-0", tags$i(class = "bi bi-check-circle me-2"), "Selesai.")
      } else {
        div(class = "alert alert-secondary mb-0", "Siap dijalankan.")
      }
    })
    
    output$result_map <- renderPlot({
      req(analysis_result())
      plot(analysis_result()$map["idx_padan"], main = "Indeks PADAN (Integrasi Akhir)", border = 0.1)
    })
    
    output$result_table <- renderTable({
      req(analysis_result())
      # Tampilkan kolom utama saja untuk ringkasan
      head(analysis_result()$table[, c("id_pu", "idx_serasi", "idx_padu_final", "idx_padan")], 100)
    })
    
    output$validation_log <- renderText({ analysis_log() })
  })
}