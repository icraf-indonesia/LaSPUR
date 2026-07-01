# ui/modules/mod_recommendation.R
# ============================================================
#  MODULE: Recommendation (4. Recommendation)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
recommendation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 20px;",
      h4("4. Analisis Rekomendasi", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Menentukan opsi penyelesaian konflik dalam integrasi tata ruang darat-laut.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),
    
    layout_column_wrap(
      width = 1/2,
      
      # ── Card A: Input & Thresholds ───────────────────────
      card(
        card_header("Input & Parameters"),

        fileInput(ns("idx_padan_file"), "Pilih Peta Hasil Analisis PADAN (.gpkg)", accept = ".gpkg"),

        fileInput(ns("matrix_file"), "Pilih Matriks Serasi (.xlsx)", accept = ".xlsx"),
        
        layout_column_wrap(
          width = 1/2,
          fileInput(ns("rtrw_priority_file"), "Tabel Prioritas RTRW (.xlsx)", accept = ".xlsx"),
          fileInput(ns("rzwp3k_priority_file"), "Tabel Prioritas RZWP3K (.xlsx)", accept = ".xlsx")
        ),
        
        hr(),

        layout_column_wrap(
          width = 1/2,
          numericInput(ns("threshold_serasi"), "Nilai Ambang SERASI", value = 0.6, min = 0, max = 1, step = 0.05),
          numericInput(ns("threshold_padu"), "Nilai Ambang PADU", value = 0.65, min = 0, max = 1, step = 0.05)
        ),
        
        sliderInput(ns("alpha_val"), "Proporsi Alpha (\u03B1)", min = 0, max = 1, value = 0.5, step = 0.1),
        
        hr(),
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_run"),
                       tagList(tags$i(class = "bi bi-lightning-charge-fill me-1"), "Generate Recommendation"),
                       class = "btn-success btn-sm")
        )
      ),
      
      # ── Card B: Output & Results ─────────────────────────
      card(
        card_header("Output & Hasil Rekomendasi"),
        
        uiOutput(ns("status_box")),
        
        hr(),
        
        navset_tab(
          nav_panel(
            "Decision Table",
            div(
              style = "overflow-x: auto; max-height: 400px; overflow-y: auto;",
              tableOutput(ns("result_table"))
            )
          ),
          nav_panel(
            "Visual Log",
            verbatimTextOutput(ns("validation_log"))
          )
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────
recommendation_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    
    analysis_result <- reactiveVal(NULL)
    analysis_log    <- reactiveVal("Siap untuk analisis rekomendasi.")
    is_running      <- reactiveVal(FALSE)
    
    observeEvent(input$btn_run, {
      req(!is_running())
      req(input$idx_padan_file, input$matrix_file, 
          input$rtrw_priority_file, input$rzwp3k_priority_file)
      
      is_running(TRUE)
      analysis_result(NULL)
      
      tryCatch({
        matriks_serasi <- load_validate_matrix_table(input$matrix_file$datapath, title = "serasi")
        
        idx_padan_map    <- sf::st_read(input$idx_padan_file$datapath, quiet = TRUE)
        rtrw_prioritas   <- load_and_validate_table(input$rtrw_priority_file$datapath)
        rzwp3k_prioritas <- load_and_validate_table(input$rzwp3k_priority_file$datapath)
        
        alpha    <- input$alpha_val
        t_serasi <- input$threshold_serasi
        t_padu   <- input$threshold_padu

        idx_padan_map_alt <- idx_padan_map %>%
          rowwise() %>%
          mutate(
            alt_RTRW = as.character(get_alternative_zone(RZWP3K, RTRW, "RTRW", matriks_serasi)),
            alt_RZWP3K = as.character(get_alternative_zone(RTRW, RZWP3K, "RZWP3K", matriks_serasi)),
            idx_serasi_rtrw_alt = get_alternative_serasi(RZWP3K, alt_RTRW, matriks_serasi),
            idx_serasi_rzwp3k_alt = get_alternative_serasi(RTRW, alt_RZWP3K, matriks_serasi)
          ) %>%
          ungroup()
        
        idx_padan_map_alt <- idx_padan_map_alt %>%
          mutate(
            idx_padan_rtrw_alt = (alpha * idx_serasi_rtrw_alt) + ((1 - alpha) * idx_padu_final),
            idx_padan_rzwp3k_alt = (alpha * idx_serasi_rzwp3k_alt) + ((1 - alpha) * idx_padu_final)
          )

        priority_rtrw   <- as.character(rtrw_prioritas[[1]][rtrw_prioritas$Prioritas == 1])
        priority_rzwp3k <- as.character(rzwp3k_prioritas[[1]][rzwp3k_prioritas$Prioritas == 1])
        
        idx_padan_map_alt <- idx_padan_map_alt %>%
          mutate(
            recommendation = case_when(
              is.na(RTRW) | is.na(RZWP3K) | is.na(idx_serasi) | is.na(idx_padu_final) ~ NA_character_,
              as.character(RTRW) %in% priority_rtrw ~ "Tetap (Prioritas RTRW)",
              as.character(RZWP3K) %in% priority_rzwp3k ~ "Ubah_RTRW",
              idx_serasi >= t_serasi ~ "Koordinasi",
              idx_padu_final >= t_padu ~ "Ubah_RZWP3K",
              TRUE ~ "Ubah_RTRW"
            )
          )

        idx_padan_map_alt <- idx_padan_map_alt %>%
          mutate(
            alt_RTRW = as.character(ifelse(is.na(alt_RTRW), "", alt_RTRW)),
            alt_RZWP3K = as.character(ifelse(is.na(alt_RZWP3K), "", alt_RZWP3K))
          ) %>%
          mutate(
            decision = case_when(
              is.na(recommendation) ~ NA_character_,
              
              recommendation == "Ubah_RZWP3K" & 
                !is.na(idx_padan_rzwp3k_alt) & 
                idx_padan_rzwp3k_alt > idx_padan ~ 
                paste("Ubah RZWP3K ke", alt_RZWP3K),
              
              recommendation == "Ubah_RTRW" & 
                !is.na(idx_padan_rtrw_alt) & 
                idx_padan_rtrw_alt > idx_padan ~ 
                paste("Ubah RTRW ke", alt_RTRW),
              
              TRUE ~ "Tetap/Koordinasi"
            ),
            
            idx_padan_final_rec = case_when(
              grepl("Ubah RTRW", decision)   ~ idx_padan_rtrw_alt,
              grepl("Ubah RZWP3K", decision) ~ idx_padan_rzwp3k_alt,
              TRUE                          ~ idx_padan
            )
          )

        out_gpkg <- file.path(output_dir(), "idx_padan_recommendation.gpkg")
        out_xlsx <- file.path(output_dir(), "idx_padan_recommendation.xlsx")
        
        sf::st_write(idx_padan_map_alt, out_gpkg, delete_dsn = TRUE, quiet = TRUE)
        openxlsx::write.xlsx(sf::st_drop_geometry(idx_padan_map_alt), out_xlsx)

        analysis_result(list(map = idx_padan_map_alt, table = sf::st_drop_geometry(idx_padan_map_alt)))
        analysis_log("Analisis rekomendasi selesai.")
        showNotification("Berhasil! File rekomendasi telah disimpan.", type = "message")
        
      }, error = function(e) {
        analysis_log(paste("Error:", e$message))
        showNotification(paste("Gagal:", e$message), type = "error", duration = 10)
      })
      
      is_running(FALSE)
    })
    
    output$status_box <- renderUI({
      if (is_running()) {
        div(class = "alert alert-info mb-0", tags$i(class = "bi bi-hourglass-split me-2"), "Memproses...")
      } else if (!is.null(analysis_result())) {
        div(class = "alert alert-success mb-0", tags$i(class = "bi bi-check-circle me-2"), "Selesai.")
      } else {
        div(class = "alert alert-secondary mb-0", "Siap dijalankan.")
      }
    })
    
    output$result_table <- renderTable({
      req(analysis_result())
      cols <- c("id_pu", "RTRW", "RZWP3K", "recommendation", "decision", "idx_padan_final_rec")
      head(analysis_result()$table[, intersect(cols, names(analysis_result()$table))], 100)
    })
    
    output$validation_log <- renderText({ analysis_log() })
  })
}