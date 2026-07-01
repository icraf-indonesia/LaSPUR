# ui/modules/mod_rekonsiliasi.R
# ============================================================
#  MODULE: Rekonsiliasi (Tahap 8)
# ============================================================

source("R/functions.R")
source("R/helpers.R")

# ── UI ───────────────────────────────────────────────────────
rekonsiliasi_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    div(
      style = "margin-bottom: 20px;",
      h4("Rekonsiliasi Kasus Integrasi Peta RTRW & RZWP3K", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Rekonsiliasi melalui negosiasi dan pengambilan keputusan oleh pengguna berdasarkan pertimbangan analisis SERASI, PADU, dan PADAN.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),
    
    layout_column_wrap(
      width = 1/2,
      
      # ── Card A: Input & Parameter ────────────────────────
      card(
        card_header("Input & Parameter"),
        
        tags$p(tags$i(class = "bi bi-map me-1"),
               "Peta Indeks Padan & Rekomendasi",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Unggah semua komponen shapefile indeks PADAN (.shp, .dbf, .prj, .shx)."
        ),
        
        fileInput(ns("idx_padan_alt"),
                  label    = NULL,
                  accept   = c(".gpkg", ".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        
        tags$p(tags$i(class = "bi bi-map me-1"),
               "Peta RTRW (.shp)",
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
               "Peta RZWP3K (.shp)",
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
          actionButton(ns("btn_generate_template"),
                       tagList(tags$i(class = "bi bi-file-earmark-excel me-1"),
                               "Buat Template Rekonsiliasi"),
                       class = "btn-outline-primary btn-sm")
        ),
        
        hr(),
        
        tags$p(tags$i(class = "bi bi-upload me-1"),
               "Unggah Tabel Keputusan",
               style = "font-weight: 600; margin-bottom: 4px;"),
        tags$small(
          style = "color: #6c757d; display: block; margin-bottom: 8px;",
          "Setelah mengisi file Excel dari langkah sebelumnya, unggah di sini."
        ),
        fileInput(ns("recon_table_file"),
                  label  = NULL,
                  accept = ".xlsx"),
        
        hr(),
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_run"),
                       tagList(tags$i(class = "bi bi-play-fill me-1"),
                               "Jalankan Rekonsiliasi"),
                       class = "btn-success btn-sm")
        ),
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
            div(
              style = "max-height: 300px; overflow-y: auto; background-color: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 0.9rem; white-space: pre-wrap;",
              verbatimTextOutput(ns("validation_log"))
            )
          )
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────
rekonsiliasi_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    
    analysis_result <- reactiveVal(NULL)
    log_messages    <- reactiveVal("")   # log real-time
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
    
    # ── Log helper ───────────────────────────────────────────
    append_log <- function(msg) {
      current <- log_messages()
      log_messages(paste0(current, format(Sys.time(), "[%H:%M:%S] "), msg, "\n"))
    }
    
    # ── Reactives ────────────────────────────────────────────
    idx_padan_alt_vect <- reactive({
      req(input$idx_padan_alt)
      path <- extract_vector_path(input$idx_padan_alt)
      load_and_validate_shapefile(path)
    })
    
    rtrw_vect <- reactive({
      req(input$rtrw_file)
      load_and_validate_shapefile(extract_shp_path(input$rtrw_file))
    })
    
    rzwp3k_vect <- reactive({
      req(input$rzwp3k_file)
      load_and_validate_shapefile(extract_shp_path(input$rzwp3k_file))
    })
    
    rtrw_prioritas <- reactive({
      req(input$rtrw_prioritas_file)
      load_and_validate_table(input$rtrw_prioritas_file$datapath)
    })
    
    rzwp3k_prioritas <- reactive({
      req(input$rzwp3k_prioritas_file)
      load_and_validate_table(input$rzwp3k_prioritas_file$datapath)
    })
    
    # ── Buat union overlap (digunakan di dua tempat) ────────
    union_overlap <- reactive({
      req(rtrw_vect(), rzwp3k_vect())
      rtrw <- rtrw_vect() %>% dplyr::mutate(id_rtrw = dplyr::row_number())
      rzwp3k <- rzwp3k_vect() %>% dplyr::mutate(id_rzwp3k = dplyr::row_number())
      identify_overlaps_union(rtrw, rzwp3k)
    })
    
    # ── Generate template rekonsiliasi ──────────────────────
    observeEvent(input$btn_generate_template, {
      req(union_overlap(), rtrw_prioritas(), rzwp3k_prioritas())
      
      append_log(">>> Membuat template rekonsiliasi...")
      tryCatch({
        # Gunakan union_overlap sebagai recon_map
        recon_map <- union_overlap()
        
        # Generate Excel
        generate_reconciliation_excel(
          recon_map          = idx_padan_alt_vect(),
          rtrw_prioritas     = rtrw_prioritas(),
          rzwp3k_prioritas   = rzwp3k_prioritas(),
          output_dir         = output_dir(),
          step               = 1,
          file_name          = "overlaps_reconcilliation_table.xlsx"
        )
        
        out_path <- file.path(output_dir(), "overlaps_reconcilliation_table.xlsx")
        append_log(paste("Template rekonsiliasi berhasil dibuat →", out_path))
        showNotification(paste("Template dibuat:", out_path),
                         type = "message", duration = 5)
        
      }, error = function(e) {
        msg <- conditionMessage(e)
        append_log(paste("Gagal membuat template:", msg))
        showNotification(paste("Gagal membuat template:", msg), type = "error", duration = 8)
      })
    })
    
    # ── Jalankan rekonsiliasi ────────────────────────────────
    observeEvent(input$btn_run, {
      req(!is_running())
      req(union_overlap(), rtrw_prioritas(), rzwp3k_prioritas(), input$recon_table_file)
      
      is_running(TRUE)
      analysis_result(NULL)
      log_messages("")   # reset log
      
      withProgress(message = "Menjalankan Rekonsiliasi", value = 0, {
        
        tryCatch({
          # Step 1: Load data (10%)
          incProgress(0.1, detail = "Memuat data...")
          append_log("Memulai proses rekonsiliasi...")
          union <- union_overlap()
          rtrw_prior <- rtrw_prioritas()
          rzwp3k_prior <- rzwp3k_prioritas()
          append_log("Data utama berhasil dimuat.")
          
          # Step 2: Baca tabel rekonsiliasi (20%)
          incProgress(0.1, detail = "Membaca tabel rekonsiliasi...")
          append_log("Membaca tabel rekonsiliasi yang diunggah...")
          recon_table <- load_and_validate_table(input$recon_table_file$datapath)
          append_log("Tabel rekonsiliasi berhasil dimuat.")
          
          # Step 3: Filter overlaps by threshold (opsional) - 30%
          incProgress(0.1, detail = "Menyaring berdasarkan luas minimum...")
          if (input$threshold_ha > 0) {
            append_log(paste("Menyaring poligon dengan luas ≥", input$threshold_ha, "ha..."))
            union <- filter_overlaps(union, input$threshold_ha)
            append_log(paste("  Sisa", nrow(union), "poligon setelah penyaringan."))
          }
          
          # Step 4: Reconcile (60%)
          incProgress(0.3, detail = "Melakukan rekonsiliasi...")
          append_log("Menjalankan reconcile_map_overlap...")
          final_map <- reconcile_map_overlap(
            union = union,
            recon_table = recon_table,
            rtrw_prioritas = rtrw_prior,
            rzwp3k_prioritas = rzwp3k_prior
          )
          append_log("Rekonsiliasi selesai.")
          
          # Step 5: Pisahkan hasil (80%)
          incProgress(0.2, detail = "Memisahkan hasil RTRW dan RZWP3K...")
          rtrw_final <- final_map[final_map$stat_pu_final == "RTRW", ]
          rzwp3k_final <- final_map[final_map$stat_pu_final == "RZWP3K", ]
          append_log(paste("  Jumlah poligon RTRW:", nrow(rtrw_final)))
          append_log(paste("  Jumlah poligon RZWP3K:", nrow(rzwp3k_final)))
          
          # Step 6: Simpan (95%)
          incProgress(0.15, detail = "Menyimpan hasil...")
          out_rtrw <- file.path(output_dir(), "ST_Union_Resolved_Overlaps.gpkg")
          out_rzwp3k <- file.path(output_dir(), "ST_RZWP3K_Resolved_Overlaps.gpkg")
          sf::st_write(rtrw_final, out_rtrw, delete_dsn = TRUE, quiet = TRUE)
          sf::st_write(rzwp3k_final, out_rzwp3k, delete_dsn = TRUE, quiet = TRUE)
          append_log(paste("  RTRW disimpan →", out_rtrw))
          append_log(paste("  RZWP3K disimpan →", out_rzwp3k))
          
          # Gabungkan untuk tampilan peta (opsional)
          final_map_combined <- rbind(rtrw_final, rzwp3k_final)
          analysis_result(list(
            map = final_map_combined,
            table = sf::st_drop_geometry(final_map_combined)
          ))
          
          append_log("Rekonsiliasi berhasil diselesaikan.")
          
          incProgress(0.05, detail = "Selesai!")
          showNotification("Rekonsiliasi selesai! Hasil disimpan ke direktori output.",
                           type = "message", duration = 5)
          
        }, error = function(e) {
          msg <- conditionMessage(e)
          if (is.null(msg) || msg == "") msg <- "Error tidak diketahui (lihat konsol untuk detail)"
          append_log(paste("ERROR:", msg))
          showNotification(paste("Rekonsiliasi gagal:", msg), type = "error", duration = 10)
        })
        
      }) # end withProgress
      
      is_running(FALSE)
    })
    
    # ── Status box ───────────────────────────────────────────
    output$status_box <- renderUI({
      if (is_running()) {
        div(class = "alert alert-info mb-0",
            tags$i(class = "bi bi-hourglass-split me-2"),
            "Menjalankan rekonsiliasi...")
      } else if (!is.null(analysis_result())) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Rekonsiliasi selesai.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Siap. Unggah file dan buat template, lalu jalankan rekonsiliasi.")
      }
    })
    
    # ── Map output ───────────────────────────────────────────
    output$result_map <- renderPlot({
      req(analysis_result())
      plot(analysis_result()$map["stat_pu_final"], 
           main = "Peta Hasil Rekonsiliasi", 
           border = "grey60")
    })
    
    # ── Table output ─────────────────────────────────────────
    output$result_table <- renderTable({
      req(analysis_result())
      head(analysis_result()$table, 100)
    })
    
    # ── Validation log (real-time) ──────────────────────────
    output$validation_log <- renderPrint({
      invalidateLater(100, session)   # perbarui setiap 100ms
      cat(log_messages())
    })
    
  })
}