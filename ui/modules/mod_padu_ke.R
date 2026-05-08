# ui/modules/mod_padu_ke.R
# ============================================================
#  MODULE: PADU-KE (2.1 PADU-KE)
# ============================================================

source("../R/functions.R")
source("../R/helpers.R")

# ── UI ─────────────────────────────────────────────────────
padu_ke_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    div(
      style = "margin-bottom: 20px;",
      h4("2.1 PADU-KE: Land Cover Adjacency Analysis", style = "margin: 0; font-weight: 700;"),
      tags$p(
        "Calculate PADU-KE index based on land cover adjacency and compatibility matrix.",
        style = "color: #6c757d; margin: 4px 0 0 0; font-size: 0.9rem;"
      )
    ),
    
    layout_column_wrap(
      width = 1/2,
      
      # ── Card A: Input & Parameters ──────────────────────────
      card(
        card_header("Input & Parameters"),
        
        fileInput(ns("lulc_file"), "Land Cover Shapefile",
                  accept = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                  multiple = TRUE),
        tags$div(
          class = "form-text text-muted",
          style = "margin-top: -8px; margin-bottom: 12px; font-size: 0.8rem;",
          "Polygon layer with LULC classes (e.g., PL2024_Coral_Seagrass_Union.shp)"
        ),
        
        fileInput(ns("idx_serasi_file"), "SERASI Result Map (.gpkg or .shp)",
                  accept = c(".gpkg", ".shp", ".dbf", ".prj", ".shx"),
                  multiple = TRUE),
        tags$div(
          class = "form-text text-muted",
          style = "margin-top: -8px; margin-bottom: 12px; font-size: 0.8rem;",
          "Output from Overlap module (idx_serasi.gpkg)"
        ),
        
        hr(),

        fileInput(ns("matriks_padu_ke_file"), "PADU-KE Matrix Table (.xlsx)",
                  accept = ".xlsx"),
        tags$div(
          class = "form-text text-muted",
          style = "margin-top: -8px; margin-bottom: 12px; font-size: 0.8rem;",
          "Columns: class1, class2, adj_index"
        ),
        
        hr(),
        
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton(ns("btn_generate_matrix"),
                       tagList(tags$i(class = "bi bi-file-earmark-excel me-1"),
                               "Generate Matrix Template"),
                       class = "btn-outline-primary btn-sm"),
          actionButton(ns("btn_run"),
                       tagList(tags$i(class = "bi bi-play-fill me-1"),
                               "Run Analysis"),
                       class = "btn-success btn-sm")
        )
      ),
      
      # ── Card B: Output & Results ────────────────────────────
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

# ── Server ─────────────────────────────────────────────────
padu_ke_server <- function(id, output_dir) {
  moduleServer(id, function(input, output, session) {
    
    analysis_result <- reactiveVal(NULL)
    analysis_log    <- reactiveVal("No analysis run yet.")
    is_running      <- reactiveVal(FALSE)
    
    # ── Helper: extract shapefile path from uploaded components ──
    extract_shp_path <- function(file_input) {
      shp_row <- file_input[grepl("\\.shp$", file_input$name, ignore.case = TRUE), ]
      validate(need(
        nrow(shp_row) == 1,
        "Please upload all shapefile components (.shp, .dbf, .prj, .shx)"
      ))
      stem <- tools::file_path_sans_ext(shp_row$datapath)
      for (i in seq_len(nrow(file_input))) {
        ext <- tools::file_ext(file_input$name[i])
        file.rename(file_input$datapath[i], paste0(stem, ".", ext))
      }
      paste0(stem, ".shp")
    }
    
    # Helper for gpkg (single file)
    extract_gpkg_path <- function(file_input) {
      req(file_input)
      file_input$datapath[1]
    }
    
    # ── Reactive: load LULC shapefile ─────────────────────────
    lulc_vect <- reactive({
      req(input$lulc_file)
      shp_path <- extract_shp_path(input$lulc_file)
      load_and_validate_shapefile(shp_path)
    })
    
    lulc_ref <- reactive({
      lulc_vect() %>% 
        sf::st_drop_geometry() %>% 
        dplyr::distinct(ID, LC) %>% 
        dplyr::arrange(ID)
    })
    
    # ── Reactive: load idx_serasi_map (GPKG or shapefile) ─────
    idx_serasi_map <- reactive({
      req(input$idx_serasi_file)
      path <- input$idx_serasi_file$datapath[1]
      ext <- tolower(tools::file_ext(path))
      if (ext == "gpkg") {
        sf::st_read(path, quiet = TRUE)
      } else if (ext == "shp") {
        shp_path <- extract_shp_path(input$idx_serasi_file)
        sf::st_read(shp_path, quiet = TRUE)
      } else {
        validate("Unsupported file type for SERASI result. Please upload .gpkg or .shp")
      }
    })
    
    # ── Generate matrix template ──────────────────────────────
    observeEvent(input$btn_generate_matrix, {
      tryCatch({
        template <- generate_matrix_padu_ke(lulc_ref())
        out_path <- file.path(output_dir(), "matriks_padu_ke_template.xlsx")
        write.xlsx(template, out_path, overwrite = TRUE)
        showNotification(paste("Matrix template generated →", out_path),
                         type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Error generating matrix:", e$message),
                         type = "error", duration = 8)
      })
    })
    
    # ── Run full analysis ─────────────────────────────────────
    observeEvent(input$btn_run, {
      req(!is_running())
      req(input$lulc_file, 
          input$idx_serasi_file,
          input$matriks_padu_ke_file)
      
      is_running(TRUE)
      analysis_result(NULL)
      
      tryCatch({
        # Load PADU-KE matrix table
        matriks_raw <- load_validate_matrix_table(input$matriks_padu_ke_file$datapath, title = "padu_ke")
        idx_col <- setdiff(names(matriks_raw), c("class1", "class2"))
        names(matriks_raw)[names(matriks_raw) == idx_col] <- "adj_index"
        
        matriks_padu_ke_id <- matriks_raw %>%
          mutate(
            class1_id = lulc_ref()[[1]][match(class1, lulc_ref()[[2]])],
            class2_id = lulc_ref()[[1]][match(class2, lulc_ref()[[2]])]
          ) %>%
          filter(!is.na(class1_id), !is.na(class2_id)) %>%
          select(
            class_id1 = class1_id,
            class_id2 = class2_id,
            adj_index
          )
        
        # Load SERASI map & LULC map
        idx_map <- idx_serasi_map()
        lulc_vect_data <- lulc_vect()
        class_col <- intersect(c("ID", "Class", "class", "LULC", "Kelas"), names(lulc_vect_data))[1]
        
        # Calculate adjacencies
        lulc_adjacencies <- calculate_lulc_adjacency(
          lulc       = lulc_vect_data,
          admin_vector = idx_map,
          id_col     = "id_pu",
          class_col  = class_col
        )

        lulc_adjacencies <- lulc_adjacencies %>%
          mutate(
            Class_A = as.integer(as.character(Class_A)),
            Class_B = as.integer(as.character(Class_B))
          )
        
        # Calculate PADU-KE index
        idx_padu_ke <- calculate_padu_ke(lulc_adjacencies, matriks_padu_ke_id) %>%
          select(id_pu, idx_padu_ke)
        
        # Join back to SERASI map
        idx_padu_ke_map <- idx_map %>%
          mutate(id_pu = as.character(id_pu)) %>%
          left_join(idx_padu_ke, by = "id_pu") %>%
          mutate(idx_padu_ke = ifelse(is.na(idx_padu_ke), 0, idx_padu_ke))
        
        # Save result
        out_gpkg <- file.path(output_dir(), "idx_padu_ke.gpkg")
        sf::st_write(idx_padu_ke_map, out_gpkg, delete_dsn = TRUE, quiet = TRUE)
        message("Saved result to: ", out_gpkg)
        
        result_table <- as_tibble(sf::st_drop_geometry(idx_padu_ke_map))
        analysis_result(list(map = idx_padu_ke_map, table = result_table))
        analysis_log("✅ PADU-KE analysis completed successfully.")
        showNotification("Analysis complete! Check the Map and Table tabs.", type = "message", duration = 5)
        
      }, error = function(e) {
        msg <- conditionMessage(e)
        if (is.null(msg) || msg == "") msg <- "Unknown error (see console for details)"
        message("\n!!! ERROR in PADU-KE analysis: ", msg)
        if (!is.null(e$call)) message("   Call: ", deparse(e$call))
        analysis_log(paste("❌", msg))
        showNotification(paste("Analysis failed:", msg), type = "error", duration = 10)
      })
      
      is_running(FALSE)
    })
    
    # ── Status box ────────────────────────────────────────────
    output$status_box <- renderUI({
      if (is_running()) {
        div(class = "alert alert-info mb-0",
            tags$i(class = "bi bi-hourglass-split me-2"),
            "Running analysis...")
      } else if (!is.null(analysis_result())) {
        div(class = "alert alert-success mb-0",
            tags$i(class = "bi bi-check-circle me-2"),
            "Analysis complete.")
      } else {
        div(class = "alert alert-secondary mb-0",
            tags$i(class = "bi bi-circle me-2"),
            "Ready. Upload files and click Run.")
      }
    })
    
    # ── Map output ────────────────────────────────────────────
    output$result_map <- renderPlot({
      req(analysis_result())
      plot(analysis_result()$map["idx_padu_ke"], 
           main = "PADU-KE Index Map")
    })
    
    # ── Table output ──────────────────────────────────────────
    output$result_table <- renderTable({
      req(analysis_result())
      analysis_result()$table
    })
    
    # ── Validation log ────────────────────────────────────────
    output$validation_log <- renderText({
      analysis_log()
    })
    
  })
}