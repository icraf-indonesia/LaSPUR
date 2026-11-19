# R/init.R
# ============================================================
# LaSPUR Master Initialization & Deterministic Dependency Loader
# ============================================================

# 1. Package Dependencies
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  terra, sf, openxlsx, tibble, dplyr, landscapemetrics, purrr, tidyr,
  stringr, exactextractr, here, rmarkdown, kableExtra, DT, units, utils,
  furrr, future, data.table, leaflet, leaflet.extras, htmltools, readxl,
  shiny, shinyjs, shinyFiles, promises, bslib, base64enc, tidyterra,
  slickR, igraph, rhandsontable, visNetwork, reactable
)

# 2. Global Options & Environment Config
options(shiny.maxRequestSize = 2000 * 1024^2)

# Resolve project root safely
.laspur_root <- tryCatch({
  here::here()
}, error = function(e) {
  getwd()
})

# Safe source helper relative to project root
.source_laspur <- function(rel_path) {
  full_path <- file.path(.laspur_root, rel_path)
  if (file.exists(full_path)) {
    source(full_path, local = FALSE)
  }
}

# 3. Application Services
.source_laspur("R/services/storage_service.R")
.source_laspur("R/services/io_validators.R")
.source_laspur("R/services/formatters.R")
.source_laspur("R/services/map_renderers.R")
.source_laspur("R/services/report_generator.R")

# 4. Core Analytical Engine
.source_laspur("R/engine/01_spatial_primitives.R")
.source_laspur("R/engine/02_analysis_overlap.R")
.source_laspur("R/engine/03_analysis_adjacent.R")
.source_laspur("R/engine/04_analysis_interdependent.R")
.source_laspur("R/engine/05_matrix_serasi.R")
.source_laspur("R/engine/indices/00_padu_common.R")
.source_laspur("R/engine/indices/padu_ke.R")
.source_laspur("R/engine/indices/padu_hs.R")
.source_laspur("R/engine/indices/padu_kl.R")
.source_laspur("R/engine/indices/padu_kh.R")
.source_laspur("R/engine/indices/padu_rtp.R")
.source_laspur("R/engine/indices/padu_se.R")
.source_laspur("R/engine/indices/padu_ki.R")
.source_laspur("R/engine/indices/padu_combine.R")
.source_laspur("R/engine/06_economic_valuation.R")
.source_laspur("R/engine/07_recommendation.R")
.source_laspur("R/engine/08_export_excel.R")

# 5. UI Presentation Components
.source_laspur("R/ui_components/result_views.R")
.source_laspur("R/ui_components/report_modal.R")
.source_laspur("R/ui_components/ui_shell.R")
.source_laspur("R/shared_inputs.R")
.source_laspur("R/visualisation.R")

# 6. Shiny Analysis Modules
safe_source <- function(file, ui_fn_name, srv_fn_name) {
  full_path <- file.path(.laspur_root, file)
  if (file.exists(full_path)) {
    source(full_path, local = FALSE)
  } else {
    assign(ui_fn_name, function(id) {
      tagList(
        div(
          style = paste(
            "display: flex; flex-direction: column;",
            "align-items: center; justify-content: center;",
            "padding: 60px 20px; color: #64748b; text-align: center;"
          ),
          icon("file-circle-xmark", class = "mb-3", style = "font-size: 3.5rem; color: #cbd5e1;"),
          tags$p(style = "font-size: 1.1rem; margin: 0; font-weight: 600; color: #475569;",
                 "Modul tidak tersedia"),
          tags$p(style = "font-size: 0.9rem; margin: 4px 0 0 0;",
                 paste0("File tidak ditemukan: ", file))
        )
      )
    }, envir = .GlobalEnv)
    
    assign(srv_fn_name, function(id, output_dir, module_id = NULL) {
      moduleServer(id, function(input, output, session) {})
    }, envir = .GlobalEnv)
  }
}

safe_source("modules/mod_overlap.R",         "overlap_ui",         "overlap_server")
safe_source("modules/mod_adjacent.R",        "adjacent_ui",        "adjacent_server")
safe_source("modules/mod_interconnection.R", "interconnection_ui", "interconnection_server")
safe_source("modules/mod_padu_ke.R",         "padu_ke_ui",         "padu_ke_server")
safe_source("modules/mod_padu_hs.R",         "padu_hs_ui",         "padu_hs_server")
safe_source("modules/mod_padu_kl.R",         "padu_kl_ui",         "padu_kl_server")
safe_source("modules/mod_padu_kh.R",         "padu_kh_ui",         "padu_kh_server")
safe_source("modules/mod_padu_rtp.R",        "padu_rtp_ui",        "padu_rtp_server")
safe_source("modules/mod_padu_se.R",         "padu_se_ui",         "padu_se_server")
safe_source("modules/mod_padu_ki.R",         "padu_ki_ui",         "padu_ki_server")
safe_source("modules/mod_padu_combine.R",    "padu_combine_ui",    "padu_combine_server")
safe_source("modules/mod_padan.R",           "padan_ui",           "padan_server")
safe_source("modules/mod_recommendation_overlaps.R",  "recommendation_overlaps_ui",  "recommendation_overlaps_server")
safe_source("modules/mod_recommendation_adjacent.R",  "recommendation_adjacent_ui",  "recommendation_adjacent_server")
safe_source("modules/mod_reconcile.R",       "reconcile_ui",       "reconcile_server")

# 7. Application State & Orchestration
.source_laspur("R/state/workflow_coordinator.R")
