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

# 4. Core Analytical Engine
.source_laspur("R/engine/01_spatial_primitives.R")
.source_laspur("R/engine/02_analysis_overlap.R")
.source_laspur("R/engine/03_analysis_adjacent.R")
.source_laspur("R/engine/04_analysis_interdependent.R")
.source_laspur("R/engine/05_matrix_serasi.R")
.source_laspur("R/engine/06_economic_valuation.R")
.source_laspur("R/engine/07_recommendation.R")
.source_laspur("R/engine/08_export_excel.R")

# 5. UI Presentation Components
.source_laspur("R/shared_inputs.R")
.source_laspur("R/visualisation.R")

# 6. Legacy Bridge (Active during incremental migration until Ticket 05)
if (file.exists(file.path(.laspur_root, "R/helpers.R"))) {
  .source_laspur("R/helpers.R")
}
if (file.exists(file.path(.laspur_root, "R/functions.R"))) {
  .source_laspur("R/functions.R")
}
