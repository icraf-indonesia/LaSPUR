# app.R
# ============================================================
# LaSPUR – Land and Seascape Planning Unit Reconciliation
# ============================================================

source("R/init.R")

future::plan(future::multisession)
options(shiny.maxRequestSize = 2000 * 1024^2)

ui <- create_ui_shell()

server <- function(input, output, session) {
  setup_workflow_coordinator(input, output, session)
}

shinyApp(ui, server)
