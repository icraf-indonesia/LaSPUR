# ui/app.R
# ============================================================

library(shiny)
library(bslib)
library(future)
library(promises)
library(shinyFiles)

plan(multisession)

options(shiny.maxRequestSize = 500 * 1024^2)

# ── safe_source ──────────────────────────────────────────────
safe_source <- function(file, ui_fn_name, srv_fn_name) {
  if (file.exists(file)) {
    source(file)
  } else {
    assign(ui_fn_name, function(id) {
      tagList(
        div(
          style = paste(
            "display: flex; flex-direction: column;",
            "align-items: center; justify-content: center;",
            "padding: 60px 20px; color: #adb5bd; text-align: center;"
          ),
          tags$i(class = "bi bi-file-earmark-x",
                 style = "font-size: 3rem; margin-bottom: 12px;"),
          tags$p(style = "font-size: 1rem; margin: 0; font-weight: 600;",
                 "Module not available"),
          tags$p(style = "font-size: 0.8rem; margin: 4px 0 0 0;",
                 paste0("Missing file: ", file))
        )
      )
    }, envir = .GlobalEnv)
    
    assign(srv_fn_name, function(id, output_dir) {
      moduleServer(id, function(input, output, session) {})
    }, envir = .GlobalEnv)
  }
}

# ── Source all modules ───────────────────────────────────────
safe_source("modules/mod_overlap.R",        "overlap_ui",         "overlap_server")
safe_source("modules/mod_adjacent.R",        "adjacent_ui",        "adjacent_server")
safe_source("modules/mod_interconnection.R", "interconnection_ui", "interconnection_server")
safe_source("modules/mod_padu_ke.R",         "padu_ke_ui",         "padu_ke_server")
safe_source("modules/mod_padu_hs.R",         "padu_hs_ui",         "padu_hs_server")
safe_source("modules/mod_padu_kl.R",         "padu_kl_ui",         "padu_kl_server")
safe_source("modules/mod_padu_kh.R",         "padu_kh_ui",         "padu_kh_server")
safe_source("modules/mod_padu_rtp.R",        "padu_rtp_ui",        "padu_rtp_server")
safe_source("modules/mod_padu_se.R",         "padu_se_ui",         "padu_se_server")
safe_source("modules/mod_padu_ki.R",         "padu_ki_ui",         "padu_ki_server")
safe_source("modules/mod_padan.R",           "padan_ui",           "padan_server")
safe_source("modules/mod_recommendation.R",  "recommendation_ui",  "recommendation_server")

# ── Sidebar nav helper ───────────────────────────────────────
nav_item <- function(input_id, number, label) {
  div(
    style = paste(
      "display: flex; align-items: center; gap: 8px;",
      "padding: 6px 10px; border-radius: 6px; cursor: pointer;",
      "transition: background 0.15s ease;",
      "margin-bottom: 2px;"
    ),
    onmouseover = "this.style.background='rgba(0,0,0,0.07)'",
    onmouseout  = "this.style.background='transparent'",
    onclick     = sprintf("Shiny.setInputValue('%s', Math.random())", input_id),
    tags$span(
      style = paste(
        "font-size: 0.7rem; font-weight: 700; color: #fff;",
        "background: #18bc9c; border-radius: 4px;",
        "padding: 1px 6px; min-width: 28px; text-align: center;",
        "flex-shrink: 0;"
      ),
      number
    ),
    tags$span(
      style = "font-size: 0.875rem; color: #2c3e50; line-height: 1.3;",
      label
    )
  )
}

# ── Tab config ───────────────────────────────────────────────
tab_config <- list(
  overlap         = list(label = "1.1 Overlap",         ui_fn = overlap_ui,         srv_fn = overlap_server),
  adjacent        = list(label = "1.2 Adjacent",        ui_fn = adjacent_ui,        srv_fn = adjacent_server),
  interconnection = list(label = "1.3 Interconnection", ui_fn = interconnection_ui, srv_fn = interconnection_server),
  padu_ke         = list(label = "2.1 PADU-KE",         ui_fn = padu_ke_ui,         srv_fn = padu_ke_server),
  padu_hs         = list(label = "2.2 PADU-HS",         ui_fn = padu_hs_ui,         srv_fn = padu_hs_server),
  padu_kl         = list(label = "2.3 PADU-KL",         ui_fn = padu_kl_ui,         srv_fn = padu_kl_server),
  padu_kh         = list(label = "2.4 PADU-KH",         ui_fn = padu_kh_ui,         srv_fn = padu_kh_server),
  padu_rtp        = list(label = "2.5 PADU-RTp",        ui_fn = padu_rtp_ui,        srv_fn = padu_rtp_server),
  padu_se         = list(label = "2.6 PADU-SE",         ui_fn = padu_se_ui,         srv_fn = padu_se_server),
  padu_ki         = list(label = "2.7 PADU-KI",         ui_fn = padu_ki_ui,         srv_fn = padu_ki_server),
  padan           = list(label = "3. PADAN",            ui_fn = padan_ui,           srv_fn = padan_server),
  recommendation  = list(label = "4. Recommendation",   ui_fn = recommendation_ui,  srv_fn = recommendation_server)
)

# ── UI ───────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Land & Sea Planning Unit Reconcilliation (LaSPUR)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Jelajahi Modul",
    
    # ── Output Directory ────────────────────────────────────
    div(
      style = "margin-bottom: 16px;",
      tags$label("Output Directory",
                 style = paste("font-size: 0.85rem; font-weight: 600;",
                               "margin-bottom: 6px; display: block;")),
      shinyDirButton(
        id    = "btn_browse_output",
        label = "Browse Folder",
        title = "Select Output Directory",
        icon  = icon("folder-open"),
        style = "width: 100%;"
      ),
      div(style = "margin-top: 6px;",
          uiOutput("output_dir_status"))
    ),
    
    hr(),
    
    accordion(
      open = FALSE,
      
      accordion_panel(
        "1. Identify Spatial Conflict",
        nav_item("nav_overlap",         "1.1", "Type 1: Overlap"),
        nav_item("nav_adjacent",        "1.2", "Type 2: Adjacent"),
        nav_item("nav_interconnection", "1.3", "Type 3: Interconnection")
      ),
      accordion_panel(
        "2. PADU Analysis",
        nav_item("nav_padu_ke",  "2.1", "PADU-KE"),
        nav_item("nav_padu_hs",  "2.2", "PADU-HS"),
        nav_item("nav_padu_kl",  "2.3", "PADU-KL"),
        nav_item("nav_padu_kh",  "2.4", "PADU-KH"),
        nav_item("nav_padu_rtp", "2.5", "PADU-RTp"),
        nav_item("nav_padu_se",  "2.6", "PADU-SE"),
        nav_item("nav_padu_ki",  "2.7", "PADU-KI")
      ),
      accordion_panel(
        "3. PADAN Analysis",
        nav_item("nav_padan", "3", "PADAN Analysis")
      ),
      accordion_panel(
        "4. Recommendation",
        nav_item("nav_recommendation", "4", "Recommendation")
      )
    )
  ),
  
  # ── Confirmation modal ──────────────────────────────────────
  tags$div(
    id = "close_confirm_modal", class = "modal fade",
    tabindex = "-1", `data-bs-backdrop` = "static", `data-bs-keyboard` = "false",
    tags$div(class = "modal-dialog modal-dialog-centered",
             tags$div(class = "modal-content",
                      tags$div(class = "modal-header bg-danger text-white",
                               tags$h5(class = "modal-title",
                                       tags$i(class = "bi bi-exclamation-triangle-fill me-2"), "Close Tab"),
                               tags$button(type = "button", class = "btn-close btn-close-white",
                                           `data-bs-dismiss` = "modal")
                      ),
                      tags$div(class = "modal-body",
                               tags$p(class = "mb-0",
                                      "Are you sure you want to close ",
                                      tags$strong(id = "modal_tab_label", "this tab"),
                                      "? Any unsaved changes will be lost.")
                      ),
                      tags$div(class = "modal-footer",
                               tags$button(type = "button", class = "btn btn-secondary",
                                           `data-bs-dismiss` = "modal",
                                           tags$i(class = "bi bi-x-circle me-1"), "No, Keep It"),
                               actionButton("confirm_close_yes",
                                            label = tagList(tags$i(class = "bi bi-check-circle me-1"),
                                                            "Yes, Close It"),
                                            class = "btn btn-danger")
                      )
             )
    )
  ),
  
  navset_card_pill(id = "tabs")
)

# ── Server ───────────────────────────────────────────────────
server <- function(input, output, session) {
  
  open_tabs     <- reactiveVal(character(0))
  pending_close <- reactiveVal(NULL)
  
  # ── Output directory ────────────────────────────────────────
  roots <- c(
    Home    = path.expand("~"),
    Project = normalizePath(".."),
    C       = "C:/"
  )
  
  shinyDirChoose(input, "btn_browse_output",
                 roots   = roots,
                 session = session)
  
  output_dir <- reactive({
    req(input$btn_browse_output)
    if (is.integer(input$btn_browse_output)) return("output")
    path <- parseDirPath(roots, input$btn_browse_output)
    if (length(path) == 0 || path == "") return("output")
    as.character(path)
  })
  
  observeEvent(output_dir(), {
    path <- output_dir()
    if (!dir.exists(path)) {
      tryCatch({
        dir.create(path, recursive = TRUE)
        showNotification(paste("Created output directory:", path),
                         type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Could not create directory:", e$message),
                         type = "error", duration = 5)
      })
    }
  }, ignoreInit = FALSE)
  
  output$output_dir_status <- renderUI({
    path <- output_dir()
    if (dir.exists(path)) {
      tags$small(
        style = "color: #18bc9c; word-break: break-all;",
        tags$i(class = "bi bi-check-circle me-1"),
        normalizePath(path, mustWork = FALSE)
      )
    } else {
      tags$small(
        style = "color: #e74c3c;",
        tags$i(class = "bi bi-x-circle me-1"),
        "No folder selected"
      )
    }
  })
  
  session$userData$output_dir <- output_dir
  
  # ── Add tab ──────────────────────────────────────────────────
  add_tab <- function(tab_id) {
    cfg <- tab_config[[tab_id]]
    
    if (tab_id %in% open_tabs()) {
      updateTabsetPanel(session, "tabs", selected = tab_id)
      return()
    }
    
    appendTab(
      inputId = "tabs",
      tabPanel(
        title = cfg$label,
        value = tab_id,
        div(
          style = "padding: 20px;",
          cfg$ui_fn(tab_id),
          hr(),
          actionButton(
            session$ns(paste0("close_", tab_id)),
            tagList(tags$i(class = "bi bi-x-lg me-1"), "Close Tab"),
            class = "btn-outline-danger btn-sm"
          )
        )
      ),
      select = TRUE
    )
    
    open_tabs(c(open_tabs(), tab_id))
    cfg$srv_fn(tab_id, session$userData$output_dir)
    
    observeEvent(input[[paste0("close_", tab_id)]], {
      pending_close(tab_id)
      session$sendCustomMessage("update_modal_label", list(label = cfg$label))
      session$sendCustomMessage("show_close_modal", list())
    }, once = FALSE, ignoreInit = TRUE)
  }
  
  # ── Confirm close ────────────────────────────────────────────
  observeEvent(input$confirm_close_yes, {
    tab_id <- pending_close()
    req(!is.null(tab_id))
    session$sendCustomMessage("hide_close_modal", list())
    removeTab(inputId = "tabs", target = tab_id)
    open_tabs(open_tabs()[open_tabs() != tab_id])
    pending_close(NULL)
  })
  
  # ── Sidebar observers ────────────────────────────────────────
  observeEvent(input$nav_overlap,         { add_tab("overlap") })
  observeEvent(input$nav_adjacent,        { add_tab("adjacent") })
  observeEvent(input$nav_interconnection, { add_tab("interconnection") })
  observeEvent(input$nav_padu_ke,         { add_tab("padu_ke") })
  observeEvent(input$nav_padu_hs,         { add_tab("padu_hs") })
  observeEvent(input$nav_padu_kl,         { add_tab("padu_kl") })
  observeEvent(input$nav_padu_kh,         { add_tab("padu_kh") })
  observeEvent(input$nav_padu_rtp,        { add_tab("padu_rtp") })
  observeEvent(input$nav_padu_se,         { add_tab("padu_se") })
  observeEvent(input$nav_padu_ki,         { add_tab("padu_ki") })
  observeEvent(input$nav_padan,           { add_tab("padan") })
  observeEvent(input$nav_recommendation,  { add_tab("recommendation") })
}

# ── JS handlers ──────────────────────────────────────────────
jsCode <- "
$(document).ready(function() {
  Shiny.addCustomMessageHandler('show_close_modal', function(msg) {
    var modal = new bootstrap.Modal(document.getElementById('close_confirm_modal'));
    modal.show();
  });
  Shiny.addCustomMessageHandler('hide_close_modal', function(msg) {
    var modal = bootstrap.Modal.getInstance(document.getElementById('close_confirm_modal'));
    if (modal) modal.hide();
  });
  Shiny.addCustomMessageHandler('update_modal_label', function(msg) {
    document.getElementById('modal_tab_label').innerText = msg.label;
  });
});
"

ui$children <- c(ui$children, list(tags$script(HTML(jsCode))))

shinyApp(ui, server)