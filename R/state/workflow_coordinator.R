# R/state/workflow_coordinator.R
# ============================================================
# Central Application Workflow Coordinator & State Management
# ============================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Tab configuration mapping tab IDs to UI labels and functions
#' @export
tab_config <- list(
  overlap         = list(label = "1.1 Area Tumpang Tindih",      ui_fn = overlap_ui,         srv_fn = overlap_server),
  adjacent        = list(label = "1.2 Area Bertetangga",         ui_fn = adjacent_ui,        srv_fn = adjacent_server),
  interconnection = list(label = "1.3 Area Saling Terhubung",    ui_fn = interconnection_ui, srv_fn = interconnection_server),
  padu_ke         = list(label = "2.1 PADU-KE",             ui_fn = padu_ke_ui,         srv_fn = padu_ke_server),
  padu_hs         = list(label = "2.2 PADU-HS",             ui_fn = padu_hs_ui,         srv_fn = padu_hs_server),
  padu_kl         = list(label = "2.3 PADU-KL",             ui_fn = padu_kl_ui,         srv_fn = padu_kl_server),
  padu_kh         = list(label = "2.4 PADU-KH",             ui_fn = padu_kh_ui,         srv_fn = padu_kh_server),
  padu_rtp        = list(label = "2.5 PADU-RTp",            ui_fn = padu_rtp_ui,        srv_fn = padu_rtp_server),
  padu_se         = list(label = "2.6 PADU-SE",             ui_fn = padu_se_ui,         srv_fn = padu_se_server),
  padu_ki         = list(label = "2.7 PADU-KI",             ui_fn = padu_ki_ui,         srv_fn = padu_ki_server),
  padu_combine    = list(label = "2.8 PADU-Kombinasi",        ui_fn = padu_combine_ui,    srv_fn = padu_combine_server),
  padan           = list(label = "3. PADAN",                ui_fn = padan_ui,           srv_fn = padan_server),
  recommendation_overlaps  = list(label = "4.1 Penyusunan Alternatif Tumpang Tindih",          ui_fn = recommendation_overlaps_ui,  srv_fn = recommendation_overlaps_server),
  recommendation_adjacent  = list(label = "4.2 Penyusunan Alternatif Bertetangga",          ui_fn = recommendation_adjacent_ui,  srv_fn = recommendation_adjacent_server),
  reconcile    = list(label = "5. Rekonsiliasi",             ui_fn = reconcile_ui,    srv_fn = reconcile_server)
)

#' Mapping of module identifiers to output folder subdirectories
#' @export
module_dir_map <- list(
  overlap                 = "Analisis SERASI",
  adjacent                = "Analisis SERASI",
  interconnection         = "Analisis SERASI",
  padu_ke                 = "Analisis PADU-KE",
  padu_hs                 = "Analisis PADU-HS",
  padu_kl                 = "Analisis PADU-KL",
  padu_kh                 = "Analisis PADU-KH",
  padu_rtp                = "Analisis PADU-RTp",
  padu_se                 = "Analisis PADU-SE",
  padu_ki                 = "Analisis PADU-KI",
  padu_combine            = "Analisis PADU-Kombinasi",
  padan                   = "Analisis PADAN",
  recommendation_overlaps = "Penyusunan Alternatif",
  recommendation_adjacent = "Penyusunan Alternatif",
  reconcile               = "Rekonsiliasi"
)

#' Initialize and Bind the LaSPUR Application Workflow Coordinator
#'
#' @param input Shiny session input object
#' @param output Shiny session output object
#' @param session Shiny session object
#' @export
setup_workflow_coordinator <- function(input, output, session) {
  
  session$userData$module_results <- reactiveValues()
  
  open_tabs     <- reactiveVal(character(0))
  pending_close <- reactiveVal(NULL)
  pending_action <- reactiveVal(NULL)
  active_path   <- reactiveVal("")
  report_path    <- reactiveVal(NULL)
  
  session$userData$active_path <- active_path
  session$userData$selected_serasi <- reactiveVal(NULL)
  
  output$active_path_indicator <- renderUI({
    path <- active_path()
    if (path == "") return(NULL)
    if (path == "overlap") {
      tags$div(class = "pulse-badge pulse-overlap",
               tags$div(class = "pulse-dot"),
               "Jalur: Tumpang Tindih")
    } else if (path == "adjacent") {
      tags$div(class = "pulse-badge pulse-adjacent",
               tags$div(class = "pulse-dot"),
               "Jalur: Bertetangga")
    }
  })
  
  shinyjs::onclick("logo_home", {
    updateTabsetPanel(session, "tabs", selected = "home")
  })
  
  observeEvent(input$btn_home, {
    updateTabsetPanel(session, "tabs", selected = "home")
  })
  
  tab_state <- new.env(parent = emptyenv())
  tab_state$gen        <- list()
  tab_state$observers <- list()
  
  destroy_tab_observers <- function(tab_id) {
    obs_list <- tab_state$observers[[tab_id]]
    if (!is.null(obs_list)) {
      for (o in obs_list) if (!is.null(o)) o$destroy()
    }
    tab_state$observers[[tab_id]] <- NULL
  }
  
  seq_overlap <- c("overlap", "padu_ke", "padu_hs", "padu_kl", "padu_kh", "padu_rtp", "padu_se", "padu_ki", "padu_combine", "padan", "recommendation_overlaps", "reconcile")
  seq_adjacent <- c("adjacent", "padu_ke", "padu_hs", "padu_kl", "padu_kh", "padu_rtp", "padu_se", "padu_ki", "padu_combine", "padan", "recommendation_adjacent", "reconcile")
  
  disable_tabs <- function(tabs_to_disable, warning_message) {
    closed_any <- FALSE
    current_open <- open_tabs()
    for (t in tabs_to_disable) {
      if (t %in% current_open) {
        removeTab(inputId = "tabs", target = t)
        destroy_tab_observers(t)
        current_open <- current_open[current_open != t]
        closed_any <- TRUE
      }
    }
    open_tabs(current_open)
    if (closed_any) showNotification(warning_message, type = "warning", duration = 8)
  }
  
  perform_action <- function(action) {
    if (action == "overlap") {
      active_path("overlap")
      shinyjs::show("sidebar_menus")
      shinyjs::show("wrapper_nav_overlap")
      shinyjs::show("wrapper_nav_recommendation_overlaps")
      shinyjs::hide("wrapper_nav_adjacent")
      shinyjs::hide("wrapper_nav_recommendation_adjacent")
      disable_tabs(c("adjacent", "recommendation_adjacent"), "Jalur diubah ke Tumpang Tindih. Tab Area Bertetangga ditutup.")
      add_tab("overlap")
    } else if (action == "adjacent") {
      active_path("adjacent")
      shinyjs::show("sidebar_menus")
      shinyjs::hide("wrapper_nav_overlap")
      shinyjs::hide("wrapper_nav_recommendation_overlaps")
      shinyjs::show("wrapper_nav_adjacent")
      shinyjs::show("wrapper_nav_recommendation_adjacent")
      disable_tabs(c("overlap", "recommendation_overlaps"), "Jalur diubah ke Bertetangga. Tab Area Tumpang Tindih ditutup.")
      add_tab("adjacent")
    }
    session$sendCustomMessage("expand_sidebar", list())
  }
  
  observeEvent(input$btn_path_overlap, {
    pending_action("overlap")
    session$sendCustomMessage("show_info_modal", list(
      title = "Konfirmasi Analisis Tumpang Tindih",
      body_text = "Analisis tumpang tindih bertujuan mengidentifikasi dan menyelesaikan kasus tumpang tindih antara peta RTRW dan RZWP3K yang belum terintegrasi. Pastikan Anda telah memiliki setidaknya data peta RTRW dan RZWP3K yang belum terintegrasi.",
      link_href = "https://laspur.landseascape.id/",
      link_text = "Pelajari lebih lanjut"
    ))
  })
  
  observeEvent(input$btn_path_adjacent, {
    pending_action("adjacent")
    session$sendCustomMessage("show_info_modal", list(
      title = "Konfirmasi Analisis Bertetangga",
      body_text = "Analisis bertetangga bertujuan mengidentifikasi dan menyelesaikan kasus kawasan RTRW terintegrasi yang bertetangga dengan kawasan yang tidak serasi berdasarkan fungsi dan tujuan penetapannya, sehingga berpotensi menimbulkan spillover effect. Pastikan Anda telah memiliki peta RTRW terintegrasi atau hasil rekonsiliasi dari analisis tumpang tindih.",
      link_href = "https://laspur.landseascape.id/",
      link_text = "Pelajari lebih lanjut"
    ))
  })
  
  observeEvent(input$confirm_info_yes, {
    req(!is.null(pending_action()))
    action <- pending_action()
    session$sendCustomMessage("hide_info_modal", list())
    perform_action(action)
    pending_action(NULL)
  })
  
  observeEvent(input$info_modal_dismissed, {
    pending_action(NULL)
  })
  
  observeEvent(input$btn_path_interconnect, {
    showNotification("Fitur ini sedang dalam pengembangan.", type = "warning", duration = 5)
  })
  
  roots <- c(Home = path.expand("~"), Project = normalizePath(".."), shinyFiles::getVolumes()())
  shinyDirChoose(input, "btn_browse_output", roots = roots, session = session)
  
  output_dir <- reactive({
    if (is.null(input$btn_browse_output) || is.integer(input$btn_browse_output)) return("")
    path <- parseDirPath(roots, input$btn_browse_output)
    if (length(path) == 0 || path == "") return("")
    as.character(path)
  })
  
  observeEvent(output_dir(), {
    path <- output_dir()
    report_path(NULL)
    if (!nzchar(path)) return()
    if (!dir.exists(path)) {
      tryCatch({
        dir.create(path, recursive = TRUE)
        showNotification(paste("Direktori output dibuat:", path), type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Gagal membuat direktori:", e$message), type = "error", duration = 5)
      })
    }
  }, ignoreInit = TRUE)
  
  output$output_dir_status <- renderUI({
    path <- output_dir()
    if (dir.exists(path)) {
      tags$small(
        style = "color: #106665; font-weight: 600; word-break: break-all; line-height: 1.4; display: block;",
        icon("check-circle", class="me-1"), normalizePath(path, mustWork = FALSE)
      )
    } else {
      tags$small(style = "color: #94A3B8; font-weight: 500;", icon("circle-info", class="me-1"), " Folder belum dipilih")
    }
  })
  session$userData$output_dir <- output_dir
  
  add_tab <- function(tab_id) {
    cfg <- tab_config[[tab_id]]
    if (tab_id %in% open_tabs()) {
      updateTabsetPanel(session, "tabs", selected = tab_id)
      return()
    }
    destroy_tab_observers(tab_id)
    gen <- (tab_state$gen[[tab_id]] %||% 0L) + 1L
    tab_state$gen[[tab_id]] <- gen
    instance_id <- paste0(tab_id, "__g", gen)
    
    nav_buttons <- div(
      class = "laspur-nav-buttons",
      style = "display: flex; justify-content: flex-end; align-items: center; gap: 12px;",
      if (tab_id %in% c("overlap", "adjacent")) {
        actionButton(paste0("btn_back_", tab_id), "Beranda", icon = icon("house"), class = "btn-outline-secondary btn-sm", style = "font-weight: 600; padding: 8px 16px; border-radius: 8px;")
      } else {
        actionButton(paste0("btn_back_", tab_id), "Kembali", icon = icon("arrow-left"), class = "btn-outline-secondary btn-sm", style = "font-weight: 600; padding: 8px 16px; border-radius: 8px;")
      },
      if (tab_id != "reconcile") {
        actionButton(paste0("btn_next_", tab_id), "Selanjutnya", icon = icon("arrow-right"), class = "btn-primary btn-sm", style = "font-weight: 600; padding: 8px 20px; border-radius: 8px;")
      }
    )
    
    appendTab(
      inputId = "tabs",
      tabPanel(
        title = cfg$label,
        value = tab_id,
        div(
          style = "padding: 16px 20px; background-color: #FFFFFF; border-radius: 0 0 12px 12px; border: 1px solid #E2E8F0; border-top: none;",
          nav_buttons,
          div(
            class = "module-panel-wrapper",
            `data-module-dir` = module_dir_map[[tab_id]] %||% "",
            cfg$ui_fn(instance_id)
          )
        )
      ),
      select = TRUE
    )
    
    open_tabs(c(open_tabs(), tab_id))
    cfg$srv_fn(instance_id, session$userData$output_dir)
    session$sendCustomMessage("add_close_buttons", list())
    
    obs_back <- observeEvent(input[[paste0("btn_back_", tab_id)]], {
      if (tab_id %in% c("overlap", "adjacent")) {
        updateTabsetPanel(session, "tabs", selected = "home")
      } else {
        seq <- if (active_path() == "adjacent") seq_adjacent else seq_overlap
        idx <- match(tab_id, seq)
        if (!is.na(idx) && idx > 1) add_tab(seq[idx - 1])
      }
    }, ignoreInit = TRUE)
    
    obs_next <- NULL
    if (tab_id != "reconcile") {
      obs_next <- observeEvent(input[[paste0("btn_next_", tab_id)]], {
        seq <- if (active_path() == "adjacent") seq_adjacent else seq_overlap
        idx <- match(tab_id, seq)
        if (!is.na(idx) && idx < length(seq)) add_tab(seq[idx + 1])
      }, ignoreInit = TRUE)
    }
    tab_state$observers[[tab_id]] <- list(obs_back, obs_next)
  }
  
  observeEvent(input$request_close_tab, {
    tab_id <- input$request_close_tab
    if (tab_id %in% open_tabs()) {
      cfg <- tab_config[[tab_id]]
      pending_close(tab_id)
      session$sendCustomMessage("update_modal_label", list(label = cfg$label))
      session$sendCustomMessage("show_close_modal", list())
    }
  })
  
  observeEvent(input$confirm_close_yes, {
    tab_id <- pending_close()
    req(!is.null(tab_id))
    session$sendCustomMessage("hide_close_modal", list())
    removeTab(inputId = "tabs", target = tab_id)
    destroy_tab_observers(tab_id)
    open_tabs(open_tabs()[open_tabs() != tab_id])
    pending_close(NULL)
  })
  
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
  observeEvent(input$nav_padu_combine,    { add_tab("padu_combine") })
  observeEvent(input$nav_padan,           { add_tab("padan") })
  observeEvent(input$nav_recommendation_overlaps,  { add_tab("recommendation_overlaps") })
  observeEvent(input$nav_recommendation_adjacent,  { add_tab("recommendation_adjacent") })
  observeEvent(input$nav_reconcile,    { add_tab("reconcile") })
  
  output$btn_open_report_ui <- renderUI({
    p <- report_path()
    if (is.null(p) || !file.exists(p)) return(NULL)
    div(
      style = "margin-top: 8px;",
      actionButton(
        "btn_open_report",
        tagList(
          icon("folder-open", class = "menu-icon fa-fw"),
          tags$span(class = "menu-text", "Buka Laporan")
        ),
        title = "Buka Laporan",
        class = "btn w-100 d-flex align-items-center",
        style = paste(
          "text-align: left; color: #FFFFFF; background-color: #106665;",
          "border: none; padding: 14px 16px; font-weight: 600; border-radius: 8px;"
        )
      )
    )
  })
  
  observeEvent(input$btn_open_report, {
    p <- report_path()
    if (!is.null(p) && file.exists(p)) {
      utils::browseURL(p)
    } else {
      showNotification("File laporan tidak ditemukan.", type = "warning", duration = 5)
    }
  })
  
  observeEvent(input$btn_generate_report, {
    show_report_modal(session, output_dir())
  })
  
  observeEvent(input$open_module_dir_click, {
    req(input$open_module_dir_click)
    dir_name <- input$open_module_dir_click
    if (!nzchar(dir_name)) return()
    open_folder_crossplatform(file.path(output_dir(), dir_name))
  })
  
  observeEvent(input$btn_report_generate, {
    handle_report_generation(input$report_modules_selected, output_dir(), session, report_path)
  })
}
