# ui/app.R
# ============================================================

library(shiny)
library(bslib)
library(future)
library(promises)
library(shinyFiles)
library(shinyjs) 

plan(multisession)

options(shiny.maxRequestSize = 2000 * 1024^2)

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
                 "Modul tidak tersedia"),
          tags$p(style = "font-size: 0.8rem; margin: 4px 0 0 0;",
                 paste0("File tidak ditemukan: ", file))
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
safe_source("modules/mod_padu_combine.R",    "padu_combine_ui",    "padu_combine_server")
safe_source("modules/mod_padan.R",           "padan_ui",           "padan_server")
safe_source("modules/mod_recommendation_overlaps.R",  "recommendation_overlaps_ui",  "recommendation_overlaps_server")
safe_source("modules/mod_recommendation_adjacent.R",  "recommendation_adjacent_ui",  "recommendation_adjacent_server")
safe_source("modules/mod_reconcile.R",    "reconcile_ui",    "reconcile_server")  

# ── Sidebar nav helper ───────────────────────────────────────
nav_item <- function(input_id, number, label) {
  div(
    id = paste0("wrapper_", input_id),
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
  overlap         = list(label = "1.1 Area Tumpang Tindih",      ui_fn = overlap_ui,         srv_fn = overlap_server),
  adjacent        = list(label = "1.2 Area Bertetangga",        ui_fn = adjacent_ui,        srv_fn = adjacent_server),
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
  recommendation_overlaps  = list(label = "4.1 Rekomendasi Tumpang Tindih",          ui_fn = recommendation_overlaps_ui,  srv_fn = recommendation_overlaps_server),
  recommendation_adjacent  = list(label = "4.2 Rekomendasi Bertetangga",          ui_fn = recommendation_adjacent_ui,  srv_fn = recommendation_adjacent_server),
  reconcile    = list(label = "5. Rekonsiliasi",            ui_fn = reconcile_ui,    srv_fn = reconcile_server) 
)

# ── Landing Page UI ──────────────────────────────────────────
landing_page <- tabPanel(
  title = "Beranda",
  value = "home",
  div(
    style = "padding: 40px 20px; max-width: 1200px; margin: 0 auto; text-align: center;",
    
    tags$img(src = "pur_icon.png", style = "max-width: 120px; margin-bottom: 20px;"),
    
    h1("LaSPUR", style = "color: #246484; font-weight: 800; font-size: 3.5rem; margin-bottom: 20px;"),
    
    p(
      "Land and Seascape Planning Unit Reconciliation adalah alat bantu yang dirancang untuk mengintegrasikan dan merekonsiliasi tata ruang darat (Rencana Tata Ruang Wilayah Provinsi/RTRWP) dengan tata ruang laut (Rencana Zonasi Wilayah Pesisir dan Pulau-Pulau Kecil/RZWP3K).",
      style = "font-size: 1.15rem; color: #4a5a6a; margin-bottom: 50px; max-width: 900px; margin-left: auto; margin-right: auto; line-height: 1.6;"
    ),
    
    layout_columns(
      col_widths = c(4, 4, 4),
      
      div(
        style = "background-color: #246484; border-radius: 20px; padding: 40px 25px; color: white; display: flex; flex-direction: column; height: 100%; box-shadow: 0 10px 20px rgba(0,0,0,0.1);",
        
        div(
          style = "width: 130px; height: 130px; border-radius: 50%; background-color: #fff; border: 10px solid #F4A300; margin: 0 auto 25px auto; display: flex; align-items: center; justify-content: center;",
          icon("layer-group", style = "font-size: 3.5rem; color: #F4A300;")
        ),
        
        h3(
          "Tumpang Tindih",
          style = "font-weight: 700; color: #F4A300; margin-bottom: 20px;"
        ),
        
        p(
          "Merekomendasikan penyelesaian persoalan alokasi ruang darat dan laut saling bertampalan secara spasial pada lokasi yang sama, baik sebagian maupun keseluruhan.",
          style = "font-size: 0.95rem; flex-grow: 1; line-height: 1.5;"
        ),
        
        actionButton(
          "btn_path_overlap",
          "Pilih Tumpang Tindih",
          class = "btn-light w-100",
          style = "color: #246484 !important; font-weight: bold; font-size: 1.1rem; padding: 12px; margin-top: 20px; border-radius: 10px;"
        )
      ),
      
      div(
        style = "background-color: #246484; border-radius: 20px; padding: 40px 25px; color: white; display: flex; flex-direction: column; height: 100%; box-shadow: 0 10px 20px rgba(0,0,0,0.1);",
        
        div(
          style = "width: 130px; height: 130px; border-radius: 50%; background-color: #fff; border: 10px solid #F4A300; margin: 0 auto 25px auto; display: flex; align-items: center; justify-content: center;",
          icon("map", style = "font-size: 3.5rem; color: #F4A300;")
        ),
        
        h3(
          "Bertetangga",
          style = "font-weight: 700; color: #F4A300; margin-bottom: 20px;"
        ),
        
        p(
          "Merekomendasikan penyelesaian persoalan batas peruntukan ruang darat dan laut saling berbatasan langsung.",
          style = "font-size: 0.95rem; flex-grow: 1; line-height: 1.5;"
        ),
        
        actionButton(
          "btn_path_adjacent",
          "Pilih Bertetangga",
          class = "btn-light w-100",
          style = "color: #246484 !important; font-weight: bold; font-size: 1.1rem; padding: 12px; margin-top: 20px; border-radius: 10px;"
        )
      ),
      
      div(
        style = "background-color: #246484; border-radius: 20px; padding: 40px 25px; color: white; display: flex; flex-direction: column; height: 100%; box-shadow: 0 10px 20px rgba(0,0,0,0.1);",
        
        div(
          style = "width: 130px; height: 130px; border-radius: 50%; background-color: #fff; border: 10px solid #F4A300; margin: 0 auto 25px auto; display: flex; align-items: center; justify-content: center;",
          icon("project-diagram", style = "font-size: 3.5rem; color: #F4A300;")
        ),
        
        h3(
          "Berpengaruh",
          style = "font-weight: 700; color: #F4A300; margin-bottom: 20px;"
        ),
        
        p(
          "Merekomendasikan alokasi ruang darat atau laut yang memberikan dampak ekologis, sosial, atau ekonomi terhadap sisi lainnya melalui keterhubungan sistem alami maupun fungsional.",
          style = "font-size: 0.95rem; flex-grow: 1; line-height: 1.5;"
        ),
        
        actionButton(
          "btn_path_interconnect",
          "Pilih Berpengaruh",
          class = "btn-light w-100",
          style = "color: #246484 !important; font-weight: bold; font-size: 1.1rem; padding: 12px; margin-top: 20px; border-radius: 10px;"
        )
      )
    ),
    
    div(
      style = "margin-top: 60px; text-align: center;",
      tags$img(src = "logo_konsorsium.png", style = "max-height: 80px; max-width: 100%;")
    )
  )
)

# ── UI ───────────────────────────────────────────────────────
ui <- page_sidebar(
  useShinyjs(), 
  
  title = "Land & Sea Planning Unit Reconcilliation (LaSPUR)",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "cerulean",
    bg = "#f8fafc",
    fg = "#1a2a3a",
    primary = "#2ba6cb",
    base_font = font_google("Nunito")
  ),
  
  tags$style(HTML("
    /* Cards – soft shadows & no harsh borders */
    .card {
      border: none !important;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.05), 0 1px 3px rgba(0, 0, 0, 0.03) !important;
      background-color: #ffffff !important;
    }
    .card-header {
      border-bottom: 1px solid #e5edf2 !important;
      background-color: transparent !important;
      color: #1a2a3a;
    }
  
    /* Buttons – subtle shadows & bright white text */
    .btn {
      color: #ffffff !important;       
      
      border: none !important;
      box-shadow: 0 2px 6px rgba(0, 0, 0, 0.06) !important;
    }
    .btn:hover {
      box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1) !important;
      transform: translateY(-1px);
    }
  
    .btn-outline-primary {
      color: #2ba6cb !important; 
    }
    .btn-outline-secondary {
      color: #6c7a8a !important;
    }
  
    /* Sidebar – light border */
    .sidebar {
      border-right: 1px solid #e5edf2 !important;
      background-color: #f8fafc !important;
    }
  
    /* Tabs – clean underline */
    .nav-tabs .nav-link {
      border: none !important;
      color: #4a5a6a !important;
      padding: 8px 16px;
      border-bottom: 3px solid transparent !important;
    }
    
    /* Active tab font color set to white */
    .nav-tabs .nav-link.active {
      border-bottom: 3px solid #2ba6cb !important;
      background-color: #2ba6cb !important;
      color: #ffffff !important;       
    }
  
    /* Accordion – light borders */
    .accordion-item {
      border: 1px solid #e5edf2 !important;
      box-shadow: 0 1px 3px rgba(0,0,0,0.02) !important;
    }
    
    /* Hide Home tab title in main navigation for a cleaner look */
    .nav-tabs li:first-child a {
      display: none !important;
    }
  ")),
  
  sidebar = sidebar(
    tags$div(
      style = "text-align: center; margin-bottom: 15px;",
      tags$img(
        src = "pur_icon.png",
        width = "100%",
        max_width = "75px",
        style = "border-radius: 8px; margin-bottom: 15px;"
      ),
      actionButton("btn_home", "Beranda / Ubah Jalur", icon = icon("home"), 
                   class = "btn-primary w-100", 
                   style = "font-weight: bold; font-size: 0.9rem;")
    ),
    
    title = "Jelajahi Modul",
    
    div(
      style = "margin-bottom: 16px;",
      tags$label("Direktori Output",
                 style = paste("font-size: 0.85rem; font-weight: 600;",
                               "margin-bottom: 6px; display: block;")),
      shinyDirButton(
        id    = "btn_browse_output",
        label = "Pilih Folder",
        title = "Pilih Direktori Output",
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
        "1. Identifikasi Konflik Spasial",
        nav_item("nav_overlap",         "1.1", "Area Tumpang Tindih"),
        nav_item("nav_adjacent",        "1.2", "Area Bertetangga"),
        shinyjs::hidden(nav_item("nav_interconnection", "1.3", "Area Saling Terhubung"))
      ),
      accordion_panel(
        "2. Analisis PADU",
        nav_item("nav_padu_ke",      "2.1", "PADU-KE"),
        nav_item("nav_padu_hs",      "2.2", "PADU-HS"),
        nav_item("nav_padu_kl",      "2.3", "PADU-KL"),
        nav_item("nav_padu_kh",      "2.4", "PADU-KH"),
        nav_item("nav_padu_rtp",     "2.5", "PADU-RTp"),
        nav_item("nav_padu_se",      "2.6", "PADU-SE"),
        nav_item("nav_padu_ki",      "2.7", "PADU-KI"),
        nav_item("nav_padu_combine", "2.8", "PADU-Kombinasi")
      ),
      accordion_panel(
        "3. Analisis PADAN",
        nav_item("nav_padan", "3", "Analisis PADAN")
      ),
      accordion_panel(
        "4. Rekomendasi",
        nav_item("nav_recommendation_overlaps", "4.1", "Rekomendasi Tumpang Tindih"),
        nav_item("nav_recommendation_adjacent", "4.2", "Rekomendasi Bertetangga")
      ),
      accordion_panel(
        "5. Rekonsiliasi",
        nav_item("nav_reconcile", "5", "Rekonsiliasi")
      )
    )
  ),
  
  tags$div(
    id = "close_confirm_modal", class = "modal fade",
    tabindex = "-1", `data-bs-backdrop` = "static", `data-bs-keyboard` = "false",
    tags$div(class = "modal-dialog modal-dialog-centered",
             tags$div(class = "modal-content",
                      tags$div(class = "modal-header bg-danger text-white",
                               tags$h5(class = "modal-title",
                                       tags$i(class = "bi bi-exclamation-triangle-fill me-2"),
                                       "Tutup Tab"),
                               tags$button(type = "button", class = "btn-close btn-close-white",
                                           `data-bs-dismiss` = "modal")
                      ),
                      tags$div(class = "modal-body",
                               tags$p(class = "mb-0",
                                      "Apakah Anda yakin ingin menutup ",
                                      tags$strong(id = "modal_tab_label", "tab ini"),
                                      "? Perubahan yang belum disimpan akan hilang.")
                      ),
                      tags$div(class = "modal-footer",
                               tags$button(type = "button", class = "btn btn-secondary",
                                           `data-bs-dismiss` = "modal",
                                           tags$i(class = "bi bi-x-circle me-1"), "Tidak, Batal"),
                               actionButton("confirm_close_yes",
                                            label = tagList(tags$i(class = "bi bi-check-circle me-1"),
                                                            "Ya, Tutup"),
                                            class = "btn btn-danger")
                      )
             )
    )
  ),
  
  tags$a(
    id = "user-guide-link",
    href = "https://lumens.or.id/id/",
    target = "_blank",
    class = "btn btn-warning btn-sm",
    style = "
    display: none;
    white-space: nowrap;
    padding: 6px 14px;
    font-weight: 600;
    display: inline-flex;
    align-items: center;
    gap: 8px;
  ",
    tags$i(class = "bi bi-question-circle"),
    span("User Guide")
  ),
  
  navset_card_pill(id = "tabs", landing_page)
)

# ── Server ───────────────────────────────────────────────────
server <- function(input, output, session) {
  
  open_tabs     <- reactiveVal(character(0))
  pending_close <- reactiveVal(NULL)
  
  active_path   <- reactiveVal("overlap") 
  
  seq_overlap <- c("overlap", "padu_ke", "padu_hs", "padu_kl", "padu_kh", "padu_rtp", "padu_se", "padu_ki", "padu_combine", "padan", "recommendation_overlaps", "reconcile")
  seq_adjacent <- c("adjacent", "padu_ke", "padu_hs", "padu_kl", "padu_kh", "padu_rtp", "padu_se", "padu_ki", "padu_combine", "padan", "recommendation_adjacent", "reconcile")
  
  disable_tabs <- function(tabs_to_disable, warning_message) {
    closed_any <- FALSE
    current_open <- open_tabs()
    
    for (t in tabs_to_disable) {
      if (t %in% current_open) {
        removeTab(inputId = "tabs", target = t)
        current_open <- current_open[current_open != t]
        closed_any <- TRUE
      }
    }
    
    open_tabs(current_open)
    
    if (closed_any) {
      showNotification(warning_message, type = "warning", duration = 8)
    }
  }
  
  observeEvent(input$btn_home, {
    updateTabsetPanel(session, "tabs", selected = "home")
  })
  
  observeEvent(input$btn_path_overlap, {
    active_path("overlap")
    
    shinyjs::show("wrapper_nav_overlap")
    shinyjs::show("wrapper_nav_recommendation_overlaps")
    
    shinyjs::hide("wrapper_nav_adjacent")
    shinyjs::hide("wrapper_nav_recommendation_adjacent")
    
    disable_tabs(
      c("adjacent", "recommendation_adjacent"),
      "Jalur diubah ke Tumpang Tindih. Tab Area Bertetangga dinonaktifkan dan ditutup."
    )
    
    showNotification("Jalur Tumpang Tindih aktif. Silakan pilih modul di menu sebelah kiri.", type = "message", duration = 5)
    add_tab("overlap")
  })
  
  observeEvent(input$btn_path_adjacent, {
    active_path("adjacent")
    
    shinyjs::hide("wrapper_nav_overlap")
    shinyjs::hide("wrapper_nav_recommendation_overlaps")
    
    shinyjs::show("wrapper_nav_adjacent")
    shinyjs::show("wrapper_nav_recommendation_adjacent")
    
    disable_tabs(
      c("overlap", "recommendation_overlaps"),
      "Jalur diubah ke Bertetangga. Tab Area Tumpang Tindih dinonaktifkan dan ditutup."
    )
    
    showNotification("Jalur Bertetangga aktif. Silakan pilih modul di menu sebelah kiri.", type = "message", duration = 5)
    add_tab("adjacent")
  })
  
  observeEvent(input$btn_path_interconnect, {
    showNotification("Fitur ini sedang dalam pengembangan.", type = "warning", duration = 5)
  })
  
  roots <- c(
    Home    = path.expand("~"),
    Project = normalizePath(".."),
    shinyFiles::getVolumes()()  
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
        showNotification(paste("Direktori output dibuat:", path),
                         type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Gagal membuat direktori:", e$message),
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
        "Belum ada folder yang dipilih"
      )
    }
  })
  
  session$userData$output_dir <- output_dir
  
  # ── Add tab ───────────────────────────────────────────────────
  add_tab <- function(tab_id) {
    cfg <- tab_config[[tab_id]]
    
    if (tab_id %in% open_tabs()) {
      updateTabsetPanel(session, "tabs", selected = tab_id)
      return()
    }
    
    # ── Top Navigation Bar Layout ──────────────────────────────────
    nav_buttons <- div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px; border-bottom: 1px solid #e5edf2; padding-bottom: 15px;",
      
      # LEFT SIDE: Home/Back & Next Buttons
      div(
        style = "display: flex; gap: 10px;",
        
        # Home (Beranda) if on the first module, else Back (Sebelumnya)
        if (tab_id %in% c("overlap", "adjacent")) {
          actionButton(paste0("btn_back_", tab_id), "Beranda", icon = icon("home"), class = "btn-outline-secondary btn-sm")
        } else {
          actionButton(paste0("btn_back_", tab_id), "Sebelumnya", icon = icon("arrow-left"), class = "btn-outline-secondary btn-sm")
        },
        
        # Next (hidden on the last module 'reconcile')
        if (tab_id != "reconcile") {
          actionButton(paste0("btn_next_", tab_id), "Selanjutnya", icon = icon("arrow-right"), class = "btn-primary btn-sm")
        }
      ),
      
      # RIGHT SIDE: Close Tab Button
      div(
        actionButton(
          paste0("close_", tab_id),
          tagList(tags$i(class = "bi bi-x-lg me-1"), "Tutup Tab"),
          class = "btn-outline-danger btn-sm"
        )
      )
    )
    
    appendTab(
      inputId = "tabs",
      tabPanel(
        title = cfg$label,
        value = tab_id,
        div(
          style = "padding: 20px;",
          nav_buttons, # Top Navigation bar injected here
          cfg$ui_fn(tab_id)
        )
      ),
      select = TRUE
    )
    
    open_tabs(c(open_tabs(), tab_id))
    cfg$srv_fn(tab_id, session$userData$output_dir)
    
    # Listeners
    observeEvent(input[[paste0("btn_back_", tab_id)]], {
      if (tab_id %in% c("overlap", "adjacent")) {
        updateTabsetPanel(session, "tabs", selected = "home")
      } else {
        seq <- if (active_path() == "adjacent") seq_adjacent else seq_overlap
        idx <- match(tab_id, seq)
        if (!is.na(idx) && idx > 1) {
          prev_tab <- seq[idx - 1]
          add_tab(prev_tab) 
        }
      }
    }, ignoreInit = TRUE)
    
    if (tab_id != "reconcile") {
      observeEvent(input[[paste0("btn_next_", tab_id)]], {
        seq <- if (active_path() == "adjacent") seq_adjacent else seq_overlap
        idx <- match(tab_id, seq)
        if (!is.na(idx) && idx < length(seq)) {
          next_tab <- seq[idx + 1]
          add_tab(next_tab) 
        }
      }, ignoreInit = TRUE)
    }
    
    observeEvent(input[[paste0("close_", tab_id)]], {
      pending_close(tab_id)
      session$sendCustomMessage("update_modal_label", list(label = cfg$label))
      session$sendCustomMessage("show_close_modal", list())
    }, once = FALSE, ignoreInit = TRUE)
  }
  
  observeEvent(input$confirm_close_yes, {
    tab_id <- pending_close()
    req(!is.null(tab_id))
    session$sendCustomMessage("hide_close_modal", list())
    removeTab(inputId = "tabs", target = tab_id)
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
}

jsCode <- "
$(document).ready(function() {

  function addUserGuideButton() {

    if ($('#navbar-user-guide').length)
      return;

    var btn = $('#user-guide-link');

    if (!btn.length)
      return;

    btn.attr('id', 'navbar-user-guide');
    btn.css('display', 'inline-flex');

    // Right side of the title bar
    $('.navbar').append(
      $('<div>')
        .css({
          'margin-left':'auto',
          'margin-right':'15px'
        })
        .append(btn)
    );
  }

  addUserGuideButton();

  Shiny.addCustomMessageHandler('show_close_modal', function(msg) {
    var modal = new bootstrap.Modal(
      document.getElementById('close_confirm_modal')
    );
    modal.show();
  });

  Shiny.addCustomMessageHandler('hide_close_modal', function(msg) {
    var modal = bootstrap.Modal.getInstance(
      document.getElementById('close_confirm_modal')
    );
    if (modal) modal.hide();
  });

  Shiny.addCustomMessageHandler('update_modal_label', function(msg) {
    document.getElementById('modal_tab_label').innerText = msg.label;
  });

});
"

ui$children <- c(ui$children, list(tags$script(HTML(jsCode))))

shinyApp(ui, server)