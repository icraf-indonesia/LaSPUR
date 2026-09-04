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

# ── small utility ─────────────────────────────────────────────
`%||%` <- function(a, b) if (is.null(a)) b else a

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
    
    assign(srv_fn_name, function(id, output_dir) {
      moduleServer(id, function(input, output, session) {})
    }, envir = .GlobalEnv)
  }
}

# ── Source all modules ───────────────────────────────────────
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
safe_source("modules/mod_reconcile.R",    "reconcile_ui",    "reconcile_server")  

# ── UI Helpers ───────────────────────────────────────────────

nav_item <- function(input_id, number, label) {
  div(
    id = paste0("wrapper_", input_id),
    class = "nav-item-wrapper",
    title = label, 
    style = "display: flex; align-items: center; padding: 8px 12px; border-radius: 8px; cursor: pointer; transition: all 0.2s ease; margin-bottom: 4px; border: 1px solid transparent; width: 100%; box-sizing: border-box;",
    onmouseover = "this.style.background='#eef6fc'; this.style.borderColor='#E2E8F0'; this.style.color='#1b75ba'",
    onmouseout  = "this.style.background='transparent'; this.style.borderColor='transparent'; this.style.color='inherit'",
    onclick     = sprintf("Shiny.setInputValue('%s', Math.random())", input_id),
    
    tags$div(
      class = "nav-item-content",
      style = "display: flex; align-items: center; gap: 10px; width: 100%; box-sizing: border-box;",
      tags$span(
        class = "nav-number",
        style = "font-size: 0.75rem; font-weight: 700; color: #106665; background: #e6f2f2; border-radius: 6px; padding: 2px 8px; min-width: 32px; text-align: center; flex-shrink: 0;",
        number
      ),
      tags$span(
        class = "nav-label",
        style = "font-size: 0.85rem; font-weight: 600; color: #334155; line-height: 1.3;",
        label
      )
    )
  )
}

acc_title <- function(fa_icon, label_text) {
  tags$div(
    class = "acc-title-wrapper",
    title = label_text,
    style = "display: flex; align-items: center; width: 100%; justify-content: space-between; box-sizing: border-box;",
    fa_icon,
    tags$span(class = "menu-text", label_text)
  )
}

# ── Tab config ───────────────────────────────────────────────
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

# ── Report module config ─────────────────────────────────────
report_module_config <- list(
  serasi = list(
    label    = "Analisis SERASI",
    template = "report/LaSPUR_SERASI_report_template.Rmd"
  ),
  padu = list(
    padu_ke = list(
      label    = "PADU-KE",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    ),
    padu_hs = list(
      label    = "PADU-HS",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    ),
    padu_kl = list(
      label    = "PADU-KL",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    ),
    padu_kh = list(
      label    = "PADU-KH",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    ),
    padu_rtp = list(
      label    = "PADU-RTp",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    ),
    padu_se = list(
      label    = "PADU-SE",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    ),
    padu_ki = list(
      label    = "PADU-KI",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    ),
    padu_combine = list(
      label    = "PADU-Kombinasi",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    )
  ),
  padan = list(
    label    = "Analisis PADAN",
    template = "report/LaSPUR_PADAN_report_template.Rmd"
  ),
  recommendation = list(
    label    = "Analisis Penyusunan Alternatif",
    template = "report/LaSPUR_ALTERNATIVE_report_template.Rmd"
  ),
  reconcile = list(
    label    = "Rekonsiliasi",
    template = "report/LaSPUR_RECONCILLIATION_report_template.Rmd"
  )
)

# ── Landing Page UI ──────────────────────────────────────────
landing_page <- tabPanel(
  title = "Beranda",
  value = "home",
  div(
    style = "padding: 60px 20px; max-width: 1100px; margin: 0 auto; text-align: center;",
    
    tags$img(src = "logo_laspur.png", style = "max-width: 600px; margin-bottom: 24px; border-radius: 20px;"),
    
    # h1("LaSPUR", style = "color: #1b75ba; font-weight: 800; font-size: 3.5rem; margin-bottom: 16px; letter-spacing: -1px;"),
    
    p(
      "Land and Seascape Planning Unit Reconciliation adalah alat bantu perancangan tata ruang darat (RTRWP) dengan tata ruang laut (RZWP3K).",
      style = "font-size: 1.15rem; color: #64748B; margin-bottom: 60px; max-width: 750px; margin-left: auto; margin-right: auto; line-height: 1.6; font-weight: 400;"
    ),
    
    layout_columns(
      col_widths = c(4, 4, 4),
      
      div(
        class = "landing-card",
        style = "background-color: #FFFFFF; border: 1px solid #E2E8F0; border-radius: 24px; padding: 40px 30px; display: flex; flex-direction: column; height: 100%; box-shadow: 0 10px 25px -5px rgba(0,0,0,0.05); transition: transform 0.3s ease, box-shadow 0.3s ease;",
        div(
          style = "width: 80px; height: 80px; border-radius: 20px; background-color: #e6f2f2; color: #106665; margin: 0 auto 24px auto; display: flex; align-items: center; justify-content: center;",
          icon("layer-group", style = "font-size: 2.5rem;")
        ),
        h3("Tumpang Tindih", style = "font-weight: 700; color: #1E293B; margin-bottom: 16px; font-size: 1.4rem;"),
        p("Rekomendasi penyelesaian alokasi ruang darat & laut saling bertampalan secara spasial pada lokasi yang sama.",
          style = "font-size: 0.95rem; color: #64748B; flex-grow: 1; line-height: 1.6;"),
        actionButton("btn_path_overlap", "Pilih Tumpang Tindih", class = "btn-primary w-100 mt-4",
                     style = "background-color: #1b75ba; border: none; font-weight: 600; padding: 12px; border-radius: 12px;")
      ),
      
      div(
        class = "landing-card",
        style = "background-color: #FFFFFF; border: 1px solid #E2E8F0; border-radius: 24px; padding: 40px 30px; display: flex; flex-direction: column; height: 100%; box-shadow: 0 10px 25px -5px rgba(0,0,0,0.05); transition: transform 0.3s ease, box-shadow 0.3s ease;",
        div(
          style = "width: 80px; height: 80px; border-radius: 20px; background-color: #eef6fc; color: #1b75ba; margin: 0 auto 24px auto; display: flex; align-items: center; justify-content: center;",
          icon("map", style = "font-size: 2.5rem;")
        ),
        h3("Bertetangga", style = "font-weight: 700; color: #1E293B; margin-bottom: 16px; font-size: 1.4rem;"),
        p("Rekomendasi penyelesaian persoalan batas peruntukan ruang darat dan laut saling berbatasan langsung.",
          style = "font-size: 0.95rem; color: #64748B; flex-grow: 1; line-height: 1.6;"),
        actionButton("btn_path_adjacent", "Pilih Bertetangga", class = "btn-primary w-100 mt-4",
                     style = "background-color: #1b75ba; border: none; font-weight: 600; padding: 12px; border-radius: 12px;")
      ),
      
      div(
        class = "landing-card",
        style = "background-color: #FFFFFF; border: 1px solid #E2E8F0; border-radius: 24px; padding: 40px 30px; display: flex; flex-direction: column; height: 100%; box-shadow: 0 10px 25px -5px rgba(0,0,0,0.05); transition: transform 0.3s ease, box-shadow 0.3s ease;",
        div(
          style = "width: 80px; height: 80px; border-radius: 20px; background-color: #F8FAFC; color: #94A3B8; margin: 0 auto 24px auto; display: flex; align-items: center; justify-content: center;",
          icon("diagram-project", style = "font-size: 2.5rem;")
        ),
        h3("Berpengaruh", style = "font-weight: 700; color: #1E293B; margin-bottom: 16px; font-size: 1.4rem;"),
        p("Rekomendasi alokasi ruang yang memberi dampak sosio-ekologis melalui keterhubungan alami fungsional.",
          style = "font-size: 0.95rem; color: #64748B; flex-grow: 1; line-height: 1.6;"),
        actionButton("btn_path_interconnect", "Segera Hadir", class = "btn-light w-100 mt-4",
                     style = "background-color: #F1F5F9; color: #64748B; border: none; font-weight: 600; padding: 12px; border-radius: 12px;")
      )
    ),
    
    div(
      style = "margin-top: 80px; text-align: center; opacity: 0.85;",
      tags$img(src = "logo_konsorsium.png", style = "max-height: 60px;")
    )
  )
)

# ── UI ───────────────────────────────────────────────────────
ui <- page_sidebar(
  useShinyjs(), 
  
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "logotype_laspur.png"),
    tags$link(rel = "stylesheet", href = "icons/bootstrap-icons.css"),
    tags$style(HTML("
      @font-face {
        font-family: 'Plus Jakarta Sans';
        src: url('fonts/PlusJakartaSans-VariableFont_wght.ttf') format('truetype');
        font-weight: 200 800;
        font-style: normal;
      }
    "))
  ),
  
  title = tags$div(
    id = "logo_home",
    class = "d-flex align-items-center",
    style = "padding-left: 10px; cursor: pointer;",
    tags$img(src = "logotype_laspur.png", style = "height: 25px; margin-right: 12px; border-radius: 6px;"),
    # tags$span("LaSPUR", style = "font-weight: 800; font-size: 1.4rem; color: #1b75ba; letter-spacing: -0.5px;"),
    uiOutput("active_path_indicator", inline = TRUE)
  ),
  # window_title = "LaSPUR",
  
  theme = bs_theme(
    version = 5,
    bg = "#F8FAFC",
    fg = "#1E293B",
    primary = "#1b75ba",
    success = "#106665",
    base_font = font_google("Plus Jakarta Sans") 
  ),
  
  tags$style(HTML("
    /* Base Overrides */
    body { font-family: 'Plus Jakarta Sans', sans-serif !important; overflow-x: hidden; }
    .navbar { border-bottom: 1px solid #E2E8F0 !important; background-color: #FFFFFF !important; box-shadow: 0 1px 3px rgba(0,0,0,0.02) !important;}
    
    /* ========================================================
       CUSTOM SCROLLBAR AUTO-HIDE
       ======================================================== */
    ::-webkit-scrollbar { width: 6px; height: 6px; }
    ::-webkit-scrollbar-track { background: transparent; }
    ::-webkit-scrollbar-thumb { background-color: rgba(148, 163, 184, 0); border-radius: 10px; transition: background-color 0.3s ease; }
    :hover::-webkit-scrollbar-thumb { background-color: rgba(148, 163, 184, 0.4); }
    ::-webkit-scrollbar-thumb:hover { background-color: rgba(148, 163, 184, 0.7); }
    * { scrollbar-width: thin; scrollbar-color: rgba(148, 163, 184, 0.4) transparent; }

    /* ========================================================
       ANIMASI PULSE INDICATOR DI NAVBAR
       ======================================================== */
    .pulse-badge {
      display: inline-flex; align-items: center; gap: 8px;
      padding: 4px 14px; border-radius: 20px; font-size: 0.75rem; font-weight: 700;
      text-transform: uppercase; letter-spacing: 0.5px; margin-left: 18px;
      animation: fadeIn 0.4s ease forwards;
    }
    @keyframes fadeIn { from { opacity: 0; transform: translateX(-10px); } to { opacity: 1; transform: translateX(0); } }
    .pulse-dot { width: 8px; height: 8px; border-radius: 50%; }

    .pulse-overlap { background-color: #e6f2f2; color: #106665; border: 1px solid #106665; }
    .pulse-overlap .pulse-dot { background-color: #106665; animation: pulse-emerald 1.5s infinite; }
    @keyframes pulse-emerald { 0% { transform: scale(0.95); box-shadow: 0 0 0 0 rgba(16, 102, 101, 0.7); } 70% { transform: scale(1); box-shadow: 0 0 0 6px rgba(16, 102, 101, 0); } 100% { transform: scale(0.95); box-shadow: 0 0 0 0 rgba(16, 102, 101, 0); } }

    .pulse-adjacent { background-color: #eef6fc; color: #1b75ba; border: 1px solid #1b75ba; }
    .pulse-adjacent .pulse-dot { background-color: #1b75ba; animation: pulse-blue 1.5s infinite; }
    @keyframes pulse-blue { 0% { transform: scale(0.95); box-shadow: 0 0 0 0 rgba(27, 117, 186, 0.7); } 70% { transform: scale(1); box-shadow: 0 0 0 6px rgba(27, 117, 186, 0); } 100% { transform: scale(0.95); box-shadow: 0 0 0 0 rgba(27, 117, 186, 0); } }

    /* ========================================================
       AMBIL ALIH LAYOUT DARI BSLIB
       ======================================================== */
    .bslib-sidebar-layout > .collapse-toggle,
    .bslib-sidebar-layout > .bslib-sidebar-resizer,
    [data-bslib-sidebar-resizer] { display: none !important; pointer-events: none !important; }

    @media (min-width: 768px) {
      .bslib-sidebar-layout { display: grid !important; grid-template-columns: 330px minmax(0, 1fr) !important; transition: grid-template-columns 0.35s cubic-bezier(0.4, 0, 0.2, 1) !important; }
      body.sidebar-mini .bslib-sidebar-layout { grid-template-columns: 80px minmax(0, 1fr) !important; }
    }
    .bslib-sidebar-layout > aside {
      width: 100% !important; max-width: 100% !important; min-width: 100% !important;
      border-right: 1px solid #E2E8F0 !important; background-color: #FFFFFF !important;
      padding-top: 15px !important; overflow-x: hidden !important;
    }

    /* Cards Umum */
    .card { border: 1px solid #E2E8F0 !important; border-radius: 16px !important; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05) !important; background-color: #FFFFFF !important;}
    .btn-primary { background-color: #1b75ba !important; border: none !important; box-shadow: 0 4px 6px -1px rgba(27, 117, 186, 0.2) !important; }
    .btn-primary:hover { background-color: #155d96 !important; transform: translateY(-1px); box-shadow: 0 6px 8px -1px rgba(27, 117, 186, 0.3) !important;}
    .btn-outline-secondary { color: #475569 !important; border: 1px solid #CBD5E1 !important; background: transparent; }
    .btn-outline-secondary:hover { background-color: #F8FAFC !important; border-color: #94A3B8 !important; color: #1E293B !important;}
    .landing-card:hover { transform: translateY(-5px); box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.1) !important; }
    
    /* ========================================================
       PERBAIKAN ALIGNMENT (ACCORDION BUTTON & BODY)
       ======================================================== */
    /* Normal Mode */
    aside .menu-text { display: block; font-weight: 700; white-space: nowrap; }
    aside .menu-icon { display: none !important; }
    aside .accordion-item { border: none !important; background: transparent !important; }
    
    aside .accordion-button {
      background-color: transparent !important; color: #475569 !important; font-size: 0.95rem; 
      padding: 16px 14px !important; width: 100% !important; box-sizing: border-box !important;
      box-shadow: none !important; border-bottom: 1px solid #F1F5F9; white-space: nowrap; transition: all 0.2s ease;
    }
    aside .accordion-body { 
      padding: 8px 14px 16px 14px !important; 
      width: 100% !important; 
      box-sizing: border-box !important; 
    }
    
    aside .accordion-button:not(.collapsed) { color: #1b75ba !important; background-color: transparent !important; }
    aside .accordion-button:focus { box-shadow: none !important; }

    /* ========================================================
       SIDEBAR HIDE (MINI MODE)
       ======================================================== */
    body.sidebar-mini aside .menu-text, 
    body.sidebar-mini aside .nav-label,
    body.sidebar-mini aside .sidebar-title-text { display: none !important; }
    
    body.sidebar-mini aside .menu-icon { display: flex !important; justify-content: center; align-items: center; margin: 0 auto !important; font-size: 1.35rem !important; width: 32px !important; height: 32px !important; color: #1b75ba; }
    
    body.sidebar-mini aside .accordion-button { padding: 16px 0 !important; justify-content: center !important; display: flex !important; width: 100% !important; box-sizing: border-box !important; }
    body.sidebar-mini aside .accordion-button::after { display: none !important; }
    
    /* Gunakan Flex Column untuk memastikan sub-item presisi di tengah */
    body.sidebar-mini aside .accordion-body { 
      padding: 8px 0 16px 0 !important; 
      display: flex !important; 
      flex-direction: column !important; 
      align-items: center !important;
      width: 100% !important; 
      box-sizing: border-box !important;
    }
    
    body.sidebar-mini aside .sidebar-header { justify-content: center !important; padding-bottom: 16px !important; }
    body.sidebar-mini aside #sidebar-toggle-btn { margin: 0 auto; }
    body.sidebar-mini aside #btn_home { padding: 14px 0 !important; justify-content: center !important; background-color: transparent !important; }
    body.sidebar-mini aside #btn_home:hover { background-color: #eef6fc !important; }
    
    body.sidebar-mini aside .dir-chooser-wrapper { opacity: 0; height: 0; padding: 0 !important; margin: 0 !important; overflow: hidden; border: none !important; }
    
    body.sidebar-mini aside .nav-item-wrapper { 
      padding: 8px 0 !important; justify-content: center !important; background: transparent !important; border: none !important; 
      width: 48px !important; margin: 0 0 4px 0 !important; box-sizing: border-box !important;
    }
    body.sidebar-mini aside .nav-item-content { justify-content: center !important; width: 100% !important; gap: 0 !important; }
    body.sidebar-mini aside .nav-number { 
      margin: 0 !important; font-size: 0.75rem !important; padding: 0 !important; 
      width: 32px !important; min-width: 32px !important; height: 32px !important; 
      display: flex !important; align-items: center !important; justify-content: center !important; border-radius: 8px !important;
    }
    
    /* ── Report footer button in mini mode ── */
    body:not(.sidebar-mini) aside #btn_generate_report .menu-icon,
    body:not(.sidebar-mini) aside #btn_home .menu-icon {
      display: inline-block !important;
      margin-right: 8px !important;
    }
    
    body.sidebar-mini aside .report-sidebar-footer {
      border-top: none !important;
      padding-top: 8px !important;
      margin-top: 8px !important;
      display: flex !important;
      flex-direction: column !important;
      align-items: center !important;
    }
    body.sidebar-mini aside .report-sidebar-footer .report-sidebar-subtitle { display: none !important; }
    body.sidebar-mini aside #btn_generate_report {
      width: 48px !important;
      padding: 14px 0 !important;
      justify-content: center !important;
      background-color: transparent !important;
    }
    body.sidebar-mini aside #btn_generate_report:hover { background-color: #eef6fc !important; }
    
    /* ========================================================
       TABS BROWSER ALA CHROME & TOMBOL CLOSE PADA HOVER
       ======================================================== */
    .card-header { padding: 0 !important; border-bottom: 1px solid #E2E8F0 !important; background-color: #F8FAFC !important; border-radius: 16px 16px 0 0 !important; }
    
    #tabs.nav-pills { padding-top: 8px; padding-left: 8px; margin: 0 !important; border-bottom: none !important;}
    #tabs.nav-pills .nav-link {
      border: 1px solid transparent !important; color: #64748B !important; font-weight: 600;
      padding: 10px 16px; margin-right: 4px; border-radius: 10px 10px 0 0 !important; display: flex; align-items: center; transition: all 0.2s ease;
    }
    #tabs.nav-pills .nav-link:hover { background-color: #F1F5F9; border-color: #E2E8F0 #E2E8F0 transparent; }
    
    #tabs.nav-pills .nav-link.active { 
      background-color: #FFFFFF !important; color: #1b75ba !important; 
      border-color: #E2E8F0 #E2E8F0 #FFFFFF !important; margin-bottom: -1px; padding-bottom: 11px;
    }
    #tabs.nav-pills li:first-child a { display: none !important; }
    
    .close-tab-btn {
      margin-left: 12px; padding: 2px; width: 20px; height: 20px; display: inline-flex; align-items: center;
      justify-content: center; border-radius: 50%; color: #94A3B8; font-size: 0.85rem; transition: all 0.2s ease; cursor: pointer;
      opacity: 0; 
    }
    
    #tabs.nav-pills .nav-link:hover .close-tab-btn,
    #tabs.nav-pills .nav-link.active .close-tab-btn { opacity: 1; }
    .close-tab-btn:hover { background-color: #FEE2E2 !important; color: #EF4444 !important; }

    /* ========================================================
       COLLAPSIBLE LEFT PANEL (INPUT & PARAMETER)
       ======================================================== */

    /* The row must not wrap so collapse works cleanly */
    .module-panel-wrapper > .row {
      flex-wrap: nowrap;
      overflow: hidden;
    }

    /* Left col: transition flex-basis + opacity + padding */
    .module-panel-wrapper > .row > .col-sm-4 {
      flex: 0 0 33.3333%;
      max-width: 33.3333%;
      overflow: hidden;
      transition: flex      0.35s cubic-bezier(0.4, 0, 0.2, 1),
                  max-width 0.35s cubic-bezier(0.4, 0, 0.2, 1),
                  opacity   0.25s ease,
                  padding   0.35s ease;
    }

    /* Right col */
    .module-panel-wrapper > .row > .col-sm-8 {
      flex: 0 0 66.6667%;
      max-width: 66.6667%;
      transition: flex      0.35s cubic-bezier(0.4, 0, 0.2, 1),
                  max-width 0.35s cubic-bezier(0.4, 0, 0.2, 1);
    }

    /* Collapsed — left col shrinks to zero */
    .module-panel-wrapper.panel-collapsed > .row > .col-sm-4 {
      flex: 0 0 0% !important;
      max-width: 0 !important;
      opacity: 0;
      padding-left: 0 !important;
      padding-right: 0 !important;
      pointer-events: none;
    }
    /* Right col fills full width */
    .module-panel-wrapper.panel-collapsed > .row > .col-sm-8 {
      flex: 0 0 100% !important;
      max-width: 100% !important;
    }

    /* ========================================================
       TOGGLE BUTTON STICKY ON RIGHT PANEL
       ======================================================== */
    .panel-toggle-container {
      position: sticky;
      top: 0;
      z-index: 10;
      background: #FFFFFF;
      padding: 8px 16px;
      border-bottom: 1px solid #E2E8F0;
      display: flex;
      justify-content: flex-end;
      align-items: center;
      box-shadow: 0 2px 4px rgba(0,0,0,0.02);
      border-radius: 0 0 8px 8px;
      margin-bottom: 8px;
    }
    .panel-toggle-btn {
      display: inline-flex;
      align-items: center;
      gap: 6px;
      padding: 6px 14px;
      font-size: 0.8rem;
      font-weight: 600;
      color: #475569;
      background-color: #F1F5F9;
      border: 1px solid #E2E8F0;
      border-radius: 8px;
      cursor: pointer;
      white-space: nowrap;
      transition: background-color 0.2s ease, color 0.2s ease, border-color 0.2s ease;
      line-height: 1.4;
    }
    .panel-toggle-btn:hover {
      background-color: #eef6fc;
      color: #1b75ba;
      border-color: #1b75ba;
    }
    
    /* ========================================================
       COLLAPSIBLE TOGGLE BUTTON ON RIGHT PANEL
       ======================================================== */
    /* ── Equal height for left & right panels ── */
    .module-panel-wrapper > .row {
      display: flex;
      flex-wrap: wrap;
      align-items: stretch;
    }
    .module-panel-wrapper > .row > [class*='col-'] {
      display: flex;
      flex-direction: column;
    }
    .module-panel-wrapper > .row > [class*='col-'] > .card,
    .module-panel-wrapper > .row > [class*='col-'] > div:not(.panel-toggle-container) {
      flex: 1;
      height: 100%;
    }
    
    /* ── Equal height & alignment for card headers ── */
    .card-header {
      min-height: 56px;
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 8px 20px !important;
      border-radius: 16px 16px 0 0 !important;
      overflow: visible !important;
    }
    
    .card-header .card-title,
    .card-header h5,
    .card-header h4,
    .card-header h3 {
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      flex-shrink: 1;
      margin: 0;
    }
    
    .panel-toggle-btn {
      margin: 0 !important;
      align-self: center;
    }

    /* ── COMPACTNESS IMPROVEMENTS ──────────────────────────── */
    /* Reduce card spacing */
    .card {
      margin-bottom: 0.75rem !important;
    }
    .card-body {
      padding: 0.75rem 1rem !important;
    }
    .card-header {
      padding: 0.5rem 1rem !important;
    }

    /* Tighter column gutters inside module panels */
    .module-panel-wrapper > .row {
      margin-left: -8px;
      margin-right: -8px;
    }
    .module-panel-wrapper > .row > [class*='col-'] {
      padding-left: 8px;
      padding-right: 8px;
    }

    /* Reduce landing card padding */
    .landing-card {
      padding: 24px 20px !important;
    }

    /* ── Tighter spacing for file inputs & form elements ── */
    .module-panel-wrapper .form-group,
    .module-panel-wrapper .shiny-input-container {
      margin-bottom: 0.5rem !important;
    }
    .module-panel-wrapper .form-group label,
    .module-panel-wrapper .shiny-input-container label {
      margin-bottom: 0.15rem !important;
      font-size: 0.9rem;
    }
    .module-panel-wrapper .shiny-input-container .btn-file {
      padding: 0.25rem 0.9rem !important;
    }
    .module-panel-wrapper .shiny-file-input .progress {
      height: 8px !important;
    }
    .module-panel-wrapper .shiny-input-container .help-block {
      margin-top: 0.1rem !important;
      font-size: 0.85rem;
    }
    /* Reduce spacing inside card bodies in the left column */
    .module-panel-wrapper > .row > .col-sm-4 .card-body {
      padding: 0.5rem 0.75rem !important;
    }
    /* Tighter margins for paragraphs and lists inside panels */
    .module-panel-wrapper p,
    .module-panel-wrapper ul,
    .module-panel-wrapper ol {
      margin-bottom: 0.3rem !important;
    }
    .module-panel-wrapper hr {
      margin: 0.5rem 0 !important;
    }
  ")),
  
  sidebar = sidebar(
    tags$div(
      class = "sidebar-header d-flex align-items-center",
      style = "margin-bottom: 24px; padding-bottom: 12px; border-bottom: 1px solid #F1F5F9; justify-content: space-between;",
      tags$span("Dashboard", class = "sidebar-title-text", style = "font-weight: 700; font-size: 0.85rem; color: #94A3B8; text-transform: uppercase; letter-spacing: 0.5px;"),
      tags$button(
        id = "sidebar-toggle-btn",
        class = "btn btn-sm btn-light",
        title = "Tampilkan/Sembunyikan Menu", 
        style = "background: transparent; border: none; color: #64748B; padding: 4px 8px; box-shadow: none;",
        icon("bars", class = "fa-fw", style = "font-size: 1.25rem;")
      )
    ),
    
    div(
      class = "dir-chooser-wrapper",
      style = "margin-bottom: 16px; padding: 16px; background-color: #F8FAFC; border-radius: 12px; border: 1px dashed #CBD5E1; transition: all 0.3s ease;",
      tags$label("Direktori Output",
                 style = paste("font-size: 0.8rem; font-weight: 700; color: #64748B;",
                               "margin-bottom: 10px; display: block; text-transform: letter-spacing: 0.5px;")),
      shinyDirButton(
        id    = "btn_browse_output",
        label = "Pilih Folder",
        title = "Pilih Direktori Output",
        icon  = icon("folder-open"),
        class = "btn-light w-100",
        style = "background-color: #FFFFFF; border: 1px solid #E2E8F0; color: #475569; font-weight: 600; border-radius: 8px; text-align: left; box-shadow: 0 1px 2px rgba(0,0,0,0.05);"
      ),
      div(style = "margin-top: 10px;",
          uiOutput("output_dir_status"))
    ),
    
    actionButton("btn_home", 
                 tagList(
                   icon("home", class = "menu-icon fa-fw"),
                   tags$span(class = "menu-text", "Beranda Utama")
                 ),
                 title = "Beranda Utama",
                 class = "btn w-100 d-flex align-items-center",
                 style = "text-align: left; color: #1b75ba; background-color: #eef6fc; border: none; padding: 14px 16px; margin-bottom: 12px; border-radius: 8px;"),
    
    div(
      id = "sidebar_menus",
      style = "display: none;",
      accordion(
        open = FALSE,
        accordion_panel(
          title = acc_title(icon("search", class = "menu-icon fa-fw"), "1. Analisis SERASI"),
          value = "panel_identifikasi",
          nav_item("nav_overlap",         "1.1", "Area Tumpang Tindih"),
          nav_item("nav_adjacent",        "1.2", "Area Bertetangga"),
          shinyjs::hidden(nav_item("nav_interconnection", "1.3", "Area Saling Terhubung"))
        ),
        accordion_panel(
          title = acc_title(icon("chart-line", class = "menu-icon fa-fw"), "2. Analisis PADU"),
          value = "panel_padu",
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
          title = acc_title(icon("scale-balanced", class = "menu-icon fa-fw"), "3. Analisis PADAN"),
          value = "panel_padan",
          nav_item("nav_padan", "3", "Analisis PADAN")
        ),
        accordion_panel(
          title = acc_title(icon("lightbulb", class = "menu-icon fa-fw"), "4. Penyusunan Alternatif"),
          value = "panel_rekomendasi",
          nav_item("nav_recommendation_overlaps", "4.1", "Penyusunan Alternatif Tumpang Tindih"),
          nav_item("nav_recommendation_adjacent", "4.2", "Penyusunan Alternatif Bertetangga")
        ),
        accordion_panel(
          title = acc_title(icon("handshake", class = "menu-icon fa-fw"), "5. Rekonsiliasi"),
          value = "panel_rekonsiliasi",
          nav_item("nav_reconcile", "5", "Rekonsiliasi")
        )
      )
    ),
    div(
      class = "report-sidebar-footer",
      style = "margin-top: 20px; padding-top: 16px; border-top: 1px solid #E2E8F0;",
      actionButton(
        "btn_generate_report",
        tagList(
          icon("file-lines", class = "menu-icon fa-fw"),
          tags$span(class = "menu-text", "Buat Laporan")
        ),
        title = "Buat Laporan",
        class = "btn w-100 d-flex align-items-center",
        style = "text-align: left; color: #1b75ba; background-color: #eef6fc; border: none; padding: 14px 16px; font-weight: 600; border-radius: 8px;"
      ),
      tags$small(
        class = "report-sidebar-subtitle",
        style = "display: block; margin-top: 6px; color: #94A3B8; font-size: 0.75rem;",
        "Hasil dari modul yang sudah dijalankan"
      )
    )
  ),
  
  # ── Modal Konfirmasi Tutup Tab ──────────────────────────────
  tags$div(
    id = "close_confirm_modal", class = "modal fade",
    tabindex = "-1", `data-bs-backdrop` = "static", `data-bs-keyboard` = "false",
    tags$div(class = "modal-dialog modal-dialog-centered",
             tags$div(class = "modal-content", style = "border-radius: 16px; border: none; overflow: hidden; box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.25);",
                      tags$div(class = "modal-header", style = "background-color: #FEF2F2; color: #DC2626; border-bottom: 1px solid #FEE2E2; padding: 20px 24px;",
                               tags$h5(class = "modal-title d-flex align-items-center", style = "font-weight: 700; font-size: 1.1rem;",
                                       icon("triangle-exclamation", class="me-2"),
                                       "Tutup Tab Konfirmasi"),
                               tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal")
                      ),
                      tags$div(class = "modal-body", style = "padding: 24px; color: #475569; font-size: 1.05rem;",
                               tags$p(class = "mb-0",
                                      "Apakah Anda yakin ingin menutup tab ",
                                      tags$strong(id = "modal_tab_label", style = "color: #0F172A;"),
                                      "? Semua perubahan yang belum tersimpan mungkin akan hilang.")
                      ),
                      tags$div(class = "modal-footer", style = "border-top: 1px solid #F1F5F9; padding: 16px 24px; background-color: #F8FAFC;",
                               tags$button(type = "button", class = "btn btn-light", style = "font-weight: 600; color: #64748B; border: 1px solid #E2E8F0;",
                                           `data-bs-dismiss` = "modal", "Batal"),
                               actionButton("confirm_close_yes",
                                            label = "Ya, Tutup Tab",
                                            class = "btn btn-danger", style = "font-weight: 600; background-color: #DC2626; border: none; box-shadow: 0 4px 6px -1px rgba(220, 38, 38, 0.2);")
                      )
             )
    )
  ),
  
  # ── Modal Informasi Konfirmasi Awal ─────────────────────────
  tags$div(
    id = "info_confirm_modal", class = "modal fade",
    tabindex = "-1", `data-bs-backdrop` = "static", `data-bs-keyboard` = "false",
    tags$div(class = "modal-dialog modal-dialog-centered modal-lg",
             tags$div(class = "modal-content", style = "border-radius: 16px; border: none; overflow: hidden; box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.25);",
                      tags$div(class = "modal-header", style = "background-color: #eef6fc; color: #1b75ba; border-bottom: 1px solid #dbeafe; padding: 20px 24px;",
                               tags$h5(class = "modal-title d-flex align-items-center", style = "font-weight: 700; font-size: 1.1rem;",
                                       icon("info-circle", class="me-2"),
                                       tags$span(id = "info_modal_title", "Konfirmasi")),
                               tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal")
                      ),
                      tags$div(class = "modal-body", style = "padding: 24px; color: #1e293b; font-size: 1rem; line-height: 1.6;",
                               tags$div(id = "info_modal_body_text", style = "margin-bottom: 12px;"),
                               tags$p(style = "margin-top: 8px;",
                                      tags$a(id = "info_modal_link", href = "#", target = "_blank", 
                                             style = "font-weight: 600; color: #1b75ba; text-decoration: underline;",
                                             "Pelajari lebih lanjut")
                               )
                      ),
                      tags$div(class = "modal-footer", style = "border-top: 1px solid #E2E8F0; padding: 16px 24px; background-color: #F8FAFC;",
                               tags$button(type = "button", class = "btn btn-light", style = "font-weight: 600; color: #64748B; border: 1px solid #E2E8F0;",
                                           `data-bs-dismiss` = "modal", "Kembali"),
                               actionButton("confirm_info_yes",
                                            label = "Ya, saya mengerti",
                                            class = "btn-primary", style = "font-weight: 600; border: none; box-shadow: 0 4px 6px -1px rgba(27,117,186,0.2);")
                      )
             )
    )
  ),
  
  # ── Tombol Panduan ──────────────────────────────────────────
  tags$a(
    id = "user-guide-link",
    href = "https://laspur.landseascape.id/",
    target = "_blank",
    class = "btn btn-sm",
    style = "display: none; white-space: nowrap; padding: 8px 18px; font-weight: 700; background-color: #eef6fc; color: #1b75ba; border: 1px solid #E2E8F0; border-radius: 8px; align-items: center; gap: 10px; transition: all 0.2s ease;",
    onmouseover = "this.style.backgroundColor='#1b75ba'; this.style.color='#FFFFFF'; this.style.borderColor='#1b75ba';",
    onmouseout = "this.style.backgroundColor='#eef6fc'; this.style.color='#1b75ba'; this.style.borderColor='#E2E8F0';",
    icon("book"),
    span("Panduan Pengguna")
  ),
  
  navset_card_pill(id = "tabs", landing_page)
)

# ── Server ───────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Shared storage for module results ──────────────────────
  session$userData$module_results <- reactiveValues()
  
  open_tabs     <- reactiveVal(character(0))
  pending_close <- reactiveVal(NULL)
  pending_action <- reactiveVal(NULL)   # "overlap" or "adjacent"
  
  active_path   <- reactiveVal("") 
  
  output$active_path_indicator <- renderUI({
    path <- active_path()
    if (path == "") return(NULL)
    
    if (path == "overlap") {
      tags$div(class = "pulse-badge pulse-overlap",
               tags$div(class = "pulse-dot"),
               "Jalur: Tumpang Tindih"
      )
    } else if (path == "adjacent") {
      tags$div(class = "pulse-badge pulse-adjacent",
               tags$div(class = "pulse-dot"),
               "Jalur: Bertetangga"
      )
    }
  })
  
  # ── Logo click handler ──────────────────────────────────────
  shinyjs::onclick("logo_home", {
    updateTabsetPanel(session, "tabs", selected = "home")
  })
  
  # ── Beranda Utama button handler ────────────────────────────
  observeEvent(input$btn_home, {
    updateTabsetPanel(session, "tabs", selected = "home")
  })
  
  tab_state <- new.env(parent = emptyenv())
  tab_state$gen        <- list()
  tab_state$observers <- list()
  
  destroy_tab_observers <- function(tab_id) {
    obs_list <- tab_state$observers[[tab_id]]
    if (!is.null(obs_list)) {
      for (o in obs_list) {
        if (!is.null(o)) o$destroy()
      }
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
    if (closed_any) {
      showNotification(warning_message, type = "warning", duration = 8)
    }
  }
  
  # ── Function to perform the actual path switch ──────────────
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
    # also expand sidebar if mini
    session$sendCustomMessage("expand_sidebar", list())
  }
  
  # ── Observers for the landing page buttons ──────────────────
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
  
  # ── Confirm button on info modal ────────────────────────────
  observeEvent(input$confirm_info_yes, {
    req(!is.null(pending_action()))
    action <- pending_action()
    # hide modal
    session$sendCustomMessage("hide_info_modal", list())
    # perform action
    perform_action(action)
    pending_action(NULL)  # clear after execution
  })
  
  # ── Dismiss modal (via backdrop/close) clears pending action ──
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
    if (!nzchar(path)) return() 
    if (!dir.exists(path)) {
      tryCatch({
        dir.create(path, recursive = TRUE)
        showNotification(paste("Direktori output dibuat:", path), type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Gagal membuat direktori:", e$message), type = "error", duration = 5)
      })
    }
  }, ignoreInit = FALSE)
  
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
  
  # ── Add tab ───────────────────────────────────────────────────
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
      style = "display: flex; justify-content: flex-end; align-items: center; margin-bottom: 12px; padding-bottom: 8px; border-bottom: 1px solid #E2E8F0; gap: 12px;",
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
  
  # ── Helper to create standard dynamic checkbox item ──────────
  make_cb_item <- function(val_id, label_text, is_ready, indent = FALSE) {
    badge <- if (is_ready) {
      tags$span(
        class = "badge ms-2",
        style = "background-color:#106665; font-size:0.7rem; vertical-align:middle;",
        "Siap"
      )
    } else {
      tags$span(
        class = "badge ms-2",
        style = "background-color:#94A3B8; font-size:0.7rem; vertical-align:middle;",
        "Belum dijalankan"
      )
    }
    
    tags$div(
      class = "form-check mb-2",
      style = if (indent) "margin-left: 24px;" else NULL,
      tags$input(
        class    = "form-check-input report-mod-cb",
        type     = "checkbox",
        id       = paste0("cb_mod_", gsub("\\$", "_", val_id)),
        value    = val_id,
        checked  = if (is_ready) NA else NULL,
        disabled = if (!is_ready) NA else NULL
      ),
      tags$label(
        class = "form-check-label",
        `for` = paste0("cb_mod_", gsub("\\$", "_", val_id)),
        style = if (!is_ready) "color:#94A3B8;" else "",
        label_text,
        badge
      )
    )
  }
  
  # ── Helper to create standard dynamic checkbox item ──────────
  make_cb_item <- function(val_id, label_text, is_ready, indent = FALSE, extra_class = "") {
    badge <- if (is_ready) {
      tags$span(
        class = "badge ms-2",
        style = "background-color:#106665; font-size:0.7rem; vertical-align:middle;",
        "Siap"
      )
    } else {
      tags$span(
        class = "badge ms-2",
        style = "background-color:#94A3B8; font-size:0.7rem; vertical-align:middle;",
        "Belum dijalankan"
      )
    }
    
    tags$div(
      class = "form-check mb-2",
      style = if (indent) "margin-left: 24px;" else NULL,
      tags$input(
        class    = trimws(paste("form-check-input report-mod-cb", extra_class)),
        type     = "checkbox",
        id       = paste0("cb_mod_", gsub("\\$", "_", val_id)),
        value    = val_id,
        checked  = if (is_ready) NA else NULL,
        disabled = if (!is_ready) NA else NULL
      ),
      tags$label(
        class = "form-check-label",
        `for` = paste0("cb_mod_", gsub("\\$", "_", val_id)),
        style = if (!is_ready) "color:#94A3B8;" else "",
        label_text,
        badge
      )
    )
  }
  
  # ── Dynamic Report Generation UI ────────────────────────────────
  observeEvent(input$btn_generate_report, {
    if (is.null(output_dir()) || !nzchar(output_dir()) || !validate_output_dir(output_dir())) {
      showNotification(
        "Direktori output belum diatur. Harap pilih folder output terlebih dahulu.",
        type = "error", duration = 5
      )
      return()
    }
    
    check_ready <- function(res_obj) {
      if (is.null(res_obj)) return(FALSE)
      if (is.list(res_obj)) return(length(res_obj) > 0)
      length(res_obj) > 0
    }
    
    mod_ids    <- names(report_module_config)
    choices_ui <- list()
    
    for (m_id in mod_ids) {
      cfg <- report_module_config[[m_id]]
      
      if (!is.null(cfg$label) == FALSE || (is.list(cfg[[1]]) && is.null(cfg$label))) {
        
        any_ready <- any(vapply(names(cfg), function(c_id) {
          check_ready(session$userData$module_results[[m_id]][[c_id]])
        }, logical(1)))
        
        # Render Parent Checkbox 
        choices_ui[[length(choices_ui) + 1]] <- tags$div(
          class = "form-check mt-2 mb-1",
          tags$input(
            class = "form-check-input parent-mod-cb",
            type = "checkbox",
            id = paste0("cb_parent_", m_id),
            `data-target-class` = paste0("child-of-", m_id),
            disabled = if (!any_ready) NA else NULL
          ),
          tags$label(
            class = "form-check-label",
            `for` = paste0("cb_parent_", m_id),
            style = if (!any_ready) "color:#94A3B8;" else "color: #334155;",
            if (m_id == "padu") "Analisis PADU" else toupper(m_id)
          )
        )
        
        # Render Child Checkboxes
        for (child_id in names(cfg)) {
          child_cfg   <- cfg[[child_id]]
          child_res   <- session$userData$module_results[[m_id]][[child_id]]
          is_ready    <- check_ready(child_res)
          val_string  <- paste0(m_id, "$", child_id)
          
          choices_ui[[length(choices_ui) + 1]] <- make_cb_item(
            val_id      = val_string,
            label_text  = child_cfg$label,
            is_ready    = is_ready,
            indent      = TRUE,
            extra_class = paste0("child-of-", m_id) 
          )
        }
      } 
      else {
        res      <- session$userData$module_results[[m_id]]
        is_ready <- check_ready(res)
        
        choices_ui[[length(choices_ui) + 1]] <- make_cb_item(
          val_id     = m_id,
          label_text = cfg$label,
          is_ready   = is_ready,
          indent     = FALSE
        )
      }
    }
    
    # JS to handle Parent/Child toggle and form submission
    sync_js <- tags$script(HTML("
      function updateSelectedModules() {
        var selected = [];
        $('.report-mod-cb:checked').each(function() {
          selected.push($(this).val());
        });
        // Send array to Shiny every time a change happens
        Shiny.setInputValue('report_modules_selected', selected);
      }
  
      // 1. Parent toggles all enabled children
      $(document).off('change', '.parent-mod-cb').on('change', '.parent-mod-cb', function() {
        var isChecked = $(this).is(':checked');
        var targetClass = $(this).attr('data-target-class');
        $('.' + targetClass).not(':disabled').prop('checked', isChecked);
        updateSelectedModules();
      });
  
      // 2. Child unchecks parent if unselected, checks if all are selected
      $(document).off('change', '.report-mod-cb').on('change', '.report-mod-cb', function() {
        var classes = $(this).attr('class').split(' ');
        var parentClass = null;
        for (var i = 0; i < classes.length; i++) {
          if (classes[i].indexOf('child-of-') === 0) {
            parentClass = classes[i];
            break;
          }
        }
        if (parentClass) {
           var allEnabled = $('.' + parentClass).not(':disabled');
           var allChecked = allEnabled.length > 0 && (allEnabled.length === allEnabled.filter(':checked').length);
           $('.parent-mod-cb[data-target-class=\"' + parentClass + '\"]').prop('checked', allChecked);
        }
        updateSelectedModules();
      });
  
      // 3. Initialize state immediately when modal opens
      setTimeout(updateSelectedModules, 100);
    "))
    
    showModal(
      modalDialog(
        title = tagList(
          icon("file-lines", class = "me-2"),
          "Buat Laporan — Pilih Modul"
        ),
        tags$p(
          style = "color:#64748B; font-size:0.9rem; margin-bottom:16px;",
          "Centang modul yang ingin disertakan dalam laporan. Modul yang belum dijalankan tidak dapat dipilih."
        ),
        tags$div(
          style = "padding: 8px 4px; max-height: 400px; overflow-y: auto;",
          if (length(choices_ui) > 0) choices_ui else
            tags$p(class = "text-muted", "Tidak ada modul yang terdaftar.")
        ),
        sync_js,
        footer = tagList(
          modalButton("Batal"),
          actionButton(
            "btn_report_generate",
            tagList(icon("file-export", class = "me-2"), "Buat Laporan"),
            class = "btn-primary",
            style = "font-weight: 600; border: none; box-shadow: 0 4px 6px -1px rgba(27,117,186,0.2);"
          )
        ),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  observeEvent(input$btn_report_generate, {
    selected <- input$report_modules_selected
    
    if (length(selected) == 0) {
      showNotification("Pilih setidaknya satu modul.", type = "warning")
      return()
    }
    
    removeModal()
    n <- length(selected)
    
    withProgress(message = "Membuat laporan...", value = 0, {
      success_count <- 0
      
      for (i in seq_along(selected)) {
        item_path <- selected[[i]]
        
        if (grepl("\\$", item_path)) {
          parts     <- strsplit(item_path, "\\$")[[1]]
          parent    <- parts[1]
          child     <- parts[2]
          
          cfg       <- report_module_config[[parent]][[child]]
          out       <- session$userData$module_results[[parent]][[child]]
          mod_name  <- paste0(parent, "_", child)
        } else {
          cfg       <- report_module_config[[item_path]]
          out       <- session$userData$module_results[[item_path]]
          mod_name  <- item_path
        }
        
        incProgress(
          amount = 1 / n,
          detail = paste0("Modul: ", cfg$label, " (", i, "/", n, ")")
        )
        
        if (is.null(out)) {
          showNotification(
            paste("Hasil untuk modul", item_path, "tidak ditemukan. Dilewati."),
            type = "warning", duration = 6
          )
          next
        }
        
        tryCatch({
          generate_report(
            output        = out,
            dir           = output_dir(),
            module_name   = mod_name,
            template_path = cfg$template
          )
          success_count <- success_count + 1
        }, error = function(e) {
          showNotification(
            paste0("Gagal membuat laporan untuk modul '", item_path, "': ", conditionMessage(e)),
            type = "error", duration = 10
          )
        })
      }
      
      if (success_count > 0) {
        showNotification(
          paste0(success_count, " laporan berhasil dibuat di folder: ", output_dir()),
          type = "message", duration = 7
        )
      }
    })
  })
}

jsCode <- "
$(document).ready(function() {
  $('body').addClass('sidebar-mini'); 

  function addUserGuideButton() {
    if ($('#navbar-user-guide').length) return;
    var btn = $('#user-guide-link');
    if (!btn.length) return;
    btn.attr('id', 'navbar-user-guide');
    btn.css('display', 'inline-flex');
    $('.navbar > .container-fluid').append(
      $('<div>').css({'margin-left':'auto', 'margin-right':'12px'}).append(btn)
    );
  }
  addUserGuideButton();

  // Toggle Mode Sidebar Mini
  $(document).on('click', '#sidebar-toggle-btn', function() {
    $('body').toggleClass('sidebar-mini');
  });

  // ── Expand sidebar (used after confirm) ──
  Shiny.addCustomMessageHandler('expand_sidebar', function(msg) {
    $('body').removeClass('sidebar-mini');
  });

  // ── Collapsible Left Panel ──────────────────────────────
  function injectToggleButtons() {
    $('.module-panel-wrapper').each(function() {
      var $wrapper = $(this);
      var $rightCol = $wrapper.find('> .row > .col-sm-8');
      if (!$rightCol.length) return;

      $rightCol.find('.panel-toggle-container, .panel-toggle-btn').remove();

      var $header = $rightCol.find('.card-header');
      var $btn;

      function createToggleButton(appendTo) {
        $btn = $('<button class=\"panel-toggle-btn\" type=\"button\" title=\"Sembunyikan / Tampilkan Panel Input\">' +
          '<i class=\"bi bi-layout-sidebar-inset-reverse\"></i>' +
          '<span>Perluas</span>' +
          '</button>');
        if (appendTo.is('.card-header')) {
          $btn.css({
            'float': 'right',
            'margin-top': '5px',
            'margin-right': '5px'
          });
        } else {
          var $container = $('<div class=\"panel-toggle-container\" style=\"display: flex; justify-content: flex-end; padding: 8px 16px;\">');
          $container.append($btn);
          appendTo = $container;
          $rightCol.prepend($container);
        }
        appendTo.append($btn);

        $btn.on('click', function(e) {
          e.stopPropagation();
          var $icon  = $(this).find('i');
          var $label = $(this).find('span');
          var isCollapsed = $wrapper.hasClass('panel-collapsed');

          $wrapper.toggleClass('panel-collapsed');

          if (isCollapsed) {
            $icon.removeClass('bi-layout-sidebar-inset').addClass('bi-layout-sidebar-inset-reverse');
            $label.text('Perluas');
          } else {
            $icon.removeClass('bi-layout-sidebar-inset-reverse').addClass('bi-layout-sidebar-inset');
            $label.text('Ringkas');
          }

          setTimeout(function() { $(window).trigger('resize'); }, 380);
        });
      }

      if ($header.length) {
        createToggleButton($header);
      } else {
        createToggleButton($rightCol);
      }
    });
  }

  function attachCloseButtons() {
    $('#tabs.nav-pills .nav-link').each(function() {
      var $link = $(this);
      var tabId = $link.attr('data-value');
      if (tabId && tabId !== 'home' && $link.find('.close-tab-btn').length === 0) {
        var $btn = $('<span class=\"close-tab-btn\" title=\"Tutup Tab\"><i class=\"fa fa-times\"></i></span>');
        $btn.on('click', function(e) {
          e.preventDefault();
          e.stopPropagation(); 
          Shiny.setInputValue('request_close_tab', tabId, {priority: 'event'});
        });
        $link.append($btn);
      }
    });
  }

  attachCloseButtons();
  injectToggleButtons();

  var observer = new MutationObserver(function(mutations) {
    attachCloseButtons();
    setTimeout(injectToggleButtons, 100);
  });

  var targetNode = document.getElementById('tabs');
  if(targetNode) {
    observer.observe(targetNode, { childList: true, subtree: true });
  } else {
    observer.observe(document.body, { childList: true, subtree: true });
  }

  // ── Info Modal ──────────────────────────────────────────────
  Shiny.addCustomMessageHandler('show_info_modal', function(msg) {
    document.getElementById('info_modal_title').innerText = msg.title;
    document.getElementById('info_modal_body_text').innerHTML = msg.body_text;
    var link = document.getElementById('info_modal_link');
    link.href = msg.link_href;
    link.innerText = msg.link_text;
    var modal = new bootstrap.Modal(document.getElementById('info_confirm_modal'));
    modal.show();
  });

  Shiny.addCustomMessageHandler('hide_info_modal', function(msg) {
    var modal = bootstrap.Modal.getInstance(document.getElementById('info_confirm_modal'));
    if (modal) modal.hide();
  });

  // Send signal when modal is hidden (user clicks backdrop, close, or Kembali)
  $('#info_confirm_modal').on('hidden.bs.modal', function() {
    Shiny.setInputValue('info_modal_dismissed', Math.random());
  });

  // ── Close Tab Modal ─────────────────────────────────────────
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