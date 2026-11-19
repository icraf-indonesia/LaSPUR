# tests/testthat/test-06-ui-shell-and-coordinator.R
# ============================================================
# Unit Tests for UI Shell and Workflow Coordinator (Ticket 06)
# ============================================================

testthat::describe("UI Shell Components", {
  
  it("nav_item generates valid shiny HTML tags with required classes and attributes", {
    item <- nav_item("nav_test", "9.9", "Test Nav")
    html_str <- as.character(item)
    
    testthat::expect_true(grepl("wrapper_nav_test", html_str))
    testthat::expect_true(grepl("nav-item-wrapper", html_str))
    testthat::expect_true(grepl("9.9", html_str))
    testthat::expect_true(grepl("Test Nav", html_str))
    testthat::expect_true(grepl("Shiny\\.setInputValue\\(.*nav_test", html_str))
  })
  
  it("acc_title generates valid accordion header tags", {
    header <- acc_title(shiny::icon("map"), "Test Title")
    html_str <- as.character(header)
    
    testthat::expect_true(grepl("acc-title-wrapper", html_str))
    testthat::expect_true(grepl("menu-text", html_str))
    testthat::expect_true(grepl("Test Title", html_str))
  })
  
  it("landing_page is a properly configured tabPanel", {
    testthat::expect_s3_class(landing_page, "shiny.tag")
    testthat::expect_equal(landing_page$attribs$`data-value`, "home")
  })
  
  it("create_ui_shell instantiates the complete application page", {
    shell <- create_ui_shell()
    testthat::expect_true(is.list(shell))
    
    # Check that client JS is attached
    script_attached <- any(sapply(shell$children, function(ch) {
      if (inherits(ch, "shiny.tag")) {
        return(ch$name == "script" && grepl("laspurAutoSizeReactable", as.character(ch)))
      }
      FALSE
    }))
    testthat::expect_true(script_attached)
  })
})

testthat::describe("Workflow Coordinator Configurations", {
  
  it("tab_config contains all 15 expected tabs with valid ui_fn and srv_fn", {
    expected_tabs <- c(
      "overlap", "adjacent", "interconnection",
      "padu_ke", "padu_hs", "padu_kl", "padu_kh",
      "padu_rtp", "padu_se", "padu_ki", "padu_combine",
      "padan", "recommendation_overlaps", "recommendation_adjacent", "reconcile"
    )
    
    testthat::expect_true(all(expected_tabs %in% names(tab_config)))
    
    for (tab_name in expected_tabs) {
      cfg <- tab_config[[tab_name]]
      testthat::expect_true(is.list(cfg), info = tab_name)
      testthat::expect_true(nzchar(cfg$label), info = tab_name)
      testthat::expect_true(is.function(cfg$ui_fn), info = tab_name)
      testthat::expect_true(is.function(cfg$srv_fn), info = tab_name)
    }
  })
  
  it("module_dir_map contains correct directory mappings", {
    testthat::expect_equal(module_dir_map$overlap, "Analisis SERASI")
    testthat::expect_equal(module_dir_map$adjacent, "Analisis SERASI")
    testthat::expect_equal(module_dir_map$padu_ke, "Analisis PADU-KE")
    testthat::expect_equal(module_dir_map$padan, "Analisis PADAN")
    testthat::expect_equal(module_dir_map$recommendation_overlaps, "Penyusunan Alternatif")
    testthat::expect_equal(module_dir_map$reconcile, "Rekonsiliasi")
  })
  
  it("setup_workflow_coordinator is callable as a server coordinator", {
    testthat::expect_true(is.function(setup_workflow_coordinator))
    testthat::expect_equal(names(formals(setup_workflow_coordinator)), c("input", "output", "session"))
  })
})

testthat::describe("Report Selection Modal Components", {
  
  it("report_module_config includes all key report sections", {
    testthat::expect_true(all(c("serasi", "padu", "padan", "recommendation", "reconcile") %in% names(report_module_config)))
    testthat::expect_true(length(report_module_config$padu) == 8)
  })
  
  it("make_cb_item creates checkbox inputs with proper state badges", {
    cb_ready <- make_cb_item("serasi", "Analisis SERASI", is_ready = TRUE)
    html_ready <- as.character(cb_ready)
    testthat::expect_true(grepl("Siap", html_ready))
    testthat::expect_true(grepl("checked", html_ready))
    
    cb_not_ready <- make_cb_item("serasi", "Analisis SERASI", is_ready = FALSE)
    html_not_ready <- as.character(cb_not_ready)
    testthat::expect_true(grepl("Belum dijalankan", html_not_ready))
    testthat::expect_true(grepl("disabled", html_not_ready))
  })
})
