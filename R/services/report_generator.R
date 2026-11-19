# R/services/report_generator.R
# ============================================================
# Report Generator & Markdown Rendering Service
# ============================================================

#' Wrapper around rmarkdown::render with reactive domain protection
#' @export
laspur_render <- function(...) {
  if (requireNamespace("shiny", quietly = TRUE)) {
    shiny::withReactiveDomain(NULL, rmarkdown::render(...))
  } else {
    rmarkdown::render(...)
  }
}

#' Default Bootstrap theme for LaSPUR HTML reports
#' @export
laspur_report_theme <- function() {
  "bootstrap"
}

#' Ensure Pandoc is configured for rmarkdown
#' @keywords internal
.ensure_pandoc_available <- function() {
  tryCatch({
    if (nzchar(Sys.which("pandoc"))) {
      return(invisible(TRUE))  
    }
  }, error = function(e) NULL)
  
  app_root <- tryCatch({
    here::here()
  }, error = function(e) {
    getwd()
  })
  
  pandoc_dir <- file.path(dirname(app_root), "R-Portable/App/pandoc")
  pandoc_exe <- file.path(pandoc_dir, "pandoc.exe")
  
  if (file.exists(pandoc_exe)) {
    tryCatch({
      current_path <- Sys.getenv("PATH")
      Sys.setenv(PATH = paste(pandoc_dir, current_path, sep = .Platform$path.sep))
      Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
      return(invisible(TRUE))
    }, error = function(e) {
      warning("Failed to configure Pandoc: ", conditionMessage(e))
      return(invisible(FALSE))
    })
  }
  
  return(invisible(FALSE))
}

# Auto-configure pandoc on file load
.ensure_pandoc_available()

#' Module File Paths and Result Variable Names
#' @keywords internal
module_file_config <- list(
  serasi = list(
    folder  = "Analisis SERASI",
    rda     = "log/idx_serasi_log.rda",
    png_dir = "log"
  ),
  padu_ke = list(
    folder  = "Analisis PADU-KE",
    gpkg    = "idx_padu_ke.gpkg",
    xlsx    = "idx_padu_ke.xlsx",
    rda     = "log/idx_padu_ke_log.rda",
    png_dir = "log"
  ),
  padu_hs = list(
    folder  = "Analisis PADU-HS",
    gpkg    = "idx_padu_hs.gpkg",
    xlsx    = "idx_padu_hs.xlsx",
    rda     = "log/idx_padu_hs_log.rda",
    png_dir = "log"
  ),
  padu_kl = list(
    folder  = "Analisis PADU-KL",
    gpkg    = "idx_padu_kl.gpkg",
    xlsx    = "idx_padu_kl.xlsx",
    rda     = "log/idx_padu_kl_log.rda",
    png_dir = "log"
  ),
  padu_kh = list(
    folder  = "Analisis PADU-KH",
    gpkg    = "idx_padu_kh.gpkg",
    xlsx    = "idx_padu_kh.xlsx",
    rda     = "log/idx_padu_kh_log.rda",
    png_dir = "log"
  ),
  padu_rtp = list(
    folder  = "Analisis PADU-RTp",
    gpkg    = "idx_padu_rtp.gpkg",
    xlsx    = "idx_padu_rtp.xlsx",
    rda     = "log/idx_padu_rtp_log.rda",
    png_dir = "log"
  ),
  padu_se = list(
    folder  = "Analisis PADU-SE",
    gpkg    = "idx_padu_se.gpkg",
    xlsx    = "idx_padu_se.xlsx",
    rda     = "log/idx_padu_se_log.rda",
    png_dir = "log"
  ),
  padu_ki = list(
    folder  = "Analisis PADU-KI",
    gpkg    = "idx_padu_ki.gpkg",
    xlsx    = "idx_padu_ki.xlsx",
    rda     = "log/idx_padu_ki_log.rda",
    png_dir = "log"
  ),
  padu_combine = list(
    folder  = "Analisis PADU-Kombinasi",
    gpkg    = "idx_padu_combine.gpkg",
    xlsx    = "idx_padu_combine.xlsx",
    rda     = "log/idx_padu_combine_log.rda",
    png_dir = "log"
  ),
  padan = list(
    folder  = "Analisis PADAN",
    gpkg    = "idx_padan.gpkg",
    xlsx    = "idx_padan.xlsx",
    rda     = "log/idx_padan_log.rda",
    png_dir = "log"
  ),
  recommendation = list(
    folder  = "Penyusunan Alternatif",
    png_dir = "log"
  ),
  reconcile = list(
    folder  = "Rekonsiliasi",
    rda     = "log/idx_reconcile_log.rda",
    png_dir = "log"
  )
)

#' Module Result Variable Names
#' @keywords internal
module_result_names <- list(
  serasi              = list(map = "idx_serasi_map",    table = "idx_serasi_table"),
  padu_ke             = list(map = "idx_padu_ke_map",   table = "idx_padu_ke_table"),
  padu_hs             = list(map = "idx_padu_hs_map",   table = "idx_padu_hs_table"),
  padu_kl             = list(map = "idx_padu_kl_map",   table = "idx_padu_kl_table"),
  padu_kh             = list(map = "idx_padu_kh_map",   table = "idx_padu_kh_table"),
  padu_rtp            = list(map = "idx_padu_rtp_map",  table = "idx_padu_rtp_table"),
  padu_se             = list(map = "idx_padu_se_map",   table = "idx_padu_se_table"),
  padu_ki             = list(map = "idx_padu_ki_map",   table = "idx_padu_ki_table"),
  padu_combine        = list(map = "idx_padu_map",      table = "idx_padu_table"),
  padan               = list(map = "idx_padan_map",     table = "idx_padan_table"),
  reconcile           = list(map = "idx_reconcile_map", table = "idx_reconcile_table")
)

#' Special Loading for SERASI Module (Overlap/Adjacent)
#' @keywords internal
load_serasi_from_files <- function(base_dir, cfg) {
  rda_path <- file.path(base_dir, cfg$rda)
  if (!file.exists(rda_path)) return(list(ready = FALSE, data = NULL))
  
  env <- new.env()
  load(rda_path, envir = env)
  inputs <- env$inputs
  case <- inputs$case
  
  case_plural <- paste0(case, "s")
  gpkg_name_plural <- paste0("idx_serasi_", case_plural, ".gpkg")
  xlsx_name_plural <- paste0("idx_serasi_", case_plural, ".xlsx")
  gpkg_path_plural <- file.path(base_dir, gpkg_name_plural)
  xlsx_path_plural <- file.path(base_dir, xlsx_name_plural)
  
  gpkg_name <- paste0("idx_serasi_", case, ".gpkg")
  xlsx_name <- paste0("idx_serasi_", case, ".xlsx")
  gpkg_path <- file.path(base_dir, gpkg_name)
  xlsx_path <- file.path(base_dir, xlsx_name)
  
  if (file.exists(gpkg_path_plural) && file.exists(xlsx_path_plural)) {
    gpkg_path <- gpkg_path_plural
    xlsx_path <- xlsx_path_plural
  } else if (!file.exists(gpkg_path) || !file.exists(xlsx_path)) {
    return(list(ready = FALSE, data = NULL))
  }
  
  png_dir <- file.path(base_dir, cfg$png_dir)
  if (!dir.exists(png_dir) || length(list.files(png_dir, pattern = "\\.png$", ignore.case = TRUE)) == 0) {
    return(list(ready = FALSE, data = NULL))
  }
  
  tryCatch({
    map_obj <- sf::st_read(gpkg_path, quiet = TRUE)
    table_obj <- openxlsx::read.xlsx(xlsx_path)
    names_list <- module_result_names[["serasi"]]
    result <- list()
    result[[names_list$map]] <- map_obj
    result[[names_list$table]] <- table_obj
    
    log_dir <- file.path(base_dir, "log")
    matriks_result <- NULL
    if (dir.exists(log_dir)) {
      candidate_paths <- character(0)
      uploaded_name <- tryCatch(inputs$matriks_serasi_uploaded, error = function(e) NULL)
      if (!is.null(uploaded_name) && length(uploaded_name) == 1 && nzchar(uploaded_name)) {
        candidate_paths <- c(candidate_paths, file.path(log_dir, uploaded_name))
      }
      canonical_name <- sprintf("matriks_serasi_%s_filled.xlsx", case)
      candidate_paths <- c(candidate_paths, file.path(log_dir, canonical_name))
      
      pattern_files <- list.files(log_dir, pattern = "^matriks_serasi.*_filled.*\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
      if (length(pattern_files) > 0) {
        pattern_files <- pattern_files[order(file.info(pattern_files)$mtime, decreasing = TRUE)]
        candidate_paths <- c(candidate_paths, pattern_files)
      }
      fallback_files <- list.files(log_dir, pattern = "^matriks_serasi.*\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
      if (length(fallback_files) > 0) {
        fallback_files <- fallback_files[order(file.info(fallback_files)$mtime, decreasing = TRUE)]
        candidate_paths <- c(candidate_paths, fallback_files)
      }
      
      for (p in candidate_paths) {
        if (!file.exists(p)) next
        parsed <- tryCatch(load_validate_matrix_table(p, title = "serasi"), error = function(e) NULL)
        if (!is.null(parsed)) {
          matriks_result <- parsed
          break
        }
      }
    }
    
    if (!is.null(matriks_result)) {
      result$matriks_serasi <- matriks_result
    }
    
    out <- list(inputs = inputs, result = result)
    return(list(ready = TRUE, data = out, source = "files"))
  }, error = function(e) {
    warning("Failed to load SERASI from files: ", e$message)
    return(list(ready = FALSE, data = NULL))
  })
}

#' Special Loading for Recommendation Module (Overlaps/Adjacent)
#' @keywords internal
load_recommendation_from_files <- function(base_dir, cfg) {
  rda_candidates <- c(
    file.path(base_dir, "log", "idx_alternatives_overlaps.rda"),
    file.path(base_dir, "log", "idx_alternatives_adjacent.rda")
  )
  rda_existing <- rda_candidates[file.exists(rda_candidates)]
  if (length(rda_existing) == 0) return(list(ready = FALSE, data = NULL))
  
  rda_path <- rda_existing[order(file.info(rda_existing)$mtime, decreasing = TRUE)][1]
  
  env <- new.env()
  load(rda_path, envir = env)
  inputs <- env$inputs
  case <- inputs$case
  if (is.null(case) || is.na(case) || !nzchar(case)) {
    case <- if (grepl("overlaps", basename(rda_path), fixed = TRUE)) "overlaps"
    else if (grepl("adjacent", basename(rda_path), fixed = TRUE)) "adjacent"
    else NA_character_
  }
  if (is.na(case)) return(list(ready = FALSE, data = NULL))
  
  gpkg_name <- sprintf("idx_alternatives_%s.gpkg", case)
  xlsx_name <- sprintf("idx_alternatives_%s.xlsx", case)
  gpkg_path <- file.path(base_dir, gpkg_name)
  xlsx_path <- file.path(base_dir, xlsx_name)
  
  if (!file.exists(gpkg_path) || !file.exists(xlsx_path)) {
    return(list(ready = FALSE, data = NULL))
  }
  
  png_dir <- file.path(base_dir, cfg$png_dir)
  if (!dir.exists(png_dir) || length(list.files(png_dir, pattern = "\\.png$", ignore.case = TRUE)) == 0) {
    return(list(ready = FALSE, data = NULL))
  }
  
  tryCatch({
    map_obj   <- sf::st_read(gpkg_path, quiet = TRUE)
    table_obj <- openxlsx::read.xlsx(xlsx_path)
    
    result <- if (identical(case, "overlaps")) {
      list(
        idx_alternative_overlaps_map   = map_obj,
        idx_alternative_overlaps_table = table_obj
      )
    } else {
      list(
        idx_alternative_adjacent_map   = map_obj,
        idx_alternative_adjacent_table = table_obj
      )
    }
    
    out <- list(inputs = inputs, result = result)
    return(list(ready = TRUE, data = out, source = "files"))
  }, error = function(e) {
    warning("Failed to load Recommendation from files: ", e$message)
    return(list(ready = FALSE, data = NULL))
  })
}

#' Special Loading for Reconcile Module (Overlaps/Adjacent)
#' @keywords internal
load_reconcile_from_files <- function(base_dir, cfg) {
  rda_path <- file.path(base_dir, cfg$rda)
  if (!file.exists(rda_path)) return(list(ready = FALSE, data = NULL))
  
  png_dir <- file.path(base_dir, cfg$png_dir)
  if (!dir.exists(png_dir) || length(list.files(png_dir, pattern = "\\.png$", ignore.case = TRUE)) == 0) {
    return(list(ready = FALSE, data = NULL))
  }
  
  gpkg_candidates <- list.files(
    base_dir, pattern = "^rtrwp_terintegrasi_.*\\.gpkg$",
    full.names = TRUE, ignore.case = TRUE
  )
  xlsx_candidates <- list.files(
    base_dir, pattern = "^rtrwp_terintegrasi_.*\\.xlsx$",
    full.names = TRUE, ignore.case = TRUE
  )
  
  if (length(gpkg_candidates) == 0 || length(xlsx_candidates) == 0) {
    return(list(ready = FALSE, data = NULL))
  }
  
  gpkg_path <- gpkg_candidates[order(file.info(gpkg_candidates)$mtime, decreasing = TRUE)][1]
  xlsx_path <- xlsx_candidates[order(file.info(xlsx_candidates)$mtime, decreasing = TRUE)][1]
  
  tryCatch({
    env <- new.env()
    load(rda_path, envir = env)
    inputs <- env$inputs
    
    map_obj   <- sf::st_read(gpkg_path, quiet = TRUE)
    table_obj <- openxlsx::read.xlsx(xlsx_path)
    
    names_list <- module_result_names[["reconcile"]]
    result <- list()
    result[[names_list$map]]   <- map_obj
    result[[names_list$table]] <- table_obj
    
    out <- list(inputs = inputs, result = result)
    return(list(ready = TRUE, data = out, source = "files"))
  }, error = function(e) {
    warning("Failed to load Reconcile from files: ", e$message)
    return(list(ready = FALSE, data = NULL))
  })
}

#' Check Module Readiness and Load Data
#' @export
module_ready_and_data <- function(module_id, output_dir, session) {
  mod_key <- module_id
  if (!is.null(module_id) && grepl("$", module_id, fixed = TRUE)) {
    parts <- strsplit(module_id, "$", fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      mod_key <- parts[2]
    }
  }
  
  if (!is.null(session) && !is.null(session$userData$module_results)) {
    mem_data <- session$userData$module_results[[mod_key]]
    if (!is.null(mem_data) && length(mem_data) > 0) {
      return(list(ready = TRUE, data = mem_data, source = "memory"))
    }
  }
  
  if (is.null(output_dir) || !nzchar(output_dir)) {
    return(list(ready = FALSE, data = NULL, source = NULL))
  }
  
  cfg <- module_file_config[[mod_key]]
  if (is.null(cfg)) {
    return(list(ready = FALSE, data = NULL))
  }
  
  base_dir <- file.path(output_dir, cfg$folder)
  if (!dir.exists(base_dir)) {
    return(list(ready = FALSE, data = NULL))
  }
  
  if (mod_key == "serasi") {
    return(load_serasi_from_files(base_dir, cfg))
  }
  if (mod_key == "reconcile") {
    return(load_reconcile_from_files(base_dir, cfg))
  }
  if (mod_key == "recommendation") {
    return(load_recommendation_from_files(base_dir, cfg))
  }
  
  gpkg_path <- file.path(base_dir, cfg$gpkg)
  xlsx_path <- file.path(base_dir, cfg$xlsx)
  rda_path  <- file.path(base_dir, cfg$rda)
  png_dir   <- file.path(base_dir, cfg$png_dir)
  
  if (!file.exists(gpkg_path) || !file.exists(xlsx_path) || !file.exists(rda_path)) {
    return(list(ready = FALSE, data = NULL))
  }
  if (!dir.exists(png_dir) || length(list.files(png_dir, pattern = "\\.png$", ignore.case = TRUE)) == 0) {
    return(list(ready = FALSE, data = NULL))
  }
  
  tryCatch({
    env <- new.env()
    load(rda_path, envir = env)
    inputs <- env$inputs
    
    map_obj <- sf::st_read(gpkg_path, quiet = TRUE)
    table_obj <- openxlsx::read.xlsx(xlsx_path)
    
    names_list <- module_result_names[[mod_key]]
    if (is.null(names_list)) {
      stop("No variable name mapping for module: ", mod_key)
    }
    result <- list()
    result[[names_list$map]] <- map_obj
    result[[names_list$table]] <- table_obj
    
    if (identical(mod_key, "padu_ke")) {
      log_dir <- file.path(base_dir, "log")
      matriks_result <- NULL
      if (dir.exists(log_dir)) {
        candidate_paths <- character(0)
        uploaded_name <- tryCatch(inputs$matriks_padu_ke_uploaded, error = function(e) NULL)
        if (!is.null(uploaded_name) && length(uploaded_name) == 1 && nzchar(uploaded_name)) {
          candidate_paths <- c(candidate_paths, file.path(log_dir, uploaded_name))
        }
        candidate_paths <- c(candidate_paths, file.path(log_dir, "matriks_padu_ke_filled.xlsx"))
        pattern_files <- list.files(log_dir, pattern = "^matriks_padu_ke.*_filled.*\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
        if (length(pattern_files) > 0) {
          pattern_files <- pattern_files[order(file.info(pattern_files)$mtime, decreasing = TRUE)]
          candidate_paths <- c(candidate_paths, pattern_files)
        }
        for (p in candidate_paths) {
          if (!file.exists(p)) next
          parsed <- tryCatch(load_validate_matrix_table(p, title = "padu_ke"), error = function(e) NULL)
          if (!is.null(parsed)) {
            matriks_result <- parsed
            break
          }
        }
      }
      if (!is.null(matriks_result)) {
        result$matriks_padu_ke <- matriks_result
      }
    }
    
    out <- list(inputs = inputs, result = result)
    return(list(ready = TRUE, data = out, source = "files"))
  }, error = function(e) {
    warning("Failed to load module from files: ", mod_key, " - ", e$message)
    return(list(ready = FALSE, data = NULL))
  })
}

#' Knit an Rmd Module as a Child Document
#' @keywords internal
knit_child_module <- function(template_path, module_params, envir = parent.frame()) {
  abs_path <- here::here(template_path)
  if (!file.exists(abs_path)) {
    return(paste0("\n\n*Template tidak ditemukan: ", template_path, " (at ", abs_path, ")*\n\n"))
  }
  
  lines <- readLines(abs_path, warn = FALSE)
  yaml_start <- which(lines == "---")[1]
  yaml_end <- which(lines == "---")[2]
  
  if (!is.na(yaml_start) && !is.na(yaml_end) && yaml_start < yaml_end) {
    body_lines <- lines[-(yaml_start:yaml_end)]
  } else {
    body_lines <- lines
  }
  
  body_text <- paste(body_lines, collapse = "\n")
  child_env <- new.env(parent = envir)
  child_env$params <- module_params
  
  knitr::knit_child(text = body_text, envir = child_env, quiet = TRUE)
}

#' Generate a Module or Master Report
#' @export
generate_report <- function(output, dir, module_name = NULL,
                            template_path = "report/LaSPUR_SERASI_report_template.Rmd",
                            master_params = NULL) {
  .ensure_pandoc_available()
  
  if (!dir.exists(dir)) {
    stop("Output directory does not exist: ", dir)
  }
  
  test_file <- file.path(dir, ".write_test_tmp")
  tryCatch({
    writeLines("test", test_file)
    unlink(test_file)
  }, error = function(e) {
    stop("Cannot write to output directory. Check disk space and permissions: ", dir)
  })
  
  if (!is.null(master_params)) {
    template_path <- "report/LaSPUR_master_report_template.Rmd"
    if (!file.exists(template_path)) {
      stop("Master template not found: ", template_path)
    }
    
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
    output_file <- paste0("LaSPUR_Master_Report_", timestamp, ".html")
    
    laspur_render(
      input         = template_path,
      output_file   = output_file,
      output_dir    = dir,
      params        = master_params,
      output_options = list(theme = laspur_report_theme())
    )
    return(invisible())
  }
  
  if (is.null(template_path) || !nzchar(template_path) || !file.exists(template_path)) {
    template_path <- tempfile(fileext = ".Rmd")
    writeLines(c(
      "---",
      paste0("title: \"Laporan Modul ", module_name, "\""),
      "output: html_document",
      "params:",
      "  inputs: NA",
      "  result: NA",
      "  module_name: NA",
      "---",
      "",
      "### Laporan Belum Tersedia",
      "",
      "Template laporan spesifik untuk modul ini sedang dalam tahap pengembangan."
    ), template_path)
  }
  
  inputs_payload <- output$inputs
  if (is.null(inputs_payload) || !is.list(inputs_payload)) {
    inputs_payload <- list()
  }
  
  all_params <- list(
    start_time  = Sys.time(),
    end_time    = Sys.time(),
    inputs      = inputs_payload,
    result      = output$result,
    module_name = module_name
  )
  
  declared_params <- tryCatch({
    yml <- rmarkdown::yaml_front_matter(template_path)
    if (!is.null(yml$params) && is.list(yml$params)) names(yml$params) else NULL
  }, error = function(e) NULL)
  
  if (!is.null(declared_params)) {
    report_params <- all_params[intersect(names(all_params), declared_params)]
  } else {
    report_params <- list(inputs = inputs_payload, result = output$result, module_name = module_name)
  }
  
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
  if (!is.null(module_name) && nzchar(module_name)) {
    base_name <- paste0("LaSPUR_", module_name, "_Report_", timestamp)
  } else {
    base_name <- paste0("LaSPUR_Report_", timestamp)
  }
  
  output_file <- paste0(base_name, ".html")
  
  laspur_render(
    input         = template_path,
    output_format = "html_document",
    output_file   = output_file,
    output_dir    = dir,
    params        = report_params,
    knit_root_dir = getwd(),
    quiet         = TRUE
  )
}

#' Plot with automatic key margin expansion
#' @export
plot_with_autokey <- function(obj, start_cm = 5, step = 3, max_cm = 80) {
  width <- start_cm
  repeat {
    result <- tryCatch(
      {
        plot(obj, key.pos = 4, key.width = lcm(width))
        "ok"
      },
      error = function(e) "retry"
    )
    if (result == "ok" || width >= max_cm) break
    width <- width + step
  }
}

#' Encode image to HTML base64 img tag
#' @export
png_to_img_tag <- function(path, max_width = "100%") {
  if (is.null(path) || !file.exists(path)) return(NULL)
  src <- tryCatch({
    if (requireNamespace("base64enc", quietly = TRUE)) {
      paste0("data:image/png;base64,", base64enc::base64encode(path))
    } else {
      path
    }
  }, error = function(e) path)
  htmltools::tags$img(
    src = src,
    style = paste0("max-width: ", max_width,
                   "; height: auto; display: block; margin: 0 auto;")
  )
}

#' Create CSS scroll-snapping carousel for images
#' @export
css_carousel <- function(paths) {
  enc_ok <- requireNamespace("base64enc", quietly = TRUE)
  imgs <- lapply(paths, function(p) {
    src <- if (enc_ok) {
      paste0("data:image/png;base64,", base64enc::base64encode(p))
    } else {
      p
    }
    htmltools::tags$img(
      src = src,
      style = paste(
        "width: 100%; height: auto; display: block;",
        "scroll-snap-align: center; border-radius: 4px;"
      )
    )
  })
  htmltools::tags$div(
    style = paste(
      "display: flex; overflow-x: auto;",
      "scroll-snap-type: x mandatory; gap: 12px; padding: 8px 0;"
    ),
    imgs
  )
}
