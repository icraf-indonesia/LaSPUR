# R/services/storage_service.R
# ============================================================
# Storage Service: Artifact and Directory Management for LaSPUR
# ============================================================

#' Map module keys to standard output subdirectory names
#'
#' @param module_name Character. Key identifying the module.
#' @return Character. Name of the directory under output_dir.
#' @export
storage_module_directory_name <- function(module_name) {
  mapping <- c(
    "serasi"                  = "Analisis SERASI",
    "overlap"                 = "Analisis SERASI",
    "adjacent"                = "Analisis SERASI",
    "padu_ke"                 = "Analisis PADU-KE",
    "padu_hs"                 = "Analisis PADU-HS",
    "padu_kl"                 = "Analisis PADU-KL",
    "padu_kh"                 = "Analisis PADU-KH",
    "padu_rtp"                = "Analisis PADU-RTp",
    "padu_se"                 = "Analisis PADU-SE",
    "padu_ki"                 = "Analisis PADU-KI",
    "padu_combine"            = "Analisis PADU-Kombinasi",
    "padan"                   = "Analisis PADAN",
    "recommendation_overlaps" = "Penyusunan Alternatif",
    "recommendation_adjacent" = "Penyusunan Alternatif",
    "recommendation"          = "Penyusunan Alternatif",
    "reconcile"               = "Rekonsiliasi"
  )
  
  if (module_name %in% names(mapping)) {
    mapping[[module_name]]
  } else {
    paste0("Analisis_", gsub("[^a-zA-Z0-9_-]", "_", module_name))
  }
}

#' Resolve or create the output directory for a specific module
#'
#' @param output_dir Character. Root output directory.
#' @param module_name Character. Module key.
#' @param create Logical. Whether to create directory if it does not exist.
#' @return Character. Full path to the module output directory.
#' @export
storage_get_module_dir <- function(output_dir, module_name, create = TRUE) {
  if (is.null(output_dir) || !nzchar(output_dir)) return(NULL)
  sub_dir <- storage_module_directory_name(module_name)
  target_path <- file.path(output_dir, sub_dir)
  if (create && !dir.exists(target_path)) {
    dir.create(target_path, recursive = TRUE, showWarnings = FALSE)
  }
  target_path
}

#' Save spatial objects and attribute tables in standard GIS and spreadsheet formats
#'
#' @param sf_obj `sf` object to save.
#' @param dir_path Character. Target directory path.
#' @param base_filename Character. Base filename without extension.
#' @param formats Character vector. Subset of c("gpkg", "xlsx", "rds", "shp").
#' @param quiet Logical. Silence output messages.
#' @return Named list of created file paths.
#' @export
storage_save_spatial <- function(sf_obj,
                                 dir_path,
                                 base_filename = "result",
                                 formats = c("gpkg", "xlsx", "rds"),
                                 quiet = TRUE) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  results <- list()
  base_path <- file.path(dir_path, base_filename)
  
  if ("gpkg" %in% formats) {
    gpkg_path <- paste0(base_path, ".gpkg")
    sf::st_write(sf_obj, gpkg_path, delete_dsn = TRUE, quiet = quiet)
    results$gpkg <- gpkg_path
  }
  
  if ("xlsx" %in% formats) {
    xlsx_path <- paste0(base_path, ".xlsx")
    tbl <- sf::st_drop_geometry(sf_obj)
    openxlsx::write.xlsx(tbl, xlsx_path)
    results$xlsx <- xlsx_path
  }
  
  if ("rds" %in% formats) {
    rds_path <- paste0(base_path, ".rds")
    saveRDS(sf_obj, rds_path)
    results$rds <- rds_path
  }
  
  if ("shp" %in% formats) {
    shp_path <- paste0(base_path, ".shp")
    sf::st_write(sf_obj, shp_path, delete_layer = TRUE, quiet = quiet)
    results$shp <- shp_path
  }
  
  results
}

#' Check if a module has generated valid output artifacts in output_dir
#'
#' @param output_dir Character. Root output directory.
#' @param module_name Character. Module key.
#' @return Logical. TRUE if output directory exists and contains expected files.
#' @export
storage_check_module_ready <- function(output_dir, module_name) {
  if (is.null(output_dir) || !nzchar(output_dir) || !dir.exists(output_dir)) return(FALSE)
  mod_dir <- storage_get_module_dir(output_dir, module_name, create = FALSE)
  if (!dir.exists(mod_dir)) return(FALSE)
  
  # Check for common extensions
  files <- list.files(mod_dir, pattern = "\\.(gpkg|shp|xlsx|rds)$", ignore.case = TRUE)
  length(files) > 0
}
