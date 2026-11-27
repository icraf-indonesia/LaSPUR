# Load libraries
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

library(pacman)

pacman::p_load(
  terra, sf, openxlsx, tibble, dplyr, landscapemetrics, purrr, tidyr,
  stringr, exactextractr, here, rmarkdown, kableExtra, DT, units, utils,
  furrr, future, data.table, leaflet, leaflet.extras, htmltools, readxl,
  shiny, shinyjs, shinyFiles, promises, bslib, base64enc, tidyterra,
  slickR, igraph, rhandsontable, visNetwork, reactable
)

laspur_render <- function(...) {
  if (requireNamespace("shiny", quietly = TRUE)) {
    shiny::withReactiveDomain(NULL, rmarkdown::render(...))
  } else {
    rmarkdown::render(...)
  }
}

laspur_report_theme <- function() {
  return("bootstrap")
}


if (!exists("render_loaded_file_bar", mode = "function")) {
  source("R/shared_inputs.R")
}

if (!exists("write_id_group_bipartite_plots", mode = "function")) {
  source("R/visualisation.R")
}

# Ensure Pandoc is configured for rmarkdown
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

.ensure_pandoc_available()

#' Load and Validate Shapefile
#'
#' @description
#' Reads an ESRI Shapefile or GeoPackage and validates its existence, sidecar files,
#' geometry presence/validity, CRS, and attribute completeness.
#' Automatically drops Z/M dimensions to ensure 2D geometry and conditionally repairs invalid topologies.
#'
#' @param shp_path Character. Path to the .shp or .gpkg file.
#'
#' @return An sf object.
#'
#' @importFrom sf st_read st_geometry st_is_valid st_crs st_drop_geometry
#'   st_geometry_type st_zm st_make_valid
#' @importFrom tools file_path_sans_ext
#'
#' @export
load_and_validate_shapefile <- function(shp_path) {
  file_ext <- tolower(tools::file_ext(shp_path))
  
  if (file_ext == "gpkg") {
    # GeoPackage: single file, no sidecar check needed
    if (!file.exists(shp_path)) {
      stop("Missing required files: ", shp_path)
    }
    message(">> Reading GeoPackage: ", shp_path, " ...")
    sf_object <- tryCatch(
      sf::st_read(shp_path, quiet = TRUE),
      error = function(e) stop("Failed to read GeoPackage: ", e$message)
    )
    message("   GeoPackage successfully read.")
  } else {
    # Shapefile: check sidecar components
    required_ext <- c(".shp", ".shx", ".dbf", ".prj")
    base_path <- tools::file_path_sans_ext(shp_path)
    missing_files <- required_ext[!file.exists(paste0(base_path, required_ext))]
    
    if (length(missing_files) > 0) {
      stop("Missing required files: ", paste(missing_files, collapse = ", "))
    }
    
    # Read shapefile
    message(">> Reading shapefile: ", shp_path, " ...")
    sf_object <- tryCatch(
      sf::st_read(shp_path, quiet = TRUE),
      error = function(e) stop("Failed to read shapefile: ", e$message)
    )
    message("   Shapefile successfully read.")
  }
  
  # Check geometry
  message(">> Checking geometry and properties ...")
  if (is.null(sf::st_geometry(sf_object))) {
    stop("Shapefile contains no geometry.")
  }
  
  sf_object <- sf::st_zm(sf_object, drop = TRUE, what = "ZM")
  
  if (nrow(sf_object) <= 5000) {
    invalid_geom <- !sf::st_is_valid(sf_object)
    if (any(invalid_geom, na.rm = TRUE)) {
      warning(sum(invalid_geom, na.rm = TRUE), " invalid geometries detected. ",
              "Automatically repairing using sf::st_make_valid().")
      sf_object <- sf::st_make_valid(sf_object)
    } else {
      message("   All geometries are valid.")
    }
  } else {
    message("   Dataset besar (", nrow(sf_object), " fitur). Validasi topologi penuh ditunda ke tahap analisis.")
  }
  
  # Check CRS
  if (is.na(sf::st_crs(sf_object))) {
    warning("No CRS set. Use sf::st_set_crs() to assign appropriate CRS.")
  } else {
    message("   CRS detected: ", sf::st_crs(sf_object)$input)
  }
  
  # Check attribute table
  message(">> Checking attribute table completeness ...")
  if (ncol(sf_object) <= 1) {
    warning("Attribute table has no columns besides geometry.")
  }
  
  na_counts <- sapply(sf::st_drop_geometry(sf_object), function(col) sum(is.na(col)))
  cols_with_na <- na_counts[na_counts > 0]
  
  if (length(cols_with_na) > 0) {
    warning("NA values in: ", paste(names(cols_with_na), "(", cols_with_na, ")", collapse = ", "))
  } else {
    message("   No NA values found.")
  }
  
  message("\n[DONE] Shapefile validated.",
          "\n  - Features: ", nrow(sf_object),
          "\n  - Columns : ", ncol(sf_object) - 1,
          "\n  - Geometry: ", as.character(sf::st_geometry_type(sf_object, by_geometry = FALSE))
  )
  
  return(sf_object)
}

#' Load and Validate an Excel Table
#'
#' @description
#' Loads an Excel file (.xlsx or .xls) and validates file extension, table
#' structure, and data completeness.
#'
#' @param table_path Character. Path to the Excel file to be loaded.
#'
#' @return A `tibble` object that has been validated and is ready for analysis.
#'
#' @details
#' Validation steps:
#' \enumerate{
#'   \item Checks file extension (.xlsx or .xls)
#'   \item Reads the Excel file using `openxlsx::read.xlsx()`
#'   \item Checks for empty columns, NA values, and duplicate rows
#' }
#'
#' @examples
#' \dontrun{
#' population_data <- load_and_validate_table("data/population_2023.xlsx")
#' head(population_data)
#' }
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom tibble as_tibble
#' @importFrom tools file_ext
#'
#' @export
load_and_validate_table <- function(table_path) {
  # Check file extension
  extension <- tolower(tools::file_ext(table_path))
  if (!extension %in% c("xlsx", "xls")) {
    stop("Invalid extension: '.", extension, "'. Only .xlsx or .xls accepted.")
  }
  if (!file.exists(table_path)) stop("File not found: ", table_path)
  
  # Read table
  message(">> Reading table: ", table_path, " ...")
  raw_table <- tryCatch(
    openxlsx::read.xlsx(table_path),
    error = function(e) stop("Failed to read Excel file: ", e$message)
  )
  
  tbl_object <- tibble::as_tibble(raw_table)
  message("   Table successfully read.")
  
  # Check completeness
  message(">> Checking completeness ...")
  if (ncol(tbl_object) == 0) stop("Table has no columns.")
  if (nrow(tbl_object) == 0) stop("Table has no data rows.")
  
  empty_cols <- sapply(tbl_object, function(col) all(is.na(col)))
  if (any(empty_cols)) {
    warning("Empty columns: ", paste(names(tbl_object)[empty_cols], collapse = ", "))
  }
  
  na_counts <- sapply(tbl_object, function(col) sum(is.na(col)))
  cols_with_na <- na_counts[na_counts > 0 & !empty_cols]
  if (length(cols_with_na) > 0) {
    warning("NA values in: ", paste(names(cols_with_na), "(", cols_with_na, ")", collapse = ", "))
  } else {
    message("   No NA values found.")
  }
  
  n_duplicates <- sum(duplicated(tbl_object))
  if (n_duplicates > 0) {
    warning(n_duplicates, " duplicate rows found. Consider dplyr::distinct().")
  } else {
    message("   No duplicate rows found.")
  }
  
  message("\n[DONE] Table validated.",
          "\n  - Rows   : ", nrow(tbl_object),
          "\n  - Columns: ", ncol(tbl_object)
  )
  
  return(tbl_object)
}

#' Load and Validate a Compatibility Matrix from Excel
#'
#' @description
#' Reads a compatibility matrix from Excel where the first column contains
#' class1 names, the first row contains class2 names, and inner cells contain
#' numeric compatibility values. Converts to long format.
#'
#' @param matrix_path Character. Path to Excel file (.xlsx or .xls).
#' @param title Character. Optional name for the index column (default NULL).
#'
#' @return A tibble with columns: `class1`, `class2`, and `idx_[title]`.
#'
#' @details
#' Validation steps:
#' \enumerate{
#'   \item Checks file extension (.xlsx or .xls)
#'   \item Validates matrix dimensions (minimum 2x2)
#'   \item Checks that row and column headers are non-missing
#'   \item Ensures all inner cells are numeric
#'   \item Converts to long format
#' }
#'
#' @examples
#' \dontrun{
#' compat_long <- load_validate_matrix_table("compatibility_matrix.xlsx")
#' result <- merge_attributes_to_map(overlap_sf, compat_long)
#' }
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr rename
#' @importFrom tools file_ext
#'
#' @export
load_validate_matrix_table <- function(matrix_path, title = NULL) {
  # Check file extension
  extension <- tolower(tools::file_ext(matrix_path))
  if (!extension %in% c("xlsx", "xls")) {
    stop("Invalid extension: '.", extension, "'. Only .xlsx or .xls accepted.")
  }
  if (!file.exists(matrix_path)) stop("File not found: ", matrix_path)
  
  # Read matrix
  message(">> Reading matrix file: ", matrix_path, " ...")
  raw_mat <- tryCatch(
    openxlsx::read.xlsx(matrix_path, colNames = FALSE, rowNames = FALSE),
    error = function(e) stop("Failed to read Excel file: ", e$message)
  )
  
  mat <- as.data.frame(raw_mat, stringsAsFactors = FALSE)
  
  # Check dimensions
  if (nrow(mat) < 2 || ncol(mat) < 2) {
    stop("Matrix must have at least 2 rows and 2 columns (including headers).")
  }
  
  # Extract and validate headers
  class1_names <- mat[-1, 1]
  if (any(is.na(class1_names)) || any(class1_names == "")) {
    stop("First column (class1 names) contains missing or empty values.")
  }
  class1_names <- as.character(class1_names)
  message("   class1 (", length(class1_names), "): ",
          paste(head(class1_names, 3), collapse = ", "),
          ifelse(length(class1_names) > 3, "...", "")
  )
  
  class2_names <- as.character(mat[1, -1])
  if (any(is.na(class2_names)) || any(class2_names == "")) {
    stop("First row (class2 names) contains missing or empty values.")
  }
  message("   class2 (", length(class2_names), "): ",
          paste(head(class2_names, 3), collapse = ", "),
          ifelse(length(class2_names) > 3, "...", "")
  )
  
  # Validate inner matrix (must be numeric)
  inner_mat <- mat[-1, -1, drop = FALSE]
  for (i in seq_len(nrow(inner_mat))) {
    for (j in seq_len(ncol(inner_mat))) {
      val <- inner_mat[i, j]
      if (!is.na(val) && !is.numeric(val)) {
        coerced <- suppressWarnings(as.numeric(val))
        if (is.na(coerced) && !is.na(val)) {
          stop("Non-numeric value at row ", i + 1, ", col ", j + 1, ": '", val, "'")
        } else {
          inner_mat[i, j] <- coerced
        }
      }
    }
  }
  inner_mat <- as.matrix(inner_mat)
  mode(inner_mat) <- "numeric"
  message("   Matrix dimensions: ", nrow(inner_mat), " x ", ncol(inner_mat))
  
  # Convert to long format
  idx_col <- ifelse(is.null(title), "idx_value", paste0("idx_", title))
  df_long <- data.frame(
    class1 = rep(class1_names, times = ncol(inner_mat)),
    class2 = rep(class2_names, each = nrow(inner_mat)),
    value = as.vector(inner_mat)
  )
  names(df_long)[3] <- idx_col
  result <- tibble::as_tibble(df_long)
  
  message("\n[DONE] Matrix converted to long format.",
          "\n  - Total combinations: ", nrow(result),
          "\n  - Unique class1: ", length(unique(result$class1)),
          "\n  - Unique class2: ", length(unique(result$class2))
  )
  
  return(result)
}

#' Load and Validate a GeoTIFF Raster File
#'
#' @description
#' Loads a GeoTIFF file (.tif or .tiff) using `terra::rast()` and performs
#' validation checks on dimensions, CRS, and extent.
#'
#' @param raster_path Character. Path to GeoTIFF file.
#' @param check_crs Logical. If TRUE, checks that CRS is defined. Default TRUE.
#' @param check_extent Logical. If TRUE, validates extent. Default TRUE.
#' @param check_min_layers Integer. Minimum number of layers required. Default 1.
#'
#' @return A `SpatRaster` object from the `terra` package.
#'
#' @details
#' Validation steps:
#' \enumerate{
#'   \item Checks file extension (.tif or .tiff)
#'   \item Reads raster using `terra::rast()`
#'   \item Validates number of layers
#'   \item Optionally checks CRS definition
#'   \item Optionally validates extent (finite, positive dimensions)
#' }
#'
#' @examples
#' \dontrun{
#' dem <- load_and_validate_raster("data/dem.tif")
#' landsat <- load_and_validate_raster("data/landsat.tif", check_crs = TRUE)
#' }
#'
#' @importFrom terra rast crs ext nlyr nrow ncol xres yres
#' @importFrom tools file_ext
#'
#' @export
load_and_validate_raster <- function(raster_path,
                                     check_crs = TRUE,
                                     check_extent = TRUE,
                                     check_min_layers = 1) {
  # Check file extension
  extension <- tolower(tools::file_ext(raster_path))
  if (nzchar(extension) && !extension %in% c("tif", "tiff", "geotiff")) {
    stop("Invalid extension: '.", extension, "'. Only .tif or .tiff accepted.")
  }
  if (!file.exists(raster_path)) stop("File not found: ", raster_path)
  
  # Read raster
  message(">> Reading GeoTIFF: ", raster_path, " ...")
  r <- tryCatch(
    terra::rast(raster_path),
    error = function(e) stop("Failed to read GeoTIFF: ", e$message)
  )
  
  # Check dimensions
  n_layers <- terra::nlyr(r)
  if (n_layers < check_min_layers) {
    stop("Raster has ", n_layers, " layer(s). Minimum required: ", check_min_layers)
  }
  
  # Check CRS
  if (check_crs) {
    crs_info <- terra::crs(r)
    if (is.na(crs_info) || crs_info == "") {
      warning("Raster has no defined CRS.")
    } else {
      message("   CRS: ", crs_info)
    }
  }
  
  # Check extent
  if (check_extent) {
    ext <- terra::ext(r)
    ext_vec <- as.vector(ext)
    if (any(is.na(ext_vec)) || ext$xmin >= ext$xmax || ext$ymin >= ext$ymax) {
      stop("Invalid extent: ", paste(ext[1:4], collapse = ", "))
    } else {
      message("   Extent: xmin=", ext$xmin, ", xmax=", ext$xmax,
              ", ymin=", ext$ymin, ", ymax=", ext$ymax)
    }
  }
  
  # Summary
  message("\n[DONE] GeoTIFF validated.",
          "\n  - Dimensions: ", terra::nrow(r), " rows, ", terra::ncol(r), " cols, ", n_layers, " layers",
          "\n  - Resolution: x=", terra::xres(r), ", y=", terra::yres(r),
          "\n  - File size : ", round(file.info(raster_path)$size / 1024^2, 2), " MB"
  )
  
  return(r)
}

#' Validate and Create Output Directory
#'
#' @description
#' Validates whether an output directory exists, and creates it (recursively)
#' if it does not.
#'
#' @param dir_path Character string. Path to the output directory.
#'
#' @return Logical. `TRUE` if the directory is valid/exists/created,
#'   `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#' validate_output_dir("output/Analisis SERASI")
#' }
#'
#' @export
validate_output_dir <- function(dir_path) {
  if (is.null(dir_path) || dir_path == "") {
    return(FALSE)
  }
  if (!dir.exists(dir_path)) {
    tryCatch({
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  return(TRUE)
}

#' Ensure the Geometry Column is Named "geometry"
#'
#' @description
#' Renames the active simple features geometry column to `"geometry"` if it
#' currently has a different name.
#'
#' @param sf_obj An `sf` object.
#'
#' @return The input `sf` object with its geometry column renamed to
#'   `"geometry"` (if it wasn't already).
#'
#' @importFrom sf st_set_geometry
#'
#' @export
ensure_geometry_name <- function(sf_obj) {
  geom_col <- attr(sf_obj, "sf_column")
  if (!is.null(geom_col) && geom_col != "geometry") {
    names(sf_obj)[names(sf_obj) == geom_col] <- "geometry"
    sf_obj <- sf::st_set_geometry(sf_obj, "geometry")
  }
  return(sf_obj)
}

#' Normalize Legacy (Un-namespaced) Adjacent-Map IDs
#'
#' @description
#' Before the layer-namespacing fix, `identify_adjacent()` assigned
#' `id_SRC = row_number()` independently to RTRW and RZWP3K, so RTRW
#' feature 5 and RZWP3K feature 5 shared the integer key `5`. Saved
#' gpkg files produced by that version carry these colliding ids and
#' will produce incorrect group assignments in `identify_adjacent_group()`.
#'
#' This helper detects feature-level adjacent maps whose `id` column is
#' purely integer and prefixes each id with `"R"` or `"Z"` based on
#' which side (RTRW or RZWP3K) the row belongs to. Maps that are already
#' namespaced, or that lack an `id` column, or that are not in the
#' feature-level adjacent form (2 rows per `id_pu`) are returned unchanged.
#'
#' @param sf_obj An `sf` object (or `data.frame`) with an `id` column.
#' @param id_col Name of the id column (default `"id"`).
#' @return The input object, with namespaced ids in `id_col` when applicable.
#' @export
normalize_legacy_ids <- function(sf_obj, id_col = "id") {
  if (is.null(sf_obj)) return(sf_obj)
  if (!id_col %in% names(sf_obj)) return(sf_obj)
  if (!all(c("RTRW", "RZWP3K", "id_pu") %in% names(sf_obj))) return(sf_obj)
  
  ids_chr <- as.character(sf_obj[[id_col]])
  non_na  <- ids_chr[!is.na(ids_chr)]
  if (length(non_na) == 0) return(sf_obj)
  
  # Already namespaced? ("R<digits>" or "Z<digits>")
  if (any(grepl("^[RZ][0-9]+$", non_na))) return(sf_obj)
  
  # Only proceed if ALL values are pure integers (legacy format)
  if (!all(grepl("^[0-9]+$", non_na))) return(sf_obj)
  
  # Only proceed if every id_pu has exactly 2 rows (feature-level adjacent form)
  pu_counts <- table(table(sf_obj$id_pu))
  if (length(pu_counts) != 1 || names(pu_counts)[1] != "2") return(sf_obj)
  
  is_rtrw <- !is.na(sf_obj$RTRW)
  is_rzwp <- !is.na(sf_obj$RZWP3K)
  
  # Each pair must have exactly one RTRW row and one RZWP3K row
  if (!all(xor(is_rtrw, is_rzwp))) return(sf_obj)
  
  new_ids <- ids_chr
  new_ids[is_rtrw] <- paste0("R", ids_chr[is_rtrw])
  new_ids[is_rzwp] <- paste0("Z", ids_chr[is_rzwp])
  sf_obj[[id_col]] <- new_ids
  sf_obj
}

#' Generate HTML Popup Table for Overlay Vector Features
#'
#' @description
#' Builds an HTML key-value table string for each feature in an `sf` object,
#' enabling attribute inspection on Leaflet click.
#'
#' @param sf_obj An `sf` object with attribute columns.
#' @return A character vector of HTML popup strings corresponding to each row.
#'
#' @importFrom htmltools htmlEscape HTML
#' @export
generate_overlay_popups <- function(sf_obj) {
  df_attr <- sf::st_drop_geometry(sf_obj)
  n <- nrow(sf_obj)
  if (n == 0) return(character(0))
  if (ncol(df_attr) == 0) {
    return(rep("<div style='padding: 6px; font-family: sans-serif; font-size: 12px;'><em>Tidak ada atribut</em></div>", n))
  }
  
  col_names <- colnames(df_attr)
  popups <- vector("character", n)
  
  for (i in seq_len(n)) {
    rows_html <- character(0)
    for (col in col_names) {
      val <- df_attr[[col]][i]
      if (is.null(val) || length(val) == 0 || is.na(val)) {
        val_str <- "-"
      } else if (is.numeric(val)) {
        val_str <- as.character(round(val, 3))
      } else {
        val_str <- htmltools::htmlEscape(as.character(val))
      }
      rows_html <- c(rows_html, paste0(
        "<tr style='border-bottom: 1px solid #f1f5f9;'>",
        "<td style='padding: 4px 8px; font-weight: 600; color: #334155;'>", htmltools::htmlEscape(col), "</td>",
        "<td style='padding: 4px 8px; color: #1e293b;'>", val_str, "</td>",
        "</tr>"
      ))
    }
    
    popups[i] <- paste0(
      "<div style='max-height: 240px; overflow-y: auto; font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, sans-serif; font-size: 11px;'>",
      "<table style='width: 100%; border-collapse: collapse; text-align: left;'>",
      "<thead><tr style='background-color: #f8fafc; border-bottom: 2px solid #e2e8f0;'>",
      "<th style='padding: 5px 8px; color: #64748b; font-size: 10px; text-transform: uppercase;'>Atribut</th>",
      "<th style='padding: 5px 8px; color: #64748b; font-size: 10px; text-transform: uppercase;'>Nilai</th>",
      "</tr></thead>",
      "<tbody>", paste(rows_html, collapse = ""), "</tbody>",
      "</table></div>"
    )
  }
  popups
}

#' Distinct Contrast Palette for Vector Overlay Layers
#' @export
OVERLAY_CONTRAST_PALETTE <- c("#2563EB", "#DC2626", "#16A34A", "#D97706", "#9333EA", "#0891B2", "#EA580C")

#' Process Uploaded Vector Dataset for Map Overlay
#'
#' @description
#' Validates and processes uploaded Shapefiles (requiring `.shp`, `.shx`, `.dbf`, `.prj`)
#' or GeoPackages into an EPSG:4326 `sf` object ready for Leaflet canvas overlay.
#' Reprojects to EPSG:4326, repairs invalid geometries via `st_make_valid()`,
#' drops Z/M dimensions, applies geometry simplification for dense datasets (> 500 features
#' or > 5000 vertices), and generates interactive popup tables.
#'
#' @param file_df A dataframe returned by Shiny `fileInput` containing columns `name` and `datapath`.
#' @param simplify_tolerance Numeric. Simplification tolerance (in map units / meters) applied
#'   to dense geometries (> 500 features or > 5000 vertices). Default `5`.
#' @param color Character. Optional hex color code. If `NULL`, automatically cycles through
#'   `OVERLAY_CONTRAST_PALETTE` based on `existing_count`.
#' @param existing_count Integer. Number of existing vector overlay layers for color cycling. Default `0`.
#' @param layer_id Character. Optional deterministic layer ID for testing. Defaults to timestamp + random hash.
#' @param z_index Integer. Visual stacking index for layer. Default `1`.
#'
#' @return A standardized list with `id`, `name`, `type = "vector"`, `data` (sf object in EPSG:4326),
#'   `popups` (HTML character vector), `bbox` (named numeric vector), `color`, `visible`, `opacity`, and `z_index`.
#'
#' @importFrom sf st_read st_geometry st_zm st_is_valid st_make_valid st_crs st_transform st_bbox st_simplify st_coordinates st_is_longlat
#' @export
process_overlay_vector <- function(file_df, simplify_tolerance = 5, color = NULL, existing_count = 0, layer_id = NULL, z_index = 1) {
  if (!is.data.frame(file_df) || nrow(file_df) == 0 || !all(c("name", "datapath") %in% names(file_df))) {
    stop("Input file vektor tidak valid.")
  }
  
  shp_idx <- grep("\\.shp$", file_df$name, ignore.case = TRUE)
  gpkg_idx <- grep("\\.gpkg$", file_df$name, ignore.case = TRUE)
  
  if (length(shp_idx) == 0 && length(gpkg_idx) == 0) {
    stop("Komponen .shp tidak ditemukan. Unggah file Shapefile (.shp, .shx, .dbf, .prj) atau GeoPackage (.gpkg).")
  }
  
  if (length(shp_idx) > 0) {
    shp_row <- file_df[shp_idx[1], ]
    stem_name <- tools::file_path_sans_ext(shp_row$name)
    
    # Check sidecars matching the stem name
    matching_files <- file_df$name[tolower(tools::file_path_sans_ext(file_df$name)) == tolower(stem_name)]
    present_exts <- tolower(paste0(".", tools::file_ext(matching_files)))
    required_exts <- c(".shp", ".shx", ".dbf", ".prj")
    missing_exts <- setdiff(required_exts, present_exts)
    
    if (length(missing_exts) > 0) {
      stop("Komponen shapefile tidak lengkap. File yang dibutuhkan hilang: ", paste(missing_exts, collapse = ", "))
    }
    
    # Stage into temporary directory
    stage_dir <- tempfile(pattern = "overlay_vec_stage_")
    dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
    for (i in seq_len(nrow(file_df))) {
      dest_file <- file.path(stage_dir, file_df$name[i])
      file.copy(file_df$datapath[i], dest_file, overwrite = TRUE)
    }
    
    target_shp <- file.path(stage_dir, shp_row$name)
    sf_obj <- tryCatch(
      sf::st_read(target_shp, quiet = TRUE),
      error = function(e) stop("Gagal membaca shapefile: ", conditionMessage(e))
    )
  } else {
    gpkg_row <- file_df[gpkg_idx[1], ]
    stem_name <- tools::file_path_sans_ext(gpkg_row$name)
    sf_obj <- tryCatch(
      sf::st_read(gpkg_row$datapath, quiet = TRUE),
      error = function(e) stop("Gagal membaca GeoPackage: ", conditionMessage(e))
    )
  }
  
  if (is.null(sf::st_geometry(sf_obj)) || nrow(sf_obj) == 0) {
    stop("Dataset vektor tidak memiliki fitur atau geometri valid.")
  }
  
  # Ensure 2D geometry (drop Z and M coordinates)
  sf_obj <- sf::st_zm(sf_obj, drop = TRUE, what = "ZM")
  
  # Validate and repair geometries
  invalid_mask <- !sf::st_is_valid(sf_obj)
  if (any(invalid_mask, na.rm = TRUE)) {
    s2_was_on <- sf::sf_use_s2()
    if (isTRUE(s2_was_on)) sf::sf_use_s2(FALSE)
    sf_obj <- sf::st_make_valid(sf_obj)
    if (isTRUE(s2_was_on)) sf::sf_use_s2(TRUE)
  }
  
  # Coordinate Reference System verification and reprojection
  if (is.na(sf::st_crs(sf_obj))) {
    stop("Dataset vektor tidak memiliki sistem koordinat (CRS) terdefinisi.")
  }
  
  # Apply geometry simplification for dense geometries (> 500 features or > 5000 vertices)
  num_features <- nrow(sf_obj)
  num_vertices <- tryCatch(nrow(sf::st_coordinates(sf_obj)), error = function(e) 0)
  
  if ((num_features > 500 || num_vertices > 5000) && !is.null(simplify_tolerance) && simplify_tolerance > 0) {
    tol <- if (isTRUE(sf::st_is_longlat(sf_obj)) && simplify_tolerance >= 0.5) {
      simplify_tolerance / 111320
    } else {
      simplify_tolerance
    }
    sf_obj <- tryCatch({
      simp <- sf::st_simplify(sf_obj, dTolerance = tol, preserveTopology = TRUE)
      if (any(!sf::st_is_valid(simp))) {
        simp <- sf::st_make_valid(simp)
      }
      simp
    }, error = function(e) sf_obj)
  }
  
  crs_epsg <- sf::st_crs(sf_obj)$epsg
  if (is.null(crs_epsg) || is.na(crs_epsg) || crs_epsg != 4326) {
    sf_obj <- sf::st_transform(sf_obj, 4326)
  }
  
  popups <- generate_overlay_popups(sf_obj)
  
  bb <- sf::st_bbox(sf_obj)
  bbox <- c(
    xmin = unname(bb["xmin"]),
    ymin = unname(bb["ymin"]),
    xmax = unname(bb["xmax"]),
    ymax = unname(bb["ymax"])
  )
  
  final_id <- if (!is.null(layer_id) && nzchar(layer_id)) {
    layer_id
  } else {
    paste0("overlay_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
  }
  
  layer_color <- if (!is.null(color) && nzchar(color)) {
    color
  } else {
    idx <- if (is.numeric(existing_count)) (as.integer(existing_count) %% length(OVERLAY_CONTRAST_PALETTE)) + 1 else 1
    OVERLAY_CONTRAST_PALETTE[idx]
  }
  
  list(
    id = final_id,
    name = stem_name,
    type = "vector",
    data = sf_obj,
    popups = popups,
    bbox = bbox,
    color = layer_color,
    visible = TRUE,
    opacity = 0.7,
    z_index = z_index
  )
}

#' Process Uploaded Raster Dataset for Map Overlay
#'
#' @description
#' Validates and processes uploaded GeoTIFF files into an EPSG:4326 `SpatRaster`
#' ready for Leaflet canvas overlay. Automatically downsamples rasters exceeding
#' 1500 pixels along either dimension to prevent browser freezing, and prepares
#' a continuous Viridis color palette.
#'
#' @param file_df A dataframe returned by Shiny `fileInput` containing columns `name` and `datapath`.
#' @return A standardized list with `id`, `name`, `type = "raster"`, `data` (SpatRaster in EPSG:4326),
#'   `pal` (colorNumeric palette function), `domain` (numeric range), and `bbox` (named numeric vector).
#'
#' @importFrom terra rast crs ext same.crs project aggregate values
#' @importFrom leaflet colorNumeric
#' @export
process_overlay_raster <- function(file_df, layer_id = NULL, z_index = 1) {
  if (!is.data.frame(file_df) || nrow(file_df) == 0 || !all(c("name", "datapath") %in% names(file_df))) {
    stop("Input file raster tidak valid.")
  }
  
  tif_idx <- grep("\\.(tif|tiff)$", file_df$name, ignore.case = TRUE)
  if (length(tif_idx) == 0) {
    stop("File raster harus berformat GeoTIFF (.tif atau .tiff).")
  }
  
  tif_row <- file_df[tif_idx[1], ]
  stem_name <- tools::file_path_sans_ext(tif_row$name)
  
  # Stage to clean file with correct extension
  stage_tif <- tempfile(pattern = "overlay_rast_stage_", fileext = paste0(".", tools::file_ext(tif_row$name)))
  file.copy(tif_row$datapath, stage_tif, overwrite = TRUE)
  
  r <- tryCatch(
    terra::rast(stage_tif),
    error = function(e) stop("Gagal membaca file raster: ", conditionMessage(e))
  )
  
  # Check CRS
  r_crs <- terra::crs(r)
  if (is.na(r_crs) || r_crs == "") {
    stop("File raster tidak memiliki sistem koordinat (CRS) terdefinisi.")
  }
  
  # Check extent
  ext_r <- terra::ext(r)
  if (is.null(ext_r) || any(is.na(as.vector(ext_r)))) {
    stop("File raster tidak memiliki extent spasial yang valid.")
  }
  
  # Reproject to EPSG:4326 if not already
  if (!terra::same.crs(r, "EPSG:4326")) {
    r <- terra::project(r, "EPSG:4326")
  }
  
  # Downsample/resample if dimensions exceed 1500 px
  max_dim <- max(nrow(r), ncol(r))
  if (max_dim > 1500) {
    fact <- ceiling(max_dim / 1500)
    if (fact > 1) {
      r <- terra::aggregate(r, fact = fact, fun = "mean", na.rm = TRUE)
    }
  }
  
  # Prepare Viridis palette and domain
  vals <- terra::values(r, mat = FALSE)
  vals <- vals[!is.na(vals) & !is.nan(vals) & !is.infinite(vals)]
  domain <- if (length(vals) > 0) range(vals) else c(0, 1)
  pal <- leaflet::colorNumeric(palette = "viridis", domain = domain, na.color = "transparent")
  
  # Compute bounding box
  ext_v <- as.vector(terra::ext(r))
  bbox <- c(
    xmin = unname(ext_v["xmin"]),
    ymin = unname(ext_v["ymin"]),
    xmax = unname(ext_v["xmax"]),
    ymax = unname(ext_v["ymax"])
  )
  
  final_id <- if (!is.null(layer_id) && nzchar(layer_id)) {
    layer_id
  } else {
    paste0("overlay_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
  }
  
  list(
    id = final_id,
    name = stem_name,
    type = "raster",
    data = r,
    pal = pal,
    domain = domain,
    bbox = bbox,
    visible = TRUE,
    opacity = 0.7,
    color = NULL,
    z_index = z_index
  )
}

#' Mutate Single Layer in Overlay Stack
#'
#' @description
#' Finds a layer by `layer_id` in `layers` and applies `mutate_fn(layer)` to it,
#' returning the updated `layers` list. If the layer is not found or inputs are invalid,
#' returns `layers` unchanged.
#'
#' @param layers List of overlay layer objects.
#' @param layer_id Character. ID of the layer to mutate.
#' @param mutate_fn Function taking a single layer list and returning the modified layer list.
#' @return Updated list of overlay layers.
#' @export
overlay_mutate_layer <- function(layers, layer_id, mutate_fn) {
  if (!is.list(layers) || length(layers) == 0) return(layers)
  if (!is.function(mutate_fn)) return(layers)
  
  for (i in seq_along(layers)) {
    if (identical(layers[[i]]$id, layer_id)) {
      layers[[i]] <- mutate_fn(layers[[i]])
      break
    }
  }
  layers
}

#' Update Overlay Layer Opacity
#'
#' @description
#' Updates the opacity of a target layer in the overlay layer list.
#' Automatically clamps the opacity value to between 0.0 and 1.0.
#'
#' @param layers List of overlay layer objects.
#' @param layer_id Character. ID of the layer to update.
#' @param opacity Numeric. Desired opacity value between 0.0 and 1.0.
#' @return Updated list of overlay layers.
#' @export
overlay_update_layer_opacity <- function(layers, layer_id, opacity) {
  if (is.null(opacity) || is.na(opacity)) return(layers)
  op <- min(max(as.numeric(opacity), 0.0), 1.0)
  overlay_mutate_layer(layers, layer_id, function(layer) {
    layer$opacity <- op
    layer
  })
}

#' Toggle or Set Overlay Layer Visibility
#'
#' @description
#' Toggles or explicitly sets the visibility flag of a target layer in the
#' overlay layer list.
#'
#' @param layers List of overlay layer objects.
#' @param layer_id Character. ID of the layer to toggle or set.
#' @param visible Optional logical. If `NULL`, toggles current visibility. If logical, sets explicitly.
#' @return Updated list of overlay layers.
#' @export
overlay_toggle_layer_visibility <- function(layers, layer_id, visible = NULL) {
  overlay_mutate_layer(layers, layer_id, function(layer) {
    if (is.null(visible)) {
      cur_vis <- if (is.null(layer$visible)) TRUE else isTRUE(layer$visible)
      layer$visible <- !cur_vis
    } else {
      layer$visible <- isTRUE(visible)
    }
    layer
  })
}

#' Update Overlay Layer Color
#'
#' @description
#' Updates the display color of a target vector layer in the overlay layer list.
#'
#' @param layers List of overlay layer objects.
#' @param layer_id Character. ID of the layer to update.
#' @param color Character. Hex color code string (e.g. `"#EF4444"`).
#' @return Updated list of overlay layers.
#' @export
overlay_update_layer_color <- function(layers, layer_id, color) {
  if (is.null(color) || !nzchar(color)) return(layers)
  overlay_mutate_layer(layers, layer_id, function(layer) {
    layer$color <- color
    layer
  })
}

#' Update Z-Indices for Overlay Layers
#'
#' @description
#' Assigns explicit z-index values to layers in the stack. Layer at index 1
#' (top of list) receives the highest z-index value (`length(layers)`), while the
#' last layer receives index 1.
#'
#' @param layers List of overlay layer objects.
#' @return Updated list of overlay layers with sequential `z_index` fields.
#' @export
overlay_update_z_indices <- function(layers) {
  if (!is.list(layers) || length(layers) == 0) return(layers)
  n <- length(layers)
  for (i in seq_along(layers)) {
    layers[[i]]$z_index <- n - i + 1
  }
  layers
}

#' Reorder Overlay Layers (Stacking / Z-Index)
#'
#' @description
#' Moves a layer up or down in the layer stack. Moving "up" moves the layer
#' earlier in the list (towards index 1), while "down" moves it later in the list.
#' Layers earlier in the list are rendered with higher visual Z-index (drawn on top).
#'
#' @param layers List of overlay layer objects.
#' @param layer_id Character. ID of the layer to reorder.
#' @param direction Character. Either `"up"` or `"down"`.
#' @return Updated list of overlay layers with reordered elements and updated `z_index`.
#' @export
overlay_reorder_layer <- function(layers, layer_id, direction = c("up", "down")) {
  direction <- match.arg(direction)
  if (!is.list(layers) || length(layers) <= 1) return(layers)
  
  idx <- which(vapply(layers, function(l) identical(l$id, layer_id), logical(1)))
  if (length(idx) == 0) return(layers)
  
  i <- idx[1]
  if (direction == "up" && i > 1) {
    tmp <- layers[[i - 1]]
    layers[[i - 1]] <- layers[[i]]
    layers[[i]] <- tmp
  } else if (direction == "down" && i < length(layers)) {
    tmp <- layers[[i + 1]]
    layers[[i + 1]] <- layers[[i]]
    layers[[i]] <- tmp
  }
  overlay_update_z_indices(layers)
}

#' Remove Layer from Overlay Stack
#'
#' @description
#' Removes an overlay layer by ID from the layer stack.
#'
#' @param layers List of overlay layer objects.
#' @param layer_id Character. ID of the layer to remove.
#' @return Updated list of overlay layers without the specified layer.
#' @export
overlay_remove_layer <- function(layers, layer_id) {
  if (!is.list(layers) || length(layers) == 0) return(layers)
  res <- Filter(function(l) !identical(l$id, layer_id), layers)
  overlay_update_z_indices(res)
}

#' Extract and Validate Bounding Box for "Zoom to Layer"
#'
#' @description
#' Extracts and validates the geographic bounding box of an overlay layer
#' suitable for Leaflet `fitBounds`.
#'
#' @param layer A standardized overlay layer list.
#' @return A named numeric vector `c(xmin, ymin, xmax, ymax)` or `NULL` if invalid.
#' @export
overlay_get_layer_bbox <- function(layer) {
  if (is.null(layer) || is.null(layer$bbox)) return(NULL)
  bb <- layer$bbox
  req_names <- c("xmin", "ymin", "xmax", "ymax")
  if (!all(req_names %in% names(bb))) return(NULL)
  if (any(is.na(bb[req_names]))) return(NULL)
  bb[req_names]
}

#' Add Single Overlay Layer to Leaflet Map or LeafletProxy
#'
#' @description
#' Renders a vector or raster overlay layer onto a Leaflet map or
#' LeafletProxy object, respecting its visibility, opacity, and color settings.
#'
#' @param map_or_proxy A `leaflet` map or `leafletProxy` object.
#' @param layer A standardized overlay layer list.
#' @return The modified map or proxy object.
#'
#' @importFrom sf st_geometry_type
#' @importFrom leaflet addCircleMarkers addPolylines addPolygons addRasterImage
#' @export
add_overlay_layer_to_map <- function(map_or_proxy, layer) {
  if (is.null(map_or_proxy) || is.null(layer)) return(map_or_proxy)
  
  is_vis <- if (is.null(layer$visible)) TRUE else isTRUE(layer$visible)
  if (!is_vis) return(map_or_proxy)
  
  opacity <- if (!is.null(layer$opacity) && is.numeric(layer$opacity)) {
    min(max(as.numeric(layer$opacity), 0.0), 1.0)
  } else {
    0.7
  }
  stroke_opacity <- if (opacity <= 0) 0 else min(1, opacity + 0.2)
  
  if (identical(layer$type, "vector")) {
    col <- if (!is.null(layer$color) && nzchar(layer$color)) layer$color else "#2563EB"
    geom_type <- unique(as.character(sf::st_geometry_type(layer$data)))
    is_line <- any(grepl("LINE", geom_type, ignore.case = TRUE))
    is_point <- any(grepl("POINT", geom_type, ignore.case = TRUE))
    
    if (is_point) {
      map_or_proxy <- leaflet::addCircleMarkers(
        map = map_or_proxy,
        data = layer$data,
        group = layer$id,
        popup = layer$popups,
        color = col,
        fillColor = col,
        fillOpacity = opacity,
        opacity = stroke_opacity,
        radius = 5,
        weight = 2
      )
    } else if (is_line) {
      map_or_proxy <- leaflet::addPolylines(
        map = map_or_proxy,
        data = layer$data,
        group = layer$id,
        popup = layer$popups,
        color = col,
        weight = 3,
        opacity = opacity,
        highlightOptions = leaflet::highlightOptions(
          weight = 3,
          color = "#FFFFFF",
          fillOpacity = min(1, opacity + 0.3),
          bringToFront = TRUE
        )
      )
    } else {
      map_or_proxy <- leaflet::addPolygons(
        map = map_or_proxy,
        data = layer$data,
        group = layer$id,
        popup = layer$popups,
        color = col,
        fillColor = col,
        fillOpacity = opacity,
        weight = 2,
        opacity = stroke_opacity,
        highlightOptions = leaflet::highlightOptions(
          weight = 3,
          color = "#FFFFFF",
          fillOpacity = min(1, opacity + 0.3),
          bringToFront = TRUE
        )
      )
    }
  } else if (identical(layer$type, "raster")) {
    map_or_proxy <- leaflet::addRasterImage(
      map = map_or_proxy,
      x = layer$data,
      colors = layer$pal,
      opacity = opacity,
      group = layer$id
    )
  }
  
  return(map_or_proxy)
}

#' Draw Overlay Stack on Leaflet Map or LeafletProxy in Z-Index Order
#'
#' @description
#' Draws a list of overlay layers onto a Leaflet map or LeafletProxy object.
#' Draws in reverse order (`rev(layers)`), so that the layer at index 1 in the list
#' is rendered last on the canvas and sits visually on top.
#'
#' @param map_or_proxy A `leaflet` map or `leafletProxy` object.
#' @param layers List of standardized overlay layer objects.
#' @return The modified map or proxy object.
#' @export
draw_overlay_stack <- function(map_or_proxy, layers) {
  if (is.null(map_or_proxy) || is.null(layers) || length(layers) == 0) return(map_or_proxy)
  
  # Reverse order: layer at index 1 is drawn last (sits visually on top)
  for (layer in rev(layers)) {
    map_or_proxy <- add_overlay_layer_to_map(map_or_proxy, layer)
  }
  return(map_or_proxy)
}

#' Create Result Visualization UI
#'
#' @description
#' Builds the shared Shiny UI used across modules to display analysis results:
#' a Leaflet map, a DT table, a validation log, and download buttons.
#'
#' @param ns Namespace function of the module calling this helper.
#' @param extra_tab Optional `nav_panel` to insert between the main results and the log.
#'
#' @return A Shiny UI object containing the map, table, log, and download buttons.
#'
#' @importFrom shiny tagList fluidRow column div hr downloadButton actionButton fileInput uiOutput tags icon
#' @importFrom bslib navset_tab nav_panel
#' @importFrom leaflet leafletOutput
#' @importFrom DT DTOutput
#'
#' @export
create_result_ui <- function(ns, extra_tab = NULL) {
  tagList(
    navset_tab(
      nav_panel(
        "Visualisasi Hasil",
        fluidRow(
          column(
            width = 12,
            style = "margin-top: 10px;",
            div(
              style = "position: relative;",
              leafletOutput(ns("result_map"), height = "450px"),
              div(
                id = ns("overlay_widget"),
                style = "position: absolute; top: 12px; right: 12px; z-index: 1000;",
                actionButton(
                  ns("toggle_overlay_panel"),
                  label = tags$span(
                    id = ns("overlay_active_badge"),
                    class = "badge bg-primary",
                    style = "display: none; margin-left: 4px; font-size: 10px; vertical-align: middle;",
                    "0"
                  ),
                  icon = icon("layer-group"),
                  class = "btn btn-light btn-sm shadow-sm",
                  title = "Overlay Peta Kustom",
                  style = "background: #ffffff; border: 1px solid #cbd5e1; border-radius: 6px; padding: 6px 10px; font-size: 14px; float: right; cursor: pointer;",
                  onclick = sprintf("var p = document.getElementById('%s'); if (!p.style.display || p.style.display === 'none') { p.style.display = 'block'; } else { p.style.display = 'none'; }", ns("overlay_panel"))
                ),
                tags$script(HTML(sprintf("
                  (function() {
                    var widgetId = '%s';
                    var panelId = '%s';
                    document.addEventListener('click', function(e) {
                      var widget = document.getElementById(widgetId);
                      var panel = document.getElementById(panelId);
                      if (widget && panel && !widget.contains(e.target)) {
                        panel.style.display = 'none';
                      }
                    });
                  })();
                ", ns("overlay_widget"), ns("overlay_panel")))),
                div(
                  id = ns("overlay_panel"),
                  style = "display: none; clear: both; margin-top: 42px; background: rgba(255, 255, 255, 0.96); backdrop-filter: blur(4px); border: 1px solid #cbd5e1; border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); padding: 12px; width: 320px; max-height: 420px; overflow-y: auto;",
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px; border-bottom: 1px solid #e2e8f0; padding-bottom: 4px;",
                    tags$span(style = "font-weight: 600; color: #1e293b; font-size: 12px;", "Overlay Tambahan"),
                    tags$button(
                      type = "button",
                      class = "btn-close btn-sm",
                      style = "font-size: 10px; cursor: pointer; border: none; background: transparent; color: #64748b;",
                      onclick = sprintf("document.getElementById('%s').style.display = 'none';", ns("overlay_panel")),
                      "✕"
                    )
                  ),
                  div(
                    style = "margin-bottom: 8px;",
                    fileInput(
                      ns("overlay_file_upload"),
                      label = NULL,
                      accept = c(".shp", ".dbf", ".shx", ".prj", ".cpg", ".tif", ".tiff"),
                      multiple = TRUE,
                      buttonLabel = "Pilih Layer...",
                      placeholder = "Shapefile / GeoTIFF"
                    )
                  ),
                  uiOutput(ns("overlay_layer_list"))
                )
              )
            )
          )
        ),
        hr(style = "margin: 15px 0; border-top: 1px solid #dee2e6;"),
        fluidRow(
          column(
            width = 12,
            div(
              style = "max-height: 400px; overflow-y: auto; width: 100%;",
              DT::DTOutput(ns("result_table"), width = "100%")
            )
          )
        )
      ),
      if (!is.null(extra_tab)) extra_tab,
      nav_panel(
        "Log",
        div(
          style = "max-height: 300px; overflow-y: auto; background-color: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 0.9rem; white-space: pre-wrap;",
          verbatimTextOutput(ns("validation_log"))
        )
      )
    ),
    
    div(
      style = "display: flex; gap: 8px; flex-wrap: wrap; margin-top: 12px;",
      downloadButton(ns("dl_gpkg"), "Unduh GPKG", class = "btn-outline-secondary btn-sm"),
      downloadButton(ns("dl_xlsx"), "Unduh XLSX", class = "btn-outline-secondary btn-sm")
    )
  )
}

#' Render Result Visualization Server Logic
#'
#' @description
#' Registers the shared Shiny server-side outputs used across analysis modules:
#' a Leaflet map, a DT table, a validation log, and download handlers. Renders
#' map geometry with optional simplification, dynamic popups/labels, table
#' column subsetting and rounding, and synchronized table-to-map selection.
#'
#' @param input,output,session Standard Shiny server arguments from the calling
#'   module.
#' @param rv Reactive values object containing `analysis_result$map`,
#'   `analysis_result$table`, `log_messages`, `gpkg_path`, and `xlsx_path`.
#' @param config A list of configuration options:
#'   \describe{
#'     \item{map_color_col}{Column used for map coloring.}
#'     \item{map_title}{Title for the map legend.}
#'     \item{map_label_cols}{Named list or vector of columns used for
#'       labels/popups. Example: `c("ID PU: " = "id_pu", "Indeks: " = "idx_padu_se")`.}
#'     \item{map_palette}{Palette name (e.g., `"RdYlGn"`) or a vector of colors
#'       (passed to `leaflet::colorFactor` for categorical columns).
#'       Default `"RdYlGn"`.}
#'     \item{map_simplify_tolerance}{Simplification tolerance (map units) used
#'       only for the Leaflet display copy of the geometry. Default `5`.}
#'     \item{table_cols}{Named vector for subsetting/renaming table columns.}
#'     \item{table_round_cols}{Character vector of display column names to round
#'       to 2 digits.}
#'     \item{table_optional_cols}{Character vector of *source* column names
#'       (i.e., keys of `table_cols`) that should be dropped from the rendered
#'       table when every value in that column is `NA`. Useful for conditional
#'       columns such as economic/NPV outputs.}
#'   }
#'
#' @return Invisibly `NULL`. Called for its side effects of registering
#'   outputs and observers on `output` and `session`.
#'
#' @importFrom shiny req renderPrint observeEvent observe invalidateLater reactiveVal showNotification renderUI tagList tags actionButton icon
#' @importFrom leaflet renderLeaflet leaflet addProviderTiles addPolygons addPolylines addCircleMarkers addRasterImage
#'   addLegend colorNumeric colorFactor leafletProxy clearGroup setView
#'   highlightOptions leafletOptions providers
#' @importFrom leaflet.extras addSearchFeatures searchFeaturesOptions
#'   addResetMapButton
#' @importFrom DT renderDT datatable formatRound
#' @importFrom sf st_is_longlat st_transform st_simplify st_is_valid
#'   st_make_valid st_centroid st_geometry st_coordinates
#' @importFrom htmltools HTML
#'
#' @export
render_result_server <- function(input, output, session, rv, config) {
  
  ns <- if (!is.null(session$ns)) session$ns else identity
  `%||%` <- function(a, b) if (is.null(a)) b else a

  # Custom Map Overlay layer state (persisted across modules in user session)
  session$userData$overlay_layers <- session$userData$overlay_layers %||% reactiveVal(list())
  
  # Local reactive tracking which overlay layer IDs are currently rendered on this Leaflet canvas
  rendered_layer_ids <- reactiveVal(character(0))
  
  # Default configurations
  map_color_col <- config$map_color_col
  map_title <- config$map_title
  map_palette <- if(!is.null(config$map_palette)) config$map_palette else "RdYlGn"
  table_cols <- config$table_cols
  table_round_cols <- config$table_round_cols
  table_optional_cols <- config$table_optional_cols   # NEW
  
  # Simplification tolerance (map units, typically meters for UTM data) used only for the Leaflet display copy of the geometry.
  map_simplify_tolerance <- if (!is.null(config$map_simplify_tolerance)) {
    config$map_simplify_tolerance
  } else {
    5 # meters
  }
  
  output$result_map <- renderLeaflet({
    req(rv$analysis_result)
    rendered_layer_ids(character(0))
    
    map_sf <- rv$analysis_result$map
    
    # Build a display-only copy with simplified geometry for rendering.
    if (isTRUE(map_simplify_tolerance > 0) && nrow(map_sf) > 0 && !sf::st_is_longlat(map_sf)) {
      map_sf <- tryCatch({
        simplified <- sf::st_simplify(
          map_sf,
          dTolerance = map_simplify_tolerance,
          preserveTopology = TRUE
        )
        # Guard against simplification collapsing/invalidating a geometry
        if (any(!sf::st_is_valid(simplified))) {
          simplified <- sf::st_make_valid(simplified)
        }
        simplified
      }, error = function(e) {
        warning("Map geometry simplification failed, falling back to full precision: ", conditionMessage(e))
        map_sf
      })
    }
    
    if (!sf::st_is_longlat(map_sf)) {
      map_sf <- sf::st_transform(map_sf, crs = 4326)
    }
    
    if (!map_color_col %in% names(map_sf)) {
      return(leaflet::leaflet() %>%
               leaflet::addControl(paste("Kolom", map_color_col, "tidak ditemukan."), position = "topright"))
    }
    
    # Construct popup and label HTML dynamically
    create_html <- function(row) {
      res <- ""
      for (name in names(config$map_label_cols)) {
        col <- config$map_label_cols[[name]]
        if (!col %in% names(row)) next
        val <- row[[col]]
        if (is.null(val) || length(val) == 0 || is.na(val)) {
          val <- "-"
        } else if (is.numeric(val)) {
          val <- round(val, 3)
        }
        res <- paste0(res, "<b>", name, "</b>: ", val, "<br>")
      }
      res
    }
    
    create_label <- function(row) {
      res <- ""
      for (name in names(config$map_label_cols)) {
        col <- config$map_label_cols[[name]]
        if (!col %in% names(row)) next
        val <- row[[col]]
        if (is.null(val) || length(val) == 0 || is.na(val)) {
          val <- "-"
        } else if (is.numeric(val)) {
          val <- round(val, 2)
        }
        if (res == "") {
          res <- paste0(name, " ", val)
        } else {
          res <- paste0(res, " | ", name, " ", val)
        }
      }
      res
    }
    
    # Apply to all rows
    if (nrow(map_sf) > 0) {
      popups <- sapply(1:nrow(map_sf), function(i) create_html(map_sf[i, ]))
      labels <- sapply(1:nrow(map_sf), function(i) create_label(map_sf[i, ]))
      map_sf$popup_html <- lapply(popups, htmltools::HTML)
      map_sf$search_label <- lapply(labels, htmltools::HTML)
    } else {
      map_sf$popup_html <- list()
      map_sf$search_label <- list()
    }
    
    # Check if map_color_col is numeric to decide palette type
    if (is.numeric(map_sf[[map_color_col]])) {
      vals <- map_sf[[map_color_col]]
      legend_vals <- if (any(is.finite(vals))) vals[is.finite(vals)] else c(0, 1)
      pal <- leaflet::colorNumeric(
        palette = map_palette,
        domain  = legend_vals,
        na.color = "transparent"
      )
    } else {
      legend_vals <- map_sf[[map_color_col]]
      pal <- leaflet::colorFactor(
        palette = map_palette,
        domain  = legend_vals,
        na.color = "transparent"
      )
    }
    
    base_map <- leaflet::leaflet(
      map_sf,
      options = leafletOptions(preferCanvas = TRUE)
    ) %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>% 
      leaflet::addPolygons(
        layerId     = ~id_pu,
        group       = "result_layer",
        fillColor   = ~pal(get(map_color_col)),
        fillOpacity = 0.7,
        weight      = 1,
        color       = "black",
        stroke      = FALSE,
        label       = ~search_label,
        popup       = ~popup_html,
        highlightOptions = leaflet::highlightOptions(
          weight = 3,
          color  = "red",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      leaflet.extras::addSearchFeatures(
        targetGroups = "result_layer",
        options = leaflet.extras::searchFeaturesOptions(
          propertyName = "label",
          zoom = 15,
          openPopup = TRUE,
          firstTipSubmit = TRUE,
          autoCollapse = FALSE,
          hideMarkerOnCollapse = TRUE
        )
      ) %>% leaflet.extras::addResetMapButton() %>%
      leaflet::addLegend(
        position = "bottomright",
        pal      = pal,
        values   = legend_vals,
        title    = map_title,
        opacity  = 0.7
      )
    
    # Render any existing active overlay layers from session on initial map build
    active_layers <- if (!is.null(session$userData$overlay_layers)) session$userData$overlay_layers() else list()
    full_map <- draw_overlay_stack(base_map, active_layers)
    rendered_layer_ids(if (length(active_layers) > 0) vapply(active_layers, function(l) l$id, character(1)) else character(0))
    
    full_map
  })
  
  output$result_table <- DT::renderDT({
    req(rv$analysis_result)
    
    df <- rv$analysis_result$table
    current_table_cols <- table_cols
    current_round_cols <- table_round_cols
    
    if (isTRUE(config$include_dynamic_cols)) {
      extra_cols <- setdiff(colnames(df), c(names(current_table_cols), "geometry"))
      for (ec in extra_cols) {
        disp_name <- ec
        if (grepl("^dist_", ec)) {
          disp_name <- paste0("Jarak ", gsub("_", " ", sub("^dist_", "", ec)), " (m)")
        } else if (grepl("^acc_", ec)) {
          disp_name <- paste0("Skor Akses ", gsub("_", " ", sub("^acc_", "", ec)))
        }
        current_table_cols[ec] <- disp_name
        if (is.numeric(df[[ec]])) {
          current_round_cols <- c(current_round_cols, disp_name)
        }
      }
      # Preserve dataframe column sequence for display
      col_order <- intersect(colnames(df), names(current_table_cols))
      current_table_cols <- current_table_cols[col_order]
    }
    
    # Subset by available columns
    valid_cols <- names(current_table_cols)[names(current_table_cols) %in% colnames(df)]
    df_subset <- df[, valid_cols, drop = FALSE]
    
    if (!is.null(table_optional_cols) && length(table_optional_cols) > 0) {
      drop_cols <- character(0)
      for (cn in valid_cols) {
        if (cn %in% table_optional_cols && all(is.na(df_subset[[cn]]))) {
          drop_cols <- c(drop_cols, cn)
        }
      }
      if (length(drop_cols) > 0) {
        df_subset <- df_subset[, setdiff(colnames(df_subset), drop_cols), drop = FALSE]
        valid_cols <- setdiff(valid_cols, drop_cols)
      }
    }
    
    # Rename to display names
    colnames(df_subset) <- current_table_cols[valid_cols]
    
    dt <- DT::datatable(
      df_subset,
      selection = "single",
      extensions = c('FixedColumns', 'FixedHeader'),
      options = list(
        pageLength     = 10,
        autoWidth      = TRUE,
        scrollX        = TRUE,
        scrollY        = "400px",
        scrollCollapse = TRUE,
        dom            = 'Bfrtip',
        columnDefs     = list(
          list(targets = "_all", className = "dt-center")
        )
      ),
      rownames = FALSE,
      class = "display compact stripe hover"
    )
    
    # Round specified columns
    if (!is.null(current_round_cols) && length(current_round_cols) > 0) {
      valid_round_cols <- current_round_cols[current_round_cols %in% colnames(df_subset)]
      if (length(valid_round_cols) > 0) {
        dt <- dt %>% DT::formatRound(columns = valid_round_cols, digits = 2)
      }
    }
    
    dt
  })
  
  # Table row selection targets map polygon
  observeEvent(input$result_table_rows_selected, {
    req(rv$analysis_result)
    
    selected_idx <- input$result_table_rows_selected
    df_table <- rv$analysis_result$table
    
    if (!"id_pu" %in% colnames(df_table)) return()
    
    selected_id_pu <- df_table$id_pu[selected_idx]
    
    # Subset in the original CRS
    map_sf_raw <- rv$analysis_result$map
    selected_polygon_raw <- map_sf_raw[map_sf_raw$id_pu == selected_id_pu, ]
    req(nrow(selected_polygon_raw) > 0)
    
    if (any(!sf::st_is_valid(selected_polygon_raw))) {
      selected_polygon_raw <- sf::st_make_valid(selected_polygon_raw)
    }
    
    centroid_pt <- sf::st_centroid(sf::st_geometry(selected_polygon_raw))
    if (!sf::st_is_longlat(selected_polygon_raw)) {
      centroid_pt <- sf::st_transform(centroid_pt, crs = 4326)
    }
    centroid_coord <- sf::st_coordinates(centroid_pt)
    
    selected_polygon <- if (!sf::st_is_longlat(selected_polygon_raw)) {
      sf::st_transform(selected_polygon_raw, crs = 4326)
    } else {
      selected_polygon_raw
    }
    
    # Build popup for highlighted polygon
    row_data <- selected_polygon[1, ]
    popup_text <- ""
    for (name in names(config$map_label_cols)) {
      col <- config$map_label_cols[[name]]
      val <- row_data[[col]]
      if (is.numeric(val)) val <- round(val, 3)
      popup_text <- paste0(popup_text, "<b>", name, " (Terpilih)</b>: ", val, "<br>")
    }
    
    leaflet::leafletProxy("result_map", session = session) %>%
      leaflet::clearGroup("polygon_highlight") %>%
      leaflet::setView(lng = centroid_coord[1], lat = centroid_coord[2], zoom = 13) %>%
      leaflet::addPolygons(
        data = selected_polygon,
        color = "#008B8B",
        weight = 5,
        fillColor = "#00FFFF",
        fillOpacity = 0.7,
        group = "polygon_highlight",
        popup = htmltools::HTML(popup_text)
      )
  })
  
  observe({
    if (is.null(input$result_table_rows_selected)) {
      leaflet::leafletProxy("result_map", session = session) %>% leaflet::clearGroup("polygon_highlight")
    }
  })
  
  output$validation_log <- renderPrint({
    invalidateLater(100, session)
    cat(rv$log_messages)
  })
  
  output$dl_gpkg <- downloadHandler(
    filename = function() {
      if(!is.null(rv$gpkg_path)) basename(rv$gpkg_path) else "result.gpkg"
    },
    content = function(file) {
      req(rv$gpkg_path)
      file.copy(rv$gpkg_path, file, overwrite = TRUE)
    }
  )
  
  output$dl_xlsx <- downloadHandler(
    filename = function() {
      if(!is.null(rv$xlsx_path)) basename(rv$xlsx_path) else "result.xlsx"
    },
    content = function(file) {
      req(rv$xlsx_path)
      file.copy(rv$xlsx_path, file, overwrite = TRUE)
    }
  )
  
  # --- Custom Map Overlay Server Handlers ---
  
  # Handle file upload for external vector and raster layers
  observeEvent(input$overlay_file_upload, {
    req(input$overlay_file_upload)
    file_df <- input$overlay_file_upload
    
    is_vector <- any(grepl("\\.(shp|gpkg)$", file_df$name, ignore.case = TRUE))
    is_raster <- any(grepl("\\.(tif|tiff)$", file_df$name, ignore.case = TRUE))
    
    if (!is_vector && !is_raster) {
      showNotification(
        "Format file tidak didukung. Harap unggah Shapefile (.shp, .shx, .dbf, .prj) atau GeoTIFF (.tif, .tiff).",
        type = "error",
        duration = 6
      )
      return()
    }
    
    tryCatch({
      current_layers <- session$userData$overlay_layers()
      vec_count <- sum(vapply(current_layers, function(l) identical(l$type, "vector"), integer(1)))
      
      new_layer <- if (is_vector) {
        process_overlay_vector(file_df, existing_count = vec_count)
      } else {
        process_overlay_raster(file_df)
      }
      
      # Prepend new layer so it appears at top of UI list and top of canvas stack
      all_layers <- overlay_update_z_indices(c(list(new_layer), current_layers))
      session$userData$overlay_layers(all_layers)
      
      showNotification(
        paste0("Layer '", new_layer$name, "' berhasil ditambahkan ke peta."),
        type = "message",
        duration = 4
      )
    }, error = function(e) {
      showNotification(
        paste0("Gagal memproses layer: ", conditionMessage(e)),
        type = "error",
        duration = 7
      )
    })
  })
  
  # Render list of active overlay layers in the floating widget
  output$overlay_layer_list <- renderUI({
    layers <- session$userData$overlay_layers()
    active_count <- length(layers)
    badge_js <- tags$script(HTML(sprintf(
      "var b = document.getElementById('%s'); if (b) { b.textContent = '%d'; b.style.display = %s; }",
      ns("overlay_active_badge"),
      active_count,
      if (active_count > 0) "'inline-block'" else "'none'"
    )))
    
    if (length(layers) == 0) {
      return(tagList(
        badge_js,
        div(
          style = "color: #94a3b8; font-style: italic; font-size: 11px; text-align: center; padding: 8px 0;",
          "Belum ada layer tambahan."
        )
      ))
    }
    
    tagList(
      badge_js,
      lapply(layers, function(layer) {
        is_visible <- if (is.null(layer$visible)) TRUE else isTRUE(layer$visible)
        opacity_val <- layer$opacity %||% 0.7
        pct_label <- paste0(round(opacity_val * 100), "%")
        is_vector <- identical(layer$type, "vector")
        
        vis_btn_id <- paste0("vis_overlay_", layer$id)
        opacity_input_id <- paste0("opacity_overlay_", layer$id)
        col_input_id <- paste0("col_overlay_", layer$id)
        zoom_btn_id <- paste0("zoom_overlay_", layer$id)
        up_btn_id <- paste0("up_overlay_", layer$id)
        down_btn_id <- paste0("down_overlay_", layer$id)
        del_btn_id <- paste0("del_overlay_", layer$id)
        
        type_badge <- if (is_vector) {
          tags$span(
            style = paste0(
              "display: inline-block; width: 10px; height: 10px; border-radius: 2px; margin-right: 6px; flex-shrink: 0; background-color: ",
              layer$color %||% "#2563EB", ";"
            )
          )
        } else {
          tags$span(
            style = "display: inline-block; width: 10px; height: 10px; border-radius: 2px; margin-right: 6px; flex-shrink: 0; background: linear-gradient(45deg, #440154, #21908d, #fde725);"
          )
        }
        
        div(
          id = ns(paste0("item_overlay_", layer$id)),
          style = paste0(
            "padding: 6px 8px; margin-bottom: 6px; background: #ffffff; border: 1px solid #e2e8f0; border-radius: 6px; box-shadow: 0 1px 2px rgba(0,0,0,0.03);",
            if (!is_visible) " opacity: 0.65;" else ""
          ),
          # Primary Row: Reorder, Visibility, Label/Badge, Zoom, Delete
          div(
            style = "display: flex; align-items: center; justify-content: space-between; gap: 4px;",
            # Up / Down order buttons
            div(
              style = "display: flex; gap: 2px;",
              actionButton(
                ns(up_btn_id),
                label = NULL,
                icon = icon("arrow-up"),
                class = "btn btn-light btn-sm text-secondary",
                style = "padding: 1px 4px; font-size: 9px; line-height: 1; border: 1px solid #e2e8f0;",
                title = "Naikkan posisi (ke atas / visual Z-index depan)"
              ),
              actionButton(
                ns(down_btn_id),
                label = NULL,
                icon = icon("arrow-down"),
                class = "btn btn-light btn-sm text-secondary",
                style = "padding: 1px 4px; font-size: 9px; line-height: 1; border: 1px solid #e2e8f0;",
                title = "Turunkan posisi (ke bawah / visual Z-index belakang)"
              )
            ),
            # Visibility toggle
            actionButton(
              ns(vis_btn_id),
              label = NULL,
              icon = icon(if (is_visible) "eye" else "eye-slash"),
              class = paste0("btn btn-sm ", if (is_visible) "btn-outline-primary" else "btn-outline-secondary"),
              style = "padding: 1px 5px; font-size: 10px; line-height: 1;",
              title = if (is_visible) "Sembunyikan layer" else "Tampilkan layer"
            ),
            # Layer Title & Badge
            div(
              style = "display: flex; align-items: center; flex: 1; min-width: 0; margin: 0 4px;",
              type_badge,
              tags$span(
                title = layer$name,
                style = "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-weight: 600; color: #1e293b; font-size: 11px;",
                layer$name
              )
            ),
            # Actions: Zoom & Delete
            div(
              style = "display: flex; gap: 2px;",
              actionButton(
                ns(zoom_btn_id),
                label = NULL,
                icon = icon("search"),
                class = "btn btn-outline-secondary btn-sm",
                style = "padding: 1px 5px; font-size: 10px; line-height: 1;",
                title = "Zoom ke layer"
              ),
              actionButton(
                ns(del_btn_id),
                label = NULL,
                icon = icon("trash"),
                class = "btn btn-outline-danger btn-sm",
                style = "padding: 1px 5px; font-size: 10px; line-height: 1;",
                title = "Hapus layer"
              )
            )
          ),
          # Secondary Row: Opacity Slider & Color Picker
          div(
            style = "display: flex; align-items: center; justify-content: space-between; margin-top: 5px; padding-top: 4px; border-top: 1px dashed #f1f5f9; font-size: 10px; color: #64748b;",
            # Opacity slider
            div(
              style = "display: flex; align-items: center; gap: 4px;",
              tags$span(style = "font-weight: 500;", "Transparansi:"),
              tags$input(
                type = "range",
                id = ns(opacity_input_id),
                min = "0",
                max = "1",
                step = "0.05",
                value = as.character(opacity_val),
                style = "width: 70px; height: 14px; cursor: pointer; accent-color: #2563EB;",
                oninput = sprintf(
                  "Shiny.setInputValue('%s', parseFloat(this.value), {priority: 'event'}); var lbl = document.getElementById('%s'); if(lbl) lbl.innerText = Math.round(this.value * 100) + '%%';",
                  ns(opacity_input_id),
                  ns(paste0("opacity_txt_", layer$id))
                )
              ),
              tags$span(
                id = ns(paste0("opacity_txt_", layer$id)),
                style = "width: 28px; text-align: right; font-variant-numeric: tabular-nums; font-size: 9px;",
                pct_label
              )
            ),
            # Color picker (vectors only)
            if (is_vector) {
              div(
                style = "display: flex; align-items: center; gap: 4px;",
                tags$span(style = "font-weight: 500;", "Warna:"),
                tags$input(
                  type = "color",
                  id = ns(col_input_id),
                  value = layer$color %||% "#2563EB",
                  style = "width: 22px; height: 18px; padding: 0 1px; border: 1px solid #cbd5e1; border-radius: 3px; cursor: pointer;",
                  title = "Pilih warna layer",
                  oninput = sprintf(
                    "Shiny.setInputValue('%s', this.value, {priority: 'event'});",
                    ns(col_input_id)
                  )
                )
              )
            } else {
              tags$span(style = "font-size: 9px; color: #94a3b8; font-style: italic;", "Viridis")
            }
          )
        )
      })
    )
  })
  
  # Register dynamic observers for overlay layer actions
  registered_layer_handlers <- character(0)
  
  observe({
    layers <- session$userData$overlay_layers()
    req(length(layers) > 0)
    
    for (layer in layers) {
      lid <- layer$id
      if (lid %in% registered_layer_handlers) next
      registered_layer_handlers <<- c(registered_layer_handlers, lid)
      
      local({
        this_id <- lid
        
        # 1. Delete button
        observeEvent(input[[paste0("del_overlay_", this_id)]], {
          curr <- session$userData$overlay_layers()
          session$userData$overlay_layers(overlay_remove_layer(curr, this_id))
        }, ignoreInit = TRUE)
        
        # 2. Visibility toggle button / checkbox
        observeEvent(input[[paste0("vis_overlay_", this_id)]], {
          val <- input[[paste0("vis_overlay_", this_id)]]
          curr <- session$userData$overlay_layers()
          if (is.logical(val)) {
            session$userData$overlay_layers(overlay_toggle_layer_visibility(curr, this_id, visible = val))
          } else {
            session$userData$overlay_layers(overlay_toggle_layer_visibility(curr, this_id))
          }
        }, ignoreInit = TRUE)
        
        # 3. Opacity slider
        observeEvent(input[[paste0("opacity_overlay_", this_id)]], {
          new_opacity <- as.numeric(input[[paste0("opacity_overlay_", this_id)]])
          req(!is.na(new_opacity))
          curr <- session$userData$overlay_layers()
          session$userData$overlay_layers(overlay_update_layer_opacity(curr, this_id, new_opacity))
        }, ignoreInit = TRUE)
        
        # 4. Color picker (strictly col_overlay_<id>)
        observeEvent(input[[paste0("col_overlay_", this_id)]], {
          val <- input[[paste0("col_overlay_", this_id)]]
          req(val, nzchar(val))
          curr <- session$userData$overlay_layers()
          session$userData$overlay_layers(overlay_update_layer_color(curr, this_id, val))
        }, ignoreInit = TRUE)
        
        # 5. Zoom to layer button
        observeEvent(input[[paste0("zoom_overlay_", this_id)]], {
          curr <- session$userData$overlay_layers()
          idx <- which(vapply(curr, function(l) l$id == this_id, logical(1)))
          if (length(idx) > 0) {
            bb <- overlay_get_layer_bbox(curr[[idx]])
            if (!is.null(bb)) {
              leaflet::leafletProxy("result_map", session = session) %>%
                leaflet::fitBounds(
                  lng1 = as.numeric(bb[["xmin"]]),
                  lat1 = as.numeric(bb[["ymin"]]),
                  lng2 = as.numeric(bb[["xmax"]]),
                  lat2 = as.numeric(bb[["ymax"]])
                )
            }
          }
        }, ignoreInit = TRUE)
        
        # 6. Up / Down order buttons (Z-index stacking)
        observeEvent(input[[paste0("up_overlay_", this_id)]], {
          curr <- session$userData$overlay_layers()
          session$userData$overlay_layers(overlay_reorder_layer(curr, this_id, direction = "up"))
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("down_overlay_", this_id)]], {
          curr <- session$userData$overlay_layers()
          session$userData$overlay_layers(overlay_reorder_layer(curr, this_id, direction = "down"))
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # Synchronize overlay layers with Leaflet canvas map
  observe({
    req(rv$analysis_result)
    current_layers <- session$userData$overlay_layers()
    current_ids <- if (length(current_layers) > 0) vapply(current_layers, function(l) l$id, character(1)) else character(0)
    rendered_ids <- rendered_layer_ids()
    
    proxy <- leaflet::leafletProxy("result_map", session = session)
    
    # Clear all groups that were rendered before or are current
    groups_to_clear <- unique(c(rendered_ids, current_ids))
    for (gid in groups_to_clear) {
      proxy %>% leaflet::clearGroup(gid)
    }
    
    # Redraw active visible layers in reverse order (bottom to top, so top of list sits visually on top)
    draw_overlay_stack(proxy, current_layers)
    
    rendered_layer_ids(current_ids)
  })
}

#' Plot Continuous Raster or Vector (sf) Map with Optional PNG Export
#'
#' @description
#' Creates a continuous map using **ggplot2**, supporting either a
#' [`SpatRaster`][terra::SpatRaster] (plotted via **tidyterra**) or an
#' [`sf`][sf::st_sf] object (plotted via `geom_sf()`). Instead of an HTML
#' download button, the plot can optionally be exported directly to a PNG file
#' by supplying `filepath`.
#'
#' @details
#' When `filepath` is supplied, the exported PNG is drawn on top of an
#' **Esri.WorldGrayCanvas** basemap (via the \pkg{basemaps} package). The
#' data is temporarily reprojected to EPSG:3857 (Web Mercator) so it aligns
#' with the tile service; the returned `ggplot` object is unaffected and keeps
#' the original CRS. If the basemap cannot be fetched (e.g. no internet, or
#' \pkg{basemaps} not installed), a warning is emitted and the PNG is saved
#' without a basemap.
#'
#' @param map A [`SpatRaster`][terra::SpatRaster] or [`sf`][sf::st_sf] object to plot.
#' @param title A character string giving the overall map title, shown above
#'   the plot. If `NULL` (default), no title is shown. Independent of `legend`,
#'   which labels the color bar.
#' @param column A character string giving the name of the numeric column to
#'   plot as the continuous fill/color variable. Required when `map` is an `sf`
#'   object; ignored when `map` is a `SpatRaster`.
#' @param legend A character string giving the legend title. If `NULL`, no
#'   legend title is shown.
#' @param low A character string specifying the color for the low end of the gradient.
#' @param high A character string specifying the color for the high end of the gradient.
#' @param na_color A character string for the color of `NA` values. Defaults to `"white"`.
#' @param filepath A string giving the file path (including extension, e.g.
#'   `"output/map.png"`) to export the plot as a PNG. If `NULL` (default), no
#'   file is written.
#' @param width Numeric width (inches) for the exported PNG. Defaults to `7`.
#' @param height Numeric height (inches) for the exported PNG. Also used to size
#'   the legend color bar, which is drawn at 80% of this height. Defaults to `5`.
#' @param dpi An integer giving the resolution (dots per inch) for the exported
#'   PNG. Defaults to `300`.
#'
#' @return A `ggplot` object. If `filepath` is supplied, the plot is also saved
#'   as a PNG (with an Esri.WorldGrayCanvas basemap) to that path as a side
#'   effect.
#'
#' @importFrom ggplot2 ggplot aes geom_sf scale_fill_gradient scale_color_gradient
#'   theme_bw labs coord_sf guides guide_colorbar theme
#'   element_blank element_text margin ggsave
#' @importFrom tidyterra geom_spatraster
#' @importFrom sf st_zm st_make_valid st_is_empty st_geometry_type st_crs st_transform
#' @importFrom terra crs project
#' @importFrom grid unit
#'
#' @export
plot_continuous_map <- function(map, title = NULL, column = NULL, legend, low, high,
                                na_color = "white", filepath = NULL,
                                width = 7, height = 5, dpi = 300) {
  
  build_plot <- function(map_data, basemap_layer = NULL) {
    
    if (inherits(map_data, "SpatRaster")) {
      
      p <- ggplot2::ggplot()
      if (!is.null(basemap_layer)) p <- p + basemap_layer
      p <- p +
        tidyterra::geom_spatraster(data = map_data) +
        ggplot2::scale_fill_gradient(
          low = low,
          high = high,
          na.value = na_color,
          name = if (!is.null(legend)) legend else NULL
        )
      
    } else if (inherits(map_data, "sf")) {
      
      if (is.null(column)) {
        stop("`column` must be specified when `map` is an sf object.")
      }
      if (!column %in% names(map_data)) {
        stop(sprintf("Column '%s' not found in `map`.", column))
      }
      
      map_data <- sf::st_zm(map_data, drop = TRUE, what = "ZM")
      map_data <- sf::st_make_valid(map_data)
      map_data <- map_data[!sf::st_is_empty(map_data), ]
      map_data[[column]] <- as.numeric(map_data[[column]])
      map_data[[column]][!is.finite(map_data[[column]])] <- NA
      
      geom_types <- unique(as.character(sf::st_geometry_type(map_data)))
      is_polygon <- any(grepl("POLYGON", geom_types))
      
      p <- ggplot2::ggplot()
      if (!is.null(basemap_layer)) p <- p + basemap_layer
      
      if (is_polygon) {
        p <- p +
          ggplot2::geom_sf(data = map_data, ggplot2::aes(fill = .data[[column]]), color = NA) +
          ggplot2::scale_fill_gradient(
            low = low,
            high = high,
            na.value = na_color,
            name = if (!is.null(legend)) legend else NULL
          )
      } else {
        p <- p +
          ggplot2::geom_sf(data = map_data, ggplot2::aes(color = .data[[column]])) +
          ggplot2::scale_color_gradient(
            low = low,
            high = high,
            na.value = na_color,
            name = if (!is.null(legend)) legend else NULL
          )
      }
      
    } else {
      stop("`map` must be a SpatRaster or an sf object.")
    }
    
    p +
      ggplot2::theme_bw() +
      ggplot2::labs(fill = NULL, title = title) +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::guides(
        fill = ggplot2::guide_colorbar(
          title.position = "top",
          direction = "vertical",
          barwidth = grid::unit(0.4, "cm"),
          barheight = grid::unit(0.8 * height, "in")
        ),
        color = ggplot2::guide_colorbar(
          title.position = "top",
          direction = "vertical",
          barwidth = grid::unit(0.4, "cm"),
          barheight = grid::unit(0.8 * height, "in")
        )
      ) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 8),
        axis.text.y = ggplot2::element_text(size = 8),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0),
        legend.title = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 10),
        legend.position = "right",
        legend.justification = c(0, 0.5),
        legend.box.spacing = grid::unit(0.5, "cm"),
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        plot.margin = ggplot2::margin(t = 5, r = 5, b = 2, l = 2)
      )
  }
  
  plot_lc <- build_plot(map)
  
  if (!is.null(filepath)) {
    map_3857 <- .reproject_to_3857(map)
    bm <- .get_esri_gray_basemap(map_3857)
    export_plot <- if (!is.null(bm)) build_plot(map_3857, bm) else plot_lc
    ggplot2::ggsave(filename = filepath, plot = export_plot,
                    width = width, height = height, dpi = dpi)
  }
  
  return(plot_lc)
}


#' Plot Categorical Raster or Vector (sf) Map with Optional PNG Export
#'
#' @description
#' Creates a categorical (discrete class) map using **ggplot2**, supporting
#' either a [`SpatRaster`][terra::SpatRaster] (plotted via **tidyterra**) or an
#' [`sf`][sf::st_sf] object (plotted via `geom_sf()`). Uses the same overall
#' styling as [plot_continuous_map()]. Long class names in the legend are
#' automatically wrapped so they aren't cropped by the map's dimensions, and the
#' legend can be split into multiple columns if there are many categories.
#'
#' @details
#' When `filepath` is supplied, the exported PNG is drawn on top of an
#' **Esri.WorldGrayCanvas** basemap (via the \pkg{basemaps} package), following
#' the same approach as [plot_continuous_map()]. If the basemap cannot be
#' fetched, a warning is emitted and the PNG is saved without a basemap.
#'
#' @param map A [`SpatRaster`][terra::SpatRaster] or [`sf`][sf::st_sf] object to plot.
#' @param title A character string giving the overall map title, shown above the
#'   plot, left-justified. If `NULL` (default), no title is shown.
#' @param column A character string giving the name of the categorical column to
#'   plot. Required when `map` is an `sf` object; ignored when `map` is a
#'   `SpatRaster`.
#' @param lookup For `SpatRaster` input **only** (required): a file path to a
#'   `.csv`, `.xlsx`, or `.xls` file containing the raster's class table, with
#'   (at least) an ID column matching the raster's integer cell values and a
#'   class-name column.
#' @param id_col Column name in `lookup` holding the raster cell ID values.
#'   Defaults to `"ID"`.
#' @param class_col Column name in `lookup` holding the class name/label.
#'   Defaults to `"class"`.
#' @param legend A character string giving the legend title. If `NULL`, no
#'   legend title is shown.
#' @param colors An optional named character vector of colors, with names
#'   matching the category labels (from `class_col` for rasters, or the unique
#'   values of `column` for `sf`). If `NULL` (default), a default discrete
#'   palette is generated automatically.
#' @param na_color A character string for the color of `NA` values. Defaults to `"white"`.
#' @param label_wrap_width Integer giving the number of characters after which
#'   legend labels wrap onto a new line. Defaults to `15`. Increase for a wider
#'   legend column, decrease if labels are still being cut off.
#' @param legend_ncol Integer giving the number of columns to arrange legend
#'   keys into. Defaults to `1`. Increase this if there are many categories and
#'   the legend is taller than the map (getting cropped vertically).
#' @param filepath A string giving the file path (including extension, e.g.
#'   `"output/map.png"`) to export the plot as a PNG. If `NULL` (default), no
#'   file is written.
#' @param width Numeric width (inches) for the exported PNG. Defaults to `7`.
#' @param height Numeric height (inches) for the exported PNG. Defaults to `5`.
#' @param dpi An integer giving the resolution (dots per inch) for the exported
#'   PNG. Defaults to `300`.
#'
#' @return A `ggplot` object. If `filepath` is supplied, the plot is also saved
#'   as a PNG (with an Esri.WorldGrayCanvas basemap) to that path as a side
#'   effect.
#'
#' @importFrom ggplot2 ggplot aes geom_sf scale_fill_manual scale_color_manual
#'   theme_bw labs coord_sf guides guide_legend theme
#'   element_blank element_text margin ggsave
#' @importFrom tidyterra geom_spatraster
#' @importFrom sf st_zm st_make_valid st_is_empty st_geometry_type st_crs st_transform
#' @importFrom scales hue_pal label_wrap
#' @importFrom grid unit
#' @importFrom terra levels crs project
#' @importFrom stats setNames
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#'
#' @export
plot_categorical_map <- function(map, title = NULL, column = NULL, lookup = NULL,
                                 id_col = "ID", class_col = "class", legend = NULL,
                                 colors = NULL, na_color = "white",
                                 label_wrap_width = 30, legend_ncol = 1,
                                 filepath = NULL, width = 7, height = 5, dpi = 300) {
  
  read_lookup <- function(path) {
    ext <- tolower(tools::file_ext(path))
    if (ext == "csv") {
      utils::read.csv(path, stringsAsFactors = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package 'readxl' is required to read xlsx/xls lookup files.")
      }
      readxl::read_excel(path)
    } else {
      stop("`lookup` must be a .csv, .xlsx, or .xls file.")
    }
  }
  
  build_plot <- function(map_data, basemap_layer = NULL) {
    
    if (inherits(map_data, "SpatRaster")) {
      
      if (is.null(lookup)) {
        stop("`lookup` (a .csv/.xlsx file with ID and class columns) is required for SpatRaster input.")
      }
      
      lookup_df <- read_lookup(lookup)
      if (!all(c(id_col, class_col) %in% names(lookup_df))) {
        stop(sprintf("`lookup` must contain columns '%s' and '%s'.", id_col, class_col))
      }
      
      cat_df <- data.frame(
        id    = lookup_df[[id_col]],
        class = as.character(lookup_df[[class_col]])
      )
      terra::levels(map_data) <- cat_df
      
      class_labels <- as.character(terra::levels(map_data)[[1]][[2]])
      
      if (is.null(colors)) {
        pal <- stats::setNames(scales::hue_pal()(length(class_labels)), class_labels)
      } else {
        pal <- colors
      }
      
      p <- ggplot2::ggplot()
      if (!is.null(basemap_layer)) p <- p + basemap_layer
      p <- p +
        tidyterra::geom_spatraster(data = map_data) +
        ggplot2::scale_fill_manual(
          values   = pal,
          na.value = na_color,
          labels   = scales::label_wrap(label_wrap_width),
          name     = if (!is.null(legend)) legend else NULL
        )
      
    } else if (inherits(map_data, "sf")) {
      
      if (is.null(column) || is.na(column) || column == "") {
        map_data <- sf::st_zm(map_data, drop = TRUE, what = "ZM")
        map_data <- sf::st_make_valid(map_data)
        map_data <- map_data[!sf::st_is_empty(map_data), ]
        
        geom_types <- unique(as.character(sf::st_geometry_type(map_data)))
        is_polygon <- any(grepl("POLYGON", geom_types))
        
        p <- ggplot2::ggplot()
        if (!is.null(basemap_layer)) p <- p + basemap_layer
        
        if (is_polygon) {
          p <- p + ggplot2::geom_sf(data = map_data, fill = "lightblue",
                                    color = "darkblue", size = 0.2)
        } else {
          p <- p + ggplot2::geom_sf(data = map_data, color = "darkblue")
        }
        
      } else {
        if (!column %in% names(map_data)) {
          stop(sprintf("Column '%s' not found in `map`.", column))
        }
        
        map_data <- sf::st_zm(map_data, drop = TRUE, what = "ZM")
        map_data <- sf::st_make_valid(map_data)
        map_data <- map_data[!sf::st_is_empty(map_data), ]
        
        map_data[[column]] <- factor(map_data[[column]])
        class_labels <- levels(map_data[[column]])
        
        if (is.null(colors)) {
          pal <- stats::setNames(scales::hue_pal()(length(class_labels)), class_labels)
        } else {
          pal <- colors
        }
        
        geom_types <- unique(as.character(sf::st_geometry_type(map_data)))
        is_polygon <- any(grepl("POLYGON", geom_types))
        
        p <- ggplot2::ggplot()
        if (!is.null(basemap_layer)) p <- p + basemap_layer
        
        if (is_polygon) {
          p <- p +
            ggplot2::geom_sf(data = map_data, ggplot2::aes(fill = .data[[column]]), color = NA) +
            ggplot2::scale_fill_manual(
              values   = pal,
              na.value = na_color,
              labels   = scales::label_wrap(label_wrap_width),
              name     = if (!is.null(legend)) legend else NULL
            )
        } else {
          p <- p +
            ggplot2::geom_sf(data = map_data, ggplot2::aes(color = .data[[column]])) +
            ggplot2::scale_color_manual(
              values   = pal,
              na.value = na_color,
              labels   = scales::label_wrap(label_wrap_width),
              name     = if (!is.null(legend)) legend else NULL
            )
        }
      }
      
    } else {
      stop("`map` must be a SpatRaster or an sf object.")
    }
    
    p <- p +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title) +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 8),
        axis.text.y = ggplot2::element_text(size = 8),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0),
        plot.margin = ggplot2::margin(t = 5, r = 5, b = 2, l = 2)
      )
    
    if (inherits(map_data, "sf") && (is.null(column) || is.na(column) || column == "")) {
      p <- p + ggplot2::theme(legend.position = "none")
    } else {
      p <- p +
        ggplot2::guides(
          fill = ggplot2::guide_legend(
            title.position = "top",
            ncol      = legend_ncol,
            keywidth  = grid::unit(0.4, "cm"),
            keyheight = grid::unit(0.4, "cm")
          ),
          color = ggplot2::guide_legend(
            title.position = "top",
            ncol      = legend_ncol,
            keywidth  = grid::unit(0.4, "cm"),
            keyheight = grid::unit(0.4, "cm")
          )
        ) +
        ggplot2::theme(
          legend.title = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 9),
          legend.position = "right",
          legend.justification = c(0, 0.5),
          legend.box.spacing = grid::unit(0.5, "cm"),
          legend.margin = ggplot2::margin(0, 0, 0, 0)
        )
    }
    
    p
  }
  
  plot_lc <- build_plot(map)
  
  if (!is.null(filepath)) {
    map_3857 <- .reproject_to_3857(map)
    bm <- .get_esri_gray_basemap(map_3857)
    export_plot <- if (!is.null(bm)) build_plot(map_3857, bm) else plot_lc
    ggplot2::ggsave(filename = filepath, plot = export_plot,
                    width = width, height = height, dpi = dpi)
  }
  
  return(plot_lc)
}

#' @noRd
#' @keywords internal
.reproject_to_3857 <- function(map) {
  target <- "EPSG:3857"
  if (inherits(map, "SpatRaster")) {
    cur <- terra::crs(map)
    if (is.na(cur) || is.null(cur) || identical(cur, "")) return(map)
    if (!identical(terra::crs(map, proj = TRUE),
                   terra::crs(target, proj = TRUE))) {
      map <- terra::project(map, target)
    }
  } else if (inherits(map, "sf")) {
    cur <- sf::st_crs(map)
    if (is.na(cur)) return(map)
    if (cur != sf::st_crs(target)) {
      map <- sf::st_transform(map, target)
    }
  }
  map
}

#' Fetch Esri.WorldGrayCanvas tiles and return them as a ggplot layer.
#'
#' Uses \pkg{maptiles} to download the tiles and \pkg{tidyterra} to render
#' the returned RGB \code{SpatRaster} as a ggplot layer. Fails gracefully
#' (returns \code{NULL} with a warning) if either package is unavailable or
#' the tiles cannot be fetched (e.g. no internet).
#'
#' @noRd
#' @keywords internal
.get_esri_gray_basemap <- function(map) {
  if (!requireNamespace("maptiles", quietly = TRUE)) {
    warning("Package 'maptiles' is required to fetch the Esri.WorldGrayCanvas ",
            "basemap. Install with install.packages('maptiles'). ",
            "Proceeding without basemap.", call. = FALSE)
    return(NULL)
  }
  tryCatch({
    tiles <- maptiles::get_tiles(
      x        = map,
      provider = "Esri.WorldGrayCanvas",
      crop     = TRUE,
      project  = TRUE  
    )
    tidyterra::geom_spatraster_rgb(data = tiles)
  }, error = function(e) {
    warning("Failed to fetch Esri.WorldGrayCanvas basemap: ",
            conditionMessage(e),
            ". Proceeding without basemap.", call. = FALSE)
    NULL
  })
}

#' Module File Paths and Result Variable Names
#'
#' @description
#' Configuration mapping each analysis module to its folder name and expected
#' output file names (GPKG, XLSX, RDA log, and PNG directory).
#'
#' @format A named list of lists.
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
#'
#' @description
#' Maps each analysis module to the variable names used inside its result list
#' for the map (`sf`) and table objects.
#'
#' @format A named list of lists, each containing `map` and `table` entries.
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

#' Validate that the Output Directory Exists and is Writable
#'
#' @description
#' Lightweight validator that returns `TRUE` only when a non-empty directory
#' path is supplied and the directory currently exists.
#'
#' @param dir Character string. Directory path to validate.
#'
#' @return Logical. `TRUE` if `dir` is non-empty and exists, otherwise `FALSE`.
#'
#' @keywords internal
validate_output_dir <- function(dir) {
  if (is.null(dir) || !nzchar(dir)) return(FALSE)
  if (!dir.exists(dir)) return(FALSE)
  return(TRUE)
}

#' Special Loading for SERASI Module (Overlap/Adjacent)
#'
#' @description
#' Loads the SERASI module's saved results from disk. Because SERASI output
#' filenames depend on the chosen case (`"overlap"` or `"adjacent"`), this
#' helper inspects the saved `inputs` object in the log RDA to determine which
#' files to load.
#'
#' @param base_dir Character. Base output directory for the SERASI module.
#' @param cfg List. The module's file configuration (as stored in
#'   `module_file_config$serasi`).
#'
#' @return A list with elements `ready` (logical) and, when `ready = TRUE`,
#'   `data` (a list with `inputs` and `result`) and `source` (character).
#'   Returns `list(ready = FALSE, data = NULL)` when the required files are
#'   missing or loading fails.
#'
#' @importFrom sf st_read
#' @importFrom openxlsx read.xlsx
#'
#' @keywords internal
load_serasi_from_files <- function(base_dir, cfg) {
  rda_path <- file.path(base_dir, cfg$rda)
  if (!file.exists(rda_path)) return(list(ready = FALSE, data = NULL))
  
  env <- new.env()
  load(rda_path, envir = env)
  inputs <- env$inputs
  case <- inputs$case  # "overlap" or "adjacent"
  
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

      uploaded_name <- tryCatch(inputs$matriks_serasi_uploaded,
                                error = function(e) NULL)
      if (!is.null(uploaded_name) && length(uploaded_name) == 1 &&
          nzchar(uploaded_name)) {
        candidate_paths <- c(candidate_paths,
                             file.path(log_dir, uploaded_name))
      }

      canonical_name <- sprintf("matriks_serasi_%s_filled.xlsx", case)
      candidate_paths <- c(candidate_paths,
                           file.path(log_dir, canonical_name))

      pattern_files <- list.files(
        log_dir,
        pattern = "^matriks_serasi.*_filled.*\\.xlsx$",
        full.names = TRUE, ignore.case = TRUE
      )
      if (length(pattern_files) > 0) {
        pattern_files <- pattern_files[
          order(file.info(pattern_files)$mtime, decreasing = TRUE)
        ]
        candidate_paths <- c(candidate_paths, pattern_files)
      }

      fallback_files <- list.files(
        log_dir,
        pattern = "^matriks_serasi.*\\.xlsx$",
        full.names = TRUE, ignore.case = TRUE
      )
      if (length(fallback_files) > 0) {
        fallback_files <- fallback_files[
          order(file.info(fallback_files)$mtime, decreasing = TRUE)
        ]
        candidate_paths <- c(candidate_paths, fallback_files)
      }
      
      for (p in candidate_paths) {
        if (!file.exists(p)) next
        parsed <- tryCatch(
          load_validate_matrix_table(p, title = "serasi"),
          error = function(e) NULL
        )
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
#'
#' @description
#' The two "Penyusunan Alternatif" modules (Tumpang Tindih / Bertetangga)
#' write to the same output folder with a case-dependent filename and a
#' case-dependent RDA log:
#'   - overlaps: idx_padan_overlaps_recommendation.{gpkg,xlsx,rda}
#'   - adjacent: idx_padan_adjacent_recommendation.{gpkg,xlsx,rda}
#' This helper inspects the log RDA's `inputs$case` to determine which
#' GPKG/XLSX pair to load, and returns a result list whose variable names
#' match `module_result_names[["recommendation_*"]]`.
#'
#' @param base_dir Character. Base output directory for the module.
#' @param cfg List. The module's file configuration
#'   (as stored in `module_file_config$recommendation`).
#'
#' @return A list with elements `ready` (logical) and, when `ready = TRUE`,
#'   `data` (a list with `inputs` and `result`) and `source` (character).
#'
#' @importFrom sf st_read
#' @importFrom openxlsx read.xlsx
#'
#' @keywords internal
load_recommendation_from_files <- function(base_dir, cfg) {
  rda_candidates <- c(
    file.path(base_dir, "log", "idx_alternatives_overlaps.rda"),
    file.path(base_dir, "log", "idx_alternatives_adjacent.rda")
  )
  rda_existing <- rda_candidates[file.exists(rda_candidates)]
  if (length(rda_existing) == 0) return(list(ready = FALSE, data = NULL))
  
  # Prefer the most recently written log
  rda_path <- rda_existing[order(file.info(rda_existing)$mtime, decreasing = TRUE)][1]
  
  env <- new.env()
  load(rda_path, envir = env)
  inputs <- env$inputs
  case <- inputs$case %||% NA_character_
  
  # Fall back to filename detection if `case` was not recorded
  if (is.na(case) || !nzchar(case)) {
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
  if (!dir.exists(png_dir) ||
      length(list.files(png_dir, pattern = "\\.png$", ignore.case = TRUE)) == 0) {
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
#'
#' @description
#' Reconcile output filenames depend on the reconciliation step:
#' `rtrwp_terintegrasi_overlaps.*` for step 1 (overlaps) or
#' `rtrwp_terintegrasi_adjacent.*` for step 2 (adjacent). This helper
#' inspects the module folder, picks the most recent matching GPKG/XLSX
#' pair, and loads the accompanying log RDA.
#'
#' @param base_dir Character. Base output directory for the Reconcile module.
#' @param cfg List. The module's file configuration
#'   (as stored in `module_file_config$reconcile`).
#'
#' @return A list with elements `ready` (logical) and, when `ready = TRUE`,
#'   `data` (a list with `inputs` and `result`) and `source` (character).
#'   Returns `list(ready = FALSE, data = NULL)` when the required files are
#'   missing or loading fails.
#'
#' @importFrom sf st_read
#' @importFrom openxlsx read.xlsx
#'
#' @keywords internal
load_reconcile_from_files <- function(base_dir, cfg) {
  rda_path <- file.path(base_dir, cfg$rda)
  if (!file.exists(rda_path)) return(list(ready = FALSE, data = NULL))
  
  png_dir <- file.path(base_dir, cfg$png_dir)
  if (!dir.exists(png_dir) ||
      length(list.files(png_dir, pattern = "\\.png$", ignore.case = TRUE)) == 0) {
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
  
  # Prefer the most recent file
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
#'
#' @description
#' Determines whether a module has completed results available, preferring
#' in-memory results stored in `session$userData$module_results` and falling
#' back to reading the module's output files from `output_dir`.
#'
#' @param module_id Character. The module identifier. May include a parent and
#'   child separated by `"$"` (e.g. `"parent$child"`).
#' @param output_dir Character. Root output directory.
#' @param session Shiny session object, used to look up in-memory results.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{ready}{Logical. Whether the module results are available.}
#'     \item{data}{The module result payload when `ready = TRUE`, else `NULL`.}
#'     \item{source}{Either `"memory"`, `"files"`, or `NULL`.}
#'   }
#'
#' @importFrom sf st_read
#' @importFrom openxlsx read.xlsx
#'
#' @keywords internal
module_ready_and_data <- function(module_id, output_dir, session) {
  mod_key <- module_id
  parent <- NULL
  child <- NULL
  
  if (!is.null(module_id) && grepl("$", module_id, fixed = TRUE)) {
    parts <- strsplit(module_id, "$", fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      parent <- parts[1]
      child <- parts[2]
      mod_key <- child
    } else {
      mod_key <- module_id
    }
  } else {
    mod_key <- module_id
  }
  
  mem_data <- session$userData$module_results[[mod_key]]
  if (!is.null(mem_data) && length(mem_data) > 0) {
    return(list(ready = TRUE, data = mem_data, source = "memory"))
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
        uploaded_name <- tryCatch(inputs$matriks_padu_ke_uploaded,
                                  error = function(e) NULL)
        if (!is.null(uploaded_name) && length(uploaded_name) == 1 &&
            nzchar(uploaded_name)) {
          candidate_paths <- c(candidate_paths,
                               file.path(log_dir, uploaded_name))
        }
        candidate_paths <- c(candidate_paths,
                             file.path(log_dir, "matriks_padu_ke_filled.xlsx"))
        pattern_files <- list.files(
          log_dir,
          pattern = "^matriks_padu_ke.*_filled.*\\.xlsx$",
          full.names = TRUE, ignore.case = TRUE
        )
        if (length(pattern_files) > 0) {
          pattern_files <- pattern_files[
            order(file.info(pattern_files)$mtime, decreasing = TRUE)
          ]
          candidate_paths <- c(candidate_paths, pattern_files)
        }
        for (p in candidate_paths) {
          if (!file.exists(p)) next
          parsed <- tryCatch(
            load_validate_matrix_table(p, title = "padu_ke"),
            error = function(e) NULL
          )
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
#'
#' @description
#' Reads an R Markdown template, strips its YAML front matter, and knits the
#' remaining body as a child document using the supplied module parameters.
#'
#' @param template_path Character. Path to the Rmd template file.
#' @param module_params List. Parameters to expose to the child document via
#'   `params`.
#' @param envir Environment in which to evaluate the child. Defaults to the
#'   parent frame.
#'
#' @return A character vector/string containing the rendered Markdown.
#'
#' @importFrom knitr knit_child
#'
#' @keywords internal
knit_child_module <- function(template_path, module_params, envir = parent.frame()) {
  abs_path <- here::here(template_path)
  if (!file.exists(abs_path)) {
    return(paste0("\n\n*Template tidak ditemukan: ", template_path, " (at ", abs_path, ")*\n\n"))
  }
  
  # Read the template file
  lines <- readLines(abs_path, warn = FALSE)
  
  # Find the YAML front matter (between --- lines)
  yaml_start <- which(lines == "---")[1]
  yaml_end <- which(lines == "---")[2]
  
  if (!is.na(yaml_start) && !is.na(yaml_end) && yaml_start < yaml_end) {
    # Remove the YAML front matter
    body_lines <- lines[-(yaml_start:yaml_end)]
  } else {
    body_lines <- lines
  }
  
  # Combine into a single string
  body_text <- paste(body_lines, collapse = "\n")
  
  # Create a new environment with the params set
  child_env <- new.env(parent = envir)
  child_env$params <- module_params
  
  # Knit the body text with the child environment
  # quiet = TRUE suppresses the child's progress bar / processing messages,
  # which would otherwise be captured as stdout inside this chunk's output.
  result <- knitr::knit_child(text = body_text, envir = child_env, quiet = TRUE)
  
  return(result)
}

#' Generate a Module or Master Report
#'
#' @description
#' Renders an HTML report for a specific module or, when `master_params` is
#' supplied, renders the master report template. Falls back to a minimal
#' placeholder template when the module-specific template cannot be found.
#'
#' @param output The output object returned by a module (expected to contain
#'   `inputs` and `result` elements).
#' @param dir Character. Directory where the rendered HTML report will be
#'   written.
#' @param module_name Character. Optional module name used in the output file
#'   name and passed to the template as `module_name`.
#' @param template_path Character. Path to the module-specific Rmd template.
#'   Defaults to the SERASI report template.
#' @param master_params List. When provided, forces rendering of the master
#'   report template with these parameters.
#'
#' @return Invisibly `NULL`. Called for its side effect of rendering an HTML
#'   report.
#'
#' @importFrom rmarkdown render yaml_front_matter
#'
#' @export
generate_report <- function(output, dir, module_name = NULL,
                            template_path = "report/LaSPUR_SERASI_report_template.Rmd",
                            master_params = NULL) {
  
  # Ensure Pandoc is available in this execution context
  .ensure_pandoc_available()
  
  # Check if output directory is writable and has space
  if (!dir.exists(dir)) {
    stop("Output directory does not exist: ", dir)
  }
  
  # Simple write test to check disk space
  test_file <- file.path(dir, ".write_test_tmp")
  tryCatch({
    writeLines("test", test_file)
    unlink(test_file)
  }, error = function(e) {
    stop("Cannot write to output directory. Check disk space and permissions: ", dir)
  })
  
  # If master_params is provided, use the master template
  if (!is.null(master_params)) {
    # Use the master template (override template_path if provided)
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
  
  # Fallback for modules that don't have a template yet
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
  
  # Prepare inputs payload
  inputs_payload <- output$inputs
  if (is.null(inputs_payload) || !is.list(inputs_payload)) {
    inputs_payload <- list()
  }
  
  # Build all possible parameters
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

# Numeric display helper — fixed 2 decimals, padded with trailing zeros
fmt2 <- function(x) {
  if (inherits(x, "units")) x <- as.numeric(x)
  if (!is.numeric(x)) return(x)
  ifelse(
    is.na(x),
    NA_character_,
    formatC(x, format = "f", digits = 2)
  )
}

laspur_rename_cols <- function(df) {
  mapping <- c(
    "id_pu" = "ID PU",
    "RTRW" = "RTRW",
    "RZWP3K" = "RZWP3K",
    "area_ha" = "Luas (ha)",
    "idx_serasi" = "Indeks SERASI",
    "idx_padu_final" = "Indeks PADU",
    "idx_padan" = "Indeks PADAN",
    "recommendation" = "Rekomendasi",
    "idx_padan_new" = "PADAN Proyeksi",
    "delta_idx_padan" = "Kenaikan (Delta)"
  )
  
  current_names <- names(df)
  new_names <- current_names
  
  for (i in seq_along(current_names)) {
    if (current_names[i] %in% names(mapping)) {
      new_names[i] <- mapping[current_names[i]]
    }
  }
  
  names(df) <- new_names
  df
}

laspur_format_numbers <- function(df, numeric_cols = NULL, integer_cols = NULL) {
  if (is.null(df)) return(NULL)
  
  if (!is.null(numeric_cols)) {
    for (col in numeric_cols) {
      if (col %in% names(df)) {
        df[[col]] <- fmt2(as.numeric(df[[col]]))
      }
    }
  }
  
  if (!is.null(integer_cols)) {
    for (col in integer_cols) {
      if (col %in% names(df)) {
        df[[col]] <- as.character(as.integer(df[[col]]))
      }
    }
  }
  df
}

# open folder helper
open_folder_crossplatform <- function(path) {
  if (is.null(path) || length(path) != 1 || !nzchar(path) || !dir.exists(path)) {
    showNotification("Direktori belum tersedia.", type = "warning", duration = 5)
    return(invisible(FALSE))
  }
  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  
  tryCatch({
    os <- .Platform$OS.type
    if (os == "windows") {
      shell.exec(path_norm)
    } else if (os == "unix") {
      sysname <- Sys.info()[["sysname"]]
      if (sysname == "Darwin") {
        system2("open", shQuote(path_norm), wait = FALSE)
      } else {
        system2("xdg-open", shQuote(path_norm), wait = FALSE)
      }
    } else {
      showNotification("Sistem operasi tidak dikenali.", type = "warning")
      return(invisible(FALSE))
    }
    invisible(TRUE)
  }, error = function(e) {
    showNotification(paste("Gagal membuka folder:", conditionMessage(e)),
                     type = "error", duration = 5)
    invisible(FALSE)
  })
}

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

# ==============================================================================
# Execution & Timing Utilities
# ==============================================================================

#' Run Expression in Parallel with Sequential Fallback
#'
#' @description
#' Executes `parallel_fn()` inside `tryCatch()`; if it errors, emits a warning
#' and falls back to `sequential_fn()`.
#'
#' @param label Character label identifying the step in warning messages.
#' @param parallel_fn Zero-argument function executing the parallel path.
#' @param sequential_fn Zero-argument function executing the sequential fallback.
#'
#' @return The return value of `parallel_fn()`, or `sequential_fn()` on error.
#' @keywords internal
.run_with_parallel_fallback <- function(label, parallel_fn, sequential_fn) {
  tryCatch(
    parallel_fn(),
    error = function(e) {
      warning(sprintf("[%s] Eksekusi paralel gagal (%s); beralih ke mode sekuensial.",
                      label, conditionMessage(e)), call. = FALSE)
      sequential_fn()
    }
  )
}

.timing_state <- new.env(parent = emptyenv())
.timing_state$rows <- list()

#' Reset Execution Timing Log
#'
#' @description
#' Clears accumulated step timing records stored in `.timing_state`.
#'
#' @return Invisibly returns `NULL`.
#' @export
reset_timing_log <- function() {
  .timing_state$rows <- list()
  invisible(NULL)
}

#' Execute and Time a Pipeline Step
#'
#' @description
#' Evaluates `expr`, logs start/end timestamps and elapsed duration into
#' `.timing_state$rows`, and prints progress messages.
#'
#' @param step_name Character label for the pipeline step.
#' @param expr Expression to evaluate.
#'
#' @return The result of evaluating `expr`.
#' @export
run_timed_step <- function(step_name, expr) {
  t0 <- Sys.time()
  message(sprintf("[%s] Mulai: %s ...", format(t0, "%Y-%m-%d %H:%M:%S"), step_name))
  res <- force(expr)
  t1 <- Sys.time()
  sec <- as.numeric(difftime(t1, t0, units = "secs"))
  .timing_state$rows[[length(.timing_state$rows) + 1L]] <- tibble::tibble(
    step        = step_name,
    started_at  = format(t0, "%Y-%m-%d %H:%M:%S"),
    finished_at = format(t1, "%Y-%m-%d %H:%M:%S"),
    elapsed_sec = round(sec, 2),
    elapsed_min = round(sec / 60, 2)
  )
  message(sprintf("[%s] Selesai: %s (%.2f detik / %.2f menit)",
                  format(t1, "%Y-%m-%d %H:%M:%S"), step_name, sec, sec / 60))
  res
}

#' Save Execution Timing Log to Excel and CSV
#'
#' @description
#' Aggregates recorded step timings from `.timing_state$rows`, appends a
#' `TOTAL_TERCATAT` summary row, and writes both `.xlsx` and `.csv` files into
#' `output_dir`.
#'
#' @param output_dir Character directory path where timing files will be saved.
#' @param filename Character Excel filename. Default `"processing_time_log.xlsx"`.
#'
#' @return A `tibble` of recorded step timings including the total row, or
#'   `NULL` invisibly if no steps were recorded.
#' @export
save_timing_log <- function(output_dir, filename = "processing_time_log.xlsx") {
  if (length(.timing_state$rows) == 0L) return(invisible(NULL))
  df <- dplyr::bind_rows(.timing_state$rows)
  tot_sec <- sum(df$elapsed_sec, na.rm = TRUE)
  summary_df <- dplyr::bind_rows(
    df,
    tibble::tibble(
      step        = "TOTAL_TERCATAT",
      started_at  = df$started_at[1],
      finished_at = df$finished_at[nrow(df)],
      elapsed_sec = round(tot_sec, 2),
      elapsed_min = round(tot_sec / 60, 2)
    )
  )
  xlsx_path <- file.path(output_dir, filename)
  openxlsx::write.xlsx(summary_df, xlsx_path)
  utils::write.csv(summary_df, sub("\\.xlsx$", ".csv", xlsx_path), row.names = FALSE)
  summary_df
}
