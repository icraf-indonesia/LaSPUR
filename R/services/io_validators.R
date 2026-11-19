# R/services/io_validators.R
# ============================================================
# Input/Output Data Loaders and Schema Validators
# ============================================================
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
  if (is.null(dir_path) || !nzchar(dir_path)) {
    return(FALSE)
  }
  if (!dir.exists(dir_path)) {
    return(FALSE)
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
