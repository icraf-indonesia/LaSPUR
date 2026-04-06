# List of helpers function that repeatedly use among modules

# Load and validate input data --------------------------------------------

# 1. load&validate_shapefile()

#' Load and Validate a Shapefile
#'
#' @description
#' Loads a shapefile (.shp) and performs a series of validation checks
#' including file completeness, geometry, properties, and attribute table
#' integrity.
#'
#' @param shp_path \code{character} Path to the shapefile (.shp) to be loaded.
#'
#' @return A vector object in \code{sf} format that has been validated and is
#'   ready for spatial analysis.
#'
#' @details
#' The function performs four sequential validation steps:
#' \enumerate{
#'   \item \strong{Shapefile completeness} — Verifies that all mandatory
#'     companion files (\code{.shp}, \code{.shx}, \code{.dbf}) exist in the
#'     same directory.
#'   \item \strong{Read shapefile} — Reads the file using
#'     \code{\link[sf]{st_read}} from the \code{sf} package.
#'   \item \strong{Geometry and properties check} — Validates geometry
#'     presence, detects invalid geometries, and checks for a defined CRS.
#'   \item \strong{Attribute table check} — Checks attribute column
#'     completeness and detects missing (NA) values.
#' }
#'
#' @note
#' Invalid geometries can be repaired using \code{sf::st_make_valid()}.
#' The function issues \code{warning()} for non-fatal issues and calls
#' \code{stop()} for errors that prevent data from being read.
#'
#' @examples
#' \dontrun{
#' # Load an administrative boundary shapefile
#' province_boundary <- load_and_validate_shapefile("data/province_boundary.shp")
#'
#' # Inspect the resulting object
#' print(province_boundary)
#' sf::st_crs(province_boundary)
#' }
#'
#' @seealso
#' \code{\link[sf]{st_read}}, \code{\link[sf]{st_is_valid}},
#' \code{\link[sf]{st_make_valid}}
#'
#' @importFrom sf st_read st_geometry st_is_valid st_crs st_drop_geometry st_geometry_type
#' @importFrom tools file_path_sans_ext
#'
#' @export
load_and_validate_shapefile <- function(shp_path) {
  library(sf)
  
  # 1. Check shapefile file completeness
  required_extensions <- c(".shp", ".shx", ".dbf", ".prj")
  base_path <- tools::file_path_sans_ext(shp_path)
  
  missing_files <- required_extensions[
    !file.exists(paste0(base_path, required_extensions))
  ]
  
  if (length(missing_files) > 0) {
    stop(paste(
      "Incomplete shapefile. The following required files are missing:",
      paste(missing_files, collapse = ", ")
    ))
  }
  
  # 2. Read shapefile
  message(">> Reading shapefile: ", shp_path, " ...")
  sf_object <- tryCatch(
    sf::st_read(shp_path, quiet = TRUE),
    error = function(e) {
      stop("Failed to read shapefile. Error details: ", e$message)
    }
  )
  message("   Shapefile successfully read.")
  
  # 3. Check geometry and properties
  message(">> Checking geometry and properties ...")
  
  if (is.null(sf::st_geometry(sf_object))) {
    stop("Shapefile contains no geometry. The file may be corrupted or empty.")
  }
  
  invalid_geom <- !sf::st_is_valid(sf_object)
  if (any(invalid_geom, na.rm = TRUE)) {
    warning(
      sum(invalid_geom, na.rm = TRUE),
      " invalid geometry/geometries detected. ",
      "Consider repairing them with sf::st_make_valid()."
    )
  } else {
    message("   All geometries are valid.")
  }
  
  if (is.na(sf::st_crs(sf_object))) {
    warning(
      "No CRS (Coordinate Reference System) is set on this shapefile. ",
      "Use sf::st_set_crs() to assign the appropriate CRS."
    )
  } else {
    message("   CRS detected: ", sf::st_crs(sf_object)$input)
  }
  
  # 4. Check attribute table completeness
  message(">> Checking attribute table completeness ...")
  
  if (ncol(sf_object) <= 1) {
    warning(
      "Attribute table is empty. The shapefile has no attribute columns besides geometry."
    )
  }
  
  na_counts <- sapply(
    sf::st_drop_geometry(sf_object),
    function(col) sum(is.na(col))
  )
  cols_with_na <- na_counts[na_counts > 0]
  
  if (length(cols_with_na) > 0) {
    warning(
      "NA values found in the following columns: ",
      paste(
        paste0(names(cols_with_na), " (", cols_with_na, " NA)"),
        collapse = ", "
      )
    )
  } else {
    message("   No NA values found in the attribute table.")
  }
  
  message(
    "\n[DONE] Shapefile successfully loaded and validated.",
    "\n  - Number of features : ", nrow(sf_object),
    "\n  - Number of columns  : ", ncol(sf_object) - 1, " (excluding geometry)",
    "\n  - Geometry type      : ", as.character(sf::st_geometry_type(sf_object, by_geometry = FALSE))
  )
  
  return(sf_object)
}

# 2. load&validate_table()

#' Load and Validate an Excel Table
#'
#' @description
#' Loads an Excel table (\code{.xlsx} or \code{.xls}) and performs a series
#' of validation checks on the file extension, table structure, and the
#' completeness of rows and column contents.
#'
#' @param table_path \code{character} Path to the Excel file (\code{.xlsx} or
#'   \code{.xls}) to be loaded.
#'
#' @return A table object in \code{tibble} format that has been validated and
#'   is ready for data analysis.
#'
#' @details
#' The function performs three sequential validation steps:
#' \enumerate{
#'   \item \strong{File extension check} — Ensures the file has a \code{.xlsx}
#'     or \code{.xls} extension. Any other extension will stop execution.
#'   \item \strong{Read table} — Reads the file using
#'     \code{\link[openxlsx]{read.xlsx}} and converts it to a \code{tibble}.
#'   \item \strong{Column and row completeness check} — Verifies the presence
#'     of rows and columns, detects fully empty columns, NA values per column,
#'     and duplicated rows.
#' }
#'
#' @note
#' This function requires the \code{openxlsx} and \code{tibble} packages.
#' Ensure both are installed before use. Duplicate rows can be removed using
#' \code{dplyr::distinct()}.
#'
#' @examples
#' \dontrun{
#' # Load a population data table
#' population_data <- load_and_validate_table("data/population_2023.xlsx")
#'
#' # Inspect the first few rows
#' head(population_data)
#'
#' # Check table structure
#' dplyr::glimpse(population_data)
#' }
#'
#' @seealso
#' \code{\link[openxlsx]{read.xlsx}}, \code{\link[tibble]{as_tibble}},
#' \code{\link[dplyr]{distinct}}
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom tibble as_tibble
#' @importFrom tools file_ext
#'
#' @export
load_and_validate_table <- function(table_path) {
  library(openxlsx)
  library(tibble)
  
  # 1. Check file extension is .xlsx or .xls
  extension <- tolower(tools::file_ext(table_path))
  
  if (!extension %in% c("xlsx", "xls")) {
    stop(paste0(
      "Invalid file extension: '.", extension, "'. ",
      "Only .xlsx or .xls files are accepted."
    ))
  }
  
  if (!file.exists(table_path)) {
    stop("File not found at the specified path: ", table_path)
  }
  
  # 2. Read table
  message(">> Reading table: ", table_path, " ...")
  raw_table <- tryCatch(
    openxlsx::read.xlsx(table_path),
    error = function(e) {
      stop("Failed to read Excel file. Error details: ", e$message)
    }
  )
  
  tbl_object <- tibble::as_tibble(raw_table)
  message("   Table successfully read and converted to tibble format.")
  
  # 3. Check column and row completeness and table content
  message(">> Checking column and row completeness ...")
  
  if (ncol(tbl_object) == 0) {
    stop("The table has no columns. The file may be empty or corrupted.")
  }
  
  if (nrow(tbl_object) == 0) {
    stop("The table has no data rows. The file may only contain a header row.")
  }
  
  # Check for fully empty columns
  empty_cols <- sapply(tbl_object, function(col) all(is.na(col)))
  if (any(empty_cols)) {
    warning(
      "Fully empty columns detected (all values are NA): ",
      paste(names(tbl_object)[empty_cols], collapse = ", ")
    )
  }
  
  # Check for NA values per column (excluding fully empty ones)
  na_counts <- sapply(tbl_object, function(col) sum(is.na(col)))
  cols_with_na <- na_counts[na_counts > 0 & !empty_cols]
  
  if (length(cols_with_na) > 0) {
    warning(
      "NA values found in the following columns: ",
      paste(
        paste0(names(cols_with_na), " (", cols_with_na, " NA)"),
        collapse = ", "
      )
    )
  } else {
    message("   No NA values found in the table.")
  }
  
  # Check for duplicate rows
  n_duplicates <- sum(duplicated(tbl_object))
  if (n_duplicates > 0) {
    warning(
      n_duplicates, " duplicate row(s) found in the table. ",
      "Consider removing them using dplyr::distinct()."
    )
  } else {
    message("   No duplicate rows found.")
  }
  
  message(
    "\n[DONE] Table successfully loaded and validated.",
    "\n  - Number of rows    : ", nrow(tbl_object),
    "\n  - Number of columns : ", ncol(tbl_object)
  )
  
  return(tbl_object)
}

# 3. load&validate_matrix_table()
# 4. load&validate_raster()

# 5. export_table()


# 6. export_map()
# 7. generate_report()