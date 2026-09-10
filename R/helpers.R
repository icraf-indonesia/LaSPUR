# Load libraries
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

library(pacman)

pacman::p_load(
  terra,
  sf,
  openxlsx,
  tibble,
  dplyr,
  landscapemetrics,
  purrr,
  tidyr,
  stringr,
  exactextractr,
  here,
  rmarkdown,
  kableExtra,
  DT,
  units,
  utils,
  furrr,
  future,
  data.table,
  leaflet,
  leaflet.extras,
  htmltools,
  readxl,
  shinyjs,
  shinyFiles,
  promises,
  bslib
)

#' Load and Validate a Shapefile
#'
#' @description
#' Loads a shapefile (.shp) and performs validation checks including file
#' completeness, geometry, properties, and attribute table integrity.
#'
#' @param shp_path Character. Path to the shapefile (.shp) to be loaded.
#'
#' @return An `sf` object that has been validated and is ready for analysis.
#'
#' @details
#' Validation steps:
#' \enumerate{
#'   \item Checks that .shp, .shx, .dbf, and .prj files exist
#'   \item Reads the shapefile using `sf::st_read()`
#'   \item Validates geometry presence and checks for invalid geometries
#'   \item Checks for defined CRS and missing values in attribute table
#' }
#'
#' @examples
#' \dontrun{
#' province_boundary <- load_and_validate_shapefile("data/province_boundary.shp")
#' print(province_boundary)
#' }
#'
#' @importFrom sf st_read st_geometry st_is_valid st_crs st_drop_geometry
#'   st_geometry_type
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
  
  invalid_geom <- !sf::st_is_valid(sf_object)
  if (any(invalid_geom, na.rm = TRUE)) {
    warning(sum(invalid_geom, na.rm = TRUE), " invalid geometries detected. ",
            "Consider using sf::st_make_valid().")
  } else {
    message("   All geometries are valid.")
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
  if (!extension %in% c("tif", "tiff")) {
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
    if (any(is.na(ext)) || ext$xmin >= ext$xmax || ext$ymin >= ext$ymax) {
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

#' Create Result Visualization UI
#'
#' @description
#' Builds the shared Shiny UI used across modules to display analysis results:
#' a Leaflet map, a DT table, a validation log, and download buttons.
#'
#' @param ns Namespace function of the module calling this helper.
#'
#' @return A Shiny UI object containing the map, table, log, and download
#'   buttons.
#'
#' @importFrom shiny tagList fluidRow column div hr downloadButton
#' @importFrom bslib navset_tab nav_panel
#' @importFrom leaflet leafletOutput
#' @importFrom DT DTOutput
#'
#' @export
create_result_ui <- function(ns) {
  tagList(
    navset_tab(
      nav_panel(
        "Visualisasi Hasil",
        fluidRow(
          column(
            width = 12,
            style = "margin-top: 10px;",
            leafletOutput(ns("result_map"), height = "450px")
          )
        ),
        hr(style = "margin: 15px 0; border-top: 1px solid #dee2e6;"),
        fluidRow(
          column(
            width = 12,
            div(
              style = "max-height: 400px; overflow-y: auto;",
              DT::DTOutput(ns("result_table"))
            )
          )
        )
      ),
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
#'     \item{map_palette}{Palette name (e.g., `"RdYlGn"`). Default `"RdYlGn"`.}
#'     \item{map_simplify_tolerance}{Simplification tolerance (map units) used
#'       only for the Leaflet display copy of the geometry. Default `5`.}
#'     \item{table_cols}{Named vector for subsetting/renaming table columns.}
#'     \item{table_round_cols}{Character vector of display column names to round
#'       to 2 digits.}
#'   }
#'
#' @return Invisibly `NULL`. Called for its side effects of registering
#'   outputs and observers on `output` and `session`.
#'
#' @importFrom shiny req renderPrint observeEvent observe invalidateLater
#' @importFrom leaflet renderLeaflet leaflet addProviderTiles addPolygons
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
  
  # Default configurations
  map_color_col <- config$map_color_col
  map_title <- config$map_title
  map_palette <- if(!is.null(config$map_palette)) config$map_palette else "RdYlGn"
  table_cols <- config$table_cols
  table_round_cols <- config$table_round_cols
  
  # Simplification tolerance (map units, typically meters for UTM data) used only for the Leaflet display copy of the geometry.
  map_simplify_tolerance <- if (!is.null(config$map_simplify_tolerance)) {
    config$map_simplify_tolerance
  } else {
    5 # meters
  }
  
  output$result_map <- renderLeaflet({
    req(rv$analysis_result)
    
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
        val <- row[[col]]
        if (is.numeric(val)) val <- round(val, 3)
        res <- paste0(res, "<b>", name, "</b>: ", val, "<br>")
      }
      res
    }
    
    create_label <- function(row) {
      res <- ""
      for (name in names(config$map_label_cols)) {
        col <- config$map_label_cols[[name]]
        val <- row[[col]]
        if (is.numeric(val)) val <- round(val, 2)
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
      pal <- leaflet::colorNumeric(
        palette = map_palette,
        domain  = map_sf[[map_color_col]],
        na.color = "transparent"
      )
    } else {
      pal <- leaflet::colorFactor(
        palette = map_palette,
        domain  = map_sf[[map_color_col]],
        na.color = "transparent"
      )
    }
    
    leaflet::leaflet(
      map_sf,
      options = leafletOptions(preferCanvas = TRUE)
    ) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
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
        values   = as.formula(paste0("~`", map_color_col, "`")),
        title    = map_title,
        opacity  = 0.7
      )
  })
  
  output$result_table <- DT::renderDT({
    req(rv$analysis_result)
    
    df <- rv$analysis_result$table
    
    # Subset and rename columns
    valid_cols <- names(table_cols)[names(table_cols) %in% colnames(df)]
    df_subset <- df[, valid_cols, drop = FALSE]
    colnames(df_subset) <- table_cols[valid_cols]
    
    dt <- DT::datatable(
      df_subset,
      selection = "single",
      extensions = c('FixedColumns', 'FixedHeader'),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'Bfrtip',
        fixedColumns = list(
          leftColumns = 3
        ),
        fixedHeader = TRUE
      ),
      rownames = FALSE,
      class = "display compact stripe hover"
    )
    
    # Round specified columns
    if (!is.null(table_round_cols) && length(table_round_cols) > 0) {
      valid_round_cols <- table_round_cols[table_round_cols %in% colnames(df_subset)]
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
        popup = lapply(
          popup_text,
          htmltools::HTML
        )
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
#'   as a PNG to that path as a side effect.
#'
#' @importFrom ggplot2 ggplot aes geom_sf scale_fill_gradient scale_color_gradient
#'   scale_x_continuous theme_bw labs coord_sf guides guide_colorbar theme
#'   element_blank element_text margin ggsave
#' @importFrom tidyterra geom_spatraster
#' @importFrom sf st_zm st_make_valid st_is_empty st_geometry_type
#' @importFrom scales breaks_pretty
#' @importFrom grid unit
#'
#' @export
plot_continuous_map <- function(map, title = NULL, column = NULL, legend, low, high, na_color = "white",
                                filepath = NULL, width = 7, height = 5, dpi = 300) {
  
  if (inherits(map, "SpatRaster")) {
    
    plot_lc <- ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = map) +
      ggplot2::scale_fill_gradient(
        low = low,
        high = high,
        na.value = na_color,
        name = if (!is.null(legend)) legend else NULL
      )
    
  } else if (inherits(map, "sf")) {
    
    if (is.null(column)) {
      stop("`column` must be specified when `map` is an sf object.")
    }
    if (!column %in% names(map)) {
      stop(sprintf("Column '%s' not found in `map`.", column))
    }
    
    map <- sf::st_zm(map, drop = TRUE, what = "ZM")
    map <- sf::st_make_valid(map)
    map <- map[!sf::st_is_empty(map), ]
    map[[column]] <- as.numeric(map[[column]])
    map[[column]][!is.finite(map[[column]])] <- NA
    
    geom_types <- unique(as.character(sf::st_geometry_type(map)))
    is_polygon <- any(grepl("POLYGON", geom_types))
    
    if (is_polygon) {
      plot_lc <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = map, ggplot2::aes(fill = .data[[column]]), color = NA) +
        ggplot2::scale_fill_gradient(
          low = low,
          high = high,
          na.value = na_color,
          name = if (!is.null(legend)) legend else NULL
        )
    } else {
      plot_lc <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = map, ggplot2::aes(color = .data[[column]])) +
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
  
  plot_lc <- plot_lc +
    ggplot2::theme_bw() +
    ggplot2::labs(fill = NULL, title = title) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
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
  
  if (!is.null(filepath)) {
    ggplot2::ggsave(filename = filepath, plot = plot_lc, width = width, height = height, dpi = dpi)
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
#'   as a PNG to that path as a side effect.
#'
#' @importFrom ggplot2 ggplot aes geom_sf scale_fill_manual scale_color_manual
#'   scale_x_continuous theme_bw labs coord_sf guides guide_legend theme
#'   element_blank element_text margin ggsave
#' @importFrom tidyterra geom_spatraster
#' @importFrom sf st_zm st_make_valid st_is_empty st_geometry_type
#' @importFrom scales hue_pal label_wrap breaks_pretty
#' @importFrom grid unit
#' @importFrom terra levels
#' @importFrom stats setNames
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#'
#' @export
plot_categorical_map <- function(map, title = NULL, column = NULL, lookup = NULL,
                                 id_col = "ID", class_col = "class", legend = NULL, colors = NULL,
                                 na_color = "white", label_wrap_width = 30, legend_ncol = 1,
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
  
  if (inherits(map, "SpatRaster")) {
    
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
    terra::levels(map) <- cat_df
    
    class_labels <- as.character(terra::levels(map)[[1]][[2]])
    
    if (is.null(colors)) {
      pal <- stats::setNames(scales::hue_pal()(length(class_labels)), class_labels)
    } else {
      pal <- colors
    }
    
    plot_lc <- ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = map) +
      ggplot2::scale_fill_manual(
        values   = pal,
        na.value = na_color,
        labels   = scales::label_wrap(label_wrap_width),
        name     = if (!is.null(legend)) legend else NULL
      )
    
  } else if (inherits(map, "sf")) {
    
    if (is.null(column) || is.na(column) || column == "") {
      map <- sf::st_zm(map, drop = TRUE, what = "ZM")
      map <- sf::st_make_valid(map)
      map <- map[!sf::st_is_empty(map), ]
      
      geom_types <- unique(as.character(sf::st_geometry_type(map)))
      is_polygon <- any(grepl("POLYGON", geom_types))
      
      if (is_polygon) {
        plot_lc <- ggplot2::ggplot() +
          ggplot2::geom_sf(data = map, fill = "lightblue", color = "darkblue", size = 0.2)
      } else {
        plot_lc <- ggplot2::ggplot() +
          ggplot2::geom_sf(data = map, color = "darkblue")
      }
      
    } else {
      if (!column %in% names(map)) {
        stop(sprintf("Column '%s' not found in `map`.", column))
      }
      
      map <- sf::st_zm(map, drop = TRUE, what = "ZM")
      map <- sf::st_make_valid(map)
      map <- map[!sf::st_is_empty(map), ]
      
      map[[column]] <- factor(map[[column]])
      class_labels <- levels(map[[column]])
      
      if (is.null(colors)) {
        pal <- stats::setNames(scales::hue_pal()(length(class_labels)), class_labels)
      } else {
        pal <- colors
      }
      
      geom_types <- unique(as.character(sf::st_geometry_type(map)))
      is_polygon <- any(grepl("POLYGON", geom_types))
      
      if (is_polygon) {
        plot_lc <- ggplot2::ggplot() +
          ggplot2::geom_sf(data = map, ggplot2::aes(fill = .data[[column]]), color = NA) +
          ggplot2::scale_fill_manual(
            values   = pal,
            na.value = na_color,
            labels   = scales::label_wrap(label_wrap_width),
            name     = if (!is.null(legend)) legend else NULL
          )
      } else {
        plot_lc <- ggplot2::ggplot() +
          ggplot2::geom_sf(data = map, ggplot2::aes(color = .data[[column]])) +
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
  
  plot_lc <- plot_lc +
    ggplot2::theme_bw() +
    ggplot2::labs(title = title) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
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
  
  if (inherits(map, "sf") && (is.null(column) || is.na(column) || column == "")) {
    plot_lc <- plot_lc + ggplot2::theme(legend.position = "none")
  } else {
    plot_lc <- plot_lc +
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
  
  if (!is.null(filepath)) {
    ggplot2::ggsave(filename = filepath, plot = plot_lc, width = width, height = height, dpi = dpi)
  }
  
  return(plot_lc)
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
  recommendation_overlaps = list(
    folder  = "Analisis Alternatif",
    gpkg    = "idx_alternative_overlaps.gpkg",
    xlsx    = "idx_alternative_overlaps.xlsx",
    rda     = "log/idx_alternative_overlaps_log.rda",
    png_dir = "log"
  ),
  recommendation_adjacent = list(
    folder  = "Analisis Alternatif",
    gpkg    = "idx_alternative_adjacent.gpkg",
    xlsx    = "idx_alternative_adjacent.xlsx",
    rda     = "log/idx_alternative_adjacent_log.rda",
    png_dir = "log"
  ),
  reconcile = list(
    folder  = "Analisis Rekonsiliasi",
    gpkg    = "idx_reconcile.gpkg",
    xlsx    = "idx_reconcile.xlsx",
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
  recommendation_overlaps = list(map = "idx_alternative_overlaps_map", table = "idx_alternative_overlaps_table"),
  recommendation_adjacent = list(map = "idx_alternative_adjacent_map", table = "idx_alternative_adjacent_table"),
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
    
    matriks_xlsx <- file.path(base_dir, "matriks_serasi_input.xlsx")
    if (file.exists(matriks_xlsx)) {
      result$matriks_serasi <- openxlsx::read.xlsx(matriks_xlsx)
    }
    
    out <- list(inputs = inputs, result = result)
    return(list(ready = TRUE, data = out, source = "files"))
  }, error = function(e) {
    warning("Failed to load SERASI from files: ", e$message)
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
  if (!file.exists(template_path)) {
    return(paste0("\n\n*Template tidak ditemukan: ", template_path, "*\n\n"))
  }
  
  # Read the template file
  lines <- readLines(template_path, warn = FALSE)
  
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
  
  # If master_params is provided, use the master template
  if (!is.null(master_params)) {
    # Use the master template (override template_path if provided)
    template_path <- "report/LaSPUR_master_report_template.Rmd"
    if (!file.exists(template_path)) {
      stop("Master template not found: ", template_path)
    }
    
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
    output_file <- paste0("LaSPUR_Master_Report_", timestamp, ".html")
    
    rmarkdown::render(
      input         = template_path,
      output_format = "html_document",
      output_file   = output_file,
      output_dir    = dir,
      params        = master_params,
      knit_root_dir = getwd(),
      quiet         = TRUE
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
  
  rmarkdown::render(
    input         = template_path,
    output_format = "html_document",
    output_file   = output_file,
    output_dir    = dir,
    params        = report_params,
    knit_root_dir = getwd(),
    quiet         = TRUE
  )
}