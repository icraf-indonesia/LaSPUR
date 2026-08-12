# Helper Functions for Data Loading and Validation ------------------------

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

# 5. export_table()
# 6. export_map()
# 7. generate_report()

#' Generate LaSPUR Report
#' 
#' Generates a report for the LaSPUR using R Markdown.
#'
#' @param output List. Output from LaSPUR module.
#' @param dir Character string. Directory to save the report.
#' @param output_format Character string. The format of the output report. 
#' Options are "html" (default) or "pdf".
#' 
#' @importFrom rmarkdown render
#'
#' @export
generate_report <- function(output, dir, output_format = c("html", "pdf")) {
  # Match the input argument to ensure it's either "html" or "pdf"
  output_format <- match.arg(output_format)
  
  report_params <- list(
    inputs = output$inputs,
    result = output$result
  )
  
  # Determine file extension and rmarkdown output format type
  if (output_format == "html") {
    file_ext <- ".html"
    fmt_target <- "html_document"
  } else {
    file_ext <- ".pdf"
    fmt_target <- "pdf_document"
  }
  
  output_file <- paste0("LaSPUR_Report_", Sys.Date(), file_ext)
  
  rmarkdown::render(
    input = "report/LaSPUR_type1_report_template.Rmd",
    output_format = fmt_target,
    output_file = output_file,
    output_dir = dir,
    params = report_params,
    knit_root_dir = getwd() 
  )
}

#' Validate and create output directory if missing
#'
#' @param dir_path Character string: path to the output directory.
#' @return Logical: TRUE if directory is valid/exists/created, FALSE otherwise.
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

# Ensure geometry column is named "geometry"
ensure_geometry_name <- function(sf_obj) {
  geom_col <- attr(sf_obj, "sf_column")
  if (!is.null(geom_col) && geom_col != "geometry") {
    names(sf_obj)[names(sf_obj) == geom_col] <- "geometry"
    sf_obj <- sf::st_set_geometry(sf_obj, "geometry")
  }
  return(sf_obj)
}

# ── Shared UI for Result Visualization ──────────────────────────
#' Create Result Visualization UI
#'
#' @param ns Namespace function of the module calling this.
#' @return A Shiny UI object containing the map, table, log, and download buttons.
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

# ── Shared Server for Result Visualization ──────────────────────
#' Render Result Visualization Server Logic
#'
#' @param input,output,session Standard shiny server arguments from the calling module.
#' @param rv Reactive values object containing analysis_result$map, analysis_result$table, log_messages, gpkg_path, xlsx_path.
#' @param config A list containing configuration options:
#'   - map_color_col: Column to use for map coloring.
#'   - map_title: Title for the map legend.
#'   - map_label_cols: Named list or vector of columns for labels/popups. Example: c("ID PU: " = "id_pu", "Indeks: " = "idx_padu_se").
#'   - map_palette: Palette name (e.g., "RdYlGn"). Default is "RdYlGn".
#'   - table_cols: Named vector for subsetting and renaming table columns. c("colname" = "Display Name").
#'   - table_round_cols: Character vector of display column names to round to 2 digits.
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
  
  # ── Map output ──────────────────
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
  
  # ── Table output ───────────────────────────────────────────
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
  
  # ── Validation log ─────────────────────────────────────────
  output$validation_log <- renderPrint({
    invalidateLater(100, session)
    cat(rv$log_messages)
  })
  
  # ── Download handlers ──────────────────────────────────────
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