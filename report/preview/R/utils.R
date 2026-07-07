# ============================================================
# LaSPUR Documentation Framework
# utils.R
# ============================================================

# ============================================================
# Packages
# ============================================================

required_packages <- c(
  "terra",
  "sf",
  "leaflet",
  "DT",
  "dplyr",
  "htmltools",
  "RColorBrewer",
  "viridisLite",
  "readxl"
)

invisible(
  lapply(required_packages, library,
         character.only = TRUE)
)

# ============================================================
# PATH HELPERS
# ============================================================

data_path <- function(...) {
  
  file.path("data", ...)
  
}

image_path <- function(...) {
  
  file.path("images", ...)
  
}

table_path <- function(...) {
  
  file.path("data", "tables", ...)
  
}

raster_path <- function(...) {
  
  file.path("data", "raster", ...)
  
}

vector_path <- function(...) {
  
  file.path("data", "vector", ...)
  
}

# ============================================================
# CRS
# ============================================================

project_to_wgs84 <- function(x){
  
  # -----------------------------
  # Vector (sf)
  # -----------------------------
  if (inherits(x, "sf")) {
    
    if (sf::st_crs(x)$epsg != 4326) {
      x <- sf::st_transform(x, 4326)
    }
    
    return(x)
    
  }
  
  # -----------------------------
  # Raster (terra)
  # -----------------------------
  if (inherits(x, "SpatRaster")) {
    
    crs_txt <- terra::crs(x)
    
    if (is.na(crs_txt) || crs_txt == "") {
      return(x)
    }
    
    if (!grepl("4326", crs_txt)) {
      x <- terra::project(x, "EPSG:4326")
    }
    
    return(x)
    
  }
  
  stop("Unsupported spatial object.")
}

# ============================================================
# VECTOR MAP
# ============================================================

create_vector_map <- function(file, color_field = NULL){
  
  # Read vector
  v <- sf::st_read(file, quiet = TRUE)
  
  # Remove Z dimension if exists
  v <- sf::st_zm(v, drop = TRUE)
  
  # Project to WGS84
  if (sf::st_crs(v)$epsg != 4326) {
    v <- sf::st_transform(v, 4326)
  }
  
  # Automatically detect attribute column
  if (is.null(color_field)) {
    
    geom_col <- attr(v, "sf_column")
    
    fields <- setdiff(names(v), geom_col)
    
    if (length(fields) == 0) {
      
      leaflet(v) |>
        addTiles() |>
        addPolygons()
      
    } else {
      
      color_field <- fields[1]
      
    }
    
  }
  
  # Number of categories
  n_class <- length(unique(v[[color_field]]))
  
  # Color palette
  pal <- colorFactor(
    palette = colorRampPalette(
      RColorBrewer::brewer.pal(8, "Set2")
    )(n_class),
    domain = v[[color_field]]
  )
  
  leaflet(v) |>
    
    addTiles() |>
    
    addPolygons(
      
      fillColor = ~pal(get(color_field)),
      
      weight = 2,
      
      opacity = 1,
      
      color = "white",
      
      dashArray = "3",
      
      fillOpacity = 0.7,
      
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      
      label = ~get(color_field),
      
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
        textsize = "15px",
        direction = "auto"
      )
      
    ) |>
    
    addLegend(
      pal = pal,
      values = v[[color_field]],
      title = color_field
    )
  
}

# ============================================================
# RASTER MAP
# ============================================================

create_raster_map <- function(file,
                              title=NULL){
  
  r <- terra::rast(file)
  
  r <- project_to_wgs84(r)
  
  pal <- colorNumeric(
    
    palette="viridis",
    
    domain=terra::values(r),
    
    na.color="transparent"
    
  )
  
  leaflet() |>
    
    addProviderTiles(
      providers$Esri.WorldImagery
    ) |>
    
    addRasterImage(
      
      r,
      
      colors=pal,
      
      opacity=.8
      
    ) |>
    
    addLegend(
      
      pal=pal,
      
      values=terra::values(r),
      
      title=title
      
    )
  
}

# ============================================================
# VECTOR METADATA
# ============================================================

get_vector_metadata <- function(file){
  
  v <- st_read(file,
               quiet=TRUE)
  
  bb <- st_bbox(v)
  
  data.frame(
    
    Properti=c(
      
      "Format",
      
      "Sistem Koordinat",
      
      "Bounding Box",
      
      "Geometry",
      
      "Jumlah Feature"
      
    ),
    
    Nilai=c(
      
      tools::file_ext(file),
      
      st_crs(v)$Name,
      
      paste(round(bb,5),
            collapse=", "),
      
      unique(st_geometry_type(v)),
      
      nrow(v)
      
    )
    
  )
  
}

# ============================================================
# RASTER METADATA
# ============================================================

get_raster_metadata <- function(file){
  
  r <- rast(file)
  
  e <- ext(r)
  
  data.frame(
    
    Properti=c(
      
      "Format",
      
      "Sistem Koordinat",
      
      "Bounding Box",
      
      "Resolusi",
      
      "Jumlah Layer"
      
    ),
    
    Nilai=c(
      
      tools::file_ext(file),
      
      crs(r),
      
      paste(
        
        round(c(
          
          e$xmin,
          e$ymin,
          e$xmax,
          e$ymax
          
        ),5),
        
        collapse=", "
        
      ),
      
      paste(res(r),
            collapse=" x "),
      
      nlyr(r)
      
    )
    
  )
  
}

# ============================================================
# TABLE METADATA
# ============================================================

get_table_metadata <- function(file){
  
  ext <- tools::file_ext(file)
  
  if(ext %in% c("xlsx","xls")){
    
    x <- readxl::read_excel(file)
    
  }else{
    
    x <- read.csv(file)
    
  }
  
  data.frame(
    
    Properti=c(
      
      "Format",
      
      "Jumlah Baris",
      
      "Jumlah Kolom",
      
      "Nama Kolom"
      
    ),
    
    Nilai=c(
      
      toupper(ext),
      
      nrow(x),
      
      ncol(x),
      
      paste(
        names(x),
        collapse=", "
      )
      
    )
    
  )
  
}

# ============================================================
# VECTOR VALIDATION
# ============================================================

check_vector <- function(file){
  
  v <- sf::st_read(file, quiet = TRUE)
  
  status <- c(
    !is.na(sf::st_crs(v)),
    all(sf::st_is_valid(v)),
    !any(is.na(v)),
    !any(duplicated(v))
  )
  
  data.frame(
    
    Pemeriksaan = c(
      "CRS",
      "Geometry",
      "Missing Value",
      "Duplikasi"
    ),
    
    Status = ifelse(status, "✔", "✖"),
    
    Keterangan = c(
      if (status[1]) {
        paste("CRS tersedia:", sf::st_crs(v)$Name)
      } else {
        "CRS belum didefinisikan."
      },
      
      if (status[2]) {
        "Seluruh geometri valid."
      } else {
        paste(sum(!sf::st_is_valid(v)), "geometri tidak valid.")
      },
      
      if (status[3]) {
        "Tidak terdapat nilai kosong (NA)."
      } else {
        paste(sum(is.na(v)), "nilai kosong (NA) ditemukan.")
      },
      
      if (status[4]) {
        "Tidak terdapat fitur duplikat."
      } else {
        paste(sum(duplicated(v)), "fitur duplikat ditemukan.")
      }
    ),
    
    check.names = FALSE
    
  )
  
}

# ============================================================
# RASTER VALIDATION
# ============================================================

check_raster <- function(file){
  
  r <- terra::rast(file)
  
  status <- c(
    !is.na(terra::crs(r)),
    !is.null(terra::ext(r)),
    all(terra::res(r) > 0),
    TRUE
  )
  
  data.frame(
    
    Pemeriksaan = c(
      "CRS",
      "Extent",
      "Resolution",
      "Missing Value"
    ),
    
    Status = ifelse(status, "✔", "✖"),
    
    Keterangan = c(
      
      if (status[1]) {
        "Coordinate Reference System tersedia."
      } else {
        "Coordinate Reference System belum tersedia."
      },
      
      if (status[2]) {
        paste(
          "Bounding Box:",
          paste(round(as.vector(terra::ext(r)), 4), collapse = ", ")
        )
      } else {
        "Extent tidak tersedia."
      },
      
      if (status[3]) {
        paste(
          "Resolusi:",
          paste(terra::res(r), collapse = " × ")
        )
      } else {
        "Resolusi raster tidak valid."
      },
      
      if (status[4]) {
        "Pemeriksaan nilai NoData belum dilakukan."
      } else {
        "Terdapat nilai NoData."
      }
      
    ),
    
    check.names = FALSE
    
  )
  
}

# ============================================================
# TABLE PREVIEW
# ============================================================

preview_table <- function(file){
  
  ext <- tools::file_ext(file)
  
  tbl <- switch(
    
    ext,
    
    csv = read.csv(file),
    
    xlsx = readxl::read_excel(file),
    
    xls = readxl::read_excel(file),
    
    stop("Format tidak didukung.")
    
  )
  
  DT::datatable(
    
    tbl,
    
    extensions="Buttons",
    
    options=list(
      
      pageLength=10,
      
      scrollX=TRUE,
      
      dom="Bfrtip"
      
    ),
    
    rownames=FALSE
    
  )
  
}