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
  
  # Geometry type
  geom_type <- unique(as.character(sf::st_geometry_type(v)))
  geom_type <- gsub("^MULTI", "", geom_type)
  
  map <- leaflet(v) |>
    addTiles()
  
  if (all(geom_type == "POLYGON")) {
    
    map <- map |>
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
      )
    
  } else if (all(geom_type == "LINESTRING")) {
    
    map <- map |>
      addPolylines(
        
        color = ~pal(get(color_field)),
        
        weight = 3,
        
        opacity = 1,
        
        label = ~get(color_field),
        
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "15px",
          direction = "auto"
        )
      )
    
  } else if (all(geom_type == "POINT")) {
    
    map <- map |>
      addCircleMarkers(
        
        radius = 5,
        
        stroke = TRUE,
        
        weight = 1,
        
        color = "white",
        
        fillColor = ~pal(get(color_field)),
        
        fillOpacity = 0.9,
        
        label = ~get(color_field),
        
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "15px",
          direction = "auto"
        )
      )
    
  } else {
    
    stop("Unsupported geometry type: ", paste(geom_type, collapse = ", "))
    
  }
  
  map |>
    addLegend(
      pal = pal,
      values = v[[color_field]],
      title = color_field
    )
  
}

# ============================================================
# RASTER MAP
# ============================================================

create_raster_map <- function(
    file,
    title = NULL,
    max_cells = 250000
){
  
  # ----------------------------------------------------------
  # Read raster
  # ----------------------------------------------------------
  
  r <- terra::rast(file)
  
  r <- project_to_wgs84(r)
  
  # ----------------------------------------------------------
  # Downsample raster
  # ----------------------------------------------------------
  
  if (terra::ncell(r) > max_cells){
    
    fact <- ceiling(
      sqrt(
        terra::ncell(r) / max_cells
      )
    )
    
    if (terra::is.factor(r)){
      
      r <- terra::aggregate(
        r,
        fact = fact,
        fun = "modal",
        na.rm = TRUE
      )
      
    }else{
      
      r <- terra::aggregate(
        r,
        fact = fact,
        fun = "mean",
        na.rm = TRUE
      )
      
    }
    
  }
  
  # ----------------------------------------------------------
  # Detect categorical raster
  # ----------------------------------------------------------
  
  is_cat <- terra::is.factor(r)
  
  if(!is_cat){
    
    f <- tryCatch(
      terra::freq(r),
      error=function(e) NULL
    )
    
    if(!is.null(f)){
      
      if(nrow(f) <= 20){
        
        is_cat <- TRUE
        
      }
      
    }
    
  }
  
  # ==========================================================
  # CONTINUOUS RASTER
  # ==========================================================
  
  if(!is_cat){
    
    rng <- terra::minmax(r)
    
    pal <- leaflet::colorNumeric(
      
      palette = "viridis",
      
      domain = c(rng[1], rng[2]),
      
      na.color = "transparent"
      
    )
    
    return(
      
      leaflet::leaflet() |>
        
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery
        ) |>
        
        leaflet::addRasterImage(
          
          x = r,
          
          colors = pal,
          
          opacity = 0.8,
          
          project = FALSE
          
        ) |>
        
        leaflet::addLegend(
          
          pal = pal,
          
          values = c(rng[1], rng[2]),
          
          title = title
          
        )
      
    )
    
  }
  
  # ==========================================================
  # CATEGORICAL RASTER
  # ==========================================================
  
  rat <- tryCatch(
    terra::levels(r)[[1]],
    error=function(e) NULL
  )
  
  if(!is.null(rat)){
    
    value_col <- names(rat)[1]
    
    label_col <- names(rat)[2]
    
    values <- rat[[value_col]]
    
    labels <- as.character(rat[[label_col]])
    
  }else{
    
    freq <- terra::freq(r)
    
    values <- freq[,1]
    
    labels <- as.character(values)
    
  }
  
  pal <- leaflet::colorFactor(
    
    palette = RColorBrewer::brewer.pal(
      max(3,min(8,length(labels))),
      "Set2"
    ),
    
    domain = labels,
    
    ordered = FALSE
    
  )
  
  color_fun <- function(x){
    
    idx <- match(x, values)
    
    cols <- rep("#00000000", length(idx))
    
    ok <- !is.na(idx)
    
    cols[ok] <- pal(labels[idx[ok]])
    
    cols
    
  }
  
  leaflet::leaflet() |>
    
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery
    ) |>
    
    leaflet::addRasterImage(
      
      x = r,
      
      colors = color_fun,
      
      opacity = 0.8,
      
      project = FALSE
      
    ) |>
    
    leaflet::addLegend(
      
      colors = pal(labels),
      
      labels = labels,
      
      title = title,
      
      opacity = 1
      
    )
  
}

# ============================================================
# VECTOR METADATA
# ============================================================

get_vector_metadata <- function(file){
  
  library(sf)
  library(htmltools)
  
  v <- st_read(file, quiet = TRUE)
  
  bb <- st_bbox(v)
  
  fields <- setdiff(names(v), attr(v, "sf_column"))
  
  tags$div(
    
    class = "metadata-section",
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Format"),
             tags$div(class="meta-value",toupper(tools::file_ext(file)))
    ),
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Geometry"),
             tags$div(class="meta-value",
                      as.character(unique(st_geometry_type(v))))
    ),
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Coordinate System"),
             tags$div(class="meta-value",
                      st_crs(v)$Name)
    ),
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Feature Count"),
             tags$div(class="meta-value",
                      format(nrow(v), big.mark=","))
    ),
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Extent"),
             tags$div(class="meta-value",
                      
                      HTML(paste(
                        sprintf("xmin : %.5f",bb["xmin"]),
                        sprintf("ymin : %.5f",bb["ymin"]),
                        sprintf("xmax : %.5f",bb["xmax"]),
                        sprintf("ymax : %.5f",bb["ymax"]),
                        sep="<br>"
                      ))
                      
             )
    ),
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Fields"),
             tags$div(class="meta-value",
                      
                      HTML(
                        paste(fields,
                              collapse="<br>")
                      )
                      
             )
    )
    
  )
  
}

# ============================================================
# RASTER METADATA
# ============================================================

get_raster_metadata <- function(file){
  
  library(terra)
  library(htmltools)
  
  r <- rast(file)
  
  e <- ext(r)
  
  crs_info <- terra::crs(r, describe = TRUE)
  
  if (!is.null(crs_info) &&
      !is.na(crs_info$code) &&
      nzchar(crs_info$name)) {
    
    crs_text <- sprintf("%s (EPSG:%s)",
                        crs_info$name,
                        crs_info$code)
    
  } else {
    
    crs_text <- terra::crs(r, proj = FALSE)
    
  }
  
  tags$div(
    
    class="metadata-section",
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Format"),
             tags$div(class="meta-value",toupper(tools::file_ext(file)))
    ),
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Coordinate System"),
             tags$div(class="meta-value",crs_text)
    ),
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Layer"),
             tags$div(class="meta-value",nlyr(r))
    ),
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Resolution"),
             tags$div(class="meta-value",
                      
                      paste0(
                        res(r)[1],
                        " × ",
                        res(r)[2]
                      )
                      
             )
    ),
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Extent"),
             
             tags$div(class="meta-value",
                      
                      HTML(paste(
                        sprintf("xmin : %.5f",e$xmin),
                        sprintf("ymin : %.5f",e$ymin),
                        sprintf("xmax : %.5f",e$xmax),
                        sprintf("ymax : %.5f",e$ymax),
                        sep="<br>"
                      ))
                      
             )
    )
    
  )
  
}

# ============================================================
# TABLE METADATA
# ============================================================

get_table_metadata <- function(file){
  
  library(htmltools)
  
  ext <- tools::file_ext(file)
  
  if(ext %in% c("xlsx","xls")){
    
    x <- readxl::read_excel(file)
    
  }else{
    
    x <- read.csv(file)
    
  }
  
  tags$div(
    
    class="metadata-section",
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Format"),
             tags$div(class="meta-value",toupper(ext))
    ),
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Rows"),
             tags$div(class="meta-value",
                      format(nrow(x),big.mark=","))
    ),
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Columns"),
             tags$div(class="meta-value",ncol(x))
    ),
    
    tags$div(class="meta-row",
             tags$div(class="meta-name","Fields"),
             
             tags$div(class="meta-value",
                      
                      HTML(
                        paste(
                          names(x),
                          collapse="<br>"
                        )
                      )
                      
             )
    )
    
  )
  
}

# ============================================================
# VECTOR VALIDATION
# ============================================================

check_vector <- function(file){
  
  library(sf)
  library(htmltools)
  
  v <- sf::st_read(file, quiet = TRUE)
  
  status <- c(
    !is.na(sf::st_crs(v)),
    all(sf::st_is_valid(v)),
    !any(is.na(v)),
    !any(duplicated(v))
  )
  
  result <- c(
    ifelse(status[1], "✔ Passed", "✖ Failed"),
    ifelse(status[2], "✔ Passed", "✖ Failed"),
    ifelse(status[3], "✔ Passed", "✖ Failed"),
    ifelse(status[4], "✔ Passed", "✖ Failed")
  )
  
  description <- c(
    
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
    
  )
  
  tags$div(
    
    class = "metadata-section",
    
    lapply(seq_along(result), function(i){
      
      tags$div(
        
        class = "meta-row",
        
        tags$div(
          class = "meta-name",
          c("CRS",
            "Geometry",
            "Missing Value",
            "Duplicate Feature")[i]
        ),
        
        tags$div(
          
          class = "meta-value",
          
          tags$b(result[i]),
          
          tags$br(),
          
          description[i]
          
        )
        
      )
      
    })
    
  )
  
}

# ============================================================
# RASTER VALIDATION
# ============================================================

check_raster <- function(file){
  
  library(terra)
  library(htmltools)
  
  r <- terra::rast(file)
  
  status <- c(
    !is.na(terra::crs(r)),
    !is.null(terra::ext(r)),
    all(terra::res(r) > 0),
    TRUE
  )
  
  result <- c(
    ifelse(status[1], "✔ Passed", "✖ Failed"),
    ifelse(status[2], "✔ Passed", "✖ Failed"),
    ifelse(status[3], "✔ Passed", "✖ Failed"),
    ifelse(status[4], "✔ Passed", "✖ Failed")
  )
  
  description <- c(
    
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
        "Resolution:",
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
    
  )
  
  tags$div(
    
    class = "metadata-section",
    
    lapply(seq_along(result), function(i){
      
      tags$div(
        
        class = "meta-row",
        
        tags$div(
          class = "meta-name",
          c("CRS",
            "Extent",
            "Resolution",
            "Missing Value")[i]
        ),
        
        tags$div(
          
          class = "meta-value",
          
          tags$b(result[i]),
          
          tags$br(),
          
          description[i]
          
        )
        
      )
      
    })
    
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
  
  tbl <- head(tbl,20)
  
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