# R/engine/indices/padu_kh.R
# ============================================================
# PADU-KH (Komposisi Habitat) Engine
# ============================================================

#' Calculate PADU-KH Index (Komposisi Habitat / Coastal Habitat Composition)
#'
#' Computes the proportion of coastal habitat overlay within each planning unit,
#' assigning an index from 0.0 (no habitat) to 1.0 (entirely covered by habitat).
#' Habitat areas can be supplied either as an `sf` polygon object (`habitat_area`)
#' or extracted from a LULC vector layer (`lulc`) filtered by specific class IDs (`habitat_ids`).
#'
#' @param pu `sf` object of planning units. Must contain `area_ha` column.
#' @param habitat_area Optional `sf` object containing coastal habitat boundary polygons.
#' @param lulc Optional `sf` object containing Land Use / Land Cover polygons.
#' @param habitat_ids Optional numeric or character vector of class IDs in `lulc` defining habitats.
#' @param lulc_id_col Optional character naming the ID column in `lulc`.
#' @param parallel Logical. Execute spatial overlay in parallel.
#' @param workers Number of parallel workers (optional).
#' @return A standardized list containing:
#'   \item{data}{`sf` object containing `idx_padu_kh`, `coastal_habitat_ha`, and `coastal_habitat_pct`}
#'   \item{score_col}{Character "idx_padu_kh"}
#'   \item{summary}{`tibble` distribution of indicator scores}
#'   \item{metadata}{List of runtime parameters and timestamp}
#'   \item{idx_padu_kh_map}{Alias for `data`}
#'   \item{idx_padu_kh}{`tibble` of attribute data without geometry}
#' @export
calculate_padu_kh <- function(pu,
                              habitat_area = NULL,
                              lulc = NULL,
                              habitat_ids = NULL,
                              lulc_id_col = NULL,
                              parallel = FALSE,
                              workers = NULL) {
  if (!inherits(pu, "sf")) stop("pu must be an sf object")
  if (!"area_ha" %in% names(pu)) {
    pu$area_ha <- as.numeric(sf::st_area(pu)) / 10000
  }
  
  if (is.null(habitat_area)) {
    if (is.null(lulc)) {
      stop("Either 'habitat_area' or 'lulc' must be provided.")
    }
    if (!inherits(lulc, "sf")) stop("lulc must be an sf object")
    if (is.null(habitat_ids) || length(habitat_ids) == 0) {
      stop("habitat_ids must be provided when using lulc as habitat source.")
    }
    
    if (is.null(lulc_id_col)) {
      candidates <- c("ID", "id", "Id", "class_id", "Class_ID", "kode", "Kode")
      found <- intersect(candidates, names(lulc))
      if (length(found) == 0) {
        stop("Could not automatically identify ID column in lulc. Specify 'lulc_id_col'.")
      }
      lulc_id_col <- found[1]
    } else {
      if (!lulc_id_col %in% names(lulc)) {
        stop("Specified 'lulc_id_col' not found in lulc.")
      }
    }
    
    habitat_area <- lulc[lulc[[lulc_id_col]] %in% habitat_ids, ]
    if (nrow(habitat_area) == 0) {
      stop("No features in lulc matched the provided habitat_ids.")
    }
  } else {
    if (!inherits(habitat_area, "sf")) {
      stop("habitat_area must be an sf object")
    }
  }
  
  res_map <- calculate_overlay_pct(
    pu           = pu,
    overlay_area = habitat_area,
    title        = "coastal_habitat",
    parallel     = parallel,
    workers      = workers
  ) %>%
    dplyr::mutate(idx_padu_kh = .data$coastal_habitat_pct / 100)
  
  score_col <- "idx_padu_kh"
  summary_tbl <- padu_distribution_summary(res_map, score_col)
  res_table <- tibble::as_tibble(sf::st_drop_geometry(res_map))
  
  list(
    data = res_map,
    score_col = score_col,
    summary = summary_tbl,
    metadata = list(
      timestamp = Sys.time(),
      parallel = parallel,
      workers = workers
    ),
    idx_padu_kh_map = res_map,
    idx_padu_kh     = res_table
  )
}
