# R/engine/indices/padu_hs.R
# ============================================================
# PADU-HS (Hidrologi & Sedimentasi) Engine
# ============================================================

#' Calculate PADU-HS Index from Estuary Distance and TSS Rasters
#'
#' Evaluates wildlife habitat suitability based on proximity to estuaries
#' and total suspended solids (TSS) water quality.
#'
#' @param idx_serasi_map `sf` object of planning units.
#' @param estuari_euc_dist `SpatRaster` of Euclidean distances to estuaries.
#' @param tss_rast `SpatRaster` of Total Suspended Solids concentration.
#' @param id_col Character naming planning unit ID column (default "id_pu").
#' @param max_dist Numeric maximum distance in meters for estuary decay (default 5000).
#' @return A standardized list containing:
#'   \item{data}{`sf` object containing `idx_padu_hs` score column}
#'   \item{score_col}{Character "idx_padu_hs"}
#'   \item{summary}{`tibble` distribution of indicator scores}
#'   \item{metadata}{List of runtime parameters and timestamp}
#'   \item{idx_padu_hs_map}{Alias for `data`}
#'   \item{idx_padu_hs}{`tibble` of attribute data without geometry}
#' @export
calculate_padu_hs <- function(idx_serasi_map,
                              estuari_euc_dist,
                              tss_rast,
                              id_col = "id_pu",
                              max_dist = 5000) {
  if (!inherits(idx_serasi_map, "sf")) stop("idx_serasi_map must be an sf object")
  if (!id_col %in% names(idx_serasi_map)) stop("Column '", id_col, "' not found in idx_serasi_map")
  
  estuari_dist_extracted <- extract_raster_to_sf(
    idx_serasi_map,
    estuari_euc_dist,
    id_col = id_col,
    new_col = "estuari_dist_mean"
  )
  
  tss_extracted <- extract_raster_to_sf(
    idx_serasi_map,
    tss_rast,
    id_col = id_col,
    new_col = "tss_mean"
  )
  
  tss_to_merge <- tss_extracted %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::all_of(c(id_col, "tss_mean")))
  
  if (is.na(max_dist) || max_dist <= 0) {
    max_dist_val <- max(estuari_dist_extracted$estuari_dist_mean, na.rm = TRUE)
    if (is.infinite(max_dist_val) || max_dist_val == 0) {
      max_dist_val <- 1
      warning("All distances are NA or zero. Estuary sub-index set to 1 for all non-NA rows.")
    }
  } else {
    max_dist_val <- max_dist
  }
  
  min_tss <- min(tss_extracted$tss_mean, na.rm = TRUE)
  max_tss <- max(tss_extracted$tss_mean, na.rm = TRUE)
  
  if (is.infinite(min_tss) || is.infinite(max_tss)) {
    min_tss <- 0
    max_tss <- 1
    tss_constant <- FALSE  
    warning("All TSS values are NA. TSS sub-index will be NA for all rows.")
  } else if (max_tss == min_tss) {
    tss_constant <- TRUE
  } else {
    tss_constant <- FALSE
  }
  
  # Calculate index hs
  idx_padu_hs_map <- estuari_dist_extracted %>%
    dplyr::left_join(tss_to_merge, by = id_col) %>%
    dplyr::mutate(
      filter_estuari = pmax(0, pmin(1,
        1 - pmin(abs(.data$estuari_dist_mean) / max_dist_val, 1)
      )),
      
      tss_norm = dplyr::case_when(
        is.na(.data$tss_mean) ~ NA_real_,
        tss_constant ~ 1.0, 
        TRUE ~ pmax(0, pmin(1,
          (max_tss - .data$tss_mean) / (max_tss - min_tss)
        ))
      ),
      
      idx_padu_hs = (filter_estuari + tss_norm) / 2
    ) %>%
    dplyr::select(-filter_estuari, -tss_norm)
  
  idx_padu_hs_tbl <- tibble::as_tibble(
    idx_padu_hs_map %>% sf::st_drop_geometry()
  )
  
  score_col <- "idx_padu_hs"
  summary_tbl <- padu_distribution_summary(idx_padu_hs_map, score_col)
  
  list(
    data = idx_padu_hs_map,
    score_col = score_col,
    summary = summary_tbl,
    metadata = list(
      timestamp = Sys.time(),
      id_col = id_col,
      max_dist = max_dist
    ),
    idx_padu_hs_map = idx_padu_hs_map,
    idx_padu_hs     = idx_padu_hs_tbl
  )
}
