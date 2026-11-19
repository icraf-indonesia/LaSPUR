# R/engine/indices/padu_rtp.R
# ============================================================
# PADU-RTp (Risiko & Tekanan Pembangunan) Engine
# ============================================================

#' Calculate PADU-RTp Index from Industry and Shipping Route Proximity
#'
#' Computes the coastal pressure / hazard avoidance index (PADU-RTp)
#' based on distance to industrial areas and shipping lanes.
#'
#' @param idx_serasi_map `sf` object of planning units.
#' @param industry_euc_dist `SpatRaster` of Euclidean distances to industrial areas.
#' @param pelayaran_euc_dist `SpatRaster` of Euclidean distances to shipping lanes.
#' @param id_col Character naming planning unit ID column (default "id_pu").
#' @param industry_max_dist Numeric maximum distance in meters for industrial impact (default 8000).
#' @param pelayaran_max_dist Numeric maximum distance in meters for shipping impact (default 5000).
#' @return A standardized list containing:
#'   \item{data}{`sf` object containing `idx_padu_rtp` score column}
#'   \item{score_col}{Character "idx_padu_rtp"}
#'   \item{summary}{`tibble` distribution of indicator scores}
#'   \item{metadata}{List of runtime parameters and timestamp}
#'   \item{idx_padu_rtp_map}{Alias for `data`}
#'   \item{idx_padu_rtp}{`tibble` of attribute data without geometry}
#' @export
calculate_padu_rtp <- function(idx_serasi_map,
                               industry_euc_dist,
                               pelayaran_euc_dist,
                               id_col = "id_pu",
                               industry_max_dist = 8000,
                               pelayaran_max_dist = 5000) {
  if (!inherits(idx_serasi_map, "sf")) stop("idx_serasi_map must be an sf object")
  if (!id_col %in% names(idx_serasi_map)) stop("Column '", id_col, "' not found in idx_serasi_map")
  
  # Extract industry distance
  industry_dist_extracted <- extract_raster_to_sf(
    idx_serasi_map,
    industry_euc_dist,
    id_col = id_col,
    new_col = "industry_dist_mean"
  )
  
  # Extract shipping lane distance
  pelayaran_dist_extracted <- extract_raster_to_sf(
    idx_serasi_map,
    pelayaran_euc_dist,
    id_col = id_col,
    new_col = "pelayaran_dist_mean"
  )
  
  # Drop geometry from industry for joining
  industry_to_merge <- industry_dist_extracted %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::all_of(c(id_col, "industry_dist_mean")))
  
  # Join and calculate PADU-RTp
  idx_padu_rtp_map <- pelayaran_dist_extracted %>%
    dplyr::left_join(industry_to_merge, by = id_col) %>%
    dplyr::mutate(
      industry_clean = dplyr::if_else(is.na(.data$industry_dist_mean), 0,
                                      pmax(.data$industry_dist_mean, 0)),
      pelayaran_clean = dplyr::if_else(is.na(.data$pelayaran_dist_mean), 0,
                                       pmax(.data$pelayaran_dist_mean, 0)),
      
      filter_industry = 1 - pmin(.data$industry_clean / industry_max_dist, 1),
      filter_pelayaran = 1 - pmin(.data$pelayaran_clean / pelayaran_max_dist, 1),
      
      idx_padu_rtp = pmax(0, 1 - (.data$filter_industry + .data$filter_pelayaran) / 2)
    ) %>%
    dplyr::select(-dplyr::all_of(c("industry_clean", "pelayaran_clean",
                                   "filter_industry", "filter_pelayaran")))
  
  idx_padu_rtp_tbl <- tibble::as_tibble(
    idx_padu_rtp_map %>% sf::st_drop_geometry()
  )
  
  score_col <- "idx_padu_rtp"
  summary_tbl <- padu_distribution_summary(idx_padu_rtp_map, score_col)
  
  list(
    data = idx_padu_rtp_map,
    score_col = score_col,
    summary = summary_tbl,
    metadata = list(
      timestamp = Sys.time(),
      industry_max_dist = industry_max_dist,
      pelayaran_max_dist = pelayaran_max_dist
    ),
    idx_padu_rtp_map = idx_padu_rtp_map,
    idx_padu_rtp     = idx_padu_rtp_tbl
  )
}
