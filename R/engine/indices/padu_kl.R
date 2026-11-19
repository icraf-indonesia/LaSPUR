# R/engine/indices/padu_kl.R
# ============================================================
# PADU-KL (Kawasan Lindung) Engine
# ============================================================

#' Calculate PADU-KL Index (Kawasan Lindung / Protected Areas)
#'
#' Computes the proportion of protected area overlay within each planning unit,
#' assigning an index from 0.0 (no protected area) to 1.0 (entirely protected).
#'
#' @param pu `sf` object of planning units. Must contain `area_ha` column.
#' @param protected_area `sf` object containing protected area boundary polygons.
#' @param parallel Logical. Execute spatial overlay in parallel.
#' @param workers Number of parallel workers (optional).
#' @return A standardized list containing:
#'   \item{data}{`sf` object containing `idx_padu_kl`, `protected_ha`, and `protected_pct`}
#'   \item{score_col}{Character "idx_padu_kl"}
#'   \item{summary}{`tibble` distribution of indicator scores}
#'   \item{metadata}{List of runtime parameters and timestamp}
#'   \item{idx_padu_kl_map}{Alias for `data`}
#'   \item{idx_padu_kl}{`tibble` of attribute data without geometry}
#' @export
calculate_padu_kl <- function(pu,
                              protected_area,
                              parallel = FALSE,
                              workers = NULL) {
  if (!inherits(pu, "sf")) stop("pu must be an sf object")
  if (!inherits(protected_area, "sf")) stop("protected_area must be an sf object")
  if (!"area_ha" %in% names(pu)) {
    pu$area_ha <- as.numeric(sf::st_area(pu)) / 10000
  }
  
  idx_padu_kl_map <- calculate_overlay_pct(
    pu           = pu,
    overlay_area = protected_area,
    title        = "protected",
    parallel     = parallel,
    workers      = workers
  ) %>%
    dplyr::mutate(idx_padu_kl = .data$protected_pct / 100)
  
  score_col <- "idx_padu_kl"
  summary_tbl <- padu_distribution_summary(idx_padu_kl_map, score_col)
  res_table <- tibble::as_tibble(sf::st_drop_geometry(idx_padu_kl_map))
  
  list(
    data = idx_padu_kl_map,
    score_col = score_col,
    summary = summary_tbl,
    metadata = list(
      timestamp = Sys.time(),
      parallel = parallel,
      workers = workers
    ),
    idx_padu_kl_map = idx_padu_kl_map,
    idx_padu_kl     = res_table
  )
}
