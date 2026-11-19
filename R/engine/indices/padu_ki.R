# R/engine/indices/padu_ki.R
# ============================================================
# PADU-KI (Ketahanan Iklim) Engine
# ============================================================

#' Calculate PADU-KI Index from Disaster Risk Vector Layer
#'
#' Computes the infrastructure suitability index based on disaster hazard/risk
#' exposure. Lower disaster risk yields higher suitability (score inverted).
#'
#' @param idx_serasi_map `sf` object of planning units.
#' @param disaster_risk_vect `sf` polygon object containing disaster risk values.
#' @param value_col Character naming attribute column holding numeric hazard risk values.
#' @param pu_id Character naming planning unit ID column (default "id_pu").
#' @param new_col Character naming temporary extracted mean column (default "disaster_risk_mean").
#' @param parallel Logical. Run spatial intersection in parallel.
#' @param workers Number of parallel workers (optional).
#' @return A standardized list containing:
#'   \item{data}{`sf` object containing `idx_padu_ki` score column}
#'   \item{score_col}{Character "idx_padu_ki"}
#'   \item{summary}{`tibble` distribution of indicator scores}
#'   \item{metadata}{List of runtime parameters and timestamp}
#'   \item{idx_padu_ki_map}{Alias for `data`}
#'   \item{idx_padu_ki}{`tibble` of attribute data without geometry}
#' @export
calculate_padu_ki <- function(idx_serasi_map,
                              disaster_risk_vect,
                              value_col,
                              pu_id = "id_pu",
                              new_col = "disaster_risk_mean",
                              parallel = FALSE,
                              workers = NA) {
  if (!inherits(idx_serasi_map, "sf")) stop("idx_serasi_map must be an sf object")
  if (!inherits(disaster_risk_vect, "sf")) stop("disaster_risk_vect must be an sf object")
  if (!value_col %in% names(disaster_risk_vect)) {
    stop("Column '", value_col, "' not found in disaster_risk_vect")
  }
  
  # Extract disaster risk values
  disaster_risk_extracted <- extract_sf_to_sf(
    pu = idx_serasi_map,
    value_sf = disaster_risk_vect,
    value_col = value_col,
    new_col = new_col,
    pu_id = pu_id,
    parallel = parallel,
    workers = workers
  )
  
  # Get min and max of extracted values
  min_val <- min(disaster_risk_extracted[[new_col]], na.rm = TRUE)
  max_val <- max(disaster_risk_extracted[[new_col]], na.rm = TRUE)
  
  # Compute normalized risk (0–1)
  if (is.infinite(min_val) || is.infinite(max_val) || max_val == min_val) {
    normalized <- rep(0, nrow(disaster_risk_extracted))
  } else {
    normalized <- (disaster_risk_extracted[[new_col]] - min_val) / (max_val - min_val)
  }
  
  # Build final index
  idx_padu_ki_map <- disaster_risk_extracted %>%
    dplyr::mutate(
      idx_padu_ki = dplyr::if_else(
        is.na(.data[[new_col]]),
        NA_real_,
        1 - normalized
      )
    ) %>%
    dplyr::select(-dplyr::all_of(new_col))
  
  idx_padu_ki <- tibble::as_tibble(
    idx_padu_ki_map %>% sf::st_drop_geometry()
  )
  
  score_col <- "idx_padu_ki"
  summary_tbl <- padu_distribution_summary(idx_padu_ki_map, score_col)
  
  list(
    data = idx_padu_ki_map,
    score_col = score_col,
    summary = summary_tbl,
    metadata = list(
      timestamp = Sys.time(),
      value_col = value_col,
      pu_id = pu_id
    ),
    idx_padu_ki_map = idx_padu_ki_map,
    idx_padu_ki     = idx_padu_ki
  )
}
