# R/engine/indices/00_padu_common.R
# ============================================================
# Shared PADU Indicator Utilities & Distribution Summaries
# ============================================================

#' Calculate summary distribution statistics for a PADU indicator
#'
#' @param data `sf` or `data.frame` containing the indicator scores.
#' @param score_col Character naming the indicator score column.
#' @return A `tibble` with count, min, q25, median, mean, q75, max, and sd.
#' @export
padu_distribution_summary <- function(data, score_col) {
  vals <- data[[score_col]]
  tibble::tibble(
    indicator = score_col,
    n = length(vals),
    n_na = sum(is.na(vals)),
    min = if (all(is.na(vals))) NA_real_ else min(vals, na.rm = TRUE),
    q25 = if (all(is.na(vals))) NA_real_ else as.numeric(stats::quantile(vals, 0.25, na.rm = TRUE, names = FALSE)),
    median = if (all(is.na(vals))) NA_real_ else stats::median(vals, na.rm = TRUE),
    mean = if (all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE),
    q75 = if (all(is.na(vals))) NA_real_ else as.numeric(stats::quantile(vals, 0.75, na.rm = TRUE, names = FALSE)),
    max = if (all(is.na(vals))) NA_real_ else max(vals, na.rm = TRUE),
    sd = if (all(is.na(vals)) || length(stats::na.omit(vals)) < 2) NA_real_ else stats::sd(vals, na.rm = TRUE)
  )
}

#' Standardize any PADU indicator output into the unified contract
#'
#' Converts legacy return shapes, plain `sf` objects, or standardized lists
#' into a uniform `list(data, score_col, summary, metadata)` structure.
#'
#' @param res An `sf` object, `data.frame`, or list returned by a PADU indicator calculation.
#' @param indicator_name Optional character identifying the indicator (e.g. "padu_ke", "padu_se").
#' @return A standardized list with components: `data`, `score_col`, `summary`, `metadata`.
#' @export
standardize_padu_result <- function(res, indicator_name = NULL) {
  if (is.null(res)) return(NULL)
  
  # Already standardized?
  if (is.list(res) && all(c("data", "score_col", "summary") %in% names(res))) {
    return(res)
  }
  
  # If list with $data or $idx_padu_*_map
  data_sf <- NULL
  if (is.list(res) && !inherits(res, "sf")) {
    map_idx <- grep("_map$", names(res))[1]
    if (!is.na(map_idx)) {
      data_sf <- res[[map_idx]]
    } else if ("data" %in% names(res)) {
      data_sf <- res$data
    } else {
      # find first sf element in list
      sf_elements <- vapply(res, function(x) inherits(x, "sf"), logical(1))
      if (any(sf_elements)) {
        data_sf <- res[[which(sf_elements)[1]]]
      }
    }
  } else if (inherits(res, "sf")) {
    data_sf <- res
  }
  
  if (is.null(data_sf)) {
    stop("Could not locate an sf spatial object in indicator result.")
  }
  
  # Determine score column
  padu_cols <- names(data_sf)[grepl("^idx_padu_", names(data_sf))]
  padu_cols <- setdiff(padu_cols, c("idx_padu_all", "idx_padu_lindung", "idx_padu_budidaya", "idx_padu_final"))
  
  score_col <- if (length(padu_cols) > 0) padu_cols[1] else paste0("idx_", indicator_name %||% "padu")
  
  # Generate summary
  summary_tbl <- if (score_col %in% names(data_sf)) {
    padu_distribution_summary(data_sf, score_col)
  } else {
    tibble::tibble(indicator = score_col, n = nrow(data_sf))
  }
  
  list(
    data = data_sf,
    score_col = score_col,
    summary = summary_tbl,
    metadata = list(timestamp = Sys.time(), indicator = indicator_name)
  )
}
