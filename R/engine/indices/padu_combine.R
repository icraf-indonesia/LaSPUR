# R/engine/indices/padu_combine.R
# ============================================================
# Multi-Criteria Composite PADU Index Synthesis Engine
# ============================================================

#' Validate SERASI layer for multi-criteria PADU synthesis
#'
#' Verifies that the input SERASI layer exists and contains the required
#' statutory area function column ('fungsi').
#'
#' @param idx_serasi_map An `sf` or `data.frame` object representing the SERASI layer.
#' @return A list with fields `valid` (logical) and `message` (character).
#' @export
validate_padu_combine_serasi <- function(idx_serasi_map) {
  if (is.null(idx_serasi_map)) {
    return(list(
      valid = FALSE,
      message = "Peta SERASI belum tersedia. Jalankan modul 1.1/1.2 terlebih dahulu atau unggah berkas."
    ))
  }
  if (!"fungsi" %in% names(idx_serasi_map)) {
    return(list(
      valid = FALSE,
      message = "Peta SERASI tidak memiliki kolom 'fungsi'. Silakan jalankan ulang SERASI dengan tabel acuan prioritas."
    ))
  }
  list(valid = TRUE, message = "OK")
}

#' Calculate composite PADU index from multiple standardized or tabular indicators
#'
#' Accepts a list of PADU indicators (standardized engine result lists, `sf` objects,
#' or data frames), joins them to the base planning unit layer, and evaluates
#' composite suitability indices:
#' \itemize{
#'   \item \code{idx_padu_all}: Composite across all available indicators
#'   \item \code{idx_padu_lindung}: Composite supporting protection functions (MFL: ke, hs, kl, kh, ki)
#'   \item \code{idx_padu_budidaya}: Composite supporting cultivation functions (MFB: rtp, se, ki)
#'   \item \code{idx_padu_final}: Dynamic assignment based on the statutory \code{fungsi} column
#' }
#'
#' @param padu_list List of indicator results (each can be a standardized list with \code{$data}, an \code{sf} object, or a \code{data.frame}).
#' @param idx_padu_map An \code{sf} object serving as the base spatial layer containing \code{id_pu}.
#' @param padu_idx_weight Optional data frame or tibble with two columns: indicator code and weight.
#' @return An \code{sf} object with joined individual indices and composite columns.
#' @export
calculate_padu_index <- function(padu_list, idx_padu_map, padu_idx_weight = NULL) {
  if (is.null(padu_list) || length(padu_list) == 0) {
    stop("padu_list must be a non-empty list of indicator results.")
  }
  if (!inherits(idx_padu_map, "sf")) {
    stop("idx_padu_map must be an sf object.")
  }
  
  # Normalize input items in padu_list (extract $data if standardized list is passed)
  padu_list_clean <- lapply(padu_list, function(item) {
    if (is.list(item) && !inherits(item, c("sf", "data.frame")) && "data" %in% names(item)) {
      item <- item$data
    }
    item
  })
  
  # Merge all PADU files onto idx_padu_map
  idx_padu_map <- purrr::reduce(
    padu_list_clean,
    .init = idx_padu_map,
    .f = function(x, y) {
      idx_col <- names(y)[grepl("^idx_padu_", names(y))][1]
      y_clean <- y %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(id_pu = as.integer(id_pu)) %>%
        dplyr::select(id_pu, dplyr::all_of(idx_col))
      dplyr::left_join(x, y_clean, by = "id_pu")
    }
  )

  # Replace NA in individual idx_padu_* columns with 0
  idx_padu_map <- idx_padu_map %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^idx_padu_"), ~tidyr::replace_na(., 0)))

  # Identify available individual PADU index columns
  idx_cols <- names(idx_padu_map)[grepl("^idx_padu_", names(idx_padu_map))]
  if ("idx_padu_ke_abs" %in% idx_cols) {
    message("Removing 'idx_padu_ke_abs' from composite calculation (ignored).")
    idx_cols <- setdiff(idx_cols, "idx_padu_ke_abs")
  }
  idx_cols <- setdiff(idx_cols, c("idx_padu_all", "idx_padu_lindung", "idx_padu_budidaya", "idx_padu_final"))

  if (length(idx_cols) == 0) {
    stop("No valid 'idx_padu_*' indicator columns found in padu_list.")
  }

  idx_codes <- tolower(stringr::str_remove(idx_cols, "^idx_padu_"))

  # Parse weight table if supplied
  weights_df <- NULL
  if (!is.null(padu_idx_weight) && is.data.frame(padu_idx_weight) && nrow(padu_idx_weight) > 0) {
    w_code <- tolower(trimws(as.character(padu_idx_weight[[1]])))
    w_val  <- as.numeric(padu_idx_weight[[2]])
    valid_w <- !is.na(w_code) & !is.na(w_val) & nzchar(w_code)
    weights_df <- data.frame(
      code  = w_code[valid_w],
      value = w_val[valid_w],
      stringsAsFactors = FALSE
    )
  }

  # Indicator pillar groupings:
  # MFL (Mendukung Fungsi Lindung): ke, hs, kl, kh, ki
  # MFB (Mendukung Fungsi Budidaya): rtp, se, ki
  mfl_keys <- c("ke", "hs", "kl", "kh", "ki")
  mfb_keys <- c("rtp", "se", "ki")

  # Helper to compute composite vector with dynamic weight rescaling
  compute_composite <- function(df, target_keys) {
    matched_idx <- which(idx_codes %in% target_keys)
    if (length(matched_idx) == 0) {
      return(rep(NA_real_, nrow(df)))
    }

    cols_to_use  <- idx_cols[matched_idx]
    codes_to_use <- idx_codes[matched_idx]
    k <- length(cols_to_use)

    w <- rep(1 / k, k)
    if (!is.null(weights_df)) {
      matched_weights <- weights_df$value[match(codes_to_use, weights_df$code)]
      if (all(!is.na(matched_weights)) && sum(matched_weights, na.rm = TRUE) > 0) {
        w <- matched_weights / sum(matched_weights, na.rm = TRUE)
      }
    }

    mat <- as.matrix(sf::st_drop_geometry(df[, cols_to_use, drop = FALSE]))
    storage.mode(mat) <- "double"
    as.vector(mat %*% w)
  }

  # Calculate the 3 composite indices
  comp_all      <- compute_composite(idx_padu_map, idx_codes)
  comp_lindung  <- compute_composite(idx_padu_map, mfl_keys)
  comp_budidaya <- compute_composite(idx_padu_map, mfb_keys)

  # Assign active idx_padu_final based on fungsi
  if ("fungsi" %in% names(idx_padu_map)) {
    f_clean <- tolower(trimws(as.character(idx_padu_map$fungsi)))
    comp_final <- ifelse(
      f_clean == "lindung" & !is.na(comp_lindung),
      comp_lindung,
      ifelse(
        f_clean == "budidaya" & !is.na(comp_budidaya),
        comp_budidaya,
        comp_all
      )
    )
  } else {
    comp_final <- comp_all
  }

  idx_padu_map$idx_padu_all      <- comp_all
  idx_padu_map$idx_padu_lindung  <- comp_lindung
  idx_padu_map$idx_padu_budidaya <- comp_budidaya
  idx_padu_map$idx_padu_final    <- comp_final

  return(idx_padu_map)
}
