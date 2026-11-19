# R/engine/07_recommendation.R
# ============================================================
# Alternative Zone Generation, Pair Dissolving & Reconciliation
# ============================================================
# Perhitungan Rekomendasi -------------------------------------------------

# calculate_alternative_zone

#' Determine alternative zones using compatibility matrix
#'
#' @description
#' Given a pair zone (e.g., RZWP3K or RTRW) and a current zone value, this function
#' returns all candidate zones from the opposite classification system that have a
#' higher SERASI index than the current zone. If none are higher, it returns all
#' candidates with the same index (excluding the current zone itself). Ties are
#' included in full.
#'
#' @param pair_zone Character: the zone value used as the filter criterion.
#'   For `return_type = "RTRW"`, this should be an RZWP3K value.
#'   For `return_type = "RZWP3K"`, this should be an RTRW value.
#' @param current_zone Character: the current value of the zone type we are
#'   trying to replace. Used to determine its SERASI index from the matrix.
#' @param return_type Character: either `"RTRW"` or `"RZWP3K"`.
#'   - `"RTRW"`: filter by `class2` (RZWP3K), return a `class1` (RTRW) zone.
#'   - `"RZWP3K"`: filter by `class1` (RTRW), return a `class2` (RZWP3K) zone.
#' @param df A data frame (or tibble) containing the compatibility matrix with
#'   exactly three columns:
#'   \enumerate{
#'     \item `class1` – RTRW zone names
#'     \item `class2` – RZWP3K zone names
#'     \item `idx_serasi` – numeric compatibility scores
#'   }
#' @param n_alt Integer or `NULL`. Maximum number of alternatives to return.
#'   If `NULL` (default), all qualifying alternatives are returned.
#'   If a positive integer, returns at most that many (after sorting by
#'   descending score and then alphabetically).
#'
#' @return A character vector of alternative zone names (length 0 if none).
#'
#' @examples
#' \dontrun{
#' # Example compatibility matrix
#' mat <- tibble::tribble(
#'   ~class1,                          ~class2,                  ~idx_serasi,
#'   "Kawasan Lindung",                "Suaka",                  1.0,
#'   "Kawasan Perikanan",              "Suaka",                  0.5,
#'   "Kawasan Lindung",                "Taman",                  0.8,
#'   "Kawasan Perikanan",              "Taman",                  1.0,
#'   "Kawasan Budidaya",               "Suaka",                  1.0
#' )
#'
#' # Return at most 2 alternatives for RZWP3K = "Suaka", current RTRW = "Kawasan Lindung"
#' get_alternative_zone("Suaka", "Kawasan Lindung", "RTRW", mat, n_alt = 2)
#' # Returns "Kawasan Budidaya" (score 1.0) – only one qualifies, so returns one.
#' }
#'
#' @export
get_alternative_zone <- function(pair_zone, current_zone, return_type, df, n_alt = NULL) {
  
  # Validate n_alt if provided
  if (!is.null(n_alt)) {
    if (!is.numeric(n_alt) || length(n_alt) != 1 || n_alt < 1) {
      stop("n_alt must be NULL or a positive integer")
    }
    n_alt <- as.integer(n_alt)
  }
  
  # Early exit if pair_zone is missing
  if (is.na(pair_zone) || pair_zone == "") return(character(0))
  
  # Determine filter column and return column based on return_type
  if (return_type == "RTRW") {
    filter_col <- 2   # class2 (RZWP3K)
    return_col <- 1   # class1 (RTRW)
  } else if (return_type == "RZWP3K") {
    filter_col <- 1   # class1 (RTRW)
    return_col <- 2   # class2 (RZWP3K)
  } else {
    stop("return_type must be 'RTRW' or 'RZWP3K'")
  }
  
  # Filter rows where the filter column equals pair_zone
  filtered <- df[df[[filter_col]] == pair_zone, ]
  if (nrow(filtered) == 0) return(character(0))
  
  # Extract candidates and scores
  candidates <- filtered[[return_col]]
  scores <- filtered[[3]]  # idx_serasi
  
  # Find the score of the current zone
  current_idx <- which(candidates == current_zone)
  if (length(current_idx) == 0) {
    # If current_zone is not in the matrix for this pair, we cannot compare.
    # Return all candidates as a fallback (with warning).
    warning("current_zone not found in the filtered data for pair_zone = ", pair_zone,
            ". Returning all candidates.")
    selected <- candidates
  } else {
    current_score <- scores[current_idx[1]]  
    
    # Find candidates with a higher score than current
    higher_mask <- scores > current_score
    if (any(higher_mask)) {
      selected <- candidates[higher_mask]
    } else {
      # If none higher, select candidates with equal score but not the current zone itself
      equal_mask <- scores == current_score & candidates != current_zone
      selected <- candidates[equal_mask]
    }
  }
  
  # Sort selected by descending score and then alphabetically for ties
  if (length(selected) > 0) {
    # Get scores for selected
    sel_scores <- scores[candidates %in% selected]
    ord <- order(-sel_scores, selected)
    selected <- selected[ord]
    
    # Apply limit if n_alt is provided
    if (!is.null(n_alt)) {
      selected <- head(selected, n_alt)
    }
  }
  
  return(selected)
}

#' Compute system recommendation for adjacent cases
#'
#' Uses the top-1 alternatives (`alt_RTRW_1`, `alt_RZWP3K_1`) produced by
#' `get_alternative_zone()` to derive the projected PADAN deltas `dR`/`dZ`,
#' then applies the priority-based recommendation rule. Falls back to the
#' other side (or "Tetap/Koordinasi") when the recommended side has no
#' alternative.
#'
#' @param df Data frame with one row per `id_pu`. Must contain `RTRW`,
#'   `RZWP3K`; optionally `idx_padan`, `idx_padu_final`, `alt_RTRW_1`,
#'   `alt_RZWP3K_1`.
#' @param priority_rtrw Character vector of priority RTRW zones.
#' @param priority_rzwp3k Character vector of priority RZWP3K zones.
#' @param matriks_serasi Long-form SERASI matrix (`class1`, `class2`,
#'   `idx_serasi`).
#' @param alpha Numeric weight in `[0,1]` (default `0.5`).
#' @return Character vector of recommendations, one per row of `df`.
#' @export
compute_adjacent_recommendation <- function(df, priority_rtrw, priority_rzwp3k,
                                            matriks_serasi, alpha = 0.5) {
  if (!all(c("RTRW", "RZWP3K") %in% names(df))) {
    stop("compute_adjacent_recommendation: df must contain 'RTRW' and 'RZWP3K'.")
  }
  
  n <- nrow(df)
  if (n == 0) return(character(0))
  
  rtrw_v <- as.character(df$RTRW)
  rz_v   <- as.character(df$RZWP3K)
  idx_padan_v <- if ("idx_padan"      %in% names(df)) as.numeric(df$idx_padan)      else rep(NA_real_, n)
  idx_padu_v  <- if ("idx_padu_final" %in% names(df)) as.numeric(df$idx_padu_final) else rep(NA_real_, n)
  alt_r <- if ("alt_RTRW_1"   %in% names(df)) as.character(df$alt_RTRW_1)   else rep(NA_character_, n)
  alt_z <- if ("alt_RZWP3K_1" %in% names(df)) as.character(df$alt_RZWP3K_1) else rep(NA_character_, n)
  
  alt_r[alt_r %in% c("", "No alternative")] <- NA_character_
  alt_z[alt_z %in% c("", "No alternative")] <- NA_character_
  
  # Vectorised SERASI lookup
  lookup_keys <- paste(matriks_serasi$class1, matriks_serasi$class2, sep = "||")
  lookup_vals <- matriks_serasi$idx_serasi
  lookup <- function(x, y) {
    lookup_vals[match(paste(x, y, sep = "||"), lookup_keys)]
  }
  
  comp_if_rtrw <- ifelse(!is.na(alt_r) & !is.na(rz_v),   lookup(alt_r, rz_v),   NA_real_)
  comp_if_rz   <- ifelse(!is.na(alt_z) & !is.na(rtrw_v), lookup(rtrw_v, alt_z), NA_real_)
  
  N_if_rtrw <- alpha * comp_if_rtrw + (1 - alpha) * idx_padu_v
  N_if_rz   <- alpha * comp_if_rz   + (1 - alpha) * idx_padu_v
  
  dR <- N_if_rtrw - idx_padan_v
  dZ <- N_if_rz   - idx_padan_v
  
  # Treat missing deltas as "no improvement"
  dR_v <- ifelse(is.na(dR), 0, dR)
  dZ_v <- ifelse(is.na(dZ), 0, dZ)
  
  rtrw_pri <- rtrw_v %in% priority_rtrw
  rz_pri   <- rz_v   %in% priority_rzwp3k
  rtrw_pri[is.na(rtrw_v)] <- FALSE
  rz_pri[is.na(rz_v)]     <- FALSE
  
  rec <- dplyr::case_when(
    rtrw_pri & rz_pri ~ dplyr::case_when(
      dZ_v > dR_v ~ "Ubah RZ",
      dR_v > dZ_v ~ "Ubah RTRW",
      TRUE        ~ "Tetap/Koordinasi"
    ),
    rtrw_pri & dZ_v > 0 ~ "Ubah RZ",
    rz_pri   & dR_v > 0 ~ "Ubah RTRW",
    dR_v > 0 | dZ_v > 0 ~ dplyr::if_else(dR_v >= dZ_v, "Ubah RTRW", "Ubah RZ"),
    TRUE ~ "Tetap/Koordinasi"
  )
  
  no_alt_r <- is.na(alt_r)
  no_alt_z <- is.na(alt_z)
  
  # Fallback if the recommended side has no alternative
  rec <- dplyr::case_when(
    rec == "Ubah RTRW" & no_alt_r & !no_alt_z & dZ_v > 0 ~ "Ubah RZ",
    rec == "Ubah RTRW" & no_alt_r ~ "Tetap/Koordinasi",
    rec == "Ubah RZ"   & no_alt_z & !no_alt_r & dR_v > 0 ~ "Ubah RTRW",
    rec == "Ubah RZ"   & no_alt_z ~ "Tetap/Koordinasi",
    TRUE ~ rec
  )
  
  rec
}

# determine serasi index for the alternative zones

#' Look up idx_serasi value from a compatibility matrix, trying both class orders
#'
#' @description
#' Searches for a matching pair `(class_a, class_b)` in the reference matrix
#' `serasi_df`. If no exact match is found, it tries the reversed order
#' `(class_b, class_a)`. Returns `NA` if neither order yields a match or if
#' any input is `NA`.
#'
#' @param class_a Character string: first class name
#' @param class_b Character string: second class name
#' @param serasi_df Data frame with columns `class1`, `class2`, `idx_serasi`
#'                  (the compatibility matrix)
#'
#' @return A numeric value (the `idx_serasi`) or `NA_real_` if not found.
#'
#' @examples
#' \dontrun{
#' # Sample matrix
#' mat <- data.frame(
#'   class1 = c("A", "B"),
#'   class2 = c("B", "C"),
#'   idx_serasi = c(0.5, 1)
#' )
#'
#' get_alternative_serasi("A", "B", mat)  # returns 0.5 (original order)
#' get_alternative_serasi("B", "A", mat)  # returns 0.5 (reversed order)
#' get_alternative_serasi("X", "Y", mat)  # returns NA
#' }
#'
#' @export
get_alternative_serasi <- function(class_a, class_b, serasi_df) {
  if (is.na(class_a) || is.na(class_b)) return(NA_real_)
  
  # Try original order
  match_row <- serasi_df[serasi_df$class1 == class_a & serasi_df$class2 == class_b, ]
  if (nrow(match_row) == 1) return(match_row$idx_serasi)
  
  # Try reversed order
  match_row <- serasi_df[serasi_df$class1 == class_b & serasi_df$class2 == class_a, ]
  if (nrow(match_row) == 1) return(match_row$idx_serasi)
  
  return(NA_real_)
}

#' Compute top-N alternative zone candidates per row (shared across modules)
#'
#' @description
#' Central helper used by every "Penyusunan Alternatif" module and by the
#' Rekonsiliasi module so the option pools offered to the user are computed
#' exactly the same way. For each row, asks \code{\link{get_alternative_zone}}
#' for better RTRW classes (given the current RZWP3K) and better RZWP3K classes
#' (given the current RTRW), capped at \code{n_alt} per side. Results are
#' widened into \code{alt_RTRW_1..N} and \code{alt_RZWP3K_1..N}.
#'
#' @param df data.frame / tibble / sf with at least columns \code{RTRW},
#'   \code{RZWP3K}. sf input is dropped to geometry internally and the result
#'   is re-attached as sf so downstream \code{st_drop_geometry()} calls still
#'   work.
#' @param matriks_serasi Long-form SERASI matrix: data.frame with
#'   \code{class1}, \code{class2}, \code{idx_serasi}.
#' @param n_alt Integer, max candidates per side (default 5).
#' @return \code{df} augmented with \code{alt_RTRW_1..n_alt},
#'   \code{alt_RZWP3K_1..n_alt}. Short rows are NA-padded.
#' @export
compute_alternative_options <- function(df, matriks_serasi, n_alt = 5L) {
  if (!is.data.frame(df))
    stop("compute_alternative_options: df must be a data.frame.")
  if (!all(c("RTRW", "RZWP3K") %in% names(df)))
    stop("compute_alternative_options: df must contain 'RTRW' and 'RZWP3K'.")
  if (!is.data.frame(matriks_serasi) ||
      !all(c("class1", "class2", "idx_serasi") %in% names(matriks_serasi)))
    stop("compute_alternative_options: matriks_serasi must have class1, class2, idx_serasi.")
  n_alt <- as.integer(n_alt)
  if (is.na(n_alt) || n_alt < 1L)
    stop("compute_alternative_options: n_alt must be a positive integer.")
  
  was_sf   <- inherits(df, "sf")
  geom_col <- if (was_sf) attr(df, "sf_column") else NULL
  
  df_work <- if (was_sf) sf::st_drop_geometry(df) else df
  df_work <- tibble::as_tibble(df_work)
  
  out <- df_work %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      alt_RTRW_list   = list(get_alternative_zone(
        RZWP3K, RTRW, "RTRW", matriks_serasi, n_alt = n_alt)),
      alt_RZWP3K_list = list(get_alternative_zone(
        RTRW, RZWP3K, "RZWP3K", matriks_serasi, n_alt = n_alt))
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest_wider(alt_RTRW_list,   names_sep = "_", names_repair = "unique") %>%
    tidyr::unnest_wider(alt_RZWP3K_list, names_sep = "_", names_repair = "unique") %>%
    dplyr::rename_with(~ gsub("alt_RTRW_list_",   "alt_RTRW_",   .x),
                       dplyr::starts_with("alt_RTRW_list_")) %>%
    dplyr::rename_with(~ gsub("alt_RZWP3K_list_", "alt_RZWP3K_", .x),
                       dplyr::starts_with("alt_RZWP3K_list_"))
  
  if (was_sf && !is.null(geom_col)) {
    out[[geom_col]] <- df[[geom_col]]
    out <- sf::st_as_sf(out, sf_column_name = geom_col)
  }
  out
}

#' Dissolve adjacent feature pairs to one row per `id_pu`
#'
#' @description
#' Takes an sf/data.frame with 2 rows per `id_pu` (one RTRW feature row and
#' one RZWP3K feature row, as produced by `identify_adjacent()`) and returns a
#' 1-row-per-`id_pu` data.frame with side-by-side attributes.
#'
#' Column mapping:
#' \itemize{
#'   \item `id`      → `id_rtrw` + `id_rzwp3k`
#'   \item `RTRW`    → filled from the RTRW side; `RZWP3K` from the RZWP3K side
#'   \item `area_ha` → `area_ha_rtrw` + `area_ha_rzwp3k`
#'   \item everything else → first non-NA value across the two sides
#' }
#'
#' @param sf_obj An `sf` or data.frame in feature form (2 rows per `id_pu`).
#'
#' @return A `tibble` with one row per `id_pu`.
#' @export
dissolve_adjacent_pairs <- function(sf_obj) {
  if (inherits(sf_obj, "sf")) {
    df <- sf::st_drop_geometry(sf_obj)
  } else if (is.data.frame(sf_obj)) {
    df <- as.data.frame(sf_obj, stringsAsFactors = FALSE)
  } else {
    stop("dissolve_adjacent_pairs: 'sf_obj' must be an sf object or data.frame.")
  }
  
  required <- c("id_pu", "RTRW", "RZWP3K")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("dissolve_adjacent_pairs: missing columns: ",
         paste(missing, collapse = ", "))
  }
  
  rtrw_df <- df[!is.na(df$RTRW), , drop = FALSE]
  rz_df   <- df[!is.na(df$RZWP3K), , drop = FALSE]
  
  if (nrow(rtrw_df) != nrow(rz_df)) {
    stop("dissolve_adjacent_pairs: unequal number of RTRW and RZWP3K rows (",
         nrow(rtrw_df), " vs ", nrow(rz_df), ").")
  }
  if (nrow(rtrw_df) == 0) return(tibble::tibble())
  if (!all(rtrw_df$id_pu == rz_df$id_pu)) {
    stop("dissolve_adjacent_pairs: id_pu mismatch between RTRW and RZWP3K rows.")
  }
  
  out <- tibble::tibble(id_pu = rtrw_df$id_pu)
  if ("id" %in% names(df)) {
    out$id_rtrw   <- rtrw_df$id
    out$id_rzwp3k <- rz_df$id
  }
  out$RTRW   <- as.character(rtrw_df$RTRW)
  out$RZWP3K <- as.character(rz_df$RZWP3K)
  if ("area_ha" %in% names(df)) {
    out$area_ha_rtrw   <- rtrw_df$area_ha
    out$area_ha_rzwp3k <- rz_df$area_ha
  }
  
  handled    <- c("id", "id_pu", "RTRW", "RZWP3K", "area_ha")
  other_cols <- setdiff(intersect(names(rtrw_df), names(rz_df)), handled)
  for (col in other_cols) {
    out[[col]] <- dplyr::coalesce(rtrw_df[[col]], rz_df[[col]])
  }
  
  special <- c("id_pu", "id_rtrw", "id_rzwp3k", "RTRW", "RZWP3K",
               "area_ha_rtrw", "area_ha_rzwp3k")
  front <- intersect(special, names(out))
  rest  <- setdiff(names(out), front)
  out[, c(front, rest), drop = FALSE]
}

#' Expand a dissolved adjacent table back to feature form
#'
#' @description
#' Reverse of `dissolve_adjacent_pairs()`. Takes a 1-row-per-`id_pu` table
#' and returns a 2-rows-per-`id_pu` data.frame (one row per side).
#'
#' @param dissolved_df A data.frame with columns `id_pu`, `RTRW`, `RZWP3K`
#'   (optionally `id_rtrw`/`id_rzwp3k` and `area_ha_rtrw`/`area_ha_rzwp3k`).
#' @param decisions_rtrw_col   Optional column name; if present, its value is
#'   kept only on the RTRW row and set to `NA` on the RZWP3K row.
#' @param decisions_rzwp3k_col Optional column name; if present, its value is
#'   kept only on the RZWP3K row and set to `NA` on the RTRW row.
#'
#' @return A `tibble` with 2 rows per `id_pu` (`id`, `id_pu`, `RTRW`, `RZWP3K`,
#'   `area_ha` restored).
#' @export
undissolve_adjacent_pairs <- function(dissolved_df,
                                      decisions_rtrw_col   = NULL,
                                      decisions_rzwp3k_col = NULL) {
  df <- as.data.frame(dissolved_df, stringsAsFactors = FALSE)
  
  if (!"id_pu"  %in% names(df)) stop("undissolve_adjacent_pairs: 'id_pu' missing.")
  if (!"RTRW"   %in% names(df)) stop("undissolve_adjacent_pairs: 'RTRW' missing.")
  if (!"RZWP3K" %in% names(df)) stop("undissolve_adjacent_pairs: 'RZWP3K' missing.")
  
  has_ids        <- all(c("id_rtrw", "id_rzwp3k") %in% names(df))
  has_area_split <- all(c("area_ha_rtrw", "area_ha_rzwp3k") %in% names(df))
  n <- nrow(df)
  if (n == 0) return(tibble::tibble())
  
  ids_rtrw   <- if (has_ids) df$id_rtrw   else seq_len(n)
  ids_rzwp3k <- if (has_ids) df$id_rzwp3k else seq_len(n)
  
  special <- c("id_rtrw", "id_rzwp3k", "RTRW", "RZWP3K",
               "area_ha_rtrw", "area_ha_rzwp3k")
  common_cols <- setdiff(names(df), special)
  
  row_rtrw <- df[, common_cols, drop = FALSE]
  row_rtrw$id     <- ids_rtrw
  row_rtrw$RTRW   <- as.character(df$RTRW)
  row_rtrw$RZWP3K <- NA_character_
  if (has_area_split) row_rtrw$area_ha <- df$area_ha_rtrw
  
  row_rz <- df[, common_cols, drop = FALSE]
  row_rz$id     <- ids_rzwp3k
  row_rz$RTRW   <- NA_character_
  row_rz$RZWP3K <- as.character(df$RZWP3K)
  if (has_area_split) row_rz$area_ha <- df$area_ha_rzwp3k
  
  if (!is.null(decisions_rtrw_col) && decisions_rtrw_col %in% names(row_rz)) {
    row_rz[[decisions_rtrw_col]] <- NA
  }
  if (!is.null(decisions_rzwp3k_col) && decisions_rzwp3k_col %in% names(row_rtrw)) {
    row_rtrw[[decisions_rzwp3k_col]] <- NA
  }
  
  # Ensure identical column ordering before interleaving
  all_cols <- union(names(row_rtrw), names(row_rz))
  for (c_ in setdiff(all_cols, names(row_rtrw))) row_rtrw[[c_]] <- NA
  for (c_ in setdiff(all_cols, names(row_rz)))   row_rz[[c_]]   <- NA
  row_rtrw <- row_rtrw[, all_cols, drop = FALSE]
  row_rz   <- row_rz[,   all_cols, drop = FALSE]
  
  out_list <- vector("list", 2 * n)
  for (i in seq_len(n)) {
    out_list[[2 * i - 1]] <- row_rtrw[i, , drop = FALSE]
    out_list[[2 * i]]     <- row_rz[i,   , drop = FALSE]
  }
  dplyr::bind_rows(out_list)
}

#' Determine alternative zones and create an Excel workbook with dropdowns
#'
#' @param idx_padan_map_filter sf object. For step = "step2", must contain
#'   columns: id, id_pu, RTRW, RZWP3K, admin, area_ha, length, idx_serasi. For
#'   step = "step1", must contain columns: id_pu, id_rtrw, id_rzwp3k, RTRW,
#'   RZWP3K, area_ha, idx_serasi. The row order matters for step2's
#'   lead/lag operations.
#' @param serasi_matrix data.frame or tibble. Compatibility matrix used by
#'   `get_alternative_zone()`.
#' @param step character. Either "step2" (default, uses lead/lag on
#'   neighboring rows' RTRW/RZWP3K to derive alternatives) or "step1" (uses
#'   each row's own RTRW/RZWP3K directly, no lead/lag).
#' @param n_alt integer. Number of alternatives to consider (passed to
#'   `get_alternative_zone`). Default = 5.
#' @param output_dir character. Directory where the output Excel file will be
#'   saved. The file will be named "adjacent_alternative_zones_selections.xlsx"
#'   for step2, or "overlaps_alternative_zones_selections.xlsx" for step1.
#'   Default = "." (current working directory).
#' @param dissolve_step2 logical. If TRUE, dissolve paired rows before
#'   generating alternatives for step2.
#' @param compute_recommendation logical. If TRUE, compute the system
#'   recommendation before writing the template and lock the non-changing
#'   side. Only supported for step = "step2".
#' @param priority_rtrw character vector. RTRW zones marked as priority.
#'   Required when `compute_recommendation = TRUE`.
#' @param priority_rzwp3k character vector. RZWP3K zones marked as priority.
#'   Required when `compute_recommendation = TRUE`.
#' @param alpha numeric in `[0,1]`. Weight used in the PADAN formula.
#'   Required when `compute_recommendation = TRUE`.
#'
#' @return A list with two components:
#'   \item{workbook}{The openxlsx workbook object.}
#'   \item{data}{The final cleaned data frame.}
determine_alternative_zones <- function(idx_padan_map_filter,
                                        serasi_matrix,
                                        step = c("step2", "step1"),
                                        n_alt = 5,
                                        output_dir = ".",
                                        dissolve_step2 = TRUE,
                                        compute_recommendation = FALSE,
                                        priority_rtrw    = NULL,
                                        priority_rzwp3k  = NULL,
                                        alpha = 0.5) {
  
  step <- match.arg(step)
  
  if (!require(openxlsx)) stop("Package 'openxlsx' is required but not installed.")
  if (!require(dplyr))    stop("Package 'dplyr' is required but not installed.")
  if (!require(tidyr))    stop("Package 'tidyr' is required but not installed.")
  if (!require(sf))       stop("Package 'sf' is required but not installed.")
  
  if (!inherits(idx_padan_map_filter, "sf")) {
    stop("'idx_padan_map_filter' must be an sf object.")
  }
  if (nrow(idx_padan_map_filter) == 0) {
    warning("Input idx_padan_map_filter has 0 rows. Returning empty workbook.")
    wb <- createWorkbook()
    addWorksheet(wb, "Data")
    addWorksheet(wb, "Validation_Lists")
    return(list(workbook = wb, data = data.frame()))
  }
  if (!is.data.frame(serasi_matrix)) {
    stop("'serasi_matrix' must be a data.frame or tibble.")
  }
  matriks_serasi <- serasi_matrix
  input_cols <- names(idx_padan_map_filter)
  
  if (compute_recommendation && step != "step2") {
    stop("compute_recommendation = TRUE is only supported for step = 'step2'.")
  }

  if (step == "step2") {
    input_is_dissolved <- all(c("id_rtrw", "id_rzwp3k") %in% input_cols) &&
      !"id" %in% input_cols
    
    if (dissolve_step2) {
      if (input_is_dissolved) {
        work <- sf::st_drop_geometry(idx_padan_map_filter)
      } else {
        required_cols <- c("id", "id_pu", "id_group", "RTRW", "RZWP3K",
                           "admin", "area_ha", "length", "idx_serasi")
        missing <- setdiff(required_cols, input_cols)
        if (length(missing) > 0)
          stop("Input data missing required columns: ",
               paste(missing, collapse = ", "))
        work <- dissolve_adjacent_pairs(idx_padan_map_filter)
      }
      
      base_cols <- c("id_pu", "id_group", "id_rtrw", "id_rzwp3k",
                     "RTRW", "RZWP3K", "admin",
                     "area_ha_rtrw", "area_ha_rzwp3k",
                     "length", "idx_serasi")
      base_cols <- intersect(base_cols, names(work))
      
      idx_padan_map_alt <- compute_alternative_options(
        work, matriks_serasi, n_alt = n_alt)
      
      if (compute_recommendation) {
        if (is.null(priority_rtrw) || is.null(priority_rzwp3k)) {
          stop("compute_recommendation = TRUE requires non-NULL ",
               "priority_rtrw and priority_rzwp3k.")
        }
        rec_vec <- compute_adjacent_recommendation(
          df              = idx_padan_map_alt,
          priority_rtrw   = priority_rtrw,
          priority_rzwp3k = priority_rzwp3k,
          matriks_serasi  = matriks_serasi,
          alpha           = alpha
        )
        idx_padan_map_alt$recommendation <- rec_vec
        idx_padan_map_alt <- idx_padan_map_alt %>%
          mutate(
            alt_RTRW   = dplyr::if_else(recommendation == "Ubah RTRW",
                                        NA_character_, as.character(RTRW)),
            alt_RZWP3K = dplyr::if_else(recommendation == "Ubah RZ",
                                        NA_character_, as.character(RZWP3K))
          )
        
        df_export <- idx_padan_map_alt %>%
          filter(!is.na(id_pu) & id_pu != "") %>%
          select(all_of(base_cols), recommendation, alt_RTRW, alt_RZWP3K,
                 starts_with("alt_RTRW_"), starts_with("alt_RZWP3K_"))
      } else {
        df_export <- idx_padan_map_alt %>%
          filter(!is.na(id_pu) & id_pu != "") %>%
          select(all_of(base_cols),
                 starts_with("alt_RTRW_"), starts_with("alt_RZWP3K_")) %>%
          mutate(alt_RTRW = NA_character_, alt_RZWP3K = NA_character_) %>%
          select(all_of(base_cols), alt_RTRW, alt_RZWP3K, everything())
      }
      
    } else {
      required_cols <- c("id", "id_pu", "id_group", "RTRW", "RZWP3K",
                         "admin", "area_ha", "length", "idx_serasi")
      missing <- setdiff(required_cols, input_cols)
      if (length(missing) > 0)
        stop("Input data missing required columns: ",
             paste(missing, collapse = ", "))
      base_cols <- required_cols
      
      idx_padan_map_alt <- idx_padan_map_filter %>%
        mutate(RZWP3K_plus1 = lead(RZWP3K), RTRW_minus1 = lag(RTRW)) %>%
        rowwise() %>%
        mutate(
          alt_RTRW_list   = list(get_alternative_zone(RZWP3K_plus1, RTRW, "RTRW",
                                                      matriks_serasi, n_alt = n_alt)),
          alt_RZWP3K_list = list(get_alternative_zone(RTRW_minus1, RZWP3K, "RZWP3K",
                                                      matriks_serasi, n_alt = n_alt))
        ) %>%
        ungroup() %>%
        unnest_wider(alt_RTRW_list,   names_sep = "_", names_repair = "unique") %>%
        unnest_wider(alt_RZWP3K_list, names_sep = "_", names_repair = "unique") %>%
        select(-RZWP3K_plus1, -RTRW_minus1) %>%
        rename_with(~ gsub("alt_RTRW_list_",   "alt_RTRW_",   .x),
                    starts_with("alt_RTRW_list_")) %>%
        rename_with(~ gsub("alt_RZWP3K_list_", "alt_RZWP3K_", .x),
                    starts_with("alt_RZWP3K_list_"))
      
      df_export <- idx_padan_map_alt %>%
        st_drop_geometry() %>%
        filter(!is.na(id_pu) & id_pu != "") %>%
        select(all_of(base_cols),
               starts_with("alt_RTRW_"), starts_with("alt_RZWP3K_")) %>%
        mutate(alt_RTRW = NA_character_, alt_RZWP3K = NA_character_) %>%
        select(all_of(base_cols), alt_RTRW, alt_RZWP3K, everything())
    }
    
  } else {
    # step == "step1"
    required_cols <- c("id_pu", "id_rtrw", "id_rzwp3k", "RTRW", "RZWP3K",
                       "admin", "area_ha", "idx_serasi")
    base_cols     <- required_cols
    missing <- setdiff(required_cols, input_cols)
    if (length(missing) > 0)
      stop("Input data missing required columns: ",
           paste(missing, collapse = ", "))
    
    idx_padan_map_alt <- compute_alternative_options(
      idx_padan_map_filter, matriks_serasi, n_alt = n_alt)
    
    df_export <- idx_padan_map_alt %>%
      st_drop_geometry() %>%
      filter(!is.na(id_pu) & id_pu != "") %>%
      select(all_of(base_cols),
             starts_with("alt_RTRW_"), starts_with("alt_RZWP3K_")) %>%
      mutate(alt_RTRW = NA_character_, alt_RZWP3K = NA_character_) %>%
      select(all_of(base_cols), alt_RTRW, alt_RZWP3K, everything())
  }
  
  rtrw_cols   <- grep("^alt_RTRW_",   names(df_export), value = TRUE)
  rzwp3k_cols <- grep("^alt_RZWP3K_", names(df_export), value = TRUE)
  
  clean_alternatives <- function(df, cols, actual_col) {
    mat    <- as.matrix(df[, cols, drop = FALSE])
    actual <- as.character(df[[actual_col]])
    
    cleaned <- t(apply(cbind(mat, actual), 1, function(x) {
      act  <- x[length(x)]
      cand <- x[-length(x)]
      u <- unique(cand[!is.na(cand) & cand != "" & cand != "No alternative"])
      if (length(u) == 0) u <- "No alternative"
      if (!is.na(act) && nzchar(act) && !act %in% u) {
        u <- c(u, act)
      }
      c(u, rep(NA_character_, length(cols) + 1L - length(u)))
    }))
    
    out <- as.data.frame(cleaned, stringsAsFactors = FALSE)
    names(out) <- c(cols, paste0(actual_col, "_actual"))
    out
  }
  
  df_lists_rtrw   <- clean_alternatives(df_export, rtrw_cols,   "RTRW")
  df_lists_rzwp3k <- clean_alternatives(df_export, rzwp3k_cols, "RZWP3K")
  df_validation_lists <- cbind(df_lists_rtrw, df_lists_rzwp3k)
  names(df_validation_lists) <- c(
    paste0("RTRW_",   seq_len(ncol(df_lists_rtrw))),
    paste0("RZWP3K_", seq_len(ncol(df_lists_rzwp3k)))
  )

  df_export_clean <- df_export %>% select(-all_of(c(rtrw_cols, rzwp3k_cols)))
  
  df_export_clean <- df_export_clean %>%
    dplyr::mutate(use_recommendation = "Ya") %>%
    dplyr::relocate(use_recommendation, .after = alt_RZWP3K)

  wb <- createWorkbook()
  addWorksheet(wb, "Data")
  addWorksheet(wb, "Validation_Lists")
  writeData(wb, "Data", df_export_clean, startRow = 1, startCol = 1)
  writeData(wb, "Validation_Lists", df_validation_lists, startRow = 1, startCol = 1)
  
  # Alternating group colouring
  unique_pu <- unique(df_export_clean$id_pu)
  if (length(unique_pu) > 0) {
    style_group1 <- createStyle(fgFill = "#DCE6F1")
    style_group2 <- createStyle(fgFill = "#FFFFFF")
    for (i in seq_along(unique_pu)) {
      rows_excel <- which(df_export_clean$id_pu == unique_pu[i]) + 1
      style <- if (i %% 2 == 1) style_group1 else style_group2
      addStyle(wb, "Data", style = style, rows = rows_excel,
               cols = 1:ncol(df_export_clean), gridExpand = TRUE)
    }
  }
  
  alt_rtrw_col_idx   <- which(names(df_export_clean) == "alt_RTRW")
  alt_rzwp3k_col_idx <- which(names(df_export_clean) == "alt_RZWP3K")
  rtrw_start   <- int2col(1)
  rtrw_end     <- int2col(length(rtrw_cols) + 1)
  rzwp3k_start <- int2col(length(rtrw_cols) + 2)
  rzwp3k_end   <- int2col(length(rtrw_cols) + 1 + length(rzwp3k_cols) + 1)
  
  get_contiguous_blocks <- function(indices) {
    if (length(indices) == 0) return(list())
    split(indices, cumsum(c(1, diff(indices) != 1)))
  }
  
  has_alt_rtrw   <- df_lists_rtrw[, 1]   != "No alternative"
  has_alt_rzwp3k <- df_lists_rzwp3k[, 1] != "No alternative"
  
  if (compute_recommendation && "recommendation" %in% names(df_export_clean)) {
    rec <- df_export_clean$recommendation
    
    rtrw_editable   <- rec == "Ubah RTRW" & has_alt_rtrw
    rz_editable     <- rec == "Ubah RZ"   & has_alt_rzwp3k
    valid_rows_rtrw   <- which(rtrw_editable) + 1
    valid_rows_rzwp3k <- which(rz_editable)   + 1
    
    for (block in get_contiguous_blocks(valid_rows_rtrw)) {
      start_r <- min(block); end_r <- max(block)
      dataValidation(wb, "Data", cols = alt_rtrw_col_idx, rows = start_r:end_r,
                     type = "list",
                     value = sprintf("'Validation_Lists'!$%s%d:$%s%d",
                                     rtrw_start, start_r, rtrw_end, start_r))
    }
    for (block in get_contiguous_blocks(valid_rows_rzwp3k)) {
      start_r <- min(block); end_r <- max(block)
      dataValidation(wb, "Data", cols = alt_rzwp3k_col_idx, rows = start_r:end_r,
                     type = "list",
                     value = sprintf("'Validation_Lists'!$%s%d:$%s%d",
                                     rzwp3k_start, start_r, rzwp3k_end, start_r))
    }
    
    gray_style  <- createStyle(fgFill = "#E0E0E0")
    black_style <- createStyle(fgFill = "#000000", fontColour = "#000000")
    
    locked_rtrw <- which(rec != "Ubah RTRW") + 1
    locked_rz   <- which(rec != "Ubah RZ")   + 1
    if (length(locked_rtrw) > 0)
      addStyle(wb, "Data", style = gray_style,
               cols = alt_rtrw_col_idx, rows = locked_rtrw, gridExpand = FALSE)
    if (length(locked_rz) > 0)
      addStyle(wb, "Data", style = gray_style,
               cols = alt_rzwp3k_col_idx, rows = locked_rz, gridExpand = FALSE)
    
    for (r in which(rec == "Ubah RTRW" & !has_alt_rtrw) + 1)
      addStyle(wb, "Data", style = black_style, cols = alt_rtrw_col_idx, rows = r)
    for (r in which(rec == "Ubah RZ" & !has_alt_rzwp3k) + 1)
      addStyle(wb, "Data", style = black_style, cols = alt_rzwp3k_col_idx, rows = r)
    
  } else {
    valid_rows_rtrw   <- which(has_alt_rtrw)   + 1
    valid_rows_rzwp3k <- which(has_alt_rzwp3k) + 1
    for (block in get_contiguous_blocks(valid_rows_rtrw)) {
      start_r <- min(block); end_r <- max(block)
      dataValidation(wb, "Data", cols = alt_rtrw_col_idx, rows = start_r:end_r,
                     type = "list",
                     value = sprintf("'Validation_Lists'!$%s%d:$%s%d",
                                     rtrw_start, start_r, rtrw_end, start_r))
    }
    for (block in get_contiguous_blocks(valid_rows_rzwp3k)) {
      start_r <- min(block); end_r <- max(block)
      dataValidation(wb, "Data", cols = alt_rzwp3k_col_idx, rows = start_r:end_r,
                     type = "list",
                     value = sprintf("'Validation_Lists'!$%s%d:$%s%d",
                                     rzwp3k_start, start_r, rzwp3k_end, start_r))
    }
    
    black_style <- createStyle(fgFill = "#000000", fontColour = "#000000")
    for (r in which(!has_alt_rtrw) + 1)
      addStyle(wb, "Data", style = black_style, cols = alt_rtrw_col_idx, rows = r)
    for (r in which(!has_alt_rzwp3k) + 1)
      addStyle(wb, "Data", style = black_style, cols = alt_rzwp3k_col_idx, rows = r)
  }
  
  use_rec_col_idx <- which(names(df_export_clean) == "use_recommendation")
  if (length(use_rec_col_idx) == 1) {
    dataValidation(
      wb, "Data",
      cols = use_rec_col_idx,
      rows = 2:(nrow(df_export_clean) + 1),
      type = "list",
      value = '"Ya,Tidak"',
      allowBlank = FALSE,
      showErrorMsg = TRUE
    )
  }
  
  freezePane(wb, "Data", firstRow = TRUE)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_filename <- if (step == "step2") "adjacent_alternative_zones_selections.xlsx"
  else                 "overlaps_alternative_zones_selections.xlsx"
  saveWorkbook(wb, file.path(output_dir, output_filename), overwrite = TRUE)
  
  return_data <- df_export_clean
  alt_cols <- c(grep("^alt_RTRW_[0-9]+$",   names(df_export), value = TRUE),
                grep("^alt_RZWP3K_[0-9]+$", names(df_export), value = TRUE))
  if (length(alt_cols) > 0 &&
      "id_pu" %in% names(df_export) &&
      "id_pu" %in% names(return_data)) {
    idx <- match(as.character(return_data$id_pu),
                 as.character(df_export$id_pu))
    for (col in alt_cols) {
      return_data[[col]] <- df_export[[col]][idx]
    }
  }
  
  invisible(list(workbook = wb, data = return_data))
}

#' Calculate economic values (NPV) from land use/cover distributions
#'
#' This function performs the full economic assessment (Stages 7.1–7.3) for
#' RTRW and RZWP3K zones. It computes actual and recommended land cover
#' distributions, NPV per hectare, and total economic values, then joins
#' the results back to the input spatial feature set.
#'
#' @param alt_map_with_decision An `sf` object containing planning units,
#'   with columns: `id`, `id_pu`, `RTRW`, `RZWP3K`, and `area_ha` (or as
#'   specified). The geometry is preserved.
#' @param npv_lulc A data frame with LULC classes and their NPV per hectare
#'   (columns as expected by `calculate_npv_ha_per_unit()`).
#' @param matriks_land_distribution_rtrw A matrix or data frame of land
#'   distribution rules for RTRW zones.
#' @param matriks_land_distribution_rzwp3k A matrix or data frame of land
#'   distribution rules for RZWP3K zones.
#' @param class_name Character. Name of the LULC class column (used in pivoting).
#'   Default `"LC"`. This must match the class names in the matrix tables.
#' @param id_pu Character. Name of the planning unit ID column. Default `"id_pu"`.
#' @param area_col Character. Name of the area column (in hectares). Default
#'   `"area_ha"`.
#' @param id_col Character. Name of the unique feature ID column (for merging).
#'   Default `"id"`.
#'
#' @return An `sf` object identical to `alt_map_with_decision` but augmented
#'   with additional columns:
#'   - `npv_ha_actual_rtrw`, `npv_ha_actual_rzwp3k`
#'   - `npv_ha_recom_rtrw`, `npv_ha_recom_rzwp3k`
#'   - `econ_rtrw_actual`, `econ_rtrw_recom`, `econ_rtrw_delta`
#'   - `econ_rzwp3k_actual`, `econ_rzwp3k_recom`, `econ_rzwp3k_delta`
#'
#' @importFrom dplyr filter select left_join mutate rename all_of
#' @importFrom tidyr pivot_wider
#' @importFrom sf st_drop_geometry
#' @export
calculate_economic_npv <- function(
    alt_map_with_decision,
    npv_lulc,
    matriks_land_distribution_rtrw,
    matriks_land_distribution_rzwp3k,
    class_name = "LC",
    id_pu = "id_pu",
    area_col = "area_ha",
    id_col = "id"
) {
  # Input validation 
  required_cols <- c(id_col, id_pu, "RTRW", "RZWP3K", area_col)
  missing <- setdiff(required_cols, names(alt_map_with_decision))
  if (length(missing) > 0) {
    stop("alt_map_with_decision missing columns: ", paste(missing, collapse = ", "))
  }
  
  # Filter planning units with non-NA zone assignments
  pu_rtrw <- alt_map_with_decision %>%
    dplyr::filter(!is.na(RTRW)) %>%
    dplyr::select(-RZWP3K)
  
  pu_rzwp3k <- alt_map_with_decision %>%
    dplyr::filter(!is.na(RZWP3K)) %>%
    dplyr::select(-RTRW)
  
  # Helper to get distribution (long format)
  get_distribution <- function(pu, matrix_tbl, zone_type) {
    calculate_land_distribution(
      pu = pu,
      id_pu = id_pu,
      lulc_class = class_name,
      calculate_from_matrix = TRUE,
      matrix_tbl = matrix_tbl,
      selected_zone = zone_type
    )
  }
  
  # RTRW actual
  land_dist_rtrw_long <- get_distribution(pu_rtrw, matriks_land_distribution_rtrw, "RTRW")
  land_dist_rtrw_wide <- land_dist_rtrw_long %>%
    tidyr::pivot_wider(
      names_from = all_of(class_name),
      values_from = proportion,
      values_fill = 0
    )
  land_dist_rtrw <- sf::st_drop_geometry(pu_rtrw) %>%
    dplyr::left_join(land_dist_rtrw_wide, by = id_pu)
  
  # RZWP3K actual
  land_dist_rzwp3k_long <- get_distribution(pu_rzwp3k, matriks_land_distribution_rzwp3k, "RZWP3K")
  land_dist_rzwp3k_wide <- land_dist_rzwp3k_long %>%
    tidyr::pivot_wider(
      names_from = all_of(class_name),
      values_from = proportion,
      values_fill = 0
    )
  land_dist_rzwp3k <- sf::st_drop_geometry(pu_rzwp3k) %>%
    dplyr::left_join(land_dist_rzwp3k_wide, by = id_pu)
  
  # Actual NPV/ha
  npv_actual_rtrw <- calculate_npv_ha_per_unit(land_dist_rtrw, npv_lulc) %>%
    dplyr::rename(npv_ha_actual_rtrw = npv_ha)
  
  npv_actual_rzwp3k <- calculate_npv_ha_per_unit(land_dist_rzwp3k, npv_lulc) %>%
    dplyr::rename(npv_ha_actual_rzwp3k = npv_ha)
  
  # Recommended LULC distribution and NPV/ha
  # RTRW recommended
  land_dist_rtrw_recom_only <- calculate_land_distribution(
    pu = pu_rtrw,
    id_pu = id_pu,
    lulc_class = class_name,
    calculate_from_matrix = TRUE,
    matrix_tbl = matriks_land_distribution_rtrw,
    selected_zone = "alt_RTRW"
  ) %>%
    tidyr::pivot_wider(
      names_from = all_of(class_name),
      values_from = proportion,
      values_fill = 0
    ) %>%
    sf::st_drop_geometry()
  
  land_dist_rtrw_recom <- npv_actual_rtrw %>%
    dplyr::left_join(land_dist_rtrw_recom_only, by = id_pu)
  
  # RZWP3K recommended
  land_dist_rzwp3k_recom_only <- calculate_land_distribution(
    pu = pu_rzwp3k,
    id_pu = id_pu,
    lulc_class = class_name,
    calculate_from_matrix = TRUE,
    matrix_tbl = matriks_land_distribution_rzwp3k,
    selected_zone = "alt_RZWP3K"
  ) %>%
    tidyr::pivot_wider(
      names_from = all_of(class_name),
      values_from = proportion,
      values_fill = 0
    ) %>%
    sf::st_drop_geometry()
  
  land_dist_rzwp3k_recom <- npv_actual_rzwp3k %>%
    dplyr::left_join(land_dist_rzwp3k_recom_only, by = id_pu)
  
  # Recommended NPV/ha
  npv_recom_rtrw <- calculate_npv_ha_per_unit(land_dist_rtrw_recom, npv_lulc) %>%
    dplyr::rename(npv_ha_recom_rtrw = npv_ha)
  
  npv_recom_rzwp3k <- calculate_npv_ha_per_unit(land_dist_rzwp3k_recom, npv_lulc) %>%
    dplyr::rename(npv_ha_recom_rzwp3k = npv_ha)
  
  # Total economic values
  # RTRW
  npv_rtrw <- npv_recom_rtrw %>%
    dplyr::mutate(
      econ_rtrw_actual = .data[[area_col]] * npv_ha_actual_rtrw,
      econ_rtrw_recom  = .data[[area_col]] * npv_ha_recom_rtrw,
      econ_rtrw_delta  = econ_rtrw_recom - econ_rtrw_actual
    )
  
  # RZWP3K
  npv_rzwp3k <- npv_recom_rzwp3k %>%
    dplyr::mutate(
      econ_rzwp3k_actual = .data[[area_col]] * npv_ha_actual_rzwp3k,
      econ_rzwp3k_recom  = .data[[area_col]] * npv_ha_recom_rzwp3k,
      econ_rzwp3k_delta  = econ_rzwp3k_recom - econ_rzwp3k_actual
    )
  
  # Merge into final map
  # Identify new columns not already in alt_map_with_decision
  new_cols_rtrw <- setdiff(names(npv_rtrw), names(alt_map_with_decision))
  new_cols_rzwp3k <- setdiff(names(npv_rzwp3k), names(alt_map_with_decision))
  
  adjacent_economy_map <- alt_map_with_decision %>%
    dplyr::left_join(
      npv_rtrw %>% dplyr::select(dplyr::all_of(c(id_col, id_pu, new_cols_rtrw))),
      by = c(id_col, id_pu)
    ) %>%
    dplyr::left_join(
      npv_rzwp3k %>% dplyr::select(dplyr::all_of(c(id_col, id_pu, new_cols_rzwp3k))),
      by = c(id_col, id_pu)
    )
  
  return(adjacent_economy_map)
}

#' Generate an Excel file for reconciliation with dropdown validation
#'
#' This function creates an Excel workbook containing spatial reconciliation data
#' and provides dropdown lists for manual decision entries. It takes an `sf`
#' object, extracts its attribute table, adds decision columns, and sets up
#' data validation using predefined priority options. 
#'
#' @param recon_map An `sf` object (spatial data frame) containing the
#'   reconciliation map. Its geometry column is dropped before writing to Excel.
#' @param rtrw_prioritas A data frame with a column named `"RTRW"` containing
#'   the valid priority options for the RTRW decision dropdown. Non-`NA` values
#'   are used as the list of choices.
#' @param rzwp3k_prioritas A data frame with a column named `"RZWP3K"` containing
#'   the valid priority options for the RZWP3K decision dropdown. Non-`NA` values
#'   are used as the list of choices.
#' @param step Integer, must be `1` or `2`. If `2`, the workbook contains two
#'   decision columns (`decision_rtrw` and `decision_rzwp3k`) with separate
#'   dropdown lists. If `1`, only one column (`user_decision`) is added, with a
#'   dropdown combining both lists (simple concatenation, no deduplication).
#' @param output_dir Character string specifying the directory where the Excel
#'   file will be saved. The directory is created recursively if it does not exist.
#' @param file_name Character string giving the name of the output Excel file.
#'   Defaults to `"recon_map.xlsx"`.
#' @param group_col Character string naming a column in `recon_map` to group rows
#'   for alternating background colors. If `NULL` (default), the function looks
#'   for a column named `"id_pu"`; if found, it is used. If no suitable column
#'   exists, no row coloring is applied.
#'
#' @return The function is called for its side effect of creating an Excel file.
#'   It returns `NULL` invisibly.
#'
#' @details The workbook now contains three sheets:
#' \itemize{
#'   \item **Data**: Main data with dropdown validations.
#'   \item **Lists**: Option lists for dropdowns.
#'   \item **Glossary**: Column descriptions extracted from the documentation.
#' }
#' Additionally, the header row and the first four columns of the **Data** sheet
#' are frozen for easier navigation.
#'
#' @examples
#' \dontrun{
#' # Step 2 with grouping and glossary
#' generate_reconciliation_excel(recon_sf, rtrw_prior, rzp3k_prior, 
#'                               step = 2, output_dir = "out",
#'                               group_col = "id_pu")
#' }
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData dataValidation
#'   saveWorkbook createStyle addStyle freezePane
#' @importFrom sf st_drop_geometry
#' @export
#'
#' @param sf_obj An `sf` object (simple feature collection) that must contain 
#'   the following columns: `id`, `id_pu`, `RTRW`, `RZWP3K`, `area_ha`, 
#'   `length`, `area_buffer_ha`, `idx_serasi`, and `admin`. Each `id_pu` should 
#'   have exactly two rows: one with a non-`NA` `RTRW` and the other with a 
#'   non-`NA` `RZWP3K`.
#'
#' @return An `sf` object with one feature per unique `id_pu`. The output 
#'   includes the following columns:
#'   \itemize{
#'     \item `id`: new identifier in the format `"<RTRW_id>_<RZWP3K_id>"`.
#'     \item `id_pu`: original grouping identifier.
#'     \item `RTRW`: the non-`NA` value from the RTRW row.
#'     \item `RZWP3K`: the non-`NA` value from the RZWP3K row.
#'     \item `area_ha`: numeric sum of the two areas.
#'     \item `admin`: concatenated admin values in the format 
#'       `"<RTRW_admin>_<RZWP3K_admin>"`.
#'     \item `length`: first value (assumed identical).
#'     \item `area_buffer_ha`: first value (assumed identical).
#'     \item `idx_serasi`: first value (assumed identical).
#'     \item `geometry`: unioned (dissolved) geometry.
#'   }
#'   The CRS is preserved from the input object.
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Splits the data into RTRW and RZWP3K subsets.
#'   \item Validates that the number of rows and `id_pu` values match between 
#'         the two subsets.
#'   \item Joins the attribute tables to combine `id`, `area_ha`, and `admin` 
#'         from both rows, then computes the new `id`, sums `area_ha`, and 
#'         concatenates `admin`.
#'   \item Unions the geometries for each `id_pu`.
#'   \item Merges the attributes with the unioned geometries and restores the CRS.
#' }
#'
#' @importFrom dplyr filter st_drop_geometry select inner_join mutate 
#'   group_by summarise rename st_as_sf
#' @importFrom sf st_union st_crs
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Assuming `idx_serasi_map` is your sf object with the required columns
#' dissolved <- dissolve_id_pu(idx_serasi_map)
#' plot(dissolved["area_ha"])
#' }
#'
#' @export
dissolve_id_pu <- function(sf_obj) {
  
  base_required <- c("id", "id_pu", "RTRW", "RZWP3K", "area_ha", "admin")
  stopifnot(all(base_required %in% colnames(sf_obj)))
  
  geom_col <- attr(sf_obj, "sf_column")
  
  rtrw   <- sf_obj %>% filter(!is.na(RTRW))
  rzwp3k <- sf_obj %>% filter(!is.na(RZWP3K))
  
  if (nrow(rtrw) != nrow(rzwp3k)) stop("Unequal number of RTRW and RZWP3K rows.")
  if (!all(rtrw$id_pu == rzwp3k$id_pu)) stop("Mismatched id_pu between RTRW and RZWP3K rows.")
  
  special_cols <- c("id", "id_pu", "RTRW", "RZWP3K", "area_ha", "admin", geom_col)
  generic_cols <- setdiff(colnames(sf_obj), special_cols)
  
  rtrw_df <- rtrw %>% st_drop_geometry() %>%
    select(id_pu, id_rtrw = id, RTRW, area_ha_rtrw = area_ha, admin_rtrw = admin,
           dplyr::all_of(generic_cols))
  
  rzwp3k_df <- rzwp3k %>% st_drop_geometry() %>%
    select(id_pu, id_rzwp3k = id, RZWP3K, area_ha_rzwp3k = area_ha, admin_rzwp3k = admin,
           dplyr::all_of(generic_cols)) %>%
    rename_with(~ paste0(.x, "_alt"), dplyr::all_of(generic_cols))
  
  combined <- rtrw_df %>%
    inner_join(rzwp3k_df, by = "id_pu") %>%
    mutate(
      new_id  = paste0(id_rtrw, "_", id_rzwp3k),
      area_ha = area_ha_rtrw + area_ha_rzwp3k,
      admin   = paste0(admin_rtrw, "_", admin_rzwp3k)
    )
  
  for (col in generic_cols) {
    combined[[col]] <- dplyr::coalesce(combined[[col]], combined[[paste0(col, "_alt")]])
  }
  
  combined <- combined %>%
    select(id_pu, new_id, RTRW, RZWP3K, area_ha, admin, dplyr::all_of(generic_cols))
  
  geom_union <- sf_obj %>%
    group_by(id_pu) %>%
    summarise(geometry = st_union(.data[[geom_col]]), .groups = "drop") %>%
    sf::st_make_valid()
  
  result <- geom_union %>%
    inner_join(combined, by = "id_pu") %>%
    rename(id = new_id) %>%
    select(id, id_pu, RTRW, RZWP3K, area_ha, admin, dplyr::all_of(generic_cols), geometry) %>%
    st_as_sf()
  
  st_crs(result) <- st_crs(sf_obj)
  result
}

# Look up compatibility
get_compat <- function(x, y) {
  if (is.na(x) || is.na(y)) return(NA_real_)
  val <- matriks_serasi %>%
    filter(class1 == x, class2 == y) %>%
    pull(idx_serasi)
  if (length(val) == 0) NA_real_ else val
}

#' Reconcile spatial layer with update and exclusion mask (Step 2)
#'
#' This function integrates an updated spatial layer into a base map, using an exclusion mask
#' to prevent updates from extending into protected areas. It trims the updates, removes the
#' updated footprint from the base map, and combines the unchanged base areas with the updated
#' polygons. The result includes attributes indicating whether each polygon was reconciled,
#' whether it originated from the update layer (adjacent), and carries forward relevant identifiers.
#'
#' @param base_map An `sf` object representing the base map. Must contain an `id` column and a column
#'   named after `layer_name` (the original class column). If any of the required additional columns
#'   (`id_pu`, `idx_serasi`, `idx_padu_final`, `idx_padan`) are missing, they will be added as `NA`.
#' @param exclusion_mask An `sf` object containing polygons that define areas where updates should
#'   be excluded (e.g., protected areas). These areas are subtracted from the update polygons.
#' @param update_layer An `sf` object containing the updated polygons. Must contain columns named
#'   `layer_name` (original class) and `paste0("user_decision_", layer_name)` (new class).
#'   It may also contain `id`, `id_pu`, and the index columns; if missing, `NA` will be used.
#' @param layer_name A character string naming the thematic layer being processed (e.g., `"rtrw"`).
#'   This is used to construct column names.
#'
#' @return An `sf` object with `MULTIPOLYGON` geometry. The output includes the following columns:
#'   \item{id_pu}{Original polygon identifier.}
#'   \item{\{layer_name\}_old}{Original class values.}
#'   \item{\{layer_name\}_new}{New class values (named as `user_decision_{layer_name}`).}
#'   \item{Reconcile}{Character flag: `"Yes"` if the polygon was updated (class change), otherwise `"No"`.}
#'   \item{adjacent}{Character flag: `"Yes"` if the polygon originated from the update layer, `"No"` if it originated from the base map.}
#'   \item{idx_serasi}{Index value from base map (unchanged) or update layer (updated).}
#'   \item{idx_padu_final}{Likewise.}
#'   \item{idx_padan}{Likewise.}
#'   \item{geometry}{`MULTIPOLYGON` geometry.}
#'
#' @details The function performs several steps:
#' \enumerate{
#'   \item Ensures the required columns exist in both `base_map` and `update_layer`.
#'   \item Prepares the update layer, assigning `adjacent = "Yes"`.
#'   \item Trims the update polygons using [sf::st_difference()] against the exclusion mask.
#'   \item Subtracts the update footprint from the base map, assigning `adjacent = "No"` to the preserved base areas.
#'   \item Binds the layers and determines `Reconcile` status.
#' }
#'
#' @import sf dplyr rlang
#' @export
reconcile_map_step2 <- function(base_map, exclusion_mask, update_layer, layer_name) {
  col_orig       <- layer_name
  col_new        <- tolower(paste0("user_decision_", layer_name))
  col_old_output <- paste0(layer_name, "_old")
  
  id_cols  <- "id_pu"
  idx_cols <- c("idx_serasi", "idx_padu_final", "idx_padan")
  
  for (col in c(id_cols, idx_cols)) {
    if (!col %in% names(base_map))      base_map[[col]]      <- NA
    if (!col %in% names(update_layer))  update_layer[[col]]  <- NA
  }
  if (!"id" %in% names(base_map)) base_map$id <- seq_len(nrow(base_map))

  normalize_geom <- function(x) {
    gcol <- attr(x, "sf_column")
    if (!is.null(gcol) && gcol != "geometry") {
      names(x)[names(x) == gcol] <- "geometry"
      sf::st_geometry(x) <- "geometry"
    }
    x
  }
  base_map       <- normalize_geom(base_map)
  update_layer   <- normalize_geom(update_layer)
  exclusion_mask <- normalize_geom(exclusion_mask)
  
  message("Processing integration for: ", layer_name, " ...")
  
  max_base_id <- suppressWarnings(max(base_map$id, na.rm = TRUE))
  if (!is.finite(max_base_id)) max_base_id <- 0L

  update_prep <- update_layer %>%
    dplyr::filter(!sf::st_is_empty(.data$geometry)) %>%
    dplyr::filter(!is.na(!!rlang::sym(col_new))) %>%
    dplyr::select(
      Old = !!rlang::sym(col_orig),
      New = !!rlang::sym(col_new),
      dplyr::all_of(id_cols),
      dplyr::all_of(idx_cols),
      dplyr::all_of("geometry")
    ) %>%
    dplyr::mutate(
      id       = as.integer(max_base_id + dplyr::row_number()),
      Adjacent = "Yes"
    ) %>%
    sf::st_make_valid()

  mask_geom <- sf::st_combine(sf::st_geometry(sf::st_make_valid(exclusion_mask)))
  
  update_trimmed <- sf::st_difference(update_prep, mask_geom) %>%
    sf::st_collection_extract("POLYGON")
  
  update_footprint <- sf::st_union(update_trimmed)

  base_cutout <- sf::st_difference(sf::st_make_valid(base_map), update_footprint) %>%
    dplyr::rename(Old = !!rlang::sym(col_orig)) %>%
    dplyr::mutate(New = Old, Adjacent = "No")

  final_map <- dplyr::bind_rows(base_cutout, update_trimmed) %>%
    dplyr::mutate(
      Reconcile = dplyr::if_else(
        dplyr::coalesce(as.character(Old), "") ==
          dplyr::coalesce(as.character(New), ""),
        "No", "Yes"
      )
    ) %>%
    dplyr::rename(
      !!rlang::sym(col_old_output) := Old,
      !!rlang::sym(col_new)        := New
    ) %>%
    dplyr::select(
      dplyr::all_of(id_cols),
      dplyr::all_of(col_old_output),
      dplyr::all_of(col_new),
      "Reconcile", "Adjacent",
      dplyr::all_of(idx_cols),
      "geometry"
    ) %>%
    sf::st_make_valid() %>%
    sf::st_cast("MULTIPOLYGON")
  
  message("Success! Integrated map for ", layer_name, " generated.")
  return(final_map)
}

#' Step 2 of reconciliation: process and integrate RTRW/RZWP3K with compatibility
#'
#' This function reads a reconciliation table, computes majority decisions,
#' joins them to a spatial layer, reconciles two base maps using a custom
#' reconciliation function, standardizes and merges the results, then computes
#' compatibility indices and updates the spatial data frame.
#'
#' @param recon_table_path Path to the Excel reconciliation table.
#' @param adjacent_recom_map An sf object containing the adjacent reconciliation polygons.
#'   Must have columns `id`, `id_pu`, and geometry.
#' @param rtrw_vect RTRW base map (spatial object) accepted by `reconcile_map_step2`.
#' @param rzwp3k_vect RZWP3K base map (spatial object) accepted by `reconcile_map_step2`.
#' @param matriks_serasi A pre-loaded compatibility matrix (as returned by
#'   `load_validate_matrix_table`). This matrix is used by `get_compat`.
#' @param alpha Numeric weight for the new compatibility index (default 0.5).
#'
#' @return An sf object `integrated_map_idx` with columns including
#'   `id_pu`, `Source`, `Zoning_Old`, `Zoning_New`, `Adjacent`, `Reconcile`,
#'   `idx_serasi`, `idx_padu_final`, `idx_padan`, `idx_serasi_new`,
#'   `idx_padan_new`, `delta_idx_padan`, and geometry.
#'
#' @details This function depends on the following external functions that must
#'   be defined in the calling environment:
#'   - `reconcile_map_step2(base_map, exclusion_mask, update_layer, layer_name)`
#'   - `get_compat(z_a, z_b)` – uses the loaded compatibility matrix (`matriks_serasi`).
#'
#'   The compatibility matrix is expected to be loaded externally (e.g., via
#'   `load_validate_matrix_table`) and passed as an argument to avoid reloading.
#'
#'   It also requires the following packages: `dplyr`, `tidyr`, `sf`, `readxl`, `purrr`.
#'
#' @examples
#' \dontrun{
#' matriks <- load_validate_matrix_table("path/to/matriks_serasi.xlsx", title = "serasi")
#' result <- reconcilliation_step2(
#'   recon_table_path = "D:/step2_sulteng/adjacent_reconcilliation_table_filled.xlsx",
#'   adjacent_recom_map = my_adjacent_map,
#'   rtrw_vect = rtrw_spatial,
#'   rzwp3k_vect = rzwp3k_spatial,
#'   matriks_serasi = matriks,
#'   alpha = 0.5
#' )
#' }
reconcilliation_step2 <- function(recon_table_path,
                                  adjacent_recom_map,
                                  rtrw_vect,
                                  rzwp3k_vect,
                                  matriks_serasi,
                                  alpha = 0.5) {
  
  get_compat <- function(x, y) {
    if (is.na(x) || is.na(y)) return(NA_real_)
    val <- matriks_serasi %>%
      dplyr::filter(class1 == x, class2 == y) %>%
      dplyr::pull(idx_serasi)
    if (length(val) == 0) NA_real_ else val[1]
  }
  
  # Read reconciliation table
  if (is.data.frame(recon_table_path)) {
    recon_table <- recon_table_path
  } else {
    recon_table <- read_xlsx(recon_table_path)
  }
  
  # Most common RTRW decision per (id, id_pu)
  decision_rtrw <- recon_table %>%
    filter(!is.na(user_decision_rtrw), user_decision_rtrw != "") %>%
    group_by(id, id_pu) %>%
    summarise(
      user_decision_rtrw = names(sort(table(user_decision_rtrw), decreasing = TRUE))[1],
      .groups = "drop"
    )
  
  # Most common RZWP3K decision per (id, id_pu)
  decision_rzwp3k <- recon_table %>%
    filter(!is.na(user_decision_rzwp3k), user_decision_rzwp3k != "") %>%
    group_by(id, id_pu) %>%
    summarise(
      user_decision_rzwp3k = names(sort(table(user_decision_rzwp3k), decreasing = TRUE))[1],
      .groups = "drop"
    )
  
  # Merge decisions into a clean table (one row per pair)
  recon_table_clean <- recon_table %>%
    distinct(id, id_pu, .keep_all = TRUE) %>%
    select(id, id_pu) %>%
    left_join(decision_rtrw, by = c("id", "id_pu")) %>%
    left_join(decision_rzwp3k, by = c("id", "id_pu"))
  
  # Join decisions to the spatial adjacent reconciliation layer
  adjacent_reconcile_map <- adjacent_recom_map %>%
    left_join(recon_table_clean, by = c("id", "id_pu"))
  
  # Reconcile RTRW and RZWP3K using the custom reconciliation function
  rtrw_reconciled <- reconcile_map_step2(
    base_map = rtrw_vect,
    exclusion_mask = rzwp3k_vect,
    update_layer = adjacent_reconcile_map,
    layer_name = "RTRW"
  )
  
  rzwp3k_reconciled <- reconcile_map_step2(
    base_map = rzwp3k_vect,
    exclusion_mask = rtrw_vect,
    update_layer = adjacent_reconcile_map,
    layer_name = "RZWP3K"
  )
  
  # Standardise column names and select relevant columns
  rtrw_clean <- rtrw_reconciled %>%
    rename(Zoning_Old = RTRW_old,
           Zoning_New = user_decision_rtrw) %>%
    mutate(Source = "RTRW") %>%
    select(id_pu, Source, Zoning_Old, Zoning_New, Adjacent, Reconcile,
           idx_serasi, idx_padu_final, idx_padan, geometry)
  
  rzwp3k_clean <- rzwp3k_reconciled %>%
    rename(Zoning_Old = RZWP3K_old,
           Zoning_New = user_decision_rzwp3k) %>%
    mutate(Source = "RZWP3K") %>%
    select(id_pu, Source, Zoning_Old, Zoning_New, Adjacent, Reconcile,
           idx_serasi, idx_padu_final, idx_padan, geometry)
  
  # Merge both layers into one integrated map
  integrated_map <- bind_rows(rtrw_clean, rzwp3k_clean) %>%
    st_make_valid()
  
  # Compute new compatibility indices for adjacent polygons
  compatibility_lookup <- integrated_map %>%
    filter(Adjacent == "Yes") %>%
    group_by(id_pu) %>%
    summarize(z_a = first(Zoning_New), z_b = last(Zoning_New), .groups = "drop") %>%
    st_drop_geometry() %>%
    mutate(idx_serasi_new = map2_dbl(z_a, z_b, get_compat)) %>%
    select(id_pu, idx_serasi_new)
  
  # Add new indices to the integrated map
  integrated_map_idx <- integrated_map %>%
    left_join(compatibility_lookup, by = "id_pu") %>%
    mutate(
      idx_serasi_new = as.numeric(idx_serasi_new),
      idx_padu_final = as.numeric(idx_padu_final),
      idx_padan      = as.numeric(idx_padan),
      idx_padan_new  = if_else(Adjacent == "Yes",
                               (alpha * idx_serasi_new) + ((1 - alpha) * idx_padu_final),
                               NA_real_),
      delta_idx_padan = if_else(Adjacent == "Yes",
                                idx_padan_new - idx_padan,
                                NA_real_)
    )
  
  return(integrated_map_idx)
}

#' Step 1 of reconciliation: process overlaps with priority-based decisions
#'
#' This function reconciles overlapping RTRW and RZWP3K polygons using priority
#' lists, computes compatibility indices for the chosen decisions, and returns
#' the two reconciled layers separately (not merged).
#'
#' @param rtrw_base   sf object of the original RTRW layer (must have columns
#'                    `id_pu`, `RTRW`, `idx_serasi`, `idx_padu_final`, `idx_padan`)
#' @param rzwp3k_base sf object of the original RZWP3K layer (similar columns)
#' @param overlaps_map sf object of overlapping polygons, with columns
#'                     `id_pu`, `RTRW`, `RZWP3K`, `user_decision`
#' @param rtrw_priority    data frame of RTRW priority classes (column `RTRW`)
#' @param rzwp3k_priority  data frame of RZWP3K priority classes (column `RZWP3K`)
#' @param alpha       numeric weight for new compatibility (default 0.5)
#'
#' @return A list with two `sf` objects:
#'   - `rtrw`: reconciled RTRW layer with new indices
#'   - `rzwp3k`: reconciled RZWP3K layer with new indices
#'
#'   Each has columns: `id_pu`, `Zoning_Old`, `Zoning_New`, `Overlap`, `Reconcile`,
#'   `idx_serasi`, `idx_padu_final`, `idx_padan`,
#'   `Overlap_Pair`, `idx_serasi_new`, `idx_padan_new`, `delta_idx_padan`,
#'   and `geometry`.
#'
#' @details Requires `get_compat` to be defined in the calling environment.
#'
#' @examples
#' \dontrun{
#' result <- reconciliation_step1(
#'   rtrw_base   = rtrw_vect,
#'   rzwp3k_base = rzwp3k_vect,
#'   overlaps_map = overlaps_reconcilliation_map,
#'   rtrw_priority    = rtrw_priority_table,
#'   rzwp3k_priority  = rzwp3k_priority_table,
#'   alpha       = 0.5
#' )
#' rtrw_final_idx   <- result$rtrw
#' rzwp3k_final_idx <- result$rzwp3k
#' }
reconciliation_step1 <- function(rtrw_base, rzwp3k_base, overlaps_map,
                                 rtrw_priority, rzwp3k_priority, matriks_serasi, alpha = 0.5) {
  
  if (missing(matriks_serasi) || is.null(matriks_serasi)) {
    stop("'matriks_serasi' must be provided (the loaded compatibility matrix).")
  }
  
  get_compat <- function(x, y) {
    if (is.na(x) || is.na(y)) return(NA_real_)
    val <- matriks_serasi %>%
      dplyr::filter(class1 == x, class2 == y) %>%
      dplyr::pull(idx_serasi)
    if (length(val) == 0) NA_real_ else val[1]
  }
  
  init_cols <- function(df) {
    df %>%
      mutate(
        id_pu = if ("id_pu" %in% names(.)) id_pu else NA_real_,
        idx_serasi = if ("idx_serasi" %in% names(.)) idx_serasi else NA_real_,
        idx_padu_final = if ("idx_padu_final" %in% names(.)) idx_padu_final else NA_real_,
        idx_padan = if ("idx_padan" %in% names(.)) idx_padan else NA_real_
      )
  }
  
  normalize_geometry <- function(x) {
    geom_col <- attr(x, "sf_column")
    
    if (!is.null(geom_col) && geom_col != "geometry") {
      names(x)[names(x) == geom_col] <- "geometry"
      st_geometry(x) <- "geometry"
    }
    
    x
  }
  
  # Clean geometries
  rtrw_base <- st_zm(rtrw_base, drop = TRUE) %>%
    init_cols() %>%
    normalize_geometry()
  
  rzwp3k_base <- st_zm(rzwp3k_base, drop = TRUE) %>%
    init_cols() %>%
    normalize_geometry()
  
  overlaps_map <- st_zm(overlaps_map, drop = TRUE) %>%
    normalize_geometry()
  
  # Standardize geometry column name
  geom_col <- attr(overlaps_map, "sf_column")
  if (geom_col != "geometry") {
    names(overlaps_map)[names(overlaps_map) == geom_col] <- "geometry"
    st_geometry(overlaps_map) <- "geometry"
  }
  
  # Determine winners
  overlaps_map <- overlaps_map %>%
    mutate(
      winner = case_when(
        user_decision %in% rtrw_priority$RTRW ~ "RTRW",
        user_decision %in% rzwp3k_priority$RZWP3K ~ "RZWP3K",
        TRUE ~ "None"
      )
    )
  
  # Remove overlap from base maps
  overlap_union <- st_union(st_make_valid(overlaps_map))
  
  rtrw_cutout <- st_difference(rtrw_base, overlap_union) %>%
    mutate(
      Zoning_Old = RTRW,
      Zoning_New = RTRW,
      Overlap = "No",
      Reconcile = "No"
    ) %>%
    select(
      id_pu, Zoning_Old, Zoning_New,
      Overlap, Reconcile,
      idx_serasi, idx_padu_final, idx_padan
    )
  
  rzwp3k_cutout <- st_difference(rzwp3k_base, overlap_union) %>%
    mutate(
      Zoning_Old = RZWP3K,
      Zoning_New = RZWP3K,
      Overlap = "No",
      Reconcile = "No"
    ) %>%
    select(
      id_pu, Zoning_Old, Zoning_New,
      Overlap, Reconcile,
      idx_serasi, idx_padu_final, idx_padan
    )
  
  # Winning polygons
  rtrw_wins <- overlaps_map %>%
    filter(winner == "RTRW") %>%
    mutate(
      Zoning_Old = RTRW,
      Zoning_New = user_decision,
      Overlap = "Yes",
      Reconcile = if_else(Zoning_Old == Zoning_New, "No", "Yes")
    ) %>%
    select(
      id_pu, Zoning_Old, Zoning_New,
      Overlap, Reconcile,
      idx_serasi, idx_padu_final, idx_padan
    )
  
  rzwp3k_wins <- overlaps_map %>%
    filter(winner == "RZWP3K") %>%
    mutate(
      Zoning_Old = RZWP3K,
      Zoning_New = user_decision,
      Overlap = "Yes",
      Reconcile = if_else(Zoning_Old == Zoning_New, "No", "Yes")
    ) %>%
    select(
      id_pu, Zoning_Old, Zoning_New,
      Overlap, Reconcile,
      idx_serasi, idx_padu_final, idx_padan
    )
  
  rtrw_final <- rbind(rtrw_cutout, rtrw_wins) %>%
    st_make_valid()
  
  rzwp3k_final <- rbind(rzwp3k_cutout, rzwp3k_wins) %>%
    st_make_valid()
  
  lookup <- overlaps_map %>%
    st_drop_geometry() %>%
    select(id_pu, RTRW, RZWP3K) %>%
    distinct(id_pu, .keep_all = TRUE)
  
  # RTRW output
  rtrw_idx <- rtrw_final %>%
    left_join(
      lookup %>% select(id_pu, RZWP3K),
      by = "id_pu"
    ) %>%
    mutate(
      Overlap_Pair = if_else(
        Overlap == "Yes",
        RZWP3K,
        NA_character_
      ),
      idx_serasi_new = if_else(
        Overlap == "Yes",
        map2_dbl(Zoning_New, RZWP3K, get_compat),
        NA_real_
      ),
      idx_padan_new = if_else(
        Overlap == "Yes",
        alpha * idx_serasi_new + (1 - alpha) * idx_padu_final,
        NA_real_
      ),
      delta_idx_padan = if_else(
        Overlap == "Yes",
        idx_padan_new - idx_padan,
        NA_real_
      )
    ) %>%
    select(-RZWP3K)
  
  # RZWP3K output
  rzwp3k_idx <- rzwp3k_final %>%
    left_join(
      lookup %>% select(id_pu, RTRW),
      by = "id_pu"
    ) %>%
    mutate(
      Overlap_Pair = if_else(
        Overlap == "Yes",
        RTRW,
        NA_character_
      ),
      idx_serasi_new = if_else(
        Overlap == "Yes",
        map2_dbl(RTRW, Zoning_New, get_compat),
        NA_real_
      ),
      idx_padan_new = if_else(
        Overlap == "Yes",
        alpha * idx_serasi_new + (1 - alpha) * idx_padu_final,
        NA_real_
      ),
      delta_idx_padan = if_else(
        Overlap == "Yes",
        idx_padan_new - idx_padan,
        NA_real_
      )
    ) %>%
    select(-RTRW)
  
  common_cols <- c(
    "id_pu", "Zoning_Old", "Zoning_New", "Overlap_Pair",
    "Overlap", "Reconcile",
    "idx_serasi", "idx_padu_final", "idx_padan",
    "idx_serasi_new", "idx_padan_new",
    "delta_idx_padan", "geometry"
  )
  
  rtrw_clean <- rtrw_idx %>%
    select(all_of(common_cols))
  
  rzwp3k_clean <- rzwp3k_idx %>%
    select(all_of(common_cols))
  
  list(
    rtrw = rtrw_clean,
    rzwp3k = rzwp3k_clean
  )
}
