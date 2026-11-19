# R/services/formatters.R
# ============================================================
# Display, Number Formatting, Column Mapping & OS Helpers
# ============================================================

#' Numeric display helper — fixed 2 decimals, padded with trailing zeros
#'
#' @param x Numeric or unit value.
#' @return Formatted character string or NA.
#' @export
fmt2 <- function(x) {
  if (inherits(x, "units")) x <- as.numeric(x)
  if (!is.numeric(x)) return(x)
  ifelse(
    is.na(x),
    NA_character_,
    formatC(x, format = "f", digits = 2)
  )
}

#' Map internal technical column names to display-friendly Indonesian labels
#'
#' @param df Data frame with columns to rename.
#' @return Data frame with mapped column names.
#' @export
laspur_rename_cols <- function(df) {
  mapping <- c(
    "id_pu" = "ID PU",
    "RTRW" = "RTRW",
    "RZWP3K" = "RZWP3K",
    "area_ha" = "Luas (ha)",
    "idx_serasi" = "Indeks SERASI",
    "idx_padu_final" = "Indeks PADU",
    "idx_padan" = "Indeks PADAN",
    "recommendation" = "Rekomendasi",
    "idx_padan_new" = "PADAN Proyeksi",
    "delta_idx_padan" = "Kenaikan (Delta)"
  )
  
  current_names <- names(df)
  new_names <- current_names
  
  for (i in seq_along(current_names)) {
    if (current_names[i] %in% names(mapping)) {
      new_names[i] <- mapping[current_names[i]]
    }
  }
  
  names(df) <- new_names
  df
}

#' Format specific numeric and integer columns in a data frame
#'
#' @param df Data frame to format.
#' @param numeric_cols Column names to format with 2 decimal places.
#' @param integer_cols Column names to format as integers.
#' @return Data frame with formatted columns.
#' @export
laspur_format_numbers <- function(df, numeric_cols = NULL, integer_cols = NULL) {
  if (is.null(df)) return(NULL)
  
  if (!is.null(numeric_cols)) {
    for (col in numeric_cols) {
      if (col %in% names(df)) {
        df[[col]] <- fmt2(as.numeric(df[[col]]))
      }
    }
  }
  
  if (!is.null(integer_cols)) {
    for (col in integer_cols) {
      if (col %in% names(df)) {
        df[[col]] <- as.character(as.integer(df[[col]]))
      }
    }
  }
  df
}

#' Cross-platform folder opener for Windows, macOS, and Linux
#'
#' @param path Character. Directory path to open in file manager.
#' @return Invisible logical. TRUE on success.
#' @export
open_folder_crossplatform <- function(path) {
  if (is.null(path) || length(path) != 1 || !nzchar(path) || !dir.exists(path)) {
    if (requireNamespace("shiny", quietly = TRUE)) {
      shiny::showNotification("Direktori belum tersedia.", type = "warning", duration = 5)
    }
    return(invisible(FALSE))
  }
  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  
  tryCatch({
    os <- .Platform$OS.type
    if (os == "windows") {
      shell.exec(path_norm)
    } else if (os == "unix") {
      sysname <- Sys.info()[["sysname"]]
      if (sysname == "Darwin") {
        system2("open", shQuote(path_norm), wait = FALSE)
      } else {
        system2("xdg-open", shQuote(path_norm), wait = FALSE)
      }
    } else {
      if (requireNamespace("shiny", quietly = TRUE)) {
        shiny::showNotification("Sistem operasi tidak dikenali.", type = "warning")
      }
      return(invisible(FALSE))
    }
    invisible(TRUE)
  }, error = function(e) {
    if (requireNamespace("shiny", quietly = TRUE)) {
      shiny::showNotification(paste("Gagal membuka folder:", conditionMessage(e)),
                               type = "error", duration = 5)
    }
    invisible(FALSE)
  })
}
