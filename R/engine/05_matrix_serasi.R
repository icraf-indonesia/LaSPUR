# R/engine/05_matrix_serasi.R
# ============================================================
# Compatibility Matrix Generation & Area Function Allocation
# ============================================================

#' Build a DrawingML shape with multi-line text for openxlsx2 Excel export
#'
#' @param lines Character vector of text lines.
#' @param shape Character shape type (default "rect").
#' @param name Shape identifier name.
#' @param fill_color Hex fill color without "#" (default "F5F5DC").
#' @param text_color Hex font color without "#" (default "000000").
#' @param line_color Hex border line color without "#" (default "000000").
#' @param text_align Text alignment ("left", "center", "right").
#' @param from_col 0-based start column.
#' @param from_row 0-based start row.
#' @param to_col 0-based end column.
#' @param to_row 0-based end row.
#' @param ... Additional arguments.
#' @return XML object ready for openxlsx2 wb$add_drawing().
#' @export
create_multiline_shape <- function(lines,
                                   shape = "rect",
                                   name = "instructions_box",
                                   fill_color = "F5F5DC",
                                   text_color = "000000",
                                   line_color = "000000",
                                   text_align = "left",
                                   from_col = 1, from_row = 1,
                                   to_col   = 8, to_row   = 12,
                                   ...) {
  clean_col <- function(col) {
    col_str <- gsub("^#", "", as.character(col))
    if (length(col_str) > 1) col_str <- col_str[1]
    if (nchar(col_str) == 8 && toupper(substr(col_str, 1, 2)) == "FF") {
      col_str <- substr(col_str, 3, 8)
    }
    col_str
  }
  
  fill_hex <- clean_col(fill_color)
  text_hex <- clean_col(text_color)
  line_hex <- clean_col(line_color)
  
  align_val <- switch(tolower(text_align),
                      "left"    = "l",
                      "right"   = "r",
                      "center"  = "ctr",
                      "justify" = "just",
                      "l")
  
  escape_xml <- function(txt) {
    txt <- gsub("&", "&amp;", txt, fixed = TRUE)
    txt <- gsub("<", "&lt;",  txt, fixed = TRUE)
    txt <- gsub(">", "&gt;",  txt, fixed = TRUE)
    txt
  }
  
  p_runs <- paste(
    vapply(lines, function(line) {
      if (trimws(line) == "") {
        return(sprintf('<a:p><a:pPr algn="%s"/><a:endParaRPr sz="1100"/></a:p>', align_val))
      }
      sprintf(
        '<a:p><a:pPr algn="%s"/><a:r><a:rPr sz="1100"><a:solidFill><a:srgbClr val="%s"/></a:solidFill></a:rPr><a:t xml:space="preserve">%s</a:t></a:r></a:p>',
        align_val, text_hex, escape_xml(line)
      )
    }, NA_character_),
    collapse = ""
  )
  
  sp_str <- sprintf(
    '<xdr:sp>
      <xdr:nvSpPr>
        <xdr:cNvPr id="1" name="%s"/>
        <xdr:cNvSpPr/>
      </xdr:nvSpPr>
      <xdr:spPr>
        <a:xfrm><a:off x="0" y="0"/><a:ext cx="0" cy="0"/></a:xfrm>
        <a:prstGeom prst="%s"><a:avLst/></a:prstGeom>
        <a:solidFill><a:srgbClr val="%s"/></a:solidFill>
        <a:ln><a:solidFill><a:srgbClr val="%s"/></a:solidFill></a:ln>
      </xdr:spPr>
      <xdr:txBody>
        <a:bodyPr vertOverflow="clip" horzOverflow="clip" rtlCol="0" wrap="square"/>
        <a:lstStyle/>
        %s
      </xdr:txBody>
    </xdr:sp>',
    name, shape, fill_hex, line_hex, p_runs
  )
  
  xml_str <- sprintf(
    '<xdr:wsDr xmlns:xdr="http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
      <xdr:twoCellAnchor editAs="oneCell">
        <xdr:from>
          <xdr:col>%d</xdr:col>
          <xdr:colOff>0</xdr:colOff>
          <xdr:row>%d</xdr:row>
          <xdr:rowOff>0</xdr:rowOff>
        </xdr:from>
        <xdr:to>
          <xdr:col>%d</xdr:col>
          <xdr:colOff>0</xdr:colOff>
          <xdr:row>%d</xdr:row>
          <xdr:rowOff>0</xdr:rowOff>
        </xdr:to>
        %s
        <xdr:clientData/>
      </xdr:twoCellAnchor>
    </xdr:wsDr>',
    from_col, from_row, to_col, to_row, sp_str
  )
  
  openxlsx2:::read_xml(xml_str, pointer = FALSE)
}

#' Generate a Compatibility Matrix (SERASI)
#'
#' @param sf_1 `sf` object; first column used for row classes.
#' @param sf_2 `sf` object; first column used for column headers.
#' @param fill_value Initial value for matrix cells (default NA).
#' @param file_path Optional path to write a styled Excel file. If NULL, returns tibble.
#' @return A tibble (if file_path is NULL), or invisibly returns the workbook object.
#' @export
generate_matrix_serasi <- function(sf_1, sf_2, fill_value = NA, file_path = NULL) {
  rows <- sf_1 |> sf::st_drop_geometry() |> dplyr::distinct(dplyr::across(1)) |> dplyr::pull(1)
  cols <- sf_2 |> sf::st_drop_geometry() |> dplyr::distinct(dplyr::across(1)) |> dplyr::pull(1)
  
  mat <- matrix(fill_value, nrow = length(rows), ncol = length(cols))
  colnames(mat) <- cols
  
  matrix_tibble <- tibble::as_tibble(mat) |>
    tibble::add_column(RTRW_RZWP3K = rows, .before = 1)
  
  if (is.null(file_path)) {
    return(matrix_tibble)
  }
  
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Package 'openxlsx2' is required but not installed.")
  }
  
  wb <- openxlsx2::wb_workbook()$add_worksheet(sheet = "Matriks SERASI")
  wb$add_data(x = matrix_tibble, col_names = TRUE, row_names = FALSE)
  
  nrows <- nrow(matrix_tibble) + 1
  ncols <- ncol(matrix_tibble)
  
  header_dims    <- paste0("A1:", openxlsx2::int2col(ncols), "1")
  first_col_dims <- paste0("A1:A", nrows)
  
  black <- openxlsx2::wb_color(hex = "FF000000")
  white <- openxlsx2::wb_color(hex = "FFFFFFFF")
  
  wb$
    add_fill(dims = header_dims, color = openxlsx2::wb_color(hex = "FF1F4E79"))$
    add_font(dims = header_dims, color = white, size = 11, bold = TRUE)$
    add_border(dims = header_dims,
               top_border = "thin", top_color = black,
               bottom_border = "thin", bottom_color = black,
               left_border = "thin", left_color = black,
               right_border = "thin", right_color = black,
               inner_vgrid = "thin", inner_vcolor = black)$
    add_cell_style(dims = header_dims, horizontal = "center", vertical = "center", wrap_text = TRUE)
  
  wb$
    add_fill(dims = first_col_dims, color = openxlsx2::wb_color(hex = "FF1F4E79"))$
    add_font(dims = first_col_dims, color = white, size = 11, bold = TRUE)$
    add_border(dims = first_col_dims,
               top_border = "thin", top_color = black,
               bottom_border = "thin", bottom_color = black,
               left_border = "thin", left_color = black,
               right_border = "thin", right_color = black,
               inner_hgrid = "thin", inner_hcolor = black)$
    add_cell_style(dims = first_col_dims, horizontal = "center", vertical = "center", wrap_text = TRUE)
  
  if (nrows >= 2 && ncols >= 2) {
    body_dims <- paste0(openxlsx2::int2col(2), "2:", openxlsx2::int2col(ncols), nrows)
    wb$
      add_fill(dims = body_dims, color = openxlsx2::wb_color(hex = "FFF5F5DC"))$
      add_border(dims = body_dims,
                 top_border = "thin", top_color = black,
                 bottom_border = "thin", bottom_color = black,
                 left_border = "thin", left_color = black,
                 right_border = "thin", right_color = black,
                 inner_hgrid = "thin", inner_hcolor = black,
                 inner_vgrid = "thin", inner_vcolor = black)$
      add_cell_style(dims = body_dims, horizontal = "center", vertical = "center", wrap_text = TRUE)
  }
  
  wb$freeze_pane(first_row = TRUE, first_col = TRUE)  
  wb$set_col_widths(cols = 1, width = 30)
  if (ncols >= 2) wb$set_col_widths(cols = 2:ncols, width = 25)
  
  wb$save(file_path, overwrite = TRUE)
  return(invisible(matrix_tibble))
}

#' Merge Compatibility Scores to Spatial Planning Units
#'
#' Joins compatibility values to an `sf` object using columns RTRW and RZWP3K.
#' Handles both single-row (overlap) and paired-row (adjacent) data structures.
#'
#' @param sf_obj `sf` object with at least 4 non-geometry columns.
#' @param lookup_table Data frame with 3 columns: RTRW class, RZWP3K class, and numeric score.
#' @param default_compat Numeric value for missing combinations (default NA_real_).
#' @return Original `sf` object with added `idx_serasi` column.
#' @export
merge_attributes_to_map <- function(sf_obj, lookup_table, default_compat = NA_real_) {
  if (!inherits(sf_obj, "sf")) stop("sf_obj must be an sf object.")
  if (ncol(sf_obj) - 1 < 4) stop("sf_obj needs at least 4 non-geometry columns.")
  if (!is.data.frame(lookup_table)) stop("lookup_table must be a data frame.")
  if (ncol(lookup_table) < 3) stop("lookup_table needs at least 3 columns.")
  if (!is.numeric(lookup_table[[3]])) stop("Third column of lookup_table must be numeric.")
  if (!is.numeric(default_compat)) stop("default_compat must be numeric.")
  
  sf_col_names <- names(sf_obj)
  rtrw_col <- sf_col_names[sf_col_names == "RTRW"]
  rzpw_col <- sf_col_names[sf_col_names == "RZWP3K"]
  
  if (length(rtrw_col) == 0) rtrw_col <- sf_col_names[3]
  if (length(rzpw_col) == 0) rzpw_col <- sf_col_names[4]
  
  if ("id_pu" %in% sf_col_names) {
    sf_non_geo <- sf::st_drop_geometry(sf_obj)
    id_vals <- unique(sf_non_geo$id_pu)
    
    is_paired <- TRUE
    for (pid in id_vals) {
      sub <- sf_non_geo[sf_non_geo$id_pu == pid, ]
      n_rtrw <- sum(!is.na(sub[[rtrw_col]]))
      n_rzpw <- sum(!is.na(sub[[rzpw_col]]))
      if (n_rtrw != 1 || n_rzpw != 1) {
        is_paired <- FALSE
        break
      }
    }
    
    if (is_paired) {
      lookup_keys <- paste(lookup_table[[1]], lookup_table[[2]], sep = "||")
      compat_vec <- lookup_table[[3]]
      names(compat_vec) <- lookup_keys
      
      compat_by_id <- data.frame(id_pu = id_vals, compat = NA_real_)
      for (i in seq_along(id_vals)) {
        pid <- id_vals[i]
        sub <- sf_non_geo[sf_non_geo$id_pu == pid, ]
        rtrw_val <- sub[[rtrw_col]][!is.na(sub[[rtrw_col]])][1]
        rzpw_val <- sub[[rzpw_col]][!is.na(sub[[rzpw_col]])][1]
        key <- paste(rtrw_val, rzpw_val, sep = "||")
        compat <- compat_vec[key]
        if (is.na(compat)) compat <- default_compat
        compat_by_id$compat[i] <- compat
      }
      
      sf_obj$idx_serasi <- compat_by_id$compat[match(sf_obj$id_pu, compat_by_id$id_pu)]
      return(sf_obj)
    }
  }
  
  sf_keys <- sf::st_drop_geometry(sf_obj)[, c(rtrw_col, rzpw_col)]
  sf_keys$key <- paste(sf_keys[[1]], sf_keys[[2]], sep = "||")
  lookup_keys <- paste(lookup_table[[1]], lookup_table[[2]], sep = "||")
  compat_vec <- lookup_table[[3]]
  names(compat_vec) <- lookup_keys
  matched_compat <- compat_vec[sf_keys$key]
  matched_compat[is.na(matched_compat)] <- default_compat
  sf_obj$idx_serasi <- matched_compat
  
  return(sf_obj)
}

#' Assign Statutory Area Function Classification to Spatial Units
#'
#' Classifies spatial planning units with a statutory area function (`fungsi`)
#' ("lindung", "budidaya", "campuran") based on priority designations of intersecting or adjacent zones.
#'
#' @param sf_obj An `sf` object or `data.frame` containing RTRW and RZWP3K zone designations.
#' @param rtrw_prioritas A `data.frame` containing RTRW zone names and numeric `Prioritas` column.
#' @param rzwp3k_prioritas A `data.frame` containing RZWP3K zone names and numeric `Prioritas` column.
#' @return Original `sf_obj` with added character column `fungsi`.
#' @export
assign_area_function <- function(sf_obj, rtrw_prioritas, rzwp3k_prioritas) {
  if (!is.data.frame(sf_obj)) {
    stop("sf_obj must be an sf object or data frame.")
  }
  if (!is.data.frame(rtrw_prioritas) || !is.data.frame(rzwp3k_prioritas)) {
    stop("rtrw_prioritas and rzwp3k_prioritas must be data frames.")
  }

  extract_prio_map <- function(tbl, default_pat, tbl_name) {
    if (ncol(tbl) < 2) {
      stop(sprintf("%s reference table must have at least 2 columns.", tbl_name))
    }
    cols <- names(tbl)
    
    prio_col_idx <- grep("^prioritas$", cols, ignore.case = TRUE)
    if (length(prio_col_idx) == 0) {
      prio_col_idx <- grep("prioritas|priority", cols, ignore.case = TRUE)
    }
    if (length(prio_col_idx) == 0) {
      if (ncol(tbl) >= 3 && is.numeric(tbl[[3]])) {
        prio_col_idx <- 3
      } else {
        stop(sprintf("%s reference table must contain a 'Prioritas' column.", tbl_name))
      }
    } else {
      prio_col_idx <- prio_col_idx[1]
    }
    
    zone_col_idx <- grep(paste0("^", default_pat, "$"), cols, ignore.case = TRUE)
    if (length(zone_col_idx) == 0) {
      candidates <- grep(default_pat, cols, ignore.case = TRUE)
      candidates <- setdiff(candidates, prio_col_idx)
      non_code <- candidates[!grepl("^(kode|id|no[._])", cols[candidates], ignore.case = TRUE)]
      if (length(non_code) > 0) zone_col_idx <- non_code else zone_col_idx <- candidates
    }
    if (length(zone_col_idx) == 0) {
      candidates <- grep("pola|zona|subzona|class|nama|name", cols, ignore.case = TRUE)
      candidates <- setdiff(candidates, prio_col_idx)
      non_code <- candidates[!grepl("^(kode|id|no[._])", cols[candidates], ignore.case = TRUE)]
      if (length(non_code) > 0) zone_col_idx <- non_code else zone_col_idx <- candidates
    }
    if (length(zone_col_idx) == 0) {
      zone_col_idx <- if (ncol(tbl) >= 2 && prio_col_idx != 2) 2 else 1
    } else {
      zone_col_idx <- zone_col_idx[1]
    }
    
    zones <- trimws(as.character(tbl[[zone_col_idx]]))
    raw_prio <- tbl[[prio_col_idx]]
    
    char_prio <- tolower(trimws(as.character(raw_prio)))
    num_prio <- suppressWarnings(as.numeric(char_prio))
    need_mapping <- is.na(num_prio) & !is.na(char_prio) & nzchar(char_prio)
    if (any(need_mapping)) {
      char_map <- ifelse(grepl("^lindung", char_prio), 1,
                  ifelse(grepl("^budidaya", char_prio), 0, NA_real_))
      num_prio[need_mapping] <- char_map[need_mapping]
    }
    
    invalid_prio <- !is.na(num_prio) & !(num_prio %in% c(0, 1))
    if (any(invalid_prio)) {
      warning(sprintf(
        "%s reference table contains priority values outside {0, 1}. They will be treated as NA.",
        tbl_name
      ))
      num_prio[invalid_prio] <- NA_real_
    }
    
    valid <- !is.na(zones) & nzchar(zones)
    zones <- zones[valid]
    num_prio <- num_prio[valid]
    
    dup_zones <- zones[duplicated(zones)]
    if (length(dup_zones) > 0) {
      conflicts <- vapply(unique(dup_zones), function(z) {
        z_prios <- unique(stats::na.omit(num_prio[zones == z]))
        length(z_prios) > 1
      }, logical(1))
      if (any(conflicts)) {
        conflict_names <- names(conflicts)[conflicts]
        warning(sprintf(
          "%s reference table contains duplicate entries with conflicting priorities for: %s. Using first occurrence.",
          tbl_name, paste(conflict_names, collapse = ", ")
        ))
      }
    }
    
    unique_mask <- !duplicated(zones)
    zones <- zones[unique_mask]
    num_prio <- num_prio[unique_mask]
    
    stats::setNames(num_prio, zones)
  }

  rtrw_prio_map <- extract_prio_map(rtrw_prioritas, "rtrw", "RTRW")
  rzwp_prio_map <- extract_prio_map(rzwp3k_prioritas, "rzwp3k", "RZWP3K")

  lookup_prio <- function(vals, prio_map) {
    if (length(vals) == 0) return(numeric(0))
    res <- unname(prio_map[vals])
    na_idx <- which(is.na(res) & !is.na(vals) & nzchar(vals))
    if (length(na_idx) > 0 && length(prio_map) > 0) {
      lower_names <- tolower(names(prio_map))
      unique_lower <- !duplicated(lower_names)
      lower_map <- stats::setNames(unname(prio_map)[unique_lower], lower_names[unique_lower])
      res[na_idx] <- unname(lower_map[tolower(vals[na_idx])])
    }
    res
  }

  classify_pr_pz <- function(pr, pz) {
    res <- rep(NA_character_, length(pr))
    lindung_mask <- !is.na(pr) & !is.na(pz) & pr == 1 & pz == 1
    budidaya_mask <- !is.na(pr) & !is.na(pz) & pr == 0 & pz == 0
    campuran_mask <- !is.na(pr) & !is.na(pz) & (
      (pr == 1 & pz == 0) | (pr == 0 & pz == 1)
    )
    res[lindung_mask] <- "Lindung"
    res[budidaya_mask] <- "Budidaya"
    res[campuran_mask] <- "Campuran"
    res
  }

  if (nrow(sf_obj) == 0) {
    sf_obj$fungsi <- character(0)
    return(sf_obj)
  }

  sf_cols <- names(sf_obj)
  rtrw_idx <- which(toupper(sf_cols) == "RTRW")
  if (length(rtrw_idx) == 0) rtrw_idx <- grep("^(rtrw|pola|pola[._]ruang)$", sf_cols, ignore.case = TRUE)
  if (length(rtrw_idx) == 0) rtrw_idx <- grep("rtrw|pola", sf_cols, ignore.case = TRUE)
  
  rzwp_idx <- which(toupper(sf_cols) %in% c("RZWP3K", "RZWP"))
  if (length(rzwp_idx) == 0) rzwp_idx <- grep("^(rzwp|rzwp3k|zona|subzona)$", sf_cols, ignore.case = TRUE)
  if (length(rzwp_idx) == 0) rzwp_idx <- grep("rzwp|zona", sf_cols, ignore.case = TRUE)

  if (length(rtrw_idx) == 0 || length(rzwp_idx) == 0) {
    geom_col <- if (inherits(sf_obj, "sf")) attr(sf_obj, "sf_column") else NULL
    data_cols <- setdiff(sf_cols, geom_col)
    if (length(data_cols) >= 4) {
      if (length(rtrw_idx) == 0) rtrw_col <- data_cols[3] else rtrw_col <- sf_cols[rtrw_idx[1]]
      if (length(rzwp_idx) == 0) rzwp_col <- data_cols[4] else rzwp_col <- sf_cols[rzwp_idx[1]]
    } else {
      stop("sf_obj must contain 'RTRW' and 'RZWP3K' columns.")
    }
  } else {
    rtrw_col <- sf_cols[rtrw_idx[1]]
    rzwp_col <- sf_cols[rzwp_idx[1]]
  }

  has_id_pu <- "id_pu" %in% sf_cols
  is_paired_adjacent <- FALSE
  if (has_id_pu && any(duplicated(sf_obj$id_pu[!is.na(sf_obj$id_pu)]))) {
    r_has_val <- !is.na(sf_obj[[rtrw_col]]) & nzchar(trimws(as.character(sf_obj[[rtrw_col]])))
    z_has_val <- !is.na(sf_obj[[rzwp_col]]) & nzchar(trimws(as.character(sf_obj[[rzwp_col]])))
    if (any(r_has_val & !z_has_val) && any(!r_has_val & z_has_val)) {
      is_paired_adjacent <- TRUE
    }
  }

  if (is_paired_adjacent) {
    sf_non_geo <- if (inherits(sf_obj, "sf")) sf::st_drop_geometry(sf_obj) else sf_obj
    valid_id_mask <- !is.na(sf_non_geo$id_pu)
    sub_df <- sf_non_geo[valid_id_mask, , drop = FALSE]

    r_rows <- sub_df[!is.na(sub_df[[rtrw_col]]) & nzchar(trimws(as.character(sub_df[[rtrw_col]]))), , drop = FALSE]
    z_rows <- sub_df[!is.na(sub_df[[rzwp_col]]) & nzchar(trimws(as.character(sub_df[[rzwp_col]]))), , drop = FALSE]

    dup_r_ids <- r_rows$id_pu[duplicated(r_rows$id_pu)]
    if (length(dup_r_ids) > 0) {
      conflicts_r <- vapply(unique(dup_r_ids), function(pid) {
        length(unique(r_rows[[rtrw_col]][r_rows$id_pu == pid])) > 1
      }, logical(1))
      if (any(conflicts_r)) {
        warning("assign_area_function: Conflicting RTRW zone classes detected within the same id_pu in adjacent data. Using first occurrence.")
      }
    }

    dup_z_ids <- z_rows$id_pu[duplicated(z_rows$id_pu)]
    if (length(dup_z_ids) > 0) {
      conflicts_z <- vapply(unique(dup_z_ids), function(pid) {
        length(unique(z_rows[[rzwp_col]][z_rows$id_pu == pid])) > 1
      }, logical(1))
      if (any(conflicts_z)) {
        warning("assign_area_function: Conflicting RZWP3K zone classes detected within the same id_pu in adjacent data. Using first occurrence.")
      }
    }

    r_first <- r_rows[!duplicated(r_rows$id_pu), , drop = FALSE]
    z_first <- z_rows[!duplicated(z_rows$id_pu), , drop = FALSE]

    r_map_by_id <- stats::setNames(trimws(as.character(r_first[[rtrw_col]])), as.character(r_first$id_pu))
    z_map_by_id <- stats::setNames(trimws(as.character(z_first[[rzwp_col]])), as.character(z_first$id_pu))

    id_str <- as.character(sf_obj$id_pu)
    pu_rtrw <- unname(r_map_by_id[id_str])
    pu_rzwp <- unname(z_map_by_id[id_str])

    pr <- lookup_prio(pu_rtrw, rtrw_prio_map)
    pz <- lookup_prio(pu_rzwp, rzwp_prio_map)

    all_unique_ids <- unique(sf_obj$id_pu[valid_id_mask])
    all_u_str <- as.character(all_unique_ids)
    u_rtrw <- unname(r_map_by_id[all_u_str])
    u_rzwp <- unname(z_map_by_id[all_u_str])
    u_pr <- lookup_prio(u_rtrw, rtrw_prio_map)
    u_pz <- lookup_prio(u_rzwp, rzwp_prio_map)

    unmatched_r <- sum(!is.na(u_rtrw) & nzchar(u_rtrw) & is.na(u_pr))
    unmatched_z <- sum(!is.na(u_rzwp) & nzchar(u_rzwp) & is.na(u_pz))
    if (unmatched_r > 0 || unmatched_z > 0) {
      warning(sprintf(
        "assign_area_function: %d RTRW and %d RZWP3K zone(s) could not be matched in priority tables. 'fungsi' set to NA for those units.",
        unmatched_r, unmatched_z
      ))
    }

    res_fungsi <- classify_pr_pz(pr, pz)
    res_fungsi[!valid_id_mask] <- NA_character_
    sf_obj$fungsi <- res_fungsi
  } else {
    r_vals <- trimws(as.character(sf_obj[[rtrw_col]]))
    z_vals <- trimws(as.character(sf_obj[[rzwp_col]]))

    pr <- lookup_prio(r_vals, rtrw_prio_map)
    pz <- lookup_prio(z_vals, rzwp_prio_map)

    unmatched_r <- sum(!is.na(r_vals) & nzchar(r_vals) & is.na(pr))
    unmatched_z <- sum(!is.na(z_vals) & nzchar(z_vals) & is.na(pz))
    if (unmatched_r > 0 || unmatched_z > 0) {
      warning(sprintf(
        "assign_area_function: %d RTRW and %d RZWP3K zone(s) could not be matched in priority tables. 'fungsi' set to NA for those units.",
        unmatched_r, unmatched_z
      ))
    }

    sf_obj$fungsi <- classify_pr_pz(pr, pz)
  }

  return(sf_obj)
}
