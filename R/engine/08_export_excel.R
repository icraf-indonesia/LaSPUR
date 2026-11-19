# R/engine/08_export_excel.R
# ============================================================
# Excel Workbook Generation, Formatting & Reconciliation Export
# ============================================================
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
  
  # Columns/rows in DrawingML anchors are 0-based
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
#' @param sf_1 `sf` object; first column used for row classes
#' @param sf_2 `sf` object; first column used for column headers
#' @param fill_value Initial value for matrix cells (default NA)
#' @param file_path Optional path to write a styled Excel file. If NULL, returns the tibble.
#'
#' @return A tibble (if file_path is NULL), or invisibly returns the workbook object.
#' @export

generate_reconciliation_excel <- function(recon_map, 
                                          rtrw_prioritas, 
                                          rzwp3k_prioritas, 
                                          output_dir, 
                                          step, 
                                          file_name = "recon_map.xlsx",
                                          group_col = NULL,
                                          dissolve_adjacent = TRUE,
                                          matriks_serasi = NULL,
                                          narrow_options = FALSE,
                                          n_alt = 5L) {
  
  if (missing(step) || !(step %in% c(1, 2))) {
    stop("'step' must be explicitly provided and must be either 1 or 2.")
  }
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("package 'openxlsx' is required.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("package 'sf' is required.")
  
  if (step == 2 && isTRUE(dissolve_adjacent)) {
    df_flat <- dissolve_adjacent_pairs(recon_map)
  } else {
    df_flat <- sf::st_drop_geometry(recon_map)
  }
  
  if (step == 2) {
    df_flat$user_decision_rtrw   <- NA_character_
    df_flat$user_decision_rzwp3k <- NA_character_
  } else { 
    df_flat$user_decision <- NA_character_
  }
  
  # ---- Narrowed per-row option pools -------------------------------------
  narrow_options <- isTRUE(narrow_options) &&
    !is.null(matriks_serasi) &&
    all(c("RTRW", "RZWP3K") %in% names(df_flat)) &&
    nrow(df_flat) > 0
  
  option_matrix <- NULL
  combined_opts <- character(0)  
  
  if (narrow_options) {
    df_alt <- tryCatch(
      compute_alternative_options(df_flat, matriks_serasi, n_alt = n_alt),
      error = function(e) {
        message("compute_alternative_options failed in template: ", e$message)
        NULL
      }
    )
    if (is.null(df_alt)) narrow_options <- FALSE
  }
  
  if (narrow_options) {
    alt_cols_r <- grep("^alt_RTRW_[0-9]+$",   names(df_alt), value = TRUE)
    alt_cols_z <- grep("^alt_RZWP3K_[0-9]+$", names(df_alt), value = TRUE)
    
    collect <- function(row_df, alt_cols, actual_col) {
      pool <- as.character(row_df[[actual_col]])
      for (col in alt_cols) {
        v <- as.character(row_df[[col]])
        if (!is.na(v) && nzchar(v) && v != "No alternative") pool <- c(pool, v)
      }
      unique(pool[!is.na(pool) & nzchar(pool)])
    }
    
    if (identical(step, 2L)) {
      rtrw_pool <- lapply(seq_len(nrow(df_alt)), function(i) {
        collect(df_alt[i, , drop = FALSE], alt_cols_r, "RTRW")
      })
      rz_pool <- lapply(seq_len(nrow(df_alt)), function(i) {
        collect(df_alt[i, , drop = FALSE], alt_cols_z, "RZWP3K")
      })
      
      if (all(c("id_rtrw", "id_rzwp3k") %in% names(df_alt))) {
        id_r <- as.character(df_alt$id_rtrw)
        id_z <- as.character(df_alt$id_rzwp3k)
        for (f in unique(id_r)) {
          idx <- which(id_r == f)
          if (length(idx) > 1) {
            common <- Reduce(intersect, rtrw_pool[idx])
            for (j in idx) rtrw_pool[[j]] <- common
          }
        }
        for (f in unique(id_z)) {
          idx <- which(id_z == f)
          if (length(idx) > 1) {
            common <- Reduce(intersect, rz_pool[idx])
            for (j in idx) rz_pool[[j]] <- common
          }
        }
      }
      
      max_r <- max(1L, max(vapply(rtrw_pool, length, integer(1))))
      max_z <- max(1L, max(vapply(rz_pool,   length, integer(1))))
      rtrw_mat <- matrix(NA_character_, nrow(df_alt), max_r)
      rz_mat   <- matrix(NA_character_, nrow(df_alt), max_z)
      for (i in seq_len(nrow(df_alt))) {
        v <- rtrw_pool[[i]]; if (length(v) > 0) rtrw_mat[i, seq_along(v)] <- v
        v <- rz_pool[[i]];   if (length(v) > 0) rz_mat[i,   seq_along(v)] <- v
      }
      option_matrix <- cbind(
        as.data.frame(rtrw_mat, stringsAsFactors = FALSE),
        as.data.frame(rz_mat,   stringsAsFactors = FALSE)
      )
      names(option_matrix) <- c(paste0("RTRW_",   seq_len(max_r)),
                                paste0("RZWP3K_", seq_len(max_z)))
    } else {
      pool_list <- lapply(seq_len(nrow(df_alt)), function(i) {
        pool <- c(as.character(df_alt$RTRW[i]), as.character(df_alt$RZWP3K[i]))
        for (col in c(alt_cols_r, alt_cols_z)) {
          v <- as.character(df_alt[[col]][i])
          if (!is.na(v) && nzchar(v) && v != "No alternative") pool <- c(pool, v)
        }
        unique(pool[!is.na(pool) & nzchar(pool)])
      })
      max_len <- max(1L, max(vapply(pool_list, length, integer(1))))
      mat <- matrix(NA_character_, nrow(df_alt), max_len)
      for (i in seq_len(nrow(df_alt))) {
        v <- pool_list[[i]]; if (length(v) > 0) mat[i, seq_along(v)] <- v
      }
      option_matrix <- as.data.frame(mat, stringsAsFactors = FALSE)
      names(option_matrix) <- paste0("Options_", seq_len(max_len))
    }
  }
  
  # ---- Workbook and sheets ----------------------------------------------
  rtrw_opts   <- as.character(rtrw_prioritas$RTRW)
  rzwp3k_opts <- as.character(rzwp3k_prioritas$RZWP3K)
  rtrw_opts   <- rtrw_opts[!is.na(rtrw_opts)]
  rzwp3k_opts <- rzwp3k_opts[!is.na(rzwp3k_opts)]
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Data")
  openxlsx::writeData(wb, "Data", df_flat)
  
  # Alternating group colouring (unchanged)
  use_col <- NULL
  if (!is.null(group_col) && group_col %in% names(df_flat)) {
    use_col <- group_col
  } else if ("id_pu" %in% names(df_flat)) {
    use_col <- "id_pu"
    message("Using column 'id_pu' for alternating row colors.")
  } else {
    message("No suitable grouping column found; skipping row coloring.")
  }
  
  if (!is.null(use_col)) {
    unique_vals <- unique(df_flat[[use_col]])
    if (length(unique_vals) > 0) {
      color1 <- "#DCE6F1"
      color2 <- "#FFFFFF"
      style_group1 <- openxlsx::createStyle(fgFill = color1)
      style_group2 <- openxlsx::createStyle(fgFill = color2)
      for (i in seq_along(unique_vals)) {
        val <- unique_vals[i]
        rows_excel <- which(df_flat[[use_col]] == val) + 1
        style <- if (i %% 2 == 1) style_group1 else style_group2
        openxlsx::addStyle(wb, "Data", style = style,
                           rows = rows_excel,
                           cols = 1:ncol(df_flat),
                           gridExpand = TRUE)
      }
    }
  }
  
  if (narrow_options) {
    openxlsx::addWorksheet(wb, "Validation_Lists")
    openxlsx::writeData(wb, "Validation_Lists", option_matrix)
  } else {
    openxlsx::addWorksheet(wb, "Lists")
    openxlsx::writeData(wb, "Lists", x = "RTRW Options", startCol = 1, startRow = 1)
    if (length(rtrw_opts) > 0)
      openxlsx::writeData(wb, "Lists", x = rtrw_opts, startCol = 1, startRow = 2, colNames = FALSE)
    openxlsx::writeData(wb, "Lists", x = "RZWP3K Options", startCol = 2, startRow = 1)
    if (length(rzwp3k_opts) > 0)
      openxlsx::writeData(wb, "Lists", x = rzwp3k_opts, startCol = 2, startRow = 2, colNames = FALSE)
    if (identical(step, 1L)) {
      combined_opts <- c(rtrw_opts, rzwp3k_opts)
      openxlsx::writeData(wb, "Lists", x = "Combined Options", startCol = 3, startRow = 1)
      if (length(combined_opts) > 0)
        openxlsx::writeData(wb, "Lists", x = combined_opts, startCol = 3, startRow = 2, colNames = FALSE)
    }
  }
  
  # ---- Data validation ---------------------------------------------------
  rows <- 2:(nrow(df_flat) + 1)
  
  if (narrow_options) {
    if (identical(step, 1L)) {
      col_decision <- which(names(df_flat) == "user_decision")
      if (length(col_decision) == 0) stop("Column 'user_decision' not found.")
      ncol_opt <- ncol(option_matrix)
      for (i in seq_len(nrow(df_flat))) {
        openxlsx::dataValidation(
          wb, "Data",
          col = col_decision, rows = i + 1,
          type = "list",
          value = sprintf("'Validation_Lists'!$A$%d:$%s$%d",
                          i + 1, openxlsx::int2col(ncol_opt), i + 1))
      }
    } else {
      col_rtrw   <- which(names(df_flat) == "user_decision_rtrw")
      col_rzwp3k <- which(names(df_flat) == "user_decision_rzwp3k")
      if (length(col_rtrw) == 0 || length(col_rzwp3k) == 0)
        stop("Required decision columns not found.")
      n_r <- sum(grepl("^RTRW_",   names(option_matrix)))
      n_z <- sum(grepl("^RZWP3K_", names(option_matrix)))
      for (i in seq_len(nrow(df_flat))) {
        openxlsx::dataValidation(
          wb, "Data", col = col_rtrw, rows = i + 1, type = "list",
          value = sprintf("'Validation_Lists'!$A$%d:$%s$%d",
                          i + 1, openxlsx::int2col(n_r), i + 1))
        openxlsx::dataValidation(
          wb, "Data", col = col_rzwp3k, rows = i + 1, type = "list",
          value = sprintf("'Validation_Lists'!$%s$%d:$%s$%d",
                          openxlsx::int2col(n_r + 1), i + 1,
                          openxlsx::int2col(n_r + n_z), i + 1))
      }
    }
  } else if (identical(step, 1L)) {
    col_decision <- which(names(df_flat) == "user_decision")
    if (length(col_decision) == 0) stop("Column 'user_decision' not found in data frame.")
    last_row_combined <- length(combined_opts) + 1
    openxlsx::dataValidation(wb, "Data",
                             col = col_decision, rows = rows,
                             type = "list",
                             value = paste0("=Lists!$C$2:$C$", last_row_combined))
  } else {
    col_rtrw   <- which(names(df_flat) == "user_decision_rtrw")
    col_rzwp3k <- which(names(df_flat) == "user_decision_rzwp3k")
    if (length(col_rtrw) == 0 || length(col_rzwp3k) == 0)
      stop("Required decision columns not found in data frame.")
    last_row_rtrw   <- length(rtrw_opts) + 1
    last_row_rzwp3k <- length(rzwp3k_opts) + 1
    openxlsx::dataValidation(wb, "Data",
                             col = col_rtrw, rows = rows,
                             type = "list",
                             value = paste0("=Lists!$A$2:$A$", last_row_rtrw))
    openxlsx::dataValidation(wb, "Data",
                             col = col_rzwp3k, rows = rows,
                             type = "list",
                             value = paste0("=Lists!$B$2:$B$", last_row_rzwp3k))
  }
  
  glossary_data <- data.frame(
    Kelompok = c(
      rep("Kolom Identitas", 4),
      rep("Kolom Zona", 3),
      rep("Kolom Indeks SERASI & PADU", 9),
      rep("Kolom Indeks PADAN", 1),
      rep("Kolom Alternatif Zona", 2),
      rep("Kolom Indeks Alternatif", 4),
      rep("Kolom Rekomendasi & Keputusan Sistem", 3),
      rep("Kolom Keputusan Pengguna", 1)
    ),
    Kolom = c(
      "id_pu", "stat_pu", "id_rtrw", "id_rzwp3k",
      "RTRW", "RZWP3K", "area_ha",
      "idx_serasi", "idx_padu_hs", "idx_padu_ke", "idx_padu_kh",
      "idx_padu_ki", "idx_padu_kl", "idx_padu_se", "idx_padu_rtp", "idx_padu_final",
      "idx_padan",
      "alt_RTRW", "alt_RZWP3K",
      "idx_serasi_rtrw_alt", "idx_serasi_rzwp3k_alt",
      "idx_padan_rtrw_alt", "idx_padan_rzwp3k_alt",
      "recommendation", "decision", "idx_padan_final",
      "user_decision"
    ),
    Deskripsi = c(
      "ID unik unit perencanaan (Planning Unit). Merupakan penomoran tiap area konflik tumpang tindih yang diidentifikasi.",
      "Status geometri unit perencanaan. Nilai tipikal: `intersection` (area tumpang tindih antara RTRW dan RZWP3K).",
      "ID fitur RTRW asal yang membentuk unit perencanaan ini.",
      "ID fitur RZWP3K asal yang membentuk unit perencanaan ini.",
      "Nama kelas/zona kawasan RTRW yang berlaku pada unit perencanaan ini (kondisi eksisting).",
      "Nama kelas/zona RZWP3K yang berlaku pada unit perencanaan ini (kondisi eksisting).",
      "Luas unit perencanaan dalam satuan hektar (ha).",
      "Indeks SERASI — mengukur tingkat kesesuaian/kompatibilitas antara kelas RTRW dan RZWP3K berdasarkan matriks serasi. Rentang 0–1, semakin tinggi semakin serasi.",
      "Indeks PADU-HS (Hidrologi dan Sedimentasi) — menilai kepaduan lingkungan berdasarkan kedekatan terhadap estuari dan tingkat TSS.",
      "Indeks PADU-KE (Keterpaduan Penggunaan Lahan dan Perairan) — menilai kepaduan berdasarkan ketetanggaan kelas tutupan/penggunaan lahan dan perairan.",
      "Indeks PADU-KH (Komposisi Habitat) — menilai kepaduan berdasarkan persentase tutupan habitat pesisir (mangrove, terumbu karang, lamun).",
      "Indeks PADU-KI (Ketahanan Iklim) — menilai kepaduan berdasarkan tingkat risiko bencana pada unit perencanaan.",
      "Indeks PADU-KL (Kawasan Lindung) — menilai kepaduan berdasarkan kondisi kualitas lingkungan perairan.",
      "Indeks PADU-SE (Sosial & Ekonomi) — menilai kepaduan berdasarkan aktivitas manusia yang mengindikasikan adanya nilai ekonomi dan sosial.",
      "Indeks PADU-RTp (Risiko dan Tekanan) — menilai kepaduan berdasarkan jarak ke sumber tekanan (industri dan alur pelayaran).",
      "Indeks PADU gabungan — hasil pembobotan dari seluruh komponen PADU (KE, HS, KL, KH, RTp, SE, KI). Rentang 0–1.",
      "Indeks PADAN eksisting — nilai integrasi tata ruang darat-laut saat ini, dihitung dari kombinasi indeks SERASI dan PADU: `(α × idx_serasi) + ((1−α) × idx_padu_final)`.",
      "Kawasan/Zona RTRW alternatif yang direkomendasikan untuk menggantikan kawasan/zona RTRW eksisting guna meningkatkan integrasi (Indeks SERASI).",
      "Kawasan/Zona RZWP3K alternatif yang direkomendasikan untuk menggantikan kawasan/zona RZWP3K eksisting guna meningkatkan integrasi (Indeks SERASI).",
      "Indeks SERASI yang dihitung jika zona RTRW diganti dengan `alt_RTRW` (berpasangan dengan RZWP3K eksisting).",
      "Indeks SERASI yang dihitung jika zona RZWP3K diganti dengan `alt_RZWP3K` (berpasangan dengan RTRW eksisting).",
      "Indeks PADAN proyeksi jika zona RTRW diganti dengan `alt_RTRW`.",
      "Indeks PADAN proyeksi jika zona RZWP3K diganti dengan `alt_RZWP3K`.",
      "Rekomendasi awal dari sistem berdasarkan logika prioritas zona dan perbandingan indeks PADAN alternatif.",
      "Keputusan akhir sistem — hasil evaluasi apakah penggantian zona benar-benar meningkatkan indeks PADAN.",
      "Indeks PADAN setelah keputusan diterapkan. Jika keputusan adalah `Tetap/Koordinasi`, nilai ini sama dengan `idx_padan` eksisting.",
      "Kolom yang diisi oleh pengguna. Keputusan rekonsiliasi akhir yang dipilih berdasarkan keputusan pengguna untuk setiap unit perencanaan. Nilai yang valid adalah salah satu dari: zona RTRW baru, zona RZWP3K baru, atau `Tetap/Koordinasi`. Kolom ini menjadi input utama untuk proses rekonsiliasi spasial di modul berikutnya."
    ),
    stringsAsFactors = FALSE
  )
  
  openxlsx::addWorksheet(wb, "Glossary")
  openxlsx::writeData(wb, "Glossary", glossary_data, startRow = 1, startCol = 1)
  openxlsx::freezePane(wb, "Glossary", firstRow = TRUE)
  openxlsx::freezePane(wb, "Data", firstActiveRow = 2, firstActiveCol = 5)
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  full_path <- file.path(output_dir, file_name)
  openxlsx::saveWorkbook(wb, full_path, overwrite = TRUE)
  
  message(paste("Workbook successfully saved to:", full_path))
  invisible(NULL)
}

#' Dissolve paired RTRW and RZWP3K polygons by `id_pu`
#'
#' This function takes an `sf` object where each `id_pu` groups exactly two 
#' features: one with a non-`NA` value in the `RTRW` column and one with a 
#' non-`NA` value in the `RZWP3K` column. It dissolves (unions) the geometries 
#' for each `id_pu`, creates a new identifier by concatenating the original 
#' `id` values from the RTRW and RZWP3K rows (separated by an underscore), 
#' sums the `area_ha` fields, concatenates the `admin` values, and retains 
#' the other attributes (assuming they are identical within the pair).
