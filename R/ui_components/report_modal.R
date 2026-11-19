# R/ui_components/report_modal.R
# ============================================================
# Report Selection Modal & Generation Orchestrator
# ============================================================

#' Configuration for Report Modules and Templates
#' @export
report_module_config <- list(
  serasi = list(
    label    = "Analisis SERASI",
    template = "report/LaSPUR_SERASI_report_template.Rmd"
  ),
  padu = list(
    padu_ke = list(
      label    = "PADU-KE",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    ),
    padu_hs = list(
      label    = "PADU-HS",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    ),
    padu_kl = list(
      label    = "PADU-KL",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    ),
    padu_kh = list(
      label    = "PADU-KH",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    ),
    padu_rtp = list(
      label    = "PADU-RTp",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    ),
    padu_se = list(
      label    = "PADU-SE",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    ),
    padu_ki = list(
      label    = "PADU-KI",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    ),
    padu_combine = list(
      label    = "PADU-Kombinasi",
      template = "report/LaSPUR_PADU_report_template.Rmd"
    )
  ),
  padan = list(
    label    = "Analisis PADAN",
    template = "report/LaSPUR_PADAN_report_template.Rmd"
  ),
  recommendation = list(
    label    = "Penyusunan Alternatif",
    template = "report/LaSPUR_Alternative_report_template.Rmd"
  ),
  reconcile = list(
    label    = "Rekonsiliasi",
    template = "report/LaSPUR_Reconcilliation_report_template.Rmd"
  )
)

#' Helper to generate checkbox items for the report selection modal
#' @keywords internal
make_cb_item <- function(val_id, label_text, is_ready, indent = FALSE, extra_class = "") {
  badge <- if (is_ready) {
    tags$span(
      class = "badge ms-2",
      style = "background-color:#106665; font-size:0.7rem; vertical-align:middle;",
      "Siap"
    )
  } else {
    tags$span(
      class = "badge ms-2",
      style = "background-color:#94A3B8; font-size:0.7rem; vertical-align:middle;",
      "Belum dijalankan"
    )
  }
  
  tags$div(
    class = "form-check mb-2",
    style = if (indent) "margin-left: 24px;" else NULL,
    tags$input(
      class    = trimws(paste("form-check-input report-mod-cb", extra_class)),
      type     = "checkbox",
      id       = paste0("cb_mod_", gsub("\\$", "_", val_id)),
      value    = val_id,
      checked  = if (is_ready) NA else NULL,
      disabled = if (!is_ready) NA else NULL
    ),
    tags$label(
      class = "form-check-label",
      `for` = paste0("cb_mod_", gsub("\\$", "_", val_id)),
      style = if (!is_ready) "color:#94A3B8;" else "",
      label_text,
      badge
    )
  )
}

#' Show the Report Module Selection Modal
#' @export
show_report_modal <- function(session, output_dir_val) {
  if (is.null(output_dir_val) || !nzchar(output_dir_val) || !validate_output_dir(output_dir_val)) {
    showNotification(
      "Direktori output belum diatur. Harap pilih folder output terlebih dahulu.",
      type = "error", duration = 5
    )
    return()
  }
  
  choices_ui <- list()
  
  for (m_id in names(report_module_config)) {
    cfg <- report_module_config[[m_id]]
    
    is_group <- is.list(cfg) && length(cfg) > 0 &&
      all(sapply(cfg, function(x) is.list(x) && !is.null(x$label) && !is.null(x$template)))
    
    if (is_group) {
      any_ready <- FALSE
      child_choices <- list()
      for (child_id in names(cfg)) {
        child_cfg <- cfg[[child_id]]
        full_id <- paste0(m_id, "$", child_id)
        ready_info <- module_ready_and_data(full_id, output_dir_val, session)
        is_ready <- ready_info$ready
        any_ready <- any_ready || is_ready
        
        child_choices[[length(child_choices) + 1]] <- make_cb_item(
          val_id      = full_id,
          label_text  = child_cfg$label,
          is_ready    = is_ready,
          indent      = TRUE,
          extra_class = paste0("child-of-", m_id)
        )
      }
      
      parent_label <- if (m_id == "padu") {
        "Analisis PADU"
      } else if (m_id == "recommendation") {
        "Analisis Penyusunan Alternatif"
      } else {
        toupper(m_id)
      }
      choices_ui[[length(choices_ui) + 1]] <- tags$div(
        class = "form-check mt-2 mb-1",
        tags$input(
          class = "form-check-input parent-mod-cb",
          type = "checkbox",
          id = paste0("cb_parent_", m_id),
          `data-target-class` = paste0("child-of-", m_id),
          disabled = if (!any_ready) NA else NULL
        ),
        tags$label(
          class = "form-check-label",
          `for` = paste0("cb_parent_", m_id),
          style = if (!any_ready) "color:#94A3B8;" else "color: #334155;",
          parent_label
        )
      )
      choices_ui <- c(choices_ui, child_choices)
      
    } else {
      ready_info <- module_ready_and_data(m_id, output_dir_val, session)
      is_ready <- ready_info$ready
      choices_ui[[length(choices_ui) + 1]] <- make_cb_item(
        val_id     = m_id,
        label_text = cfg$label,
        is_ready   = is_ready,
        indent     = FALSE
      )
    }
  }
  
  sync_js <- tags$script(HTML("
    function updateSelectedModules() {
      var selected = [];
      $('.report-mod-cb:checked').each(function() {
        selected.push($(this).val());
      });
      Shiny.setInputValue('report_modules_selected', selected);
    }
  
    function syncParentStates() {
      $('.parent-mod-cb').each(function() {
        var targetClass = $(this).attr('data-target-class');
        if (!targetClass) return;
        var $children = $('.' + targetClass).not(':disabled');
        if ($children.length === 0) return;
        var allChecked = ($children.filter(':checked').length === $children.length);
        $(this).prop('checked', allChecked);
      });
    }
  
    $(document).off('change', '.parent-mod-cb').on('change', '.parent-mod-cb', function() {
      var isChecked = $(this).is(':checked');
      var targetClass = $(this).attr('data-target-class');
      $('.' + targetClass).not(':disabled').prop('checked', isChecked);
      updateSelectedModules();
    });
  
    $(document).off('change', '.report-mod-cb').on('change', '.report-mod-cb', function() {
      var classes = $(this).attr('class').split(' ');
      var parentClass = null;
      for (var i = 0; i < classes.length; i++) {
        if (classes[i].indexOf('child-of-') === 0) {
          parentClass = classes[i];
          break;
        }
      }
      if (parentClass) {
         var allEnabled = $('.' + parentClass).not(':disabled');
         var allChecked = allEnabled.length > 0 && (allEnabled.length === allEnabled.filter(':checked').length);
         $('.parent-mod-cb[data-target-class=\"' + parentClass + '\"]').prop('checked', allChecked);
      }
      updateSelectedModules();
    });
  
    setTimeout(function() {
      syncParentStates();
      updateSelectedModules();
    }, 100);
  "))
  
  showModal(
    modalDialog(
      title = tagList(icon("file-lines", class = "me-2"), "Buat Laporan — Pilih Modul"),
      tags$p(style = "color:#64748B; font-size:0.9rem; margin-bottom:16px;",
             "Centang modul yang ingin disertakan dalam laporan. Modul yang belum dijalankan tidak dapat dipilih."),
      tags$div(
        style = "padding: 8px 4px; max-height: 400px; overflow-y: auto;",
        if (length(choices_ui) > 0) choices_ui else
          tags$p(class = "text-muted", "Tidak ada modul yang terdaftar.")
      ),
      sync_js,
      footer = tagList(
        modalButton("Batal"),
        actionButton(
          "btn_report_generate",
          tagList(icon("file-export", class = "me-2"), "Buat Laporan"),
          class = "btn-primary",
          style = "font-weight: 600; border: none; box-shadow: 0 4px 6px -1px rgba(27,117,186,0.2);"
        )
      ),
      easyClose = TRUE,
      size = "m"
    )
  )
}

#' Handle Report Generation Process
#' @export
handle_report_generation <- function(selected, output_dir_val, session, report_path_rv) {
  if (length(selected) == 0) {
    showNotification("Pilih setidaknya satu modul.", type = "warning")
    return()
  }
  removeModal()
  
  master_params <- list()
  any_ready     <- FALSE
  padu_combined <- list(inputs = list(), result = list())
  padu_selected <- FALSE
  
  for (sel in selected) {
    if (sel == "serasi") {
      info <- module_ready_and_data("serasi", output_dir_val, session)
      if (info$ready) {
        master_params$serasi <- info$data
        any_ready <- TRUE
      } else {
        showNotification("Modul SERASI tidak siap. Dilewati.", type = "warning")
      }
    } else if (grepl("^padu\\$", sel)) {
      padu_selected <- TRUE
      sub_key <- gsub("^padu\\$", "", sel)
      info <- module_ready_and_data(sel, output_dir_val, session)
      if (info$ready) {
        if (length(padu_combined$inputs) == 0) {
          padu_combined$inputs <- info$data$inputs
        }
        for (res_name in names(info$data$result)) {
          if (grepl("_map$", res_name)) {
            padu_combined$result[[res_name]] <- info$data$result[[res_name]]
          }
        }
        any_ready <- TRUE
      } else {
        showNotification(paste("Modul PADU", sub_key, "tidak siap. Dilewati."),
                         type = "warning")
      }
    } else if (sel == "padan") {
      info <- module_ready_and_data("padan", output_dir_val, session)
      if (info$ready) {
        master_params$padan <- info$data
        any_ready <- TRUE
      } else {
        showNotification("Modul PADAN tidak siap. Dilewati.", type = "warning")
      }
    } else if (sel == "recommendation") {
      info <- module_ready_and_data("recommendation", output_dir_val, session)
      if (info$ready) {
        case <- tryCatch(info$data$inputs$case, error = function(e) NA_character_)
        if (is.na(case) || !nzchar(case)) {
          if (!is.null(info$data$result$idx_alternative_overlaps_map)) {
            case <- "overlaps"
          } else if (!is.null(info$data$result$idx_alternative_adjacent_map)) {
            case <- "adjacent"
          }
        }
        
        if (is.null(master_params$recommendation)) {
          master_params$recommendation <- list()
        }
        if (identical(case, "overlaps")) {
          master_params$recommendation$overlaps <- info$data
          any_ready <- TRUE
        } else if (identical(case, "adjacent")) {
          master_params$recommendation$adjacent <- info$data
          any_ready <- TRUE
        } else {
          showNotification(
            "Modul Penyusunan Alternatif tidak dapat menentukan kasus (overlaps/adjacent). Dilewati.",
            type = "warning"
          )
        }
      } else {
        showNotification("Modul Penyusunan Alternatif tidak siap. Dilewati.",
                         type = "warning")
      }
    } else if (sel == "reconcile") {
      info <- module_ready_and_data("reconcile", output_dir_val, session)
      if (info$ready) {
        master_params$reconcile <- info$data
        any_ready <- TRUE
      } else {
        showNotification("Modul Rekonsiliasi tidak siap. Dilewati.",
                         type = "warning")
      }
    }
  }
  
  if (padu_selected && length(padu_combined$result) > 0) {
    master_params$padu <- padu_combined
  }
  
  if (!any_ready) {
    showNotification("Tidak ada modul yang siap untuk dibuat laporannya.",
                     type = "error", duration = 7)
    return()
  }
  
  count_render_steps <- function(mp) {
    n <- 0L
    if (!is.null(mp$serasi))  n <- n + 1L
    if (!is.null(mp$padu) && length(mp$padu$result) > 0) n <- n + 1L
    if (!is.null(mp$padan))   n <- n + 1L
    if (!is.null(mp$recommendation)) {
      if (!is.null(mp$recommendation$overlaps)) n <- n + 1L
      if (!is.null(mp$recommendation$adjacent)) n <- n + 1L
    }
    if (!is.null(mp$reconcile)) n <- n + 1L
    max(n, 1L)
  }
  
  n_modules  <- count_render_steps(master_params)
  n_steps    <- n_modules + 1L
  step_value <- 1 / n_steps
  step_done  <- 0L
  
  withProgress(
    message = "Merender laporan terpadu...",
    detail  = sprintf("Memulai (0 dari %d modul)", n_modules),
    value   = 0,
    {
      options(laspur_progress_callback = function(module_name) {
        step_done <<- step_done + 1L
        tryCatch({
          incProgress(
            step_value,
            detail = sprintf("[%d/%d] Selesai merender: %s",
                             step_done, n_modules, module_name)
          )
        }, error = function(e) NULL)
      })
      on.exit(options(laspur_progress_callback = NULL), add = TRUE)
      
      tryCatch({
        ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
        report_filename <- paste0("Laporan Analisis LaSPUR - ", ts, ".html")
        
        generate_report(
          output        = report_filename,
          dir           = output_dir_val,
          module_name   = "Master",
          template_path = NULL,
          master_params = master_params
        )
        
        incProgress(step_value, detail = "Menyimpan berkas laporan...")
        
        report_full <- file.path(output_dir_val, report_filename)
        if (!file.exists(report_full)) {
          htmls <- list.files(output_dir_val, pattern = "\\.html$", full.names = TRUE)
          if (length(htmls) > 0) {
            htmls <- htmls[order(file.info(htmls)$mtime, decreasing = TRUE)]
            report_full <- htmls[1]
          }
        }
        if (file.exists(report_full)) report_path_rv(report_full)
        
        showNotification(
          paste("Laporan terpadu berhasil dibuat di folder:", output_dir_val),
          type = "message", duration = 7
        )
      }, error = function(e) {
        showNotification(
          paste("Gagal membuat laporan terpadu:", conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    }
  )
}
