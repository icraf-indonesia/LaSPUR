# R/shared_inputs.R
# ============================================================
#  Shared input helpers (auto-discovery + bar + SERASI picker)
# ============================================================
#
#  Contents:
#    • `%||%`                     – null-coalescing operator (defensive)
#    • render_loaded_file_bar()   – blue "loaded" bar under a fileInput
#    • serasi_input()             – auto-resolving SERASI file picker
#                                   with cross-module shared selection
#
#  All UI/server code that consumes these lives in the individual modules.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# ─────────────────────────────────────────────────────────────
#  render_loaded_file_bar()
# ─────────────────────────────────────────────────────────────
#' Render a "loaded from session" bar under a fileInput
#'
#' @param state Character; one of `"session"`, `"file"`, `"shared"`,
#'   `"manual"`, or `NULL`.
#'   - `"session"` / `"file"` / `"shared"`: draw the loaded-bar and inject
#'     `filename` into the readonly text field.
#'   - `"manual"`: return `NULL` — the caller manages the fileInput's state.
#'   - `NULL` (or any other value): reset the fileInput back to its native
#'     "No file selected" placeholder, but only if we had injected a name.
#'
#' @param input_id Character; the namespaced id of the fileInput.
#' @param filename Character; the filename to inject.
#'
#' @return A Shiny `tagList` (or `NULL`).
#' @keywords internal
render_loaded_file_bar <- function(state, input_id, filename = NULL) {
  if (is.null(input_id) || !nzchar(input_id)) return(NULL)
  
  if (identical(state, "manual")) return(NULL)
  
  is_active <- isTRUE(length(state) == 1 && !is.na(state) &&
                        state %in% c("session", "file", "shared"))
  
  # ── Reset the readonly text if we had injected a name ──
  if (!is_active) {
    return(tags$script(HTML(sprintf(
      "setTimeout(function(){
         var input = document.getElementById('%s');
         if (!input) return;
         var group = input.closest('.input-group');
         if (!group) return;
         var txt = group.querySelector('.form-control');
         if (!txt) return;
         if (txt.dataset.laspurAutofilled === '1') {
           txt.value = '';
           txt.placeholder = 'No file selected';
           txt.dataset.laspurAutofilled = '0';
         }
       }, 40);",
      input_id
    ))))
  }
  
  msg <- switch(state,
                "session" = "File tersedia dari sesi saat ini",
                "file"    = "File tersedia dari sesi sebelumnya",
                "shared"  = "File tersedia dari sesi sebelumnya"
  )
  
  if (is.null(filename) || !nzchar(filename)) filename <- ""
  
  tagList(
    tags$div(
      class = "laspur-loaded-bar",
      style = paste("height: 20px;", "border-radius: 4px;",
                    "overflow: hidden;", "background-color: #eef2f6;",
                    "width: 100%;", "box-sizing: border-box;"),
      tags$div(
        class = "laspur-loaded-bar-fill",
        style = paste(
          "width: 100%;", "height: 20px;",
          "background: linear-gradient(90deg, #1b75ba 0%, #3b92d1 100%);",
          "color: #ffffff;", "font-size: 0.72rem;", "font-weight: 600;",
          "letter-spacing: 0.2px;", "line-height: 20px;",
          "text-align: center;", "white-space: nowrap;",
          "overflow: hidden;", "text-overflow: ellipsis;",
          "padding: 0 8px;", "box-sizing: border-box;"),
        msg
      )
    ),
    tags$script(HTML(sprintf(
      "[30, 90, 200, 400, 700].forEach(function(d){
         setTimeout(function(){
           var input = document.getElementById('%s');
           if (!input) return;
           var group = input.closest('.input-group');
           if (!group) return;
           var txt = group.querySelector('.form-control');
           if (!txt) return;
           txt.value = '%s';
           txt.placeholder = '%s';
           txt.dataset.laspurAutofilled = '1';
         }, d);
       });",
      input_id, filename, filename
    )))
  )
}

# ─────────────────────────────────────────────────────────────
#  serasi_input()
# ─────────────────────────────────────────────────────────────
#' Shared SERASI-file input with cross-module shared selection
#'
#' @description
#' A reusable Shiny input widget that resolves a SERASI map from (in
#' priority order):
#'   1. a manual upload in this module (also propagated to the shared
#'      selection),
#'   2. the current session's `module_results$serasi$result$idx_serasi_map`
#'      (matched to the active path),
#'   3. the shared selection in `session$userData$selected_serasi`
#'      (set by whichever PADU module first resolved a file).
#'
#' No disk scanning, no dropdown — the fileInput is always visible so the
#' user can still upload a different file, which updates the shared
#' selection for subsequent modules.
#'
#' @param input,output,session Standard Shiny server objects.
#' @param output_dir Reactive returning the current output directory.
#'   (Kept for API compatibility; not used for discovery.)
#' @param input_id Character; the fileInput's id (unqualified).
#' @param label Character; the fileInput's label text.
#' @param accept_extra Character vector of extra file extensions accepted
#'   alongside `.gpkg`.
#' @param multiple Logical; pass to `fileInput`.
#'
#' @return A list with:
#'   \item{idx_serasi_map}{`reactive()` → resolved `sf` or `NULL`.}
#'   \item{hash}{`reactive()` → xxhash64 of the sorted `id_pu` set.}
#'   \item{filename}{`reactive()` → the resolved file's basename.}
#'   \item{source}{`reactive()` → `"session" | "manual" | "shared" | NULL`.}
#'   \item{active_path}{`reactive()` → current active path.}
#'   \item{ui_block}{`function()` → `tagList` with fileInput + bar.}
#' @export
serasi_input <- function(input, output, session, output_dir,
                         input_id     = "idx_serasi_file",
                         label        = "Peta indeks SERASI (.gpkg)",
                         accept_extra = c(".shp", ".dbf", ".prj", ".shx", ".cpg"),
                         multiple     = TRUE) {
  ns <- session$ns
  
  # ── Internal state ──────────────────────────────────────
  internal      <- reactiveValues(source = NULL)
  manual_map_rv <- reactiveVal(NULL)
  
  # ── Active path ─────────────────────────────────────────
  active_path_val <- reactive({
    ap <- tryCatch(session$userData$active_path, error = function(e) NULL)
    if (is.null(ap)) return("")
    if (is.function(ap)) return(tryCatch(ap(), error = function(e) ""))
    as.character(ap)
  })
  
  # ── Shared selection accessors ──────────────────────────
  get_shared <- function() {
    rv <- tryCatch(session$userData$selected_serasi, error = function(e) NULL)
    if (is.null(rv)) return(NULL)
    tryCatch(rv(), error = function(e) NULL)
  }
  set_shared <- function(val) {
    rv <- tryCatch(session$userData$selected_serasi, error = function(e) NULL)
    if (is.null(rv)) return(invisible(NULL))
    tryCatch(rv(val), error = function(e) NULL)
    invisible(NULL)
  }
  
  # ── Current-session SERASI result (case-matched) ────────
  session_serasi_map <- reactive({
    res <- tryCatch(session$userData$module_results$serasi, error = function(e) NULL)
    if (is.null(res) || is.null(res$result$idx_serasi_map)) return(NULL)
    ap <- active_path_val()
    case_in_session <- res$inputs$case %||% ""
    if (nzchar(ap) && !identical(case_in_session, ap)) return(NULL)
    res$result$idx_serasi_map
  })
  
  # ── Discovery: session → shared (no disk, no dropdown) ──
  observe({
    # Manual override stays until module reopens
    if (identical(internal$source, "manual")) return()
    
    # Session wins
    if (!is.null(session_serasi_map())) {
      internal$source <- "session"
      return()
    }
    
    # Fall back to shared selection
    s <- get_shared()
    if (!is.null(s) && !is.null(s$map) && inherits(s$map, "sf")) {
      internal$source <- "shared"
      return()
    }
    
    internal$source <- NULL
  })
  
  # ── Manual upload ───────────────────────────────────────
  observeEvent(input[[input_id]], {
    fi <- input[[input_id]]
    if (is.null(fi) || nrow(fi) == 0) return()
    
    tryCatch({
      is_gpkg <- any(grepl("\\.gpkg$", fi$name, ignore.case = TRUE))
      if (is_gpkg) {
        path <- fi$datapath[grepl("\\.gpkg$", fi$name, ignore.case = TRUE)][1]
      } else {
        shp_row <- fi[grepl("\\.shp$", fi$name, ignore.case = TRUE), ]
        if (nrow(shp_row) != 1) stop("Komponen .shp tidak ditemukan.")
        stem <- tools::file_path_sans_ext(shp_row$datapath)
        for (i in seq_len(nrow(fi))) {
          ext <- tools::file_ext(fi$name[i])
          file.copy(fi$datapath[i], paste0(stem, ".", ext), overwrite = TRUE)
        }
        path <- paste0(stem, ".shp")
      }
      
      m <- load_and_validate_shapefile(path)
      m <- ensure_geometry_name(m)
      
      # Reject wrong-case manual upload when active_path is known
      ap <- active_path_val()
      if (nzchar(ap)) {
        cols <- names(m)
        detected <- if ("stat_pu" %in% cols) "overlap"
        else if ("length" %in% cols && "id_group" %in% cols) "adjacent"
        else NA_character_
        if (!is.na(detected) && !identical(detected, ap)) {
          manual_map_rv(NULL)
          internal$source <- NULL
          showNotification(
            sprintf(
              "Berkas yang Anda unggah adalah kasus %s, tetapi jalur aktif saat ini adalah %s.",
              if (detected == "overlap") "Tumpang Tindih" else "Bertetangga",
              if (ap == "overlap") "Tumpang Tindih" else "Bertetangga"
            ), type = "error", duration = 10
          )
          return()
        }
      }
      
      manual_map_rv(m)
      internal$source <- "manual"
      set_shared(list(name = fi$name[1], map = m, source = "manual"))
      
      session$sendCustomMessage("set_fileinput_text", list(
        input_id = ns(input_id),
        filename = fi$name[1]
      ))
    }, error = function(e) {
      manual_map_rv(NULL)
      internal$source <- NULL
      showNotification(paste("Gagal memuat berkas SERASI:", e$message),
                       type = "error", duration = 10)
    })
  }, ignoreInit = TRUE)
  
  # ── Resolve the map ─────────────────────────────────────
  idx_serasi_map <- reactive({
    raw <- if (identical(internal$source, "manual")) {
      manual_map_rv()
    } else {
      sess <- session_serasi_map()
      if (!is.null(sess)) {
        sess
      } else {
        s <- get_shared()
        if (!is.null(s) && !is.null(s$map) && inherits(s$map, "sf")) s$map
        else NULL
      }
    }
    if (is.null(raw)) return(NULL)
    normalize_legacy_ids(raw)
  })
  
  # ── Fingerprint (for PADU-Combine consistency check) ────
  hash <- reactive({
    m <- idx_serasi_map()
    if (is.null(m) || !"id_pu" %in% names(m)) return(NA_character_)
    ids <- sort(unique(as.character(m$id_pu)))
    if (length(ids) == 0) return(NA_character_)
    digest::digest(paste(ids, collapse = "|"), algo = "xxhash64")
  })
  
  # ── Filename ────────────────────────────────────────────
  filename <- reactive({
    src <- internal$source
    if (identical(src, "session")) {
      ap <- active_path_val()
      return(switch(ap,
                    "overlap"  = "idx_serasi_overlaps.gpkg",
                    "adjacent" = "idx_serasi_adjacent.gpkg",
                    "idx_serasi.gpkg"
      ))
    }
    if (identical(src, "manual")) {
      fi <- input[[input_id]]
      if (!is.null(fi) && nrow(fi) > 0) return(fi$name[1])
    }
    s <- get_shared()
    if (!is.null(s) && !is.null(s$name)) return(s$name)
    "idx_serasi.gpkg"
  })
  
  # ── Loaded bar ──────────────────────────────────────────
  output$serasi_loaded_bar <- renderUI({
    render_loaded_file_bar(internal$source, ns(input_id), filename())
  })
  
  # ── Public API ──────────────────────────────────────────
  list(
    idx_serasi_map = idx_serasi_map,
    hash           = hash,
    filename       = filename,
    source         = reactive({ internal$source }),
    active_path    = active_path_val,
    ui_block       = function() {
      div(
        style = "margin-bottom: 16px;",
        tagList(
          fileInput(ns(input_id),
                    label    = label,
                    accept   = c(".gpkg", accept_extra),
                    multiple = multiple),
          uiOutput(ns("serasi_loaded_bar"))
        )
      )
    }
  )
}