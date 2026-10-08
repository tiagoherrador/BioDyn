#' Build a presence/absence column at checklist level (checklist required)
#'
#' Supports both TSV (tab-separated) and CSV (comma-separated) files.
#'
#' @param sightings_file Character scalar. Path to the sightings file.
#' @param checklist_file Character scalar. Path to the checklist file (required).
#' @param id_col Character scalar. Shared identifier column.
#' @param pres_col Character scalar. Name of the output presence/absence column.
#' @param label_pos Character scalar. Label for detected presence.
#' @param label_neg Character scalar. Label for absence.
#' @param sep Character scalar. Field separator. Supported values are "\\t" (TSV) and "," (CSV).
#' @param cols_sightings Optional character vector of column names to keep from the sightings file.
#'
#' @return A data.frame: checklist rows with presence/absence.
#'
#' @export
#' @importFrom readr read_tsv read_csv cols col_character
build_data <- function(sightings_file,
                       checklist_file,
                       id_col = "SAMPLING EVENT IDENTIFIER",
                       pres_col = "presence",
                       label_pos = "Present",
                       label_neg = "Absent",
                       sep = "\t",
                       cols_sightings = NULL) {
  ## ---- Soft type checks ----
  to_scalar_char <- function(x, name, allow_null = FALSE) {
    if (is.null(x)) {
      if (allow_null) {
        return(NULL)
      }
      stop("`", name, "` must not be NULL.", call. = FALSE)
    }

    if (!is.character(x) || length(x) != 1L) {
      warning(
        "`", name, "` should be a character scalar. ",
        "Coercing with `as.character(x)[1]`."
      )
      x <- as.character(x)[1]
    }

    trimws(x)
  }

  ## ---- Drop columns with blank names (prevents readr ...NN columns) ----
  drop_blank_name_cols <- function(df) {
    nms <- trimws(names(df))
    keep <- nzchar(nms) # keep only non-empty names
    df <- df[, keep, drop = FALSE]
    names(df) <- nms[keep]
    df
  }

  sightings_file <- to_scalar_char(sightings_file, "sightings_file")
  checklist_file <- to_scalar_char(checklist_file, "checklist_file")
  id_col <- to_scalar_char(id_col, "id_col")
  pres_col <- to_scalar_char(pres_col, "pres_col")
  label_pos <- to_scalar_char(label_pos, "label_pos")
  label_neg <- to_scalar_char(label_neg, "label_neg")
  sep <- to_scalar_char(sep, "sep")

  if (identical(checklist_file, "")) {
    stop("`checklist_file` must not be an empty string.", call. = FALSE)
  }

  if (!is.null(cols_sightings) && !is.character(cols_sightings)) {
    warning("`cols_sightings` should be a character vector. Coercing with `as.character()`.")
    cols_sightings <- as.character(cols_sightings)
  }

  ## ---- Normalize/handle sep (remove unsupported separator warning) ----
  if (!nzchar(sep)) {
    sep <- "\t" # default silently to TSV if empty string
  }

  ## ---- Choose reader based on separator ----
  reader <- switch(
    sep,
    "\t" = readr::read_tsv,
    ","  = readr::read_csv,
    readr::read_tsv # fallback silently (no warning)
  )

  ## ---- Read sightings (all columns as character) ----
  sightings <- reader(
    file = sightings_file,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE,
    name_repair = "minimal" # prevents "New names: `` -> ...NN" messages
  )

  ## ---- Normalize + drop blank-name columns ----
  names(sightings) <- trimws(names(sightings))
  sightings <- drop_blank_name_cols(sightings)

  ## ---- Optional subset of sightings columns ----
  if (!is.null(cols_sightings)) {
    cols_sightings <- trimws(cols_sightings)
    cols_keep <- intersect(cols_sightings, names(sightings))

    if (length(cols_keep) == 0L) {
      warning("None of the names in `cols_sightings` were found in `sightings_file`. No subsetting performed.")
    } else {
      sightings <- sightings[, cols_keep, drop = FALSE]
    }
  }

  ## ---- Check id_col presence in sightings ----
  if (!id_col %in% names(sightings)) {
    stop(
      "Column '", id_col, "' not found in `sightings_file`.\nAvailable columns:\n",
      paste(names(sightings), collapse = ", "),
      call. = FALSE
    )
  }

  ## ---- Read checklist (all columns as character) ----
  checklist <- reader(
    file = checklist_file,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE,
    name_repair = "minimal" # prevents "New names: `` -> ...NN" messages
  )

  ## ---- Normalize + drop blank-name columns ----
  names(checklist) <- trimws(names(checklist))
  checklist <- drop_blank_name_cols(checklist)

  ## ---- Check id_col presence in checklist ----
  if (!id_col %in% names(checklist)) {
    stop(
      "Column '", id_col, "' not found in `checklist_file`.\nAvailable columns:\n",
      paste(names(checklist), collapse = ", "),
      call. = FALSE
    )
  }

  ## ---- Handle pres_col conflict in checklist ----
  if (pres_col %in% names(checklist)) {
    new_name <- paste0(pres_col, "_checklist")
    warning(
      "Column '", pres_col,
      "' already exists in checklist. Renaming to '", new_name, "'."
    )
    names(checklist)[names(checklist) == pres_col] <- new_name
  }

  ## ---- Build presence/absence (original behavior) ----
  sightings[[pres_col]] <- label_pos
  sightings_min <- unique(sightings[, c(id_col, pres_col), drop = FALSE])

  merged <- merge(
    x = checklist,
    y = sightings_min,
    by = id_col,
    all.x = TRUE
  )

  merged[[pres_col]][is.na(merged[[pres_col]])] <- label_neg

  merged
}
