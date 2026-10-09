#' Split observation date into DAY, MONTH and YEAR
#'
#' Takes either a data.frame or a file path and extracts
#' calendar components from the column `OBSERVATION DATE`.
#'
#' @param x A data.frame or a character string giving a path to a CSV file
#'   with a column `OBSERVATION DATE`.
#' @param preview Logical, if TRUE prints a small preview
#'   of the resulting date columns. Default: TRUE.
#'
#' @return A data.frame with a parsed `OBSERVATION DATE` column (POSIXct, UTC)
#'   and new integer columns: `DAY`, `MONTH`, and `YEAR`. Values that cannot
#'   be parsed are set to \code{NA}.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- split_dates("observations.csv")
#' df2 <- split_dates(df)
#' }
split_dates <- function(x, preview = TRUE) {

  # validate preview
  if (!is.logical(preview) || length(preview) != 1L) {
    warning("`preview` should be a logical scalar; coercing.")
    preview <- as.logical(preview)[1]
  }

  # --------------------------
  # Load data.frame or file
  # --------------------------
  if (is.data.frame(x)) {
    df <- x
  } else if (is.character(x) && length(x) == 1L) {
    if (!file.exists(x)) {
      stop("File not found: ", x)
    }
    df <- utils::read.csv(x, encoding = "latin1", check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    stop("`x` must be a data.frame or a valid file path.")
  }

  # --------------------------
  # Check required column
  # --------------------------
  if (!"OBSERVATION DATE" %in% names(df)) {
    stop("Column 'OBSERVATION DATE' not found.")
  }

  # --------------------------
  # Prepare and normalize date strings
  # --------------------------
  date_chr <- as.character(df[["OBSERVATION DATE"]])
  date_chr <- trimws(date_chr)
  # If empty strings -> NA
  date_chr[date_chr == ""] <- NA_character_
  df[["OBSERVATION DATE"]] <- date_chr

  # --------------------------
  # Convert to POSIXct using a set of common formats
  # --------------------------
  # tryFormats expects format strings (not labels). Include common ISO T variants.
  try_formats <- c(
    "%Y-%m-%dT%H:%M:%OSZ",    # 2021-05-01T12:34:56Z
    "%Y-%m-%dT%H:%M:%OS%z",   # 2021-05-01T12:34:56+0000
    "%Y-%m-%dT%H:%M:%OS",     # 2021-05-01T12:34:56
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d",
    "%d-%m-%Y",
    "%d/%m/%Y",
    "%m/%d/%Y",
    "%Y%m%d"
  )

  parsed <- suppressWarnings(
    as.POSIXct(date_chr, tz = "UTC", tryFormats = try_formats)
  )

  # If any remain NA, try as.Date conversion (returns Date), then coerce to POSIXct
  still_na <- is.na(parsed) & !is.na(date_chr)
  if (any(still_na)) {
    parsed_date <- suppressWarnings(as.Date(date_chr[still_na], tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y")))
    parsed[still_na] <- as.POSIXct(parsed_date, tz = "UTC")
  }

  df[["OBSERVATION DATE"]] <- parsed

  # --------------------------
  # Prepare empty columns
  # --------------------------
  df$DAY   <- NA_integer_
  df$MONTH <- NA_integer_
  df$YEAR  <- NA_integer_

  valid_idx <- which(!is.na(df[["OBSERVATION DATE"]]))

  if (length(valid_idx) > 0L) {
    dates <- df[["OBSERVATION DATE"]][valid_idx]

    df$DAY[valid_idx]   <- as.integer(format(dates, "%d"))
    df$MONTH[valid_idx] <- as.integer(format(dates, "%m"))
    df$YEAR[valid_idx]  <- as.integer(format(dates, "%Y"))
  }

  # --------------------------
  # Optional preview
  # --------------------------
  if (isTRUE(preview)) {
    message("Preview of extracted date columns:")
    cols <- c("OBSERVATION DATE", "DAY", "MONTH", "YEAR")
    existing <- intersect(cols, names(df))
    print(utils::head(df[, existing, drop = FALSE]))
  }

  df
}




#' Add a season label based on month and latitude (row-wise hemisphere)
#'
#' Adds a new column `season` based on `MONTH` and `LATITUDE`, determining
#' hemisphere per row (LATITUDE < 0 => south; LATITUDE > 0 => north).
#'
#' @param df A data.frame containing `MONTH` and `LATITUDE` columns.
#' @param month_col Character. Name of the month column. Default: "MONTH".
#' @param lat_col Character. Name of the latitude column. Default: "LATITUDE".
#' @param equator Character. How to treat LATITUDE == 0. One of:
#'   "na" (default), "north", "south".
#'
#' @return The input data.frame with an added `season` column.
#' @export
add_season <- function(df,
                       month_col = "MONTH",
                       lat_col = "LATITUDE",
                       equator = c("na", "north", "south")) {

  equator <- match.arg(equator)

  # ---- required columns ----
  if (!month_col %in% names(df)) {
    stop("Column '", month_col, "' not found in df.")
  }
  if (!lat_col %in% names(df)) {
    stop("Column '", lat_col, "' not found in df.")
  }

  # ---- coerce month ----
  month_vec <- suppressWarnings(as.integer(df[[month_col]]))
  if (all(is.na(month_vec))) {
    stop("Column '", month_col, "' could not be coerced to integers (values are invalid).")
  }

  # ---- coerce latitude ----
  lat_vec <- suppressWarnings(as.numeric(df[[lat_col]]))
  if (all(is.na(lat_vec))) {
    stop("Column '", lat_col, "' could not be coerced to numeric (values are invalid).")
  }

  # ---- initialize output ----
  df$season <- NA_character_

  # valid months 1..12
  ok_month <- !is.na(month_vec) & month_vec >= 1L & month_vec <= 12L
  if (!any(ok_month)) return(df)

  # hemisphere masks (row-wise)
  is_south <- ok_month & !is.na(lat_vec) & lat_vec < 0
  is_north <- ok_month & !is.na(lat_vec) & lat_vec > 0
  is_eq    <- ok_month & !is.na(lat_vec) & lat_vec == 0

  # Optional equator handling
  if (equator == "north") is_north <- is_north | is_eq
  if (equator == "south") is_south <- is_south | is_eq
  # if equator == "na", leave as NA

  # ---- assign seasons (meteorological seasons) ----
  # SOUTH: DJF Summer, MAM Autumn, JJA Winter, SON Spring
  if (any(is_south)) {
    m <- month_vec[is_south]
    df$season[is_south] <- dplyr::case_when(
      m %in% c(12L, 1L, 2L) ~ "Summer",
      m %in% c(3L,  4L, 5L) ~ "Autumn",
      m %in% c(6L,  7L, 8L) ~ "Winter",
      m %in% c(9L, 10L,11L) ~ "Spring",
      TRUE ~ NA_character_
    )
  }

  # NORTH: DJF Winter, MAM Spring, JJA Summer, SON Autumn
  if (any(is_north)) {
    m <- month_vec[is_north]
    df$season[is_north] <- dplyr::case_when(
      m %in% c(12L, 1L, 2L) ~ "Winter",
      m %in% c(3L,  4L, 5L) ~ "Spring",
      m %in% c(6L,  7L, 8L) ~ "Summer",
      m %in% c(9L, 10L,11L) ~ "Autumn",
      TRUE ~ NA_character_
    )
  }

  df
}




#' Filter rows by season
#'
#' Keeps only rows whose `season` value is in the provided list.
#'
#' @param df A data.frame with a `season` column.
#' @param keep Character vector of seasons to retain.
#'   Default: c("Summer", "Winter").
#' @param modify Logical. If TRUE, modifies the input df and returns it.
#'   If FALSE (default), returns a filtered copy.
#'
#' @return A filtered data.frame.
#' @export
filter_season <- function(df,
                          keep = c("Summer", "Winter"),
                          modify = FALSE) {
  if (!"season" %in% names(df)) {
    stop("Column 'season' not found. Did you run add_season() ?")
  }

  if (!is.character(keep)) {
    warning("`keep` should be a character vector; coercing.")
    keep <- as.character(keep)
  }

  if (!is.logical(modify) || length(modify) != 1L) {
    warning("`modify` should be a logical scalar; coercing.")
    modify <- as.logical(modify)[1]
  }

  sel <- df$season %in% keep
  sel[is.na(sel)] <- FALSE
  out <- df[sel, , drop = FALSE]
  rownames(out) <- NULL

  if (modify) {
    # modify in place semantics are the same as returning filtered copy in R
    return(out)
  }
  out
}
