#' Filter eBird data according to sampling effort and quality criteria
#'
#' This function filters an eBird dataset based on effort constraints,
#' protocol type, number of observers, checklist completeness, and optional
#' duplicate removal.
#'
#' @param data A \code{data.frame} containing at least the following columns:
#'   \itemize{
#'     \item \code{"EFFORT DISTANCE KM"}
#'     \item \code{"NUMBER OBSERVERS"}
#'     \item \code{"DURATION MINUTES"}
#'     \item \code{"PROTOCOL NAME"}
#'     \item \code{"ALL SPECIES REPORTED"}
#'   }
#' @param max_distance_km Numeric scalar. Maximum allowed distance in
#'   \code{"EFFORT DISTANCE KM"}. Default: 5.
#' @param max_observers Numeric/Integer scalar. Maximum allowed number of
#'   observers. Default: 10.
#' @param max_duration_min Numeric/Integer scalar. Maximum allowed duration
#'   (minutes). Default: 300.
#' @param protocols Character vector. Allowed values in
#'   \code{"PROTOCOL NAME"}. Default: \code{c("Stationary", "Traveling")}.
#' @param require_all_species Logical. If TRUE, keep only rows with
#'   \code{"ALL SPECIES REPORTED"} interpreted as TRUE/1. Default: TRUE.
#' @param drop_duplicates Logical. If TRUE, remove duplicates based on
#'   \code{"OBSERVATION DATE"} and \code{"LATITUDE"}. Default: TRUE.
#'
#' @return A filtered \code{data.frame}.
#'
#' @examples
#' \dontrun{
#' # basic usage
#' df_filtered <- filter_ebird(data)
#'
#' # custom protocol list and relaxed duration
#' df_filtered2 <- filter_ebird(
#'   data,
#'   protocols = c("Stationary", "Traveling", "Incidental"),
#'   max_duration_min = 600
#' )
#' }
#'
#' @export
filter_ebird <- function(data,
                         max_distance_km = 5,
                         max_observers = 10,
                         max_duration_min = 300,
                         protocols = c("Stationary", "Traveling"),
                         require_all_species = TRUE,
                         drop_duplicates = TRUE) {

  ## ---- Basic type checks ----
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }

  if (!is.numeric(max_distance_km) || length(max_distance_km) != 1L) {
    warning("`max_distance_km` must be a numeric scalar. Coercing.")
    max_distance_km <- as.numeric(max_distance_km)[1]
  }
  if (!is.numeric(max_observers) || length(max_observers) != 1L) {
    warning("`max_observers` must be numeric. Coercing.")
    max_observers <- as.numeric(max_observers)[1]
  }
  if (!is.numeric(max_duration_min) || length(max_duration_min) != 1L) {
    warning("`max_duration_min` must be numeric. Coercing.")
    max_duration_min <- as.numeric(max_duration_min)[1]
  }
  if (!is.character(protocols)) {
    warning("`protocols` must be a character vector. Coercing.")
    protocols <- as.character(protocols)
  }
  if (!is.logical(require_all_species) || length(require_all_species) != 1L) {
    warning("`require_all_species` must be a logical scalar. Coercing.")
    require_all_species <- as.logical(require_all_species)[1]
  }
  if (!is.logical(drop_duplicates) || length(drop_duplicates) != 1L) {
    warning("`drop_duplicates` must be a logical scalar. Coercing.")
    drop_duplicates <- as.logical(drop_duplicates)[1]
  }

  ## ---- Required columns ----
  required_cols <- c(
    "EFFORT DISTANCE KM",
    "NUMBER OBSERVERS",
    "DURATION MINUTES",
    "PROTOCOL NAME",
    "ALL SPECIES REPORTED"
  )

  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  data_out <- data

  ## ---- Helper: safe numeric coercion (handles factors) ----
  to_num_safe <- function(x, col_name) {
    if (is.factor(x)) x <- as.character(x)
    x_chr <- as.character(x)
    x_num <- suppressWarnings(as.numeric(x_chr))
    if (any(is.na(x_num) & !is.na(x_chr))) {
      warning("Coercion to numeric introduced NAs in column '", col_name, "'.")
    }
    x_num
  }

  ## ---- Helper: interpret 'ALL SPECIES REPORTED' flexibly as 1/0/NA ----
  to_flag <- function(x, col_name) {
    if (is.factor(x)) x <- as.character(x)
    if (is.numeric(x)) return(as.numeric(x))
    x_chr <- trimws(tolower(as.character(x)))
    res <- rep(NA_real_, length(x_chr))
    res[x_chr %in% c("1", "true", "t", "y", "yes")] <- 1
    res[x_chr %in% c("0", "false", "f", "n", "no")] <- 0
    if (any(is.na(res) & !is.na(x_chr))) {
      warning("Coercion to flag (1/0) introduced NAs in column '", col_name, "'.")
    }
    res
  }

  ## ---- Coerce effort columns to numeric (safe) ----
  dist_km <- to_num_safe(data_out[["EFFORT DISTANCE KM"]], "EFFORT DISTANCE KM")
  n_obs   <- to_num_safe(data_out[["NUMBER OBSERVERS"]], "NUMBER OBSERVERS")
  dur_min <- to_num_safe(data_out[["DURATION MINUTES"]], "DURATION MINUTES")
  all_spp <- to_flag(data_out[["ALL SPECIES REPORTED"]], "ALL SPECIES REPORTED")

  ## ---- Apply effort-based filters (vectorized) ----
  keep <- !is.na(dist_km) & dist_km <= max_distance_km
  data_out <- data_out[keep, , drop = FALSE]
  n_obs   <- n_obs[keep]
  dur_min <- dur_min[keep]
  all_spp <- all_spp[keep]

  keep <- !is.na(n_obs) & n_obs <= max_observers
  data_out <- data_out[keep, , drop = FALSE]
  dur_min <- dur_min[keep]
  all_spp <- all_spp[keep]

  keep <- !is.na(dur_min) & dur_min <= max_duration_min
  data_out <- data_out[keep, , drop = FALSE]
  all_spp <- all_spp[keep]

  ## ---- Protocol filter (trim whitespace + case-insensitive match) ----
  prot_raw  <- data_out[["PROTOCOL NAME"]]
  prot_trim <- trimws(as.character(prot_raw))
  prot_l    <- tolower(prot_trim)
  allowed_l <- tolower(trimws(as.character(protocols)))

  keep_proto <- prot_l %in% allowed_l
  data_out <- data_out[keep_proto, , drop = FALSE]
  all_spp <- all_spp[keep_proto]

  ## ---- Completeness filter ----
  if (require_all_species) {
    keep <- !is.na(all_spp) & all_spp == 1
    data_out <- data_out[keep, , drop = FALSE]
  }

  ## ---- Duplicate removal ----
  if (drop_duplicates) {
    key_cols <- c("OBSERVATION DATE", "LATITUDE")

    if (!all(key_cols %in% names(data_out))) {
      warning(
        "Columns for duplicate removal not found in the filtered data. ",
        "Skipping duplicate removal."
      )
    } else {
      data_out <- data_out[!duplicated(data_out[, key_cols, drop = FALSE]), , drop = FALSE]
      rownames(data_out) <- NULL
    }
  }

  data_out
}
