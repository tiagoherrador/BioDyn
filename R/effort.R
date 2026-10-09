#' Create a latitude-by-season relative proportion plot (EN)
#'
#' Generates a stacked bar plot showing relative (within-latitude composition)
#' proportions of presence/total observations across seasons and latitude bands.
#' Uses `season_palette` from the package namespace by default.
#'
#' @param data A data.frame containing at least latitude, presence and season columns.
#' @param lat_col Name of the latitude column. Default: `"LATITUDE"`.
#' @param pres_col Name of the presence/absence column. Default: `"presence"`.
#' @param pres_label Value representing presence in `pres_col`. Default: `"Present"`.
#' @param season_col Name of the season column. Default: `"season"`.
#' @param palette Optional named character vector of colours. If `NULL`, uses
#'   `season_palette` from the package. Must contain names: `"Summer"`, `"Autumn"`,
#'   `"Winter"`, `"Spring"`.
#' @param show_global_percent_in_labels Logical; kept for compatibility but ignored
#'   in this variant (default: `TRUE`).
#' @param top_n Integer, kept for compatibility (default: `10`).
#' @param min_total_count Integer, kept for compatibility (default: `5`).
#' @param verbose Logical, kept for compatibility (default: `FALSE`).
#'
#' @return The ggplot object for the relative proportion plot (invisibly).
#' @importFrom stats setNames
#' @export
plot_effort <- function(data,
                        lat_col = "LATITUDE",
                        pres_col = "presence",
                        pres_label = "Present",
                        season_col = "season",
                        palette = NULL,
                        show_global_percent_in_labels = TRUE,
                        top_n = 10,
                        min_total_count = 5,
                        verbose = FALSE) {
  for (pkg in c("ggplot2", "dplyr", "scales", "tidyr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required.", pkg))
    }
  }

  # ---- If sf, drop geometry (work as plain data.frame) ----
  if (inherits(data, "sf")) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' is required to handle sf objects.")
    }
    data <- sf::st_drop_geometry(data)
  }

  # ---- Basic checks ----
  required_cols <- c(lat_col, pres_col, season_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste0("'", missing_cols, "'", collapse = ", "))
  }

  # Subset and rename
  df_local <- data[, c(lat_col, pres_col, season_col), drop = FALSE]
  names(df_local) <- c("lat_raw", "pres_raw", "season_raw")

  # Latitude numeric and filter
  df_local$lat <- suppressWarnings(as.numeric(df_local$lat_raw))
  df_local <- df_local[!is.na(df_local$lat), , drop = FALSE]
  if (nrow(df_local) == 0L) {
    stop("No valid numeric latitude values in the dataset.")
  }

  # Presence logical (trim)
  pres_chr <- trimws(as.character(df_local$pres_raw))
  df_local$is_presence <- pres_chr == pres_label

  # Season: trim and ensure valid factor
  df_local$season <- trimws(as.character(df_local$season_raw))
  df_local$season[is.na(df_local$season) | df_local$season == ""] <- "unknown"
  df_local$season <- factor(df_local$season)

  # lat degree
  df_local$lat_deg <- floor(df_local$lat)

  # ---- Counts by lat_deg per season for total and presence ----
  total_data <- df_local |>
    dplyr::count(lat_deg, season, name = "count_total") |>
    dplyr::mutate(type = "total")

  pres_data <- df_local |>
    dplyr::filter(is_presence) |>
    dplyr::count(lat_deg, season, name = "count_presence") |>
    dplyr::mutate(type = "presence")

  # Full grid
  all_lat <- sort(unique(df_local$lat_deg))
  all_seasons <- levels(df_local$season)
  all_types <- c("total", "presence")
  combos <- expand.grid(
    lat_deg = all_lat,
    season = all_seasons,
    type = all_types,
    stringsAsFactors = FALSE
  )

  total_long <- total_data |>
    dplyr::transmute(
      lat_deg = lat_deg,
      season = as.character(season),
      type = "total",
      count = count_total
    )

  pres_long <- pres_data |>
    dplyr::transmute(
      lat_deg = lat_deg,
      season = as.character(season),
      type = "presence",
      count = count_presence
    )

  combined <- dplyr::bind_rows(total_long, pres_long) |>
    dplyr::mutate(season = as.character(season))

  combined_full <- combos |>
    dplyr::left_join(combined, by = c("lat_deg", "season", "type")) |>
    dplyr::mutate(
      count = dplyr::if_else(is.na(count), 0L, as.integer(count)),
      season = factor(season, levels = all_seasons),
      type = factor(type, levels = all_types)
    )

  # ---- Proportions ----
  combined_full <- combined_full |>
    dplyr::group_by(type) |>
    dplyr::mutate(
      total_global = sum(count),
      prop_absolute = ifelse(total_global > 0, count / total_global, 0)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(type, lat_deg) |>
    dplyr::mutate(
      total_in_lat = sum(count),
      prop_relative = ifelse(total_in_lat > 0, count / total_in_lat, 0)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      label_relative = dplyr::if_else(count > 0, sprintf("%.0f%%", 100 * prop_relative), "")
    )

  if (all(combined_full$count == 0)) {
    stop("All counts are zero after processing. Nothing to plot.")
  }

  # ==========================================================
  # PALETA: en paquete, usar season_palette del namespace
  # ==========================================================
  expected_seasons <- c("Summer", "Autumn", "Winter", "Spring")
  is_named <- function(x) !is.null(names(x)) && !any(names(x) == "")

  pal_to_use <- if (!is.null(palette)) {
    palette
  } else {
    season_palette
  }

  if (!is_named(pal_to_use) || !all(expected_seasons %in% names(pal_to_use))) {
    stop(
      "`palette` (o `season_palette`) debe ser un vector con nombres: ",
      paste(expected_seasons, collapse = ", "),
      "."
    )
  }

  resolved_colors <- pal_to_use[expected_seasons]

  # Asegura el orden y que coincidan los nombres con la paleta
  combined_full$season <- factor(as.character(combined_full$season), levels = expected_seasons)

  fill_scale <- ggplot2::scale_fill_manual(values = resolved_colors, name = "Season")

  # ---- Plot (HORIZONTAL) + facets lado a lado ----
  p_relative <- ggplot2::ggplot(
    combined_full,
    ggplot2::aes(
      y = factor(lat_deg),
      x = prop_relative,
      fill = season
    )
  ) +
    ggplot2::geom_col(
      position = "stack",
      color = "white",
      linewidth = 0.2
    ) +
    ggplot2::facet_wrap(
      ~ type,
      nrow = 1,
      scales = "free_x",
      labeller = ggplot2::labeller(
        type = c(
          total = "Total observations",
          presence = "Presence observations"
        )
      )
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      name = "Relative proportion (composition per latitude)",
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::scale_y_discrete(name = "Latitude (degree)") +
    fill_scale +
    ggplot2::labs(title = "Relative proportions by latitude (each bar sums to 100%)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 7),
      panel.spacing = grid::unit(1, "lines")
    ) +
    ggplot2::geom_text(
      data = combined_full |>
        dplyr::filter(prop_relative > 0),
      ggplot2::aes(label = label_relative),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 2.2,
      color = "black"
    )

  print(p_relative)
  invisible(p_relative)
}
