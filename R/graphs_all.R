#' Plot a numeric variable with density, histogram, boxplot or smoothed line
#'
#' This function creates visualizations for a single numeric variable from a data.frame.
#' It supports global plots or per-segment plots, optional season filtering, and several
#' plot types: \code{"density"}, \code{"histogram"}, \code{"boxplot"}, and a smoothed
#' \code{"line"} (density curve drawn as a line). When \code{segment = TRUE} the function
#' prints one plot per unique value in the \code{Segment} column and returns invisibly;
#' when \code{segment = FALSE} it returns a ggplot object.
#'
#' The function assumes that \code{season_palette} is available (e.g. defined in a
#' palette script) when using seasonal colour scales.
#'
#' @param df A data.frame containing the data. Must contain a column named \code{"season"}.
#'   If \code{segment = TRUE} it must also contain a column named \code{"Segment"}.
#' @param var A single character string with the name of the numeric column to plot.
#' @param segment Logical scalar; if \code{TRUE}, produce plots per unique value of the
#'   \code{Segment} column (printed) instead of returning a single ggplot. Default \code{FALSE}.
#' @param type Character scalar specifying the plot type. One of \code{"density"},
#'   \code{"histogram"}, \code{"boxplot"}, \code{"line"}, or \code{"auto"}. If \code{"auto"},
#'   the function chooses \code{"boxplot"} when \code{segment = TRUE} and \code{"density"}
#'   otherwise. Default \code{"auto"}.
#' @param seasons Either \code{NULL}, a character vector of seasons to keep, or \code{FALSE}.
#'   If \code{NULL} the function filters to a default set of valid seasons
#'   (\code{"Summer"}, \code{"Autumn"}, \code{"Winter"}, \code{"Spring"}).
#'   If \code{FALSE} the function will ignore seasons (no season filtering is performed).
#'   Default \code{NULL}.
#' @param print_means Logical; when \code{TRUE} and plotting per-segment for density,
#'   histogram or line types the function prints the mean of \code{var} for each segment.
#'   Default \code{TRUE}.
#' @param title Optional plot title. If \code{NULL} the function uses a default title
#'   (empty string for global plots, \code{"Segment <value>"} for per-segment printed plots).
#' @param x_title Optional x-axis label. If \code{NULL} a sensible default based on \code{var}
#'   is used.
#' @param y_title Optional y-axis label. If \code{NULL} a sensible default based on \code{type}
#'   is used.
#'
#' @return If \code{segment = FALSE}, a ggplot object is returned. If \code{segment = TRUE},
#'   the function prints one plot per segment and returns invisibly (\code{NULL}).
#'
#' @details
#' The \code{"line"} plot type draws a smoothed frequency curve by using
#' \code{stat_density(..., geom = "line", y = ..count..)}. The smoothing amount can
#' be adjusted by changing the \code{adjust} parameter within the function code.
#'
#' When seasons are used, the function expects the \code{season} column to contain
#' (at least) some of: \code{"Summer"}, \code{"Autumn"}, \code{"Winter"}, \code{"Spring"}.
#'
#' @importFrom ggplot2 ggplot geom_density geom_histogram geom_boxplot stat_summary
#'   stat_density scale_fill_manual scale_colour_manual labs theme_bw coord_cartesian
#' @importFrom dplyr filter mutate
#' @importFrom rlang sym
#' @export
plot_variable <- function(df,
                          var,
                          segment = FALSE,
                          type = "auto",
                          seasons = NULL,
                          print_means = TRUE,
                          title = NULL,
                          x_title = NULL,
                          y_title = NULL) {
  
  for (pkg in c("ggplot2", "dplyr", "rlang")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required.", pkg))
    }
  }
  
  # Validations
  if (!is.character(var) || length(var) != 1) {
    stop("`var` must be a single character string (e.g., 'Temperature').")
  }
  
  if (!var %in% names(df)) {
    stop("Column '", var, "' not found in data frame.")
  }
  
  if (!"season" %in% names(df)) {
    stop("Column 'season' is required.")
  }
  
  if (segment && !"Segment" %in% names(df)) {
    stop("Column 'Segment' is required when segment = TRUE.")
  }
  
  # Handle seasons = FALSE vs filtering
  ignore_season <- isFALSE(seasons)
  valid_seasons <- c("Summer", "Autumn", "Winter", "Spring")
  
  if (!ignore_season) {
    df_season <- df |>
      dplyr::mutate(season = as.character(season))
    
    if (is.null(seasons)) {
      df_filtered <- df_season |>
        dplyr::filter(season %in% valid_seasons)
    } else {
      seasons <- unique(seasons)
      unknown <- setdiff(seasons, valid_seasons)
      if (length(unknown) > 0) {
        warning(
          "Unknown season(s): ",
          paste(unknown, collapse = ", "),
          ". Ignoring them."
        )
        seasons <- intersect(seasons, valid_seasons)
      }
      if (length(seasons) == 0) {
        stop("No valid seasons provided in `seasons`.")
      }
      df_filtered <- df_season |>
        dplyr::filter(season %in% seasons)
    }
    
    if (nrow(df_filtered) == 0) {
      stop("No data remains after season filtering.")
    }
    
    df <- df_filtered |>
      dplyr::mutate(
        season = factor(
          season,
          levels  = valid_seasons,
          ordered = TRUE
        )
      )
    use_season <- TRUE
  } else {
    use_season <- FALSE
  }
  
  # Variable type check
  var_type <- class(df[[var]])[1]
  is_numeric <- var_type %in% c("numeric", "integer", "double")
  
  if (!is_numeric) {
    warning(
      "Variable '", var, "' is not numeric (class: ",
      var_type, "). Only numeric variables supported."
    )
    return(invisible(NULL))
  }
  
  # Define default x label early
  default_x <- switch(
    var,
    LATITUDE      = "Latitude",
    Temperature   = "Mean annual temperature",
    Precipitation = "Total annual precipitation",
    Altitude      = "Altitude",
    var
  )
  
  # Handle 'auto' type
  allowed_types <- c("density", "histogram", "boxplot", "line")
  if (type == "auto") {
    type <- if (segment) "boxplot" else "density"
  } else {
    type <- match.arg(type, choices = allowed_types)
  }
  
  # Default y label
  default_y <- switch(
    type,
    density   = "Observation frequency",
    histogram = "Count",
    line      = "Count",
    boxplot   = default_x
  )
  default_title <- if (!segment) "" else NULL
  
  x_lab <- if (is.null(x_title)) default_x else x_title
  y_lab <- if (is.null(y_title)) default_y else y_title
  
  # Global x-limits for density/histogram/line (only global view)
  if (!segment && type %in% c("density", "histogram", "line")) {
    xlim_global <- range(df[[var]], na.rm = TRUE)
  }
  
  # === CASE: No segmentation → single plot ===
  if (!segment) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = !!rlang::sym(var))) +
      ggplot2::theme_bw()
    
    if (type == "density") {
      
      if (use_season) {
        p <- p +
          ggplot2::geom_density(
            ggplot2::aes(fill = season, colour = season),
            alpha = 0.5
          ) +
          ggplot2::scale_fill_manual(values = season_palette, drop = FALSE) +
          ggplot2::scale_colour_manual(values = season_palette, drop = FALSE)
      } else {
        p <- p +
          ggplot2::geom_density(
            alpha  = 0.5,
            fill   = "steelblue",
            colour = "black"
          )
      }
      
      p <- p +
        ggplot2::coord_cartesian(xlim = xlim_global)
      
    } else if (type == "histogram") {
      
      if (use_season) {
        p <- p +
          ggplot2::geom_histogram(
            ggplot2::aes(fill = season),
            position = "identity",
            alpha    = 0.5,
            bins     = 30
          ) +
          ggplot2::scale_fill_manual(values = season_palette, drop = FALSE)
      } else {
        p <- p +
          ggplot2::geom_histogram(
            alpha  = 0.7,
            bins   = 30,
            fill   = "steelblue",
            colour = "black"
          )
      }
      
    } else if (type == "line") {
      
      if (use_season) {
        p <- p +
          ggplot2::stat_density(
            mapping  = ggplot2::aes(y = ..count.., colour = season),
            geom     = "line",
            position = "identity",
            size     = 1,
            adjust   = 1
          ) +
          ggplot2::scale_colour_manual(values = season_palette, drop = FALSE)
      } else {
        p <- p +
          ggplot2::stat_density(
            mapping  = ggplot2::aes(y = ..count..),
            geom     = "line",
            position = "identity",
            size     = 1,
            colour   = "black",
            adjust   = 1
          )
      }
      
      p <- p +
        ggplot2::coord_cartesian(xlim = xlim_global)
      
    } else if (type == "boxplot") {
      
      if (use_season) {
        p <- p +
          ggplot2::geom_boxplot(
            ggplot2::aes(
              x    = season,
              y    = !!rlang::sym(var),
              fill = season
            ),
            colour = "black"
          ) +
          ggplot2::scale_fill_manual(values = season_palette, drop = FALSE) +
          ggplot2::stat_summary(
            fun    = mean,
            geom   = "point",
            colour = "red",
            shape  = 18,
            size   = 2
          )
        x_lab_plot <- if (is.null(x_title)) "Season" else x_title
      } else {
        p <- ggplot2::ggplot(
          df,
          ggplot2::aes(x = 1, y = !!rlang::sym(var))
        ) +
          ggplot2::geom_boxplot(
            fill   = "steelblue",
            colour = "black"
          ) +
          ggplot2::stat_summary(
            fun    = mean,
            geom   = "point",
            colour = "red",
            shape  = 18,
            size   = 2
          )
        x_lab_plot <- if (is.null(x_title)) "" else x_title
      }
      
      y_lab <- default_x
    }
    
    p <- p +
      ggplot2::labs(
        title = if (is.null(title)) default_title else title,
        x     = if (type == "boxplot") x_lab_plot else x_lab,
        y     = y_lab
      )
    
    return(p)
  }
  
  # === CASE: Segment = TRUE → loop over segments ===
  segments <- sort(unique(df$Segment))
  if (length(segments) == 0) {
    warning("No segments found.")
    return(invisible(NULL))
  }
  
  for (seg in segments) {
    subset <- df |>
      dplyr::filter(Segment == seg)
    
    if (type == "density") {
      
      xlim_seg <- if (exists("xlim_global")) xlim_global else range(subset[[var]], na.rm = TRUE)
      
      p <- ggplot2::ggplot(subset, ggplot2::aes(x = !!rlang::sym(var))) +
        ggplot2::theme_bw()
      
      if (use_season) {
        p <- p +
          ggplot2::geom_density(
            ggplot2::aes(fill = season, colour = season),
            alpha = 0.5
          ) +
          ggplot2::scale_fill_manual(values = season_palette, drop = FALSE) +
          ggplot2::scale_colour_manual(values = season_palette, drop = FALSE)
      } else {
        p <- p +
          ggplot2::geom_density(
            alpha  = 0.5,
            fill   = "steelblue",
            colour = "black"
          )
      }
      
      p <- p +
        ggplot2::labs(
          x = x_lab,
          y = "Observation frequency"
        ) +
        ggplot2::coord_cartesian(xlim = xlim_seg)
      
    } else if (type == "histogram") {
      
      p <- ggplot2::ggplot(subset, ggplot2::aes(x = !!rlang::sym(var))) +
        ggplot2::theme_bw()
      
      if (use_season) {
        p <- p +
          ggplot2::geom_histogram(
            ggplot2::aes(fill = season),
            position = "identity",
            alpha    = 0.5,
            bins     = 30
          ) +
          ggplot2::scale_fill_manual(values = season_palette, drop = FALSE)
      } else {
        p <- p +
          ggplot2::geom_histogram(
            alpha  = 0.7,
            bins   = 30,
            fill   = "steelblue",
            colour = "black"
          )
      }
      
      p <- p +
        ggplot2::labs(
          x = x_lab,
          y = "Count"
        )
      
    } else if (type == "line") {
      
      xlim_seg <- if (exists("xlim_global")) xlim_global else range(subset[[var]], na.rm = TRUE)
      
      p <- ggplot2::ggplot(subset, ggplot2::aes(x = !!rlang::sym(var))) +
        ggplot2::theme_bw()
      
      if (use_season) {
        p <- p +
          ggplot2::stat_density(
            mapping  = ggplot2::aes(y = ..count.., colour = season),
            geom     = "line",
            position = "identity",
            size     = 1,
            adjust   = 1
          ) +
          ggplot2::scale_colour_manual(values = season_palette, drop = FALSE)
      } else {
        p <- p +
          ggplot2::stat_density(
            mapping  = ggplot2::aes(y = ..count..),
            geom     = "line",
            position = "identity",
            size     = 1,
            colour   = "black",
            adjust   = 1
          )
      }
      
      p <- p +
        ggplot2::labs(
          x = x_lab,
          y = "Count"
        ) +
        ggplot2::coord_cartesian(xlim = xlim_seg)
      
    } else if (type == "boxplot") {
      
      if (use_season) {
        p <- ggplot2::ggplot(
          subset,
          ggplot2::aes(
            x = season,
            y = !!rlang::sym(var)
          )
        ) +
          ggplot2::geom_boxplot(
            ggplot2::aes(fill = season),
            colour      = "black",
            show.legend = FALSE
          ) +
          ggplot2::scale_fill_manual(values = season_palette, drop = FALSE)
        x_lab_final <- if (is.null(x_title)) "Season" else x_title
      } else {
        p <- ggplot2::ggplot(
          subset,
          ggplot2::aes(
            x = 1,
            y = !!rlang::sym(var)
          )
        ) +
          ggplot2::geom_boxplot(
            fill   = "steelblue",
            colour = "black"
          )
        x_lab_final <- if (is.null(x_title)) "" else x_title
      }
      
      p <- p +
        ggplot2::stat_summary(
          fun    = mean,
          geom   = "point",
          colour = "red",
          shape  = 18,
          size   = 2
        ) +
        ggplot2::labs(
          x = x_lab_final,
          y = default_x
        ) +
        ggplot2::theme_bw()
    }
    
    # Title logic
    if (is.null(title)) {
      plot_title <- paste("Segment", seg)
    } else if (title == "") {
      plot_title <- ""
    } else {
      plot_title <- title
    }
    
    p <- p +
      ggplot2::labs(title = plot_title)
    
    print(p)
    
    if (print_means && type %in% c("density", "histogram", "line")) {
      mean_val <- mean(subset[[var]], na.rm = TRUE)
      cat(sprintf("Mean %s for Segment %s: %.2f\n", var, seg, mean_val))
    }
  }
  
  invisible(NULL)
}
