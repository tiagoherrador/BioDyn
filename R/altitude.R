#' Compute minimum and maximum elevation per polygon
#'
#' Extracts the minimum and maximum elevation values from a DEM for each polygon.
#'
#' @param dem character or Raster*; path to a DEM file (e.g. GeoTIFF) or a Raster* object.
#' @param polygons character or sf; path to a vector file containing polygons or an \code{sf} object.
#' @param polygon_id_col character or NULL; name of the column in \code{polygons} that identifies each
#'   segment/polygon. If \code{NULL} the function will try the column named \code{"id"}; if not found,
#'   row numbers are used.
#' @param clamp_negative_to_zero logical; if \code{TRUE}, negative elevation minimum values will be
#'   clamped to zero.
#'
#' @return A \code{data.frame} with columns:
#' \describe{
#'   \item{poly_id}{polygon identifier (from \code{polygon_id_col}, or \code{id}, or row number).}
#'   \item{elev_min}{minimum elevation within the polygon (numeric).}
#'   \item{elev_max}{maximum elevation within the polygon (numeric).}
#' }
#'
#' @details
#' Dependencies (\pkg{sf}, \pkg{raster}, \pkg{exactextractr}) are checked lazily inside the function
#' so they are only required when the function is called.
#' If both DEM and polygons have coordinate reference systems, the function will attempt to transform
#' the polygons to the DEM CRS.
#'
#' @examples
#' \dontrun{
#' # Using file paths
#' tbl <- elev_minmax(
#'   dem            = "data/dem.tif",
#'   polygons       = "data/polygons.gpkg",
#'   polygon_id_col = "id"
#' )
#'
#' # Using in-memory objects
#' # r_obj    <- raster::raster("data/dem.tif")
#' # sf_polys <- sf::st_read("data/polygons.gpkg")
#' # tbl2    <- elev_minmax(dem = r_obj, polygons = sf_polys)
#' }
#'
#' @export
elev_minmax <- function(
    dem,
    polygons,
    polygon_id_col = NULL,
    clamp_negative_to_zero = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' required but not installed.")
  }
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' required but not installed.")
  }
  if (!requireNamespace("exactextractr", quietly = TRUE)) {
    stop("Package 'exactextractr' required but not installed.")
  }

  # ---- DEM ----
  if (is.null(dem)) {
    stop("Parameter 'dem' must be provided (file path or Raster* object).")
  }
  if (is.character(dem)) {
    if (!file.exists(dem)) {
      stop("DEM file does not exist: ", dem)
    }
    r <- raster::raster(dem)
  } else if (inherits(dem, "Raster")) {
    r <- dem
  } else {
    stop("Parameter 'dem' must be a file path or a Raster* object.")
  }

  # ---- Polygons ----
  if (is.null(polygons)) {
    stop("Parameter 'polygons' must be provided (file path or sf object).")
  }
  if (is.character(polygons)) {
    if (!file.exists(polygons)) {
      stop("Polygons file does not exist: ", polygons)
    }
    polys <- sf::st_read(polygons, quiet = TRUE)
  } else if (inherits(polygons, "sf")) {
    polys <- polygons
  } else {
    stop("Parameter 'polygons' must be a file path or an sf object.")
  }

  # ---- CRS handling ----
  r_crs <- tryCatch(raster::crs(r), error = function(e) NA)
  if (!is.na(r_crs) && !is.na(sf::st_crs(polys))) {
    tryCatch(
      {
        polys <- sf::st_transform(polys, crs = r_crs)
      },
      error = function(e) {
        warning("Could not transform polygons to DEM CRS: ", conditionMessage(e))
      }
    )
  } else {
    warning("DEM or polygons CRS is NA; skipping CRS transform.")
  }

  # ---- Exact extraction (min/max) ----
  mins <- exactextractr::exact_extract(r, polys, "min")
  maxs <- exactextractr::exact_extract(r, polys, "max")

  # ---- Polygon IDs ----
  if (!is.null(polygon_id_col) && polygon_id_col %in% names(polys)) {
    poly_id <- as.character(polys[[polygon_id_col]])
  } else if ("id" %in% names(polys)) {
    poly_id <- as.character(polys$id)
  } else {
    poly_id <- as.character(seq_len(nrow(polys)))
    message("No polygon id column found; using row numbers as poly_id.")
  }

  out_table <- data.frame(
    poly_id  = poly_id,
    elev_min = unlist(mins),
    elev_max = unlist(maxs),
    stringsAsFactors = FALSE
  )

  if (clamp_negative_to_zero) {
    out_table$elev_min[
      !is.na(out_table$elev_min) & out_table$elev_min < 0
    ] <- 0
  }

  out_table
}



#' Plot altitude boxplots per segment with elevation extents
#'
#' Builds a ggplot2 boxplot of observed altitudes per segment and overlays horizontal
#' segments showing polygon minimum and maximum elevation (from a DEM).
#'
#' @param dem character or Raster*; path to a DEM file or a Raster* object.
#'   Optional if \code{elev_table} is provided.
#' @param polygons character or sf; path to polygons file or an \code{sf} object.
#'   Optional if \code{elev_table} is provided.
#' @param observations data.frame or sf; observations with per-segment records
#'   (must contain \code{segment_col}).
#' @param segment_col character; name of the column in \code{observations} identifying
#'   the segment. Default: \code{"Segment"}.
#' @param polygon_id_col character or NULL; name of the id column in \code{polygons}
#'   used to match segments; if \code{NULL} tries \code{"id"}.
#' @param elev_table data.frame or NULL; optional precomputed table with columns
#'   \code{poly_id}, \code{elev_min}, \code{elev_max} (as returned by \code{elev_minmax}).
#' @param alt_col_candidates character; vector of candidate column names to detect altitude
#'   in \code{observations}; the first match is used.
#' @param season_col character; column in \code{observations} used to group/fill boxes by season.
#'   Default: \code{"season"}.
#' @param clamp_negative_to_zero logical; if \code{TRUE}, negative minima in \code{elev_table}
#'   are clamped to zero.
#' @param seg_half_width numeric; half-width of the overlaid horizontal elevation segments
#'   (in x-axis units).
#' @param color_map named character vector or NULL; manual mapping of season names to colors
#'   (names must match season levels). If \code{NULL} a default palette is chosen.
#' @param palette_file character; optional path to an R script (e.g. palettes.R)
#'   defining a named vector \code{season_palette}. Used only if \code{color_map = NULL}.
#' @param seg_order character or NULL; optional vector specifying the desired order of
#'   segment factor levels.
#' @param plot logical; whether to print the ggplot to the active device.
#'   Default: \code{TRUE}.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{plot}{the \pkg{ggplot2} object (invisible if \code{plot = TRUE} after printing).}
#'   \item{out_table}{the elevation table used/returned (with \code{poly_id}, \code{elev_min},
#'         \code{elev_max}, \code{seg_num}).}
#'   \item{data}{the processed observations data.frame (with column \code{ALTITUDE1} added).}
#' }
#'
#' @details
#' The function tries to detect an altitude column in \code{observations} from
#' \code{alt_col_candidates}, coerces it to numeric (replacing commas with dots),
#' and creates a numeric column named \code{ALTITUDE1}.
#' If \code{elev_table} is not provided, the function will compute it by calling
#' \code{elev_minmax} (requires \pkg{sf}, \pkg{raster}, \pkg{exactextractr}).
#' Colors for seasons can be supplied via \code{color_map}; otherwise a default mapping
#' or a brewer palette is used.
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot scale_x_discrete scale_y_continuous
#'   stat_summary theme_classic theme geom_segment scale_fill_manual
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rlang sym
#' @export
plot_altitude <- function(
    dem = NULL,
    polygons = NULL,
    observations,
    segment_col = "Segment",
    polygon_id_col = NULL,
    elev_table = NULL,
    alt_col_candidates = c("ALTITUD1", "Altitude", "Altitud",
                           "elev", "elevation", "ELEVATION", "ALTITUD"),
    season_col = "season",
    clamp_negative_to_zero = TRUE,
    seg_half_width = 0.35,
    color_map = NULL,
    palette_file = "C:/Users/pc/Downloads/GeoBio/R/palettes.R",
    seg_order = NULL,
    plot = TRUE
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required but not installed.")
  }
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("Package 'RColorBrewer' required but not installed.")
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Package 'rlang' required but not installed.")
  }

  # ---- Observations ----
  if (!is.data.frame(observations) && !inherits(observations, "sf")) {
    stop("Parameter 'observations' must be a data.frame or an sf object.")
  }
  data_segments <- observations

  # ---- Elevation table (DEM + polygons) ----
  if (is.null(elev_table)) {
    for (pkg in c("sf", "raster", "exactextractr")) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(sprintf("Package '%s' required but not installed.", pkg))
      }
    }
    elev_table <- elev_minmax(
      dem                    = dem,
      polygons               = polygons,
      polygon_id_col         = polygon_id_col,
      clamp_negative_to_zero = clamp_negative_to_zero
    )
  } else {
    req_cols <- c("poly_id", "elev_min", "elev_max")
    if (!all(req_cols %in% names(elev_table))) {
      stop(
        "Provided 'elev_table' must contain columns: ",
        paste(req_cols, collapse = ", ")
      )
    }
    if (clamp_negative_to_zero && "elev_min" %in% names(elev_table)) {
      elev_table$elev_min[
        !is.na(elev_table$elev_min) & elev_table$elev_min < 0
      ] <- 0
    }
  }
  out_table <- elev_table

  # ---- Altitude column detection & conversion ----
  present_alts <- intersect(alt_col_candidates, names(data_segments))
  if (length(present_alts) == 0) {
    stop(
      "No altitude column found in observations. Tried: ",
      paste(alt_col_candidates, collapse = ", ")
    )
  }
  alt_col <- present_alts[1]
  message("Using altitude column found: ", alt_col)

  alt_raw  <- data_segments[[alt_col]]
  alt_char <- as.character(alt_raw)
  alt_char <- gsub(",", ".", alt_char)
  alt_num  <- suppressWarnings(as.numeric(alt_char))
  data_segments$ALTITUDE1 <- alt_num

  na_count <- sum(is.na(data_segments$ALTITUDE1))
  if (na_count > 0) {
    message(
      "Warning: ", na_count,
      " NA values in ALTITUDE1 after conversion from ", alt_col, "."
    )
  }

  # ---- Segment factor ----
  if (!(segment_col %in% names(data_segments))) {
    stop("Segment column '", segment_col, "' not found in observations.")
  }

  if (!is.null(seg_order)) {
    seg_levels <- seg_order
  } else {
    seg_levels <- unique(as.character(data_segments[[segment_col]]))
    if (all(!is.na(suppressWarnings(as.numeric(seg_levels))))) {
      seg_levels <- as.character(sort(as.numeric(seg_levels)))
    } else {
      seg_levels <- sort(seg_levels)
    }
  }

  data_segments[[segment_col]] <- factor(
    as.character(data_segments[[segment_col]]),
    levels = seg_levels
  )

  # ---- Match elevation table with segments (seg_num for x-position) ----
  if (all(out_table$poly_id %in% seg_levels)) {
    out_table$poly_id_f <- factor(out_table$poly_id, levels = seg_levels)
    out_table$seg_num   <- as.numeric(out_table$poly_id_f)
  } else if (nrow(out_table) == length(seg_levels)) {
    message(
      "poly_id values do not match Segment values. ",
      "Fallback: assuming same row order between polygons and segments."
    )
    out_table$poly_id_f <- factor(out_table$poly_id, levels = out_table$poly_id)
    out_table$seg_num   <- seq_len(nrow(out_table))
  } else {
    message(
      "poly_id and Segment do not match and lengths differ. ",
      "Assigning seg_num by polygon row order."
    )
    out_table$poly_id_f <- factor(out_table$poly_id, levels = out_table$poly_id)
    out_table$seg_num   <- seq_len(nrow(out_table))
  }

  # ==========================================================
  # ---- Season colours (FORZAR palettes.R si color_map = NULL)
  # ==========================================================
  is_named <- function(x) !is.null(names(x)) && !any(names(x) == "")

  if (!is.null(color_map)) {
    season_colors <- color_map
  } else {
    # Niveles realmente presentes en los datos
    if (season_col %in% names(data_segments)) {
      season_levels <- sort(unique(as.character(data_segments[[season_col]])))
    } else {
      season_levels <- character(0)
    }

    pal_to_use <- NULL

    # 1) palettes.R (ruta absoluta por defecto)
    if (!is.null(palette_file) && file.exists(palette_file)) {
      envp <- new.env()
      tryCatch(
        {
          sys.source(palette_file, envir = envp)
          if (exists("season_palette", envir = envp, inherits = FALSE)) {
            pal_to_use <- envp$season_palette
          }
        },
        error = function(e) pal_to_use <<- NULL
      )
    } else if (exists("season_palette", envir = .GlobalEnv, inherits = FALSE)) {
      # 2) GlobalEnv si ya lo sourceaste en tu sesión
      pal_to_use <- get("season_palette", envir = .GlobalEnv)
    }

    # Si tengo paleta válida, filtro a niveles presentes
    if (!is.null(pal_to_use) && is_named(pal_to_use)) {
      season_colors <- pal_to_use[names(pal_to_use) %in% season_levels]
      # Si tus datos tienen exactamente Summer/Autumn/Winter/Spring, esto queda perfecto.
      # Si tienen otros nombres, no va a matchear (y caerá al fallback).
    } else {
      season_colors <- NULL
    }

    # Fallback si no se pudo cargar/matchear
    if ((is.null(season_colors) || length(season_colors) == 0) && length(season_levels) > 0) {
      pal <- RColorBrewer::brewer.pal(
        min(4, length(season_levels)),
        "Set2"
      )
      season_colors <- stats::setNames(pal, season_levels)

      warning(
        "No se pudo usar `season_palette` desde palettes.R (o no matchea con tus niveles de season). ",
        "Usando fallback RColorBrewer::Set2."
      )
    }

    if (length(season_levels) == 0) {
      season_colors <- NULL
    }
  }

  # ---- Aesthetics for ggplot ----
  if (season_col %in% names(data_segments)) {
    aes_args <- ggplot2::aes(
      x    = !!rlang::sym(segment_col),
      y    = ALTITUDE1,
      fill = !!rlang::sym(season_col)
    )
  } else {
    aes_args <- ggplot2::aes(
      x = !!rlang::sym(segment_col),
      y = ALTITUDE1
    )
  }

  # ---- Base plot: boxplots per segment ----
  p <- ggplot2::ggplot(data_segments, aes_args) +
    ggplot2::geom_boxplot(
      width        = 0.6,
      position     = ggplot2::position_dodge(width = 0.8),
      staplewidth  = 1,
      outlier.size = 0.8
    ) +
    ggplot2::scale_x_discrete(
      name   = "Segment",
      limits = seg_levels,
      labels = seg_levels
    ) +
    ggplot2::scale_y_continuous(name = "Altitude") +
    ggplot2::stat_summary(
      fun      = mean,
      geom     = "point",
      shape    = 18,
      size     = 1.5,
      colour   = "red",
      position = ggplot2::position_dodge(width = 0.8)
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      aspect.ratio    = 1
    )

  if (!is.null(season_colors)) {
    p <- p + ggplot2::scale_fill_manual(values = season_colors)
  }

  # ---- Elevation min/max segments on top ----
  seg_df <- out_table[
    !is.na(out_table$seg_num) & !is.na(out_table$elev_max),
    ,
    drop = FALSE
  ]

  if (nrow(seg_df) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data        = seg_df,
        mapping     = ggplot2::aes(
          x    = seg_num - seg_half_width,
          xend = seg_num + seg_half_width,
          y    = elev_max,
          yend = elev_max
        ),
        inherit.aes = FALSE,
        colour      = "grey70",
        linewidth   = 0.9
      ) +
      ggplot2::geom_segment(
        data        = seg_df,
        mapping     = ggplot2::aes(
          x    = seg_num - seg_half_width,
          xend = seg_num + seg_half_width,
          y    = elev_min,
          yend = elev_min
        ),
        inherit.aes = FALSE,
        colour      = "grey70",
        linewidth   = 0.9
      )
  }

  if (plot) {
    print(p)
  }

  list(
    plot      = p,
    out_table = out_table,
    data      = data_segments
  )
}
