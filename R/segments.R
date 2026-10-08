# =========================================================
# General helper: ensure numeric coordinates
# =========================================================

#' Make latitude and longitude columns numeric
#'
#' Coerces the `LATITUDE` and `LONGITUDE` columns of a data frame to numeric,
#' performing basic cleaning (trimming whitespace, converting decimal commas to
#' decimal points) and dropping rows with missing coordinates.
#'
#' @param df A data frame containing `LATITUDE` and `LONGITUDE` columns.
#'
#' @return A data frame with numeric `LATITUDE` and `LONGITUDE` columns and all
#'   rows with missing coordinates removed.
#' @export
#'
#' @examples
#' \dontrun{
#' cleaned <- make_numeric_coordinates(raw_data)
#' }
make_numeric_coordinates <- function(df) {
  # If the data frame does not have both columns, do nothing
  if (!all(c("LATITUDE", "LONGITUDE") %in% names(df))) {
    return(df)
  }

  for (col in c("LATITUDE", "LONGITUDE")) {
    if (!is.numeric(df[[col]])) {
      x <- df[[col]]

      # Factor -> character
      if (is.factor(x)) {
        x <- as.character(x)
      } else {
        # Other types (logical, integer, etc.) -> character for safe trim/sub
        x <- as.character(x)
      }

      # Clean whitespace and decimal commas
      x <- trimws(x)
      x <- gsub(",", ".", x, fixed = TRUE)

      df[[col]] <- suppressWarnings(as.numeric(x))
    }
  }

  # Filter rows with NA coordinates
  df <- df[!is.na(df$LATITUDE) & !is.na(df$LONGITUDE), , drop = FALSE]

  df
}

# =========================================================
# I/O helpers (input: file path or data.frame)
# =========================================================

#' Read a delimited text file with automatic encoding detection
#'
#' Attempts to read a delimited text file (typically a CSV) using a sequence
#' of common encodings until one succeeds. Optionally, a specific encoding
#' can be forced.
#'
#' @param path_or_buf A character scalar with the path to the file to be read.
#' @param encoding Character string specifying the encoding to use. If
#'   `"auto"`, several encodings are tried in sequence.
#' @param sep The field separator (delimiter) passed to `readr::read_delim()`.
#' @param ... Additional arguments passed on to `readr::read_delim()`.
#'
#' @return A tibble/data frame read from the file.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- read_csv_auto("data/observations.csv")
#' }
read_csv_auto <- function(path_or_buf,
                          encoding = "auto",
                          sep = ",",
                          ...) {
  if (!is.character(path_or_buf)) {
    stop("Unsupported input in `read_csv_auto()`: must be a file path.")
  }

  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required for `read_csv_auto()`.")
  }

  if (!is.character(encoding) || length(encoding) != 1L) {
    stop("`encoding` must be a character scalar.")
  }

  if (encoding != "auto") {
    return(
      readr::read_delim(
        file     = path_or_buf,
        delim    = sep,
        locale   = readr::locale(encoding = encoding),
        progress = FALSE,
        ...
      )
    )
  }

  encodings_to_try <- c("UTF-8", "UTF-8-BOM", "latin1", "CP1252")
  errors <- list()

  for (enc in encodings_to_try) {
    res <- tryCatch(
      {
        readr::read_delim(
          file     = path_or_buf,
          delim    = sep,
          locale   = readr::locale(encoding = enc),
          progress = FALSE,
          ...
        )
      },
      error = function(e) e
    )

    if (!inherits(res, "error")) {
      return(res)
    } else {
      # store the human-readable message
      errors[[enc]] <- conditionMessage(res)
    }
  }

  msg <- paste0(
    "Failed to decode the delimited file. Attempts:\n",
    paste(sprintf("- %s: %s", names(errors), unlist(errors)), collapse = "\n")
  )
  stop(msg)
}

#' Ensure a data frame input from a data frame or a CSV path
#'
#' Accepts either a data frame or a character path to a CSV file and returns
#' a data frame. Paths are read using [read_csv_auto()].
#'
#' @param obj Either a data frame or a character scalar with a path to a CSV.
#' @param encoding Character string specifying the encoding used when
#'   `obj` is a file path. See \link{read_csv_auto}.
#' @param sep Field separator (delimiter) when reading from a CSV file.
#' @param ... Additional arguments passed on to \link{read_csv_auto}.

#'
#' @return A data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' df1 <- ensure_data_frame(my_data_frame)
#' df2 <- ensure_data_frame("data/observations.csv")
#' }
ensure_data_frame <- function(obj,
                              encoding = "auto",
                              sep = ",",
                              ...) {
  if (is.data.frame(obj)) return(obj)

  if (is.character(obj) && length(obj) == 1L) {
    return(read_csv_auto(obj, encoding = encoding, sep = sep, ...))
  }

  stop(
    sprintf(
      "Unsupported input type: %s. Use a CSV file path or a data frame.",
      paste(class(obj), collapse = ", ")
    )
  )
}

# =========================================================
# 1) Interactive map with points + drawing tools
#    (RStudio Viewer + 💾 button for GeoJSON)
# =========================================================

#' Create an interactive map with observation points and drawing tools
#'
#' Builds an interactive leaflet map with observation points, a drawing
#' toolbar (polygons and rectangles), and a button to download the drawn
#' polygons as a GeoJSON file. Optionally saves the map as a self-contained
#' HTML file.
#'
#' @param data A data frame or a path to a CSV file containing at least
#'   `LATITUDE` and `LONGITUDE` columns.
#' @param html_file Name of the output HTML file for the interactive map.
#' @param save_html Logical; if `TRUE`, saves the widget as an HTML file.
#' @param encoding Encoding used when `data` is a CSV path. See
#'   \link{read_csv_auto}.
#' @param sep Field separator (delimiter) when `data` is a CSV path.
#'
#' @return Invisibly returns the leaflet map widget.
#' @export
#'
#' @examples
#' \dontrun{
#' create_segments("data/observations.csv", save_html = TRUE)
#' }
create_segments <- function(data,
                            html_file = "interactive_observations_map.html",
                            save_html = FALSE,
                            encoding = "auto",
                            sep = ",") {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.")
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required.")
  }
  if (!requireNamespace("leaflet.extras", quietly = TRUE)) {
    stop("Package 'leaflet.extras' is required.")
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Package 'htmlwidgets' is required.")
  }

  df <- ensure_data_frame(data, encoding = encoding, sep = sep)

  if (!all(c("LATITUDE", "LONGITUDE") %in% names(df))) {
    stop("Columns 'LATITUDE' and 'LONGITUDE' are required.")
  }

  # Ensure numeric coordinates
  df <- make_numeric_coordinates(df)

  if (nrow(df) == 0L) {
    message(
      "Empty data frame after converting/cleaning lat/lon. ",
      "Cannot create the map."
    )
    return(invisible(NULL))
  }

  sf_pts <- sf::st_as_sf(
    df,
    coords = c("LONGITUDE", "LATITUDE"),
    crs = 4326,
    remove = FALSE
  )

  coords <- sf::st_coordinates(sf_pts)
  map_center <- c(
    mean(coords[, "Y"], na.rm = TRUE),
    mean(coords[, "X"], na.rm = TRUE)
  )

  # Species column
  species_col <- NA_character_
  if ("species" %in% names(df)) {
    species_col <- "species"
  } else if ("SCIENTIFIC NAME" %in% names(df)) {
    species_col <- "SCIENTIFIC NAME"
  } else if ("COMMON NAME" %in% names(df)) {
    species_col <- "COMMON NAME"
  }

  popup_fun <- function(row) {
    txt <- "Details not available"
    if (!is.na(species_col)) {
      val <- row[[species_col]]
      if (is.na(val) || length(val) == 0) val <- "N/A"
      txt <- sprintf("Species: %s", val)
    }
    if ("OBSERVATION DATE" %in% names(row)) {
      txt <- paste0(txt, "<br>Date: ", row[["OBSERVATION DATE"]])
    }
    if ("OBSERVER ID" %in% names(row)) {
      txt <- paste0(txt, "<br>Observer: ", row[["OBSERVER ID"]])
    }
    txt
  }

  popups <- vapply(
    seq_len(nrow(df)),
    function(i) popup_fun(df[i, , drop = FALSE]),
    FUN.VALUE = character(1)
  )

  m <- leaflet::leaflet(df) |>
    leaflet::addTiles() |>
    leaflet::setView(lng = map_center[2], lat = map_center[1], zoom = 2) |>
    leaflet::addCircleMarkers(
      lng         = ~LONGITUDE,
      lat         = ~LATITUDE,
      radius      = 3,
      stroke      = FALSE,
      fillColor   = "red",
      fillOpacity = 0.6,
      popup       = popups
    ) |>
    leaflet.extras::addDrawToolbar(
      targetGroup      = "polygons",
      polygonOptions   = leaflet.extras::drawPolygonOptions(),
      rectangleOptions = leaflet.extras::drawRectangleOptions(),
      circleOptions    = FALSE,
      markerOptions    = FALSE,
      polylineOptions  = FALSE,
      editOptions      = leaflet.extras::editToolbarOptions()
    ) |>
    leaflet::addLayersControl(
      overlayGroups = "polygons",
      options       = leaflet::layersControlOptions(collapsed = TRUE)
    )

  # JavaScript: 💾 button to export drawn polygons as GeoJSON
  m <- htmlwidgets::onRender(
    m,
    "
    function(el, x) {
      var map = this;

      var drawnItems = new L.FeatureGroup();
      map.addLayer(drawnItems);

      map.on('draw:created', function(e) {
        drawnItems.addLayer(e.layer);
      });

      map.on('draw:deleted', function(e) {
        e.layers.eachLayer(function(layer) {
          drawnItems.removeLayer(layer);
        });
      });

      var exportControl = L.control({position: 'topright'});
      exportControl.onAdd = function(map) {
        var container = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
        var link = L.DomUtil.create('a', '', container);

        link.href = '#';
        link.innerHTML = '&#128190;';
        link.title = 'Download polygons as GeoJSON';

        L.DomEvent.on(link, 'click', function(e) {
          L.DomEvent.stopPropagation(e);
          L.DomEvent.preventDefault(e);

          var data = drawnItems.toGeoJSON();

          if (!data || !data.features || data.features.length === 0) {
            alert('There are no drawn polygons to export.');
            return;
          }

          var blob = new Blob(
            [JSON.stringify(data)],
            {type: 'application/vnd.geo+json'}
          );
          var url = URL.createObjectURL(blob);
          var a = document.createElement('a');
          a.href = url;
          a.download = 'observation_polygons.geojson';
          document.body.appendChild(a);
          a.click();
          document.body.removeChild(a);
          URL.revokeObjectURL(url);
        });

        return container;
      };
      exportControl.addTo(map);
    }
    "
  )

  if (isTRUE(save_html)) {
    full_path <- normalizePath(html_file, mustWork = FALSE)
    suppressWarnings(
      htmlwidgets::saveWidget(
        m,
        file          = full_path,
        selfcontained = TRUE
      )
    )
    message("Interactive map saved to: ", full_path)
  }

  if (interactive()) {
    print(m)
  }

  invisible(m)
}



# =========================================================
# 2) Static visualizations (ggplot2)
# =========================================================

#' Create static ggplot2 visualizations by segment
#'
#' Produces static ggplot2 plots summarizing the number of observations per
#' segment and their spatial distribution.
#'
#' @param filtered_gdf A data frame containing at least a `Segment` column
#'   and observation-level data.
#' @param segments_info A named list with segment metadata; each element should
#'   contain a `gdf` data frame with `LATITUDE` and `LONGITUDE` columns.
#'
#' @return Invisibly returns `NULL`. Plots are printed for their side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' create_ggplot_visualizations(filtered_gdf, segments_info)
#' }
create_ggplot_visualizations <- function(filtered_gdf, segments_info) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualizations.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }

  if (nrow(filtered_gdf) == 0L) {
    message("Empty filtered data frame. Cannot create visualizations.")
    return(invisible(NULL))
  }

  if (!"Segment" %in% names(filtered_gdf) ||
      dplyr::n_distinct(filtered_gdf$Segment) <= 1L) {
    message(
      "Column 'Segment' is invalid or has a single value. ",
      "Skipping per-segment visualizations."
    )
    return(invisible(NULL))
  }

  filtered_gdf$Segment <- as.factor(filtered_gdf$Segment)

  # 1) Number of observations per segment
  p1 <- ggplot2::ggplot(filtered_gdf, ggplot2::aes(x = Segment)) +
    ggplot2::geom_bar(fill = "skyblue") +
    ggplot2::labs(
      title = "Number of observations by segment",
      x     = "Segment",
      y     = "Number of observations"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  print(p1)

  # 2) Spatial distribution by segment
  pieces <- lapply(
    names(segments_info),
    function(name) {
      info <- segments_info[[name]]
      segment_gdf <- info$gdf

      if (is.null(segment_gdf) || nrow(segment_gdf) == 0L) {
        return(NULL)
      }

      if (!all(c("LONGITUDE", "LATITUDE") %in% names(segment_gdf))) {
        message(
          "Segment ", name,
          " does not have LONGITUDE/LATITUDE columns. It is omitted."
        )
        return(NULL)
      }

      data.frame(
        LONGITUDE = segment_gdf$LONGITUDE,
        LATITUDE  = segment_gdf$LATITUDE,
        Segment   = name,
        stringsAsFactors = FALSE
      )
    }
  )

  # remove NULLs and bind
  pieces <- Filter(Negate(is.null), pieces)
  df_scatter <- if (length(pieces) > 0) dplyr::bind_rows(pieces) else NULL

  if (!is.null(df_scatter) && nrow(df_scatter) > 0L) {
    p2 <- ggplot2::ggplot(
      df_scatter,
      ggplot2::aes(x = LONGITUDE, y = LATITUDE, colour = Segment)
    ) +
      ggplot2::geom_point(alpha = 0.7, size = 1.5) +
      ggplot2::labs(
        x     = "Longitude",
        y     = "Latitude",
        title = "Spatial distribution of observations by segment"
      ) +
      ggplot2::theme_minimal()

    print(p2)
  } else {
    message(
      "No valid segment data available for the spatial distribution plot."
    )
  }

  invisible(NULL)
}

# =========================================================
# 3) Interactive segment maps (leaflet + plotly)
# =========================================================

#' Create interactive maps by segment using leaflet and plotly
#'
#' Builds interactive maps combining leaflet polygons and points by segment,
#' and a global `plotly::plot_geo()` visualization of segmented observations.
#'
#' @param filtered_gdf A data frame with at least `LATITUDE`, `LONGITUDE`
#'   and `Segment` columns.
#' @param segments_info A named list with entries for each segment; each entry
#'   should contain a `gdf` data frame and a `poligono` geometry.
#' @param map_center Numeric vector of length 2 with the map center
#'   (`c(latitude, longitude)`).
#' @param species_column Optional character string with the name of the column
#'   containing the species used for popups and hover text.
#'
#' @return Invisibly returns `NULL`. HTML widgets are saved to disk.
#' @export
#'
#' @examples
#' \dontrun{
#' create_segment_interactive_maps(filtered_gdf, segments_info, c(-34, -58))
#' }
create_segment_interactive_maps <- function(filtered_gdf,
                                            segments_info,
                                            map_center,
                                            species_column = NULL) {
  if (nrow(filtered_gdf) == 0L) {
    message("Empty filtered data frame. Cannot create interactive maps.")
    return(invisible(NULL))
  }

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required.")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.")
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required.")
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Package 'htmlwidgets' is required.")
  }

  colors <- c(
    "red", "blue", "green", "orange",
    "purple", "darkred", "lightred"
  )

  m_all <- leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::setView(lng = map_center[2], lat = map_center[1], zoom = 2)

  valid_segments_mapped <- 0L

  for (i in seq_along(segments_info)) {
    segment_name <- names(segments_info)[i]
    info         <- segments_info[[i]]
    polygon      <- info$poligono
    segment_gdf  <- info$gdf

    if (!is.null(polygon) && !is.null(segment_gdf) && nrow(segment_gdf) > 0L) {
      # Ensure numeric coordinates for each segment
      segment_gdf <- make_numeric_coordinates(segment_gdf)
      if (nrow(segment_gdf) == 0L) {
        next
      }

      color <- colors[(i - 1L) %% length(colors) + 1L]

      poly_sf <- sf::st_as_sf(
        data.frame(id = 1L),
        geometry = sf::st_sfc(polygon),
        crs      = 4326
      )

      m_all <- m_all |>
        leaflet::addPolygons(
          data        = poly_sf,
          fillColor   = color,
          color       = color,
          weight      = 2,
          fillOpacity = 0.1,
          popup       = segment_name
        )

      # build popups safely (use vapply)
      popup_seg <- vapply(
        seq_len(nrow(segment_gdf)),
        function(ii) {
          row <- segment_gdf[ii, , drop = FALSE]
          txt <- sprintf("Segment: %s", segment_name)
          if (!is.null(species_column) &&
              species_column %in% names(segment_gdf)) {
            val <- row[[species_column]]
            if (is.na(val) || length(val) == 0) val <- "N/A"
            txt <- paste0("Species: ", val, " - ", segment_name)
          }
          if ("OBSERVATION DATE" %in% names(row)) {
            txt <- paste0(txt, "<br>Date: ", row[["OBSERVATION DATE"]])
          }
          txt
        },
        FUN.VALUE = character(1)
      )

      m_all <- m_all |>
        leaflet::addCircleMarkers(
          lng         = segment_gdf$LONGITUDE,
          lat         = segment_gdf$LATITUDE,
          radius      = 3,
          stroke      = FALSE,
          fillColor   = color,
          fillOpacity = 0.7,
          popup       = popup_seg
        )

      valid_segments_mapped <- valid_segments_mapped + 1L
    }
  }

  if (valid_segments_mapped > 0L) {
    leaflet_path <- normalizePath(
      "segment_interactive_map.html",
      mustWork = FALSE
    )
    suppressWarnings(
      htmlwidgets::saveWidget(
        m_all,
        file          = leaflet_path,
        selfcontained = TRUE
      )
    )
    message("Interactive segment map saved to: ", leaflet_path)
  } else {
    message(
      "No valid segment data available for the interactive segment map."
    )
  }

  # --- Plotly scatter_geo with Segment ---
  df_plotly <- filtered_gdf
  if ("geometry" %in% names(df_plotly)) {
    df_plotly$geometry <- NULL
  }

  df_plotly <- make_numeric_coordinates(df_plotly)

  if (!all(c("LATITUDE", "LONGITUDE", "Segment") %in% names(df_plotly))) {
    message(
      "Missing columns ('LATITUDE', 'LONGITUDE' or 'Segment') for the ",
      "plotly map."
    )
    return(invisible(NULL))
  }

  hover_text <- if (!is.null(species_column) &&
                    species_column %in% names(df_plotly)) {
    df_plotly[[species_column]]
  } else {
    df_plotly$Segment
  }

  fig <- plotly::plot_geo(
    df_plotly,
    lat       = ~LATITUDE,
    lon       = ~LONGITUDE,
    color     = ~Segment,
    text      = hover_text,
    hoverinfo = "text",
    mode      = "markers"
  ) |>
    plotly::layout(title = "Observations segmented by polygons")

  plotly_path <- normalizePath(
    "segment_plotly_map.html",
    mustWork = FALSE
  )
  suppressWarnings(
    htmlwidgets::saveWidget(
      fig,
      file          = plotly_path,
      selfcontained = TRUE
    )
  )
  message("Segment plotly interactive map saved to: ", plotly_path)

  invisible(NULL)
}


# =========================================================
# 4) Filter by polygons
# =========================================================

#' Filter observation points by polygons drawn and saved as GeoJSON
#'
#' Reads observation data and polygons (GeoJSON), assigns each point to the
#' first polygon that contains it, computes per-segment statistics, saves a
#' CSV with segmented data, and optionally creates visualizations and
#' interactive maps.
#'
#' @param data A data frame or a character path to a CSV file containing at
#'   least `LATITUDE` and `LONGITUDE` columns.
#' @param polygon_path Path to a GeoJSON file containing the drawn polygons.
#' @param species_column Optional name of the species column used for
#'   summaries and hover text.
#' @param output_file Name of the CSV file where segmented data are saved.
#' @param encoding Encoding used when `data` is a CSV path.
#' @param sep Field separator (delimiter) used when `data` is a CSV path.
#'
#' @return Invisibly returns the filtered `sf` object with points inside
#'   polygons.
#' @export
#'
filter_pol <- function(data,
                       polygon_path   = "data.geojson",
                       species_column = NULL,
                       output_file    = "segmented_data.csv",
                       encoding       = "auto",
                       sep            = ",") {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required.")
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required.")
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Package 'htmlwidgets' is required.")
  }

  # -------- Read input (data.frame or CSV path) --------
  if (is.data.frame(data)) {
    df <- data
  } else if (is.character(data) && length(data) == 1L) {
    df <- read_csv_auto(data, encoding = encoding, sep = sep)
  } else {
    stop("Argument `data` must be a data frame or a path to a CSV file.")
  }

  if (!all(c("LATITUDE", "LONGITUDE") %in% names(df))) {
    stop("Columns 'LATITUDE' and 'LONGITUDE' are required.")
  }

  # Ensure numeric coordinates
  df <- make_numeric_coordinates(df)

  if (nrow(df) == 0L) {
    message(
      "Empty data frame after cleaning lat/lon. ",
      "Cannot filter by polygons."
    )
    return(invisible(NULL))
  }

  # -------- Points as sf, keeping LONG/LAT --------
  gdf <- sf::st_as_sf(
    df,
    coords = c("LONGITUDE", "LATITUDE"),
    crs    = 4326,
    remove = FALSE
  )

  coords <- sf::st_coordinates(gdf)
  map_center <- c(
    mean(coords[, "Y"], na.rm = TRUE),
    mean(coords[, "X"], na.rm = TRUE)
  )

  # -------- Read drawn polygons (GeoJSON) --------
  if (!file.exists(polygon_path)) {
    message("Error: File not found: ", polygon_path)
    message(
      "Make sure you have drawn and saved the polygons first ",
      "(e.g., using `interactive_map()`)."
    )
    return(invisible(NULL))
  }

  drawn_polygons <- sf::st_read(polygon_path, quiet = TRUE)

  if (nrow(drawn_polygons) == 0L) {
    message("Error: The GeoJSON file does not contain any polygons.")
    return(invisible(NULL))
  }

  message(
    "Found ", nrow(drawn_polygons),
    " polygon(s) in the file: ", polygon_path
  )

  # Ensure same CRS
  drawn_polygons <- sf::st_transform(
    drawn_polygons,
    crs = sf::st_crs(gdf)
  )

  # -------- Assign segment to each point (now using character "1","2",...) --------
  gdf_segmented <- gdf
  gdf_segmented$Segment <- "Outside_polygons"

  segments_info <- list()

  for (i in seq_len(nrow(drawn_polygons))) {
    # Use plain character numbers as segment labels: "1", "2", "3", ...
    segment_name     <- as.character(i)
    polygon_geometry <- drawn_polygons[i, ]

    inside <- sf::st_within(
      gdf_segmented,
      polygon_geometry,
      sparse = FALSE
    )[, 1]

    idx <- inside & gdf_segmented$Segment == "Outside_polygons"
    gdf_segmented$Segment[idx] <- segment_name
  }

  gdf_filtered <- gdf_segmented[gdf_segmented$Segment != "Outside_polygons", ]

  # -------- Build segments_info (for stats and plots) --------
  for (i in seq_len(nrow(drawn_polygons))) {
    segment_name <- as.character(i)
    polygon      <- drawn_polygons[i, ]$geometry[[1]]

    gdf_segment <- gdf_filtered[gdf_filtered$Segment == segment_name, ]

    if (nrow(gdf_segment) > 0L) {
      unique_species <- if (!is.null(species_column) &&
                            species_column %in% names(gdf_segment)) {
        dplyr::n_distinct(gdf_segment[[species_column]])
      } else {
        NA
      }

      segments_info[[segment_name]] <- list(
        gdf          = as.data.frame(sf::st_drop_geometry(gdf_segment)),
        poligono     = polygon,
        estadisticas = list(
          total_puntos    = nrow(gdf_segment),
          especies_unicas = unique_species
        )
      )

      message(
        "Segment ", segment_name, ": ", nrow(gdf_segment),
        " points, ",
        if (!is.na(unique_species)) {
          paste0(unique_species, " unique species")
        } else {
          "N/A unique species"
        }
      )
    } else {
      message("Segment ", segment_name, ": 0 points")
    }
  }

  # -------- Save CSV without geometry --------
  if (nrow(gdf_filtered) > 0L) {
    filtered_df_csv <- as.data.frame(sf::st_drop_geometry(gdf_filtered))
    # Ensure Segment is character (values like "1","2",...)
    if ("Segment" %in% names(filtered_df_csv)) {
      filtered_df_csv$Segment <- as.character(filtered_df_csv$Segment)
    }
    readr::write_csv(filtered_df_csv, output_file)
    output_csv_path <- normalizePath(output_file, mustWork = FALSE)
    message("Segmented data saved to: ", output_csv_path)
  } else {
    message("No points were found inside the polygons.")
  }

  # -------- Visualizations (optional) --------
  if (nrow(gdf_filtered) > 0L && length(segments_info) > 0L) {
    filtered_gdf_df <- as.data.frame(sf::st_drop_geometry(gdf_filtered))

    # Ensure numeric coordinates just in case
    filtered_gdf_df <- make_numeric_coordinates(filtered_gdf_df)

    # ggplot (if create_ggplot_visualizations is defined)
    if (exists("create_ggplot_visualizations", mode = "function")) {
      create_ggplot_visualizations(filtered_gdf_df, segments_info)
    }

    # Original plotly: all points
    if (all(c("LATITUDE", "LONGITUDE") %in% names(df))) {
      hover_original <- if (!is.null(species_column) &&
                            species_column %in% names(df)) {
        df[[species_column]]
      } else {
        NULL
      }

      fig_original <- plotly::plot_geo(
        as.data.frame(df),
        lat   = ~LATITUDE,
        lon   = ~LONGITUDE,
        text  = hover_original,
        hoverinfo = if (is.null(hover_original)) "lon+lat" else "text",
        mode  = "markers"
      ) |>
        plotly::layout(title = "Observations (all)")

      print(fig_original)
    }

    # Plotly filtered by segments
    if (all(c("LATITUDE", "LONGITUDE", "Segment") %in% names(filtered_gdf_df))) {
      hover_filtered <- if (!is.null(species_column) &&
                            species_column %in% names(filtered_gdf_df)) {
        filtered_gdf_df[[species_column]]
      } else {
        filtered_gdf_df$Segment
      }

      fig_filtered <- plotly::plot_geo(
        filtered_gdf_df,
        lat   = ~LATITUDE,
        lon   = ~LONGITUDE,
        color = ~Segment,
        text  = hover_filtered,
        hoverinfo = "text",
        mode  = "markers"
      ) |>
        plotly::layout(
          title = "Observations filtered by polygons"
        )

      print(fig_filtered)
    }

    # Interactive segment maps (if defined)
    if (exists("create_segment_interactive_maps", mode = "function")) {
      create_segment_interactive_maps(
        filtered_gdf    = filtered_gdf_df,
        segments_info   = segments_info,
        map_center      = map_center,
        species_column  = species_column
      )
    }
  }

  # -------- Statistical summary --------
  message("\n--- STATISTICAL SUMMARY ---")
  total_original <- nrow(gdf)
  message("Total original points: ", total_original)
  total_filtered <- nrow(gdf_filtered)
  if (total_original > 0L) {
    message(
      "Total points inside polygons: ",
      total_filtered,
      sprintf(" (%.1f%%)", if (total_original > 0L) total_filtered / total_original * 100 else 0)
    )
  }

  for (segment_name in names(segments_info)) {
    info <- segments_info[[segment_name]]
    total_segment_points <- info$estadisticas$total_puntos
    perc <- if (total_original > 0L) {
      total_segment_points / total_original * 100
    } else 0
    message(
      "Segment ", segment_name, ": ",
      total_segment_points,
      sprintf(" points (%.1f%%)", perc)
    )
  }

  invisible(gdf_filtered)
}


# =========================================================
# 5) Load segmented data
# =========================================================

#' Load already segmented data
#'
#' Loads segmented observation data either from a data frame or a CSV path,
#' prints a summary of the total number of observations and segments, and
#' returns the data frame.
#'
#' @param data A data frame or a character path to a CSV file containing
#'   segmented data.
#' @param encoding Encoding used when `data` is a CSV path.
#' @param sep Field separator (delimiter) used when `data` is a CSV path.
#'
#' @return A data frame with the segmented data.
#' @export
#'
#' @examples
#' \dontrun{
#' df_segmented <- load_segmented_data("segmented_data.csv")
#' }
load_segmented_data <- function(data,
                                encoding = "auto",
                                sep = ",") {
  df <- ensure_data_frame(data, encoding = encoding, sep = sep)

  message("Total segmented observations: ", nrow(df))
  if ("Segment" %in% names(df)) {
    message(
      "Segments found: ",
      paste(unique(df$Segment), collapse = ", ")
    )
    message("Number of observations per segment:")
    print(table(df$Segment))
  }

  df
}
