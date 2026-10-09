# Steps 3-7 of the BioDyn workflow on the Neoxolmis rufiventris example. Every
# figure is the output of a BioDyn function (nothing is drawn by hand), saved to
# <out>. The three segments are rectangles drawn BY EYE from Fig. 2C of the
# article (the original GeoJSON is not available), so segment counts only
# approximate the article's.
# Usage: Rscript step2_figures.R <example-data> <CHELSA_bio1.tif> <out>
args <- commandArgs(trailingOnly = TRUE)
dd <- args[1]; bio1 <- args[2]; out <- args[3]
suppressPackageStartupMessages({library(BioDyn); library(sf); library(ggplot2)})
dir.create(out, showWarnings = FALSE, recursive = TRUE)

box <- function(x0, x1, y0, y1)
  st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))))
segs <- st_sf(id = c("1", "2", "3"),
              geometry = st_sfc(box(-66, -54, -40, -30), box(-73, -64, -44, -40),
                                box(-74, -62, -56, -44), crs = 4326))
seg_path <- file.path(out, "segments.geojson")
st_write(segs, seg_path, delete_dsn = TRUE, quiet = TRUE)

eco <- st_read(file.path(dd, "Ecoreg_monj.shp"), quiet = TRUE)
eco <- eco[eco$ECO_NAME %in% c("Patagonian steppe", "Humid Pampas"), ]

occ <- file.path(dd, "ebd_AR_chvtyr2_smp_relDec-2025.txt")
chk <- file.path(dd, "ebd_AR_chvtyr2_smp_relDec-2025_sampling.txt")
pr <- data.table::fread(occ, sep = "\t", encoding = "UTF-8")
cl <- build_data(occ, chk, sep = "\t")
eff <- function(x) filter_ebird(x, max_distance_km = 10, max_observers = 10, max_duration_min = 300)
pr <- eff(pr); cl <- eff(cl)

# 3a / 3b: the maps drawn by filter_ecoregion() and filter_radius()
pr_eco <- filter_ecoregion(pr, region_sf = eco, pres_col = "presence", buffer_km = 0,
                           interactive = FALSE, title = "Data filtered by ecoregion",
                           save_path = file.path(out, "filter_ecoregion.png"))
cl_rad <- filter_radius(cl, radius_km = 20, interactive = FALSE,
                        title = "Data filtered by radius",
                        save_path = file.path(out, "filter_radius.png"))

# 3c: filter_pol() prints its own ggplot figures (bar plot and dot plot)
png(file.path(out, "filter_pol_%d.png"), width = 1400, height = 1000, res = 200)
suppressMessages(pr_seg <- filter_pol(pr_eco, seg_path, "COMMON NAME", file.path(out, "pr_seg.csv")))
suppressMessages(cl_seg <- filter_pol(cl_rad, seg_path, "COMMON NAME", file.path(out, "cl_seg.csv")))
dev.off()
print(table(as.data.frame(sf::st_drop_geometry(pr_seg))$Segment))

# 4-5: seasons and the temperature raster
seas <- function(x) add_season(split_dates(as.data.frame(sf::st_drop_geometry(x)), preview = FALSE))
pr_s <- seas(pr_seg); cl_s <- seas(cl_seg)
pr_e <- suppressMessages(extract_rasters(pr_s, c(bio01 = bio1)))

# 6-7: the plots drawn by plot_effort() and plot_variable()
ggsave(file.path(out, "plot_effort.png"), plot_effort(cl_s), width = 9, height = 7, dpi = 200, bg = "white")
ggsave(file.path(out, "plot_variable_bio01.png"),
       plot_variable(pr_e, var = "bio01", type = "density", seasons = c("Summer", "Winter"),
                     x_title = "Annual mean temperature (CHELSA bio01, °C)"),
       width = 6.5, height = 3.8, dpi = 200, bg = "white")
cat("presences by season and segment:\n"); print(table(pr_s$season, pr_s$Segment))
