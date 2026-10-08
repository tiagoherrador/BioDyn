# Steps 3c-6 on the Neoxolmis rufiventris example, after step1_filters.R.
# The three segments are drawn BY EYE from Fig. 2C of the article (the original
# GeoJSON is not available), so segment counts only approximate the article's.
# Usage: Rscript step2_segments_seasons.R <example-data> <rasters-folder> <out>
args <- commandArgs(trailingOnly = TRUE)
dd <- args[1]; rd <- args[2]; out <- args[3]
suppressPackageStartupMessages({library(BioDyn); library(sf); library(dplyr); library(ggplot2)})
grDevices::pdf(NULL)
dir.create(out, showWarnings = FALSE, recursive = TRUE)

box <- function(x0, x1, y0, y1)
  st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))))
segs <- st_sf(id = c("1", "2", "3"),
              geometry = st_sfc(box(-66, -54, -40, -30),   # north  (Pampas)
                                box(-73, -64, -44, -40),   # centre
                                box(-74, -62, -56, -44),   # south  (Patagonia)
                                crs = 4326))
seg_path <- file.path(out, "segments.geojson")
st_write(segs, seg_path, delete_dsn = TRUE, quiet = TRUE)

eco <- st_read(file.path(dd, "Ecoreg_monj.shp"), quiet = TRUE)
eco <- eco[eco$ECO_NAME %in% c("Patagonian steppe", "Humid Pampas"), ]

occ <- file.path(dd, "ebd_AR_chvtyr2_smp_relDec-2025.txt")
chk <- file.path(dd, "ebd_AR_chvtyr2_smp_relDec-2025_sampling.txt")
pr  <- data.table::fread(occ, sep = "\t", encoding = "UTF-8")
cl  <- build_data(occ, chk, sep = "\t")
eff <- function(x) filter_ebird(x, max_distance_km = 10, max_observers = 10, max_duration_min = 300)
pr <- eff(pr); cl <- eff(cl)
pr_eco <- filter_ecoregion(pr, region_sf = eco, pres_col = "presence", buffer_km = 0, interactive = FALSE)
cl_rad <- filter_radius(cl, radius_km = 20, interactive = FALSE)
cat("presences: ecoregion", nrow(pr_eco), "| checklists within 20 km:", nrow(cl_rad), "\n")

suppressMessages({
  pr_seg <- filter_pol(pr_eco, seg_path, "COMMON NAME", file.path(out, "pr_seg.csv"))
  cl_seg <- filter_pol(cl_rad, seg_path, "COMMON NAME", file.path(out, "cl_seg.csv"))
})
print(table(as.data.frame(sf::st_drop_geometry(pr_seg))$Segment))
pr_s <- add_season(split_dates(as.data.frame(sf::st_drop_geometry(pr_seg)), preview = FALSE))
cl_s <- add_season(split_dates(as.data.frame(sf::st_drop_geometry(cl_seg)), preview = FALSE))
saveRDS(list(pr = pr_s, cl = cl_s, segs = segs, eco = eco), file.path(out, "step2.rds"))
cat("seasons (presences):\n"); print(table(pr_s$season, pr_s$Segment))
