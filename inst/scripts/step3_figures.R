# Figures for the README, from the objects saved by step2_segments_seasons.R.
# Usage: Rscript step3_figures.R <step2.rds> <assets-folder> [CHELSA_bio1.tif]
args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages({library(BioDyn); library(sf); library(ggplot2); library(dplyr)})
grDevices::pdf(NULL)
x <- readRDS(args[1]); out <- args[2]; dir.create(out, showWarnings = FALSE, recursive = TRUE)
seasons <- c("Spring", "Summer", "Autumn", "Winter")
pr <- x$pr %>% mutate(season = factor(season, levels = seasons))

# 1. Where the records are: ecoregions, the three segments and the presences
sf_use_s2(FALSE)
pts <- st_as_sf(pr, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)
p1 <- ggplot() +
  geom_sf(data = x$eco, fill = "grey95", colour = "grey60", linewidth = 0.2) +
  geom_sf(data = x$segs, fill = NA, colour = "#1f4e79", linewidth = 0.7) +
  geom_sf(data = pts, aes(colour = season), size = 1.3, alpha = 0.8) +
  geom_sf_text(data = x$segs, aes(label = paste("Segment", id)),
               nudge_x = c(-4, -5, 4), nudge_y = c(5.5, 2.5, -8), size = 3, colour = "#1f4e79") +
  scale_colour_manual(values = season_palette, name = "Season") +
  coord_sf(xlim = c(-76, -53), ylim = c(-58, -29)) +
  labs(title = "Presences by season and segment", x = NULL, y = NULL) +
  theme_minimal(base_size = 11)
ggsave(file.path(out, "map_segments.png"), p1, width = 5, height = 6.5, dpi = 200, bg = "white")

# 2. Sampling effort vs presences along latitude (plot_effort)
p2 <- plot_effort(x$cl %>% mutate(presence = presence))
ggsave(file.path(out, "effort_by_latitude.png"), p2, width = 9, height = 7, dpi = 200, bg = "white")

# 3. Share of each segment's presences falling in each month
months <- pr %>% mutate(MONTH = factor(MONTH, levels = 1:12, labels = month.abb)) %>%
  count(Segment, MONTH, season) %>% group_by(Segment) %>% mutate(prop = n / sum(n)) %>% ungroup()
p3 <- ggplot(months, aes(MONTH, prop, fill = season)) +
  geom_col(colour = "black", linewidth = 0.2, alpha = 0.85) +
  facet_wrap(~Segment, nrow = 1, labeller = labeller(Segment = function(s) paste("Segment", s))) +
  scale_fill_manual(values = season_palette, name = "Season") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Share of the segment's presences") + theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(file.path(out, "presences_by_month.png"), p3, width = 9, height = 3.6, dpi = 200, bg = "white")

# 4. (optional) annual mean temperature (CHELSA bio01) of summer vs winter presences
if (length(args) >= 3) {
  en <- suppressMessages(extract_rasters(as.data.frame(pr), c(bio01 = args[3])))
  en <- en %>% filter(season %in% c("Summer", "Winter"))
  p4 <- ggplot(en, aes(bio01, fill = season, colour = season)) +
    geom_density(alpha = 0.55) +
    scale_fill_manual(values = season_palette) + scale_colour_manual(values = season_palette) +
    labs(x = "Annual mean temperature (CHELSA bio01, \u00b0C)", y = "Density", fill = "Season", colour = "Season") +
    theme_minimal(base_size = 11)
  ggsave(file.path(out, "temperature_by_season.png"), p4, width = 6.5, height = 3.6, dpi = 200, bg = "white")
}
cat("done\n")
