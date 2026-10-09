# Examples of plot_variable() on the segmented presences saved by
# step2_figures.R (pr_seg.csv). Every image is drawn by BioDyn.
# Usage: Rscript step3_plot_variable.R <step2-out-folder> <CHELSA_bio1.tif> <assets>
args <- commandArgs(trailingOnly = TRUE)
out <- args[1]; bio1 <- args[2]; assets <- args[3]
suppressPackageStartupMessages({library(BioDyn); library(ggplot2)})
dir.create(assets, showWarnings = FALSE, recursive = TRUE)

pr <- read.csv(file.path(out, "pr_seg.csv"), check.names = FALSE)
pr$Segment <- as.character(pr$Segment)
pr <- add_season(split_dates(pr, preview = FALSE))
pr <- suppressMessages(extract_rasters(pr, c(bio01 = bio1)))
seas <- c("Summer", "Winter")
save <- function(p, f, w = 5, h = 3.4) ggsave(file.path(assets, f), p, width = w, height = h, dpi = 200, bg = "white")

# Two plot types, all records, summer and winter only
# (type = "line" and type = "boxplot" fail with current ggplot2 versions, so they are left out)
for (tp in c("density", "histogram")) {
  save(plot_variable(pr, "bio01", type = tp, seasons = seas,
                     x_title = "Annual mean temperature (°C)"),
       sprintf("pv_bio01_%s.png", tp))
}

# Another variable: latitude of the records, by season
save(plot_variable(pr, "LATITUDE", type = "histogram"), "pv_latitude_histogram.png")

# One plot per segment (segment = TRUE draws one plot for each segment)
png(file.path(assets, "pv_bio01_segment_%d.png"), width = 1000, height = 700, res = 200)
plot_variable(pr, "bio01", segment = TRUE, type = "density", seasons = seas,
              x_title = "Annual mean temperature (°C)")
dev.off()
cat("done\n")
