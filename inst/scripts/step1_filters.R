# Steps 1-3b of the BioDyn workflow on the Neoxolmis rufiventris example
# (counts to compare with the article). Usage:
#   Rscript step1_filters.R <folder-with-example-data> <output-folder>
args <- commandArgs(trailingOnly = TRUE)
dd <- args[1]; out <- args[2]; dir.create(out, showWarnings = FALSE, recursive = TRUE)
suppressPackageStartupMessages(library(BioDyn))
grDevices::pdf(NULL)

occ  <- file.path(dd, "ebd_AR_chvtyr2_smp_relDec-2025.txt")
chk  <- file.path(dd, "ebd_AR_chvtyr2_smp_relDec-2025_sampling.txt")
eco  <- file.path(dd, "Ecoreg_monj.shp")

data_raw <- data.table::fread(occ, sep = "\t", encoding = "UTF-8")
checklists_raw <- build_data(occ, chk, pres_col = "presence",
                             label_pos = "Present", label_neg = "Absent", sep = "\t")
cat("RAW presences:", nrow(data_raw), " checklists:", nrow(checklists_raw),
    " present:", sum(checklists_raw$presence == "Present"), "\n")

data_ebird <- filter_ebird(data_raw, max_distance_km = 10, max_observers = 10,
                           max_duration_min = 300, protocols = c("Stationary", "Traveling"))
checklists_ebird <- filter_ebird(checklists_raw, max_distance_km = 10, max_observers = 10,
                                 max_duration_min = 300, protocols = c("Stationary", "Traveling"))
cat("EBIRD presences:", nrow(data_ebird), " checklists:", nrow(checklists_ebird), "\n")

data_eco <- filter_ecoregion(data_ebird, eco_path = eco, pres_col = "presence",
                             pres_label = "Present", buffer_km = 0, interactive = FALSE,
                             save_path = file.path(out, "fig_ecoregion.png"))
cat("ECOREGION presences kept:", nrow(data_eco), " removed:", nrow(data_ebird) - nrow(data_eco), "\n")
chk_eco <- filter_ecoregion(checklists_ebird, eco_path = eco, pres_col = "presence",
                            pres_label = "Present", buffer_km = 0, interactive = FALSE)
cat("ECOREGION checklists kept:", nrow(chk_eco), " removed:", nrow(checklists_ebird) - nrow(chk_eco), "\n")

for (r in c(20, 30)) {
  rad <- filter_radius(checklists_ebird, radius_km = r, interactive = FALSE,
                       save_path = file.path(out, sprintf("fig_radius_%d.png", r)))
  cat("RADIUS", r, "km: kept", nrow(rad), "of", nrow(checklists_ebird), "\n")
}
saveRDS(list(data_eco = data_eco, chk_eco = chk_eco), file.path(out, "step1.rds"))
