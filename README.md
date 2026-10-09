<p align="center"><img src="assets/banner.png" width="520" alt="BioDyn"></p>

# BioDyn – looking at where and when a species is recorded, before modelling it

**BioDyn** is an R package for exploring *occurrence records* of a species (for example bird sightings from eBird) across **space, seasons and environment**. It is meant for the step *before* any model: checking what the data actually contain, how unevenly people sampled, and whether different parts of a population behave differently over the year.

It is described in the article *"BioDyn: A tool for the spatiotemporal analysis of intra-specific geographic data"* (Herrador, Reati, Huais, Osorio-Olvera, Contreras-Díaz, Peluc & Nori, *Biodiversity Informatics* 20, 2026, 275–285), and the package files are archived on Zenodo ([doi:10.5281/zenodo.19055001](https://doi.org/10.5281/zenodo.19055001)).

## Why it exists, and what it is for

Citizen-science platforms hold millions of records, but they are uneven: some places and months are visited far more than others. A map of points can then show *where observers were* instead of *where the species is*. This gets worse when you care about **movements**, such as a migratory species whose northern and southern populations are in different places in different seasons.

BioDyn puts the usual steps for this in one place, so they are reproducible and easy to repeat on another species:

- build a **presence/absence table** from eBird, so that an *absence* means "someone looked and did not see it", not "nobody went there";
- **filter the records by sampling effort** (distance, duration, number of observers, protocol);
- **cut the area** with an ecoregion, a radius around the presences, or **segments you draw yourself** on a map;
- add the **season** (aware of the hemisphere) and **environmental values** (climate, elevation) at each record;
- draw the **diagnostic plots**: sampling effort along latitude, records per month, climate and elevation by season.

It is a *diagnostic* tool. It does not fit models and it does not prove anything about migration: it helps you see the pattern, and see whether sampling effort could be fooling you.

## The workflow

The steps do not have to be run in order, and every function takes and returns a plain table, so you can stop, branch or repeat anywhere. The diagram shows the whole workflow, from the article. It has two paths: if you only have presence records (path A) the data go straight to the spatial filters; if you have checklists (path B) `build_data()` first builds the presence/absence table from the eBird files, which lets the package tell a real absence from a place nobody visited. Dotted boxes are optional steps. Everything works with presences only, except `build_data()` and `plot_effort()`, which need the absences.

<p align="center"><img src="assets/paper_fig1_workflow.png" width="560" alt="Diagram of the BioDyn workflow, from the article"></p>

## An example: a migratory bird in Argentina

The article uses the chocolate-vented tyrant (*Neoxolmis rufiventris*), which breeds in Patagonia in the austral summer and winters in the Pampas. All the figures below are from the article (Herrador et al. 2026, *Biodiversity Informatics* 20, 275–285) and were made with BioDyn.

**Cleaning and cutting the data.** From the eBird files (3,329 presence records and 1,010,756 checklists for Argentina), the effort filter (≤ 10 km, ≤ 10 observers, ≤ 300 min, stationary or travelling) keeps **814 records and 280,958 checklists**; I re-ran it with this version of the package and got the same numbers. Then the records are cut by ecoregion and by distance to the presences, and split into three segments (north, centre, south) drawn on an interactive map:

(A) `filter_ecoregion()` keeps the records inside the chosen ecoregions. (B) `filter_radius()` keeps the checklists within 20 km of a presence. (C) The three segments drawn with `create_segments()`. (D) The number of records in each segment, from `filter_pol()`.

<img src="assets/paper_fig2_filtering.png" width="760" alt="Filtering and segmentation of the records: ecoregion, radius, segments and records per segment">

**Looking at seasons, elevation and climate.** (A) `plot_effort()`: for each degree of latitude, the share of the checklists (left) and of the presences (right) that fall in each season. (B) Elevation of the records by season in each segment; the grey lines are the lowest and highest elevation available in each segment, from `elev_minmax()`. (C) Latitude of the records in summer and winter. (D) Annual mean temperature (CHELSA `bio01`) of the records in summer and winter. (E) Records per month in each segment. (F) Latitude of the records by season in each segment.

<img src="assets/paper_fig3_exploration.png" width="760" alt="Seasonal patterns of the records: effort, elevation, latitude, temperature and months">

**What it shows.** In panel A (left) every season is represented at most latitudes, so sampling effort is fairly even through the year. In panel A (right) the northern degrees are mostly autumn and winter records and the southern ones spring and summer. Since the checklists did not themselves shift with the seasons, that change is more likely to be the bird than the observers. This is exactly the check `plot_effort()` is for. Summer and winter records also hardly overlap in temperature (D), but, as the article warns, this mostly reflects that the bird is at different latitudes in each season, not that it chooses different climates.

## Install

BioDyn is distributed from Zenodo ([doi:10.5281/zenodo.19055001](https://doi.org/10.5281/zenodo.19055001)). Download the source package `BioDyn_0.1.0.tar.gz` from there and install it from R:

```r
install.packages("path/to/BioDyn_0.1.0.tar.gz", repos = NULL, type = "source")
```

The same Zenodo record has the example dataset of the article (`Example_nrufiventris.rar`).

R ≥ 4.1. It uses `sf`, `terra`, `ggplot2`, `dplyr` and `leaflet`. `elev_minmax()` also needs `raster` and `exactextractr`, and the interactive maps need `plotly` and `htmlwidgets`.

## Use

```r
library(BioDyn)

presence <- build_data(sightings_file = "occurrences.txt",
                       checklist_file = "checklists.txt", sep = "\t")

checklists <- filter_ebird(presence, max_distance_km = 10, max_observers = 10,
                           max_duration_min = 300,
                           protocols = c("Stationary", "Traveling"))

near <- filter_radius(checklists, radius_km = 20, interactive = FALSE)

seasonal <- add_season(split_dates(near))
plot_effort(seasonal)
```

The full walk-through, from raw eBird files to every plot, is the vignette: `vignette("flow_example", package = "BioDyn")`. The script [`inst/scripts/step1_filters.R`](inst/scripts/step1_filters.R) reproduces the record counts of the example above.

## Data

The eBird files are **not in this repository**. eBird's terms of use do not allow its data to be redistributed in their original form: request the *Basic Dataset* and the *sampling* file for your species and region at <https://ebird.org/data/download>. The example files of the article are on [Zenodo](https://doi.org/10.5281/zenodo.19055001). Climate layers (CHELSA) and elevation models come from their own providers.

The tests use tiny hand-made tables and need no real data.

## Things to keep in mind

- **It only works with occurrence data.** There is no abundance, movement or genetic information, so conclusions about populations or migratory connections are indirect.
- **Drawing segments is a judgement call.** That is the point (you bring your knowledge of the species), but two people can draw different segments.
- **`build_data()` treats every row of the sightings file as a detection of your species.** Give it a sightings file for one species only.
- **The effort filter is as good as its thresholds.** The defaults follow Johnston et al. (2021) but suit some questions better than others.
- **`filter_ebird()` removes duplicates by date and latitude.** That is quick, but two different checklists on the same day at the same latitude count as duplicates.
- **`plot_effort()` only looks at latitude.**
- **With recent versions of `ggplot2`, `plot_variable()` fails for `type = "line"` and for `type = "boxplot"` with seasons**; `"density"` and `"histogram"` work.
- This is version 0.1.0 of a young package.

## What this repository adds to the archived version

The package code (`R/`, `man/`, `NAMESPACE`) is exactly the one archived on Zenodo. What this repository adds around it: tests (with tiny hand-made tables, no real data), continuous integration, a script that reproduces the counts of the example, this README, the authors' details and the MIT licence in `DESCRIPTION`.

## Authors and licence

Tiago A. Herrador and Carla A. Reati wrote the package; the article lists all authors. MIT licensed. If you use BioDyn, please cite the article above and the eBird data you used.
