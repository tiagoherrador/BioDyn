<p align="center"><img src="assets/banner.png" width="520" alt="BioDyn"></p>

# BioDyn – looking at where and when a species is recorded, before modelling it

**BioDyn** is an R package for exploring *occurrence records* of a species (for example bird sightings from eBird) across **space, seasons and environment**. It is meant for the step *before* any model: checking what the data actually contain, how unevenly people sampled, and whether different parts of a population behave differently over the year.

It is described in the article *"BioDyn: A tool for the spatiotemporal analysis of intra-specific geographic data"* (Herrador, Reati, Huais, Osorio-Olvera, Contreras-Díaz, Peluc & Nori, *Biodiversity Informatics* 20, 2026, 275–285), and the package files are archived on Zenodo ([doi:10.5281/zenodo.19055001](https://doi.org/10.5281/zenodo.19055001)).

![Presence records of the chocolate-vented tyrant, by season, in three user-defined segments](assets/map_segments.png)

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

| Step | Functions | What it does |
|---|---|---|
| 1. Presence/absence | `build_data()` | Crosses eBird sightings with the checklist file: a checklist with the species is `Present`, the rest are `Absent` |
| 2. Effort filter | `filter_ebird()` | Keeps checklists within a maximum distance, number of observers and duration, for chosen protocols, complete checklists only, no duplicates |
| 3. Spatial filter and segments | `filter_ecoregion()`, `filter_radius()`, `create_segments()`, `filter_pol()` | Keep points inside an ecoregion (with optional buffer); keep absences within X km of a presence; draw polygons on an interactive map and assign each record to a segment |
| 4. Seasons | `split_dates()`, `add_season()`, `filter_season()` | Day / month / year, and a season that depends on the hemisphere of each record |
| 5. Environment | `extract_rasters()`, `elev_minmax()` | Raster values (e.g. CHELSA climate, a DEM) at each point; minimum and maximum elevation available inside each segment |
| 6. Plots | `plot_effort()`, `plot_variable()`, `plot_altitude()`, `create_ggplot_visualizations()`, `create_segment_interactive_maps()` | The diagnostic plots |

The steps do not have to be run in order, and every function takes and returns a plain table, so you can stop, branch or repeat anywhere. If you only have presence records (no checklists), everything works except `build_data()` and `plot_effort()`, which need the absences.

## An example: a migratory bird in Argentina

The article uses the chocolate-vented tyrant (*Neoxolmis rufiventris*), which breeds in Patagonia in the austral summer and winters in the Pampas. I re-ran the whole workflow on that example with this version of the package.

**The numbers of the article are reproduced.** From the eBird files (3,329 presence records and 1,010,756 checklists for Argentina), the effort filter (≤ 10 km, ≤ 10 observers, ≤ 300 min, stationary or travelling) keeps **814 records and 280,958 checklists**, the same as in the article.

**Effort and presences along latitude.** Each bar is one degree of latitude and shows what share of the records fell in each season:

![Sampling effort and presences by latitude and season](assets/effort_by_latitude.png)

On the left (all checklists) every season is represented at almost every latitude, so effort is fairly even through the year. On the right (checklists with the species), the northern degrees are mostly autumn and winter and the southern ones spring and summer. Since sampling did not itself shift with the seasons, that change is more likely to be the bird than the observers. This is exactly the check `plot_effort()` is for.

**Records per month in each segment:**

![Share of presences by month in each segment](assets/presences_by_month.png)

**Climate:** annual mean temperature (CHELSA `bio01`) at the winter and summer records hardly overlaps, which agrees with the article, but, as the article warns, this mostly reflects that the bird is at different latitudes in each season, not that it picks different climates.

![Annual mean temperature of summer and winter records](assets/temperature_by_season.png)

### What I could not reproduce exactly

- **Ecoregion filter.** The article reports 69 presence records removed; with the two ecoregions drawn in its Fig. 2A (Patagonian steppe and Humid Pampas) I get **73**. The selection of ecoregions used for the article is not in my files, so a small difference is expected.
- **Segments.** The three segments in the article were drawn by hand on the interactive map and I do not have that file. I drew three rectangles by eye from the article's figure; they give 131 / 16 / 592 records (the article's figure shows about 140 / 12 / 595). The figures in this README use those, so treat them as an illustration.
- **The radius.** The article says 20 km and its example vignette uses 30 km; I used 20 km.

## Install

```r
# install.packages("remotes")
remotes::install_github("tiagoherrador/BioDyn")
```

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

The full walk-through, from raw eBird files to every plot, is the vignette: `vignette("flow_example", package = "BioDyn")`. The scripts in [`inst/scripts`](inst/scripts) are the ones I used for the numbers and figures above.

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
- This is version 0.1.0 of a young package.

## What I changed from the archived version

The code is the version archived on Zenodo, with these differences: `split_dates()` now turns an unreadable date into `NA` (it failed with an error before, although its documentation promised `NA`); `tidyr` is no longer listed as a dependency, since nothing used it; and the package now has tests, continuous integration, an MIT licence and the authors' details, which the archived `DESCRIPTION` still had as placeholders.

## Authors and licence

Tiago A. Herrador and Carla A. Reati wrote the package; the article lists all authors. MIT licensed. If you use BioDyn, please cite the article above and the eBird data you used.
