
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Implementation of the InSiGHTS framework

<!-- badges: start -->

[![R-CMD-check](https://github.com/iiasa/ibis.insights/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/iiasa/insights/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: CC BY
4.0](https://img.shields.io/badge/license-CC%20BY%204.0-blue.svg)](https://creativecommons.org/licenses/by/4.0/)
[![Codecov
Status](https://codecov.io/gh/iiasa/ibis.insights/branch/main/graph/badge.svg)](https://app.codecov.io/gh/iiasa/ibis.insights?branch=main)<!-- badges: end -->

This R-package provides a IIASA implementation of the InSiGHTS Index of
Habitat Availability. The index captures the amount of suitable habitat
within the current or a future range of a species. This range can be
taken either from existing range maps (e.g. IUCN) or from estimates
obtained through species distribution models.

<img src="man/figures/insights_schematic.png" alt="Schematic" align="right" width="300"/>

In it’s basic configuration, the InSiGHTS framework combines the
climatic suitability from a SDM with a area of habitat (AOH) refinement
to obtain the suitable habitat for each time steps. The InSiGHTS Index
of Habitat Availability can then be defined for any given species $s$
and timestep $t$ as:

$Insights_{s,t} = \frac{AOH_{s,t} - AOH_{s, t_{ref}}}{AOH_{s, t_{ref}}}$,
where $t_{ref}$ indicates a reference or starting year.

More information on the InSiGHTS framework can be found in [Pearson et
al. 2004](https://doi.org/10.1046/j.1466-822X.2003.00042.x), [Rondini
and Visconti 2015](https://doi.org/10.1111/cobi.12532), [Visconti et
al. 2016](https://doi.org/10.1111/conl.12159) or [Baisero et
al. (2021)](https://doi.org/10.1016/j.oneear.2020.05.015).

The package is part of the
[IIASA-BEC](https://iiasa.ac.at/programs/bnr/bec) suite of biodiversity
indicators and is coupled with the
[ibis.iSDM](https://iiasa.github.io/ibis.iSDM/) species distribution
model.

## Installation

You can install the development version of Insights from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("iiasa/ibis.insights")
```

The package depends on the
[ibis.iSDM](https://iiasa.github.io/ibis.iSDM/) package, which is
currently only available via github.

## Basic usage and examples

``` r
# Basic packages for use
library(ibis.iSDM)
library(insights)
library(glmnet)
#> Warning: package 'Matrix' was built under R version 4.5.3
library(terra)
#> Warning: package 'terra' was built under R version 4.5.3
```

Now we use the **ibis.iSDM** package to train a simple SDM and apply the
InSiGHTS on it. The assumption here is that the SDMs are created using
climatic variables (temperature, precipitation, etc.) only so as to
create a climatic envelope model. The refinement with time series of
land-use is done posthoc on the resulting prediction. Note that this
also works on any other range estimate provided directly as a
SpatRaster.

``` r
# Load test data from ibis.iSDM package
background <- terra::rast(system.file('extdata/europegrid_50km.tif', package='ibis.iSDM',mustWork = TRUE))
virtual_points <- sf::st_read(system.file('extdata/input_data.gpkg', package='ibis.iSDM',mustWork = TRUE),'points',quiet = TRUE)
# Get some future predictors
ll <- list.files(system.file('extdata/predictors_presfuture/',package = "ibis.iSDM",mustWork = TRUE),
                 full.names = T)
# Load the same files future ones
suppressWarnings(
  pred_future <- stars::read_stars(ll) |> stars:::slice.stars('Time', seq(1, 86, by = 10))
)
sf::st_crs(pred_future) <- sf::st_crs(4326)

# Get only climatic predictors and take the first time slot
pred_climate <- pred_future |> stars:::select.stars(bio01, bio12)
predictors <- ibis.iSDM:::stars_to_raster(pred_climate, 1)[[1]]

# Add some pseudo-absence data
virtual_points <- ibis.iSDM::add_pseudoabsence(virtual_points,
                                               field_occurrence = 'Observed',
                                               template = background)

# Now train a small little model
fit <- distribution(background) |> # Prediction domain
  add_biodiversity_poipa(virtual_points,field_occurrence = 'Observed') |> # Add presence-only point data
  add_predictors(predictors,transform = 'scale') |> # Add simple predictors
  engine_glmnet() |> # Use glmnet for estimation
  train(verbose = FALSE) |> # Train the model 
  threshold(method = "perc", value = .33) # Percentile threshold

# --- #
# Now load some fractional land-use layers relevant for the species
# Here we assume the species only occurs in Grassland and Sparse vegetation
lu <- c(
  terra::rast(system.file('extdata/Grassland.tif', package='insights',mustWork = TRUE)),
  terra::rast(system.file('extdata/Sparsely.vegetated.areas.tif', package='insights',mustWork = TRUE))
) / 10000

# Summarize 
out <- insights_fraction(range = fit,lu = lu)

plot(out, col = c("grey90", "#FDE8A9", "#FBD35C", "#D1C34A", "#8EB65C",
                  "#56AA71", "#59A498", "#5C9EBF", "#5C8BAE", "#597182"),
     main = "Suitable habitat")
```

<img src="man/figures/README-Train a simple SDM-1.png" alt="" width="100%" />

``` r

# Summarize
insights_summary(out)
#>   time suitability unit
#> 1   NA    257678.9  km2
```

Of course it is also possible to directly supply a multi-dimensional
gridded file using the *stars* package or directly through the ibis.iSDM
scenario functionalities (see example below).

``` r

# Create a future scenario
sc <- scenario(fit) |>
  add_predictors(env = pred_climate, transform = 'scale', derivates = "none") |>
  threshold() |>
  project()
#> ! State variable of transformation not found?
#> [32m[Scenario] 2026-04-11 23:11:28.85695 | Adding scenario predictors...[39m
#> [32m[Setup] 2026-04-11 23:11:28.857741 | Transforming predictors...[39m
#> [32m[Scenario] 2026-04-11 23:11:29.563382 | Starting suitability projections for 9 timesteps from 2015-01-01 <> 2095-01-01[39m

# --- #
# Now apply insights using time series of future land use
lu <- pred_future |> stars:::select.stars(primn, secdf)
# Normalize for the sake of an example. Note that fractions are needed!
lu <- ibis.iSDM::predictor_transform(lu, "norm") |> round(2) 
#> [31m[Setup] 2026-04-11 23:11:30.483115 | When transforming future variables, ensure that unit ranges are comparable (parameter state)![39m
out <- insights_fraction(range = sc,
                         lu = lu)

# Summarize
o <- insights_summary(out)
#> Warning: package 'sf' was built under R version 4.5.2
#> Linking to GEOS 3.13.1, GDAL 3.11.4, PROJ 9.7.0; sf_use_s2() is FALSE

plot(o$suitability~o$band, type = "b",
     main = "InSiGHTS index",
     ylab = "Suitable habitat relative to 2015",
     xlab = "Year")
```

<img src="man/figures/README-With scenario-1.png" alt="" width="100%" />

## Citations

See \[CITATION.cff\] file for the recommended citation of this package.
For the InSiGHTS framework, please cite:

P. Visconti, M. Bakkenes, D. Baisero, T. Brooks, S.H.M. Butchart, L.
Joppa, R. Alkemade, M. Di Marco, L. Santini, M. Hoffmann, C. Rondinini
*Projecting global biodiversity indicators under future development
scenarios* Conserv. Lett., 9 (2016), pp. 5-13
[DOI](https://doi.org/10.1111/conl.12159)

C. Rondinini and P. Visconti, *Scenarios of large mammal loss in Europe
for the 21st century* Conserv. Biol., 29 (2015), pp. 1028-1036
[DOI](https://doi.org/10.1111/cobi.12532)

## Acknowledgement <a href="https://iiasa.ac.at"><img src="man/figures/IIASA_PNG_logo_blue.png" alt="IIASA" align="right" width="300"/></a>

**InSiGHTS** is developed and maintained by the [Biodiversity, Ecology
and Conservation group](https://iiasa.ac.at/programs/bnr/bec) at the
International Institute for Applied Systems Analysis (IIASA), Austria.
