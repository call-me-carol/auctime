
<!-- README.md is generated from README.Rmd. Please edit that file -->

# auctime

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/auctime)](https://CRAN.R-project.org/package=auctime)
<!-- badges: end -->

The goal of `auctime` is to provide an easy and reproducible way of
calculating incremental area under the curve calculations (iAUC) for
biomarkers which have measurements taken over at intervals over time.
This quantity represents the effect of a treatment on the total
concentration of a biomarker.

This is different from the AUC that results from calculation of area
under a receiver operating characteristic (ROC) curve, which represents
the accuracy of binary classifier.

## Installation

You can install the `auctime` package from GitHub with:

``` r
devtools::install_github("scrs-msu/auctime")

library(auctime)
```

## Methods

Several methods are included in calcAUC to find the area under the
curve. All methods of calculation use the trapezoidal rule to
interpolate between data points.

-   `positive` sums area that is above the baseline (first) measurement,
    ignoring any area below the baseline (Wolever & Jenkins, 1986)

-   `net` subtracts the area below baseline from the area above baseline
    (Le Floch et al., 1990)

-   `total` is the area with respect to ground (a baseline of 0)

## Example

Data should be labeled with the biomarker name and timepoint in the
format `Biomarker_Time`, with subjects’ measurements in rows:

``` r
set.seed(1234)

measurements <- data.frame(Biomarker_0 = rnorm(10,50,20),
                           Biomarker_1 = rnorm(10,70,20),
                           Biomarker_2 = rnorm(10,90,20),
                           Biomarker_3 = rnorm(10,90,20),
                           Biomarker_4 = rnorm(10,70,20),
                           Biomarker_5 = rnorm(10,60,20))

head(measurements)
#>   Biomarker_0 Biomarker_1 Biomarker_2 Biomarker_3 Biomarker_4 Biomarker_5
#> 1   25.858685    60.45615    92.68176   112.04595    98.98993    23.87937
#> 2   55.548585    50.03227    80.18628    80.48814    48.62715    48.35848
#> 3   71.688824    54.47492    81.18904    75.81120    52.89271    37.82221
#> 4    3.086046    71.28918    99.19179    79.97484    64.38754    39.70076
#> 5   58.582494    89.18988    76.12560    57.41813    50.11320    56.75381
#> 6   60.121118    67.79429    61.03590    66.64761    50.62971    71.26112
```

Use `calcAUC()` to calculate area under the curve:

``` r
output <- calcAUC(data = measurements)
```

`calcAUC()` produces output containing input data and arguments,
subject-wise calculations and plots, a data frame of calculations, and a
plot grid of all subjects:

``` r
output$input

output$subjects

output$dataframe

output$multiplot
```
