
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
```

You can load the package with:

``` r
library(auctime)
```

## Methods

Several methods are included in calcAUC to find the area under the
curve. All methods use the trapezoidal rule to interpolate between data
points.

-   `positive` sums area that is above the baseline (first) measurement,
    ignoring any area below the baseline (Wolever & Jenkins, 1986)

-   `net` subtracts the area below baseline from the area above baseline
    (Le Floch et al., 1990)

-   `total` is the area with respect to ground (a baseline of 0)

## Example

Data should be labeled with the biomarker name and timepoint in the
format `Biomarker_Time`. Each row is a subject’s measurements. For
example:

``` r
set.seed(1234)

# simulate some data
measurements <- data.frame(Biomarker_0 = rnorm(10,50,20),
                           Biomarker_1 = rnorm(10,70,20),
                           Biomarker_2 = rnorm(10,90,20),
                           Biomarker_3 = rnorm(10,90,20),
                           Biomarker_4 = rnorm(10,70,20),
                           Biomarker_5 = rnorm(10,60,20))

# view simulated data
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
#> Removed 0 subject(s) with less than two available measurements.
```

`calcAUC()` produces output containing input data and arguments,
subject-wise calculations and plots, a data frame of calculations, and a
grid of plots for all subjects:

``` r
# input data
output$input
#> $method
#> [1] "positive"
#> 
#> $biomarker
#> [1] "Biomarker"
#> 
#> $subjects
#> [1] 10
#> 
#> $removed
#> [1] 0
#> 
#> $timepoints
#> [1] 0 1 2 3 4 5
#> 
#> $interval
#> [1] "hours"
#> 
#> $data
#>    Subject Biomarker_0 Biomarker_1 Biomarker_2 Biomarker_3 Biomarker_4
#> 1        1   25.858685    60.45615    92.68176   112.04595    98.98993
#> 2        2   55.548585    50.03227    80.18628    80.48814    48.62715
#> 3        3   71.688824    54.47492    81.18904    75.81120    52.89271
#> 4        4    3.086046    71.28918    99.19179    79.97484    64.38754
#> 5        5   58.582494    89.18988    76.12560    57.41813    50.11320
#> 6        6   60.121118    67.79429    61.03590    66.64761    50.62971
#> 7        7   38.505201    59.77981   101.49511    46.39921    47.85364
#> 8        8   39.067363    51.77609    69.52689    63.18014    44.96028
#> 9        9   38.710960    53.25657    89.69723    84.11412    59.52344
#> 10      10   32.199243   118.31670    71.28103    80.68205    60.06300
#>    Biomarker_5
#> 1     23.87937
#> 2     48.35848
#> 3     37.82221
#> 4     39.70076
#> 5     56.75381
#> 6     71.26112
#> 7     92.95635
#> 8     44.53293
#> 9     92.11819
#> 10    36.84383

# output for subject 1
output$subjects[[1]]
#> $AUC
#> [1] 233.4681
#> 
#> $intervalAUC
#> [1] 47.52680 34.00450 54.95836 61.37644 35.60204
#> 
#> $plot
```

<img src="man/figures/README-example output-1.png" width="100%" />

``` r
# output data
output$dataframe
#>    Subject Biomarker_AUC
#> 1        1    233.468147
#> 2        2     38.379860
#> 3        3      7.840711
#> 4        4    288.723419
#> 5        5     87.813708
#> 6        6     48.225181
#> 7        7    124.951568
#> 8        8     85.312078
#> 9        9    142.427134
#> 10      10    233.008219

# plot grid
output$multiplot
```

<img src="man/figures/README-example output-2.png" width="100%" />
