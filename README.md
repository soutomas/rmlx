
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmlx

<!-- badges: start -->

[![R-CMD-check](https://github.com/soutomas/rmlx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/soutomas/rmlx/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of rmlx is to facilitate modelling workflow with Monolix.

## Citation

Sou T (2025). *rmlx: Convenient Functions for Monolix Modelling*. R
package version 0.0.0.9000, <https://github.com/soutomas/rmlx>.

## Installation

You can install the development version of rmlx from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("soutomas/rmlx")
```

## Example

To get a list of all Monolix model files in a directory:

``` r
library(rmlx)

# Get all model files in the current directory 
get_mlx(".")
```
