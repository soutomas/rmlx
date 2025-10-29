
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmlx

<!-- badges: start -->

[![R-CMD-check](https://github.com/soutomas/rmlx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/soutomas/rmlx/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of rmlx is to facilitate the processing of Monolix results.

## Citation

Sou T (2025). *rmlx: Convenient Functions for Processing of Monolix
Results*. R package version 0.0.0.9000,
<https://github.com/soutomas/rmlx>.

``` r
citation("rmlx")
#> To cite package 'rmlx' in publications use:
#> 
#>   Sou T (2025). _rmlx: Convenient Functions for Processing of Monolix
#>   Results_. R package version 0.0.0.9000, commit
#>   871d3c8149d64e5b331258b56cc68b48ffb7ce03,
#>   <https://github.com/soutomas/rmlx>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {rmlx: Convenient Functions for Processing of Monolix Results},
#>     author = {Tomas Sou},
#>     year = {2025},
#>     note = {R package version 0.0.0.9000, commit 871d3c8149d64e5b331258b56cc68b48ffb7ce03},
#>     url = {https://github.com/soutomas/rmlx},
#>   }
```

## Installation

You can install the development version of rmlx from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("soutomas/rmlx")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rmlx)

# Get all model files in the current directory 
get_mlx(".")
```
