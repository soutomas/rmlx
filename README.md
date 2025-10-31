
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmlx

<!-- badges: start -->

[![R-CMD-check](https://github.com/soutomas/rmlx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/soutomas/rmlx/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of rmlx is to facilitate the Modelling modelling workflow.

## Citation

Sou T (2025). *rmlx: Convenient Functions for Monolix Modelling*. R
package version 0.0.0.9000, <https://github.com/soutomas/rmlx>.

``` r
citation("rmlx")
#> To cite package 'rmlx' in publications use:
#> 
#>   Sou T (2025). _rmlx: Convenient Functions for Monolix Modelling_. R
#>   package version 0.0.0.9000, commit
#>   9cb7d6b92bab2f990eb8a42e6fec6a9b1f4f2ea6,
#>   <https://github.com/soutomas/rmlx>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {rmlx: Convenient Functions for Monolix Modelling},
#>     author = {Tomas Sou},
#>     year = {2025},
#>     note = {R package version 0.0.0.9000, commit 9cb7d6b92bab2f990eb8a42e6fec6a9b1f4f2ea6},
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

To get a list of all Monolix model files in a directory:

``` r
library(rmlx)

# Get all model files in the current directory 
get_mlx(".")
```
