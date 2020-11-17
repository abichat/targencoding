
<!-- README.md is generated from README.Rmd. Please edit that file -->

# targencoding

<!-- badges: start -->

[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![packageversion](https://img.shields.io/badge/package%20version-0.0.0.9000-orange.svg)](commits/master)
<!-- badges: end -->

**targencoding** contain extra steps for the
[recipes](https://recipes.tidymodels.org) package for using target
encoding (or effect or likelihood encoding) for categorical variables
during preprocessing.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("abichat/targencoding")
```

## Target encoding by the mean

`step_encode_mean()` will replace the levels of a categorical variable
by the means of the outcome variable for each level.

``` r
library(recipes)
library(targencoding)
data("penguins", package = "modeldata")
penguins
#> # A tibble: 344 x 7
#>    species island bill_length_mm bill_depth_mm flipper_length_… body_mass_g
#>    <fct>   <fct>           <dbl>         <dbl>            <int>       <int>
#>  1 Adelie  Torge…           39.1          18.7              181        3750
#>  2 Adelie  Torge…           39.5          17.4              186        3800
#>  3 Adelie  Torge…           40.3          18                195        3250
#>  4 Adelie  Torge…           NA            NA                 NA          NA
#>  5 Adelie  Torge…           36.7          19.3              193        3450
#>  6 Adelie  Torge…           39.3          20.6              190        3650
#>  7 Adelie  Torge…           38.9          17.8              181        3625
#>  8 Adelie  Torge…           39.2          19.6              195        4675
#>  9 Adelie  Torge…           34.1          18.1              193        3475
#> 10 Adelie  Torge…           42            20.2              190        4250
#> # … with 334 more rows, and 1 more variable: sex <fct>
```

``` r
rec <- recipe(body_mass_g ~ ., data = penguins) %>%
  step_encode_mean(all_nominal(), outcome = "body_mass_g") %>%
  prep()
juice(rec)
#> # A tibble: 344 x 7
#>    species island bill_length_mm bill_depth_mm flipper_length_…   sex
#>      <dbl>  <dbl>          <dbl>         <dbl>            <int> <dbl>
#>  1   3701.  3706.           39.1          18.7              181 4546.
#>  2   3701.  3706.           39.5          17.4              186 3862.
#>  3   3701.  3706.           40.3          18                195 3862.
#>  4   3701.  3706.           NA            NA                 NA   NA 
#>  5   3701.  3706.           36.7          19.3              193 3862.
#>  6   3701.  3706.           39.3          20.6              190 4546.
#>  7   3701.  3706.           38.9          17.8              181 3862.
#>  8   3701.  3706.           39.2          19.6              195 4546.
#>  9   3701.  3706.           34.1          18.1              193   NA 
#> 10   3701.  3706.           42            20.2              190   NA 
#> # … with 334 more rows, and 1 more variable: body_mass_g <int>
```

To get informations about the step:

``` r
tidy(rec, 1)
#> # A tibble: 8 x 4
#>   terms   group      mean id                
#>   <chr>   <chr>     <dbl> <chr>             
#> 1 species Adelie    3701. encode_count_5iuPy
#> 2 species Chinstrap 3733. encode_count_5iuPy
#> 3 species Gentoo    5076. encode_count_5iuPy
#> 4 island  Biscoe    4716. encode_count_5iuPy
#> 5 island  Dream     3713. encode_count_5iuPy
#> 6 island  Torgersen 3706. encode_count_5iuPy
#> 7 sex     female    3862. encode_count_5iuPy
#> 8 sex     male      4546. encode_count_5iuPy
```
