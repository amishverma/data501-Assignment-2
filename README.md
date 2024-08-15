
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ass2Data501

<!-- badges: start -->
<!-- badges: end -->

The goal of Ass2Data501 is to calculate and visualize influence measures
for linear models.

## Installation

You can install the development version of Ass2Data501 from

[Github](https://github.com/amishverma/data501-Assignment-2) with:

``` r
install.packages("devtools")
devtools::install_github("amishverma/data501-Assignment-2")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Ass2Data501)
data(mtcars)
model <- lm(mpg ~ wt + hp, data = mtcars)
plot_influence_measures(model,mtcars,method="cooks")
```

<img src="man/figures/README-example-1.png" width="100%" />
