
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mrsim

<!-- badges: start -->
<!-- badges: end -->

The goal of `mrsim` is to simulate mendelian randomization (MR) data
using meaningful hyper parameters.

## Installation

You can install the development version of `mrsim` from
[GitHub](https://github.com/) with

``` r
# install.packages("devtools")
devtools::install_github("GiuseppeTT/mrsim")
```

## Example

This is a basic example which shows you how to use `mrsim` to simulate
MR data and estimate the causal effect of the exposure (X) on the
outcome (Y) with the `MendelianRandomization` package.

``` r
# install.packages("MendelianRandomization")
library(mrsim)

set.seed(42)

hyper_parameters <- define_hyper_parameters(
    m = 500,
    k = 500,
    p = 25 / 100,
    r2_g_x = 0.01 / 100,
    r2_u_x = 30 / 100,
    r2_u_y = 30 / 100,
    beta_x_y = 20 / 100
)

restrictions <- define_restrictions()

parameters <- calculate_parameters(hyper_parameters, restrictions)

sample <- generate_sample(parameters,n = 10e3)

summary_statistics <- calculate_summary_statistics(sample)

filtered_summary_statistics <- summary_statistics[summary_statistics$f_statistic_g_x > 10, ]

mr_data <- MendelianRandomization::mr_input(
    bx = filtered_summary_statistics$beta_g_x,
    bxse = filtered_summary_statistics$beta_se_g_x,
    by = filtered_summary_statistics$beta_g_y,
    byse = filtered_summary_statistics$beta_se_g_y
)

model_fit <- MendelianRandomization::mr_ivw(mr_data)

estimated_beta_x_y <- model_fit@Estimate

print(estimated_beta_x_y)
#> [1] 0.316981

real_beta_x_y <- get_beta_x_y(parameters)

print(real_beta_x_y)
#> [1] 0.2
```
