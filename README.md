
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mrsim

<!-- badges: start -->
<!-- badges: end -->

The goal of mrsim is to simulate mendelian randomization (MR) data using
meaningful hyper parameters.

## Installation

You can install the development version of mrsim from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("GiuseppeTT/mrsim")
```

## Example

This is a basic example which shows you how to simulate three-sample MR
data and estimate the causal effect of the exposure (X) on the outcome
(Y) using the `MendelianRandomization` package.

``` r
# install.packages("MendelianRandomization")
library(mrsim)

hyper_parameters <- define_hyper_parameters(
    m = 500,
    k = 500,
    p = 0.25,
    r2_g_x = 0.01 / 100,
    r2_u_x = 0.3,
    r2_g_y = 0.002 / 100,
    r2_u_y = 0.3
)

restrictions <- define_restrictions()

parameters <- calculate_parameters(hyper_parameters, restrictions)

dataset <- generate_dataset(
    parameters,
    n_1 = 10e3,
    n_2 = 10e3,
    n_3 = 10e3
)

summary_statistics <- summarize_dataset(dataset)

filtered_summary_statistics <- summary_statistics[summary_statistics$sample_1_f_statistic_g_x > 10, ]

mr_data <- MendelianRandomization::mr_input(
    bx = filtered_summary_statistics$sample_2_beta_g_x,
    bxse = filtered_summary_statistics$sample_2_beta_se_g_x,
    by = filtered_summary_statistics$sample_3_beta_g_y,
    byse = filtered_summary_statistics$sample_3_beta_se_g_y
)

model_fit <- MendelianRandomization::mr_ivw(mr_data)

estimated_beta_x_y <- model_fit$Estimate

print(estimated_beta_x_y)
#> [1] 0.2862771

real_beta_x_y <- get_beta_x_y(parameters)

print(real_beta_x_y)
#> [1] 0.4472136
```

Alternatively, you can simulate just one sample

``` r
# install.packages("MendelianRandomization")
library(mrsim)

hyper_parameters <- define_hyper_parameters(
    m = 500,
    k = 500,
    p = 0.25,
    r2_g_x = 0.01 / 100,
    r2_u_x = 0.3,
    r2_g_y = 0.002 / 100,
    r2_u_y = 0.3
)

restrictions <- define_restrictions()

parameters <- calculate_parameters(hyper_parameters, restrictions)

sample <- generate_sample(
    parameters,
    n = 10e3
)

summary_statistics <- summarize_sample(sample)

filtered_summary_statistics <- summary_statistics[summary_statistics$f_statistic_g_x > 10, ]

mr_data <- MendelianRandomization::mr_input(
    bx = filtered_summary_statistics$beta_g_x,
    bxse = filtered_summary_statistics$beta_se_g_x,
    by = filtered_summary_statistics$beta_g_y,
    byse = filtered_summary_statistics$beta_se_g_y
)

model_fit <- MendelianRandomization::mr_ivw(mr_data)

estimated_beta_x_y <- model_fit$Estimate

print(estimated_beta_x_y)
#> [1] 0.5421832

real_beta_x_y <- get_beta_x_y(parameters)

print(real_beta_x_y)
#> [1] 0.4472136
```
