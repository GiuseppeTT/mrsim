
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

print(hyper_parameters)
#> # Hyper parameters
#> 
#> Number of non-zero effect G's (m): 500
#> Number of zero effect G's (k): 500
#> Minor allele frequency of G's (p): 0.25
#> Variance in X explained per G (r2_g_x): 1e-04
#> Variance in X explained by U (r2_u_x): 0.3
#> Variance in Y explained by U (r2_u_y): 0.3
#> Causal effect of X on Y (beta_x_y): 0.2 (targeted causal effect)

restrictions <- define_restrictions()

print(restrictions)
#> # Restrictions
#> 
#> G mean (mean_g): 0
#> G variance (var_g): 1
#> U mean (mean_u): 0
#> U variance (var_u): 1
#> X mean (mean_x): 0
#> X variance (var_X): 1
#> Y mean (mean_y): 0
#> Y variance (var_Y): 1

parameters <- calculate_parameters(hyper_parameters, restrictions)

print(parameters)
#> # Parameters
#> 
#> Number of non-zero effect G's (m): 500
#> Number of zero effect G's (k): 500
#> Minor allele frequency of G's (p): 0.25
#> U intercept (alpha_u): 0
#> U noise variance (sigma2_u): 1
#> X intercept (alpha_x): 0
#> Causal effect of G's on X (beta_g_x): [0.01, ..., 0] (1000 sized vector)
#> Causal effect of U on X (beta_u_x): 0.5477226
#> X noise variance (sigma2_x): 0.65
#> Y intercept (alpha_y): 0
#> Causal effect of U on Y (beta_u_y): 0.438178
#> Causal effect of X on Y (beta_x_y): 0.2 (targeted causal effect)
#> Y noise variance (sigma2_y): 0.672

sample <- generate_sample(parameters,n = 10e3)

print(sample)
#> # Sample
#> 
#> Sample size (n): 10000
#> 
#> ## Exogenous variables
#> 
#> Raw G data (gprime): [[1, ..., 0]] (10000 x 1000 sized matrix)
#> U noise (e_u): [[-1.19680097216348, ..., -0.890468699587795]] (10000 x 1 sized matrix)
#> X noise (e_x): [[0.460947832126287, ..., -2.41969073735133]] (10000 x 1 sized matrix)
#> Y noise (e_y): [[0.527904124270486, ..., 0.388857577263968]] (10000 x 1 sized matrix)
#> 
#> ## Endogenous variables
#> 
#> G data (g): [[0.816496580927726, ..., -0.816496580927726]] (10000 x 1000 sized matrix)
#> U data (u): [[-1.19680097216348, ..., -0.890468699587795]] (10000 x 1 sized matrix)
#> X data (x): [[-0.545165772082768, ..., -2.1609379955708]] (10000 x 1 sized matrix)
#> Y data (y): [[-0.20069246021126, ..., -0.503603077991844]] (10000 x 1 sized matrix)

summary_statistics <- calculate_summary_statistics(sample)

print(head(summary_statistics))
#>   snp     n     beta_g_x beta_se_g_x p_value_g_x f_statistic_g_x       r2_g_x
#> 1   1 10000  0.010238784 0.009974030   0.3046599      1.05379325 1.053893e-04
#> 2   2 10000 -0.001569459 0.009940290   0.8745478      0.02492881 2.493374e-06
#> 3   3 10000 -0.007923938 0.010041294   0.4300517      0.62273429 6.228201e-05
#> 4   4 10000  0.006464692 0.009949239   0.5158574      0.42219769 4.222643e-05
#> 5   5 10000  0.003288470 0.009979881   0.7417772      0.10857682 1.085974e-05
#> 6   6 10000  0.009602536 0.010089301   0.3412446      0.90583630 9.059354e-05
#>        beta_g_y beta_se_g_y p_value_g_y f_statistic_g_y       r2_g_y
#> 1 -0.0110235115 0.010005966  0.27062129      1.21372946 1.213825e-04
#> 2  0.0015893525 0.009972198  0.87337401      0.02540146 2.540648e-06
#> 3 -0.0007303149 0.010073837  0.94220848      0.00525570 5.256748e-07
#> 4  0.0078155899 0.009981081  0.43362190      0.61315236 6.132374e-05
#> 5 -0.0094842732 0.010011520  0.34349052      0.89744539 8.975444e-05
#> 6  0.0176732200 0.010120602  0.08079619      3.04943011 3.049110e-04

filtered_summary_statistics <- summary_statistics[summary_statistics$f_statistic_g_x > 10, ]

print(head(filtered_summary_statistics))
#>     snp     n   beta_g_x beta_se_g_x  p_value_g_x f_statistic_g_x      r2_g_x
#> 15   15 10000 0.03241914 0.009957096 1.134168e-03        10.60077 0.001059166
#> 44   44 10000 0.03422637 0.009885209 5.376580e-04        11.98809 0.001197612
#> 228 228 10000 0.03357739 0.009982621 7.722630e-04        11.31370 0.001130317
#> 244 244 10000 0.03956636 0.009937673 6.898075e-05        15.85195 0.001583002
#> 257 257 10000 0.03805379 0.010059046 1.558269e-04        14.31141 0.001429381
#> 259 259 10000 0.03705894 0.009865990 1.734711e-04        14.10927 0.001409221
#>        beta_g_y beta_se_g_y p_value_g_y f_statistic_g_y       r2_g_y
#> 15  0.010508500 0.009993799   0.2930535       1.1056565 1.105755e-04
#> 44  0.009588403 0.009922421   0.3338989       0.9338074 9.339069e-05
#> 228 0.009723977 0.010019857   0.3318355       0.9418132 9.419128e-05
#> 244 0.013624680 0.009976543   0.1720728       1.8650588 1.865084e-04
#> 257 0.005742709 0.010098391   0.5695891       0.3233920 3.234462e-05
#> 259 0.016103133 0.009903332   0.1039744       2.6439796 2.643809e-04

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
