set.seed(42)

hyper_parameters <- define_hyper_parameters(
    d = 1e3,
    s = 50 / 100,
    p = 25 / 100,
    r2_g_x = 0.01 / 100,
    r2_u_x = 30 / 100,
    r2_u_y = 30 / 100,
    beta_x_y = 20 / 100
)

restrictions <- define_restrictions()

parameters <- calculate_parameters(hyper_parameters, restrictions)

sample <- generate_sample(parameters, n = 10e3)

summary_statistics <- calculate_summary_statistics(sample)

test_that("calculate_summary_statistics works", {
    expect_s3_class(summary_statistics, "data.frame")
})
