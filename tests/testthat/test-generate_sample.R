set.seed(42)

hyper_parameters <- define_hyper_parameters(
    m = 500,
    k = 500,
    p = 25 / 100,
    r2_g_x = 0.01 / 100,
    r2_u_x = 30 / 100,
    r2_g_y = 0.002 / 100,
    r2_u_y = 30 / 100
)

restrictions <- define_restrictions()

parameters <- calculate_parameters(hyper_parameters, restrictions)

sample <- generate_sample(parameters,n = 10e3)

test_that("generate_sample works", {
    expect_s3_class(sample, "sample")
})
