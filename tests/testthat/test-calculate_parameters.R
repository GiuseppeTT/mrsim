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

test_that("calculate_parameters works", {
    expect_s3_class(parameters, "parameters")
    expect_equal(parameters$m, 500)
    expect_equal(parameters$k, 500)
    expect_equal(parameters$p, 25 / 100)
    expect_equal(parameters$alpha_u, 0)
    expect_equal(parameters$sigma2_u, 1)
    expect_equal(parameters$alpha_x, 0)
    expect_equal(parameters$beta_g_x, c(rep(0.01, 500), rep(0, 500)))
    expect_equal(parameters$beta_u_x, 0.547722557505)
    expect_equal(parameters$sigma2_x, 0.65)
    expect_equal(parameters$alpha_y, 0)
    expect_equal(parameters$beta_u_y, 0.438178046004)
    expect_equal(parameters$beta_x_y, 20 / 100)
    expect_equal(parameters$sigma2_y, 0.672)
})
