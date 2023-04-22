parameters <- define_parameters(
    m = 500,
    k = 500,
    p = 25 / 100,
    alpha_u = 0,
    sigma2_u = 1,
    alpha_x = 0,
    beta_g_x = c(rep(0.01, 500), rep(0, 500)),
    beta_u_x = 0.55,
    sigma2_x = 0.65,
    alpha_y = 0,
    beta_x_y = 0.45,
    beta_u_y = 0.30,
    sigma2_y = 0.56
)

test_that("define_parameters works", {
    expect_s3_class(parameters, "parameters")
    expect_equal(parameters$m, 500)
    expect_equal(parameters$k, 500)
    expect_equal(parameters$p, 25 / 100)
    expect_equal(parameters$alpha_u, 0)
    expect_equal(parameters$sigma2_u, 1)
    expect_equal(parameters$alpha_x, 0)
    expect_equal(parameters$beta_g_x, c(rep(0.01, 500), rep(0, 500)))
    expect_equal(parameters$beta_u_x, 0.55)
    expect_equal(parameters$sigma2_x, 0.65)
    expect_equal(parameters$alpha_y, 0)
    expect_equal(parameters$beta_x_y, 0.45)
    expect_equal(parameters$beta_u_y, 0.30)
    expect_equal(parameters$sigma2_y, 0.56)
})
