hyper_parameters <- define_hyper_parameters(
    d = 1e3,
    s = 50 / 100,
    p = 25 / 100,
    r2_g_x = 0.01 / 100,
    r2_u_x = 30 / 100,
    r2_u_y = 30 / 100,
    beta_x_y = 20 / 100
)

test_that("define_hyper_parameters works", {
    expect_s3_class(hyper_parameters, "hyper_parameters")
    expect_equal(hyper_parameters$d, 1e3)
    expect_equal(hyper_parameters$s, 50 / 100)
    expect_equal(hyper_parameters$p, 25 / 100)
    expect_equal(hyper_parameters$r2_g_x, 0.01 / 100)
    expect_equal(hyper_parameters$r2_u_x, 30 / 100)
    expect_equal(hyper_parameters$r2_u_y, 30 / 100)
    expect_equal(hyper_parameters$beta_x_y, 20 / 100)
})
