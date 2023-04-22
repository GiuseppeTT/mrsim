restrictions <- define_restrictions()

test_that("define_restrictions works", {
    expect_s3_class(restrictions, "restrictions")
    expect_equal(restrictions$mean_g, 0)
    expect_equal(restrictions$var_g, 1)
    expect_equal(restrictions$mean_u, 0)
    expect_equal(restrictions$var_u, 1)
    expect_equal(restrictions$mean_x, 0)
    expect_equal(restrictions$var_x, 1)
    expect_equal(restrictions$mean_y, 0)
    expect_equal(restrictions$var_y, 1)
})
