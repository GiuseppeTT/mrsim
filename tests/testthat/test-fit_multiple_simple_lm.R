fit_reference <- function(
    x,
    y
) {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(y))
    stopifnot(is.matrix(x))
    stopifnot(is.matrix(y))
    stopifnot(nrow(x) == nrow(y))
    stopifnot(ncol(x) >= 1)
    stopifnot(ncol(y) == 1)

    n <- nrow(x)
    p <- ncol(x)

    alphas <- numeric(p)
    betas <- numeric(p)
    beta_ses <- numeric(p)
    t_statistics <- numeric(p)
    p_values <- numeric(p)
    f_statistics <- numeric(p)
    sigma2s <- numeric(p)
    r2s <- numeric(p)
    for (i in seq_len(p)) {
        xi <- x[, i, drop = FALSE]

        fit <- summary(lm(y ~ xi))
        alphas[i] <- fit$coefficients[1, "Estimate"]
        betas[i] <- fit$coefficients[2, "Estimate"]
        beta_ses[i] <- fit$coefficients[2, "Std. Error"]
        t_statistics[i] <- fit$coefficients[2, "t value"]
        p_values[i] <- fit$coefficients[2, "Pr(>|t|)"]
        f_statistics[i] <- fit$fstatistic["value"]
        sigma2s[i] <- fit$sigma^2
        r2s[i] <- fit$r.squared
    }

    summary_statistics <- list(
        alphas = alphas,
        betas = betas,
        beta_ses = beta_ses,
        t_statistics = t_statistics,
        p_values = p_values,
        f_statistics = f_statistics,
        sigma2s = sigma2s,
        r2s = r2s
    )

    return(summary_statistics)
}

set.seed(123)

alpha <- 1
betas <- as.matrix(rep(1, 10), nrow = 10, ncol = 1)

e <- matrix(10 * rnorm(100), nrow = 100, ncol = 1)
x <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
y <- alpha + x %*% betas + e

alternative_fit <- fit_multiple_simple_lm(x, y)
reference_fit <- fit_reference(x, y)

test_that("fit_multiple_simple_lm(x, y) gives the same results as summary(lm(y ~ x))", {
    expect_equal(alternative_fit$alphas, reference_fit$alphas)
    expect_equal(alternative_fit$betas, reference_fit$betas)
    expect_equal(alternative_fit$beta_ses, reference_fit$beta_ses)
    expect_equal(alternative_fit$t_statistics, reference_fit$t_statistics)
    expect_equal(alternative_fit$p_values, reference_fit$p_values)
    expect_equal(alternative_fit$f_statistics, reference_fit$f_statistics)
    expect_equal(alternative_fit$sigma2s, reference_fit$sigma2s)
    expect_equal(alternative_fit$r2s, reference_fit$r2s)
})
