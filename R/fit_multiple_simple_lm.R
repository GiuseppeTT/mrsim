# Reference: https://stackoverflow.com/questions/51953709/fast-pairwise-simple-linear-regression-between-variables-in-a-data-frame
# Summary:
# - Employs sum of squares to calculate statistics for simple linear regresisons
# - Employs a trick to calculate residual sum of squares without calculating residues
fit_multiple_simple_lm <- function(
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

    x_means <- colMeans(x)
    y_mean <- mean(y)
    x_ss <- colSums(x^2) - n * x_means^2
    y_ss <- sum(y^2) - n * y_mean^2
    xy_ss <- crossprod(x, y) - n * tcrossprod(x_means, y_mean)

    betas <- xy_ss / x_ss
    alphas <- y_mean - betas * x_means

    r2s <- xy_ss^2 / (x_ss * y_ss)
    residue_ss <- (1 - r2s) * y_ss
    sigma2s <- residue_ss / (n - 2)

    beta_ses <- sqrt(sigma2s / x_ss)
    t_statistics <- betas / beta_ses
    p_values <- 2 * stats::pt(-abs(t_statistics), df = n - 2)

    f_statistics <- (n - 2) * r2s / (1 - r2s)

    summary_statistics <- list(
        alphas = as.vector(alphas),
        betas = as.vector(betas),
        beta_ses = as.vector(beta_ses),
        t_statistics = as.vector(t_statistics),
        p_values = as.vector(p_values),
        f_statistics = as.vector(f_statistics),
        sigma2s = as.vector(sigma2s),
        r2s = as.vector(r2s)
    )

    return(summary_statistics)
}
