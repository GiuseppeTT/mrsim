#' Define hyper parameters
#'
#' Define hyper parameters for simulating mendelian randomization (MR) data.
#'
#' Additional constraints may be posed to the hyper parameter values in order to guarantee that the simulated data is valid.
#'
#' @param d (Dimension) Number of G's. It should be a positive integer
#' @param s (Sparsity) Proportion of zero effect G's. It should be between `0` and `1`
#' @param p (Skewness) Minor allele frequency of G'. It should be between `0` and `0.5`
#' @param r2_g_x (Instrument strength) Variance in X explained per non-zero effect G. It should be between `0` and `1 / ceiling((1 - s) * d)`
#' @param r2_u_x (Confouding level) Variance in X explained by U. It should be between `0` and `1`
#' @param r2_u_y (Confouding level) Variance in Y explained by U. It should be between `0` and `1`
#' @param beta_x_y (Target causal effect) Causal effect of X on Y. It should be between `- 1 / sqrt(r2_g_x)` and `1 / sqrt(r2_g_x)`. Since all variables are standardized, a more reasonable range is between `-1` and `1`
#'
#' @return An object of class `hyper_parameters`
#'
#' @export
define_hyper_parameters <- function(
    d,
    s,
    p,
    r2_g_x,
    r2_u_x,
    r2_u_y,
    beta_x_y
) {
    hyper_parameters <- new_hyper_parameters(
        d = d,
        s = s,
        p = p,
        r2_g_x = r2_g_x,
        r2_u_x = r2_u_x,
        r2_u_y = r2_u_y,
        beta_x_y = beta_x_y
    )

    validate_hyper_parameters(hyper_parameters)

    return(hyper_parameters)
}

new_hyper_parameters <- function(
    d,
    s,
    p,
    r2_g_x,
    r2_u_x,
    r2_u_y,
    beta_x_y
) {
    stopifnot(is.numeric(d))
    stopifnot(is.numeric(s))
    stopifnot(is.numeric(p))
    stopifnot(is.numeric(r2_g_x))
    stopifnot(is.numeric(r2_u_x))
    stopifnot(is.numeric(r2_u_y))
    stopifnot(is.numeric(beta_x_y))

    values <- list(
        d = d,
        s = s,
        p = p,
        r2_g_x = r2_g_x,
        r2_u_x = r2_u_x,
        r2_u_y = r2_u_y,
        beta_x_y = beta_x_y
    )

    hyper_parameters <- structure(
        values,
        class = "hyper_parameters"
    )

    return(hyper_parameters)
}

validate_hyper_parameters <- function(
    hyper_parameters
) {
    d <- hyper_parameters$d
    s <- hyper_parameters$s
    p <- hyper_parameters$p
    r2_g_x <- hyper_parameters$r2_g_x
    r2_u_x <- hyper_parameters$r2_u_x
    r2_u_y <- hyper_parameters$r2_u_y
    beta_x_y <- hyper_parameters$beta_x_y

    m <- ceiling((1 - s) * d)
    k <- d - m

    if (! (is_positive(d) && is_discrete(d))) {
        stop("Hyper parameter `d` should be positive and discrete")
    }

    if (! is_in_zero_one(s)) {
        stop("Hyper parameter `s` should be in the [0, 1] interval")
    }

    if (! is_in_bound(p, 0, 0.5)) {
        stop("Hyper parameter `p` should be in the [0, 0.5] interval")
    }

    # Otherwise the G's would explain more than 100% of the variance of X
    if (! is_in_bound(r2_g_x, 0, 1 / m)) {
        stop("Hyper parameter `r2_g_x` should be in the [0, 1 / ceiling((1 - s) * d)] interval")
    }

    if (! is_in_zero_one(r2_u_x)) {
        stop("Hyper parameter `r2_u_x` should be in the [0, 1] interval")
    }

    total_r2_x <- m * r2_g_x + r2_u_x
    if (! is_in_zero_one(total_r2_x)) {
        stop("Hyper parameters `s`, `d`, `r2_g_x` and `r2_u_x` are not consistent. `total_r2_x = ceiling((1 - s) * d) * r2_g_x + r2_u_x` should be in the [0, 1] interval")
    }

    if (! is_in_zero_one(r2_u_y)) {
        stop("Hyper parameter `r2_u_y` should be in the [0, 1] interval")
    }

    # Otherwise the G's would explain more than 100% of the variance of Y
    # Derived using r2_g_y = beta_x_y^2 * r2_g_x
    if (! is_in_bound(beta_x_y, - 1 / sqrt(r2_g_x), 1 / sqrt(r2_g_x))) {
        stop("Hyper parameter `beta_x_y` should be in the [- 1 / sqrt(r2_g_x), 1 / sqrt(r2_g_x)] interval")
    }

    total_r2_y <- beta_x_y^2 * m * r2_g_x + r2_u_y
    if (! is_in_zero_one(total_r2_y)) {
        stop("Hyper parameters `s`, `d`, `r2_g_x`, `r2_u_y` and `beta_x_y` are not consistent. `total_r2_y = beta_x_y^2 * ceiling((1 - s) * d) * r2_g_x + r2_u_y` should be in the [0, 1] interval")
    }
}

#' Test if the object is `hyper_parameters`
#'
#' Returns TRUE for `hyper_parameters` or subclasses thereof and FALSE for all other objects.
#'
#' @param x An object
#'
#' @return A logical value
#'
#' @export
is_hyper_parameters <- function(
    x
) {
    result <- is_subclass_of(x, "hyper_parameters")

    return(result)
}

#' @export
print.hyper_parameters <- function(
    x,
    ...
) {
    args <- list(...)
    level <- args$level %||% 1
    header <- paste0(rep("#", level), collapse = "")

    cat(header, " Hyper parameters", "\n", sep = "")
    cat("\n", sep = "")
    cat("(Dimension) Number of G's (d): ", x$d, "\n", sep = "")
    cat("(Sparsity) Proportion of zero effect G's (s): ", x$s, "\n", sep = "")
    cat("(Skewness) Minor allele frequency of G' (p): ", x$p, "\n", sep = "")
    cat("(Instrument strength) Variance in X explained per non-zero effect G (r2_g_x): ", x$r2_g_x, "\n", sep = "")
    cat("(Confouding level) Variance in X explained by U (r2_u_x): ", x$r2_u_x, "\n", sep = "")
    cat("(Confouding level) Variance in Y explained by U (r2_u_y): ", x$r2_u_y, "\n", sep = "")
    cat("(Target causal effect) Causal effect of X on Y (beta_x_y): ", x$beta_x_y, "\n", sep = "")
}

#' @export
get_beta_x_y.hyper_parameters <- function(
    x,
    ...
) {
    beta_x_y <- x$beta_x_y

    return(beta_x_y)
}
