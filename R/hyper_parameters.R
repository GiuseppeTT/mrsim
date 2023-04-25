#' Define hyper parameters
#'
#' Define hyper parameters for simulating mendelian randomization (MR) data.
#'
#' Additional constraints may be posed to the hyper parameter values in order to garantee that the simulated data is valid.
#'
#' @param m Number of non-zero effect SNPs. It should be a positive integer
#' @param k Number of zero effect SNPs. It should be a positive integer
#' @param p Minor allele frequency. It should be between `0` and `0.5`
#' @param r2_g_x Variance in X explained per G. It should be between `0` and `1 / m`
#' @param r2_u_x Variance in X explained by U. It should be between `0` and `1`
#' @param r2_u_y Variance in Y explained by U. It should be between `0` and `1`
#' @param beta_x_y Causal effect of X on Y. It should be between `- 1 / sqrt(r2_g_x)` and `1 / sqrt(r2_g_x)`. Since all variables are standardized, a more reasonable range is between `-1` and `1`
#'
#' @return An object of class "hyper_parameters"
#' @export
define_hyper_parameters <- function(
    m,
    k,
    p,
    r2_g_x,
    r2_u_x,
    r2_u_y,
    beta_x_y
) {
    hyper_parameters <- new_hyper_parameters(
        m = m,
        k = k,
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
    m,
    k,
    p,
    r2_g_x,
    r2_u_x,
    r2_u_y,
    beta_x_y
) {
    stopifnot(is.numeric(m))
    stopifnot(is.numeric(k))
    stopifnot(is.numeric(p))
    stopifnot(is.numeric(r2_g_x))
    stopifnot(is.numeric(r2_u_x))
    stopifnot(is.numeric(r2_u_y))
    stopifnot(is.numeric(beta_x_y))

    values <- list(
        m = m,
        k = k,
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
    m <- hyper_parameters$m
    k <- hyper_parameters$k
    p <- hyper_parameters$p
    r2_g_x <- hyper_parameters$r2_g_x
    r2_u_x <- hyper_parameters$r2_u_x
    r2_u_y <- hyper_parameters$r2_u_y
    beta_x_y <- hyper_parameters$beta_x_y

    if (! (is_positive(m) && is_discrete(m))) {
        stop("Hyper parameter `m` should be positive and discrete")
    }

    if (! (is_positive(k) && is_discrete(k))) {
        stop("Hyper parameter `k` should be positive and discrete")
    }

    if (! is_in_bound(p, 0, 0.5)) {
        stop("Hyper parameter `p` should be in the [0, 0.5] interval")
    }

    # Otherwise the G's would explain more than 100% of the variance of X
    if (! is_in_bound(r2_g_x, 0, 1/m)) {
        stop("Hyper parameter `r2_g_x` should be in the [0, 1/m] interval")
    }

    if (! is_in_zero_one(r2_u_x)) {
        stop("Hyper parameter `r2_u_x` should be in the [0, 1] interval")
    }

    total_r2_x <- m * r2_g_x + r2_u_x
    if (! is_in_zero_one(total_r2_x)) {
        stop("Hyper parameters `m`, `r2_g_x` and `r2_u_x` are not consistent. `total_r2_x = m * r2_g_x + r2_u_x` should be in the [0, 1] interval")
    }

    if (! is_in_zero_one(r2_u_y)) {
        stop("Hyper parameter `r2_u_y` should be in the [0, 1] interval")
    }

    # Otherwise the G's would explain more than 100% of the variance of Y
    # Derived using r2_g_y = beta_x_y^2 * r2_g_x
    if (! is_in_bound(beta_x_y, -1, 1)) {
        stop("Hyper parameter `beta_x_y` should be in the [- 1 / sqrt(r2_g_x), 1 / sqrt(r2_g_x)] interval")
    }

    total_r2_y <- beta_x_y^2 * m * r2_g_x + r2_u_y
    if (! is_in_zero_one(total_r2_y)) {
        stop("Hyper parameters `m`, `r2_g_x`, `r2_u_y` and `beta_x_y` are not consistent. `total_r2_y = beta_x_y^2 * m * r2_g_x + r2_u_y` should be in the [0, 1] interval")
    }
}

#' Test if the object is `hyper_parameters`
#'
#' This function returns TRUE for `hyper_parameters` or subclasses thereof and FALSE for all other objects.
#'
#' @param x An object
#'
#' @return TRUE if the object inherits from the `hyper_parameters` class, else FALSE
#' @export
is_hyper_parameters <- function(
    x
) {
    result <- any(class(x) == "hyper_parameters")

    return(result)
}


#' Print an object as `hyper_parameters`
#'
#' This function prints an object as if its class is `hyper_parameters`. It is intended to be used only for `hyper_parameters` or subclasses thereof.
#'
#' @param x An object
#' @param ... Extra arguments. Currently not being used.
#'
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
    cat("Number of causal G's (m): ", x$m, "\n", sep = "")
    cat("Number of null G's (k): ", x$k, "\n", sep = "")
    cat("Minor allele frequency of G's (p): ", x$p, "\n", sep = "")
    cat("Variance in X explained per G (r2_g_x): ", x$r2_g_x, "\n", sep = "")
    cat("Variance in X explained by U (r2_u_x): ", x$r2_u_x, "\n", sep = "")
    cat("Variance in Y explained by U (r2_u_y): ", x$r2_u_y, "\n", sep = "")
    cat("Causal effect of X on Y (beta_x_y): ", x$beta_x_y, " (targeted causal effect)", "\n", sep = "")
}
