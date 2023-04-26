#' Calculate parameters
#'
#' Calculate parameters for simulating mendelian randomization (MR) data from the given hyper parameters and restrictions.
#'
#' Additional constraints may be posed to the parameters values in order to guarantee that the simulated data is valid.
#'
#' @param hyper_parameters An object of class `hyper_parameters`
#' @param restrictions An object of class `restrictions`
#'
#' @return An object of class `parameters`
#'
#' @export
calculate_parameters <- function(
    hyper_parameters,
    restrictions
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

    alpha_u <- 0
    sigma2_u <- 1

    total_r2_g_x <- m * r2_g_x
    alpha_x <- 0
    betaprime_g_x <- c(rep_len(1, m), rep_len(0, k))
    beta_g_x <- betaprime_g_x * sqrt(total_r2_g_x / sum(betaprime_g_x^2))
    beta_u_x <- sqrt(r2_u_x)
    sigma2_x <- 1 - beta_u_x^2 - sum(beta_g_x^2)

    alpha_y <- 0
    beta_u_y <- sqrt(r2_u_y) - beta_x_y * beta_u_x
    sigma2_y <- 1 - (beta_u_y + beta_x_y * beta_u_x)^2 - beta_x_y^2 * sum(beta_g_x^2) - beta_x_y^2 * sigma2_x

    parameters <- new_parameters(
        m = m,
        k = k,
        p = p,
        alpha_u = alpha_u,
        sigma2_u = sigma2_u,
        alpha_x = alpha_x,
        beta_g_x = beta_g_x,
        beta_u_x = beta_u_x,
        sigma2_x = sigma2_x,
        alpha_y  = alpha_y ,
        beta_u_y = beta_u_y,
        beta_x_y = beta_x_y,
        sigma2_y = sigma2_y,
        hyper_parameters = hyper_parameters,
        restrictions = restrictions
    )

    validate_parameters(parameters)

    return(parameters)
}

#' Define parameters
#'
#' Define parameters for simulating mendelian randomization (MR) data.
#'
#' Additional constraints may be posed to the parameter values in order to guarantee that the simulated data is valid.
#'
#' @param m Number of non-zero effect G's. It should be a positive integer
#' @param k Number of zero effect G's. It should be a positive integer
#' @param p Minor allele frequency. It should be between `0` and `0.5`
#' @param alpha_u Intercept of U. It should be a number
#' @param sigma2_u Variance of U noise. It should be a positive number
#' @param alpha_x Intercept of X. It should be a number
#' @param beta_g_x Causal effect of G's on X. It should be a number
#' @param beta_u_x Causal effect of U on X. It should be a number
#' @param sigma2_x Variance of X noise. It should be a positive number
#' @param alpha_y Intercept of Y. It should be a number
#' @param beta_u_y Causal effect of U on Y. It should be a number
#' @param beta_x_y Causal effect of X on Y. It should be a number
#' @param sigma2_y Variance of Y noise. It should be a positive number
#'
#' @return An object of class `parameters`
#'
#' @export
define_parameters <- function(
    m,
    k,
    p,
    alpha_u,
    sigma2_u,
    alpha_x,
    beta_g_x,
    beta_u_x,
    sigma2_x,
    alpha_y,
    beta_u_y,
    beta_x_y,
    sigma2_y
) {
    parameters <- new_parameters(
        m = m,
        k = k,
        p = p,
        alpha_u = alpha_u,
        sigma2_u = sigma2_u,
        alpha_x = alpha_x,
        beta_g_x = beta_g_x,
        beta_u_x = beta_u_x,
        sigma2_x = sigma2_x,
        alpha_y  = alpha_y ,
        beta_u_y = beta_u_y,
        beta_x_y = beta_x_y,
        sigma2_y = sigma2_y,
        hyper_parameters = NULL,
        restrictions = NULL
    )

    validate_parameters(parameters)

    return(parameters)
}

new_parameters <- function(
    m,
    k,
    p,
    alpha_u,
    sigma2_u,
    alpha_x,
    beta_g_x,
    beta_u_x,
    sigma2_x,
    alpha_y,
    beta_u_y,
    beta_x_y,
    sigma2_y,
    hyper_parameters,
    restrictions
) {
    stopifnot(is.numeric(m))
    stopifnot(is.numeric(k))
    stopifnot(is.numeric(p))
    stopifnot(is.numeric(alpha_u))
    stopifnot(is.numeric(sigma2_u))
    stopifnot(is.numeric(alpha_x))
    stopifnot(is.numeric(beta_g_x))
    stopifnot(is.numeric(beta_u_x))
    stopifnot(is.numeric(sigma2_x))
    stopifnot(is.numeric(alpha_y))
    stopifnot(is.numeric(beta_u_y))
    stopifnot(is.numeric(beta_x_y))
    stopifnot(is.numeric(sigma2_y))
    stopifnot(is.null(hyper_parameters) || is_hyper_parameters(hyper_parameters))
    stopifnot(is.null(restrictions) || is_restrictions(restrictions))

    values <- list(
        m = m,
        k = k,
        p = p,
        alpha_u = alpha_u,
        sigma2_u = sigma2_u,
        alpha_x = alpha_x,
        beta_g_x = beta_g_x,
        beta_u_x = beta_u_x,
        sigma2_x = sigma2_x,
        alpha_y  = alpha_y ,
        beta_u_y = beta_u_y,
        beta_x_y = beta_x_y,
        sigma2_y = sigma2_y
    )

    parameters <- structure(
        values,
        class = "parameters",
        hyper_parameters = hyper_parameters,
        restrictions = restrictions
    )

    return(parameters)
}

validate_parameters <- function(
    parameters
) {
    hyper_parameters <- attr(parameters, "hyper_parameters")
    restrictions <- attr(parameters, "restrictions")

    validate_parameters_parameters(parameters)

    if (xor(is.null(hyper_parameters), is.null(restrictions))) {
        stop("Either both hyper parameters and restrictions should be defined or none of them should")
    }

    if (! is.null(hyper_parameters) && ! is.null(restrictions)) {
        validate_parameters_hyper_parameters(parameters, hyper_parameters)
        validate_parameters_restrictions(parameters, restrictions)
    }
}

validate_parameters_parameters <- function(
    parameters
) {
    m <- parameters$m
    k <- parameters$k
    p <- parameters$p
    alpha_u <- parameters$alpha_u
    sigma2_u <- parameters$sigma2_u
    alpha_x <- parameters$alpha_x
    beta_g_x <- parameters$beta_g_x
    beta_u_x <- parameters$beta_u_x
    sigma2_x <- parameters$sigma2_x
    alpha_y <- parameters$alpha_y
    beta_u_y <- parameters$beta_u_y
    beta_x_y <- parameters$beta_x_y
    sigma2_y <- parameters$sigma2_y

    if (! (is_positive(m) && is_discrete(m))) {
        stop("Parameter `m` should be positive and discrete")
    }

    if (! (is_positive(k) && is_discrete(k))) {
        stop("Parameter `k` should be positive and discrete")
    }

    if (! is_in_bound(p, 0, 0.5)) {
        stop("Parameter `p` should be in the [0, 0.5] interval")
    }

    if (! is_positive(sigma2_u)) {
        stop("Parameter `sigma2_u` should be positive")
    }

    if (! is_lengthed_as(beta_g_x, m + k)) {
        stop("Parameter `beta_g_x` should have length `m + k`")
    }

    if (! is_positive(sigma2_x)) {
        stop("Parameter `sigma2_x` should be positive")
    }

    if (! is_positive(sigma2_y)) {
        stop("Parameter `sigma2_y` should be positive")
    }
}

validate_parameters_hyper_parameters <- function(
    parameters,
    hyper_parameters
) {
    m <- parameters$m
    k <- parameters$k
    p <- parameters$p
    alpha_u <- parameters$alpha_u
    sigma2_u <- parameters$sigma2_u
    alpha_x <- parameters$alpha_x
    beta_g_x <- parameters$beta_g_x
    beta_u_x <- parameters$beta_u_x
    sigma2_x <- parameters$sigma2_x
    alpha_y <- parameters$alpha_y
    beta_u_y <- parameters$beta_u_y
    beta_x_y <- parameters$beta_x_y
    sigma2_y <- parameters$sigma2_y

    d <- hyper_parameters$d
    s <- hyper_parameters$s
    hp_p <- hyper_parameters$p
    r2_g_x <- hyper_parameters$r2_g_x
    r2_u_x <- hyper_parameters$r2_u_x
    r2_u_y <- hyper_parameters$r2_u_y
    hp_beta_x_y <- hyper_parameters$beta_x_y

    if (d != m + k) {
        stop("Hyper parameter `d` should be equal to `m + k`")
    }

    if (! is_close(s, k / (m + k), tolerance = 1 / d)) {
        stop("Hyper parameter `s` should be close to `k / (m + k)`")
    }

    if (hp_p != p) {
        stop("Hyper parameter `p` should be equal to parameter `p`")
    }
}

validate_parameters_restrictions <- function(
    parameters,
    restrictions
) {
    m <- parameters$m
    k <- parameters$k
    p <- parameters$p
    alpha_u <- parameters$alpha_u
    sigma2_u <- parameters$sigma2_u
    alpha_x <- parameters$alpha_x
    beta_g_x <- parameters$beta_g_x
    beta_u_x <- parameters$beta_u_x
    sigma2_x <- parameters$sigma2_x
    alpha_y <- parameters$alpha_y
    beta_u_y <- parameters$beta_u_y
    beta_x_y <- parameters$beta_x_y
    sigma2_y <- parameters$sigma2_y

    mean_g <- restrictions$mean_g
    var_g <- restrictions$var_g
    mean_u <- restrictions$mean_u
    var_u <- restrictions$var_u
    mean_x <- restrictions$mean_x
    var_x <- restrictions$var_x
    mean_y <- restrictions$mean_y
    var_y <- restrictions$var_y

    calculated_mean_g <- (2 * p - 2 * p) / sqrt(2 * p * (1 - p))
    if (! is_close(mean_g, calculated_mean_g)) {
        stop("Restriction not enforced. Should be `mean_g = (2 * p - 2 * p) / sqrt(2 * p * (1 - p))`")
    }

    calculated_var_g <- (2 * p * (1 - p)) / (2 * p * (1 - p))
    if (! is_close(var_g, calculated_var_g)) {
        stop("Restriction not enforced. Should be `var_g = (2 * p * (1 - p)) / (2 * p * (1 - p))`")
    }

    calculated_mean_u <- alpha_u
    if (! is_close(mean_u, calculated_mean_u)) {
        stop("Restriction not enforced. Should be `mean_u = alpha_u`")
    }

    calculated_var_u <- sigma2_u
    if (! is_close(var_u, calculated_var_u)) {
        stop("Restriction not enforced. Should be `var_u = sigma2_u`")
    }

    calculated_mean_x <- alpha_x + beta_u_x * mean_u + sum(beta_g_x * mean_g)
    if (! is_close(mean_x, calculated_mean_x)) {
        stop("Restriction not enforced. Should be `mean_x = alpha_x + beta_u_x * mean_u + sum(beta_g_x * mean_g)`")
    }

    calculated_var_x <- beta_u_x^2 * var_u + sum(beta_g_x^2 * var_g) + sigma2_x
    if (! is_close(var_x, calculated_var_x)) {
        stop("Restriction not enforced. Should be `var_x = beta_u_x^2 * var_u + sum(beta_g_x^2 * var_g) + sigma2_x`")
    }

    calculated_mean_y <- alpha_y + beta_u_y * mean_u + beta_x_y * mean_x
    if (! is_close(mean_y, calculated_mean_y)) {
        stop("Restriction not enforced. Should be `mean_y = alpha_y + beta_u_y * mean_u + beta_x_y * mean_x`")
    }

    calculated_var_y <- (beta_u_y + beta_x_y * beta_u_x)^2 * var_u + beta_x_y^2 * sum(beta_g_x^2 * var_g) + beta_x_y^2 * sigma2_x + sigma2_y
    if (! is_close(var_y, calculated_var_y)) {
        stop("Restriction not enforced. Should be `var_y = beta_u_y^2 * var_u + beta_x_y^2 * beta_u_x^2 * var_u + beta_x_y^2 * sum(beta_g_x^2 * var_g) + beta_x_y^2 * sigma2_x + sigma2_y`")
    }
}

#' Test if the object is `paremeters`
#'
#' Returns TRUE for `paremeters` or subclasses thereof and FALSE for all other objects.
#'
#' @param x An object
#'
#' @return A logical value
#'
#' @export
is_parameters <- function(
    x
) {
    result <- is_subclass_of(x, "parameters")

    return(result)
}

#' @export
print.parameters <- function(
    x,
    ...
) {
    args <- list(...)
    level <- args$level %||% 1
    header <- paste0(rep("#", level), collapse = "")

    cat(header, " Parameters", "\n", sep = "")
    cat("\n", sep = "")
    cat("Number of non-zero effect G's (m): ", x$m, "\n", sep = "")
    cat("Number of zero effect G's (k): ", x$k, "\n", sep = "")
    cat("Minor allele frequency of G's (p): ", x$p, "\n", sep = "")
    cat("U intercept (alpha_u): ", x$alpha_u, "\n", sep = "")
    cat("U noise variance (sigma2_u): ", x$sigma2_u, "\n", sep = "")
    cat("X intercept (alpha_x): ", x$alpha_x, "\n", sep = "")
    cat("Causal effect of G's on X (beta_g_x): ", format_vector(x$beta_g_x), "\n", sep = "")
    cat("Causal effect of U on X (beta_u_x): ", x$beta_u_x, "\n", sep = "")
    cat("X noise variance (sigma2_x): ", x$sigma2_x, "\n", sep = "")
    cat("Y intercept (alpha_y): ", x$alpha_y, "\n", sep = "")
    cat("Causal effect of U on Y (beta_u_y): ", x$beta_u_y, "\n", sep = "")
    cat("Causal effect of X on Y (beta_x_y): ", x$beta_x_y, "\n", sep = "")
    cat("Y noise variance (sigma2_y): ", x$sigma2_y, sep = "")
}

#' @export
get_beta_x_y.parameters <- function(
    x,
    ...
) {
    beta_x_y <- x$beta_x_y

    return(beta_x_y)
}
