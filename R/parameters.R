calculate_parameters <- function(
    hyper_parameters,
    restrictions
) {
    m <- hyper_parameters$m
    k <- hyper_parameters$k
    p <- hyper_parameters$p
    r2_g_x <- hyper_parameters$r2_g_x
    r2_u_x <- hyper_parameters$r2_u_x
    r2_g_y <- hyper_parameters$r2_g_y
    r2_u_y <- hyper_parameters$r2_u_y

    total_r2_g_x <- m * r2_g_x
    total_r2_g_y <- m * r2_g_y

    alpha_u <- 0
    sigma2_u <- 1

    alpha_x <- 0
    betaprime_g_x <- c(rep_len(1, m), rep_len(0, k))
    beta_g_x <- betaprime_g_x * sqrt(total_r2_g_x / sum(betaprime_g_x^2))
    beta_u_x <- sqrt(r2_u_x)
    sigma2_x <- 1 - beta_u_x^2 - sum(beta_g_x^2)

    alpha_y <- 0
    beta_x_y <- sqrt(total_r2_g_y / sum(beta_g_x^2))
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
        beta_x_y = beta_x_y,
        beta_u_y = beta_u_y,
        sigma2_y = sigma2_y,
        hyper_parameters = hyper_parameters,
        restrictions = restrictions
    )

    validate_parameters(parameters)

    return(parameters)
}

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
    beta_x_y,
    beta_u_y,
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
        beta_x_y = beta_x_y,
        beta_u_y = beta_u_y,
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
    beta_x_y,
    beta_u_y,
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
    stopifnot(is.numeric(beta_x_y))
    stopifnot(is.numeric(beta_u_y))
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
        beta_x_y = beta_x_y,
        beta_u_y = beta_u_y,
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

    validate_parameters_parameters(
        parameters
    )

    if (xor(is.null(hyper_parameters), is.null(restrictions))) {
        stop("Either both hyper parameters and restrictions should be defined or none of them should")
    }

    if (! is.null(hyper_parameters) && ! is.null(restrictions)) {
        validate_parameters_hyper_parameters(
            parameters,
            hyper_parameters
        )

        validate_parameters_restrictions(
            parameters,
            restrictions
        )
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
    beta_x_y <- parameters$beta_x_y
    beta_u_y <- parameters$beta_u_y
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

    if (! is_lengthed_as(beta_g_x, m + k)) {
        stop("Parameter `beta_g_x` should have length `m + k`")
    }

    if (! is_positive(sigma2_u)) {
        stop("Parameter `sigma2_u` should be positive")
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
    beta_x_y <- parameters$beta_x_y
    beta_u_y <- parameters$beta_u_y
    sigma2_y <- parameters$sigma2_y

    hp_m <- hyper_parameters$m
    hp_k <- hyper_parameters$k
    hp_p <- hyper_parameters$p
    r2_g_x <- hyper_parameters$r2_g_x
    r2_u_x <- hyper_parameters$r2_u_x
    r2_g_y <- hyper_parameters$r2_g_y
    r2_u_y <- hyper_parameters$r2_u_y

    if (m != hp_m) {
        stop("Parameter `m` should be equal to hyper parameter `m`")
    }

    if (k != hp_k) {
        stop("Parameter `k` should be equal to hyper parameter `k`")
    }

    if (p != hp_p) {
        stop("Parameter `p` should be equal to hyper parameter `p`")
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
    beta_x_y <- parameters$beta_x_y
    beta_u_y <- parameters$beta_u_y
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

is_parameters <- function(
    x
) {
    result <- any(class(x) == "parameters")

    return(result)
}

print.parameters <- function(
    x
) {
    if (length(x$beta_g_x) == 1) {
        eta_g_x_string <- paste0("[", x$beta_g_x[1], "]")
    } else if (length(x$beta_g_x) == 2) {
        beta_g_x_string <- paste0("[", x$beta_g_x[1], ", ", x$beta_g_x[2], "]")
    } else {
        beta_g_x_string <- paste0("[", x$beta_g_x[1], ", ", "..., ", x$beta_g_x[length(x$beta_g_x)], "]")
    }

    cat("Parameters", "\n", sep = "")
    cat("\n", sep = "")
    cat("Number of causal G's (m): ", x$m, "\n", sep = "")
    cat("Number of null G's (k): ", x$k, "\n", sep = "")
    cat("Minor allele frequency of G's (p): ", x$p, "\n", sep = "")
    cat("U intercept (alpha_u): ", x$alpha_u, "\n", sep = "")
    cat("U noise variance (sigma2_u): ", x$sigma2_u, "\n", sep = "")
    cat("X intercept (alpha_x): ", x$alpha_x, "\n", sep = "")
    cat("Causal effect of G's on X (beta_g_x): ", beta_g_x_string, "\n", sep = "")
    cat("Causal effect of U on X (beta_u_x): ", x$beta_u_x, "\n", sep = "")
    cat("X noise variance (sigma2_x): ", x$sigma2_x, "\n", sep = "")
    cat("Y intercept (alpha_y): ", x$alpha_y, "\n", sep = "")
    cat("Causal effect of X on Y (beta_x_y): ", x$beta_x_y, " (targeted causal effect)", "\n", sep = "")
    cat("Causal effect of U on Y (beta_u_y): ", x$beta_u_y, "\n", sep = "")
    cat("Y noise variance (sigma2_y): ", x$sigma2_y, sep = "")
}
