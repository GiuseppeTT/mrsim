#' @export
define_hyper_parameters <- function(
    m,
    k,
    p,
    r2_g_x,
    r2_u_x,
    r2_g_y,
    r2_u_y
) {
    hyper_parameters <- new_hyper_parameters(
        m = m,
        k = k,
        p = p,
        r2_g_x = r2_g_x,
        r2_u_x = r2_u_x,
        r2_g_y = r2_g_y,
        r2_u_y = r2_u_y
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
    r2_g_y,
    r2_u_y
) {
    stopifnot(is.numeric(m))
    stopifnot(is.numeric(k))
    stopifnot(is.numeric(p))
    stopifnot(is.numeric(r2_g_x))
    stopifnot(is.numeric(r2_u_x))
    stopifnot(is.numeric(r2_g_y))
    stopifnot(is.numeric(r2_u_y))

    values <- list(
        m = m,
        k = k,
        p = p,
        r2_g_x = r2_g_x,
        r2_u_x = r2_u_x,
        r2_g_y = r2_g_y,
        r2_u_y = r2_u_y
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
    r2_g_y <- hyper_parameters$r2_g_y
    r2_u_y <- hyper_parameters$r2_u_y

    if (! (is_positive(m) && is_discrete(m))) {
        stop("Hyper parameter `m` should be positive and discrete")
    }

    if (! (is_positive(k) && is_discrete(k))) {
        stop("Hyper parameter `k` should be positive and discrete")
    }

    if (! is_in_bound(p, 0, 0.5)) {
        stop("Hyper parameter `p` should be in the [0, 0.5] interval")
    }

    if (! is_in_bound(r2_g_x, 0, 1/m)) {
        stop("Hyper parameter `r2_g_x` should be in the [0, 1/m] interval")
    }

    if (! is_in_zero_one(r2_u_x)) {
        stop("Hyper parameter `r2_u_x` should be in the [0, 1] interval")
    }

    if (! is_in_bound(r2_g_y, 0, 1/m)) {
        stop("Hyper parameter `r2_g_y` should be in the [0, 1/m] interval")
    }

    if (! is_in_zero_one(r2_u_y)) {
        stop("Hyper parameter `r2_u_y` should be in the [0, 1] interval")
    }
}

#' @export
is_hyper_parameters <- function(
    x
) {
    result <- any(class(x) == "hyper_parameters")

    return(result)
}

#' @export
print.hyper_parameters <- function(
    x,
    ...
) {
    cat("Hyper parameters", "\n", sep = "")
    cat("\n", sep = "")
    cat("Number of causal G's (m): ", x$m, "\n", sep = "")
    cat("Number of null G's (k): ", x$k, "\n", sep = "")
    cat("Minor allele frequency of G's (p): ", x$p, "\n", sep = "")
    cat("Variance in X explained per G (r2_g_x): ", x$r2_g_x, "\n", sep = "")
    cat("Variance in X explained by U (r2_u_x): ", x$r2_u_x, "\n", sep = "")
    cat("Variance in Y explained per G (r2_g_y): ", x$r2_g_y, "\n", sep = "")
    cat("Variance in Y explained by U (r2_u_y): ", x$r2_u_y, sep = "")
}
