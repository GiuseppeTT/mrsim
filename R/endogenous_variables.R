calculate_endogenous_variables <- function(
    exogenous_variables
) {
    gprime <- exogenous_variables$gprime
    e_u <- exogenous_variables$e_u
    e_x <- exogenous_variables$e_x
    e_y <- exogenous_variables$e_y

    parameters <- attr(exogenous_variables, "parameters")
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

    n <- attr(exogenous_variables, "n")

    g <- (gprime - 2 * p) / sqrt(2 * p * (1 - p))
    u <- alpha_u + sqrt(sigma2_u) * e_u
    x <- alpha_x + beta_u_x * u + g %*% beta_g_x + sqrt(sigma2_x) * e_x
    y <- alpha_y + beta_u_y * u + beta_x_y * x + sqrt(sigma2_y) * e_y

    endogenous_variables <- new_endogenous_variables(
        g,
        u,
        x,
        y,
        parameters,
        n
    )

    validate_endogenous_variables(endogenous_variables)

    return(endogenous_variables)
}

new_endogenous_variables <- function(
    g,
    u,
    x,
    y,
    parameters,
    n
) {
    stopifnot(is.matrix(g))
    stopifnot(is.matrix(u))
    stopifnot(is.matrix(x))
    stopifnot(is.matrix(y))
    stopifnot(is_parameters(parameters))
    stopifnot(is.numeric(n))

    values <- list(
        g = g,
        u = u,
        x = x,
        y = y
    )

    endogenous_variables <- structure(
        values,
        class = "endogenous_variables",
        parameters = parameters,
        n = n
    )

    return(endogenous_variables)
}

validate_endogenous_variables <- function(
    endogenous_variables
) {
    g <- endogenous_variables$g
    u <- endogenous_variables$u
    x <- endogenous_variables$x
    y <- endogenous_variables$y

    parameters <- attr(endogenous_variables, "parameters")
    n <- attr(endogenous_variables, "n")

    if (! is_dimentioned_as(g, n, parameters$m + parameters$k)) {
        stop("Endogenous variable `g` should have `n` rows and `m + k` columns")
    }

    if (! is_dimentioned_as(u, n, 1)) {
        stop("Endogenous variable `u` should have `n` rows and `1` columns")
    }

    if (! is_dimentioned_as(x, n, 1)) {
        stop("Endogenous variable `x` should have `n` rows and `1` columns")
    }

    if (! is_dimentioned_as(y, n, 1)) {
        stop("Endogenous variable `y` should have `n` rows and `1` columns")
    }
}

#' @export
is_endogenous_variables <- function(
    x
) {
    result <- any(class(x) == "endogenous_variables")

    return(result)
}

#' @export
print.endogenous_variables <- function(
    x,
    ...
) {
    args <- list(...)
    level <- args$level %||% 1
    header <- paste0(rep("#", level), collapse = "")

    cat(header, " Endogenous variables", "\n", sep = "")
    cat("\n", sep = "")
    cat("G data (g): ", format_matrix(x$g), "\n", sep = "")
    cat("U data (u): ", format_matrix(x$u), "\n", sep = "")
    cat("X data (x): ", format_matrix(x$x), "\n", sep = "")
    cat("Y data (y): ", format_matrix(x$y), "\n", sep = "")
}
