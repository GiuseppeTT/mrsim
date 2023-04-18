generate_exogenous_variables <- function(
    parameters,
    n
) {
    m <- parameters$m
    k <- parameters$k
    p <- parameters$p

    gprime <- rbinom(n = n * (m + k), size = 2, prob = p)
    gprime <- matrix(gprime, nrow = n, ncol = m + k, byrow = TRUE)

    e_u <- rnorm(n = n, mean = 0, sd = 1)
    e_u <- matrix(e_u, nrow = n, ncol = 1, byrow = TRUE)

    e_x <- rnorm(n = n, mean = 0, sd = 1)
    e_x <- matrix(e_x, nrow = n, ncol = 1, byrow = TRUE)

    e_y <- rnorm(n = n, mean = 0, sd = 1)
    e_y <- matrix(e_y, nrow = n, ncol = 1, byrow = TRUE)

    exogenous_variables <- new_exogenous_variables(
        gprime,
        e_u,
        e_x,
        e_y,
        parameters,
        n
    )

    validate_exogenous_variables(exogenous_variables)

    return(exogenous_variables)
}

new_exogenous_variables <- function(
    gprime,
    e_u,
    e_x,
    e_y,
    parameters,
    n
) {
    stopifnot(is.matrix(gprime))
    stopifnot(is.matrix(e_u))
    stopifnot(is.matrix(e_x))
    stopifnot(is.matrix(e_y))
    stopifnot(is_parameters(parameters))
    stopifnot(is.numeric(n))

    values <- list(
        gprime = gprime,
        e_u = e_u,
        e_x = e_x,
        e_y = e_y
    )

    exogenous_variables <- structure(
        values,
        class = "exogenous_variables",
        parameters = parameters,
        n = n
    )

    return(exogenous_variables)
}

validate_exogenous_variables <- function(
    exogenous_variables
) {
    gprime <- exogenous_variables$gprime
    e_u <- exogenous_variables$e_u
    e_x <- exogenous_variables$e_x
    e_y <- exogenous_variables$e_y

    parameters <- attr(exogenous_variables, "parameters")
    m <- parameters$m
    k <- parameters$k

    n <- attr(exogenous_variables, "n")

    if (! is_dimentioned_as(gprime, n, m + k)) {
        stop("Exogenous variable `gprime` should have `n` rows and `m + k` columns")
    }

    if (! is_dimentioned_as(e_u, n, 1)) {
        stop("Exogenous variable `e_u` should have `n` rows and `1` columns")
    }

    if (! is_dimentioned_as(e_x, n, 1)) {
        stop("Exogenous variable `e_x` should have `n` rows and `1` columns")
    }

    if (! is_dimentioned_as(e_y, n, 1)) {
        stop("Exogenous variable `e_y` should have `n` rows and `1` columns")
    }
}

#' @export
is_exogenous_variables <- function(
    x
) {
    result <- any(class(x) == "exogenous_variables")

    return(result)
}

#' @export
print.exogenous_variables <- function(
    x,
    ...
) {
    args <- list(...)
    level <- args$level %||% 1
    header <- paste0(rep("#", level), collapse = "")

    cat(header, " Exogenous variables", "\n", sep = "")
    cat("\n", sep = "")
    cat("Raw G data (gprime): ", format_matrix(x$gprime), "\n", sep = "")
    cat("U noise (e_u): ", format_matrix(x$e_u), "\n", sep = "")
    cat("X noise (e_x): ", format_matrix(x$e_x), "\n", sep = "")
    cat("Y noise (e_y): ", format_matrix(x$e_y), "\n", sep = "")
}
