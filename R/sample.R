#' Generate sample
#'
#' Generate a sample of size `n` from the model defined by `parameters`.
#'
#' @param parameters An object of class `parameters`
#' @param n Sample size. It should be a positive integer
#'
#' @return An object of class `sample`
#' @export
generate_sample <- function(
    parameters,
    n
) {
    exogenous_variables <- generate_exogenous_variables(parameters, n)
    endogenous_variables <- calculate_endogenous_variables(exogenous_variables)

    sample <- new_sample(
        exogenous_variables,
        endogenous_variables,
        parameters,
        n
    )

    validate_sample(sample)

    return(sample)
}

new_sample <- function(
    exogenous_variables,
    endogenous_variables,
    parameters,
    n
) {
    stopifnot(is.list(exogenous_variables))
    stopifnot(is.list(endogenous_variables))
    stopifnot(is_parameters(parameters))
    stopifnot(is.numeric(n))

    values <- list(
        exogenous_variables = exogenous_variables,
        endogenous_variables = endogenous_variables
    )

    sample <- structure(
        values,
        class = "sample",
        parameters = parameters,
        n = n
    )

    return(sample)
}

validate_sample <- function(
    sample
) {
}

#' Test if the object is `sample`
#'
#' Returns TRUE for `sample` or subclasses thereof and FALSE for all other objects.
#'
#' @param x An object
#'
#' @return A logical value
#'
#' @export
is_sample <- function(
    x
) {
    result <- is_subclass_of(x, "sample")

    return(result)
}

#' @export
print.sample <- function(
    x,
    ...
) {
    n <- attr(x, "n")

    args <- list(...)
    level <- args$level %||% 1
    header <- paste0(rep("#", level), collapse = "")

    cat(header, " Sample", "\n", sep="")
    cat("\n", sep="")
    cat("Sample size (n): ", n, "\n", sep = "")
    cat("\n", sep="")
    print(x$exogenous_variables, level = level + 1)
    cat("\n", sep="")
    print(x$endogenous_variables, level = level + 1)
}

#' @export
get_beta_x_y.sample <- function(
    x,
    ...
) {
    parameters <- attr(x, "parameters")
    beta_x_y <- get_beta_x_y(parameters)

    return(beta_x_y)
}
