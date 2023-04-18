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

#' @export
is_sample <- function(
    x
) {
    result <- any(class(x) == "sample")

    return(result)
}

#' @export
print.sample <- function(
    x,
    ...
) {
    n <- attr(x, "n")

    cat("Sample", "\n", sep="")
    cat("\n", sep="")
    cat("Sample size (n): ", n, "\n", sep = "")
    cat("\n", sep="")
    print(x$exogenous_variables)
    cat("\n", sep="")
    print(x$endogenous_variables)
}
