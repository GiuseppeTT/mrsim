#' @export
generate_dataset <- function(
    parameters,
    n_1,
    n_2,
    n_3
) {
    sample_1 <- generate_sample(parameters, n_1)
    sample_2 <- generate_sample(parameters, n_2)
    sample_3 <- generate_sample(parameters, n_3)

    dataset <- new_dataset(
        sample_1,
        sample_2,
        sample_3,
        parameters,
        n_1,
        n_2,
        n_3
    )

    validate_dataset(dataset)

    return(dataset)
}

new_dataset <- function(
    sample_1,
    sample_2,
    sample_3,
    parameters,
    n_1,
    n_2,
    n_3
) {
    stopifnot(is_sample(sample_1))
    stopifnot(is_sample(sample_2))
    stopifnot(is_sample(sample_3))
    stopifnot(is_parameters(parameters))
    stopifnot(is.numeric(n_1))
    stopifnot(is.numeric(n_2))
    stopifnot(is.numeric(n_3))

    values <- list(
        sample_1 = sample_1,
        sample_2 = sample_2,
        sample_3 = sample_3
    )

    sample <- structure(
        values,
        class = "dataset",
        parameters = parameters,
        n_1 = n_1,
        n_2 = n_2,
        n_3 = n_3
    )

    return(sample)
}

validate_dataset <- function(
    dataset
) {
}

#' @export
is_dataset <- function(
    x
) {
    result <- any(class(x) == "dataset")

    return(result)
}

#' @export
print.dataset <- function(
    x,
    ...
) {
    args <- list(...)
    level <- args$level %||% 1
    header <- paste0(rep("#", level), collapse = "")

    cat(header, " Dataset", "\n", sep="")
    cat("\n", sep="")
    print(x$sample_1, level = level + 1)
    cat("\n", sep="")
    print(x$sample_2, level = level + 1)
    cat("\n", sep="")
    print(x$sample_3, level = level + 1)
}
