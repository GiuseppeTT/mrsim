#' @export
summarize_dataset <- function(
    dataset
) {
    sample_1_statistics <- summarize_sample(dataset$sample_1)
    sample_2_statistics <- summarize_sample(dataset$sample_2)
    sample_3_statistics <- summarize_sample(dataset$sample_3)

    colnames(sample_1_statistics) <- paste0("sample_1_", colnames(sample_1_statistics))
    colnames(sample_2_statistics) <- paste0("sample_2_", colnames(sample_2_statistics))
    colnames(sample_3_statistics) <- paste0("sample_3_", colnames(sample_3_statistics))

    summary_statistics <- cbind(
        sample_1_statistics,
        sample_2_statistics,
        sample_3_statistics
    )

    colnames(summary_statistics)[colnames(summary_statistics) == "sample_1_snp"] <- "snp"
    summary_statistics <- summary_statistics[, ! (colnames(summary_statistics) %in% c("sample_2_snp", "sample_3_snp"))]

    return(summary_statistics)
}


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

#' @export
get_beta_x_y.dataset <- function(
    x,
    ...
) {
    parameters <- attr(x, "parameters")
    beta_x_y <- get_beta_x_y(parameters)

    return(beta_x_y)
}
