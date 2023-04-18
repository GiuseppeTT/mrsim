is_discrete <- function(
    x
) {
    result <- is_equal(x, as.integer(x))

    return(result)
}

is_lengthed_as <- function(
    x,
    n
) {
    result <- is_equal(length(x), n)

    return(result)
}


is_dimentioned_as <- function(
    x,
    row_count,
    column_count
) {
    result <- is_equal(nrow(x), row_count) && is_equal(ncol(x), column_count)

    return(result)
}

is_in_bound <- function(
    x,
    lower = - Inf,
    upper = + Inf,
    lower_strict = FALSE,
    upper_strict = FALSE
) {
    if (lower_strict) {
        compare_lower <- `<`
    } else {
        compare_lower <- `<=`
    }

    if (upper_strict) {
        compare_upper <- `<`
    } else {
        compare_upper <- `<=`
    }

    result <- compare_lower(lower, x) && compare_upper(x, upper)

    return(result)
}

is_positive <- function(
    x,
    strict = FALSE
) {
    lower <- 0
    upper <- + Inf
    lower_strict <- strict
    upper_strict <- FALSE

    result <- is_in_bound(
        x,
        lower,
        upper,
        lower_strict,
        upper_strict
    )

    return(result)
}

is_in_zero_one <- function(
    x,
    zero_strict = FALSE,
    one_strict = FALSE
) {
    lower <- 0
    upper <- 1
    lower_strict <- zero_strict
    upper_strict <- one_strict

    result <- is_in_bound(
        x,
        lower,
        upper,
        lower_strict,
        upper_strict
    )

    return(result)
}
