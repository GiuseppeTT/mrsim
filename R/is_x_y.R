is_equal <- function(
    x,
    y
) {
    result <- x == y

    return(result)
}

is_close <- function(
    x,
    y,
    ...
) {
    result <- isTRUE(all.equal(x, y, ...))

    return(result)
}

is_same_length <- function(
    x,
    y
) {
    result <- is_equal(length(x), length(y))

    return(result)
}
