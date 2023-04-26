format_vector <- function(
    x
) {
    if (is_lengthed_as(x, 1)) {
        formatted_x <- paste0("[", x[1], "]")
    } else if (is_lengthed_as(x, 2)) {
        formatted_x <- paste0("[", x[1], ", ", x[2], "]")
    } else {
        formatted_x <- paste0("[", x[1], ", ..., ", x[length(x)], "]")
    }

    suffix <- paste0("(", length(x), " sized vector)")
    formatted_x <- paste0(formatted_x, " ", suffix)

    return(formatted_x)
}

format_matrix <- function(
    x
) {
    if (is_dimentioned_as(x, 1, 1)) {
        formatted_x <- paste0("[[", x[1, 1], "]]")
    } else if (is_dimentioned_as(x, 2, 1)) {
        formatted_x <- paste0("[[", x[1, 1], ", ", x[2, 1], "]]")
    } else if (is_dimentioned_as(x, 1, 2)) {
        formatted_x <- paste0("[[", x[1, 1], ", ", x[1, 2], "]]")
    } else {
        formatted_x <- paste0("[[", x[1, 1], ", ..., ", x[nrow(x), ncol(x)], "]]")
    }

    suffix <- paste0("(", nrow(x), " x ", ncol(x), " sized matrix)")
    formatted_x <- paste0(formatted_x, " ", suffix)

    return(formatted_x)
}
