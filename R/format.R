format_vector <- function(
    x
) {
    if (length(x) == 1) {
        formatted_x <- paste0("[", x[1], "]")
    } else if (length(x) == 2) {
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
    if (nrow(x) == 1 && ncol(x) == 1) {
        formatted_x <- paste0("[[", x[1, 1], "]]")
    } else if (nrow(x) == 2 && ncol(x) == 1) {
        formatted_x <- paste0("[[", x[1, 1], ", ", x[2, 1], "]]")
    } else if (nrow(x) == 1 && ncol(x) == 2) {
        formatted_x <- paste0("[[", x[1, 1], ", ", x[1, 2], "]]")
    } else {
        formatted_x <- paste0("[[", x[1, 1], ", ..., ", x[nrow(x), ncol(x)], "]]")
    }

    suffix <- paste0("(", nrow(x), " x ", ncol(x), " sized matrix)")
    formatted_x <- paste0(formatted_x, " ", suffix)

    return(formatted_x)
}
