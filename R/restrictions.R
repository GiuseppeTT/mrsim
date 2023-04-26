#' Define restrictions
#'
#' Define restrictions for simulating mendelian randomization (MR) data.
#'
#' Additional constraints may be posed to the restrictions values in order to garantee that the simulated data is valid.
#'
#' @return An object of class `restrictions`
#'
#' @export
define_restrictions <- function(
) {
    restrictions <- new_restrictions(
        mean_g  = 0,
        var_g = 1,
        mean_u = 0,
        var_u = 1,
        mean_x = 0,
        var_x = 1,
        mean_y = 0,
        var_y = 1
    )

    validate_restrictions(restrictions)

    return(restrictions)
}

new_restrictions <- function(
    mean_g,
    var_g,
    mean_u,
    var_u,
    mean_x,
    var_x,
    mean_y,
    var_y
) {
    stopifnot(is.numeric(mean_g))
    stopifnot(is.numeric(var_g))
    stopifnot(is.numeric(mean_u))
    stopifnot(is.numeric(var_u))
    stopifnot(is.numeric(mean_x))
    stopifnot(is.numeric(var_x))
    stopifnot(is.numeric(mean_y))
    stopifnot(is.numeric(var_y))

    values <- list(
        mean_g = mean_g,
        var_g = var_g,
        mean_u = mean_u,
        var_u = var_u,
        mean_x = mean_x,
        var_x = var_x,
        mean_y = mean_y,
        var_y = var_y
    )

    restrictions <- structure(
        values,
        class = "restrictions"
    )

    return(restrictions)
}

validate_restrictions <- function(
    restrictions
) {
    mean_g <- restrictions$mean_g
    var_g <- restrictions$var_g
    mean_u <- restrictions$mean_u
    var_u <- restrictions$var_u
    mean_x <- restrictions$mean_x
    var_x <- restrictions$var_x
    mean_y <- restrictions$mean_y
    var_y <- restrictions$var_y

    if (! is_positive(var_g)) {
        stop("Restriction `var_g` should be positive")
    }

    if (! is_positive(var_u)) {
        stop("Restriction `var_u` should be positive")
    }

    if (! is_positive(var_x)) {
        stop("Restriction `var_x` should be positive")
    }

    if (! is_positive(var_y)) {
        stop("Restriction `var_y` should be positive")
    }
}

#' Test if an object is of class `restrictions`
#'
#' Returns TRUE for `restrictions` or subclasses thereof and FALSE for all other objects.
#'
#' @param x An object
#'
#' @return A logical value
#'
#' @export
is_restrictions <- function(
    x
) {
    result <- is_subclass_of(x, "restrictions")

    return(result)
}

#' @export
print.restrictions <- function(
    x,
    ...
) {
    args <- list(...)
    level <- args$level %||% 1
    header <- paste0(rep("#", level), collapse = "")

    cat(header, " Restrictions", "\n", sep = "")
    cat("\n", sep = "")
    cat("G mean (mean_g): ", x$mean_g, "\n", sep = "")
    cat("G variance (var_g): ", x$var_g, "\n", sep = "")
    cat("U mean (mean_u): ", x$mean_u, "\n", sep = "")
    cat("U variance (var_u): ", x$var_u, "\n", sep = "")
    cat("X mean (mean_x): ", x$mean_x, "\n", sep = "")
    cat("X variance (var_X): ", x$var_x, "\n", sep = "")
    cat("Y mean (mean_y): ", x$mean_y, "\n", sep = "")
    cat("Y variance (var_Y): ", x$var_y, sep = "")
}
