#' Get `beta_x_y`
#'
#' Returns the real causal effect of X on Y `beta_x_y`.
#'
#' @param x An object
#' @param ... Extra arguments. Currently not being used.
#'
#' @return A numeric value
#'
#' @export
get_beta_x_y <- function(
    x,
    ...
) {
    UseMethod("get_beta_x_y")
}
