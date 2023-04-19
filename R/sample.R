#' @export
summarize_sample <- function(
    sample
) {
    endogenous_variables <- sample$endogenous_variables
    g <- endogenous_variables$g
    x <- endogenous_variables$x
    y <- endogenous_variables$y

    n <- attr(sample, "n")

    snp_count <- ncol(g)
    beta_g_x <- numeric(snp_count)
    beta_se_g_x <- numeric(snp_count)
    p_value_g_x <- numeric(snp_count)
    f_statistic_g_x <- numeric(snp_count)
    r2_g_x <- numeric(snp_count)
    beta_g_y <- numeric(snp_count)
    beta_se_g_y <- numeric(snp_count)
    p_value_g_y <- numeric(snp_count)
    f_statistic_g_y <- numeric(snp_count)
    r2_g_y <- numeric(snp_count)
    for (i in seq_len(snp_count)) {
        gi <- g[, i, drop = FALSE]

        fit_g_x <- summary(lm(x ~ gi))
        beta_g_x[i] <- fit_g_x$coefficients[2, "Estimate"]
        beta_se_g_x[i] <- fit_g_x$coefficients[2, "Std. Error"]
        p_value_g_x[i] <- fit_g_x$coefficients[2, "Pr(>|t|)"]
        f_statistic_g_x[i] <- fit_g_x$fstatistic["value"]
        r2_g_x[i] <- fit_g_x$r.squared

        fit_g_y <- summary(lm(y ~ gi))
        beta_g_y[i] <- fit_g_y$coefficients[2, "Estimate"]
        beta_se_g_y[i] <- fit_g_y$coefficients[2, "Std. Error"]
        p_value_g_y[i] <- fit_g_y$coefficients[2, "Pr(>|t|)"]
        f_statistic_g_y[i] <- fit_g_y$fstatistic["value"]
        r2_g_y[i] <- fit_g_y$r.squared
    }

    summary_statistics <- data.frame(
        snp = seq_len(snp_count),
        n = n,
        beta_g_x = beta_g_x,
        beta_se_g_x = beta_se_g_x,
        p_value_g_x = p_value_g_x,
        f_statistic_g_x = f_statistic_g_x,
        r2_g_x = r2_g_x,
        beta_g_y = beta_g_y,
        beta_se_g_y = beta_se_g_y,
        p_value_g_y = p_value_g_y,
        f_statistic_g_y = f_statistic_g_y,
        r2_g_y = r2_g_y
    )

    return(summary_statistics)
}

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
