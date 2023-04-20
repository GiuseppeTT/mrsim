#' @export
calculate_summary_statistics <- function(
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

        fit_g_x <- summary(stats::lm(x ~ gi))
        beta_g_x[i] <- fit_g_x$coefficients[2, "Estimate"]
        beta_se_g_x[i] <- fit_g_x$coefficients[2, "Std. Error"]
        p_value_g_x[i] <- fit_g_x$coefficients[2, "Pr(>|t|)"]
        f_statistic_g_x[i] <- fit_g_x$fstatistic["value"]
        r2_g_x[i] <- fit_g_x$r.squared

        fit_g_y <- summary(stats::lm(y ~ gi))
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
calculate_summary_parameters <- function(
    parameters
) {
    m <- parameters$m
    k <- parameters$k
    p <- parameters$p
    alpha_u <- parameters$alpha_u
    sigma2_u <- parameters$sigma2_u
    alpha_x <- parameters$alpha_x
    beta_g_x <- parameters$beta_g_x
    beta_u_x <- parameters$beta_u_x
    sigma2_x <- parameters$sigma2_x
    alpha_y <- parameters$alpha_y
    beta_x_y <- parameters$beta_x_y
    beta_u_y <- parameters$beta_u_y
    sigma2_y <- parameters$sigma2_y

    hyper_parameters <- attr(parameters, "hyper_parameters")
    hp_m <- hyper_parameters$m
    hp_k <- hyper_parameters$k
    hp_p <- hyper_parameters$p
    r2_g_x <- hyper_parameters$r2_g_x
    r2_u_x <- hyper_parameters$r2_u_x
    r2_g_y <- hyper_parameters$r2_g_y
    r2_u_y <- hyper_parameters$r2_u_y

    snp_count <- m + k
    real_beta_g_x <- beta_g_x
    real_r2_g_x <- r2_g_x
    real_beta_g_y <- beta_g_x * beta_x_y
    real_r2_g_y <- r2_g_y

    summary_parameters <- data.frame(
        snp = seq_len(snp_count),
        real_beta_g_x = real_beta_g_x,
        real_r2_g_x = real_r2_g_x,
        real_beta_g_y = real_beta_g_y,
        real_r2_g_y = real_r2_g_y
    )

    return(summary_parameters)
}
