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

    fit_g_x <- fit_multiple_simple_lm(g, x)
    fit_g_y <- fit_multiple_simple_lm(g, y)

    summary_statistics <- data.frame(
        snp = seq_len(snp_count),
        n = n,
        beta_g_x = fit_g_x$betas,
        beta_se_g_x = fit_g_x$beta_ses,
        p_value_g_x = fit_g_x$p_values,
        f_statistic_g_x = fit_g_x$f_statistics,
        r2_g_x = fit_g_x$r2s,
        beta_g_y = fit_g_y$betas,
        beta_se_g_y = fit_g_y$beta_ses,
        p_value_g_y = fit_g_y$p_values,
        f_statistic_g_y = fit_g_y$f_statistics,
        r2_g_y = fit_g_y$r2s
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
