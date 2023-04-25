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
