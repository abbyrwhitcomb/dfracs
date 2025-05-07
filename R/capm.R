#' Compute CAPM diagnostics: alpha, beta, Sharpe, Treynor (with delta method)
#'
#' @param stock_returns A data frame or matrix of asset returns (each column is a stock).
#' @param market A numeric vector of market returns (e.g., S&P 500).
#' @param rf A numeric vector of risk-free rates (e.g., 3-month T-bill).
#'
#' @return A data frame with alpha, beta, their 95% CIs, Sharpe and Treynor ratios (with SEs)
#' @export
#'
#' @examples
#' stocks <- m_logret[, -1]
#' market <- m_sp500ret[, 2]
#' rf <- m_sp500ret[, 3]
#' capm_est <- capm(stocks, market, rf)
#'
capm <- function(stock_returns, market, rf) {
  excess_market <- market - rf
  excess_stocks <- sweep(stock_returns, 1, rf)
  n_assets <- ncol(excess_stocks)
  n_obs <- nrow(excess_stocks)
  results <- data.frame(
    stock = colnames(excess_stocks),
    alpha = NA, beta = NA,
    alpha_lwr = NA, alpha_upr = NA,
    beta_lwr = NA, beta_upr = NA,
    sharpe = NA, sharpe_se = NA,
    treynor = NA, treynor_se = NA
  )
  for (i in 1:n_assets) {
    model <- lm(excess_stocks[, i] ~ excess_market)
    coefs <- coef(model)
    cov_matrix <- vcov(model)
    ci <- confint(model)
    alpha <- coefs[1]
    beta <- coefs[2]
    R_bar <- mean(excess_stocks[, i])

    sd_total <- sd(excess_stocks[, i])
    sharpe <- R_bar / sd_total
    sharpe_se <- sd_total / sqrt(n_obs) / sd_total

    treynor <- R_bar / beta
    d_g <- matrix(c(0, -R_bar / (beta^2)), ncol = 1)
    treynor_var <- t(d_g) %*% cov_matrix %*% d_g
    treynor_se <- sqrt(treynor_var)

    results[i, ] <- c(
      colnames(excess_stocks)[i],
      alpha, beta,
      ci[1, 1], ci[1, 2],
      ci[2, 1], ci[2, 2],
      sharpe, sharpe_se,
      treynor, treynor_se
    )
  }
  return(results)
}
