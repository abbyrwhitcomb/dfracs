#' Alpha Hypothesis Test for CAPM regression
#'
#' Test H0: alpha = 0 for multiple CAPM regressions
#'
#' @param stocks A data frame or matrix of asset returns (each column is a stock).
#' @param market A numeric vector of market returns (e.g., S&P 500).
#' @param rf A numeric vector of risk-free rates (e.g., 3-month T-bill).
#'
#' @return A data frame with alpha, alpha SE, t-statistic, and p-value for each stock.
#' @export
#'
#' @examples
#' stocks <- m_logret[, -1]
#' market <- m_sp500ret[, 2]
#' rf <- m_sp500ret[, 3]
#' alpha_test <- alpha_capm(stocks, market, rf)
#' print(alpha_test)
#'
alpha_capm <- function(stocks, market, rf) {
  n_assets <- ncol(stocks)
  results <- data.frame(
    stock = colnames(stocks),
    alpha = NA,
    alpha_se = NA,
    t_stat = NA,
    p_value = NA
  )
  for (i in 1:n_assets) {
    excess_stock <- stocks[, i] - rf
    excess_market <- market - rf

    model <- lm(excess_stock ~ excess_market)
    summary_model <- summary(model)

    alpha_hat <- coef(model)[1]
    alpha_se <- summary_model$coefficients[1, 2]
    t_val <- alpha_hat / alpha_se
    p_val <- 2 * pt(-abs(t_val), df = summary_model$df[2])
    results[i, 2:5] <- c(alpha_hat, alpha_se, t_val, p_val)
  }
  return(results)
}
