#' Bootstrap Standard Errors for CAPM Estimates
#'
#' This function uses a nonparametric bootstrap procedure to estimate the standard errors
#' of CAPM-related quantities: alpha, beta, Sharpe ratio, and Treynor ratio.
#' It resamples the data with replacement and fits the CAPM model in each bootstrap sample.
#'
#' @param stock A numeric vector of monthly returns for a single stock.
#' @param market A numeric vector of monthly market returns (e.g., S&P 500).
#' @param rf A numeric vector of monthly risk-free rates (e.g., 3-month T-bill rates).
#' @param B Integer. Number of bootstrap replications (default is 500).
#'
#' @return A named numeric vector with bootstrap standard errors for:
#' \describe{
#'   \item{alpha_se}{Standard error of alpha (intercept from CAPM).}
#'   \item{beta_se}{Standard error of beta (slope from CAPM).}
#'   \item{sharpe_se}{Standard error of the Sharpe ratio.}
#'   \item{treynor_se}{Standard error of the Treynor ratio.}
#' }
#'
#' @export
#'
#' @examples
#'for (i in 1:ncol(stocks)) {
#'  boot_est[i, 2:5] <- bootstrap_capm(stocks[, i], market, rf)}
#'print(boot_est)
#'
bootstrap_capm <- function(stock, market, rf, B = 500) {

  set.seed(1052)
  n <- length(stock)
  alpha_vals <- numeric(B)
  beta_vals <- numeric(B)
  sharpe_vals <- numeric(B)
  treynor_vals <- numeric(B)

  for (b in 1:B) {
    boot_index <- sample(1:n, replace = TRUE)
    r_stock <- stock[boot_index]
    r_market <- market[boot_index]
    r_rf <- rf[boot_index]
    excess_stock <- r_stock - r_rf
    excess_market <- r_market - r_rf

    model <- lm(excess_stock ~ excess_market)
    alpha_vals[b] <- coef(model)[1]
    beta_vals[b] <- coef(model)[2]
    R_bar <- mean(excess_stock)
    sharpe_vals[b] <- R_bar / sd(excess_stock)
    treynor_vals[b] <- R_bar / coef(model)[2]

    alpha_se = sd(alpha_vals)
    beta_se = sd(beta_vals)
    sharpe_se = sd(sharpe_vals)
    treynor_se = sd(treynor_vals)
  }
  return(c(alpha_se, beta_se, sharpe_se, treynor_se))
}

