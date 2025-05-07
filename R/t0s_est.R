#' Estimate Structural Breakpoints in CAPM Betas for Multiple Stocks
#'
#' This function estimates the optimal \( t_0 \) in time series CAPM-style regressions for each stock.
#' The model allows for a change in beta before and after the estimated \( t_0 \), selecting the split that minimizes
#' the residual sum of squares (RSS) across all valid breakpoints.
#'
#' @param stocks A data frame or matrix of asset returns (each column is a stock, rows are time).
#' @param market A numeric vector of market returns over the same time span as `stocks`.
#' @param rf A numeric vector of risk-free rates over the same time span as `stocks`.
#' @param dates A vector of `Date` objects corresponding to each row of `stocks`, used to label `t0_date`.
#'
#' @return A data frame with one row per stock, including:
#' \describe{
#'   \item{stock}{The stock name (column name from `stocks`).}
#'   \item{t0_index}{The row index corresponding to the estimated breakpoint.}
#'   \item{t0_date}{The actual date of the estimated breakpoint.}
#' }
#'
#' @export
#'
#' @examples
#' stocks <- m_logret[, -1]
#' market <- m_sp500ret[, 2]
#' rf <- m_sp500ret[, 3]
#' t0s_est(stocks, market, rf, m_logret$Date)
#'
t0s_est <- function(stocks, market, rf, dates) {
  n <- nrow(stocks)
  t0_range <- 1:n
  result <- data.frame(
    stock = colnames(stocks),
    t0_index = NA,
    t0_date = as.Date(NA))
  for (i in seq_along(stocks)) {
    r <- stocks[, i]
    rss <- numeric(length(t0_range))
    for (j in seq_along(t0_range)) {
      t0 <- t0_range[j]
      before <- (market - rf) * (1:n < t0)
      after  <- (market - rf) * (1:n >= t0)
      model <- lm((r - rf) ~ before + after)
      rss[j] <- sum(resid(model)^2)
    }
    best_index <- t0_range[which.min(rss)]
    result[i, "t0_index"] <- best_index
    result[i, "t0_date"] <- dates[best_index]
  }
  return(result)
}
