#' Compute the Mean-Variance Efficient Frontier
#'
#' Given an expected return vector and a covariance matrix, this function computes
#' the efficient frontier by solving the mean-variance optimization problem for
#' a sequence of target expected returns.
#'
#' The optimization minimizes portfolio variance subject to two equality constraints:
#' - The portfolio weights must sum to 1
#' - The expected return must equal a specified target
#'
#' @param mu A numeric vector of expected returns (length = number of assets).
#' @param Sigma A positive-definite covariance matrix of asset returns.
#' @param targets A numeric vector of target expected returns for which to compute portfolio risk/return.
#'
#' @return A list with two numeric vectors:
#' \describe{
#'   \item{risk}{Standard deviation of portfolio returns (volatility) for each target return.}
#'   \item{return}{Expected return of the efficient portfolio for each target return.}
#' }
#'
#' @examples
#' returns <- matrix(rnorm(600), ncol = 6)
#' mu <- colMeans(returns)
#' Sigma <- cov(returns)
#' targets <- seq(min(mu), max(mu), length.out = 50)
#' frontier(mu, Sigma, targets)
#'
#' @export
frontier <- function(mu, Sigma, targets) {
  A <- cbind(rep(1, length(mu)), mu)
  Sigma_inv <- solve(Sigma)
  risks <- numeric(length(targets))
  rets <- numeric(length(targets))
  for (i in seq_along(targets)) {
    b <- c(1, targets[i])
    lambda <- solve(t(A) %*% Sigma_inv %*% A) %*% b
    w <- Sigma_inv %*% A %*% lambda
    rets[i] <- sum(w * mu)
    risks[i] <- sqrt(t(w) %*% Sigma %*% w)
  }
  return(list(risk = risks, return = rets))
}
