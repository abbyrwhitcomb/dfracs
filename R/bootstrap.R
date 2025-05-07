#' Bootstrap Standard Error of Correlation
#'
#' Uses bootstrapping to estimate the standard error of the correlation between two variables.
#'
#' @param var1 A numeric vector.
#' @param var2 A numeric vector.
#' @param k Number of bootstrap replications.
#'
#' @return A single numeric value: estimated standard error.
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' y <- 0.5 * x + rnorm(100, sd = 0.5)
#' bootstrap(x, y, k = 1000)
#'
bootstrap <- function(var1, var2, k) {
  set.seed(1052)
  resampled_cor <- numeric(k)
  n <- length(var1)
  for (i in 1:k) {
    obs <- sample(1:n, replace = TRUE)
    resampled_cor[i] <- cor(var1[obs], var2[obs])
  }
  se = sd(resampled_cor)
  return(se)
}
