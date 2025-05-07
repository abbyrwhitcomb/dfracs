#' Test Difference Between Two Coefficients in a Linear Model
#'
#' Performs a t-test on the first two coefficients of a linear model to check if they differ significantly.
#'
#' @param model An object returned by `lm()`, representing a fitted linear model.
#'
#' @return A p-value indicating whether the first two coefficients are significantly different.
#'
#' @export
#'
#' @examples
#' model <- lm(PFE ~ date_category, data = pfizer)
#' t_test(model)
#'
t_test <- function(model) {
  mu_1 <- model$coefficients[1]
  mu_2 <- model$coefficients[2]
  se_1 <- summary(model)$coefficients[1, 2]
  se_2 <- summary(model)$coefficients[2, 2]
  t <- (mu_1 - mu_2) / sqrt(se_1^2 + se_2^2)
  df <- model$df.residual
  pval <- 2 * (1 - pt(abs(t), df))
  return(pval)
}


