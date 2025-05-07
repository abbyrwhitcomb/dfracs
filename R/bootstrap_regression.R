#' Bootstrap Standard Errors for Linear Regression Coefficients
#'
#' Performs nonparametric bootstrap resampling to estimate the standard errors
#' of the coefficients from a linear regression model.
#'
#' @param data A data frame containing the variables in the model.
#' @param model_form A formula specifying the linear regression model.
#' @param k An integer specifying the number of bootstrap samples to use.
#'
#' @return A numeric vector of bootstrap standard errors for each coefficient in the model.
#'
#' @export
#'
#' @examples
#' bootstrap_se <- bootstrap_regression(w_logret, GM ~ Toyota + Ford, k = 1000)
#'
bootstrap_regression <- function(data, model_form, k) {
  set.seed(1052)
  model <- lm(model_form, data = data)
  bootstrap_coefs <- matrix(NA, nrow = k, ncol = length(coef(model)))
  n <- nrow(data)
  for (i in 1:k) {
    obs <- sample(1:n, replace = TRUE)
    bootstrap_newdata <- data[obs, ]
    bootstrap_model <- lm(model_form, data = bootstrap_newdata)
    bootstrap_coefs[i, ] <- coef(bootstrap_model)
  }
  se <- apply(bootstrap_coefs, 2, sd)
  return(se)
}
