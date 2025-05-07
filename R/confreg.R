#' Confidence Region Threshold for Linear Regression Coefficients
#'
#' Computes the scalar threshold that defines the 95% confidence ellipsoid
#' for the coefficient vector \eqn{\beta} in a linear regression model.
#'
#' @param model A linear model object returned by \code{lm()}.
#'
#' @return A numeric value representing the threshold for the 95% confidence region of \eqn{\beta}.
#'
#' @export
#'
#' @examples
#' company_names <- c("GM", "Toyota", "Ford")
#' data_subset <- w_logret[, company_names]
#' model <- lm(GM ~ Toyota + Ford, data = data_subset)
#' confreg(model)
#'
confreg <- function(model) {
  X <- model.matrix(model)
  n <- nrow(X)
  p <- length(coef(model))
  sigma_sq <- summary(model)$sigma^2
  f_val <- qf(0.95, df1 = p, df2 = n - p)
  region <- p * sigma_sq * f_val
  return(region)
}
