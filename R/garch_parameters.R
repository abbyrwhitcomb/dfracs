#' Extract and summarize key GARCH(1,1) model parameters
#'
#' This function extracts parameter estimates from a fitted GARCH(1,1) model
#' and returns a formatted data frame of values.
#'
#' @param model A fitted GARCH(1,1) model object, typically returned by `rugarch::ugarchfit()`.
#'
#' @return A one-row data frame with the following columns:
#' \describe{
#'   \item{mu_10e4}{Mean return multiplied by 10,000.}
#'   \item{omega_10e6}{Omega multiplied by 1,000,000.}
#'   \item{alpha}{ARCH parameter (α₁).}
#'   \item{beta}{GARCH parameter (β₁).}
#'   \item{alpha_plus_beta}{Volatility persistence (α + β).}
#'   \item{HL}{Half-life of variance persistence, in days (or periods), using \eqn{\tau = 1 + \log(0.5)/\log(\alpha + \beta)}. Values above 100 are capped as "10^5+".}
#' }
#'
#' @examples
#' \dontrun{
#' library(rugarch)
#' spec <- ugarchspec(variance.model = list(model = "sGARCH"), mean.model = list(armaOrder = c(0,0)))
#' data <- rnorm(1000)
#' fit <- ugarchfit(spec, data = data)
#' garch_parameters(fit)
#' }
#'
#' @export
garch_parameters <- function(model) {
  coefs <- model@fit$coef
  mu <- coefs["mu"]
  omega <- coefs["omega"]
  alpha <- coefs["alpha1"]
  beta <- coefs["beta1"]
  persistence <- alpha + beta
  hl <- log(0.5) / log(persistence) + 1

  result <- data.frame(
    mu_10e4 = sprintf("%.2f", mu * 1e4),
    omega_10e6 = sprintf("%.2f", omega * 1e6),
    alpha = sprintf("%.3f", alpha),
    beta = sprintf("%.3f", beta),
    alpha_plus_beta = sprintf("%.6f", persistence),
    HL = ifelse(hl > 100, "10^5+", sprintf("%.2f", hl))
  )
  return(result)
}
