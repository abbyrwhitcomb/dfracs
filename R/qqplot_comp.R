#' Q-Q Plot for Log Returns
#'
#' Plots a Q-Q plot to assess normality of log returns for a single company.
#'
#' @param data A data frame containing log return columns.
#' @param company_name A string: the column name of the company.
#'
#' @return A Q-Q plot.
#' @export
#'
#' @examples
#' company_name <- c("Citi")
#' qqplot_comp(w_logret, company_name)
#'
qqplot_comp <- function(data, company_name) {
  vals <- data[[company_name]]
  qqnorm(vals, main = paste("Q-Q Plot:", company_name))
  qqline(vals)
}
