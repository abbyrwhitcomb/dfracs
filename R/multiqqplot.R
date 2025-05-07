#' Q-Q Plots for Multiple Stocks
#'
#' Displays Q-Q plots of weekly log returns to assess normality for each stock.
#'
#' @param data A data frame containing log return columns.
#' @param company_names A character vector of column names for companies to check.
#'
#' @return Q-Q plots for each stock in separate base R graphics panels.
#' @export
#'
#' @examples
#' company_names <- c("Citi", "PFE", "GM")
#' multiqqplot(w_logret, company_names)
multiqqplot <- function(data, company_names) {
  for (company_name in company_names) {
    qqplot_comp(data, company_name)
  }
}
