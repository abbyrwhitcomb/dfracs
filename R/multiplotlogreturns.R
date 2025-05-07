#' Plot Multiple Log Return Series
#'
#' Creates time series plots for multiple companies' log returns.
#'
#' @param data A data frame containing log return columns and a date column.
#' @param company_names A character vector of column names for companies to plot.
#' @param date_col A string: the name of the date/time column.
#' @param unit A string describing the return frequency (e.g., "Weekly").
#'
#' @return Multiple base R plots.
#'
#' @examples
#' d_logret <- read.table(system.file("extdata", "d_logret_12stocks.txt", package = "dfracs"), header = TRUE)
#' d_logret$X. <- as.Date(as.character(d_logret$X.), format = "%Y%m%d")
#' d_logret_ret_only <- d_logret[, -1]
#'
#' colnames(d_logret)
#' # [1] "X." "aapl" "adbe" "adp" "amd" "dell" "gtw" "hp" "ibm" "msft" "orcl" "sunw" "yhoo"
#'
#' company_names <- c("aapl", "adbe", "adp", "amd", "dell", "gtw", "hp", "ibm", "msft", "orcl", "sunw", "yhoo")
#' multiplotlogreturns(d_logret, company_names, "X.", "Weekly")
#'
#' @export
multiplotlogreturns <- function(data, company_names, date_col, unit) {
  for (company_name in company_names) {
    plotlogreturns(data, company_name, date_col, unit)
  }
}
