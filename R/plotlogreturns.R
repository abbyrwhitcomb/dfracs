#' Plot Log Returns Over Time
#'
#' Creates a time series plot of log returns for a selected company.
#'
#' @param data A data frame containing log returns and a date column.
#' @param company_name A string: the column name of the company to plot.
#' @param date_col A string: the column name containing date or time values.
#' @param unit A string describing the return frequency (e.g., "Weekly").
#'
#' @return A base R plot.
#' @export
#'
#' @examples
#' pfizer <- w_logret[, c(1, 3)]
#' pfizer_before <- pfizer[1:896,]
#' plotlogreturns(pfizer_before, "PFE", "Date", "Weekly (before March 8, 1999)")
plotlogreturns <- function(data, company_name, date_col, unit) {
  company_vals <- data[[company_name]]
  time_vals <- data[[date_col]]
  plot(time_vals, company_vals,
       type = "l", lwd = 0.75,
       main = paste(unit, "Log Returns of", company_name),
       xlab = "Time",
       ylab = "Log Returns")
}
