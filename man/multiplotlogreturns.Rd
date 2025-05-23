% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiplotlogreturns.R
\name{multiplotlogreturns}
\alias{multiplotlogreturns}
\title{Plot Multiple Log Return Series}
\usage{
multiplotlogreturns(data, company_names, date_col, unit)
}
\arguments{
\item{data}{A data frame containing log return columns and a date column.}

\item{company_names}{A character vector of column names for companies to plot.}

\item{date_col}{A string: the name of the date/time column.}

\item{unit}{A string describing the return frequency (e.g., "Weekly").}
}
\value{
Multiple base R plots.
}
\description{
Creates time series plots for multiple companies' log returns.
}
\examples{
d_logret <- read.table(system.file("extdata", "d_logret_12stocks.txt", package = "dfracs"), header = TRUE)
d_logret$X. <- as.Date(as.character(d_logret$X.), format = "\%Y\%m\%d")
d_logret_ret_only <- d_logret[, -1]

colnames(d_logret)
# [1] "X." "aapl" "adbe" "adp" "amd" "dell" "gtw" "hp" "ibm" "msft" "orcl" "sunw" "yhoo"

company_names <- c("aapl", "adbe", "adp", "amd", "dell", "gtw", "hp", "ibm", "msft", "orcl", "sunw", "yhoo")
multiplotlogreturns(d_logret, company_names, "X.", "Weekly")

}
