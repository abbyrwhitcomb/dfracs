#' Bootstrap Matrix of Correlation Standard Errors
#'
#' Computes a matrix of bootstrap-estimated standard errors for pairwise correlations.
#'
#' @param data A numeric data frame or matrix.
#' @param k Number of bootstrap replications.
#'
#' @return A symmetric matrix of standard errors, with variable names as row and column labels.
#' @export
#'
#' @examples
#' company_names <- c("Citi", "PFE", "GM")
#' k <- 500
#' set.seed(1052)
#' bootstrap_matrix(w_logret[, company_names], k)
bootstrap_matrix <- function(data, k) {
  set.seed(1052)
  var_num <- ncol(data)
  se_matrix <- matrix(NA, nrow = var_num, ncol = var_num)
  for (i in 1:var_num) {
    for (j in i:var_num) {
      se <- bootstrap(data[, i], data[, j], k)
      se_matrix[i, j] <- se
      se_matrix[j, i] <- se
    }
  }
  labels <- colnames(data)
  rownames(se_matrix) <- labels
  colnames(se_matrix) <- labels
  return(se_matrix)
}
