#' Standard Error
#'
#' This function allows you to calculate standard error
#' @param x A single vector.
#' @keywords standard error
#' @export
#' @examples
#' x <- rnorm(100)
#' se(x)

se <- function(x, na.rm = TRUE) {
  ifelse(na.rm == TRUE,
    sd(x, na.rm = TRUE)/sqrt(length(na.omit(x))),
    sd(x)/sqrt(length(x)))
}

#' Cross-Validated Correlation
#'
#' This function allows you to run a correlation using leave-one-out cross-validation.
#' @param x A single vector.
#' @param y A single vector.
#' @return An average correlation coefficient and the p-value for that coefficient.
#' @keywords cross-validation, correlation
#' @export
#' @examples
#' x <- rnorm(n = 25, mean = 0, sd = 1)
#' y <- rnorm(n = 25, mean = 25, sd = 5)
#' corr_cv(x, y)

# Would like to eventually get p values
corr_cv <- function(x, y) {
  corval <- vector()
  for (i in 1:length(x)) {
    corval[i] <- cor(x[-i], y[-i], use = "pairwise.complete.obs")
  }
  mean_corr <- mean(corval)
  return(mean_corr)
}



