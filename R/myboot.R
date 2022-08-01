#' Bootstrap simulation technique
#'
#' @param iter the number of iterations to run
#' @param x the original sample to resample
#' @param fun the statistic to generate using bootstrap
#' @param alpha the desired confidence level
#' @param ... extra arguments
#'
#' @return a named list containing ci, the confidence interval, pte, the point estimate, fun, the function of the statistic to estimate, and the original sample x.
#' @export
#'
#' @examples myboot(iter = 10000, x = c(1), fun = "mean", alpha = 0.05)
myboot <- function(iter = 10000, x, fun = "mean", alpha = 0.05, ...) {

  n <- length(x)
  y <- sample(x, n * iter, replace = TRUE)

  rs.mat <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat <- apply(rs.mat, 2, fun)

  ci <- stats::quantile(xstat, c((alpha / 2), 1 - (alpha / 2)))

  para <- graphics::hist(xstat,
               freq = FALSE,
               las = 1,
               main = "Histogram of Bootstrap sample statistics",
               ...)

  mat <- matrix(x, nrow = length(x), ncol = 1, byrow = TRUE)

  pte <- apply(mat, 2, fun)
  graphics::abline(v = pte, lwd = 3, col = "Black")
  graphics::segments(ci[1], 0, ci[2], 0, lwd = 4)
  graphics::text(ci[1],
       0,
       paste("(", round(ci[1], 2), sep = ""),
       col = "Red",
       cex = 3)
  graphics::text(ci[2],
       0,
       paste(round(ci[2], 2), ")", sep = ""),
       col = "Red",
       cex = 3)

  graphics::text(pte, max(para$density) / 2, round(pte, 2), cex = 3)
  return(list(ci = ci, pte = pte, fun = fun, x = x))
}
