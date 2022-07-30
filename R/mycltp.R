#' Demonstrate the Central Limit Theorem applied to the sampling distribution of the mean of the Poisson distribution.
#'
#' @param n the number of samples per sampling mean
#' @param iter the number of sampling means to calculate
#' @param lambda the lambda parameter for the lambda distribution in question
#' @param ... additional parameters for the primary histogram plot
#'
#' @return NULL, nothing, zilch.
#' @export
#'
#' @examples mycltp(n = 10, iter = 10000, lambda = 4)
mycltp <- function(n, iter, lambda = 10, ...) {

  # Take n*iter random samples from the Poisson distribution
  y <- stats::rpois(n * iter, lambda = lambda)

  # Populate a matrix with n rows and iter cols with the samples from the poisson dist.
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  # Apply the mean function columnwise
  w <- apply(data, 2, mean)

  # Generate a histogram to nab the maximum density from the samples
  # Add 10% of the max density to add a bit of margin to the plot
  param <- graphics::hist(w, plot = FALSE)
  ymax <- 1.1 * max(param$density)

  # Make a nice layout for graphing
  graphics::layout(matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow = TRUE))

  # Make the histogram, for real this time
  graphics::hist(w,
       freq = FALSE,
       ylim = c(0, ymax),
       col = grDevices::rainbow(max(w)),
       main = paste("Histogram of sample mean",
                    "\n",
                    "sample size= ",
                    n,
                    " iter=",
                    iter,
                    " lambda=",
                    lambda,
                    sep=""),
       xlab="Sample mean",
       ...) # Additional arguments passed to by the caller go here

  # Add a theoretical normal curve
  graphics::curve(stats::dnorm(x, mean = lambda, sd = sqrt(lambda/ n )),
        add = TRUE,
        col = "Red",
        lty = 2,
        lwd = 3)

  # Add a new plot for the barplot of y (since it is discrete)
  graphics::barplot(table(y) / (n * iter),
          col = grDevices::rainbow(max(y)),
          main = "Barplot of sampled y",
          ylab = "Rel. Freq",
          xlab = "y")

  x = 0:max(y)
  plot(x,
       stats::dpois(x, lambda = lambda),
       type = "h",
       lwd = 5,
       col = grDevices::rainbow(max(y)),
       main = "Probability function for Poisson",
       ylab = "Probability",
       xlab = "y")
}
