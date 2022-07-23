#' Generates a plot of a normal curve with a specified µ and σ. Fills the area
#' under the curve up to 'a', and outputs the probability P(Y <= a) which is the
#' true area under the curve from -inf to 'a'.
#'
#' @param mu the mean of the normal distribution
#' @param sigma the standard deviation of the normal distribution
#' @param a the upper limit of the probability calculation (lower limit is always -inf)
#'
#' @return the numerical value of the probability (the area under the curve)
#' @export
#'
#' @examples myncurve(mu = 5, sigma = 2, a = 3)
myncurve = function(mu, sigma, a) {
  # Prevent the checker from presuming x is global.
  x <- NULL

  # Plot the normal distribution between µ - 3*σ and µ + 3*σ.
  ncur <- graphics::curve(stats::dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma))

  # Fill the area under the curve from -inf (cheat and use µ - 3*σ) to a
  poly_x <- c(mu - 3 * sigma, ncur$x[ncur$x <= a], a)
  poly_y <- c(             0, ncur$y[ncur$x <= a], 0)
  poly_pts <- list(x = poly_x, y = poly_y)
  graphics::polygon(poly_pts, col = "blue")

  # Calculate the true area under the curve, i.e. P(Y <= a))
  stats::pnorm(a, mean = mu, sd = sigma)
}
