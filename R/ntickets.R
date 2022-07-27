#' Optimum ticket sales models for airline tickets. Determines the optimal value for ticket sales when customers don't arrive based on gamma, the likelihood of overbooking.
#'
#' @param N the number of seats available on the plane.
#' @param gamma the likelihood of overbooking
#' @param p the probability that a customer arrives to board the plane
#'
#' @return a named list containing: nd the number of tickets to sell under the discrete model, nc the number of tickets to sell under the continuous model, N the number of seats available, p the probability a customer arrives to board, and gamma the likelihood of overbooking.
#' @export
#'
#' @examples ntickets(N = 400, gamma = 0.02, p = 0.95)
ntickets <- function(N = 200, gamma = 0.02, p = 0.95) {

  # The step size of the continuous function for graphing
  cont_delta <- 0.01

  # Calculate a reasonable upper bound for the search for the optimum number of
  # tickets to sell. The interval of the search will be,
  # (N, N/p)
  upper_bound <- N + (2 * N * ((1 - p)/p))

  # Generate a "continuous" range of values for graphing the Normal approximation.
  x <- seq(from = N, to = upper_bound, by = cont_delta)

  # Generate a discrete range of values for graphing the discrete objective function.
  obj.dsz <- N:upper_bound

  # Calculate the discrete objective function.
  obj.disc <- (1 - gamma - stats::pbinom(N, size = obj.dsz, prob = p))

  # Calculate the continuous objective function (mean = np, sigma = sqrt(npq))
  # Apply +0.5 endpoint correction (N + 0.5) to include the Nth value's full
  # probability
  obj.cont <- (1 - gamma - stats::pnorm(N + 0.5, mean = (x * p), sd = sqrt((x * p * (1 - p)))))

  # Determine the optimum number of tickets 'n' in each case (discrete vs. continuous)
  nd <- N - 1 + which.min(abs(obj.disc))
  nc <- stats::optimize(continuousObjective, c(N, upper_bound), N = N, p = p, gamma = gamma)$minimum

  # Plot the results
  plot(obj.dsz, obj.disc, type = "b", bg = "blue", pch = 21, main = "", xlab = "", ylab = "")
  graphics::abline(h = 0, col = "red", lwd = 2)
  graphics::abline(v = nd, col = "red", lwd = 2)
  dTitle <- paste0("Objective Vs n to find optimal tickets sold \n(", nd, ") gamma=", gamma, " p=", p, " N=", N, " discrete")
  graphics::title(main = dTitle,
        xlab = "n",
        ylab = "Objective")

  plot(x, obj.cont, type = "l", main = "", xlab = "", ylab = "")
  graphics::abline(h = 0, col = "blue", lwd = 2)
  graphics::abline(v = nc, col = "blue", lwd = 2)
  cTitle <- paste0("Objective Vs n to find optimal tickets sold \n(", nc, ") gamma=", gamma, " p=", p, " N=", N, " continuous")
  graphics::title(main = cTitle,
        xlab = "n",
        ylab = "Objective")

  # Return the requested named list.
  list(nd = nd,
       nc = nc,
       N = N,
       p = p,
       gamma = gamma)
}


# Internal function definition to enable optimize() call in ntickets().
continuousObjective <- function(x, N = 200, p = 0.95, gamma = 0.02) {
  abs(1 - gamma - stats::pnorm(N + 0.5, mean = (x * p), sd = sqrt((x * p * (1 - p)))))
}
