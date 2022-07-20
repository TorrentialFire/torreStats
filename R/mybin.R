#' Simulation many binomial experiments.
#' Perform a simulation of "iter" number of binomial experiments with n trials
#' per experiment where p is the probability of success in each trial.
#'
#' @param iter the number of experiments to run
#' @param n the number of trials per experiment
#' @param p the probability of a success in a trial
#'
#' @return a table of the relative frequencies for each number of successes possible (given n)
#' @export
#'
#' @examples mybin(iter = 10000, n = 10, p = 0.35)
mybin <- function(iter = 100, n = 10, p = 0.5) {

  # Initialize a matrix with n rows and iter cols filled with NA's. Each column
  # will hold the results of 1 iteration of the simulation (a binomial
  # experiment of n trials).
  sam.mat <- matrix(NA,
                    nrow = n,
                    ncol = iter,
                    byrow = TRUE)

  # Initialize a vector to store the number of successes in each trial
  succ <- vector(mode = "numeric", length = iter)

  # Run "iter" number of experiments, the higher this number is, the more
  # accurately the statistical values will match the theoretical values for
  # the binomial distribution.
  for(i in 1:iter) {

    # Generate a binomial experiment with n trials, store as the ith column
    # of the simulation matrix.
    sam.mat[,i] <- sample(c(1,0),
                          n,
                          replace = TRUE,
                          prob = c(p, 1 - p))

    # Count the number of successes in the current experiment, store it as the
    # ith entry in the success vector.
    succ[i] <- sum(sam.mat[,i])
  }
  print(mean(succ))

  # Generate a table of the frequency of each count of successes over the entire
  # simulation.
  succ.tab <- table(factor(succ, levels = 0:n))

  # Make a barplot of the relative frequency of the count of successes over the
  # entire simulation.

  graphics::barplot(succ.tab/(iter),
          col = grDevices::rainbow(n + 1),
          main = "Binomial simulation",
          xlab = "Number of successes")
  succ.tab/iter
}
