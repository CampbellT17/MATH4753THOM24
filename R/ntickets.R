#' @title ntickets
#'
#' @importFrom stats pbinom uniroot
#'
#' @param N Number of seats available on a flight
#' @param gamma acceptable risk (or probability) of overbooking a flight
#' @param p probability of a customer that buys a ticket actually showing up
#' to the flight
#'
#' @return This function finds the optimal number of tickets for an airline to
#' sell in order to have a full flight (N seats filled), taking into account
#' gamma risk of accidentally overbooking and p probability of a customer who
#' buys a ticket actually showing up to the flight.
#' @export
#'
#' @examples
#' \dontrun{ntickets(N=400,gamma=0.02,p=0.95)}
#'
#'
ntickets <- function(N, gamma, p) {
  # Define the number of trials
  n_trials <- 1.1 * N

  # Initialize needed variables and vectors
  min_diff <- Inf
  nd <- NULL
  n_values <- N:n_trials
  objective_values_discrete <- rep(NA, length(n_values))

  # Calculate the discrete objective function values for different values of n
  for (i in 1:length(n_values)) {
    objective_values_discrete[i] <- abs(pbinom(N, n_values[i], p) - (1 - gamma))
  }

  # Find the value of nd that minimizes the discrete objective function
  min_index <- which.min(objective_values_discrete)
  nd <- n_values[min_index]

  # Find the value of nc that is optimal under a continuous normal function
  objective_values_continuous <- function(n) {1-gamma-pnorm(N+0.5,n*p,sqrt(n*p*(1-p)))}
  nc <- uniroot(objective_values_continuous, c(N, n_trials))
  nc <- nc$root

  # Generate the continuous objective function values vector
  objective_values_continuous <- sapply(n_values, objective_values_continuous)

  # Print a named list containing nd, nc, N, p, and gamma
  cat("Optimal values:\n")
  print(list(nd = nd, nc=nc, N = N, p = p, gamma = gamma))

  # Plot the discrete objective function versus n
  plot(n_values, objective_values_discrete, type = "b",
       xlab = "n", ylab = "Objective Function - Discrete",
       main = paste("Objective Function vs n to Find Optimal Tickets \nSold (", nd,
                    "), gamma = ", gamma, ", \nN = ", N, " - Discrete", sep = ""),
       xlim = c(N, n_trials),
       pch = 19)
  abline(v = nd, col = "red", lty = 2)

  # Add a green horizontal line at 0
  abline(h = 0, col = "green")

  # Plot the continuous objective function versus n
  plot(n_values, objective_values_continuous, type = "l",
       xlab = "n", ylab = "Objective Function - Continuous",
       main = paste("Objective Function vs n to Find Optimal Tickets \nSold (", round(nc,4),
                    "), gamma = ", gamma, ", \nN = ", N, " - Continuous", sep = ""),
       xlim = c(N, n_trials),
       pch = 19)
  abline(v = nc, col = "red", lty = 2)

  # Add a green horizontal line at 0
  abline(h = 0, col = "green")
}
