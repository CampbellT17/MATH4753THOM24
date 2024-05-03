#' @title My NCurve
#'
#' @importFrom stats dnorm pnorm
#' @export
#'
#' @importFrom graphics curve polygon
#'
#' @param mu Mean
#' @param sigma Standard Deviation
#' @param a Upper Limit of Integration
#'
#' @return 	displays the curve, the shaded area between the curve, and the
#' x-axis from -∞ to x=a, and calculate the area (probability, P(X<=a)) which is
#' released to the command-line in a list.
#'
#' @export
#'
#' @examples
#' \dontrun{myncurve(mu=10,sigma=5, a=3)}
#'
myncurve <- function(mu, sigma, a){
  x <- seq(mu - 3 * sigma, a, length.out = 100)
  y <- dnorm(x, mean = mu, sd = sigma)
  plot <- curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma),
                main = paste("Probability P(X <= ", a, ") =", round(pnorm(a, mean = mu, sd = sigma), 4)))
  polygon(x = c(x, rev(x)), y = c(rep(0, length(x)), rev(y)), col = "skyblue") # Corrected polygon definition
  list(mu = mu, sigma = sigma, Prob = pnorm(a, mean = mu, sd = sigma))
}
