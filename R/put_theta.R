#' @title Put Theta
#' @description Calculate the Theta (Option Greek) of Option Contract
#' @param s Spot Price of Underlying Asset
#' @param k Exercise Price of Contract
#' @param t Time to Expiration
#' @param sd Volatality
#' @param r Risk free rate of return
#' @param d Divident Yield (use cont.rate()), Default: 0
#' @return Output gives the Theta of a Option Contract.
#' @details Theta represents the rate of change between the option price and time, or time sensitivity - sometimes known as an option's time decay.
#' @examples
#' put.theta(100, 105, 0.25, 0.35, 0.0488)
#' @rdname put.theta
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
put.theta = function(s, k, t, sd, r, d = 0){
  d1 = (log(s/k) + (r - d + (sd^2)/2) * t) / (sd * sqrt(t))
  d2 = d1 - (sd * sqrt(t))
  nd1 = pnorm(d1)
  nd2 = pnorm(d2)
  Theta = ((-1 * s * (1/(sqrt(2 * pi))) * exp(-1 * (d1^2/2)) * sd) / (2 * sqrt(t))) + (r * k * exp(-r * t) * pnorm(-d2))
  data.frame(put.theta = Theta)
}
