#' @title Call Rho
#' @description Calculate the Rho (Option Greek) of Option Contract
#' @param s Spot Price of Underlying Asset
#' @param k Exercise Price of Contract
#' @param t Time to Expiration
#' @param sd Volatality
#' @param r Risk free rate of return
#' @param d Divident Yield (use cont.rate()), Default: 0
#' @return Output gives the Rho of a Option Contract.
#' @details Rho represents the rate of change between an option's value and a 1% change in the interest rate.
#' @examples
#' call.rho(100, 105, 0.25, 0.35, 0.0488)
#' @rdname call.rho
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
call.rho = function(s, k, t, sd, r, d = 0){
  d1 = (log(s/k) + (r - d + (sd^2)/2) * t) / (sd * sqrt(t))
  d2 = d1 - (sd * sqrt(t))
  nd1 = pnorm(d1)
  nd2 = pnorm(d2)
  Rho = (k * t * exp(-1 * r * t) * nd2)
  data.frame(Rho = Rho)
}
