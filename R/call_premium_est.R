#' @title Estimated Premium of Option Contract
#' @description Calculate the Estimated Premium of Option Contract
#' @param s Spot Price of Underlying Asset
#' @param k Exercise Price of Contract
#' @param t Time to Expiration
#' @param sd Volatality
#' @param r Risk free rate of return
#' @param d Divident Yield (use cont.rate()), Default: 0
#' @return Output gives the Estimated Premium of a Option Contract.
#' @details Estimate is calculated based on Black-Scholes Model. The Black Scholes model, also known as the Black-Scholes-Merton (BSM) model, is a mathematical model for pricing an options contract.
#' @examples
#' call.premium.est(100, 105, 0.25, 0.35, 0.0488)
#' @rdname call.premium.est
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
call.premium.est = function(s, k, t, sd, r, d = 0){
  d1 = (log(s/k) + (r - d + (sd^2)/2) * t) / (sd * sqrt(t))
  d2 = d1 - (sd * sqrt(t))
  c = (s * exp(-d * t) * pnorm(d1)) - (k * exp((-r * t)) * pnorm(d2))
  data.frame(call.premium.est = c)
}
