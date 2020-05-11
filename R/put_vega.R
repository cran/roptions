#' @title Put Vega
#' @description Calculate the Vega (Option Greek) of Option Contract
#' @param s Spot Price of Underlying Asset
#' @param k Exercise Price of Contract
#' @param t Time to Expiration
#' @param sd Volatality
#' @param r Risk free rate of return
#' @param d Divident Yield (use cont.rate()), Default: 0
#' @return Output gives the Vega of a Option Contract.
#' @details Vega represents the rate of change between an option's value and the underlying asset's implied volatility.
#' @examples
#' put.vega(100, 105, 0.25, 0.35, 0.0488)
#' @rdname put.vega
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
put.vega = function(s, k, t, sd, r, d = 0){
  d1 = (log(s/k) + (r - d + (sd^2)/2) * t) / (sd * sqrt(t))
  d2 = d1 - (sd * sqrt(t))
  nd1 = pnorm(d1)
  nd2 = pnorm(d2)
  Vega = (s * sqrt(t) * (1/sqrt(2 * pi)) * exp(-1 * ((d1 ^ 2)/2)))
  data.frame(put.vega = Vega)
}
