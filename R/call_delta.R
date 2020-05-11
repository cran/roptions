#' @title Call Delta
#' @description Calculate the Delta (Option Greek) of a Contract
#' @param s Spot Price of Underlying Asset
#' @param k Exercise Price of Contract
#' @param t Time to Expiration
#' @param sd Volatality
#' @param r Risk free rate of return
#' @param d Divident Yield (use cont.rate()), Default: 0
#' @return Oupput gives the delta of a Option Contract.
#' @details Delta represents the rate of change between the option's price and a $1 change in the underlying asset's price.
#' @examples
#' call.delta(100, 105, 0.25, 0.35, 0.0488)
#' @rdname call.delta
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
call.delta = function(s, k, t, sd, r, d = 0){
  d1 = (log(s/k) + (r - d + (sd^2)/2) * t) / (sd * sqrt(t))
  d2 = d1 - (sd * sqrt(t))
  nd1 = pnorm(d1)
  nd2 = pnorm(d2)
  Delta = nd1
  data.frame(Delta = Delta)
}
