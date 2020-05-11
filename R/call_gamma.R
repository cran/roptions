#' @title Call Gamma
#' @description Calculate the Gamma (Option Greek) of a Contract
#' @param s Spot Price of Underlying Asset
#' @param k Exercise Price of Contract
#' @param t Time to Expiration
#' @param sd Volatality
#' @param r Risk free rate of return
#' @param d Divident Yield (use cont.rate()), Default: 0
#' @return Output gives the Gamma of a Option Contract.
#' @details Gamma represents the rate of change between an option's delta and the underlying asset's price.
#' @examples
#' call.gamma(100, 105, 0.25, 0.35, 0.0488)
#' @rdname call.gamma
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
call.gamma = function(s, k, t, sd, r, d = 0){
  d1 = (log(s/k) + (r - d + (sd^2)/2) * t) / (sd * sqrt(t))
  d2 = d1 - (sd * sqrt(t))
  nd1 = pnorm(d1)
  nd2 = pnorm(d2)
  Gamma = ((1/sqrt(2 * pi)) * exp(-1 * (((d1 ^ 2)/2))))/ (s * sd * sqrt(t))
  data.frame(Gamma = Gamma)
}
