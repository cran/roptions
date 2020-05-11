#' @title Option Greek and Estimated Premium of Put Option
#' @description Calculate the Option Greek of a Contract and Estimated Premium of Contract
#' @param s Spot Price of Underlying Asset
#' @param k Exercise Price of Contract
#' @param t Time to Expiration
#' @param sd Volatality
#' @param r Risk free rate of return
#' @param d Divident Yield (use cont.rate()), Default: 0
#' @return Output gives the Option Greek of a Option Contract. Also the Premium of the contract is estimated.
#' @details "Greeks" is a term used in the options market to describe the different dimensions of risk involved in taking an options position. These Greeks are calculated in this function along with the premium of the option contract using the BSM Model.
#' @examples
#' put.estimate(100, 105, 0.25, 0.35, 0.0488)
#' @rdname put.estimate
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
put.estimate = function(s, k, t, sd, r, d = 0){
  d1 = (log(s/k) + (r - d + (sd^2)/2) * t) / (sd * sqrt(t))
  d2 = d1 - (sd * sqrt(t))
  nd1 = pnorm(d1)
  nd2 = pnorm(d2)
  p = (k * exp((-r * t)) * pnorm(-d2)) - (s *exp(-d * t)* pnorm(-d1))
  Delta = exp(-d * t) * (nd1 - 1)
  Gamma = ((1/sqrt(2 * pi)) * exp(-1 * (((d1 ^ 2)/2))))/ (s * sd * sqrt(t))
  Theta = ((-1 * s * (1/(sqrt(2 * pi))) * exp(-1 * (d1^2/2)) * sd) / (2 * sqrt(t))) + (r * k * exp(-r * t) * pnorm(-d2))
  Vega = (s * sqrt(t) * (1/sqrt(2 * pi)) * exp(-1 * ((d1 ^ 2)/2)))
  Rho = -1/100 * (k * t * exp(-1 * r * t) * pnorm(-d2))
  intrinsic.value = max(k - s,0)
  speculative.premium = p - intrinsic.value
  df2 = data.frame(estimate = c('premium.est', 'd1', 'd2', 'n(d1)', 'n(d2)', 'intrinsic.value', 'speculative.premium', 'Delta', 'Gamma', 'Theta', 'Vega','Rho'), est.put.option = c(p, d1, d2, nd1, nd2,  intrinsic.value, speculative.premium, Delta, Gamma, Theta, Vega, Rho))
  print(df2)
}
