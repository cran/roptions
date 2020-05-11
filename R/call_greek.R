#' @title Specified Call Option Greek
#' @description Calculate the Specified Option Greek of a Contract
#' @param s Spot Price of Underlying Asset
#' @param k Exercise Price of Contract
#' @param t Time to Expiration
#' @param sd Volatality
#' @param r Risk free rate of return
#' @param greek Character String of the greek to be calculated
#' @param d Divident Yield (use cont.rate()), Default: 0
#' @return Output gives the Specified Greek of a Option Contract.
#' @details Delta represents the rate of change between the option's price and a $1 change in the underlying asset's price. Theta represents the rate of change between the option price and time, or time sensitivity - sometimes known as an option's time decay. Gamma represents the rate of change between an option's delta and the underlying asset's price. Vega represents the rate of change between an option's value and the underlying asset's implied volatility. Rho represents the rate of change between an option's value and a 1% change in the interest rate.
#' @examples
#' call.greek('delta', 100, 105, 0.25, 0.35, 0.0488)
#' @rdname call.greek
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
call.greek = function(greek = c('delta', 'gamma', 'theta', 'vega', 'rho'), s, k, t, sd, r, d = 0){
  if(!is.character(greek)){
    print('unidentified greek. greek argument must be characher strings')
  }
  else if(greek == 'delta'){
    call.delta(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
  else if(greek == 'gamma'){
    call.gamma(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
  else if(greek == 'theta'){
    call.theta(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
  else if(greek == 'vega'){
    call.vega(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
  else if(greek == 'rho'){
    call.rho(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
}
