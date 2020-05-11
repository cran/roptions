#' @title Specified Minor Option Greek
#' @description Calculate the Specified Minor Option Greek of a Contract
#' @param s Spot Price of Underlying Asset
#' @param k Exercise Price of Contract
#' @param t Time to Expiration
#' @param sd Volatality
#' @param r Risk free rate of return
#' @param minorgreek Character String of the minor greek to be calculated
#' @param d Divident Yield (use cont.rate()), Default: 0
#' @return Output gives the Specified Minor Greek of a Option Contract.
#' @details Vomma is the rate at which the vega of an option will react to volatility in the market. In options trading, Lamba is the Greek letter assigned to variable which tells the ratio of how much leverage an option is providing as the price of that option changes.
#' @examples
#' put.minorgreek('lambda', 100, 105, 0.25, 0.35, 0.0488)
#' @rdname put.minorgreek
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
put.minorgreek = function(minorgreek = c('lambda', 'vomma'), s, k, t, sd, r, d = 0){
  d1 = (log(s/k) + (r - d + (sd^2)/2) * t) / (sd * sqrt(t))
  d2 = d1 - (sd * sqrt(t))
  lambda = (exp(-d * t) * (pnorm(d1) - 1)) * (s / ((k * exp((-r * t)) * pnorm(-d2)) - (s * exp(-d * t) * pnorm(-d1))))
  vomma = s * sqrt(t) * (exp(-d1^2/2)/ (sqrt(2 * pi)))
  if(!is.character(minorgreek)){
    print('minor greek is not a character string')
  }
  else if(minorgreek == 'lambda'){
    data.frame(lambda.put = lambda)
  }
  else if (minorgreek == 'vomma'){
    data.frame(vomma.option = vomma)
  }
}
