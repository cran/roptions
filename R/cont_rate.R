#' @title Continous Rate
#' @description Converts nominal rate into Continously compounded Rate
#' @param r rate (nominal)
#' @param t number of compounding period
#' @return Generates Continuously Compounded Rate
#' @details Generates Continuously Compounded Rate
#' @examples
#' cont.rate(0.025, 4)
#' @rdname cont.rate
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
cont.rate = function(r, t){
  log((1 + r/t)^t)
}
