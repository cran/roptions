#' @title Short Strangle Strategy Function
#' @description This function can be used to develop a Short Strangle Strategy.
#' @param c Premium of Short call Option
#' @param p Premium of Short Put Option
#' @param k_call Excercise Price of Short call Option
#' @param k_put Excercise Price of Short Put Option
#' @param llimit Lower limit of stock price at Expiration., Default: 20
#' @param ulimit Upper Limit of Stock Price at Expiration, Default: 20
#' @return OUTPUT_DESCRIPTION Returns the profit/loss generated from the strategy along with the profit/loss of individual contract and an interactive graph for the same.
#' @details A strangle is an options strategy where the investor holds a position in both a call and a put option with different strike prices, but with the same expiration date and underlying asset.
#' @examples
#' strangle.short(1.2, 3.2, 100, 105)
#' @rdname strangle.short
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
strangle.short = function(c, p, k_call, k_put, ulimit = 10, llimit = 10){
  stock_price_at_expiration = round((k_call - llimit)):round((ulimit + k_call))
  call_option = (-1* map_dbl(round((k_call - llimit)):round((ulimit + k_call)), .f = ~max(.x - k_call,0))) + c
  put_option = ( -1 * map_dbl(round((k_call  - llimit)):round((ulimit + k_call)), .f = ~max(k_put - .x,0))) + p
  profit_loss = call_option + put_option
  df = data.frame(stock_price_at_expiration, call_option, put_option, profit_loss)
  p2 = ggplot(data = df) +
    geom_line(aes(x = stock_price_at_expiration, y = call_option, colour = 'call_option')) +
    geom_line(aes(x = stock_price_at_expiration, y = put_option, colour = 'put_option')) +
    geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
    labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Short Straddle Plot', color = 'Option contract') +
    scale_colour_manual('', breaks = c('call_option', 'put_option', 'profit_loss'), values = c('blue', 'red', 'black'))
  print(ggplotly(p2))
  print(df)
}
