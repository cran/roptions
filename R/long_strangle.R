#' @title Long Strangle Strategy Function
#' @description This function can be used to develop a Long Strangle Strategy.
#' @param c Premium of Long call Option
#' @param p Premium of Long Put Option
#' @param k_call Excercise Price of Long call Option
#' @param k_put Excercise Price of Long Put Option
#' @param llimit Lower limit of stock price at Expiration., Default: 20
#' @param ulimit Upper Limit of Stock Price at Expiration, Default: 20
#' @return OUTPUT_DESCRIPTION Returns the profit/loss generated from the strategy along with the profit/loss of individual contract and an interactive graph for the same.
#' @details A strangle is an options strategy where the investor holds a position in both a call and a put option with different strike prices, but with the same expiration date and underlying asset.
#' @examples
#' strangle.long(1.2, 3.2, 100, 105)
#' @rdname strangle.long
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
strangle.long = function(c, p, k_call, k_put, ulimit = 10, llimit = 10){
  stock_price_at_expiration = round((k_call - llimit)):round((ulimit + k_call))
  call_option = (map_dbl(round((k_call - llimit)):round((ulimit + k_call)), .f = ~max(.x - k_call,0))) - c
  put_option = (map_dbl(round((k_call  - llimit)):round((ulimit + k_call)), .f = ~max(k_put - .x,0))) - p
  profit_loss = call_option + put_option
  df = data.frame(stock_price_at_expiration, call_option, put_option, profit_loss)
  p1 = ggplot(data = df) +
    geom_line(aes(x = stock_price_at_expiration, y = call_option, colour = 'call_option')) +
    geom_line(aes(x = stock_price_at_expiration, y = put_option, colour = 'put_option')) +
    geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
    labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Long Straddle Plot', color = 'Option contract') +
    scale_colour_manual('', breaks = c('call_option', 'put_option', 'profit_loss'), values = c('blue', 'red', 'black'))
  print(ggplotly(p1))
  print(df)
}
