#' @title Bull/Bear Put Spread Strategy Function
#' @description This function can be used to develop a Bull/Bear Put Strategy.
#' @param long_put Premium of Long Put Option
#' @param short_put Premium of Short Put Option
#' @param k1 Excercise Price of Long Put Option
#' @param k2 Excercise Price of Short Put Option
#' @param llimit Lower limit of stock price at Expiration., Default: 20
#' @param ulimit Upper Limit of Stock Price at Expiration, Default: 20
#' @return OUTPUT_DESCRIPTION Returns the profit/loss generated from the strategy along with the profit/loss of individual contract and an interactive graph for the same.
#' @details The strategy uses two put options to form a range consisting of a high strike price and a low strike price.
#' @examples
#' put.spread(1.2, 3.2, 100, 105)
#' @rdname put.spread
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
put.spread = function(k1, k2, long_put, short_put,  llimit = 20, ulimit = 20){

  if(k1>k2){
    print('This is a Bear Spread because the excercise price of long put (k1) is greater than excercise price of short put (k2)')
  }else{
    print('This is a Bull Spread because the excercise price of long put (k1) is less than excercise price of short put (k2)')
  }
  stock_price_at_expiration = round((k1 - llimit)):round((ulimit + k1))
  long_put = (map_dbl(round((k1  - llimit)):round((ulimit + k1)), .f = ~max(k1 - .x,0))) - long_put
  short_put = ( -1 * map_dbl(round((k1  - llimit)):round((ulimit + k1)), .f = ~max(k2 - .x,0))) + short_put
  profit_loss = long_put + short_put
  df = data.frame(stock_price_at_expiration, long_put, short_put, profit_loss)
  p1 = ggplot(data = df) +
    geom_line(aes(x = stock_price_at_expiration, y = long_put, colour = 'long_put')) +
    geom_line(aes(x = stock_price_at_expiration, y = short_put, colour = 'short_put')) +
    geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
    labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Bull/Bear Put Spread Plot', color = 'Option contract') +
    scale_colour_manual('', breaks = c('long_put', 'short_put', 'profit_loss'), values = c('blue', 'red', 'black'))
  print(ggplotly(p1))
  print(df)
}
