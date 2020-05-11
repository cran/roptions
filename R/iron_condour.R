#' @title Iron Condour Strategy Function
#' @description This function can be used to develop a Iron Condour Strategy.
#' @param k_long_call Excercise Price of Long call Option
#' @param k_short_call Excercise Price of Short call Option
#' @param k_long_put Excercise Price of Long Put Option
#' @param k_short_put Excercise Price of Short Put Option
#' @param c1 Premium of Long call Option
#' @param c2 Premium of Short call Option
#' @param p1 Premium of Long Put Option
#' @param p2 Premium of Short Put Option
#' @param llimit Lower limit of stock price at Expiration., Default: 20
#' @param ulimit Upper Limit of Stock Price at Expiration, Default: 20
#' @return OUTPUT_DESCRIPTION Returns the profit/loss generated from the strategy along with the profit/loss of individual contract and an interactive graph for the same.
#' @details An Iron condor is an options strategy created with four options consisting of two puts (one long and one short) and two calls (one long and one short), and four strike prices, all with the same expiration date.
#' @examples
#' iron.condour(100, 95, 105, 102, 2.3, 1.25, 3.2, 2.3)
#' @rdname iron.condour
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
iron.condour = function(k_long_call, k_short_call, k_long_put, k_short_put, c1, c2, p1, p2, llimit = 20, ulimit = 20){
  stock_price_at_expiration = round((k_long_call - llimit)):round((ulimit + k_long_call))
  long_call = (map_dbl(stock_price_at_expiration , .f = ~max(.x - k_long_call,0))) - c1
  short_call =  (-1* map_dbl(stock_price_at_expiration, .f = ~max(.x - k_short_call,0))) + c2
  long_put = (map_dbl(stock_price_at_expiration, .f = ~max(k_long_put - .x,0))) - p1
  short_put = put_option = ( -1 * map_dbl(stock_price_at_expiration, .f = ~max(k_short_put - .x,0))) + p2
  profit_loss = long_call + short_call + long_put + short_put
  df = data.frame(stock_price_at_expiration, long_call, short_call, long_put, short_put, profit_loss)
  p1 = ggplot(data = df) +
    geom_line(aes(x = stock_price_at_expiration, y = long_call, colour = 'long_call')) +
    geom_line(aes(x = stock_price_at_expiration, y = short_call, colour = 'short_call')) +
    geom_line(aes(x = stock_price_at_expiration, y = long_put, colour = 'long_put')) +
    geom_line(aes(x = stock_price_at_expiration, y = short_put, colour = 'short_put')) +
    geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
    labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Iron Condour Plot', color = 'Option contract') +
    scale_colour_manual('', breaks = c('long_call', 'short_call', 'profit_loss', 'long_put', 'short_put'), values = c('blue', 'red', 'black', 'green', 'yellow'))
  print(df)
  print(ggplotly(p1))
}
