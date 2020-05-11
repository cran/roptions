#' @title Butterfly Put Spread Strategy Function
#' @description This function can be used to develop a Butterfly Put Spread Strategy
#' @param k1 Excercise Price of 1st Long Put Option (Long Spread)/ Excercise Price of 1st Short Put Option (Short Spread)
#' @param k2 Excercise Price of Short Put Option (Long Spread) / Excercise Price of Long Put Option (Short Spread)
#' @param k3 Excercise Price of 2nd Long Put Option (Long Spread) / Excercise Price of 2nd Short Put Option (Short Spread)
#' @param p1 Premium of 1st Long Put Option (Long Spread)/ Premium of 1st Short Put Option (Short Spread)
#' @param p2 Premium of Short Put Option (Long Spread) / Premium of Long Put Option (Short Spread)
#' @param p3 Premium of 2nd Long Put Option (Long Spread) / Premium of 2nd Short Put Option (Short Spread)
#' @param spread Type of Spread, Default: c("long", "short")
#' @param llimit Lower limit of stock price at Expiration., Default: 20
#' @param ulimit Upper Limit of Stock Price at Expiration, Default: 20
#' @return OUTPUT_DESCRIPTION Returns the profit/loss generated from the strategy along with the profit/loss of individual contract and an interactive graph for the same.
#' @details The long put butterfly spread is created by buying one put with a lower strike price, selling two at-the-money puts, and buying a put with a higher strike price. Net debt is created when entering the position. The short put butterfly spread is created by writing one out-of-the-money put option with a low strike price, buying two at-the-money puts, and writing an in-the-money put option at a higher strike price.
#' @examples
#' butterfly.put(100, 105, 95, 2.2, 3.2, 1.25, spread = 'long')
#' @rdname butterfly.put
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
butterfly.put = function(k1, k2, k3, p1, p2, p3, spread = c('long', 'short'), llimit = 20, ulimit = 20){

  if(!is.character(spread)){
    print('spread argument must be a character string')
  } else if(spread == 'long'){

  stock_price_at_expiration = round((k1 - llimit)):round((ulimit + k1))
  long_put1 = (map_dbl(stock_price_at_expiration , .f = ~max(k1 - .x,0))) - p1
  short_put1 = ( -1 * map_dbl(stock_price_at_expiration, .f = ~max(k2 - .x,0))) + p2
  short_put2 = short_put1
  long_put2 = (map_dbl(stock_price_at_expiration, .f = ~max(k3 - .x,0))) - p3
  profit_loss = long_put1 + short_put1 + short_put2 + long_put2
  df = data.frame(stock_price_at_expiration, long_put1, short_put1, short_put2, long_put2,  profit_loss)
  p1 = ggplot(data = df) +
    geom_line(aes(x = stock_price_at_expiration, y = long_put1, colour = 'long_put1')) +
    geom_line(aes(x = stock_price_at_expiration, y = short_put1, colour = 'short_put1')) +
    geom_line(aes(x = stock_price_at_expiration, y = short_put2, colour = 'short_put2')) +
    geom_line(aes(x = stock_price_at_expiration, y = long_put2, colour = 'long_put2')) +
    geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
    labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Long Butterfly Put Spread Plot', color = 'Option contract') +
    scale_colour_manual('', breaks = c('long_put1', 'short_put1','short_put2', 'long_put2', 'profit_loss'), values = c('blue', 'red', 'yellow', 'purple', 'black'))
  print(df)
  print(ggplotly(p1))

  } else if(spread == 'short'){



    stock_price_at_expiration = round((k1 - llimit)):round((ulimit + k1))
    long_put1 = (map_dbl(stock_price_at_expiration , .f = ~max(k2 - .x,0))) - p2
    short_put1 = ( -1 * map_dbl(stock_price_at_expiration, .f = ~max(k1 - .x,0))) + p1
    short_put2 = ( -1 * map_dbl(stock_price_at_expiration, .f = ~max(k3 - .x,0))) + p3
    long_put2 = long_put1
    profit_loss = long_put1 + short_put1 + short_put2 + long_put2
    df = data.frame(stock_price_at_expiration,  short_put1,long_put1, long_put2, short_put2,  profit_loss)
    p1 = ggplot(data = df) +
      geom_line(aes(x = stock_price_at_expiration, y = long_put1, colour = 'long_put1')) +
      geom_line(aes(x = stock_price_at_expiration, y = short_put1, colour = 'short_put1')) +
      geom_line(aes(x = stock_price_at_expiration, y = short_put2, colour = 'short_put2')) +
      geom_line(aes(x = stock_price_at_expiration, y = long_put2, colour = 'long_put2')) +
      geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
      labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Short Butterfly Put Spread Plot', color = 'Option contract') +
      scale_colour_manual('', breaks = c('long_put1', 'short_put1','short_put2', 'long_put2', 'profit_loss'), values = c('blue', 'red', 'yellow', 'purple', 'black'))
    print(df)
    print(ggplotly(p1))
  }

}
