#' @title Butterfly Call Spread Strategy Function
#' @description This function can be used to develop a Butterfly call Spread Strategy.
#' @param k1 Excercise Price of 1st Long call Option (Long Spread)/ Excercise Price of 1st Short call Option (Short Spread)
#' @param k2 Excercise Price of Short call Option (Long Spread) / Excercise Price of Long call Option (Short Spread)
#' @param k3 Excercise Price of 2nd Long call Option (Long Spread) / Excercise Price of 2nd Short call Option (Short Spread)
#' @param c1 Premium of 1st Long call Option (Long Spread)/ Premium of 1st Short call Option (Short Spread)
#' @param c2 Premium of Short call Option (Long Spread) / Premium of Long call Option (Short Spread)
#' @param c3 Premium of 2nd Long call Option (Long Spread) / Premium of 2nd Short call Option (Short Spread)
#' @param spread Type of Spread, Default: c("long", "short")
#' @param llimit Lower limit of stock price at Expiration., Default: 20
#' @param ulimit Upper Limit of Stock Price at Expiration, Default: 20
#' @return OUTPUT_DESCRIPTION Returns the profit/loss generated from the strategy along with the profit/loss of individual contract and an interactive graph for the same.
#' @details The long butterfly call spread is created by buying one in-the-money call option with a low strike price, writing two at-the-money call options, and buying one out-of-the-money call option with a higher strike price. The short butterfly spread is created by selling one in-the-money call option with a lower strike price, buying two at-the-money call options, and selling an out-of-the-money call option at a higher strike price.
#' @examples
#' butterfly.call(100, 95, 105, 2.3, 1.25, 3.2, spread = 'long')
#' @rdname butterfly.call
#' @export
#' @importFrom purrr map_dbl
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stats pnorm
butterfly.call = function(k1, k2, k3, c1, c2, c3, spread = c('long', 'short'), llimit = 20, ulimit = 20){

  if(!is.character(spread)){
    print('spread argument must be a character string')
  } else if(spread == 'long'){

    stock_price_at_expiration = round((k1 - llimit)):round((ulimit + k1))
    long_call1 = (map_dbl(stock_price_at_expiration, .f = ~max(.x - k1,0))) - c1
    short_call1 = (-1* map_dbl(stock_price_at_expiration, .f = ~max(.x - k2,0))) + c2
    short_call2 = short_call1
    long_call2 = (map_dbl(stock_price_at_expiration, .f = ~max(.x - k3,0))) - c3
    profit_loss = long_call2 + long_call1 + short_call1 + short_call2
    df = data.frame(stock_price_at_expiration, long_call1, short_call1,  short_call2, long_call2, profit_loss)
    p1 = ggplot(data = df) +
      geom_line(aes(x = stock_price_at_expiration, y = long_call1, colour = 'long_call1')) +
      geom_line(aes(x = stock_price_at_expiration, y = short_call1, colour = 'short_call1')) +
      geom_line(aes(x = stock_price_at_expiration, y = short_call2, colour = 'short_call2')) +
      geom_line(aes(x = stock_price_at_expiration, y = long_call2, colour = 'long_call2')) +
      geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
      labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Long Butterfly Plot', color = 'Option contract') +
      scale_colour_manual('', breaks = c('long_call1', 'short_call1','short_call2', 'long_call2', 'profit_loss'), values = c('blue', 'red', 'yellow', 'purple', 'black'))
    print(df)
    print(p1)

  } else if(spread == 'short'){



    stock_price_at_expiration = round((k1 - llimit)):round((ulimit + k1))
    long_call1 = (map_dbl(stock_price_at_expiration, .f = ~max(.x - k2,0))) - c2
    short_call1 = (-1* map_dbl(stock_price_at_expiration, .f = ~max(.x - k1,0))) + c1
    short_call2 = (-1* map_dbl(stock_price_at_expiration, .f = ~max(.x - k3,0))) + c3
    long_call2 = long_call1
    profit_loss = long_call2 + long_call1 + short_call1 + short_call2
    df2 = data.frame(stock_price_at_expiration, short_call1, long_call1, long_call2,  short_call2, profit_loss)
    p2 = ggplot(data = df2) +
      geom_line(aes(x = stock_price_at_expiration, y = long_call1, colour = 'long_call1')) +
      geom_line(aes(x = stock_price_at_expiration, y = short_call1, colour = 'short_call1')) +
      geom_line(aes(x = stock_price_at_expiration, y = short_call2, colour = 'short_call2')) +
      geom_line(aes(x = stock_price_at_expiration, y = long_call2, colour = 'long_call2')) +
      geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
      labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Long Butterfly Plot', color = 'Option contract') +
      scale_colour_manual('', breaks = c('long_call1', 'short_call1','short_call2', 'long_call2', 'profit_loss'), values = c('blue', 'red', 'yellow', 'purple', 'black'))
    print(df2)
    print(ggplotly(p2))

  }

}
