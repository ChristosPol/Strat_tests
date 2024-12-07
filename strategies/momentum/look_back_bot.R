
bar <- 336
sl <- 0.075
tp <- 0.1
med_num <- 20
exc_num <- 20

df <- simple_OHLC(interval = 60, pair = "SOLUSD")
indeces <- (nrow(df)-bar):nrow(df)

max_fun <- function(x) {
  len <- length(x)
  subset <- x[-((len - exc_num + 1):len)]
  top_values <- Rfast::nth(subset, k = med_num, descending = TRUE)
  res <- median(top_values)
  return(res)
}

min_fun <- function(x) {
  len <- length(x)
  subset <- x[-((len - exc_num + 1):len)]
  smallest_values <- Rfast::nth(subset, k = med_num, descending = FALSE)
  res <- median(smallest_values)
  return(res)
}

resistance <- max_fun(x = df$close[indeces])
support <- min_fun(x = df$close[indeces])
setnames(df, "Date_POSIXct", "interval")

url <- paste0("https://api.kraken.com/0/public/Ticker?pair=SOLUSD")
tb <- jsonlite::fromJSON(url)
sell_price <-as.numeric(tb$result$SOLUSD$b[1])
buy_price <-as.numeric(tb$result$SOLUSD$a[1])


if is.na(position) {
  if(buy_price > resistance){
    object_buy
    update_trading_table
  } else if (sell_price < support){
    object_sell
    update_trading_table
  }  
}

if (!is.na(position)){
  if(position == "long"){
    percent <- (sell_price-enter_price)/enter_price
    if(percent > tp or percent<-sl){
      close_position
    }
  } else if(position == "short")
    
    if(percent > tp or percent<-sl){
      close_position
    }
}

# bid price to sell, ask to buy
# candles(df)+
#   geom_hline(yintercept = resistance, colour = "darkred")+
#   geom_hline(yintercept = support, colour = "darkgreen")
