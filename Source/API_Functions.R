# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                   Add standard order
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Inputs -----------------------------------------------------------------------
# pair = asset pair
# type = type of order (buy/sell)
# ordertype = order type:
#   market
# limit (price = limit price)
# stop-loss (price = stop loss price)
# take-profit (price = take profit price)
# stop-loss-profit (price = stop loss price, price2 = take profit price)
# stop-loss-profit-limit (price = stop loss price, price2 = take profit price)
# stop-loss-limit (price = stop loss trigger price, price2 = triggered limit price)
# take-profit-limit (price = take profit trigger price, price2 = triggered limit price)
# trailing-stop (price = trailing stop offset)
# trailing-stop-limit (price = trailing stop offset, price2 = triggered limit offset)
# stop-loss-and-limit (price = stop loss price, price2 = limit price)
# settle-position
# price = price (optional.  dependent upon ordertype)
# price2 = secondary price (optional.  dependent upon ordertype)
# volume = order volume in lots
# leverage = amount of leverage desired (optional.  default = none)
# oflags = comma delimited list of order flags (optional):
#   viqc = volume in quote currency (not available for leveraged orders)
# fcib = prefer fee in base currency
# fciq = prefer fee in quote currency
# nompp = no market price protection
# post = post only order (available when ordertype = limit)
# starttm = scheduled start time (optional):
#   0 = now (default)
# +<n> = schedule start time <n> seconds from now
# <n> = unix timestamp of start time
# expiretm = expiration time (optional):
#   0 = no expiration (default)
# +<n> = expire <n> seconds from now
# <n> = unix timestamp of expiration time
# userref = user reference id.  32-bit signed number.  (optional)
# validate = validate inputs only.  do not submit order (optional)
# optional closing order to add to system when order gets filled:
#   close[ordertype] = order type
# close[price] = price
# close[price2] = secondary price

# Values -----------------------------------------------------------------------
# descr = order description info
# order = order description
# close = conditional close order description (if conditional close set)
# txid = array of transaction ids for order (if order was added successfully)
# url      <- "https://api.kraken.com/0/private/AddOrder"
# type <- "sell"
# ordertype <- "market"
# volume <- 0.1
add_market_order <- function(url, key, secret, pair, type, ordertype, volume) {
  
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce, "&pair=", pair, "&type=", type, "&ordertype=", ordertype,
                      "&volume=", volume)
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, 
                                                                         post_data), algo = "sha256", serialize = FALSE, 
                                                         raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
  
  curl <- RCurl::getCurlHandle(useragent = "whatever")
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url, binary = TRUE, postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  
  return(query_result)
}

add_market_order_short <- function(url, key, secret, pair, type, ordertype, volume) {
  
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce, "&pair=", pair, "&type=", type, "&ordertype=", ordertype,
                      "&volume=", volume, "&leverage=2")
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, 
                                                                         post_data), algo = "sha256", serialize = FALSE, 
                                                         raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
  
  curl <- RCurl::getCurlHandle(useragent = "whatever")
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url, binary = TRUE, postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  
  return(query_result)
}

add_order <- function(url, key, secret, pair, type, price, ordertype, volume) {
  
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce, "&pair=", pair, "&type=", type, "&ordertype=", ordertype,
                      "&volume=", volume, "&price=", price)
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, 
                                                                         post_data), algo = "sha256", serialize = FALSE, 
                                                         raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
  
  curl <- RCurl::getCurlHandle(useragent = "whatever")
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url, binary = TRUE, postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  
  return(query_result)
}


cancel_order <- function(url, key, secret, txid) {
  
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce, "&txid=", txid)
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, 
                                                                         post_data), algo = "sha256", serialize = FALSE, 
                                                         raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
  
  curl <- RCurl::getCurlHandle(useragent = "whatever")
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url, binary = TRUE, postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  
  return(query_result)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                   Trade Balance
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Inputs -----------------------------------------------------------------------
# aclass = asset class (optional):  currency (default, always currency)
# asset = base asset used to determine balance (default = ZUSD)

# Values -----------------------------------------------------------------------
# eb = equivalent balance (combined bbalancealance of all currencies)
# tb = trade balance (combined  of all equity currencies)
# m = margin amount of open positions
# n = unrealized net profit/loss of open positions
# c = cost basis of open positions
# v = current floating valuation of open positions
# e = equity = trade balance + unrealized net profit/loss
# mf = free margin = equity - initial margin (maximum margin available to open new positions)
# ml = margin level = (equity / initial margin) * 100
# url      <- "https://api.kraken.com/0/private/Balance"
# https://api.kraken.com/0/private/TradeBalance
get_balance <- function (url, key, secret) {
  
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce)
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, 
                                                                         post_data), algo = "sha256", serialize = FALSE, 
                                                         raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
  
  curl <- RCurl::getCurlHandle(useragent = "whatever")
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url, binary = TRUE, postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  
  return(query_result)
}

# Download historical trade data for selected pair using initial id ------------
hist_trades_pair <- function(sleep, hist_id, pair){
  repeat {
    Sys.sleep(sleep)
    what <- tryCatch(
      {
        url <- paste0('https://api.kraken.com/0/public/Trades?pair=',pair,'&since='
                      ,hist_id)
        dat <- jsonlite::fromJSON(url)
      },
      error = function(e){})
    if(is.null(dat$result[1])) next # error, skip
    if(nrow(as.data.frame(dat$result[1])) == 0) break # last batch empty
    temp <- cbind(data.frame(dat$result[1]), dat$result$last)
    colnames(temp) <- c("price", "volume", "time", "buy_sell", "market_limit", "miscellaneous", "trade_id","last_time")
    
    # Fix column names and types
    temp$Date_POSIXct <- as.character(anytime(as.numeric(as.character(temp[,"time"]))))
    temp$Time <- strftime(temp$Date_POSIXct, format = "%H:%M:%S")
    # colnames(temp) <- c("price", "volume", "epoch_time", "buy_sell", "market_limit",
    #                      "miscellaneous", "last_id", "Date_POSIXct", "Time")
    temp$Date <- as.Date(temp$Date_POSIXct)
    temp$Hour <- substr(temp$Time, 1,5)
    # [1] "price"         "volume"        "time"          "buy_sell"      "market_limit" 
    # [6] "miscellaneous" "trade_id"      "last_time"     "Date_POSIXct"  "Time"         
    # [11] "Date"          "Hour" 
    hist_id <- dat$result$last
    file <- paste0(paste(pair_data_results, pair, sep = "/"), ".csv.gz")
    fwrite(temp, file, sep = ",", row.names = FALSE,
                col.names = FALSE,
                append = TRUE)
    print(paste0("Current time: " ,Sys.time()))
    print(paste0("Period of 1000 trades received: ",
                 head(as.character(temp$Date_POSIXct), 1),
                 "-" ,
                 tail(as.character(temp$Date_POSIXct), 1)))
  }
}

# ------------------------------------------------------------------------------
simple_OHLC <- function(interval, pair){
  
  
  what <- tryCatch(
    {
      url <- paste0('https://api.kraken.com/0/public/OHLC?pair=',pair,'&interval=', interval)
      dat <- jsonlite::fromJSON(url)
    },
    error = function(e){})
  
  df <- as.data.table(dat$result[1])
  colnames(df) <- c("time", "open", "high", "low", "close",
                    "vwap", "volume", "count")
  df[, Date_POSIXct := anytime(as.numeric(as.character(time)))]
  df$Date_POSIXct <- as.character(df$Date_POSIXct)
  
  
  # as numeric
  df$open <- as.numeric(df$open)
  df$high <- as.numeric(df$high)
  df$low <- as.numeric(df$low)
  df$close <- as.numeric(df$close)
  df$volume <- as.numeric(df$volume)
  
  return(df)
  
}

# Private API calls ------------------------------------------------------------
myfun <- function (url, key, secret) {
  
  # Nonce and post info
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce)
  
  # Strip kraken url
  method_path <- gsub("^.*?kraken.com", "", url)
  
  # Secret APi key 
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, post_data),
                                                         algo = "sha256",
                                                         serialize = FALSE, 
                                                         raw = TRUE)),
               algo = "sha512", raw = TRUE)
  # Header
  httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
  
  curl <- RCurl::getCurlHandle(useragent = "whatever")
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url,
                                                      binary = TRUE,
                                                      postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  
  return(query_result)
}


get_trade_history <- function (url, key, secret, offset) {
  
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce, "&ofs=", offset)
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, 
                                                                         post_data), algo = "sha256", serialize = FALSE, 
                                                         raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
  
  # curl <- RCurl::getCurlHandle(useragent = paste("Rbitcoin", packageVersion("Rbitcoin")))
  curl <- RCurl::getCurlHandle(useragent = "whatver")
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url, binary = TRUE, postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  
  return(query_result)
}

current_avail_funds <- function(){
  usd_balance <- get_balance(url = "https://api.kraken.com/0/private/Balance",
                             key= API_Key,
                             secret = API_Sign)
  usd_balance <- usd_balance$result$ZUSD
  Sys.sleep(1)
  open_ord <- myfun(url = "https://api.kraken.com/0/private/OpenOrders",
                    key= API_Key,
                    secret = API_Sign)
  df <- open_ord$result$open %>%
    enframe() %>%
    unnest_wider(value)
  
  df$type_der <- unlist(lapply(df$descr, "[", "type"))
  df$price_der <- unlist(lapply(df$descr, "[", "price"))
  df$pair_der <- unlist(lapply(df$descr, "[", "pair"))
  df_buy <- subset(df, df$type_der == "buy")
  
  res <- as.numeric(usd_balance) - sum(as.numeric(df_buy$price_der)*as.numeric(df_buy$vol))
  return(res)
  
}

get_n_hist_orders <- function(n){
  key = API_Key
  secret = API_Sign
  offset <- 0
  url = "https://api.kraken.com/0/private/ClosedOrders"
  sleep <- 3
  decrease <- -0.5
  increase <- 2
  counter <- 0
  counter_actual <- 0
  orders_raw <- list();i<-1
  while (offset < n) {
    start.time <- Sys.time()
    orders_raw[[i]] <- get_trade_history(url, key, secret, offset)
    offset <- offset + 50
    i <- i+1
    counter <- counter+increase+decrease*sleep
    Sys.sleep(sleep)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    counter_actual <- counter_actual+increase+decrease*time.taken
    print(paste0("Offset at ", offset))
    print(paste0("Theoretical rate limit at ", counter, " with max=20"))
    print(paste0("Actual rate limit at ", counter_actual, " with max=20"))
  }
  myls <- list()
  df_list <- list()
  for(k in 1:length(orders_raw)){
    for (i in 1:length(orders_raw[[k]]$result$closed)){
      dataframe <- as.data.frame(rbind(unlist(orders_raw[[k]]$result$closed[i][[1]])))
      dataframe$order_id <-  names(orders_raw[[k]]$result$closed[i])
      myls[[i]] <- dataframe
    }
    df_list[[k]] <- rbindlist(myls, fill =T)
  }
  all_orders_cache <- rbindlist(df_list, fill = T)
  return(all_orders_cache)
}
