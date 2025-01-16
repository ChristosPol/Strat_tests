rm(list = ls())

path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# 72 hours_0.15_5_0.1_0.5_6_0.0666666666666667

# Bot parameters
options(scipen = 999)
n_pairs <- 150
bet   <- 5
bar <- 72
tp <- 0.15
med_num <- 5
start_point <- 0.2
end_point <- 0.5

grid <-  -1*seq(start_point, end_point, step)

# Get all pairs in exchange
info <- asset_info_ticker()

# Select quote
info_usd <- info[quote =="ZUSD"]

# Calculate minimum order in USD
info_usd[, minimum_order_usd := as.numeric(ordermin)*PRICE]

# Remove pairs that minimum order is under bet size in USD
info_usd <- info_usd[minimum_order_usd <= bet]

# remove forex, gold etc
info_usd <- info_usd[!api_name %in% c("USDTZUSD", "USDCUSD", "ZEURZUSD",
                                      "ZGBPZUSD", "AUDUSD", "PAXGUSD",
                                      "USDSUSD", "USDQUSD")]
setorder(info_usd, -USD_amount)



# Select pairs by volume
selected_pairs <- info_usd[1:n_pairs]

# Calculate orders grid
df_list <- list()
for(i in 1:nrow(selected_pairs)){
  df_list[[i]] <- simple_OHLC(interval = "60", pair = selected_pairs$api_name[i])
  Sys.sleep(1)
  print(i)
}

# Trading limits 
# Max counter Intermediate = 125
# Decay rate Intermediate = 2.34 per second

# Note that the trading rate limits apply separately per currency pair,
# so reaching the rate limits for one currency pair (XBT/USD for example)
# does not affect trading on any other currency pair (LTC/EUR for example).
grid_pair <- list()
for(i in 1:length(df_list)){
  df_pair <- copy(df_list[[i]])
  support <- median(sort(tail(df_pair$close, bar))[1:med_num])
  
  if(support > tail(df_pair$close, 1)){
    support <- tail(df_pair$close, 1) - tail(df_pair$close, 1)*0.005
  }
  entries_limit <- c(support, support+support*grid)
  long_grid <- data.table(grid = entries_limit,
                          status_enter=NA_character_,
                          status_exit = NA_character_)
  long_grid[, `:=`(interval_enter = as.POSIXct(rep(NA, nrow(long_grid))),
                   interval_exit_tp = as.POSIXct(rep(NA, nrow(long_grid))),
                   position = "long",
                   exits = max(long_grid[, grid])*(1+tp),
                   bet = bet,
                   pair = selected_pairs$api_name[i],
                   buy_id = NA_character_,
                   sell_id = NA_character_,
                   message = NA_character_)]
  grid_pair[[i]] <- copy(long_grid)
  rm(long_grid)
}

batch_orders <- rbindlist(grid_pair)
batch_orders[, batch := sample(x = 1:10E5,1)]

# Decimals
batch_orders <- merge(batch_orders, selected_pairs[, .(api_name, lot_decimals, pair_decimals)], by.x = "pair", by.y ="api_name", all.x = T)
batch_orders[, vol := round(bet/grid, lot_decimals)]
batch_orders[, grid := round(grid, pair_decimals)]
batch_orders[, exits := round(exits, pair_decimals)]
i <- 1
if(!"trading_table.Rdata" %in% list.files("strategies/qfl/bot/aux_files")){
  # Export trading table
  trading_table <- copy(batch_orders)
  for(i in 1:nrow(trading_table)){
    buy_it <- add_order(url = "https://api.kraken.com/0/private/AddOrder",
                        key = API_Key, secret = API_Sign, pair = trading_table$pair[i], type = "buy",
                        ordertype = "limit", volume = trading_table$vol[i], price = trading_table$grid[i])
    if(length(buy_it$error) == 1){
      trading_table$message[i] <- buy_it$error
    } else {
      trading_table$buy_id[i] <- buy_it$result$txid
      trading_table$status_enter[i] <- "OPEN"
    }  
  }
  # Save
  save(trading_table, file ="strategies/qfl/bot/aux_files/trading_table.Rdata")
} else {
  load(file ="strategies/qfl/bot/aux_files/trading_table.Rdata")
  
  # Cancel orders from previous batch -
  # Loop
  for(i in 1:nrow(trading_table)){
    cancel_it <- cancel_order(url = "https://api.kraken.com/0/private/CancelOrder",
                              key = API_Key, secret = API_Sign, txid = trading_table$buy_id[i])
    trading_table$status_enter[i] <- "CANCELLED"
  }
  trading_table <- rbind(trading_table, batch_orders)
  
  for(i in 1:nrow(trading_table)){
    buy_it <- add_order(url = "https://api.kraken.com/0/private/AddOrder",
                        key = API_Key, secret = API_Sign, pair = trading_table$pair[i], type = "buy",
                        ordertype = "limit", volume = trading_table$vol[i], price = trading_table$grid[i])
    if(length(buy_it$error) == 1){
      trading_table$message[i] <- buy_it$error
    } else {
      trading_table$buy_id[i] <- buy_it$result$txid
      trading_table$status_enter[i] <- "OPEN"
    }  
  }
  
  # Save
  save(trading_table, file ="strategies/qfl/bot/aux_files/trading_table.Rdata")
}