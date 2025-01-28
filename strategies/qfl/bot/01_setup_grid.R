rm(list = ls())
cat("\n")
path_source <- "/Users/christospolysopoulos/Repositories/Private/Strat_tests/Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))
cat("*************** NEW BATCH STARTING ***************\n")
system_time <- Sys.time()
cat(paste0("Setup grid process initiated at: ", system_time, "\n"))
# 72 hours_0.15_5_0.1_0.5_6_0.0666666666666667
cat("-------------------------------------------------------------------------\n")

# Bot parameters
options(scipen = 999)
n_pairs <- 1
bet   <- 5
bar <- 72
tp <- 0.15
med_num <- 5
start_point <- 0.2
end_point <- 0.5
number_trades <- 8-2 # two are added in total 8
step  <- (end_point-start_point)/number_trades
avail <- current_avail_funds()
needed <- n_pairs*bet*(number_trades+2)
volume24h <- 50000
cat(paste0("Parameters are being set: ",
           "\nNumber of pairs: ",n_pairs,
           "\nBet in USD: ", bet,
           "\nBar: ", bar,
           "\nTP: ", tp,
           "\nMedian number: ", med_num,
           "\nStarting point: ", start_point,
           "\nEnding point: ", end_point,
           "\nNumber of trades: ", number_trades,
           "\nStep: ", step, "\n"))
cat("-------------------------------------------------------------------------\n")
cat(paste0("Available funds are: ", round(avail,2), "USD", "\nFunds needed to set up grid are: ", round(needed,2), "\n24H volume filter for pair selection is: ", volume24h, "USD", "\n"))

cat("-------------------------------------------------------------------------\n")

if("trading_table.Rdata" %in% list.files("/Users/christospolysopoulos/Repositories/Private/Strat_tests/strategies/qfl/bot/aux_files")){
  cat("Trading table found, canceling unopened limit buy orders\n")
} else{
  cat("Trading table not found, no need to cancel anything\n")
}

cat("Canceling orders started\n")
# First needs to be cancelled all open ones
counter_cancel <- 0
counter_cancel_error <- 0
if("trading_table.Rdata" %in% list.files("/Users/christospolysopoulos/Repositories/Private/Strat_tests/strategies/qfl/bot/aux_files")){
  load(file ="/Users/christospolysopoulos/Repositories/Private/Strat_tests/strategies/qfl/bot/aux_files/trading_table.Rdata")
  for(i in 1:nrow(trading_table)){
    msg <- tryCatch({
      if(trading_table$status_enter[i] == "OPEN"){
        cancel_it <- cancel_order(url = "https://api.kraken.com/0/private/CancelOrder",
                                  key = API_Key, secret = API_Sign, txid = trading_table$buy_id[i])
        if(length(cancel_it$error) == 1){
          trading_table$message[i] <- cancel_it$error
          counter_cancel_error <- counter_cancel_error+1
        } else {
          trading_table$status_enter[i] <- "CANCELLED"
          counter_cancel <- counter_cancel+1
        }
        Sys.sleep(1)
      }
    }, error = function(e){
    })
  }
}
cat(paste0("Number of limit buy orders succesfully cancelled: ", counter_cancel,
       "\nNumber of limit buy orders a problem has occured: ", counter_cancel_error, "\n"))
cat("-------------------------------------------------------------------------\n")
# 
# if(avail > needed){
#   "BOT"
# }

cat(paste0("Getting ticker, price and volume information for pairs selection\n")) 
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

# Need some trading volume
info_usd <- info_usd[USD_amount > volume24h]

cat(paste0("Number of pairs with above desired 24H volume: ", nrow(info_usd), "\n"))

# Select pairs by volume
if(nrow(info_usd) >= n_pairs ){
  selected_pairs <- info_usd[sample(x = nrow(info_usd), n_pairs), ]
} else {
  selected_pairs <- copy(info_usd)
}

cat(paste0("Number of pairs going into live trading: ", nrow(selected_pairs), "\n"))

cat("-------------------------------------------------------------------------\n")

# 
# if(avail > needed){
#   "BOT"
# }


grid <-  -1*seq(start_point, end_point, step)
cat(paste0("Pre-defined grid: ", paste0(grid, collapse = ", "), "\n"))

cat("Pulling 1H OHLC for all pairs\n")
# Calculate orders grid
df_list <- list()
for(i in 1:nrow(selected_pairs)){
  df_list[[i]] <- simple_OHLC(interval = "60", pair = selected_pairs$api_name[i])
  Sys.sleep(1)
}


# Trading limits
# Max counter Intermediate = 125
# Decay rate Intermediate = 2.34 per second

# Note that the trading rate limits apply separately per currency pair,
# so reaching the rate limits for one currency pair (XBT/USD for example)
# does not affect trading on any other currency pair (LTC/EUR for example).
cat("Calculating grid for all selected pairs\n")
grid_pair <- list()
for(i in 1:length(df_list)){
  df_pair <- copy(df_list[[i]])
  support <- median(sort(tail(df_pair$close, bar))[1:med_num])

  if(support > tail(df_pair$close, 1)){
    support <- tail(df_pair$close, 1) - tail(df_pair$close, 1)*0.005
  }
  entries_limit <- c(support, support+support*grid)
  long_grid <- data.table(grid = entries_limit,
                          status_enter="NOT_INITIATED",
                          status_exit = "NOT_INITIATED")
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

cat(paste0("Grid with batch number ", unique(batch_orders$batch), " has finished calculation\n"))
cat(paste0("Total number of limit buy orders to be sent: ", nrow(batch_orders), "\n"))

# Decimals
batch_orders <- merge(batch_orders, selected_pairs[, .(api_name, lot_decimals, pair_decimals)], by.x = "pair", by.y ="api_name", all.x = T)
batch_orders[, vol := round(bet/grid, lot_decimals)]
batch_orders[, grid := round(grid, pair_decimals)]
batch_orders[, exits := round(exits, pair_decimals)]
i <- 1
counter_buy <- 0
counter_buy_error <- 0
if(!"trading_table.Rdata" %in% list.files("/Users/christospolysopoulos/Repositories/Private/Strat_tests/strategies/qfl/bot/aux_files")){
  # Export trading table
  trading_table <- copy(batch_orders)
  for(i in 1:nrow(trading_table)){
    buy_it <- add_order(url = "https://api.kraken.com/0/private/AddOrder",
                        key = API_Key, secret = API_Sign, pair = trading_table$pair[i], type = "buy",
                        ordertype = "limit", volume = trading_table$vol[i], price = trading_table$grid[i])
    if(length(buy_it$error) == 1){
      trading_table$message[i] <- buy_it$error
      counter_buy_error <- counter_buy_error+1
    } else {
      trading_table$buy_id[i] <- buy_it$result$txid
      trading_table$status_enter[i] <- "OPEN"
      counter_buy <- counter_buy+1
    }
    Sys.sleep(0.5)
  }
  # Save
  save(trading_table, file ="/Users/christospolysopoulos/Repositories/Private/Strat_tests/strategies/qfl/bot/aux_files/trading_table.Rdata")
} else {
  trading_table <- rbind(trading_table, batch_orders)
  for(i in 1:nrow(trading_table)){
    if(trading_table$status_enter[i] == "NOT_INITIATED"){
      buy_it <- add_order(url = "https://api.kraken.com/0/private/AddOrder",
                          key = API_Key, secret = API_Sign, pair = trading_table$pair[i], type = "buy",
                          ordertype = "limit", volume = trading_table$vol[i], price = trading_table$grid[i])
      Sys.sleep(0.5)
      if(length(buy_it$error) == 1){
        trading_table$message[i] <- buy_it$error
        counter_buy_error <- counter_buy_error+1
      } else {
        trading_table$buy_id[i] <- buy_it$result$txid
        trading_table$status_enter[i] <- "OPEN"
        counter_buy <- counter_buy+1
      }
    }
  }
  # Save
  save(trading_table, file ="/Users/christospolysopoulos/Repositories/Private/Strat_tests/strategies/qfl/bot/aux_files/trading_table.Rdata")
}
cat(paste0("Number of limit buy orders succesfully opened: ", counter_buy,
           "\nNumber of limit buy orders a problem has occured: ", counter_buy_error, "\n"))
cat("\n")