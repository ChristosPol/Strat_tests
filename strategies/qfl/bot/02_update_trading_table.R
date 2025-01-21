# Need to add to closed open sell orders too.

rm(list = ls())

path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Load trading table
load("strategies/qfl/bot/aux_files/trading_table.Rdata")
# Load cache of closed trades
load("strategies/qfl/bot/aux_files/all_orders_cache.Rdata")

# Update cache
last_n_orders <- get_n_hist_orders(n = 500)
idx <- which(!last_n_orders$order_id%in%all_orders_cache$order_id)
all_orders_cache <- rbind(last_n_orders[idx, ], all_orders_cache, fill = T)
save(all_orders_cache, file = "strategies/qfl/bot/aux_files/all_orders_cache.Rdata")


closed <- all_orders_cache[status == "closed"]
key <- c("order_id", "opentm", "closetm", "vol", "vol_exec", "cost", "fee", "price")
closed <- closed[, ..key]

i <- 1

for(i in 1:nrow(trading_table)){
  # print(i)
  msg <- tryCatch({
    if(trading_table$status_enter[i] == "OPEN" & trading_table$buy_id[i] %in% closed$order_id){
      trading_table$status_enter[i] <- "CLOSED"
      
      sell_it <- add_order(url = "https://api.kraken.com/0/private/AddOrder",
                           key = API_Key, secret = API_Sign, pair = trading_table$pair[i], type = "sell",
                           ordertype = "limit", volume = trading_table$vol[i], price = trading_table$exits[i])
      print(sell_it)
      
      if(length(sell_it$error) ==1){
        trading_table$message[i] <- sell_it$error
        print(sell_it$error)
      } else {
        trading_table$sell_id[i] <- sell_it$result$txid
        trading_table$status_exit[i] <- "OPEN"
      }
      
      print(paste0("Limit sell order sent for: ", trading_table$pair[i]))
      Sys.sleep(10)
    } 
    
  }, error = function(e){
  })
  
}

for(i in 1:nrow(trading_table)){
  if(trading_table$status_exit[i] == "OPEN" & trading_table$sell_id[i] %in% all_orders_cache[status == "closed", order_id]){
    trading_table$status_exit[i] <- "CLOSED"
    print(paste0("Position closed for: ", trading_table$pair[i]))
  }
}

# Save
save(trading_table, file ="strategies/qfl/bot/aux_files/trading_table.Rdata")

