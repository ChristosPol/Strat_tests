# Run once, beginning of trading period or if unsure about the collection of orders
rm(list = ls())

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))

key = API_Key
secret = API_Sign

offset <- 0
url = "https://api.kraken.com/0/private/ClosedOrders"

# Call API once to get total amount of orders
firstcall <- get_trade_history(url, key, secret, offset)
norders <- round(firstcall$result$count, digits = -2)

sleep <- 3
decrease <- -0.5
increase <- 2
counter <- 0
counter_actual <- 0
orders_raw <- list();i<-1
while (offset <= norders) {
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
save(all_orders_cache, file = "strategies/qfl/bot/aux_files/all_orders_cache.Rdata")

