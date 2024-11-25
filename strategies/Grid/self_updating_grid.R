rm(list=ls())

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
pair <- "ADAUSD"
# pair <- "SHIBEUR"
# Path to save results
data_path <- "Code/Parameter_optim/Data"

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

ticks <- c(60)
units <- c("minutes")
intervals <- paste(ticks, units, sep = " ")


df <- trades_to_OHLC(pair = pair,
                     interval = intervals,
                     from_date = "2024-05-18",
                     to_date = "2024-05-20",
                     date_subset = F)
df1 <- df[[1]]

# candles(data =df1)

all_chars <- c(LETTERS, 0:9)
str_len <- 20


# Parameters
days_reset <- data.table(days_reset=c(24, 48, 72, 96, 120, 144, 168),flag=1)
exit_points <- data.table(exit=seq(0.01,0.2 ,0.01), flag=1)
exit_points <- data.table(exit=seq(0.01,0.1 ,0.01), flag=1)

start <-data.table(start= c(-0.005,-0.01, -0.015, -0.025,-0.035),flag=1)
step <- data.table(step=c(-0.001, -0.002, -0.005, -0.01, -0.02),flag=1)
params <- left_join(days_reset,exit_points)%>%left_join(start)%>%left_join(step)
res <- list()

k <- 46
for (k in 1:nrow(params)){

  first_price <-  df1[1, close]
  # Calculate short grid
  short_grid <- data.table(batch=sample(x = 1:10E5,1),
                           grid =seq(params$start[k], -0.5, by=params$step[k])[1:10],
                           init_price = first_price,
                           status_enter="open",
                           status_exit = "open")[, entries := init_price+init_price*grid][, exits:=init_price+init_price*params$exit[k]]
  short_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
  # short_grid[, interval :=as.character(NA)]
  short_grid[, interval_enter :=POSIXct()]
  short_grid[, interval_exit :=POSIXct()]
  counter <- 0
  i <- 2
  save_batch <- list()
  for (i in 2:nrow(df1)){
    
    low <- df1[i, low]
    high <- df1[i, high]
    int <- df1[i, interval]
    close <-df1[i, close]
    short_grid[, current:=low]
    
    counter <- counter+1
    if(counter >params$days_reset[k]){
      # if(counter >24){
      counter <- 0
      # print("Initializing new batch..")
      save_batch <- append(save_batch, list(short_grid))
      to_add <- short_grid[status_enter =="closed" & status_exit =="open"]
      
      # short_grid <- data.table(batch=sample(x = 1:10E5,1), grid =seq(-0.005, -0.3, by=-0.01), init_price = close, status_enter="open", status_exit = "open")[, entries := init_price+init_price*grid][, exits:=init_price+init_price*0.01]
      short_grid <- data.table(batch=sample(x = 1:10E5,1), grid = seq(params$start[k], -0.5, by=params$step[k])[1:10], init_price = close, status_enter="open", status_exit = "open")[, entries := init_price+init_price*grid][, exits:=init_price+init_price*params$exit[k]]
      short_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
      
      short_grid[, interval_enter :=POSIXct()]
      short_grid[, interval_exit :=POSIXct()]
      
      low <- df1[i, low]
      high <- df1[i, high]
      int <- df1[i, interval]
      close <-df1[i, close]
      short_grid[, current:=low]
      short_grid <- rbind(short_grid, to_add)
    }
    
    
    
    
    if (any(low <short_grid[status_enter=="open", entries])  ){
      short_grid[status_enter=="open" & current < entries, `:=`(status_enter ="closed")]
      if( any(short_grid[status_enter=="closed",   is.na(interval_enter)])  ){
        short_grid[status_enter=="closed" & is.na(interval_enter), interval_enter:=int]
      }
    }
    
    if (any(high >short_grid[status_enter=="closed", exits])  ){
      short_grid[status_enter=="closed" & current >exits, `:=`(status_exit ="closed")]
      if(any(short_grid[status_exit=="closed",   is.na(interval_exit)])  ){
        short_grid[status_exit=="closed" & is.na(interval_exit), interval_exit:=int]
      }
      
    }
    
    # print(short_grid)
    # Sys.sleep(0.1)
    # print(i)
  }
  
  
  
  bet <- 5
  d <- rbindlist(save_batch)
  dd <- d[, tail(.SD, 1L),  by = c("batch", "trade_id")]
  
  dd[status_enter =="closed" , entered_usd:= bet]
  dd[, volume := bet/entries]
  dd[status_enter =="closed" & status_exit =="closed", percent_realized:= (exits-entries)/entries]
  dd[status_enter =="closed" & status_exit =="closed", exited_usd:= (percent_realized*entered_usd)+entered_usd]
  dd[status_enter =="closed" & status_exit =="open", CURRENT_EVAL_AT_STOP:= tail(df1$close,1)*volume ]
  
  # sum(dd$entered_usd, na.rm = T)
  # sum(dd$exited_usd, na.rm = T)+
  # sum(dd$CURRENT_EVAL_AT_STOP, na.rm = T)
  
  
  res[[k]] <- (sum(dd$exited_usd, na.rm = T)+
            sum(dd$CURRENT_EVAL_AT_STOP, na.rm = T))/sum(dd$entered_usd, na.rm = T)
  
  print(k)
  print(res[[k]])
}

save(res, file="res.Rdata")

# View(cbind(params,unlist(res)))
# # 
# # max(unlist(res))
# # candles(data =df1)+
# #   geom_point(data=dd[!is.na(interval_enter)], aes(x=interval_enter, y=entries), colour="green")+
# #   geom_point(data=dd[!is.na(interval_exit)], aes(x=interval_exit, y=exits), colour="blue")
# # 
# # 
# # 
# # View(dd)
# 
# 
# params <- params[days_reset == 144 & exit ==0.02 & start ==-0.035 & step ==-0.002, ]
# k <- 1
# first_price <-  df1[1, close]
# # Calculate short grid
# short_grid <- data.table(batch=sample(x = 1:10E5,1),
#                          grid =seq(params$start[k], -0.5, by=params$step[k])[1:10],
#                          init_price = first_price,
#                          status_enter="open",
#                          status_exit = "open")[, entries := init_price+init_price*grid][, exits:=init_price+init_price*params$exit[k]]
# short_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
# # short_grid[, interval :=as.character(NA)]
# short_grid[, interval_enter :=POSIXct()]
# short_grid[, interval_exit :=POSIXct()]
# counter <- 0
# i <- 2
# save_batch <- list()
# for (i in 2:nrow(df1)){
#   
#   low <- df1[i, low]
#   high <- df1[i, high]
#   int <- df1[i, interval]
#   close <-df1[i, close]
#   short_grid[, current:=low]
#   
#   counter <- counter+1
#   if(counter >params$days_reset[k]){
#     # if(counter >24){
#     counter <- 0
#     # print("Initializing new batch..")
#     save_batch <- append(save_batch, list(short_grid))
#     to_add <- short_grid[status_enter =="closed" & status_exit =="open"]
#     
#     # short_grid <- data.table(batch=sample(x = 1:10E5,1), grid =seq(-0.005, -0.3, by=-0.01), init_price = close, status_enter="open", status_exit = "open")[, entries := init_price+init_price*grid][, exits:=init_price+init_price*0.01]
#     short_grid <- data.table(batch=sample(x = 1:10E5,1), grid = seq(params$start[k], -0.5, by=params$step[k])[1:10], init_price = first_price, status_enter="open", status_exit = "open")[, entries := init_price+init_price*grid][, exits:=init_price+init_price*params$exit[k]]
#     short_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
#     
#     short_grid[, interval_enter :=POSIXct()]
#     short_grid[, interval_exit :=POSIXct()]
#     
#     low <- df1[i, low]
#     high <- df1[i, high]
#     int <- df1[i, interval]
#     close <-df1[i, close]
#     short_grid[, current:=low]
#     short_grid <- rbind(short_grid, to_add)
#   }
#   
#   
#   
#   
#   if (any(low <short_grid[status_enter=="open", entries])  ){
#     short_grid[status_enter=="open" & current < entries, `:=`(status_enter ="closed")]
#     if( any(short_grid[status_enter=="closed",   is.na(interval_enter)])  ){
#       short_grid[status_enter=="closed" & is.na(interval_enter), interval_enter:=int]
#     }
#   }
#   
#   if (any(high >short_grid[status_enter=="closed", exits])  ){
#     short_grid[status_enter=="closed" & current >exits, `:=`(status_exit ="closed")]
#     if(any(short_grid[status_exit=="closed",   is.na(interval_exit)])  ){
#       short_grid[status_exit=="closed" & is.na(interval_exit), interval_exit:=int]
#     }
#     
#   }
#   
#   # print(short_grid)
#   # Sys.sleep(0.1)
#   # print(i)
# }
# 
# 
# 
# bet <- 5
# d <- rbindlist(save_batch)
# dd <- d[, tail(.SD, 1L),  by = c("batch", "trade_id")]
# 
# dd[status_enter =="closed" , entered_usd:= bet]
# dd[, volume := bet/entries]
# dd[status_enter =="closed" & status_exit =="closed", percent_realized:= (exits-entries)/entries]
# dd[status_enter =="closed" & status_exit =="closed", exited_usd:= (percent_realized*entered_usd)+entered_usd]
# dd[status_enter =="closed" & status_exit =="open", CURRENT_EVAL_AT_STOP:= tail(df1$close,1)*volume ]
# # 
# # 
# # 
# # bet <- 5
# # d <- rbindlist(save_batch)
# # dd <- d[, tail(.SD, 1L),  by = c("batch", "trade_id")]
# # 
# # dd[status_enter =="closed" , entered_usd:= bet]
# # dd[, volume := bet/entries]
# # dd[status_enter =="closed" & status_exit =="closed", percent_realized:= (exits-entries)/entries]
# # dd[status_enter =="closed" & status_exit =="closed", exited_usd:= (percent_realized*entered_usd)+entered_usd]
# # dd[status_enter =="closed" & status_exit =="open", CURRENT_EVAL_AT_STOP:= tail(df1$close,1)*volume ]
# # 
# sum(dd$entered_usd, na.rm = T)
# sum(dd$exited_usd, na.rm = T)+
# sum(dd$CURRENT_EVAL_AT_STOP, na.rm = T)
# # 
# # 
# candles(data =df1)+
#   geom_point(data=dd[!is.na(interval_enter)], aes(x=interval_enter, y=entries), colour="green")+
#   geom_point(data=dd[!is.na(interval_exit)], aes(x=interval_exit, y=exits), colour="blue")
