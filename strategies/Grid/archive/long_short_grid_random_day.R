rm(list=ls())

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
pair <- "SOLUSD"
# pair <- "SHIBEUR"
# Path to save results
data_path <- "Code/Parameter_optim/Data"

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

ticks <- c(24)
units <- c("hours")
intervals <- paste(ticks, units, sep = " ")

df <- trades_to_OHLC(pair = pair,
                     interval = intervals,
                     from_date = "2023-03-27",
                     to_date = "2023-03-30",
                     date_subset = F)
df1 <- df[[1]]
df1[, range:= (abs(high-low)/open)/2]
hist(df1$range, breaks=100)
median(df1$range)
mean(df1$range)
random_date <- sample(seq(as.Date(min(df1$interval)), as.Date(max(df1$interval)), by = "day"), 1)

ticks <- c(5)
units <- c("minutes")
intervals <- paste(ticks, units, sep = " ")

bet <- 5
df <- trades_to_OHLC(pair = pair,
                     interval = intervals,
                     from_date = random_date,
                     to_date = random_date,
                     date_subset = T)
df1 <- df[[1]]


df1[, sma := SMA(close, 10)]
# head(df1$close, 1)
# short_grid <- head(df1$close, 1)*seq(1.01, 1.05, by=0.0025)
# long_grid <- head(df1$close, 1)*(1-seq(0.01, 0.05, by=0.0025))
# candles(data =df1)+
#   geom_line(data =df1, aes(x= interval, y = sma), colour="black")
#   geom_hline(yintercept = short_grid, colour="red")+
#   geom_hline(yintercept = long_grid, colour="darkgreen")


all_chars <- c(LETTERS, 0:9)
str_len <- 20

# Parameters # TODO -> Cover a longer
bars_reset <- data.table(bars_reset=c(96, 192),flag=1)
exit_points <- data.table(exit=c(0.008, seq(0.01,0.030 ,0.01)), flag=1)
# exit_points <- data.table(exit=seq(0.01,0.1 ,0.01), flag=1)
# start <-data.table(start= c(0.005,0.01, 0.015, 0.025,0.035),flag=1)
start <-data.table(start= c(0.005,0.01, 0.015, 0.02,0.03, 0.04),flag=1)
# step <- data.table(step=c(0.001, 0.002, 0.005, 0.01, 0.02),flag=1)
maxim <- data.table(maxim = c(0.06),flag=1)
n_trades <- data.table(n_trades=seq(5, 40, 5),flag=1)
params <- left_join(bars_reset,exit_points)%>%left_join(start)%>%left_join(maxim)%>%left_join(n_trades)
# params <- params[bars_reset==120 & exit ==0.01 & start==0.005 & step==0.001]

h <- 1
res <- list()
total_bet <- list()
for (h in 1:nrow(params)){
  first_price <-  df1[1, close]
  
  step <- (params$maxim[h] - params$start[h])/(params$n_trades[h])
  # Calculate short grid
  short_grid <- data.table(batch = sample(x = 1:10E5,1),
                           grid = first_price*(1+seq(params$start[h], params$maxim[h], by=step)),
                           init_price = first_price,
                           status_enter="open",
                           status_exit = "open")
  short_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
  short_grid[, interval_enter :=POSIXct()]
  short_grid[, interval_exit :=POSIXct()]
  short_grid[, position := "short"]
  short_grid[, exits:=min(short_grid[, grid])*(1-params$exit[h])]
  setorder(short_grid, -grid)
  
  # Calculate long grid
  long_grid <- data.table(batch = sample(x = 1:10E5,1),
                          grid = first_price*(1-seq(params$start[h], params$maxim[h], by=step)),
                          init_price = first_price,
                          status_enter="open",
                          status_exit = "open")
  long_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
  long_grid[, interval_enter :=POSIXct()]
  long_grid[, interval_exit :=POSIXct()]
  long_grid[, position := "long"]
  long_grid[, exits:=max(short_grid[, grid])*(1+params$exit[h])]
  setorder(long_grid, -grid)
  
  grid <- rbind(short_grid, long_grid)
  
  
  counter <- 0
  i <- 2
  save_batch <- list()
  for (i in 2:nrow(df1)){
    
    low <- df1[i, low]
    high <- df1[i, high]
    int <- df1[i, interval]
    close <-df1[i, close]
    grid[, current:=close]
    current <- unique(grid$current)
    counter <- counter+1
    
    # update grid after x bars
    
    if(counter > params$bars_reset[h]){
      
      counter <- 0
      save_batch <- append(save_batch, list(grid))
      to_add <- grid[status_enter =="closed" & status_exit =="open"]
      
      # Calculate short grid
      short_grid <- data.table(batch = sample(x = 1:10E5,1),
                               grid = current*(1+seq(params$start[h], params$maxim[h], by=step)),
                               init_price = first_price,
                               status_enter="open",
                               status_exit = "open")
      short_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
      short_grid[, interval_enter :=POSIXct()]
      short_grid[, interval_exit :=POSIXct()]
      short_grid[, position := "short"]
      short_grid[, exits:=min(short_grid[, grid])*(1-params$exit[h])]
      short_grid[, current:=close]
      setorder(short_grid, -grid)
      
      # Calculate long grid
      long_grid <- data.table(batch = sample(x = 1:10E5,1),
                              grid = current*(1-seq(params$start[h], params$maxim[h], by=step)),
                              init_price = first_price,
                              status_enter="open",
                              status_exit = "open")
      long_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
      long_grid[, interval_enter :=POSIXct()]
      long_grid[, interval_exit :=POSIXct()]
      long_grid[, position := "long"]
      long_grid[, exits:=max(short_grid[, grid])*(1+params$exit[h])]
      long_grid[, current:=close]
      setorder(long_grid, -grid)
      
      grid <- rbind(short_grid, long_grid)
      grid <- rbind(grid, to_add)
    }
    
    
    
    # Enter long
    if (any(low <grid[status_enter=="open" & position=="long", grid])  ){
      grid[status_enter=="open" & current < grid& position=="long", `:=`(status_enter ="closed")]
      if( any(grid[status_enter=="closed"& position=="long",   is.na(interval_enter)])  ){
        grid[status_enter=="closed" & is.na(interval_enter)& position=="long", interval_enter:=int]
      }
    }
    
    # Exit long
    if (any(high >grid[status_enter=="closed" & position == "long", exits])  ){
      grid[status_enter=="closed" & high >exits& position == "long", `:=`(status_exit ="closed")]
      if(any(grid[status_exit=="closed"& position == "long",   is.na(interval_exit)])  ){
        grid[status_exit=="closed" & is.na(interval_exit)& position == "long", interval_exit:=int]
      }
      
    }
    
    # Enter short
    if (any(high <grid[status_enter=="open" & position=="short", grid])  ){
      grid[status_enter=="open" & high > grid & position=="short", `:=`(status_enter ="closed")]
      if( any(grid[status_enter=="closed"& position=="short",   is.na(interval_enter)])  ){
        grid[status_enter=="closed" & is.na(interval_enter) & position=="short", interval_enter:=int]
      }
    }
    
    # Exit short
    if (any(low <grid[status_enter=="closed" & position == "short", exits])  ){
      grid[status_enter=="closed" & low <exits& position == "short", `:=`(status_exit ="closed")]
      if(any(grid[status_exit=="closed"& position == "short",   is.na(interval_exit)])  ){
        grid[status_exit=="closed" & is.na(interval_exit)& position == "short", interval_exit:=int]
      }
      
    }
    
    
    
    
    # print(grid)
    # Sys.sleep(0.2)
    # print(i)
  }
  
  d <- rbindlist(save_batch)
  dd <- d[, tail(.SD, 1L),  by = c("batch", "trade_id")]
  dd[, final_exit := ifelse(status_enter =="closed" & status_exit =="closed", exits, tail(df1[, close], 1))]
  dd <- dd[status_enter!="open"]
  dd[, percent := (final_exit-grid)/grid]
  dd[position == "short", percent:=-1*percent]
  dd[, quote_res_no_fees := bet+bet*percent]
  dd[, quote_res_clean := quote_res_no_fees-(0.65/100)*quote_res_no_fees]
  dd[, bet := bet]
  

  res[[h]] <-(sum(dd$quote_res_clean)-sum(dd$bet))/(sum(dd$bet))
  total_bet[[h]] <- sum(dd$bet)
  print(paste0("Entering with ", total_bet[[h]], " USD", ", Returns: ", res[[h]], " USD returns: ", res[[h]]*total_bet[[h]]))
  print(h)
}
hodl <- (tail(df1[, close], 1)-head(df1[, close], 1))/head(df1[, close], 1)
# save(res, total_bet, random_date, hodl,params, file=paste0("res","_",random_date,".Rdata"))
# HODL
print("HODL:")
print(hodl)

# print(paste0("Best returns was: ", unlist(res[[h]])))

candles(data =df1)+
  # geom_line(data =df1, aes(x= interval, y = sma), colour="black")+
  geom_point(data=dd[!is.na(interval_enter) & position == "long"], aes(x=interval_enter, y=grid), fill="lightblue3",colour="black", shape =24, size=2)+
  geom_point(data=dd[!is.na(interval_exit) & position == "long"], aes(x=interval_exit, y=exits), fill="lightblue3", colour="black",shape =25, size=2)+

  geom_point(data=dd[!is.na(interval_enter) & position == "short"], aes(x=interval_enter, y=grid), fill="darkorchid1", colour="black", shape =25, size=2)+
  geom_point(data=dd[!is.na(interval_exit) & position == "short"], aes(x=interval_exit, y=exits), fill="darkorchid1", colour="black",shape =24, size=2)


# bet <- 5
# d <- rbindlist(save_batch)
# dd <- d[, tail(.SD, 1L),  by = c("batch", "trade_id")]
# 
# dd[status_enter =="closed" , entered_usd:= bet]
# dd[, volume := bet/entries]
# dd[status_enter =="closed" & status_exit =="closed", percent_realized:= (exits-entries)/entries]
# dd[status_enter =="closed" & status_exit =="closed", exited_usd:= (percent_realized*entered_usd)+entered_usd]
# dd[status_enter =="closed" & status_exit =="open", CURRENT_EVAL_AT_STOP:= tail(df1$close,1)*volume ]
# 
# 
# (sum(dd$exited_usd, na.rm = T)+
#     sum(dd$CURRENT_EVAL_AT_STOP, na.rm = T))/sum(dd$entered_usd, na.rm = T)
# 
# candles(data =df1)+
#   geom_point(data=dd[!is.na(interval_enter)], aes(x=interval_enter, y=entries), colour="green")+
#   geom_point(data=dd[!is.na(interval_exit)], aes(x=interval_exit, y=exits), colour="blue")
# 
# 
# 
# # candles(data =df1)+
# #   geom_point(data=dd[!is.na(interval_enter) & trade_id =="8M6JQY92C472DW59BRRQ"], aes(x=interval_enter, y=entries), colour="green")+
# #   geom_hline(yintercept = 0.4432852)+
# #   geom_vline(xintercept = as.POSIXct("2024-05-15 22:00:00") )
# # 
# # 
# # ggplot(data=df1, aes(x = interval, y = close))+
# #   geom_line()+
# #   geom_hline(yintercept = 0.4432852)+
# #   geom_vline(xintercept = as.POSIXct("2024-05-15 22:00:00") )
