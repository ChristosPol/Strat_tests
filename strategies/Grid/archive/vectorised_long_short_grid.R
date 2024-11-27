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

# Load data
ticks <- c(5)
units <- c("minutes")
intervals <- paste(ticks, units, sep = " ")

df <- trades_to_OHLC(pair = pair,
                     interval = intervals,
                     from_date = "2023-03-27",
                     to_date = "2023-03-30",
                     date_subset = F)
df1 <- df[[1]]
# Fix vector of dates
dates_vector <- unique(as.Date(df1$interval))
dates_vector <- dates_vector[-1] 

# Set parameters table
splits_reset <- data.table(splits_reset=c(2, 3, 4, 5, 6, 7, 8),flag=1)
exit_points <- data.table(exit=c(0.008, seq(0.01,0.05 ,0.01)), flag=1)
start <-data.table(start= c(0.005,0.01, 0.015, 0.02, 0.03, 0.04, 0.05),flag=1)
maxim <- data.table(maxim = c(0.06, 0.08, 0.1),flag=1)
n_trades <- data.table(n_trades=seq(10, 60, 10),flag=1)
params <- left_join(splits_reset,exit_points)%>%left_join(start)%>%left_join(maxim)%>%left_join(n_trades)

# Empty grid
closed_orders <- data.table(batch = NA_integer_,
                            grid = NA_real_,
                            init_price = NA_real_,
                            status_enter=NA_character_,
                            status_exit = NA_character_)
closed_orders[, trade_id := NA_character_]
closed_orders[, interval_enter :=POSIXct()]
closed_orders[, interval_exit :=POSIXct()]
closed_orders[, position := NA_character_]
closed_orders[, exits:=NA_real_]
closed_orders[, bar_entered:=NA_real_]
closed_orders[, bar_exited:=NA_real_]


# For trade Ids
all_chars <- c(LETTERS, 0:9)
str_len <- 20

bet <- 5
x <-200
h <- 1
s <- 1
# gc()
# Loop through calendar days ---------------------------------------------------
daily_res <- list()
for (x in 1:length(dates_vector)){
  df <- trades_to_OHLC(pair = pair,
                       interval = intervals,
                       from_date = dates_vector[x],
                       to_date = dates_vector[x],
                       date_subset = T)
  df1 <- df[[1]]
  # Loop through parameter setting ---------------------------------------------
  param_res <- list()
  for (h in 1:nrow(params)){
    # Empty grid
    closed_orders <- data.table(batch = NA_integer_,
                                grid = NA_real_,
                                init_price = NA_real_,
                                status_enter=NA_character_,
                                status_exit = NA_character_)
    closed_orders[, trade_id := NA_character_]
    closed_orders[, interval_enter :=POSIXct()]
    closed_orders[, interval_exit :=POSIXct()]
    closed_orders[, position := NA_character_]
    closed_orders[, exits:=NA_real_]
    closed_orders[, bar_entered:=NA_real_]
    closed_orders[, bar_exited:=NA_real_]
    
    # Loop through dataframe split ---------------------------------------------
    list_df <- split_dataframe(df1, params$splits_reset[h])
    
    
    for (s in 1:length(list_df)){
      temp <- list_df[[s]]
      first_low <- head(temp$low,1)
      first_high <- head(temp$high,1)
        
      # Initiate step
      step <- (params$maxim[h] - params$start[h])/(params$n_trades[h])
        
      # Initiate short grid
      short_grid <- data.table(batch = sample(x = 1:10E5,1),
                               grid = first_high*(1+seq(params$start[h], params$maxim[h], by=step)),
                               init_price = first_high,
                               status_enter="open",
                               status_exit = "open")
      short_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
      short_grid[, interval_enter :=POSIXct()]
      short_grid[, interval_exit :=POSIXct()]
      short_grid[, position := "short"]
      short_grid[, exits:=min(short_grid[, grid])*(1-params$exit[h])]
      setorder(short_grid, grid)
        
        # Initiate long grid
      long_grid <- data.table(batch = sample(x = 1:10E5,1),
                              grid = first_low*(1-seq(params$start[h], params$maxim[h], by=step)),
                              init_price = first_low,
                              status_enter="open",
                              status_exit = "open")
      long_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
      long_grid[, interval_enter :=POSIXct()]
      long_grid[, interval_exit :=POSIXct()]
      long_grid[, position := "long"]
      long_grid[, exits:=max(short_grid[, grid])*(1+params$exit[h])]
      setorder(long_grid, -grid)
      
      # Combine grids
      grid <- rbind(short_grid, long_grid)
      grid[, bar_entered:=NA_real_]
      grid[, bar_exited:=NA_real_]
      
      grid <- rbind(grid, closed_orders)
      # Define entries
      high <- temp[, high]
      low <- temp[, low]
        
      # Entries
      enter_long <- grid[position =="long" & status_enter =="open", grid]
      enter_short <- grid[position =="short" & status_enter =="open", grid]
        
      # Get indeces of entries and short
      first_index_long <- sapply(enter_long, function(x) which(low<x)[1])
      first_index_short <- sapply(enter_short, function(x) which(high > x)[1])
        
      # Update grid table
      grid[position =="long" & status_enter =="open", interval_enter:=df1[first_index_long, interval]]
      grid[position =="long" & status_enter =="open", bar_entered:=first_index_long]
      grid[position =="short" & status_enter =="open", interval_enter:=df1[first_index_short, interval]]
      grid[position =="short" & status_enter =="open", bar_entered:=first_index_short]
      grid[!is.na(interval_enter), status_enter := "closed"]
        
      # Exits
      exit_long <- unique(grid[position =="long" & status_enter =="closed" & status_exit=="open", exits])
      exit_short <- unique(grid[position =="short" & status_enter =="closed" & status_exit=="open", exits])
        
      # Indeces of exits for long and short
      first_index_long <- sapply(grid[position =="long" & status_enter =="closed" & status_exit=="open", bar_entered],
                                 function(x) which(high[x:length(high)]>exit_long)[1]+x)
      first_index_short <- sapply(grid[position =="short" & status_enter =="closed" & status_exit=="open", bar_entered],
                                  function(x) which(low[x:length(high)]<exit_short)[1]+x)
      # Update grid table for exits
      grid[position =="long" & status_enter =="closed"& status_exit=="open", interval_exit:=df1[first_index_long, interval]]
      grid[position =="long" & status_enter =="closed"& status_exit=="open", bar_exited:=first_index_long]
      grid[position =="short" & status_enter =="closed"& status_exit=="open", interval_exit:=df1[first_index_short, interval]]
      grid[position =="short" & status_enter =="closed"& status_exit=="open", bar_exited:=first_index_short]
      grid[!is.na(interval_exit), status_exit := "closed"]
      
      # These are going to be carried over
      closed_orders <- grid[status_enter =="closed"]
    }
    # Here calculate result, add total bet, returns, day, hodl etc
    closed_orders[, final_exit := ifelse(status_enter =="closed" & status_exit =="closed", exits, tail(df1[, close], 1))]
    closed_orders[, percent := (final_exit-grid)/grid]
    closed_orders[position == "short", percent:=-1*percent]
    closed_orders[, quote_res_no_fees := bet+bet*percent]
    closed_orders[, quote_res_clean := quote_res_no_fees-(0.65/100)*quote_res_no_fees]
    closed_orders[, bet := bet]
    
    
    param_result <- copy(params[h,])
    param_result[, percent := (sum(closed_orders$quote_res_clean)-sum(closed_orders$bet))/(sum(closed_orders$bet))]
    param_result[, total_bet := sum(closed_orders$bet)]
    param_result[, quote_res := total_bet*percent+total_bet]
    param_result[, day := dates_vector[x]]
    param_result[, hodl := (tail(df1[, close], 1)-head(df1[, close], 1))/head(df1[, close], 1)]
    param_res[[h]] <- param_result
    # print(h)
  }
  
  daily_res[[x]] <- rbindlist(param_res)
  print(x)
}
# Save
save(daily_res, file=paste0("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Grid/results/", paste0(ticks,"_", units,"_", pair, Sys.time()), ".Rdata"))

# candles(data =df1)+
#   geom_hline(yintercept = grid[position == "long", exits], colour="darkgreen")+
#   geom_hline(yintercept = grid[position == "long", grid], colour="green")+
#   geom_hline(yintercept = grid[position == "short", exits], colour="darkred")+
#   geom_hline(yintercept = grid[position == "short", grid], colour="red")+
#   geom_point(data=grid[!is.na(interval_enter) & position == "long"], aes(x=interval_enter, y=grid), fill="lightblue3",colour="black", shape =24, size=2)+
#   geom_point(data=grid[!is.na(interval_exit) & position == "long"], aes(x=interval_exit, y=exits), fill="lightblue3", colour="black",shape =25, size=2)+
#   geom_point(data=grid[!is.na(interval_enter) & position == "short"], aes(x=interval_enter, y=grid), fill="darkorchid1", colour="black", shape =25, size=2)+
#   geom_point(data=grid[!is.na(interval_exit) & position == "short"], aes(x=interval_exit, y=exits), fill="darkorchid1", colour="black",shape =24, size=2)+
#   geom_vline(xintercept = grid[position == "short", interval_exit])


