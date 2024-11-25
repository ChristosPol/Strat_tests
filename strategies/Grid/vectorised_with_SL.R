rm(list=ls())
gc()
# Source functions
library(profvis)
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
pair <- "MANAUSD"
# pair <- "SHIBEUR"
# Path to save results
data_path <- "Code/Parameter_optim/Data"

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

# Load data
ticks <- c(60)
units <- c("minutes")
intervals <- paste(ticks, units, sep = " ") 

df <- trades_to_OHLC(pair = pair,
                     interval = intervals,
                     from_date = "2023-03-27",
                     to_date = "2023-03-30",
                     date_subset = F)
df1 <- df[[1]]
df1[, date:=as.Date(interval)]
gc()
# Fix vector of dates
dates_vector <- unique(as.Date(df1$interval))
dates_vector <- sample(x = dates_vector, 1, replace = F)



# get the datasets
dataframes <- lapply(dates_vector, function(x){
  df <- df1[date >=x & date < x+(days(7))]
  # df <- df1[date ==x ]
  return(df)
})

# Bet size
n_t <- 20
sum(seq(5,1000, 5)[1:n_t])*2

# Set parameters table
splits_reset <- data.table(splits_reset=c(1),flag=1)
exit_points <- data.table(exit=seq(0.01,0.20 ,0.01), flag=1)
start <-data.table(start= seq(0.01, 0.1, 0.01),flag=1)
maxim <- data.table(maxim = c(0.1,0.15, 0.2),flag=1)
n_trades <- data.table(n_trades=seq(5, 20, 5),flag=1)
SL <- data.table(sl=seq(0.01, 0.1, 0.01),flag=1)
params <- left_join(splits_reset,exit_points)%>%left_join(start)%>%left_join(maxim)%>%left_join(n_trades)%>%left_join(SL)




# For trade Ids
all_chars <- c(LETTERS, 0:9)
str_len <- 20


x <-1
# 
h <- 1
# s <- 1
gc()
# p <- profvis({
  
# Loop through calendar days ---------------------------------------------------
daily_res <- list()
for (x in 1:length(dataframes)){

  df1 <- dataframes[[x]]
  
  # Loop through parameter setting ---------------------------------------------
  param_res <- list()
  for (h in 1:nrow(params)){

    first_low <- head(df1$low,1)
    first_high <- head(df1$high,1)
      
    # Initiate step
    step <- (params$maxim[h] - params$start[h])/(params$n_trades[h])
      
    # Initiate short grid
    short_grid <- data.table(batch = sample(x = 1:10E5,1),
                             grid = first_high*(1+seq(params$start[h], params$maxim[h], by=step)),
                             init_price = first_high,
                             status_enter="open",
                             status_exit = "open")
    short_grid[, `:=`(trade_id = replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = "")),
              interval_enter = POSIXct(),
              interval_exit = POSIXct(),
              position = "short",
              exits = min(short_grid[, grid])*(1-params$exit[h]),
              SL = max(grid)+max(grid)*params$sl[h],
              SL_act = F,
              bet = seq(5, nrow(short_grid)*5, 5)
              )]
    setorder(short_grid, grid)
    # short_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
    # short_grid[, interval_enter :=POSIXct()]
    # short_grid[, interval_exit :=POSIXct()]
    # short_grid[, position := "short"]
    # short_grid[, exits:=min(short_grid[, grid])*(1-params$exit[h])]
    # setorder(short_grid, grid)
    # short_grid[, SL := max(grid)+max(grid)*params$sl[h]]
    # short_grid[, SL_act := F]
    # short_grid[, bet:= seq(5, nrow(short_grid)*5, 5)]
    # Initiate long grid
    long_grid <- data.table(batch = sample(x = 1:10E5,1),
                            grid = first_low*(1-seq(params$start[h], params$maxim[h], by=step)),
                            init_price = first_low,
                            status_enter="open",
                            status_exit = "open")
    
    long_grid[, `:=`(trade_id = replicate(nrow(long_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = "")),
              interval_enter = POSIXct(),
              interval_exit = POSIXct(),
              position = "long",
              exits = max(long_grid[, grid])*(1+params$exit[h]),
              SL = min(grid)-min(grid)*params$sl[h],
              SL_act = F,
              bet = seq(5, nrow(short_grid)*5, 5)
    )]
    setorder(long_grid, grid)
    
    
    
    # long_grid[, trade_id := replicate(nrow(long_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
    # long_grid[, interval_enter :=POSIXct()]
    # long_grid[, interval_exit :=POSIXct()]
    # long_grid[, position := "long"]
    # long_grid[, exits:=max(long_grid[, grid])*(1+params$exit[h])]
    # setorder(long_grid, -grid)
    # long_grid[, SL := min(grid)-min(grid)*params$sl[h]]
    # long_grid[, SL_act := F]
    # long_grid[, bet:= seq(5, nrow(long_grid)*5, 5)]
    
    # Combine grids
    grid <- rbind(short_grid, long_grid)
    grid[, `:=`(bar_entered = NA_real_, bar_exited =NA_real_)]
    # grid[, bar_entered:=NA_real_]
    # grid[, bar_exited:=NA_real_]
    
    # grid <- rbind(grid, closed_orders)
    # Define entries
    high <- df1[, high]
    low <- df1[, low]
    close <- df1[, close]
    # Entries
    setkey(grid, position, status_enter, status_exit)
    
    enter_long <- grid[.("long", "open"), grid]
    enter_short <- grid[.("short", "open"), grid]
    # enter_long <- grid[position =="long" & status_enter =="open", grid]
    # enter_short <- grid[position =="short" & status_enter =="open", grid]
    
    # Get indeces of entries and short
    first_index_long <- sapply(enter_long, function(x) which(low<x)[1])
    first_index_short <- sapply(enter_short, function(x) which(high > x)[1])
    
    # Update grid 
    grid[.("long", "open"), `:=` (interval_enter = df1[first_index_long, interval],
                                                          bar_entered = first_index_long)]
    
    grid[.("short", "open"), `:=` (interval_enter = df1[first_index_short, interval],
                                                          bar_entered = first_index_short)]
    
    grid[!is.na(interval_enter), status_enter := "closed"]
    
    # Exits
    exit_long <- unique(grid[.("long" ,"closed" ,"open"), exits])
    exit_short <- unique(grid[.("short" ,"closed" ,"open"), exits])
    
    # Indeces of exits for long and short
    first_index_long <- sapply(grid[position =="long" & status_enter =="closed" & status_exit=="open", bar_entered],
                               function(x) which(high[x:length(high)]>exit_long)[1]+x)
    first_index_long[is.na(first_index_long)] <- 10E5
    exit_df_long <- data.table(tp = first_index_long, pos = "long", type="tp")
    
    
    first_index_short <- sapply(grid[position =="short" & status_enter =="closed" & status_exit=="open", bar_entered],
                                function(x) which(low[x:length(high)]<exit_short)[1]+x)
    first_index_short[is.na(first_index_short)] <- 10E5
    # names(first_index_short) <- rep("exit", length(first_index_short))
    exit_df_short <- data.table(tp = first_index_short, pos = "short", type="tp")
    
    # Sls
    SL_long <- unique(grid[position =="long" & status_enter =="closed" & status_exit=="open", SL])
    SL_short <- unique(grid[position =="short" & status_enter =="closed" & status_exit=="open", SL])
    
    # Indeces of SLs for long and short
    SL_index_long <- sapply(grid[position =="long" & status_enter =="closed" & status_exit=="open", bar_entered],
                               function(x) which(close[x:length(close)]<SL_long)[1]+x)
    SL_index_long[is.na(SL_index_long)] <- 10E5
    SL_df_long <- data.table(sl = SL_index_long, pos = "long", type = "sl")
    
    SL_index_short <- sapply(grid[position =="short" & status_enter =="closed" & status_exit=="open", bar_entered],
                                function(x) which(close[x:length(close)]>SL_short)[1]+x)
    SL_index_short[is.na(SL_index_short)] <- 10E5
    SL_df_short <- data.table(sl = SL_index_short, pos = "short", type = "sl")
    

    
    if(nrow(grid[position =="long" & status_enter =="closed" & status_exit=="open"])>0){
      exit_sl_tp_long <- cbind(SL_df_long,exit_df_long)
      exit_sl_tp_long[, SL_TRUE:= sl<tp]
      
      grid[position =="long" & status_enter =="closed" & status_exit=="open", `:=` (interval_exit = df1[ifelse(exit_sl_tp_long$SL_TRUE == T, exit_sl_tp_long$sl, exit_sl_tp_long$tp), interval],
                                                                                   bar_exited=ifelse(exit_sl_tp_long$SL_TRUE == T, exit_sl_tp_long$sl, exit_sl_tp_long$tp),
                                                                                   SL_act = ifelse(exit_sl_tp_long$SL_TRUE == T, T, F))]
        
    } 
    
    
    if(nrow(grid[position =="short" & status_enter =="closed" & status_exit=="open"])>0){
      exit_sl_tp_short <- cbind(SL_df_short, exit_df_short)
      exit_sl_tp_short[, SL_TRUE:= sl<tp]
      grid[position =="short" & status_enter =="closed" & status_exit=="open", `:=` (interval_exit = df1[ifelse(exit_sl_tp_short$SL_TRUE == T, exit_sl_tp_short$sl, exit_sl_tp_short$tp), interval],
                                                                                    bar_exited = ifelse(exit_sl_tp_short$SL_TRUE == T, exit_sl_tp_short$sl, exit_sl_tp_short$tp),
                                                                                    SL_act = ifelse(exit_sl_tp_short$SL_TRUE == T, T, F))]
      
    }
    
    grid[!is.na(interval_exit), status_exit := "closed"]
  
    # Here calculate result, add total bet, returns, day, hodl etc
    
    grid[.("closed" ,"closed"), final_exit := exits]
    grid[status_enter =="closed" & status_exit =="closed" & SL_act == T, final_exit := SL]
    grid[status_enter =="closed" & status_exit =="open" & is.na(final_exit), final_exit := tail(df1[, close], 1)]
    
    grid[, percent := (final_exit-grid)/grid]
    grid[position == "short", percent:=-1*percent]
    grid[, quote_res_no_fees := bet+bet*percent]
    grid[, quote_res_clean := quote_res_no_fees-(0.65/100)*quote_res_no_fees]
    grid[, bet := bet]
    grid <- grid[status_enter =="closed"]
    
    param_result <- copy(params[h,])
    
    # Precompute sums outside the table update if used multiple times
    total_bet <- sum(grid$bet)
    quote_res_clean_sum <- sum(grid$quote_res_clean)
    bet_sum <- sum(grid$bet)
    
    # Batch update `param_result` columns for efficiency
    param_result[, `:=`(
      percent = (quote_res_clean_sum - bet_sum) / bet_sum,
      total_bet = total_bet,
      quote_res = total_bet * ((quote_res_clean_sum - bet_sum) / bet_sum + 1),
      day = dates_vector[x],
      hodl = (tail(df1[, close], 1) - head(df1[, close], 1)) / head(df1[, close], 1)
    )]
    
    param_res[[h]] <- param_result
    
  }
  
  daily_res[[x]] <- rbindlist(param_res)
  print(x)
}

# })
# htmlwidgets::saveWidget(p, "profile.html")
# Save
save(daily_res, file=paste0("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Grid/results/", paste0(ticks,"_", units,"_", pair, Sys.time()), ".Rdata"))
