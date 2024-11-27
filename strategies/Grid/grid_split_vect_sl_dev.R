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
data_path <- "Data"

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
tmp <- df[[1]]
tmp[, date:=as.Date(interval)]
gc()

# Bet size
n_t <- 20
sum(seq(5,1000, 5)[1:n_t])*2

# determine number of splits
# one week, 2 weeks, 3 weeks, 4 weeks

nrow(tmp)/(7*24*4)

# Set parameters table
splits_reset <- data.table(splits_reset=c(12, 16, 25, 50),flag=1)
exit_points <- data.table(exit=seq(0.01,0.20 ,0.01), flag=1)
start <-data.table(start= seq(0.01, 0.1, 0.01),flag=1)
maxim <- data.table(maxim = c(0.1,0.15, 0.2),flag=1)
n_trades <- data.table(n_trades=seq(5, 20, 5),flag=1)
SL <- data.table(sl=seq(0.05, 0.1, 0.01),flag=1)
params <- left_join(splits_reset,exit_points)%>%left_join(start)%>%left_join(maxim)%>%left_join(n_trades)%>%left_join(SL)

# For trade Ids
all_chars <- c(LETTERS, 0:9)
str_len <- 20


x <-1
# 
h <- 1
# s <- 1
gc()
s <- 1
# p <- profvis({

# Loop through calendar days ---------------------------------------------------

results <- list()

for (h in 1:nrow(params)){
  
  list_df <- split_dataframe(tmp, params$splits_reset[h])
  offset <- unlist(lapply(list_df, nrow))
  offset[1] <- 1
  offset <- cumsum(offset)
  names(offset)<- NULL
  # closed_grid <- list()
  # open_grid <- list()
  batch <- list()
  for (s in 1:length(list_df)){
    df1 <- list_df[[s]]
    
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
                      batch=s,
                      batch_offset=offset[s],
                      interval_enter = POSIXct(),
                      interval_exit_tp = POSIXct(),
                      interval_exit_sl = POSIXct(),
                      position = "short",
                      exits = min(short_grid[, grid])*(1-params$exit[h]),
                      SL = max(grid)+max(grid)*params$sl[h],
                      SL_act = F,
                      bet = seq(5, nrow(short_grid)*5, 5)
    )]
    setorder(short_grid, grid)
    
    long_grid <- data.table(batch = sample(x = 1:10E5,1),
                            grid = first_low*(1-seq(params$start[h], params$maxim[h], by=step)),
                            init_price = first_low,
                            status_enter="open",
                            status_exit = "open")
    
    long_grid[, `:=`(trade_id = replicate(nrow(long_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = "")),
                     batch=s, 
                     batch_offset=offset[s],
                     interval_enter = POSIXct(),
                     interval_exit_tp = POSIXct(),
                     interval_exit_sl = POSIXct(),
                     position = "long",
                     exits = max(long_grid[, grid])*(1+params$exit[h]),
                     SL = min(grid)-min(grid)*params$sl[h],
                     SL_act = F,
                     bet = seq(5, nrow(short_grid)*5, 5)
    )]
    setorder(long_grid, grid)
    
    
    # Combine grids
    grid <- rbind(short_grid, long_grid)
    grid[, `:=`(bar_entered = NA_real_, bar_exited_tp =NA_real_, bar_exited_sl =NA_real_)]
    
    # grid <-rbind(grid, open_grid[[(s-1)]], fill=T)
    # grid <- rbind(grid, closed_orders)
    # Define entries
    high <- df1[, high]
    low <- df1[, low]
    close <- df1[, close]
    # Entries
    setkey(grid, position, status_enter, status_exit)
    
    enter_long <- grid[.("long", "open"), grid]
    enter_short <- grid[.("short", "open"), grid]
    
    # Get indeces of entries and short
    first_index_long <- sapply(enter_long, function(x) which(low<x)[1])
    first_index_short <- sapply(enter_short, function(x) which(high > x)[1])
    
    # Update grid 
    grid[.("long", "open"), `:=` (interval_enter = df1[first_index_long, interval],
                                  bar_entered = first_index_long)]
    
    grid[.("short", "open"), `:=` (interval_enter = df1[first_index_short, interval],
                                   bar_entered = first_index_short)]
    
    grid[!is.na(interval_enter), status_enter := "closed"]
    
    grid[, bar_entered := bar_entered + batch_offset]
    grid <-grid[status_enter !="open"]
    
    batch[[s]] <- grid
    # # Exits
    
    
  }
  
  all_batches <- rbindlist(batch)
  high_tmp <- tmp[, high]
  low_tmp <- tmp[, high]
  
  longg <- all_batches[position == "long"]
  shortt <- all_batches[position == "short"]
  
  # TP
  rr_long_tp <- function(exit,bar){
    r <- which(high_tmp[bar:length(high_tmp)]>exit)[1]+bar
    return(r)
  }
  rr_short_tp <- function(exit,bar){
    r <- which(low_tmp[bar:length(low_tmp)]<exit)[1]+bar
    return(r)
  }
  
  # SL
  rr_long_sl <- function(exit,bar){
    r <- which(low_tmp[bar:length(low_tmp)]<exit)[1]+bar
    return(r)
  }
  rr_short_sl <- function(exit,bar){
    r <- which(high_tmp[bar:length(high_tmp)]>exit)[1]+bar
    return(r)
  }
  
  
  long_exit <- mapply(rr_long_tp, longg[, exits], longg[, bar_entered])
  short_exit <- mapply(rr_short_tp, shortt[, exits], shortt[, bar_entered])
  
  long_exit_sl <- mapply(rr_long_sl, longg[, SL], longg[, bar_entered])
  short_exit_sl <- mapply(rr_short_sl, shortt[, SL], shortt[, bar_entered])
  
  longg[, interval_exit_tp := tmp[long_exit, interval]]
  longg[, bar_exited_tp := long_exit]
  
  shortt[, interval_exit_tp := tmp[short_exit, interval]]
  shortt[, bar_exited_tp := short_exit]
  
  longg[, interval_exit_sl := tmp[long_exit_sl, interval]]
  longg[, bar_exited_sl := long_exit_sl]
  
  shortt[, interval_exit_sl := tmp[short_exit_sl, interval]]
  shortt[, bar_exited_sl := short_exit_sl]
  
  final_grid <- rbind(longg, shortt)
  final_grid[is.na(bar_exited_tp), bar_exited_tp:= 10E6]
  final_grid[is.na(bar_exited_sl), bar_exited_sl:= 10E6]
  final_grid[, SL_used := bar_exited_sl<bar_exited_tp]
  final_grid[, bar_exited := ifelse(SL_used == T, bar_exited_sl, bar_exited_tp)]
  final_grid[, interval_exited := as.POSIXct(ifelse(SL_used == T, interval_exit_sl, interval_exit_tp))]
  final_grid[SL_used ==T, exit_price := SL]
  final_grid[SL_used ==F, exit_price := exits]
  final_grid[is.na(interval_exited), exit_price := tail(tmp$close, 1)]
  final_grid[!is.na(interval_exited), status_exit := "closed"]
  final_grid[, percent := ((exit_price - grid)/grid) - 2*0.0025]
  final_grid[position =="short", percent := -1*percent]
  final_grid[, usd_res := percent*bet]
  final_grid[, trade_pos_outcome := ifelse(percent >0, T, F)]
  
  param_result <- copy(params[h,])
  param_result[, aver_percent := mean(final_grid$percent)]
  
  param_result[, total_bet := sum(final_grid$bet)]
  param_result[, quote_res := sum(final_grid$usd_res)]
  param_result[, total_percent := quote_res/total_bet]
  param_result[, total_trades := nrow(final_grid)]
  param_result[, win_rate := sum(final_grid$trade_pos_outcome)/nrow(final_grid)]
  param_result[, biggest_win_usd := max(final_grid$usd_res)]
  param_result[, biggest_win_per := max(final_grid$percent)]
  
  param_result[, biggest_loss_usd := min(final_grid$usd_res)]
  param_result[, biggest_loss_per := min(final_grid$percent)]
  param_result[, hodl := (tail(tmp[, close], 1)-head(tmp[, close], 1))/head(tmp[, close], 1)]
  
  results[[h]] <- param_result
  print(h)
  
}  

View(rbindlist(results))

# })
# htmlwidgets::saveWidget(p, "profile.html")
# Save
save(daily_res, file=paste0("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Grid/results/", paste0(ticks,"_", units,"_", pair, Sys.time()), ".Rdata"))
