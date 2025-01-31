rm(list=ls())
gc()

# library(profvis)
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

setDTthreads(1)
library(doParallel)
library(foreach)
library(data.table)

# Source functions
funds <- 10000
bet <- 5


# Path to save results
data_path <- "Data"
data <- list.files(data_path, full.names = T)
names <- list.files(data_path, full.names = F)

i <- 1
data_list <- list()
for (i in 1:length(data)){
  pair <- names[i]
  pair_data_results <- paste(data_path, pair, sep ="/")
  ticks <- c(60)
  units <- c("minutes")
  intervals <- paste(ticks, units, sep = " ")
  df <- trades_to_OHLC(pair = pair,
                       interval = intervals,
                       from_date = "2022-01-01",
                       to_date = "2024-12-15",
                       date_subset = F)
  tmp <- df[[1]]
  tmp[, date:=as.Date(interval)]
  tmp[, pair := pair]
  data_list[[i]] <- tmp
  print(i)
  
}
gc()
data_list_bk <- copy(data_list)

# RESTART HERE
left_date <- "2024-01-01"
right_date <- "2025-01-25"
names <- list.files(data_path, full.names = F)
data_list <- copy(data_list_bk)


periods <- lapply(data_list, function(x) g_d(x))
periods <- data.table(left=as.Date(unlist(lapply(periods, "[[", 1))), right= as.Date(unlist(lapply(periods, "[[", 2))))
setDT(periods)
periods[, pair := names]
periods[, pair:as.factor(pair)]


# Apply the function
data_list <- filter_and_extract(data_list, left_date, right_date)
data_list <- lapply(data_list, calendar_times)


names <- unique(unlist(lapply(data_list, "[[", "pair")))
periods <- lapply(data_list, function(x) g_d(x))
periods <- data.table(left=as.Date(unlist(lapply(periods, "[[", 1))), right= as.Date(unlist(lapply(periods, "[[", 2))))
setDT(periods)
periods[, pair := names]
periods[, pair:as.factor(pair)]

# Find the volume the day before
coins <- periods$pair
vol_usd <- lapply(coins, function(x)get_volume_USD(x, date=unique(periods$left)-days(1)))
vol_usd_df <- rbindlist(vol_usd)
periods <- merge(periods, vol_usd_df, by = "pair", all.x =T)


# Use regression to see associations between params and percentage gain
# Parameters
options(scipen = 999)
look_back <- data.table(bar = c(72), flag = 1)
TP <- data.table(tp = c(0.2), flag = 1)
median_number <- data.table(med_num = 5, flag = 1)
start_point <- data.table(start_point = c(0.1), flag = 1)
end_point <- data.table(end_point = c( 0.6), flag = 1)
n_trades <- data.table(n_trades = c(6), flag = 1)
volume_usd <- data.table(usd_vol = c(50000,100000, 200000), flag = 1)
n_pairs <- data.table(n_pairs = c(40, 60 ), flag = 1)
selection <- data.table(selected = c("random"), flag = 1)
params <- left_join(look_back, TP)%>%left_join(median_number)%>%
  left_join(start_point)%>%
  left_join(end_point)%>%
  left_join(n_trades)%>%
  left_join(volume_usd)%>%
  left_join(n_pairs)%>%
  left_join(selection)
params[, step := round((end_point-start_point)/n_trades,3)]

funding_pair <- list()
performance_pair <- list()
# funding_parameter <- list()
# performance_parameter <- list()


i <-1
library(doSNOW)
cl <- makeCluster(6)
registerDoSNOW(cl)
iterations <- nrow(params)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

results <- foreach(i = 1:nrow(params), .packages = c("data.table", "dplyr"),.options.snow = opts) %dopar% {
  
  temp_perf_file <- paste0("strategies/qfl/performance/performance_", i, ".csv")  # Unique file per iteration
  
  
  # Process params
  performance_parameter <- vector("list", nrow(params))
  funding_parameter <- vector("list", nrow(params))
  
  # Selection of pairs ---------------------------------------------------------
  if(params$selected[i] == "random"){
    random_pairs <- periods[usd_vol>= params$usd_vol[i], pair]
    if(length(random_pairs)>params$n_pairs[i]){
      random_pairs <- sample(random_pairs, params$n_pairs[i], replace = FALSE)
    } else{
      random_pairs <- random_pairs
    }
  } else {
    setorder(periods, -usd_vol)
    if(nrow(periods) > params$n_pairs[i]){
      sorted_pairs <- periods[1:params$n_pairs[i], pair]
    } else {
      sorted_pairs <- periods[, pair]
    }
  }
  if(params$selected[i] == "random"){
    selected_pairs <- random_pairs
  } else {
    selected_pairs <- sorted_pairs
  }
  idx <- which(names %in%selected_pairs)
  data_sel <- data[idx]
  names_sel <- names[idx]
  data_list_sel <- data_list[idx]
  
  j <- 1
  for (j in 1:length(selected_pairs)){
    
    tmp <- copy(data_list_sel[[j]])
    tmp_size <- nrow(tmp)
    pair <- unique(tmp$pair)
    look_back <- floor(tmp_size / params$bar[i])
    grid <-  -1*seq(params$start_point[i], params$end_point[i], params$step[i])
    list_df <- split_dataframe(tmp, look_back)
    offset <- unlist(lapply(list_df, nrow))
    offset[1] <- 1
    offset <- cumsum(offset)
    names(offset)<- NULL
    
    batch <- list()
    s <- 2
    for (s in 2:length(list_df)){
      
      df1_previous <- list_df[[(s-1)]]
      # Low or close?
      support <- median(sort(df1_previous$close)[1:params$med_num[i]])
      entries_limit <- c(support, support+support*grid)
      df1 <- list_df[[s]]
      long_grid <- data.table(batch = sample(x = 1:10E5,1),
                              grid = entries_limit,
                              status_enter="open",
                              status_exit = "open")
      
      long_grid[, `:=`(batch=s, 
                       batch_offset=offset[s],
                       interval_enter = as.POSIXct(rep(NA, nrow(long_grid))),
                       interval_exit_tp = as.POSIXct(rep(NA, nrow(long_grid))),
                       position = "long",
                       exits = max(long_grid[, grid])*(1+params$tp[i]),
                       bet = bet
      )]
      setorder(long_grid, grid)
      
      # Combine grids
      long_grid[, `:=`(bar_entered = NA_real_, bar_exited_tp =NA_real_)]
      
      high <- df1[, high]
      low <- df1[, low]
      close <- df1[, close]
      
      # Entries
      setkey(long_grid, status_enter, status_exit)
      enter_long <- long_grid[.("open", "open"), grid]
      
      # Get indeces of entries and short
      first_index_long <- sapply(enter_long, function(x) which(low<x)[1])
      
      # Update grid 
      long_grid[.("open", "open"), `:=` (interval_enter = df1[first_index_long, interval],
                                         bar_entered = first_index_long)]
      
      long_grid[!is.na(interval_enter), status_enter := "closed"]
      long_grid[is.na(interval_enter),  `:=` (status_enter = "cancelled", status_exit = "cancelled")]
      
      long_grid[, bar_entered := bar_entered + batch_offset]
      long_grid <-long_grid[status_enter =="closed"]
      
      batch[[s]] <- long_grid
      # # Exits
    }
    all_batches <- rbindlist(batch)
    if(nrow(all_batches) != 0){
      high_tmp <- tmp[, high]
      low_tmp <- tmp[, high]
      longg <- all_batches[position == "long"]
      
      long_exit <- mapply(rr_long_tp, longg[, exits], longg[, bar_entered])
      longg[, interval_exit_tp := tmp[long_exit, interval]]
      longg[, bar_exited_tp := long_exit]
      final_grid <- copy(longg)
      final_grid[is.na(bar_exited_tp), bar_exited_tp:= nrow(tmp)]
      final_grid[!is.na(interval_exit_tp), price_exits := exits]
      final_grid[is.na(interval_exit_tp), price_exits := tmp$close[nrow(tmp)]]
      final_grid[is.na(interval_exit_tp), interval_exit_tp := tmp$interval[nrow(tmp)]]
      final_grid[, percent := ((price_exits - grid)/grid) - 2*0.0025]
      final_grid[, usd_res := percent*bet]
      final_grid[, trade_pos_outcome := ifelse(percent >0, T, F)]
      dd <- merge(final_grid[, list( entr = sum(bet)), by =interval_enter], final_grid[, list(exit=sum(bet+usd_res)), by =interval_exit_tp], by.x = "interval_enter", by.y ="interval_exit_tp", all = T)
      dd[is.na(entr), entr:= 0]
      dd[is.na(exit), exit:= 0]
      dd[, cumsum_entries:= cumsum(entr)]
      dd[, cumsum_exit:= cumsum(exit)]
      dd[, funds_pair:= -cumsum_entries+cumsum_exit]
      dd[, pair := unique(tmp$pair)]
      dd <- cbind(dd, params[i,])
      funding_pair[[j]] <- dd
      result_tmp <- copy(params[i,])
      result_tmp[, aver_percent := mean(final_grid$percent)]
      result_tmp[, total_bet := sum(final_grid$bet)]
      result_tmp[, quote_res := sum(final_grid$usd_res)]
      result_tmp[, total_percent := quote_res/funds]
      result_tmp[, total_trades := nrow(final_grid)]
      result_tmp[, win_rate := sum(final_grid$trade_pos_outcome)/nrow(final_grid)]
      result_tmp[, biggest_win_usd := max(final_grid$usd_res)]
      result_tmp[,biggest_win_per := max(final_grid$percent)]
      
      result_tmp[, biggest_loss_usd := min(final_grid$usd_res)]
      result_tmp[, biggest_loss_per := min(final_grid$percent)]
      result_tmp[, hodl := (tail(tmp[, close], 1)-head(tmp[, close], 1))/head(tmp[, close], 1)]
      result_tmp[, pair := pair]
      performance_pair[[j]] <- result_tmp
      
      # print(j)
      
    }
      
  }
  # Performance
  metr <- rbindlist(performance_pair)
  metr[, param_concatenated := paste(bar, tp, med_num,start_point,end_point, step, n_trades, usd_vol, n_pairs, selected,sep="_"), by =.I]
  metrix <- metr[, list(sum_bet = sum(total_bet),
                                     sum_quote = sum(quote_res)), by=.(param_concatenated)]
  metrix[, percent:= sum_quote/funds]
  metrix[, mean_hodl :=metr[, median(hodl)]]
  
  # Funding
  fundx <- rbindlist(funding_pair)
  fundx[, param_concatenated := paste(bar, tp, med_num,start_point,end_point, step, n_trades, usd_vol, n_pairs, selected,sep="_"), by =.I]
  exceeded_funds_bool <- fundx[, list(sum_funds=sum(funds_pair), sum_entr =sum(entr), sum_ex = sum(exit)), by = list(interval_enter, param_concatenated)]
  setorder(exceeded_funds_bool, param_concatenated, interval_enter)
  exceeded_funds_bool[, cum_sum_entries := cumsum(sum_entr), by = param_concatenated]
  exceeded_funds_bool[, cum_sum_exits := cumsum(sum_ex), by = param_concatenated]
  exceeded_funds_bool[, balance := funds-cum_sum_entries+cum_sum_exits]
  exceeded_funds_num <- exceeded_funds_bool[, list(overhead = min(balance)), by =param_concatenated]  
  exceeded_funds_bool[, exceeded_funds := any(balance<0), by = param_concatenated]
  exceeded_funds_bool <- unique(exceeded_funds_bool[, .(param_concatenated, exceeded_funds)])
  metrix[, funds_bool := exceeded_funds_bool[, exceeded_funds]]
  metrix[, funds_num_min := exceeded_funds_num[, overhead]]
  list(performance_parameter = rbindlist(list(metrix)))
  # fwrite(metrix, temp_perf_file)
  
}

close(pb)
stopCluster(cl)

param_results_tt$param_concatenated[duplicated(param_results_tt$param_concatenated)]

# Combine results
param_results_tt <- rbindlist(lapply(results, `[[`, "performance_parameter"))

fund_list_pair_tt[, param_concatenated := paste(bar, tp, med_num,start_point,end_point, step, n_trades, usd_vol, n_pairs, selected,sep="_"), by =.I]



exceeded_funds_bool <- fund_list_pair_tt[, list(sum_funds=sum(funds_pair), sum_entr =sum(entr), sum_ex = sum(exit)), by = list(interval_enter, param_concatenated)]
setorder(exceeded_funds_bool, param_concatenated, interval_enter)
exceeded_funds_bool[, cum_sum_entries := cumsum(sum_entr), by = param_concatenated]
exceeded_funds_bool[, cum_sum_exits := cumsum(sum_ex), by = param_concatenated]
exceeded_funds_bool[, balance := funds-cum_sum_entries+cum_sum_exits]
exceeded_funds_num <- exceeded_funds_bool[, list(overhead = min(balance)), by =param_concatenated]

ggplot(exceeded_funds_bool, aes(x = interval_enter, y = balance, colour= param_concatenated))+
  geom_line()+theme(legend.position = "none")

exceeded_funds_bool[, exceeded_funds := any(balance<0), by = param_concatenated]
exceeded_funds_bool <- unique(exceeded_funds_bool[, .(param_concatenated, exceeded_funds)])

param_results_tt[, param_concatenated := paste(bar, tp, med_num,start_point,end_point, step, n_trades, usd_vol, n_pairs, selected,sep="_"), by =.I]
metrics <- param_results_tt[, list(sum_bet = sum(total_bet),
                                  sum_quote = sum(quote_res)), by=.(param_concatenated)]
metrics[, percent:= sum_quote/funds]
param_results_tt[, list(mean_hodl = mean(hodl)), by = pair]

metrics <- merge(metrics, exceeded_funds_bool, all.x = T)
metrics <- merge(metrics, exceeded_funds_num, all.x = T)
setorder(metrics, -percent)
View(metrics)

# Equity
# test <- fund_list_pair_tt[pair == "ACAUSD"]


metrics_pair <- pair_results_tt[, list(sum_bet = sum(total_bet),
                                       sum_quote = sum(quote_res),
                                       mean_hodl = median(hodl)), by=.(param_concatenated,pair)]

View(rbind(metrics, metrics_pair, fill = T))
