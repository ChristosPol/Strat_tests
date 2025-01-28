rm(list=ls())
gc()



setDTthreads(1)
library(doParallel)
library(foreach)
library(data.table)

# Source functions
funds <- 10000
bet <- 5

library(profvis)
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Path to save results
data_path <- "Data"
data <- list.files(data_path, full.names = T)
names <- list.files(data_path, full.names = F)

# selected <- sample(names, 10,replace = F)


# idx <- which(names %in%selected)

# data <- data[idx]
# names <- names[idx]
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
# data <- data[idx]
# names <- names[idx]
# get all pairs equal dates!
# Need to calculate bet size of everything together
g_d <- function(x) {
  mi <- as.Date(min(x$interval))
  ma <- as.Date(max(x$interval))
  return(c(mi, ma))
}

periods <- lapply(data_list, function(x) g_d(x))
periods <- data.table(left=as.Date(unlist(lapply(periods, "[[", 1))), right= as.Date(unlist(lapply(periods, "[[", 2))))
setDT(periods)
periods[, pair := names]
periods[, pair:as.factor(pair)]


# ggplot(periods, aes(y = pair)) +
#   geom_segment(aes(x = left, xend = right, y = pair, yend = pair, color = pair), size = 1.5) +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 12), legend.position = "none")


# Define which pairs have the same time period
# rand_left <- sample(seq(min(periods$left),max(periods$left), "days"), 1)
# left_date <- rand_left
# right_date <- as.Date(left_date)+ days(180)
# 24 hours_0.3_5

calendar_times <- function(x){
  times <- data.frame(interval_updated = seq(min(x$interval), max(x$interval), "hours"), flag =1)
  x <- merge(times, x, by.x="interval_updated", by.y="interval", all.x = T)
  x$high <- na.locf(x$high)
  x$low <- na.locf(x$low)
  x$close <- na.locf(x$close)
  x$open <- na.locf(x$open)
  x$volume[is.na(x$volume)] <- 0
  x$pair  <- na.locf(x$pair)
  x$date <- as.Date(x$interval_updated)
  colnames(x)[which(colnames(x)== "interval_updated")] <- "interval"
  setDT(x)
  return(x)
}

# Function to filter and extract exact period
filter_and_extract <- function(dt_list, left_date, right_date) {
  dt_list_filtered <- lapply(dt_list, function(dt) {
    dt_min <- min(dt$date)
    dt_max <- max(dt$date)
    if (left_date >= dt_min && right_date <= dt_max) {
      # Subset to the exact target period
      extracted_data <- dt[date >= left_date & date <= right_date]
      # Return only if the subset is not empty
      if (nrow(extracted_data) > 0) {
        return(extracted_data)
      }
    }
    # Exclude if the period is not covered
    return(NULL)
  })
  # Remove NULL elements
  dt_list_filtered <- dt_list_filtered[!sapply(dt_list_filtered, is.null)]
  return(dt_list_filtered)
}

# Apply the function
data_list <- filter_and_extract(data_list, left_date, right_date)
data_list <- lapply(data_list, calendar_times)


names <- unique(unlist(lapply(data_list, "[[", "pair")))
periods <- lapply(data_list, function(x) g_d(x))
periods <- data.table(left=as.Date(unlist(lapply(periods, "[[", 1))), right= as.Date(unlist(lapply(periods, "[[", 2))))
setDT(periods)
periods[, pair := names]
periods[, pair:as.factor(pair)]

selected <- sample(names, 50)
# selected <- names
idx <- which(names %in%selected)
data <- data[idx]
names <- names[idx]
data_list <- data_list[idx]
# 96 selected

# For trade Ids
all_chars <- c(LETTERS, 0:9)
str_len <- 20
fund_list_param <- list()
fund_list_pair <- list()
pair_results <- list()
# Loop through all pairs

i <-1
library(doSNOW)
cl <- makeCluster(6)
registerDoSNOW(cl)
iterations <- length(data_list)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# num_cores <- detectCores() - 5
# cl <- makeCluster(num_cores)
# registerDoParallel(cl)
start_time <- Sys.time()
number_trades <- 8 -2 # two are added in total 8


pair_results <- foreach(i = seq_along(data_list), .packages = c("data.table", "dplyr"),.options.snow = opts) %dopar% {

  tmp <- copy(data_list[[i]])
  tmp_size <- nrow(tmp)
  pair <- unique(tmp$pair)
  # Precompute constants
  look_back <- data.table(bar = floor(tmp_size / c(24)), flag = 1)
  TP <- data.table(tp = c(0.02), flag = 1)
  median_number <- data.table(med_num = 5, flag = 1)
  start_point <- data.table(start_point = c(0.1), flag = 1)
  end_point <- data.table(end_point = c(0.5), flag = 1)
  n_trades <- data.table(n_trades = number_trades, flag = 1)
  
  
  params <- left_join(look_back,TP)%>%left_join(median_number)%>%
    left_join(start_point)%>%
    left_join(end_point)%>%
    left_join(n_trades)
  params[bar == floor(nrow(tmp)/(24)), bar_day := "24 hours"]
  # params[bar == floor(nrow(tmp)/(48)), bar_day := "48 hours"]
  # params[bar == floor(tmp_size/(72)), bar_day := "72 hours"]
  # params[bar == floor(nrow(tmp)/(168)), bar_day := "168 hours"]
  # params[bar == floor(nrow(tmp)/(504)), bar_day := "504 hours"]
  # params[bar == floor(nrow(tmp)/(336)), bar_day := "336 hours"]
  params[, step := (end_point-start_point)/n_trades]
  
  # Process params
  results <- vector("list", nrow(params))
  fund_list_param <- vector("list", nrow(params))
  
  for (h in 1:nrow(params)){
    grid <-  -1*seq(params$start_point[h], params$end_point[h], params$step[h])
    list_df <- split_dataframe(tmp, params$bar[h])
    offset <- unlist(lapply(list_df, nrow))
    offset[1] <- 1
    offset <- cumsum(offset)
    names(offset)<- NULL
    
    batch <- list()
    s <- 2
    for (s in 2:length(list_df)){
      
      df1_previous <- list_df[[(s-1)]]
      
      # Low or close?
      support <- median(sort(df1_previous$close)[1:params$med_num[h]])
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
                       exits = max(long_grid[, grid])*(1+params$tp[h]),
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
      # TP
      rr_long_tp <- function(exit,bar){
        r <- which(high_tmp[bar:length(high_tmp)]>exit)[1]+bar
        return(r)
      }
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
      dd <- cbind(dd, params[h,])
      fund_list_param[[h]] <- dd
      param_result <- copy(params[h,])
      param_result[, aver_percent := mean(final_grid$percent)]
      param_result[, total_bet := sum(final_grid$bet)]
      param_result[, quote_res := sum(final_grid$usd_res)]
      param_result[, total_percent := quote_res/funds]
      param_result[, total_trades := nrow(final_grid)]
      param_result[, win_rate := sum(final_grid$trade_pos_outcome)/nrow(final_grid)]
      param_result[, biggest_win_usd := max(final_grid$usd_res)]
      param_result[, biggest_win_per := max(final_grid$percent)]
      
      param_result[, biggest_loss_usd := min(final_grid$usd_res)]
      param_result[, biggest_loss_per := min(final_grid$percent)]
      param_result[, hodl := (tail(tmp[, close], 1)-head(tmp[, close], 1))/head(tmp[, close], 1)]
      param_result[, pair := pair]
      # param_result[, exceeded_funds := dd]
      results[[h]] <- param_result
      # print(param_result)
      print(paste0("i is: ", i, " and h is: ", h))
      print(param_result)
    }

  }  
  

  list(pair_results = rbindlist(results), fund_list_pair = fund_list_param)
}
close(pb)
stopCluster(cl)

end_time <- Sys.time()
total_time <- start_time-end_time

# Combine results
pair_results_tt <- rbindlist(lapply(pair_results, `[[`, "pair_results"))
fund_list_pair_tt <- lapply(pair_results, `[[`, "fund_list_pair")
fund_list_pair_tt <- rbindlist(lapply(X =fund_list_pair_tt, rbindlist ))

fund_list_pair_tt[, param_concatenated := paste(bar_day, tp, med_num,start_point,end_point,n_trades, step, sep="_"), by =.I]


# start_date <- as.POSIXct(min(fund_list_pair_tt$interval_enter), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# end_date <- as.POSIXct(max(fund_list_pair_tt$interval_enter), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# date_sequence <- seq(from = start_date, to = end_date, by = "hour")
# my_dates <- data.table(all_dates = date_sequence, flag = 1)
# test <- merge(my_dates, fund_list_pair_tt, by.x ="all_dates", by.y = "interval_enter", all.x = T)

exceeded_funds_bool <- fund_list_pair_tt[, list(sum_funds=sum(funds_pair), sum_entr =sum(entr), sum_ex = sum(exit)), by = list(interval_enter, param_concatenated)]
# exceeded_funds_bool[is.na(sum_funds), sum_funds := 0]
# exceeded_funds_bool[is.na(sum_entr), sum_entr := 0]
# exceeded_funds_bool[is.na(sum_ex), sum_ex := 0]
setorder(exceeded_funds_bool, param_concatenated, interval_enter)
exceeded_funds_bool[, cum_sum_entries := cumsum(sum_entr), by = param_concatenated]
exceeded_funds_bool[, cum_sum_exits := cumsum(sum_ex), by = param_concatenated]
exceeded_funds_bool[, balance := funds-cum_sum_entries+cum_sum_exits]
exceeded_funds_num <- exceeded_funds_bool[, list(overhead = min(balance)), by =param_concatenated]

ggplot(exceeded_funds_bool, aes(x = interval_enter, y = balance, colour= param_concatenated))+
  geom_line()+theme(legend.position = "none")

exceeded_funds_bool[, exceeded_funds := any(balance<0), by = param_concatenated]
exceeded_funds_bool <- unique(exceeded_funds_bool[, .(param_concatenated, exceeded_funds)])

pair_results_tt[, param_concatenated := paste(bar_day, tp, med_num,start_point,end_point,n_trades, step, sep="_"), by =.I]
metrics <- pair_results_tt[, list(sum_bet = sum(total_bet),
                                  sum_quote = sum(quote_res),
                                  mean_hodl = median(hodl)), by=.(param_concatenated)]
metrics[, percent:= sum_quote/funds]
metrics <- merge(metrics, exceeded_funds_bool, all.x = T)
metrics <- merge(metrics, exceeded_funds_num, all.x = T)
setorder(metrics, -percent)
metrics

# Equity
# test <- fund_list_pair_tt[pair == "ACAUSD"]


metrics_pair <- pair_results_tt[, list(sum_bet = sum(total_bet),
                                  sum_quote = sum(quote_res),
                                  mean_hodl = median(hodl)), by=.(param_concatenated,pair)]

View(rbind(metrics, metrics_pair, fill = T))
