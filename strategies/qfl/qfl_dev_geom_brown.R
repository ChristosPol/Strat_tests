rm(list=ls())
gc()
# Source functions
funds <- 10000
bet <- 5

library(tidyverse)
library(lubridate)
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

geom_brown <- function(n, init_price, mu, sigma, dt){
  time_steps <- 1:n
  epsilon <- rnorm(n, mean = 0, sd = 1)
  log_returns <- (mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * epsilon
  prices <- init_price * cumprod(exp(log_returns))
  crypto_data <- data.frame(
    Time = seq.POSIXt(from = Sys.time(), by = "min", length.out = n),
    Price = prices
  )
  ohlc_data <- crypto_data %>%
    mutate(TimeInterval = floor_date(Time, unit = "60 minutes")) %>%
    group_by(TimeInterval) %>%
    summarise(
      Open = first(Price),
      High = max(Price),
      Low = min(Price),
      Close = last(Price)
    ) %>%
    ungroup()
  setDT(ohlc_data)
  setnames(ohlc_data, c("interval","open","high", "low", "close"))
  ohlc_data[, pair := paste(sample(all_chars, str_len, replace = TRUE), collapse = "")]
  return(ohlc_data)
}

# For trade Ids
all_chars <- c(LETTERS, 0:9)
str_len <- 20
fund_list_param <- list()
fund_list_pair <- list()
pair_results <- list()
# Loop through all pairs


candles(geom_brown(n=10000, init_price = 1000, mu = 0.00005, sigma = 0.01, dt=1))
candles(geom_brown(n=10000, init_price = 1000, mu = -0.00005, sigma = 0.01, dt=1))
candles(geom_brown(n=10000, init_price = 1000, mu = 0, sigma = 0.01, dt=1))
data_list1 <- lapply(1:100, function(x)geom_brown(n=100000, init_price = 1000, mu = 0.00005, sigma = 0.01, dt=1))
data_list2 <- lapply(1:100, function(x)geom_brown(n=100000, init_price = 1000, mu = 0.00005, sigma = 0.01, dt=1))
data_list3 <- lapply(1:100, function(x)geom_brown(n=100000, init_price = 1000, mu = 0, sigma = 0.01, dt=1))

data_list<- append(data_list1,data_list2)
data_list <- append(data_list, data_list3)


grid <-  -1*seq(0.01, 0.3, 0.025)
# grid <-  -1*seq(0.01, 0.2, 0.025)
cover_funds <- length(grid)*length(data_list)*bet
# stopifnot(cover_funds<funds)
names
i <- 1
unlist(lapply(data_list, nrow))
for (i in 1:length(data_list)){
  # for (i in 120:160){
  tmp <- copy(data_list[[i]])
  
  look_back <- data.table(bar=c(floor(nrow(tmp)/(24)),
                                floor(nrow(tmp)/(48)), floor(nrow(tmp)/(72))
                                , floor(nrow(tmp)/(168))),flag=1)
  # look_back <- data.table(bar=c(floor(nrow(tmp)/(24)) ),flag=1)
  TP <- data.table(tp=c(0.02,0.05,0.1, 0.15, 0.2, 0.3),flag=1)
  # TP <- data.table(tp=c(0.05),flag=1)
  median_number <- data.table(med_num=c(1,2,4,5,3),flag=1)
  # median_number <- data.table(med_num=c(4),flag=1)
  params <- left_join(look_back,TP)%>%left_join(median_number)
  params[bar == floor(nrow(tmp)/(48)), bar_day := "48 hours"]
  params[bar == floor(nrow(tmp)/(24)), bar_day := "24 hours"]
  params[bar == floor(nrow(tmp)/(48)), bar_day := "48 hours"]
  params[bar == floor(nrow(tmp)/(72)), bar_day := "72 hours"]
  params[bar == floor(nrow(tmp)/(168)), bar_day := "168 hours"]
  # params[bar == floor(nrow(tmp)/(10)), bar_day := "10 days"]
  # params[bar == floor(nrow(tmp)/(24)), bar_day := "24 days"]
  # params[bar == floor(nrow(tmp)/(5)), bar_day := "5 days"]
  params[, bar_day :=factor(bar_day, levels = c(unique(params$bar_day)))]
  # params <- params[bar_day =="3 days" & tp =="0.1" & med_num == 1]
  
  # 40 days_0.3_1
  
  # pair operation start
  results <- list()
  h <- 1
  for (h in 1:nrow(params)){
    
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
      
      long_grid[, `:=`(trade_id = replicate(nrow(long_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = "")),
                       batch=s, 
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
      
      results[[h]] <- param_result
    }
    
    
    
    
  }  
  # Pair operation end
  tmp_res <-  rbindlist(results)
  tmp_res[, pair := unique(tmp$pair)]
  print(tmp_res)
  pair_results[[i]] <- tmp_res
  fund_list_pair[[i]] <- fund_list_param
  
}
fund_list_pair <- lapply(fund_list_pair, as.data.frame)
fund_list_pair <- rbindlist(fund_list_pair)


data_f <- fund_list_pair[,  sum(funds_pair), by = interval_enter]
setorder(data_f, interval_enter)

ggplot(data_f, aes(x = interval_enter, y=V1))+
  geom_line()+
  geom_hline(yintercept = c(funds, -funds))
rm(fund_list_pair)
rm(data_f)
test<- rbindlist(pair_results, fill = T)
test[, param_concatenated := paste(bar_day, tp, med_num, sep="_"), by =.I]


metrics <- test[, list(sum_bet = sum(total_bet),
                       sum_quote = sum(quote_res),
                       mean_hodl = median(hodl)), by=.(param_concatenated)]
metrics[, percent:= sum_quote/funds]
metrics

metrics_pair <- test[, list(sum_bet = sum(total_bet),
                            sum_quote = sum(quote_res),
                            mean_hodl = median(hodl)), by=.(pair)]
metrics_pair[, percent:= sum_quote/funds]
metrics_pair[percent>mean_hodl, .N]


# 
# Function for ticket and asset info
# asset_info_ticker <- function(){
# 
# }
# 
# 
# url <- paste0("https://api.kraken.com/0/public/AssetPairs")
# tb <- jsonlite::fromJSON(url)
# tb$result$AAVEEUR
# 
# # Extract and combine into a dataframe
# df <- do.call(rbind, lapply(tb$result, function(x) {
#   data.frame(
#     altname = x$altname,
#     base = x$base,
#     quote = x$quote,
#     aclass_base = x$aclass_base,
#     ordermin = x$ordermin,
#     costmin = x$costmin,
#     stringsAsFactors = FALSE
#   )
# }))
# rownames(df) <- NULL
# 
# 
# url <- paste0("https://api.kraken.com/0/public/Ticker")
# tb <- jsonlite::fromJSON(url)
# price_info <- data.table(PAIR = names(tb$result),
#                          PRICE = as.numeric(lapply(lapply(tb$result, "[[", 3), "[", 1)))
# volume_info <- data.table(PAIR = names(tb$result),
#                           VOLUME_24hours = as.numeric(lapply(lapply(tb$result, "[[", 4), "[", 2)))
# 
# 
# df <- merge(df,price_info, by.x = "altname", by.y = "PAIR", all.x = T)
# df <- merge(df,volume_info, by.x = "altname", by.y = "PAIR", all.x = T)
# setDT(df)
# df[, USD_amount := PRICE* VOLUME_24hours]
# 
# View(df)
# 
# 





# Parameters
# set.seed(123)          # For reproducibility
# n <- 1000              # Number of time steps
# initial_price <- 20000 # Initial price in USD
# mu <- 0.0002           # Drift (expected return per step)
# sigma <- 0.02          # Volatility (standard deviation of returns)
# dt <- 1                # Time step (1 day or 1 minute, depending on data frequency)

# # Generate random returns using GBM
# time_steps <- 1:n
# epsilon <- rnorm(n, mean = 0, sd = 1)
# log_returns <- (mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * epsilon
# 
# # Simulate price
# prices <- initial_price * cumprod(exp(log_returns))
# 
# # Create a data frame
# crypto_data <- data.frame(
#   Time = seq.POSIXt(from = Sys.time(), by = "min", length.out = n), # Adjust frequency as needed
#   Price = prices
# )
# 
# # Plot the simulated data
# library(ggplot2)
# ggplot(crypto_data, aes(x = Time, y = Price)) +
#   geom_line(color = "blue") +
#   labs(title = "Simulated Cryptocurrency Prices", x = "Time", y = "Price") +
#   theme_minimal()
# 
# # View the simulated data
# head(crypto_data)
# 
# 
# # Introduce a price spike event
# crypto_data$Price[500] <- crypto_data$Price[499] * 1.5  # Example: 50% jump at step 500
# 
# # Re-plot with the event
# ggplot(crypto_data, aes(x = Time, y = Price)) +
#   geom_line(color = "blue") +
#   labs(title = "Simulated Cryptocurrency Prices with Event", x = "Time", y = "Price") +
#   theme_minimal()
# 
# 
# crypto_data$Price <- crypto_data$Price + rnorm(n, mean = 0, sd = 10) # Add random noise
# 
# 
# 

# 
library(tidyverse)
library(lubridate)
n <- 1000
init_price <- 20000
mu <- -0.0002
sigma <- 0.02
dt <- 1

geom_brown <- function(n, init_price, mu, sigma, dt){
  time_steps <- 1:n
  epsilon <- rnorm(n, mean = 0, sd = 1)
  log_returns <- (mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * epsilon
  prices <- init_price * cumprod(exp(log_returns))
  crypto_data <- data.frame(
    Time = seq.POSIXt(from = Sys.time(), by = "min", length.out = n),
    Price = prices
  )
  ohlc_data <- crypto_data %>%
    mutate(TimeInterval = floor_date(Time, unit = "60 minutes")) %>%
    group_by(TimeInterval) %>%
    summarise(
      Open = first(Price),
      High = max(Price),
      Low = min(Price),
      Close = last(Price)
    ) %>%
    ungroup()
  setDT(ohlc_data)
  setnames(ohlc_data, c("interval","open","high", "low", "close"))
  ohlc_data[, pair := paste(sample(all_chars, str_len, replace = TRUE), collapse = "")]
  return(ohlc_data)
}

candles(geom_brown(n=100000, init_price = 1000, mu = 0.000001, sigma = 0.001, dt=1))
data_list <- lapply(1:100, function(x)geom_brown(n=100000, init_price = 1000, mu = 0.000001, sigma = 0.001, dt=1))

# 
# # Aggregating to OHLC (e.g., every 10 minutes)
# 
# 
# # View the OHLC data
# print(head(ohlc_data))



# library(ggplot2)
# 
# ggplot(ohlc_data, aes(x = TimeInterval)) +
#   geom_segment(aes(y = Low, yend = High, xend = TimeInterval), color = "black") + # High-Low line
#   geom_rect(aes(
#     ymin = pmin(Open, Close), ymax = pmax(Open, Close),
#     xmin = TimeInterval - minutes(2), xmax = TimeInterval + minutes(2)
#   ), fill = ifelse(ohlc_data$Close > ohlc_data$Open, "green", "red")) + # Candle body
#   labs(title = "Simulated OHLC Data", x = "Time", y = "Price") +
#   theme_minimal()
# 

# hist(metrics_pair$percent, breaks=50)
# hist(metrics_pair$mean_hodl, breaks=50)
# 
# library(viridis)
# p <- unique(test$pair)
# pairs <- 1
# 
# pdf("output_plots.pdf", width = 8, height = 6)
# for(pairs in 1:length(p)){
#   
#   tmp <- test[pair==p[pairs] ]
#   
#   
#   
#   plot_out1 <- ggplot(data=tmp, aes(x = tp, y = bar_day, fill = total_percent)) +
#     geom_tile(colour="black")+
#     # geom_tile(data = annot, aes(x = tp, y = sl), 
#     #           fill = "red", alpha = 0.7, inherit.aes = FALSE)+
#     facet_grid(~med_num)+
#     # scale_fill_brewer(palette = "RdYlBu")+
#     # scale_fill_viridis_d(option = "A")+
#     scale_fill_viridis(option="magma")+
#     ggtitle(paste0(p[pairs], " ", "percent") ) +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
#   print(plot_out1)
#   
#   plot_out2 <- ggplot(data=tmp, aes(x = tp, y = bar_day, fill = quote_res)) +
#     geom_tile(colour="black")+
#     # geom_tile(data = annot, aes(x = tp, y = sl), 
#     #           fill = "red", alpha = 0.7, inherit.aes = FALSE)+
#     facet_grid(~med_num)+
#     # scale_fill_brewer(palette = "RdYlBu")+
#     # scale_fill_viridis_d(option = "A")+
#     scale_fill_viridis(option="magma")+
#     ggtitle(paste0(p[pairs], " ", "quote_res") ) +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
#   print(plot_out2)
#   
# }
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # f_best <- rbindlist(results)
# # f_best <- f_best[total_percent == max(total_percent)]
# # params <- data.table(bar=f_best$bar, tp = f_best$tp, med_num = f_best$med_num)
# 
# 
# 
candles(tmp)+
  geom_point(data=final_grid[!is.na(interval_enter) & position == "long"], aes(x=interval_enter, y=grid), fill="lightblue3",colour="black", shape =24, size=2)+
  geom_point(data=final_grid[!is.na(interval_exit_tp) & position == "long" ], aes(x=interval_exit_tp, y=price_exits), fill="darkorchid1", colour="black",shape =25, size=2)
#   geom_point(data=final_grid[!is.na(interval_enter) & position == "short"], aes(x=interval_enter, y=grid), fill="darkorchid1", colour="black", shape =25, size=2)+
#   geom_point(data=final_grid[!is.na(interval_exited) & position == "short"], aes(x=interval_exited, y=exit_price), fill="darkorchid1", colour="black",shape =24, size=2)
# # param_result
# 
# 
# # })
# # htmlwidgets::saveWidget(p, "profile.html")
# # Save
# save(daily_res, file=paste0("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Grid/results/", paste0(ticks,"_", units,"_", pair, Sys.time()), ".Rdata"))
# 

