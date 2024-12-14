rm(list=ls())
gc()
# Source functions
library(profvis)
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Path to save results
data_path <- "Data"
data <- list.files(data_path, full.names = T)
names <- list.files(data_path, full.names = F)
i <- 1
data_list <- list()
for (i in 1:length(data)){
  pair <- names[i]
  pair_data_results <- paste(data_path, pair, sep ="/")
  ticks <- c(24)
  units <- c("hours")
  intervals <- paste(ticks, units, sep = " ")
  df <- trades_to_OHLC(pair = pair,
                       interval = intervals,
                       from_date = "2023-03-27",
                       to_date = "2023-03-30",
                       date_subset = F)
  tmp <- df[[1]]
  tmp[, date:=as.Date(interval)]
  tmp[, pair := pair]
  data_list[[i]] <- tmp
  print(i)
  
}
gc()

# hourly
# look_back <- data.table(bar=c(floor(nrow(tmp)/(7*24*1)), floor(nrow(tmp)/(7*24*2)), floor(nrow(tmp)/(3*24))),flag=1)
# TP <- data.table(tp=seq(0.01, 0.2, 0.01),flag=1)
# median_number <- data.table(med_num=seq(1, 10, 1),flag=1)
# grid <-  -1*seq(0.05, 0.3, 0.01)
# 
# look_back <- data.table(bar=c(floor(nrow(tmp)/(7*24*1))),flag=1)
# TP <- data.table(tp=c(0.1),flag=1)
# median_number <- data.table(med_num=c(2),flag=1)
# grid <-  -1*seq(0.05, 0.3, 0.01)

# look_back <- data.table(bar=c(floor(nrow(tmp)/(3)), floor(nrow(tmp)/(7)), floor(nrow(tmp)/(14)), floor(nrow(tmp)/(30))),flag=1)
# TP <- data.table(tp=c(0.01, 0.02, 0.03),flag=1)
# median_number <- data.table(med_num=c(1,2),flag=1)
grid <-  -1*seq(0.05, 0.3, 0.01)

# For trade Ids
all_chars <- c(LETTERS, 0:9)
str_len <- 20

pair_results <- list()
# Loopm through all pairs
i <- 1
for (i in 1:length(data_list)){
  tmp <- copy(data_list[[i]])
  
  look_back <- data.table(bar=c(floor(nrow(tmp)/(10)), floor(nrow(tmp)/(14)), floor(nrow(tmp)/(30)) ),flag=1)
  # look_back <- data.table(bar=c(floor(nrow(tmp)/(10)) ),flag=1)
  TP <- data.table(tp=c(0.01, 0.02, 0.03, 0.04, 0.05),flag=1)
  median_number <- data.table(med_num=c(1,2,3, 4,5),flag=1)
  params <- left_join(look_back,TP)%>%left_join(median_number)
  params[bar == floor(nrow(tmp)/(3)), bar_day := "3 days"]
  params[bar == floor(nrow(tmp)/(7)), bar_day := "7 days"]
  params[bar == floor(nrow(tmp)/(14)), bar_day := "14 days"]
  params[bar == floor(nrow(tmp)/(30)), bar_day := "30 days"]
  params[bar == floor(nrow(tmp)/(10)), bar_day := "10 days"]
  
  params[, bar_day :=factor(bar_day, levels = c(unique(params$bar_day)))]
  
  # pair operation start
  results <- list()
  h <- 1
  for (h in 1:nrow(params)){
    
    list_df <- split_dataframe(tmp, params$bar[h])
    offset <- unlist(lapply(list_df, nrow))
    offset[1] <- 1
    offset <- cumsum(offset)
    names(offset)<- NULL
    # closed_grid <- list()
    # open_grid <- list()
    batch <- list()
    s <- 2
    for (s in 2:length(list_df)){
      
      df1_previous <- list_df[[(s-1)]]
      
      # Low or close?
      support <- median(sort(df1_previous$close)[1:params$med_num[h]])
      entries_limit <- c(support, support+support*grid)
      # candles(df1_previous)+geom_hline(yintercept = c(support, support+support*grid))
      
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
                       bet = 5
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
    
    
  }  
  # Pair operation end
  tmp_res <-  rbindlist(results)
  tmp_res[, pair := unique(tmp$pair)]
  print(tmp_res)
  pair_results[[i]] <- tmp_res
}


test <- rbindlist(pair_results)
test[, param_concatenated := paste(bar_day, tp, med_num, sep="_"), by =.I]


metrics <- test[, list(sum_bet = sum(total_bet),
                       sum_quote = sum(quote_res)), by=.(param_concatenated)]
metrics[, percent:= sum_quote/sum_bet]
View(metrics)
library(viridis)
p <- unique(test$pair)
pairs <- 1

pdf("output_plots.pdf", width = 8, height = 6)
for(pairs in 1:length(p)){
  
  tmp <- test[pair==p[pairs] ]
  
  
  
  plot_out1 <- ggplot(data=tmp, aes(x = tp, y = bar_day, fill = total_percent)) +
    geom_tile(colour="black")+
    # geom_tile(data = annot, aes(x = tp, y = sl), 
    #           fill = "red", alpha = 0.7, inherit.aes = FALSE)+
    facet_grid(~med_num)+
    # scale_fill_brewer(palette = "RdYlBu")+
    # scale_fill_viridis_d(option = "A")+
    scale_fill_viridis(option="magma")+
    ggtitle(paste0(p[pairs], " ", "percent") ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
  print(plot_out1)
  
  plot_out2 <- ggplot(data=tmp, aes(x = tp, y = bar_day, fill = quote_res)) +
    geom_tile(colour="black")+
    # geom_tile(data = annot, aes(x = tp, y = sl), 
    #           fill = "red", alpha = 0.7, inherit.aes = FALSE)+
    facet_grid(~med_num)+
    # scale_fill_brewer(palette = "RdYlBu")+
    # scale_fill_viridis_d(option = "A")+
    scale_fill_viridis(option="magma")+
    ggtitle(paste0(p[pairs], " ", "quote_res") ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
  print(plot_out2)
  
}
dev.off()












# f_best <- rbindlist(results)
# f_best <- f_best[total_percent == max(total_percent)]
# params <- data.table(bar=f_best$bar, tp = f_best$tp, med_num = f_best$med_num)



candles(tmp)+
  geom_point(data=final_grid[!is.na(interval_enter) & position == "long"], aes(x=interval_enter, y=grid), fill="lightblue3",colour="black", shape =24, size=2)+
  geom_point(data=final_grid[!is.na(interval_exit_tp) & position == "long" ], aes(x=interval_exit_tp, y=price_exits), fill="darkorchid1", colour="black",shape =25, size=2)
#   geom_point(data=final_grid[!is.na(interval_enter) & position == "short"], aes(x=interval_enter, y=grid), fill="darkorchid1", colour="black", shape =25, size=2)+
#   geom_point(data=final_grid[!is.na(interval_exited) & position == "short"], aes(x=interval_exited, y=exit_price), fill="darkorchid1", colour="black",shape =24, size=2)
# param_result


# })
# htmlwidgets::saveWidget(p, "profile.html")
# Save
save(daily_res, file=paste0("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Grid/results/", paste0(ticks,"_", units,"_", pair, Sys.time()), ".Rdata"))

