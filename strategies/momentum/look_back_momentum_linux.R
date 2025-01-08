rm(list=ls())
gc()
# Source functions
strat_name <- "break_out_v02_daily_candles"
library(runner)
library(Rfast)
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
all_pairs <- list.files("Data")
for (z in 7:length(all_pairs)){
  pair <- all_pairs[z]
  # pair <- "XXRPZUSD"
  # Path to save results
  data_path <- "Data"
  funds <- 1000
  # Fix path
  pair_data_results <- paste(data_path, pair, sep ="/")
  
  # Load data
  ticks <- c(24)
  units <- c("hours")
  intervals <- paste(ticks, units, sep = " ") 
  
  df <- trades_to_OHLC(pair = pair,
                       interval = intervals,
                       from_date = "2023-03-27",
                       to_date = "2023-03-30",
                       date_subset = F)
  df1 <- df[[1]]
  df1[, date:=as.Date(interval)]
  gc()
  
  
  # Set parameters table
  look_back <- data.table(bar=seq(30, 90, 10),flag=1)
  SL <- data.table(sl=seq(0.1, 0.3, 0.01),flag=1)
  TP <- data.table(tp=seq(0.1, 0.2, 0.01),flag=1)
  median_number <- data.table(med_num=seq(1, 5, 1),flag=1)
  last_exclude_number <- data.table(exc_num=seq(1, 5, 1),flag=1)
  params <- left_join(look_back,SL)%>%left_join(TP)%>%left_join(median_number)%>%left_join(last_exclude_number)
  
  
  # For trade Ids
  all_chars <- c(LETTERS, 0:9)
  str_len <- 20
  
  # max_fun <- function(x){
  #   res <- median(sort(x[-((length(x) - 10):length(x))],decreasing = T)[1:10])
  #   return(res)
  # }
  # 
  # min_fun <- function(x){
  #   res <- median(sort(x[-((length(x) - 10):length(x))],decreasing = F)[1:10])
  #   return(res)
  # }
  
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
    
    max_fun <- function(x) {
      # Get parameters
      exc_num <- params$exc_num[h]
      med_num <- params$med_num[h]
      
      # Exclude the largest `exc_num` elements
      len <- length(x)
      subset <- x[-((len - exc_num + 1):len)]
      
      # Use Rfast::nth to find the top `med_num` largest values
      top_values <- Rfast::nth(subset, k = med_num, descending = TRUE)
      
      # Compute the median of these top values
      res <- median(top_values)
      return(res)
    }
    
    
    
    
    min_fun <- function(x) {
      # Get parameters
      exc_num <- params$exc_num[h]
      med_num <- params$med_num[h]
      
      # Exclude the largest `exc_num` elements
      len <- length(x)
      subset <- x[-((len - exc_num + 1):len)]
      
      # Use Rfast::nth to find the smallest `med_num` values
      smallest_values <- Rfast::nth(subset, k = med_num, descending = FALSE)
      
      # Compute the median of these smallest values
      res <- median(smallest_values)
      return(res)
    }
    
    
    
    
    tmp <- copy(df1)
    resist <- runner(x = tmp$close, k = params$bar[h], f = max_fun, na_pad = T)
    supp <- runner(x = tmp$close, k = params$bar[h], f = min_fun, na_pad = T)
    tmp[, resistance := resist]
    tmp[, support := supp]
    tmp[close>resistance, signal :="long"]
    tmp[close<support, signal :="short"]
    
    bar <- 1
    ind_enter <- which(!is.na(tmp$signal))[1]
    while(bar <= nrow(tmp)){
      ind_enter <- which(!is.na(tmp$signal[bar:nrow(tmp)]))[1]+bar-1
      if(is.na(ind_enter)) break
      
      if(tmp[ind_enter, signal] == "long"){
        if("diff" %in% colnames(tmp)){
          tmp[, diff:=NULL]
        }
        tmp[ind_enter, position := "enter_long"]
        rand <- paste(sample(all_chars, str_len, replace = TRUE), collapse = "")
        tmp[ind_enter, trade_id:=rand]
        price_entered <- tmp[ind_enter:.N, ][position == "enter_long", close]
        tmp[ind_enter:nrow(tmp), diff := c(0, cumsum(diff(tmp[ind_enter:nrow(tmp), close])))/price_entered]
        ind_exit <- which(tmp$diff>params$tp[h]|tmp$diff<params$sl[h]*(-1))[1]
        
        if(is.na(ind_exit)){
          ind_exit <- nrow(tmp)
        }
        
        tmp[ind_exit, position := "exit_long"]
        tmp[ind_exit, trade_id:=rand]
        bar <- ind_exit+1
      }
      
      if(tmp[ind_enter, signal] == "short"){
        if("diff" %in% colnames(tmp)){
          tmp[, diff:=NULL]
        }
        tmp[ind_enter, position := "enter_short"]
        rand <- paste(sample(all_chars, str_len, replace = TRUE), collapse = "")
        tmp[ind_enter, trade_id:=rand]
        price_entered <- tmp[ind_enter:.N, ][position == "enter_short", close]
        tmp[ind_enter:nrow(tmp), diff := (-1)*c(0, cumsum(diff(tmp[ind_enter:nrow(tmp), close])))/price_entered]
        ind_exit <- which(tmp$diff> params$tp[h]|tmp$diff<params$sl[h]*(-1))[1]
        
        if(is.na(ind_exit)){
          ind_exit <- nrow(tmp)
        }
        tmp[ind_exit, position := "exit_short"]
        tmp[ind_exit, trade_id:=rand]
        bar <- ind_exit+1
      }
      
      
      # print(bar)
      
    }
    a <- tmp[!is.na(position), (close[position =="exit_long"]-close[position =="enter_long"])/close[position =="enter_long"], by=trade_id]
    b <- tmp[!is.na(position), -1*(close[position =="exit_short"]-close[position =="enter_short"])/close[position =="enter_short"], by=trade_id]
    c <- rbind(a, b)
    d <- tmp[!is.na(position)]
    hours_in_long <- d[, list(hours=difftime(interval[position =="exit_long"], interval[position =="enter_long"], units="hours")), by =trade_id]
    hours_in_short <- d[, list(hours=difftime(interval[position =="exit_short"], interval[position =="enter_short"], units="hours")), by =trade_id]
    hours <- rbind(hours_in_long, hours_in_short)
    g <- d[, list(date=max(interval)), by =trade_id]
    k <- merge(c, g)
    setorder(k, date)
    k <- merge(k, hours)
    k[, trade_fees:= 0.008]
    k[, rollover_fees:= 0.0002* (as.numeric(hours)/4)]
    k[, total_fees:= trade_fees+rollover_fees]
    k[, V2 := V1 -total_fees]
    res_tmp <- params[h, ]
    res_tmp[, `:=` (n_trades = nrow(k),
                    win_rate = sum(k$V2>0)/nrow(k),
                    aver_trade = mean(k$V2),
                    sum_per = sum(k$V2),
                    init_funds =funds,
                    final_funds = funds+cumsum(funds*k$V2)[length(k$V2)])]
    
    results[[h]] <- res_tmp
    print(h)
    print(res_tmp)
    
  }
  results_exp <-rbindlist(results)
  results_exp[, pair:=pair]
  results_exp[, hodl := (tail(tmp[, close], 1)-head(tmp[, close], 1))/head(tmp[, close], 1)]
  # })
  # htmlwidgets::saveWidget(p, "profile.html")
  # Save
  save(results_exp, file = paste0(getwd(),"/results/break_out_daily/", paste0(ticks,"_", units,"_", pair,"_", strat_name), ".Rdata"))
  
  }




# candles(tmp)+
#   geom_point(data=final_grid[!is.na(interval_enter) & position == "long"], aes(x=interval_enter, y=grid), fill="lightblue3",colour="black", shape =24, size=2)+
#   geom_point(data=final_grid[!is.na(interval_exited) & position == "long" ], aes(x=interval_exited, y=exit_price), fill="lightblue3", colour="black",shape =25, size=2)+
#   geom_point(data=final_grid[!is.na(interval_enter) & position == "short"], aes(x=interval_enter, y=grid), fill="darkorchid1", colour="black", shape =25, size=2)+
#   geom_point(data=final_grid[!is.na(interval_exited) & position == "short"], aes(x=interval_exited, y=exit_price), fill="darkorchid1", colour="black",shape =24, size=2)
# param_result


# })
# htmlwidgets::saveWidget(p, "profile.html")
# Save
# save(daily_res, file=paste0("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Grid/results/", paste0(ticks,"_", units,"_", pair, Sys.time()), ".Rdata"))
