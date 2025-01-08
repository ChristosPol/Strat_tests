# Packages --------------------------------------------------------------------
# install_github("daroczig/binancer")
# https://github.com/daroczig/binancer/
suppressMessages(library(xts))
# suppressMessages(library(Rbitcoin))
suppressMessages(library(httr))
suppressMessages(library(anytime))
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
suppressMessages(library(ggplot2))
suppressMessages(library(TTR))
suppressMessages(library(openssl))
suppressMessages(library(digest))
suppressMessages(library(zoo))
suppressMessages(library(foreach))
suppressMessages(library(doParallel))
suppressMessages(library(bit64))
suppressMessages(library(nanotime))
#suppressMessages(library(gganimate))
#suppressMessages(library(gapminder))
#suppressMessages(library(gifski))
suppressMessages(library(gridExtra))
suppressMessages(library(R.utils))
#suppressMessages(library(plotly))
#suppressMessages(library(Metrics))
#suppressMessages(library(plm))
#suppressMessages(library(randomForest))
suppressMessages(library(stringr))
#suppressMessages(library(grid))
suppressMessages(library(readr))
#suppressMessages(library(rjson))
#suppressMessages(library(ggpubr))
suppressMessages(library(ggdark))
suppressMessages(library(tibble))
suppressMessages(library(tidyr))
#suppressMessages(library(rjson))

# Options
setDTthreads(1)
options(stringsAsFactors = FALSE)

# API info
#api_info <- read.table(paste0("/Users/christospolysopoulos/Repositories/Private/", "API_Keys.txt"),
#                       sep = ";", header = T)
#API_Key <- as.character(api_info$API_Key)
#API_Sign <- as.character(api_info$API_Sign)


# Functions --------------------------------------------------------------------

# Plot candlestick data --------------------------------------------------------
plot_candlesticks <- function(dta, Ns, asset){

  dta <- tail(dta, Ns)
  mn <- min(dta$low)
  mx <- max(dta$high)

  xs <- c(1:nrow(dta))
  color_list <- ifelse(dta$close >= dta$open, "green", "red")

  plot(dta$high, main = asset, xaxt = "n", xlab = "", ylab = "price", ylim = c(mn, mx), type = "n")
  par(new = T)
  plot(dta$low, main = "", axes = F, xlab = "", ylab = "", ylim = c(mn, mx), type = "n")
  segments(x0 = xs, y0 = dta$open, x1 = xs, y1 = dta$close, col = color_list, lwd = 4)
  segments(x0 = xs, y0= dta$low, x1 = xs, y1 = dta$high, col = color_list, lwd = 1)
  axis(1, at = 1:length(dta$interval), las = 2)
}

# # Plot chart with SR lines and return values
SR_lines <- function(data, roll, n_sort, pair, Ns, plot.it = FALSE){

  last_close <- data$high[nrow(data)]

  last_prices <- tail(data$high[-nrow(data)], roll)
  last_volumes <- tail(data$volume[-nrow(data)], roll)
  mydf <- data.frame(last_prices, last_volumes)
  mydf <- arrange(mydf, mydf$last_prices)

  sup_df <- head(mydf, n_sort)
  sup_w_mean <- sum(sup_df$last_prices *sup_df$last_volumes)/sum(sup_df$last_volumes)

  rs_df <- tail(mydf, n_sort)
  rs_w_mean <- sum(rs_df$last_prices *rs_df$last_volumes)/sum(rs_df$last_volumes)
  if(plot.it == TRUE){


    plot_candlesticks(dta = data, Ns = Ns, asset = pair)
    abline(h = rs_w_mean, col = "black", lty = "dashed")
    abline(h = sup_w_mean, col = "black", lty = "dashed")
  }
  return(list(SL = sup_w_mean, RL = rs_w_mean))

}

# Plots boolinger bands
bollinger_bands <- function(periods, times_sd, data){
  df <- data
  plot(df$close[-c(1:(periods-1))], type ="l", lwd =2)
  lines(SMA(df$close, n=periods)[-c(1:(periods-1))] + times_sd*rollapply(df$close, periods, sd), col ="red")
  lines(SMA(df$close, n=periods)[-c(1:(periods-1))] - times_sd*rollapply(df$close, periods, sd), col ="green")
}

# how many trades were succesful (to be functioned)
win_ratio <- function(dataset){
  df <- dataset
  mah <- subset(df, df$action %in% c("buy","sell") )
  profitable_trades <- list()
  ids_s <- unique(mah$id)
  for(i in 1:length(unique(mah$id))){

    profitable_trades[[i]] <- mah$close[mah$id == ids_s[i] & mah$action =="sell"]-mah$close[mah$id == ids_s[i] & mah$action =="buy"]
  }
  res <- table(unlist(profitable_trades) > 0)[names(table(unlist(profitable_trades) > 0)) ==T]/sum(table(unlist(profitable_trades) > 0))
  if(length(res)==0){
    res <- 0
  }

  return(res)
}

candles <- function(data,date_interval = "2 week"){
  # data[, date := as.Date(date)]
  data[close>open, colour := "green"]
  data[close<open, colour := "red"]
  # data[close==open, colour := "black"]
  data[, colour:= as.factor(data$colour)]
  
  p <- ggplot(data = data)+
    geom_segment(aes(x =interval,
                     xend=interval,
                     y =open,
                     yend =close,
                     colour= colour),
                 size=1.5)+
    geom_segment(aes(x = interval,
                     xend=interval,
                     y =high,
                     yend =low,colour= colour))+
    scale_color_manual(values=c("Forest Green","Red"))+
    theme_bw()+
    theme(legend.position ="none",
          axis.title.y = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.title= element_text(hjust=0.5))
  # +
  # scale_x_datetime(labels = date_format("%H:%M:%S"))
  # +
  # scale_x_date(date_labels="%d-%m-%Y", date_breaks  =date_interval)
  print(head(data))
  return(p)
}

# Convert historical dates to candlesticks
trades_to_OHLC <- function(pair, interval, from_date, to_date, date_subset) {
  # Read it
  file <- paste0(paste(pair_data_results, pair, sep = "/"), ".csv.gz")
  frame <- fread(file)
  
  colnames(frame) <- c("price", "volume", "epoch_time", "buy_sell", "market_limit","misc",
                       "trade_id", "last_id", "Date_POSIXct", "Time", "Date", "Hour")

  print("File loaded..")
  # Subset the time period
  if(date_subset) {
    frame <- subset(frame, frame$Date >= from_date & frame$Date <= to_date)
  }

  candles <- list()
  for (i in 1:length(intervals)){
    # Select interval
    copied <- copy(frame)
    copied[, interval := floor_date(as.POSIXct(Date_POSIXct), unit = intervals[i])]

    candles[[i]] <- copied[, .(high = max(price), low = min(price), open = first(price),
                               close = last(price), volume = sum(volume)),
                           by = .(interval)]
    setorder(candles[[i]], interval)
    
    print(paste0("Reduced to ", intervals[i], " intervals.." ))
  }
  return(candles)
}





calculate_profits_LS <- function(dataset, params){

  calcu <- dataset[action %in% c("enter_long", "enter_short", "exit_long", "exit_short"), ]
  calcu <- subset(calcu,  !calcu$id %in% names(which(table(calcu$id) ==1)))
  if (nrow(calcu) > 0) {

    profit_long <- c()
    profit_short <- c()
    profit_sum <- c()
    ids_long <- unique(calcu$id[calcu$action %in% c("enter_long", "exit_long")])
    ids_short <- unique(calcu$id[calcu$action %in% c("enter_short", "exit_short")])
    for(i in 1:length(ids_long)){

      profit_long[i] <- calcu$equity[calcu$action =="exit_long" & calcu$id == ids_long[i]] - calcu$Price[calcu$action =="enter_long" & calcu$id == ids_long[i]]
    }

    for(i in 1:length(ids_short)){

      profit_short[i] <- calcu$equity[calcu$action =="exit_short" & calcu$id == ids_short[i]] - calcu$Price[calcu$action =="enter_short" & calcu$id == ids_short[i]]
    }

    profit_sum_long <- sum(profit_long)
    profit_sum_short <- sum(profit_short)
    profit_sum <- profit_sum_long +  profit_sum_short
    dd <- data.frame(profit_sum_long = profit_sum_long, profit_sum_short = profit_sum_short, profit = profit_sum, n_trades = length(unique(calcu$id)),
                     enter_date = unique(calcu$Date)[1], exit_date = tail(unique(calcu$Date), 1))
  } else {

    dd <- data.frame(profit = 0,
                     n_trades = 0,
                     enter_date = as.Date("2020-04-07"),
                     exit_date =as.Date("2020-04-07"))
  }

  if(paraller_exec ==TRUE){
    dd$params <- params
  }

  write.table(dd, "/media/chris/DATA/Documents/Bot_Trading/Historical_data/myDF.csv",
              sep = ",", row.names = FALSE, col.names = !file.exists("/media/chris/DATA/Documents/Bot_Trading/Historical_data/myDF.csv"), append = T)
  return(dd)
}








###
# Calculate profits and number of trades ---------------------------------------

calculate_profits <- function(dataset, params){

  calcu <- dataset[action %in% c("buy", "sell"), ]
  calcu <- subset(calcu,  !calcu$id %in% names(which(table(calcu$id) ==1)))
  if (nrow(calcu) > 0) {

  profit <- c()

  ids <- unique(calcu$id)
  for(i in 1:length(ids)){

    profit[i] <-   calcu$Price[calcu$action =="sell" & calcu$id == ids[i]] - calcu$Price[calcu$action =="buy" & calcu$id == ids[i]]

  }

  profit1 <- tail(calcu$Price, 1)
  dd <- data.frame(profit = profit1-initial_budget,
                   n_trades = length(unique(calcu$id)),
                   biggest_lost =min(profit[profit < 0]),
                   biggest_win = max(profit[profit > 0 ]),
                   avg_loss = mean(profit[profit < 0]),
                   avg_win = mean(profit[profit > 0]),
                   winratio = win_ratio(myresult),
                   enter_date = unique(calcu$full_date_time)[1],
                   exit_date = tail(unique(calcu$full_date_time), 1))
  } else {

    dd <- data.frame(profit = 0,
                     n_trades = 0,
                     biggest_lost = 0,
                     biggest_win = 0,
                     avg_loss = 0,
                     avg_win = 0,
                     winratio = 0,
                     enter_date = as.Date("2020-04-07"),
                     exit_date =as.Date("2020-04-07"))
  }

  if(paraller_exec ==TRUE){
    dd$params <- params
  }

  write.table(dd, "/media/chris/DATA/Documents/Bot_Trading/Historical_data/myDF.csv",
              sep = ",", row.names = FALSE, col.names = !file.exists("/media/chris/DATA/Documents/Bot_Trading/Historical_data/myDF.csv"), append = T)
  return(dd)
}


# Download OLHC data and calcualte indicators. Write last line and append each time
#-------------------------------------------------------------------------------

OHLC_action <- function(pair, interval){
  repeat{
    # 1. Get the OHLC - Repeat this call every x interval
    what <- tryCatch(
      {
        url <- paste0('https://api.kraken.com/0/public/OHLC?pair=',pair,'&interval=', interval)
        dat <- jsonlite::fromJSON(url)
      },
      error = function(e){})

    if(is.null(dat$result[1])) next # error, skip
    if(nrow(as.data.frame(dat$result[1])) == 0) break # last batch empty

    df <- as.data.table(dat$result$XETHZEUR)
    colnames(df) <- c("time", "open", "high", "low", "close",
                      "vwap", "volume", "count")
    df[, Date_POSIXct := anytime(as.numeric(as.character(time)))]

    # as numeric
    df$open <- as.numeric(df$open)
    df$high <- as.numeric(df$high)
    df$low <- as.numeric(df$low)
    df$close <- as.numeric(df$close)

    # 2. Add Indicators
    df[, c(paste0("EMA", "_", fast_EMA), paste0("EMA", "_", slow_EMA), "crossover", "action", "Units", "Price", "id") := list(NA, NA,0, NA, NA, NA, NA) ]
    df[, c(paste0("EMA", "_", fast_EMA), paste0("EMA", "_", slow_EMA)) := list( EMA(close, n = fast_EMA), EMA(close, n = slow_EMA) ) ]
    df$crossover[get(paste0("EMA", "_", fast_EMA), df) > get(paste0("EMA", "_", slow_EMA), df)] <- 1

    print(tail(df, 1))
    Sys.sleep(interval*60)
  }
}

roundup <- function(x, towhere){
  res <- ceiling(x/towhere)*towhere
  return(res)
}


support <- function(x, n_sort, n_exclude){
  x <- x[-((length(x)-n_exclude) : length(x))]
  SP <- mean(head(sort(x), n_sort))
  return(SP)
}

resistance <- function(x, n_sort, n_exclude){
  x <- x[-((length(x)-n_exclude) : length(x))]
  RS <- mean(head(sort(x, decreasing = T), n_sort))
  return(RS)
}

mean_previous <- function(x, n_exclude){
  x <- x[-((length(x)-n_exclude) : length(x))]
  RS <- mean(head(sort(x, decreasing = T)))
  return(RS)
}



select_period <- function(unix_time, diff_time) {
  
  if (unix_time == "start_of_time") {
    initial_id <- 0
  } else if (unix_time == "manually") {
    # select number of days starting from todays date
    options("width" = 90)
    v <- nanotime(Sys.time() - as.difftime(diff_time, unit = "days"))
    initial_id <- as.integer64(v)
  } else {
    file <- paste0(pair_data_results, "/", pair, ".csv.gz")
    nL <- countLines(file)
    dt <- fread(file, skip = nL-1)
    initial_id <- dt$V6  
  }
  initial_id
}


trades_evaluation_withTPSL <- function(){
  
  df[, groups:= as.character(rep(1:length(rle(df$signal)$lengths),
                                 rle(df$signal)$lengths))]
  
  df[is.na(signal), groups := NA]
  
  df[, index :=  1:nrow(df)]
  
  entries <- df[, head(index, 1), by=groups][!is.na(groups)]
  
  df[entries$V1, entry := "enter"]
  
  df[, position := character()]
  idx_signals <- which(df$entry == "enter")
  price_signals <- as.vector(na.omit(df$close[(df$entry == "enter")]))
  
  if(length(idx_signals) >0 ){
    if(length(price_signals) == 1){
      index <-1
    } else
      index <- (length(idx_signals)-1)
    
    # for (i in 1:index){
    i <-1
    while (i <= length(idx_signals)){
      
      # print(idx_signals)
      # print(idx_signals[i])
      if(is.na(df$position[idx_signals[i]])  & df$signal[idx_signals[i]] == "long"){
        returns_1 <- round((df$close[idx_signals[i]:nrow(df)] - price_signals[i]) / price_signals[i], 3)*100
        
        ###### Experimental
        # trailing stoploss
        
        # test <- data.table(cl_tr = df$close[idx_signals[i]:nrow(df)], status= 0)
        # test[1, status := cl_tr - cl_tr*0.02]
        # l<-2
        # for(l in 2:nrow(test)){
        #   
        # 
        #   # test[l, status := (cl_tr-V2)/V2 ]
        #   if(test[l, cl_tr] > test[l-1, cl_tr]){
        #     test[l, status := cl_tr - cl_tr*0.02]
        #     
        #     if(test[l, status] < test[l-1, status]){
        #       test[l, status := test$status[l-1]]
        #     }
        #     
        #     
        # } else if (test[l, cl_tr] <= test[l-1, cl_tr]){
        #     test[l, status := test$status[l-1]]
        # }
        # }
        # 
        # exit_1 <- head(which(test$status >test$cl_tr),1)
        
        ###### Experimental
        
        returns_1 <-returns_1[-1]
        exit_1 <- head(which(returns_1 > tp | returns_1 < sl), 1)
        if(length(exit_1) == 0){
          exit_1 <- nrow(df)-idx_signals[i]
        }
        
        df[idx_signals[i]: (exit_1+idx_signals[i]), position := "long"]
        df[idx_signals[i], status := "entered_trade"]
        df[(exit_1+idx_signals[i]), status := "exited_trade"]
        
        if(is.na(df[exit_1+idx_signals[i] + 1, position]) & !is.na(df[exit_1+idx_signals[i] + 1, signal]) ){
          
          if(df[exit_1+idx_signals[i] + 1, signal] %in% c("long", "short")){
            idx_signals <- sort(append(idx_signals, values= exit_1+idx_signals[i] + 1))
            price_signals <- df[idx_signals, close]  
          }
          
        }
        
        
        # print("Position entered")
      } else if(is.na(df$position[idx_signals[i]])  & df$signal[idx_signals[i]] == "short") {
        returns_1 <- -round((df$close[idx_signals[i]:nrow(df)] - price_signals[i]) / price_signals[i], 3)*100
        returns_1 <-returns_1[-1]
        exit_1 <- head(which(returns_1 > tp | returns_1 <sl),1)
        
        if(length(exit_1) == 0){
          exit_1 <- nrow(df)-idx_signals[i]
        }
        
        df[idx_signals[i]: (exit_1+idx_signals[i]), position := "short"]
        df[idx_signals[i], status := "entered_trade"]
        df[(exit_1+idx_signals[i]), status := "exited_trade"]
        if(is.na(df[exit_1+idx_signals[i] + 1, position]) & !is.na(df[exit_1+idx_signals[i] + 1, signal]) ){
          
          if(df[exit_1+idx_signals[i] + 1, signal] %in% c("long", "short")){
            idx_signals <- sort(append(idx_signals, values= exit_1+idx_signals[i] + 1))
            price_signals <- df[idx_signals, close]  
          }
          
        }
      }
      i <- i+1 
    }
  }
  
  
  
  
  # df$returns <- c(diff(df$close), 0)
  # if(is.null(df$status) == FALSE){
  #   
  #   ids <- round(runif(n = length(which(!is.na(df$status)))/2, min = 10000, max = 10000000))
  #   ids_imp <- data.table(idx = which(!is.na(df$status)), id = rep(ids, each =2))
  #   df$trade_id <- NA
  #   df$trade_id[ids_imp$idx] <- ids_imp$id
  #   
  #   idss <- as.vector(na.omit(unique(df$trade_id)))
  #   
  #   for(g in 1:length(idss)){
  #     
  #     df$trade_id[which(df$trade_id %in% idss[g])[1]:which(df$trade_id %in% idss[g])[2]] <- idss[g]
  #     
  #   }
  #   
  #   df[status %in% c("entered_trade", "exited_trade"), fees := close*0.0026]
  #   
  #   
  #   fees <- df[, sum(fees, na.rm = T), by = trade_id][!is.na(trade_id)]
  #   df[status == "exited_trade", returns := 0]
  #   profs[j] <- df[position =="long", sum(returns)] + df[position =="short", sum(-returns)] - sum(fees$V1)
  #   
  #   
  #   summaries_long <- df[position == "long", .(outcome = sum(returns)), by = trade_id][!is.na(trade_id)][, type:= "long"]
  #   summaries_long <- merge(summaries_long, fees, by = "trade_id", all.x = T)
  #   summaries_long[, outcome:= outcome -V1]
  #   summaries_short <- df[position == "short", .(outcome = sum(-returns)), by = trade_id][!is.na(trade_id)][, type:= "short"]
  #   summaries_short <- merge(summaries_short, fees, by = "trade_id", all.x = T)
  #   summaries_short[, outcome:= outcome -V1]
  #   summaries <- rbind(summaries_long, summaries_short)
  #   summaries[outcome > 0, .N]/nrow(summaries)
  #   
  #   types <- unique(df[, .(position, trade_id)])
  #   types_profs <- merge(summaries, types, by = "trade_id", all.x = T)
  #   types_profs[, sum(outcome), by= position]
  #   
  #   
  #   # summaries
  #   strategy_results[[j]] <- data.table(HODL = tail(df[, close],1) - head(df[, close],1),
  #                                       PROFS = profs[j],
  #                                       PROFS_PER = round(profs[j] /head(df[,close], 1)*100, 2),
  #                                       WINRATIO = summaries[outcome > 0, .N]/nrow(summaries),
  #                                       N_TRADES = sum(unique(df[, .(position, trade_id)])[, table(position)]),
  #                                       N_LONG = unique(df[, .(position, trade_id)])[, table(position)][1],
  #                                       N_SHORT = unique(df[, .(position, trade_id)])[, table(position)][2],
  #                                       SHORT_PROFS = types_profs[, sum(outcome), by= position][position =="short", V1],
  #                                       LONG_PROFS = types_profs[, sum(outcome), by= position][position =="long", V1],
  #                                       SHORT_WINRATIO = types_profs[position == "short"  & outcome > 0, .N]/nrow(types_profs[position == "short"]),
  #                                       LONG_WINRATIO = types_profs[position == "long"  & outcome > 0, .N]/nrow(types_profs[position == "long"]))
  #   strategy_results[[j]] <<- cbind(strategy_results[[j]], testing_params[j, ])
  #   
  # } else {
  #   profs[j]<- 0
  #   strategy_results[[j]] <- data.table(HODL = tail(df[, close],1) - head(df[, close],1),
  #                                       PROFS = profs[j],
  #                                       PROFS_PER = round(profs[j] /head(df[,close], 1)*100, 2),
  #                                       WINRATIO = 0,
  #                                       N_TRADES = 0,
  #                                       N_LONG = 0,
  #                                       N_SHORT = 0,
  #                                       SHORT_PROFS = 0,
  #                                       LONG_PROFS = 0,
  #                                       SHORT_WINRATIO = 0,
  #                                       LONG_WINRATIO = 0)
  #   strategy_results[[j]] <<- cbind(strategy_results[[j]], testing_params[j, ])
  # }  
}

trades_evaluation_withTrail <- function(){
  
  df[, groups:= as.character(rep(1:length(rle(df$signal)$lengths),
                                 rle(df$signal)$lengths))]
  
  df[is.na(signal), groups := NA]
  
  df[, index :=  1:nrow(df)]
  
  entries <- df[, head(index, 1), by=groups][!is.na(groups)]
  
  df[entries$V1, entry := "enter"]
  df[, position := character()]
  idx_signals <- which(df$entry == "enter")
  price_signals <- as.vector(na.omit(df$close[(df$entry == "enter")]))
  
  if(length(idx_signals) >0 ){
    if(length(price_signals) == 1){
      index <-1
    } else
      index <- (length(idx_signals)-1)
    
    # for (i in 1:index){
    i <-1
    while (i <= length(idx_signals)){
      
      # print(idx_signals)
      # print(idx_signals[i])
      if(is.na(df$position[idx_signals[i]])  & df$signal[idx_signals[i]] == "long"){
        # returns_1 <- round((df$close[idx_signals[i]:nrow(df)] - price_signals[i]) / price_signals[i], 3)*100
        
        ###### Experimental
        # trailing stoploss
        
        test <- data.table(cl_tr = df$close[idx_signals[i]:nrow(df)], status= 0)
        test[1, status := cl_tr - cl_tr*trail]
        l<-2
        for(l in 2:nrow(test)){


          # test[l, status := (cl_tr-V2)/V2 ]
          if(test[l, cl_tr] > test[l-1, cl_tr]){
            test[l, status := cl_tr - cl_tr*trail]

            if(test[l, status] < test[l-1, status]){
              test[l, status := test$status[l-1]]
            }


        } else if (test[l, cl_tr] <= test[l-1, cl_tr]){
            test[l, status := test$status[l-1]]
        }
        }

        exit_1 <- head(which(test$status >test$cl_tr),1)
        
        ###### Experimental
        
        # returns_1 <-returns_1[-1]
        # exit_1 <- head(which(returns_1 > tp | returns_1 < sl), 1)
        if(length(exit_1) == 0){
          exit_1 <- nrow(df)-idx_signals[i]
        }
        
        df[idx_signals[i]: (exit_1+idx_signals[i]), position := "long"]
        df[idx_signals[i], status := "entered_trade"]
        df[(exit_1+idx_signals[i]), status := "exited_trade"]
        
        if(is.na(df[exit_1+idx_signals[i] + 1, position]) & !is.na(df[exit_1+idx_signals[i] + 1, signal]) ){
          
          if(df[exit_1+idx_signals[i] + 1, signal] %in% c("long", "short")){
            idx_signals <- sort(append(idx_signals, values= exit_1+idx_signals[i] + 1))
            price_signals <- df[idx_signals, close]  
          }
          
        }
        
        
        # print("Position entered")
      } else if(is.na(df$position[idx_signals[i]])  & df$signal[idx_signals[i]] == "short") {
        returns_1 <- -round((df$close[idx_signals[i]:nrow(df)] - price_signals[i]) / price_signals[i], 3)*100
        returns_1 <-returns_1[-1]
        exit_1 <- head(which(returns_1 > tp | returns_1 <sl),1)
        
        if(length(exit_1) == 0){
          exit_1 <- nrow(df)-idx_signals[i]
        }
        
        df[idx_signals[i]: (exit_1+idx_signals[i]), position := "short"]
        df[idx_signals[i], status := "entered_trade"]
        df[(exit_1+idx_signals[i]), status := "exited_trade"]
        if(is.na(df[exit_1+idx_signals[i] + 1, position]) & !is.na(df[exit_1+idx_signals[i] + 1, signal]) ){
          
          if(df[exit_1+idx_signals[i] + 1, signal] %in% c("long", "short")){
            idx_signals <- sort(append(idx_signals, values= exit_1+idx_signals[i] + 1))
            price_signals <- df[idx_signals, close]  
          }
          
        }
      }
      i <- i+1 
    }
  }
  
}


# Compound returns formula
# P initial balance
# r Percentage returns
# n number of times interest applied per Year
# t number of time periods elapsed
compound_returns <- function(P, r, n, t){
  A = P*(1 + (r/n) )**(n*t)
  return(A)
}

# Split data frames
split_dataframe <- function(df, n_splits) {
  # Calculate the number of rows in each split
  split_size <- ceiling(nrow(df) / n_splits)
  
  # Use split() and rep() to create a list of splits
  splits <- split(df, rep(1:n_splits, each = split_size, length.out = nrow(df)))
  
  return(splits)
}

