rm(list=ls())
gc()
# Source functions
library(profvis)
library(runner)
library(Rfast)
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
pair <- "SOLUSD"
# pair <- "SHIBEUR"
# Path to save results
data_path <- "Data"
funds <- 1000
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
candles(df1)


# Set parameters table
# look_back <- data.table(bar=c(168*1, 168*2, 168*3,168*4),flag=1)
# SL <- data.table(sl=c(0.05),flag=1)
# TP <- data.table(tp=c(0.05, 0.1),flag=1)
# median_number <- data.table(med_num=seq(1, 20, 5),flag=1)
# last_exclude_number <- data.table(exc_num=seq(1, 20, 5),flag=1)
# params <- left_join(look_back,SL)%>%left_join(TP)%>%left_join(median_number)%>%left_join(last_exclude_number)
# params <- data.table(bar=336, sl =0.075, tp = 0.1, med_num = 20, exc_num=20)


look_back <- data.table(bar=seq(48, 700, 24),flag=1)
SL <- data.table(sl=c(0.05, 0.1, 0.2),flag=1)
TP <- data.table(tp=c(0.05, 0.1, 0.2),flag=1)
median_number <- data.table(med_num=seq(1, 31, 5),flag=1)
last_exclude_number <- data.table(exc_num=seq(1, 31, 5),flag=1)
params <- left_join(look_back,SL)%>%left_join(TP)%>%left_join(median_number)%>%left_join(last_exclude_number)
# params <- data.table(bar=336, sl =0.075, tp = 0.1, med_num = 20, exc_num=20)

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
      price_entered <- tmp[ind_enter:nrow(tmp), ][position == "enter_long", close]
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
      price_entered <- tmp[ind_enter:nrow(tmp), ][position == "enter_short", close]
      tmp[ind_enter:nrow(tmp), diff := (-1)*c(0, cumsum(diff(tmp[ind_enter:nrow(tmp), close])))/price_entered]
      ind_exit <- which(tmp$diff< (-1)*params$tp[h]|tmp$diff>params$sl[h])[1]
      
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
  b <- tmp[!is.na(position), (close[position =="exit_short"]-close[position =="enter_short"])/close[position =="enter_short"], by=trade_id]
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
  k[, V1 := V1 -total_fees]
  res_tmp <- params[h, ]
  res_tmp[, `:=` (n_trades = nrow(k),
                  win_rate = sum(k$V1>0)/nrow(k),
                  aver_trade = mean(k$V1),
                  sum_per = sum(k$V1),
                  init_funds =funds,
                  final_funds = funds+cumsum(funds*k$V1)[length(k$V1)])]
  
  results[[h]] <- res_tmp
  print(h)
  print(res_tmp)
  
  }

# })
View(rbindlist(results))

p1 <- candles(tmp)+
  geom_point(data=tmp[position == "enter_long"], aes(x=interval, y=close), fill="lightblue3",colour="black", shape =24, size=2)+
  geom_point(data=tmp[position == "exit_long"], aes(x=interval, y=close), fill="lightblue3", colour="black",shape =25, size=2)+
  geom_point(data=tmp[position == "enter_short"], aes(x=interval, y=close), fill="darkorchid1", colour="black", shape =25, size=2)+
  geom_point(data=tmp[position == "exit_short"], aes(x=interval, y=close), fill="darkorchid1", colour="black",shape =24, size=2)+
  geom_line(data=tmp, aes(x=interval, y =resistance))+
  geom_line(data=tmp, aes(x=interval, y =support))
# param_result
p1

# 
# htmlwidgets::saveWidget(p, "profile.html")
# Save
# save(daily_res, file=paste0("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Grid/results/", paste0(ticks,"_", units,"_", pair, Sys.time()), ".Rdata"))
