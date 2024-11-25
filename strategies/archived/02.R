pair <- "BTCUSD"
# pair <- "SHIBEUR"
# Path to save results
data_path <- "Code/Parameter_optim/Data"

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")


ticks <- c(60)
units <- c("minutes")
intervals <- paste(ticks, units, sep = " ")


df <- trades_to_OHLC(pair = pair,
                     interval = intervals,
                     #from_date = "2022-07-01",
                     #to_date = "2023-07-01",
                     date_subset = F)
test<- read_csv("/Users/christospolysopoulos/Repositories/Private/backtesting_module/data/XXBTZUSD/OHLC.csv")
test <- df[[5]]

Calculate_spline <- function(x){
  
  smoothingSpline_fast = smooth.spline(x ~ as.numeric(1:length(x)) , spar = 0.9)
  spline <- predict(smoothingSpline_fast)$y
  deriv <- predict(smoothingSpline_fast, deriv = 1)$y
  return(spline)
}
i <- 100
for(i in 300:nrow(test)){
  df <- test[200:i,]
  spl <- Calculate_spline(df$close)
  df$splines <- spl
  
  plot(df$close)
  lines(df$splines, col = "red")
  Sys.sleep(0.3)
}
Calculate_spline(test$close)



df <- df[[1]]
setnames(df, "interval", "date")
df$week <- isoweek(as.Date(df$date))
df$weekday <- weekdays(as.Date(df$date))
df$year <- year(as.Date(df$date))
df$hour <- hour(df$date)
df$month <- month(as.Date(df$date))
df$week_grouping <- paste0(df$week,"_", df$year)
df$hourly_grouping <- paste0(df$hour,"_",df$week,"_", df$year)
df$daily_grouping <- paste0(df$weekday,"_",df$week,"_", df$year)
df$monthly_grouping <- paste0(df$month,"_", df$year)
weeks <- as.character(unique(df$week))
weeks <- weeks[!weeks %in%names(which.min(table(df$week)))]

grouping_var <- unique(df$hourly_grouping)
SP <- c()
sp_test <- list()
rs_test <- list()
RS <- c()
j <-1
# for(j in 1:length(unique(splits[, seq]))){
for(j in 1:length(grouping_var)){
# subdf <- df[splits[seq == j, idx], ]
subdf <- df[hourly_grouping == grouping_var[j], ]
SP[j] <- median(head(sort(subdf[, close]), 5))


sp_test[[j]] <- c(min(head(sort(subdf[, close]), 5)), max(head(sort(subdf[, close]), 5)))
rs_test[[j]] <- c(min(tail(sort(subdf[, close]), 5)), max(tail(sort(subdf[, close]), 5)))
RS[j] <- median(tail(sort(subdf[, close]), 5))
}
    
SP <- SP[SP<tail(df$close, 1)]
  # needed <- n_orders-length(SP)
SP <- sort(SP, decreasing = T)
SP_per <- list()
for(i in 1:length(SP)){
  
  SP_per[[i]] <-round((SP[i]-SP)/SP *100,2)
}

col <- matrix(SP)
row <- matrix(SP, nrow =1)
length(row)
mat <- matrix(NA, nrow = length(row), ncol = length(row))
rownames(mat) <-SP 
colnames(mat) <-SP 
i <- 1
for(i in 1:length(SP)){
  mat[, i] <- round(abs((SP[i] - SP)/SP)*100, 2)
}
i <- 1
summarised <- c()
for(i in 1:dim(mat)[1]){
  summarised[i] <- names(which(mat[,i]<=1))
}

unique(summarised)
# 
# pairs <- crossing(SP)
# 
# dist(SP, upper = T)
# 
# SP_per_booleans <- lapply(SP_per, function(x) abs(x)<=5)
# SP_summarised <- c()
# for(i in 1:length(SP_per_booleans)){
#   SP_summarised[i] <- mean(SP[SP_per_booleans[[i]]])
# }
# 
# 
# SP[SP_per_booleans[[3]]]
# df

as.numeric(df$date[which(df$date == "2023-11-11 22:00:00")])
setnames(df, "date", "inteval")

candles(data =df)+
  geom_hline(yintercept = as.numeric(unique(summarised)), alpha =0.2)
# +
#   geom_vline(xintercept = as.numeric(df$date[which(df$date == "2023-11-11 22:00:00")]))


df_test <- trades_to_OHLC(pair = pair,
                     interval = intervals,
                     from_date = "2023-07-02",
                     to_date = "2023-12-02",
                     date_subset = T)
df_test <- df_test[[1]]
candles(data =df_test)+
  geom_hline(yintercept = as.numeric(unique(summarised)), alpha =0.2, colour = "black")
