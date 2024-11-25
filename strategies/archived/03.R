rm(list = ls())
library(gtools)
# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Load the data and decide training and test set
data_path <- "Code/Parameter_optim/Data"

# Parameters
pair <- "ADAUSD"
look_back <- 7 # days
num_orders <- 8
level_interval <- "hour" # "month", "day", "hour" (interval loaded must be < than level interval)
exit <- "standard"
median_number <- 5
percent_apart <- 5
# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")
ticks <- c(15)
units <- c(rep("minutes", 1))
intervals <- paste(ticks, units, sep = " ")
df_all <- trades_to_OHLC(pair = pair,
                     interval = intervals,
                     from_date = "2022-06-01",
                     to_date = "2023-06-01",
                     date_subset = F)[[1]]

train_period <- c(min(df_all$interval), min(df_all$interval)+days(look_back))
test_period <- c(min(df_all$interval)+days(look_back+1), max(df_all$interval))
df_train <- df_all[interval <= train_period[2]]
df_test <- df_all[interval >= test_period[1]]


df_train$week <- isoweek(as.Date(df_train$interval))
df_train$weekday <- weekdays(as.Date(df_train$interval))
df_train$year <- year(as.Date(df_train$interval))
df_train$hour <- hour(df_train$interval)
df_train$month <- month(as.Date(df_train$interval))
df_train$week_grouping <- paste0(df_train$week,"_", df_train$year)
df_train$hourly_grouping <- df_train$interval
df_train$daily_grouping <- paste0(df_train$weekday,"_",df_train$week,"_", df_train$year)
df_train$monthly_grouping <- paste0(df_train$month,"_", df_train$year)

if(level_interval == "week"){
  grouping_level <- unique(df_train$week_grouping)
  name_level <- "week_grouping"
} else if(level_interval == "hour"){
  grouping_level <- unique(df_train$interval)
  name_level <- "interval"
} else if(level_interval == "day"){
  grouping_level <- unique(df_train$daily_grouping)
  name_level <- "daily_grouping"
}

SP <- c()
RS <- c()
j <-3

for(j in 1:length(grouping_level)){
  subdf <- df_train[get(name_level) == grouping_level[j], ]
  SP[j] <- median(head(sort(subdf[, close]), median_number))
  RS[j] <- median(tail(sort(subdf[, close]), median_number))
}

levels <- data.table(SP,
                     percent_SP = round((SP - min(SP)) / (max(SP) - min(SP))*100, 3),
                     RS = RS, 
                     percent_RS = round((RS - min(RS)) / (max(RS) - min(RS))*100, 3))
levels[, groups_SP := cut(percent_SP, breaks= seq(0, 100, by=100/num_orders), include.lowest = T)]
levels[, groups_RS := cut(percent_RS, breaks= seq(0, 100, by=100/num_orders), include.lowest = T)]

entry_points <- levels[, .(entries = median(SP)), by = groups_SP]
entry_points <- entry_points[entries<tail(df_train[, close], 1)]


exit_points <- levels[, .(exits = median(RS)), by = groups_RS]
exit_points <- exit_points[exits>tail(df_train[, close], 1)]


dat <- list(df_train[, state := "train"],df_test[, state := "test"])
dat <- rbindlist(dat, fill = T)


candles(data =dat)+
  geom_vline(xintercept = dat[, max(interval), by = state][1, V1], alpha =0.9, colour = "black", linetype="dashed")+
  geom_hline(yintercept = entry_points[, entries], alpha =0.9, colour = "darkgreen")+
  geom_hline(yintercept = exit_points[, exits], alpha =0.9, colour = "red")
