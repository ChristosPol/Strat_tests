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

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")
ticks <- c(1)
units <- c(rep("minutes", 1))
intervals <- paste(ticks, units, sep = " ")
df_all <- trades_to_OHLC(pair = pair,
                         interval = intervals,
                         from_date = "2024-03-15",
                         to_date = "2024-03-31",
                         date_subset = F)[[1]]


look_back <- 2


train_period <- c(min(df_all$interval), min(df_all$interval)+days(look_back))
test_period <- c(min(df_all$interval)+days(look_back+1), max(df_all$interval))
df_train <- df_all[interval <= train_period[2]]
p3 <- candles(data = df_test)

df_test <- df_all[interval >= test_period[1]]
p1 <- candles(data = df_train)



maxx <- max(df_train$close)
minn <- min(df_train$close)
df_train[, scale_percent := (df_train$close - minn) / (maxx - minn)*100]
p2 <- ggplot(data=df_train, aes(x = interval, y = scale_percent))+
  geom_line(colour = "black") + theme_bw()
ggarrange(p1, p2, nrow = 2)



ggarrange(p1, p3)
