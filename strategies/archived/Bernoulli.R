pair <- "BTCUSD"
data_path <- "Code/Parameter_optim/Data"

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

file <- paste0(paste(pair_data_results, pair, sep = "/"), ".csv.gz")
frame <- fread(file)
colnames(frame) <- c("price", "volume", "epoch_time", "buy_sell", "market_limit","misc",
                     "trade_id", "last_id", "Date_POSIXct", "Time", "Date", "Hour")
bet <- 1000

dates <- unique(frame$Date)
i <- 33
bool <- list()
net <- list()
for(i in 1:length(dates)){
  frame_day <- frame[Date == dates[i]]
  frame_day[, dif := (price- head(price, 1))/head(price, 1) *100 ]
  if(any(frame_day[, dif>1])){
    bool[[i]] <- 1
    net[[i]] <- bet*0.003
  }else{
    bool[[i]] <- 0
    loss <- (tail(frame_day[, price], 1)-frame_day[1, price])/frame_day[1, price]
    net[[i]] <- bet*loss
  }
}

sum(unlist(net))



bool <- list()
for(i in 1:length(dates)){
  frame_day <- frame[Date == dates[i]]
  buy_in <- bet/frame_day[1, price]
  
  frame_day[, dif := (price- head(price, 1))/head(price, 1) *100 ]
  if(any(frame_day[, dif>1])){
    bool[[i]] <- 1
  }else{
    bool[[i]] <- 0
  }
}

myvec <- unlist(bool)

sum(myvec)/length(myvec)
binconf(sum(myvec), length(myvec), alpha = 0.05, method = 'all')
# binom.confint(sum(X), length(X), conf.level = 0.95, method = 'all')

# true expected value
ev <- 0.80
ev*1000-(1-ev)*1000

# frame_day <- frame[Date == "2024-04-16"]
# frame_day[, dif := (price- head(price, 1))/head(price, 1) *100 ]
# p1 <- ggplot(data=frame_day, aes(x = Date_POSIXct, y = dif))+
#   geom_line(colour = 'grey')+
#   geom_hline(yintercept = 1, colour = "red")
# p1
# p2 <- ggplot(data=frame_day, aes(x = Date_POSIXct, y = price))+
#   geom_line(colour = 'grey')
# ggarrange(p1, p2,nrow =2)


# Confidence interval
p <- runif(1,0,1)
X <- sample(c(0,1), size = 100, replace = TRUE, prob = c(1-p, p))
library(Hmisc)
binconf(sum(X), length(X), alpha = 0.05, method = 'all')
library(binom)
binom.confint(sum(X), length(X), conf.level = 0.95, method = 'all')
