rm(list=ls())
options(scipen = 999)
library(data.table)
library(ggplot2)
library(rootSolve)
library(RcppRoll)
library(ggpubr)
library(plyr)
library(lubridate)
library(zoo)
trades <- fread("/Users/christospolysopoulos/Repositories/Private/backtesting_module/data/XXBTZUSD/trades.gz")
trades_sub <- copy(trades)
trades_sub <- trades_sub[time >= "2024-02-01" & time < "2024-02-15"]


# Calculate price changes to see if you can identify rallies -------------------
trades_sub[, price_change_1000 := rollapply(trades_sub[, price], 1000, function(x) round(100*(tail(x,1)-head(x,1))/head(x,1), 1) , fill = NA, align = "right")]
idx <- which(trades_sub[, price_change_1000 > 1.2])
idx_enter <- idx -1000
trades_sub[idx_enter, entry:=1]
trades_sub[is.na(entry), entry:=0]
trades_sub[, cost:=price*volume]


# trades_sub <- trades_sub[time >= "2024-03-12 09:00:00" & time <= "2024-03-13 21:00:00"]
trades_sub[buy_sell =="b", vol_buy := volume]
trades_sub[is.na(vol_buy), vol_buy := 0]

trades_sub[buy_sell =="s", vol_sell := volume]
trades_sub[is.na(vol_sell), vol_sell := 0]


trades_sub[, sum_buy_50 := zoo::rollsum(vol_buy, k=50, fill = NA, align = "right")]
trades_sub[, sum_sell_50 := zoo::rollsum(vol_sell, k=50, fill = NA, align = "right")]
trades_sub[, vol_ratio_50 := round(100*sum_buy_50/(sum_buy_50+sum_sell_50), 1)]
trades_sub[, cost_50 := zoo::rollsum(cost, k=50, fill = NA, align = "right")]

trades_sub[, sum_buy_100 := zoo::rollsum(vol_buy, k=100, fill = NA, align = "right")]
trades_sub[, sum_sell_100 := zoo::rollsum(vol_sell, k=100, fill = NA, align = "right")]
trades_sub[, vol_ratio_100 := round(100*sum_buy_100/(sum_buy_100+sum_sell_100), 1)]
trades_sub[, cost_100 := zoo::rollsum(cost, k=100, fill = NA, align = "right")]

trades_sub[, sum_buy_200 := zoo::rollsum(vol_buy, k=200, fill = NA, align = "right")]
trades_sub[, sum_sell_200 := zoo::rollsum(vol_sell, k=200, fill = NA, align = "right")]
trades_sub[, vol_ratio_200 := round(100*sum_buy_200/(sum_buy_200+sum_sell_200), 1)]
trades_sub[, cost_200 := zoo::rollsum(cost, k=200, fill = NA, align = "right")]

trades_sub[, sum_buy_500 := zoo::rollsum(vol_buy, k=500, fill = NA, align = "right")]
trades_sub[, sum_sell_500 := zoo::rollsum(vol_sell, k=500, fill = NA, align = "right")]
trades_sub[, vol_ratio_500 := round(100*sum_buy_500/(sum_buy_500+sum_sell_500), 1)]
trades_sub[, cost_500 := zoo::rollsum(cost, k=500, fill = NA, align = "right")]

trades_sub[, sum_buy_1000 := zoo::rollsum(vol_buy, k=1000, fill = NA, align = "right")]
trades_sub[, sum_sell_1000 := zoo::rollsum(vol_sell, k=1000, fill = NA, align = "right")]
trades_sub[, vol_ratio_1000 := round(100*sum_buy_1000/(sum_buy_1000+sum_sell_1000), 1)]
trades_sub[, cost_1000 := zoo::rollsum(cost, k=1000, fill = NA, align = "right")]

trades_sub[, sum_buy_5000 := zoo::rollsum(vol_buy, k=5000, fill = NA, align = "right")]
trades_sub[, sum_sell_5000 := zoo::rollsum(vol_sell, k=5000, fill = NA, align = "right")]
trades_sub[, vol_ratio_5000 := round(100*sum_buy_5000/(sum_buy_5000+sum_sell_5000), 1)]
trades_sub[, cost_5000 := zoo::rollsum(cost, k=5000, fill = NA, align = "right")]

trades_sub[, sum_buy_10000 := zoo::rollsum(vol_buy, k=10000, fill = NA, align = "right")]
trades_sub[, sum_sell_10000 := zoo::rollsum(vol_sell, k=10000, fill = NA, align = "right")]
trades_sub[, vol_ratio_10000 := round(100*sum_buy_10000/(sum_buy_10000+sum_sell_10000), 1)]
trades_sub[, cost_10000 := zoo::rollsum(cost, k=10000, fill = NA, align = "right")]



# Train set --------------------------------------------------------------------
train_prop <- 0.7
train_set <- trades_sub[1:(nrow(trades_sub)*train_prop)]
test_set <- trades_sub[((nrow(trades_sub)*train_prop)+1):nrow(trades_sub)]


p1 <- ggplot(data=train_set, aes(x=time, y = price))+
  geom_point()+
  geom_point(data=train_set[entry ==1], aes(x=time, y = price), colour="green")

p2 <- ggplot(data=test_set, aes(x=time, y = price))+
  geom_point()

ggarrange(p1, p2, nrow = 2)


train_set[, entry:=as.factor(entry)]
fit <- randomForest(entry~price_change_1000+vol_ratio_1000, data=train_set[!is.na(price_change_1000) & !is.na(vol_ratio_1000)])
summary(fit)


library(usethis) 
usethis::edit_r_environ()
test_set[, prediction:=predict(fit, newdata = test_set)]
table(test_set$prediction)

ggplot(data=test_set, aes(x=time, y = price))+
  geom_point()+
  geom_point(data=test_set[prediction ==1], aes(x=time, y = price), colour="green")








