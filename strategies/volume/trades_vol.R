rm(list=ls())
options(scipen = 999)
library(data.table)
library(ggplot2)
library(rootSolve)
library(RcppRoll)
library(ggpubr)
library(plyr)
# fwrite(trades, file = "/Users/christospolysopoulos/Repositories/Private/backtesting_module/data/XXBTZUSD/trades.gz")
library(lubridate)
trades <- fread("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Data/BTCUSD/BTCUSD.csv.gz")
colnames(trades) <- c("price", "volume", "epoch_time", "buy_sell", "market_limit","misc",
                      "trade_id", "last_id", "Date_POSIXct", "Time", "Date", "Hour")
trades[,`:=`(epoch_time=NULL, misc=NULL, last_id =NULL, Time=NULL, Hour=NULL)]

trades <- trades[Date =="2024-08-01"]

# Define vol sell, vol buy
trades[, volume_buy :=ifelse(buy_sell =="b", volume, 0)]
trades[, volume_sell :=ifelse(buy_sell =="s", volume, 0)]


trades[, M1 := floor_date(as.POSIXct(Date_POSIXct), unit = "1 minute")]
trades[, M2 := floor_date(as.POSIXct(Date_POSIXct), unit = "2 minute")]
trades[, M3 := floor_date(as.POSIXct(Date_POSIXct), unit = "3 minute")]
trades[, M5 := floor_date(as.POSIXct(Date_POSIXct), unit = "5 minute")]
trades[, M10 := floor_date(as.POSIXct(Date_POSIXct), unit = "10 minute")]
trades[, M15 := floor_date(as.POSIXct(Date_POSIXct), unit = "15 minute")]
trades[, M30 := floor_date(as.POSIXct(Date_POSIXct), unit = "30 minute")]
trades[, M60 := floor_date(as.POSIXct(Date_POSIXct), unit = "60 minute")]

# Calculate metrics
trades[,M1_vol_ratio := sum(volume_buy)/(sum(volume_sell)+sum(volume_buy)), by = M1]
trades[,M1_vol := sum(volume_sell*price)+sum(volume_buy*price), by = M1]

trades[,M2_vol_ratio := sum(volume_buy)/(sum(volume_sell)+sum(volume_buy)), by = M2]
trades[,M2_vol := sum(volume_sell*price)+sum(volume_buy*price), by = M2]

trades[,M3_vol_ratio := sum(volume_buy)/(sum(volume_sell)+sum(volume_buy)), by = M3]
trades[,M3_vol := sum(volume_sell*price)+sum(volume_buy*price), by = M3]

trades[,M5_vol_ratio := sum(volume_buy)/(sum(volume_sell)+sum(volume_buy)), by = M5]
trades[,M5_vol := sum(volume_sell*price)+sum(volume_buy*price), by = M5]

trades[,M10_vol_ratio := sum(volume_buy)/(sum(volume_sell)+sum(volume_buy)), by = M10]
trades[,M10_vol := sum(volume_sell*price)+sum(volume_buy*price), by = M10]

trades[,M15_vol_ratio := sum(volume_buy)/(sum(volume_sell)+sum(volume_buy)), by = M15]
trades[,M15_vol := sum(volume_sell*price)+sum(volume_buy*price), by = M15]

trades[,M30_vol_ratio := sum(volume_buy)/(sum(volume_sell)+sum(volume_buy)), by = M30]
trades[,M30_vol := sum(volume_sell*price)+sum(volume_buy*price), by = M30]

trades[,M60_vol_ratio := sum(volume_buy)/(sum(volume_sell)+sum(volume_buy)), by = M60]
trades[,M60_vol := sum(volume_sell*price)+sum(volume_buy*price), by = M60]

p1 <- ggplot(data = trades, aes(x = Date_POSIXct, y = price, group=1))+
  geom_line()

p2 <- ggplot(data = trades, aes(x = M60, y = M60_vol_ratio, group=1))+
  geom_line()

p3 <- ggplot(data = trades, aes(x = M30, y = M30_vol_ratio, group=1))+
  geom_line()

p4 <- ggplot(data = trades, aes(x = M15, y = M15_vol_ratio, group=1))+
  geom_line()

p5 <- plot_grid(p1,p2,p3,p4,  nrow=4, align="v")


p6 <- ggplot(data = trades, aes(x = M60, y = M60_vol)) + 
  geom_point()+geom_line()


p7 <- ggplot(data = trades, aes(x = M30, y = M30_vol)) + 
  geom_point()+geom_line()

p8 <- ggplot(data = trades, aes(x = M15, y = M15_vol)) + 
  geom_point()+geom_line()

p9 <- plot_grid(p1, p6,p7,p8,  nrow=4, align="v")


plot_grid(p5, p9)

trades_sub <- copy(trades)
trades_sub[, day:=day(time)]
trades_sub[, year:=year(time)]
trades_sub[, day_year:=paste0(year, "_", day)]
trades_sub[, .N, by=day_year][, mean(N)]

# Calculate price changes to see if you can identify rallies




trades_sub <- trades_sub[time >= "2024-04-20" & time < "2024-04-21"]
trades_sub[, price_change_100 := rollapply(trades_sub[, price], 1000, function(x) round(100*(tail(x,1)-head(x,1))/head(x,1), 1) , fill = NA, align = "right")]
trades_sub[, cost:= round(price*volume,1 )]



# trades_sub <- trades_sub[time >= "2024-03-12 09:00:00" & time <= "2024-03-13 21:00:00"]
trades_sub[buy_sell =="b", vol_buy := volume]
trades_sub[is.na(vol_buy), vol_buy := 0]

trades_sub[buy_sell =="s", vol_sell := volume]
trades_sub[is.na(vol_sell), vol_sell := 0]

trades_sub[, cost:=price*volume]
trades_sub[, bins:= round_any(price, 100)]

# trades_sub_sum <- trades_sub[, list(sum_dollars =sum(cost)), by=c("bins","buy_sell" )]
# setorder(trades_sub_sum, -sum_dollars)
# buys <- trades_sub_sum[buy_sell=="b", bins][1:20]
# sells <- trades_sub_sum[buy_sell=="s", bins][1:20]
# ggplot(trades_sub_sum, aes(x=sum_dollars, y=bins))+
#   geom_point()
# 
# 
# p0<-ggplot(trades_sub, aes(x=time, y=price))+
#   geom_point()+
#   geom_hline(yintercept = buys, col="green")+
#   geom_hline(yintercept = sells, col="red")


trades_sub[, sum_buy_50 := zoo::rollsum(vol_buy, k=50, fill = NA, align = "right")]
trades_sub[, sum_sell_50 := zoo::rollsum(vol_sell, k=50, fill = NA, align = "right")]
trades_sub[, vol_ratio_50 := round(100*sum_buy_50/(sum_buy_50+sum_sell_50), 1)]
trades_sub[, cost_50 := zoo::rollsum(cost, k=50, fill = NA, align = "right")]

trades_sub[, sum_buy_100 := zoo::rollsum(vol_buy, k=100, fill = NA, align = "right")]
trades_sub[, sum_sell_100 := zoo::rollsum(vol_sell, k=100, fill = NA, align = "right")]
trades_sub[, vol_ratio_100 := round(100*sum_buy_100/(sum_buy_100+sum_sell_100), 1)]
trades_sub[, cost_100 := zoo::rollsum(cost, k=100, fill = NA, align = "right")]

ggplot(data=trades_sub, aes(x=vol_ratio_1000, y = price_change_100))+
  geom_point()

ggplot(data=trades_sub[vol_ratio_1000>95], aes(x=vol_ratio_1000, y = price_change_100))+
  geom_point()


p1 <- ggplot(data=trades_sub, aes(x=time, y = price))+
  geom_point()+
  geom_point(data=trades_sub[vol_ratio_1000>85, ], aes(x =time, y =price), colour ="green")


p2 <- ggplot(data=trades_sub, aes(x=time, y = vol_ratio_1000))+
  geom_point()+
  geom_point(data=trades_sub[vol_ratio_1000>85, ], aes(x =time, y =vol_ratio_1000), colour ="green")


ggarrange(p1, p2, nrow=2)
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



trades_sub[vol_ratio_50 > 70 & vol_ratio_100 >70 & vol_ratio_200 > 70 &cost_50>1000000, position:=1]
trades_sub[vol_ratio_50 < 30 & vol_ratio_100 <30 & vol_ratio_200<30&cost_50>1000000, position:=-1]
trades_sub[, dif:=c(0,diff(price))]
trades_sub[, expected:=position*dif]
trades_sub[, sum(expected, na.rm = T)]
trades_sub[is.na(expected), expected:=0]
trades_sub[, cum:=cumsum(expected)]


p2 <- ggplot(data = trades_sub, aes(x =time, y = price, group=1, colour=position==1))+
  geom_line()
p2
View(trades_sub)

# p1 <- ggplot(data = trades_subdif# p1 <- ggplot(data = trades_sub, aes(x =time, y = vol_ratio_10000, group=1))+
#   geom_line()+
#   geom_line(aes(x =time, y = vol_ratio_5000, group=1), colour="red")+
#   geom_hline(yintercept = 50)+theme(axis.title.y=element_blank(),
#                                    axis.text.y=element_blank(),
#                                    axis.ticks.y=element_blank())
# +
#   geom_line(aes(x =time, y = vol_ratio_200, group=1), colour="darkgreen")+
#   geom_line(aes(x =time, y = vol_ratio_500, group=1), colour="darkblue")+
#   geom_line(aes(x =time, y = vol_ratio_1000, group=1), colour="black")+
#   geom_line(aes(x =time, y = vol_ratio_5000, group=1), colour="grey")

#   
# p3 <- ggplot(data = trades_sub, aes(x =time, y = cost_10000, group=1))+
#   geom_line()+theme(axis.title.y=element_blank(),
#                     axis.text.y=element_blank(),
#                     axis.ticks.y=element_blank())
# 
# ggarrange(p0, p2, p1,p3, nrow = 4)
