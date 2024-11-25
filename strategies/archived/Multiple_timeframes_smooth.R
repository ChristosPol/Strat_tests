library(data.table)
library(ggplot2)
library(ggpubr)
d <- read.csv("/Users/christospolysopoulos/Repositories/Private/backtesting_module/data/XXBTZUSD/trades.csv")

OHLC <- read.csv('/Users/christospolysopoulos/Repositories/Private/backtesting_module/data/XXBTZUSD/OHLC_D.csv');setDT(OHLC)
net <- read.csv('/Users/christospolysopoulos/Repositories/Private/backtesting_module/data/XXBTZUSD/BS_NET_D.csv');setDT(net)
net[, X:= NULL]
setorder(net, time)
OHLC <- merge(
  OHLC,
  net[,  .(b_per = volume[buy_sell =="b"]/sum(volume),
           s_per = volume[buy_sell =="s"]/sum(volume),
           tot_volume =sum(volume)), by = time],
           by="time", all.x = T
  )

p1 <- ggplot(data=OHLC, aes(x = time, y =  close))+
  geom_point()

p2 <- ggplot(data=OHLC, aes(x = time, y =  b_per))+
  geom_line(group = 1)

ggarrange(p1, p2,nrow = 2)
