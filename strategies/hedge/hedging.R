options(scipen = 999)
library(data.table)
library(ggplot2)
library(ggpubr)
OHLC <- read.csv('/Users/christospolysopoulos/Repositories/Private/backtesting_module/data/XXBTZUSD/OHLC_15Min.csv');setDT(OHLC)
ind <- sample(1:nrow(OHLC), 1)
indeces <- ind: (ind+500)
tp_long <- 0.01
tp_short <- 0.015
OHLC <- OHLC[indeces, ]
vol_trade <- 1000/OHLC[1, close]
ggplot(data=OHLC, aes(x=time, y = close, group=1))+
  geom_line()

trade_tab_long <- copy(OHLC[1:nrow(OHLC)])
trade_tab_long[, diff :=c(0, diff(close))]
trade_tab_long[, shifted :=shift(close)]
trade_tab_long[, percent_dif_long := diff/shifted]
trade_tab_long[is.na(percent_dif_long), percent_dif_long := 0]
trade_tab_long[, cum_diff_perc_long:= cumsum(percent_dif_long)]
trade_tab_long[1, status_long :="entered"]
short_time_enter <- which(trade_tab_long$cum_diff_perc_long< -0.005)[1]
trade_tab_long[short_time_enter, status_short := "entered"]

trade_tab_long[short_time_enter:nrow(trade_tab_long), percent_dif_short := diff/shifted]
trade_tab_long[short_time_enter, percent_dif_short := 0]

trade_tab_long[is.na(percent_dif_short), percent_dif_short := 0]
trade_tab_long[, cum_diff_perc_short:= cumsum(percent_dif_short)]
View(trade_tab_long)
trade_tab_long[which(trade_tab_long$cum_diff_perc_short < -0.01)[1], status_short := "exited"]
trade_tab_long[which(trade_tab_long$cum_diff_perc_short < -0.01)[1], status_long := "exited"]

# results 
vol_ent_long <- trade_tab_long[status_long =="entered", 1000/close]
cost_exit_long <- trade_tab_long[status_long =="exited", vol_ent_long*close]
1000-cost_exit_long
vol_ent_short <- trade_tab_long[status_short =="entered", 1015/close]
cost_exit_short <- trade_tab_long[status_short =="exited", vol_ent_short*close]
1015-cost_exit_short

ggplot(data=trade_tab_long[1:300], aes(x=time, y = close, group=1))+
  geom_line()+
  geom_point(data = trade_tab_long[status_long =="entered"], aes(x=time, y = close), size=2, color="green")+
  geom_point(data = trade_tab_long[status_short =="entered"], aes(x=time, y = close), size=2, color="red")+
  geom_point(data = trade_tab_long[status_short =="exited"], aes(x=time, y = close), size=2, color="blue")


1000*tp_long# prospective profit 1010
# goes wrong
# open short position 0.005 0.5% below of buy order worth 0.5% more in total cost of the initial trade
