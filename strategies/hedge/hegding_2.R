# Define long term direction
rm(list=ls())
direction <- "short"
smallest_bet <- 50
available_funds <- 5250
pair <- "BTCUSD"
hedging_positions <- 14
bet_grid <- seq(smallest_bet, available_funds, smallest_bet)
incr_bet_grid <- bet_grid[1:which(cumsum(bet_grid)==available_funds)]


price_BTC_at_enter <- 50000
vol_first_bet <- incr_bet_grid[1]/price_BTC_at_enter
tp_first_bet <- 0.01
sl_hedge_first_bet <- 0.01
price_tp_first_bet <- price_BTC_at_enter - price_BTC_at_enter*tp_first_bet
price_sl_hedge_first_bet <- price_BTC_at_enter + price_BTC_at_enter*sl_hedge_first_bet


100-100*(0.01923077+0.02)
96.07692-100

204+204*0.01923077
207.9231-204

f1 <- function(x){
  100*(0.02+x)-204*x  
}

x = seq(-1, 1, by = 0.001)
plot(x, f1(x), type="l")
abline(h = 0, col = 'blue')
uniroot.all(f1, c(-0.10, 0.10))


f2 <- function(x){
  204*(0.01+x)-306*x  
}

x = seq(-1, 1, by = 0.001)
plot(x, f2(x), type="l")
abline(h = 0, col = 'blue')
uniroot.all(f2, c(-0.10, 0.10))
library(rootSolve)



# 204x-100(2+x) -306x+204(1+x)

f3 <- function(x){
  204*x-100*(0.02+x) -306*x+204*(0.01+x)
}

x = seq(-1, 1, by = 0.001)
plot(x, f3(x), type="l")
abline(h = 0, col = 'blue')
uniroot.all(f2, c(-0.10, 0.10))


f1(x =0.01923077)+f2(x =0.02)

rm(list=ls())
options(scipen = 999)
library(data.table)
library(ggplot2)
library(rootSolve)

library(ggpubr)
trades <- read.csv('/Users/christospolysopoulos/Repositories/Private/backtesting_module/data/XXBTZUSD/trades.csv');setDT(trades)
trades_sub <- copy(trades)


trades_sub <- trades_sub[time >= "2024-03-14" & time < "2024-03-15"]
ggplot(trades_sub, aes(x = 1:nrow(trades_sub), y = price, group=1))+
  geom_line()


bet1 <- 100

short_1_price <- 62050
short_1_volume <- bet1/trades_sub$price[1]
short_1_cost <- short_1_volume*short_1_price

long_1_price <- short_1_price+short_1_price*0.02
long_1_volume <- short_1_volume*2
long_1_cost <-long_1_volume* long_1_price

short_2_price <- long_1_price-long_1_price*0.01
short_2_volume <- short_1_volume*3
short_2_cost <-short_2_volume* short_2_price

f1 <- function(x){
  short_1_cost*(0.02+x)-long_1_cost*x  
}
uniroot.all(f1, c(-0.10, 0.10))

f2 <- function(x){
  long_1_cost*(0.01+x)-short_2_cost*x  
}
uniroot.all(f2, c(-0.10, 0.10))

tp1 <- 0.01
tp2 <- uniroot.all(f1, c(-0.10, 0.10))
tp3 <- uniroot.all(f2, c(-0.10, 0.10))


status_short_1 <- data.frame(limit_order_given = F, limit_order_opened=F,limit_order_entered=F, price_enter  = short_1_price, current_price = NA, percent_evaluation =0)
status_long_1 <- data.frame(limit_order_given = F, limit_order_opened=F,limit_order_entered=F, price_enter  = long_1_price, current_price = NA, percent_evaluation =0)
status_long_2 <- data.frame()


i <- 2
for (i in 2:nrow(trades_sub)){

  
  status_short_1[1, "limit_order_opened"] <- T
  status_short_1$limit_order_given <-T
  status_short_1[1, "current_price"] <- trades_sub$price[i]
  
  if(status_short_1[1, "limit_order_opened"] & (trades_sub$price[i]>=short_1_price) &  !status_short_1[1, "limit_order_entered"]){
    status_short_1$limit_order_entered <- T
    print("1 short trade entered")
  }
  
  
  if(status_short_1[1, "limit_order_entered"]){
    status_short_1[1, "percent_evaluation"] <- (trades_sub$price[i]-status_short_1[1, "price_enter"])/status_short_1[1, "price_enter"]*(-1)
    print(status_short_1[1, "percent_evaluation"])
    print("holding position..")
  }
  
  if(!status_short_1[1, "limit_order_entered"]){
    print("price level not reached yet")
  }
  
  
  
  if (status_short_1[1, "percent_evaluation"] >=0.02) {
    print("take profit met, exiting loop")
    break
  }
  
  
  if(status_short_1$limit_order_entered & status_short_1$percent_evaluation <= -0.01){
    print("Need to monitor price..")
    
    if(trades_sub$price[i] > status_long_1$price_enter & !status_long_1$limit_order_opened){
      status_long_1$limit_order_opened <- T
    }
    
  
  }
  
  Sys.sleep(0.1)
  print(trades_sub$price[i])
  print(i)
}


#Vectorised

bet1 <- 100

short_1_price <- 72000
short_1_volume <- bet1/trades_sub$price[1]
short_1_cost <- short_1_volume*short_1_price

long_1_price <- short_1_price+short_1_price*0.005
long_1_volume <- short_1_volume*2
long_1_cost <-long_1_volume* long_1_price

short_2_price <- long_1_price-long_1_price*0.01
short_2_volume <- short_1_volume*3
short_2_cost <-short_2_volume* short_2_price

f1 <- function(x){
  short_1_cost*(0.02+x)-long_1_cost*x  
}
uniroot.all(f1, c(-0.10, 0.10))

f2 <- function(x){
  long_1_cost*(0.01+x)-short_2_cost*x  
}
uniroot.all(f2, c(-0.10, 0.10))

tp1 <- 0.01
tp2 <- uniroot.all(f1, c(-0.10, 0.10))
tp3 <- uniroot.all(f2, c(-0.10, 0.10))

sub <- trades_sub[, .(time, price)]
idx_entry_short1 <- which(sub$price >= short_1_price)[1]
sub[idx_entry_short1, short_entered := T]
sub[(idx_entry_short1+1):nrow(sub), diff_short :=price -short_1_price]
sub[(idx_entry_short1+1):nrow(sub), percent_short :=-1*diff_short/short_1_price]
idx_sl_short1 <- which(sub$percent_short <=  -0.005)[1]
sub[idx_sl_short1, short_SL := T]
sub[(idx_sl_short1+1), decision_hedge_long := T ]
sub[(idx_sl_short1+1):nrow(sub), monitor_price_for_long := ifelse(price>long_1_price, T, F) ]
sub[which(!is.na(sub$monitor_price_for_long)& sub$monitor_price_for_long)[1]:nrow(sub), long1_sent := T]
sub[is.na(long1_sent), long1_sent :=F]
idx_long_enter <- which(paste(sub$monitor_price_for_long, sub$long1_sent, sep ="_") == "FALSE_TRUE")[1]
sub[idx_long_enter, long1_entered :=T]

sub[(idx_long_enter+1):nrow(sub), diff_long1 := price -long_1_price]
sub[(idx_long_enter+1):nrow(sub), percent_long1 := diff_long1/long_1_price]
sub[percent_long1 > tp2, tp_long_reached :=T]
sub[which(sub$tp_long_reached)[1], exit_all_positions:=T]
sub[, index:= 1:nrow(sub)]
ggplot(data=sub, aes(x=index,y=price, group=1))+
  geom_line()+
  geom_point(data=sub[short_entered ==T], aes(x=index,y=price), colour = "red", size = 2)+
  geom_point(data=sub[decision_hedge_long ==T], aes(x=index,y=price), colour = "blue", size = 2)+
  geom_point(data=sub[long1_sent ==T][1], aes(x=index,y=price), colour = "green", size = 2)+
  geom_point(data=sub[exit_all_positions ==T][1], aes(x=index,y=price), colour = "red", size = 2)

View(sub)


# sub[(idx_sl_short1+1):nrow(sub)]
# which(sub$price[(idx_sl_short1+1):nrow(sub)]>long_1_price)[1]


# 
# 
# 
# library(animation)
# 
# #Set delay between frames when replaying
# ani.options(interval=.05)
# 
# # Set up a vector of colors for use below
# col.range <- heat.colors(15)
# 
# # Begin animation loop
# # Note the brackets within the parentheses
# saveGIF({
#   
#   # For the most part, it’s safest to start with graphical settings in
#   # the animation loop, as the loop adds a layer of complexity to
#   # manipulating the graphs. For example, the layout specification needs to
#   # be within animation loop to work properly.
#   layout(matrix(c(1, rep(2, 5)), 6, 1))
#   
#   # Adjust the margins a little
#   par(mar=c(4,4,2,1) + 0.1)
#   
#   # Begin the loop that creates the 150 individual graphs
#   for (i in 1:150) {
#     
#     # Pull 100 observations from a normal distribution
#     # and add a constant based on the iteration to move the distribution
#     chunk <- rnorm(100)+sqrt(abs((i)-51))
#     
#     # Reset the color of the top chart every time (so that it doesn’t change as the
#     # bottom chart changes)
#     par(fg=1)
#     
#     # Set up the top chart that keeps track of the current frame/iteration
#     # Dress it up a little just for fun
#     plot(-5, xlim = c(1,150), ylim = c(0, .3), axes = F, xlab = "", ylab = "",  main = "Iteration")
#     abline(v=i, lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))
#     abline(v=i-1, lwd=5, col = rgb(0, 0, 255, 50, maxColorValue=255))
#     abline(v=i-2, lwd=5, col = rgb(0, 0, 255, 25, maxColorValue=255))
#     
#     # Bring back the X axis
#     axis(1)
#     
#     # Set the color of the bottom chart based on the distance of the distribution’s mean from 0
#     par(fg = col.range[mean(chunk)+3])
#     
#     # Set up the bottom chart
#     plot(density(chunk), main = "", xlab = "X Value", xlim = c(-5, 15), ylim = c(0, .6))
#     
#     # Add a line that indicates the mean of the distribution. Add additional lines to track
#     # previous means
#     abline(v=mean(chunk), col = rgb(255, 0, 0, 255, maxColorValue=255))
#     if (exists("lastmean")) {abline(v=lastmean, col = rgb(255, 0, 0, 50, maxColorValue=255)); prevlastmean <- lastmean;}
#     if (exists("prevlastmean")) {abline(v=prevlastmean, col = rgb(255, 0, 0, 25, maxColorValue=255))}
#     #Fix last mean calculation
#     lastmean <- mean(chunk)
#   }
# })
# 
# 
# 


# Manual trading

enter_long1 <- 0.35/100
enter_short2 <- 0.35/100
bet1 <- 100
short_1_price <- 69000
short_1_volume <- bet1/short_1_price
short_1_cost <- short_1_volume*short_1_price



long_1_price <- short_1_price+short_1_price*enter_long1
long_1_volume <- short_1_volume*2
long_1_cost <-long_1_volume* long_1_price

short_2_price <- long_1_price-long_1_price*0.01
short_2_volume <- short_1_volume*3
short_2_cost <-short_2_volume* short_2_price

f1 <- function(x){
  short_1_cost*(0.02+x)-long_1_cost*x  
}
uniroot.all(f1, c(-0.10, 0.10))

f2 <- function(x){
  long_1_cost*(0.01+x)-short_2_cost*x  
}
uniroot.all(f2, c(-0.10, 0.10))

tp1 <- 0.01
tp2 <- uniroot.all(f1, c(-0.10, 0.10))
tp3 <- uniroot.all(f2, c(-0.10, 0.10))
