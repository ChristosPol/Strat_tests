rm(list=ls())

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Parameters for SOL_5min_random_day_10_trades_per_side
# bars_reset <- data.table(bars_reset=c(24, 48, 72, 96, 120, 144, 168),flag=1)
# exit_points <- data.table(exit=seq(0.01,0.1 ,0.01), flag=1)
# # start <-data.table(start= c(0.005,0.01, 0.015, 0.025,0.035),flag=1)
# start <-data.table(start= c(0.005,0.01, 0.015, 0.02,0.03, 0.05, 0.07),flag=1)
# step <- data.table(step=c(0.001, 0.002, 0.005, 0.01, 0.02),flag=1)
# params <- left_join(bars_reset,exit_points)%>%left_join(start)%>%left_join(step)

# Parameters for SOL_5min_random_day_20_trades_per_side_fewer_params
# bars_reset <- data.table(bars_reset=c(24, 48, 72, 96, 120, 144, 168),flag=1)
# exit_points <- data.table(exit=c(0.008, seq(0.01,0.05 ,0.01)), flag=1)
# # exit_points <- data.table(exit=seq(0.01,0.1 ,0.01), flag=1)
# # start <-data.table(start= c(0.005,0.01, 0.015, 0.025,0.035),flag=1)
# start <-data.table(start= c(0.005,0.01, 0.015, 0.02,0.03),flag=1)
# # step <- data.table(step=c(0.001, 0.002, 0.005, 0.01, 0.02),flag=1)
# step <- data.table(step=c(0.001, 0.002),flag=1)
# params <- left_join(bars_reset,exit_points)%>%left_join(start)%>%left_join(step)

bars_reset <- data.table(bars_reset=c(24, 48, 72, 96, 120, 144, 168),flag=1)
exit_points <- data.table(exit=c(0.008, seq(0.01,0.010 ,0.01)), flag=1)
# exit_points <- data.table(exit=seq(0.01,0.1 ,0.01), flag=1)
# start <-data.table(start= c(0.005,0.01, 0.015, 0.025,0.035),flag=1)
start <-data.table(start= c(0.005,0.01, 0.015, 0.02,0.03, 0.04, 0.05),flag=1)
# step <- data.table(step=c(0.001, 0.002, 0.005, 0.01, 0.02),flag=1)
step <- data.table(step=c(0.001, 0.002, 0.005, 0.01),flag=1)
params <- left_join(bars_reset,exit_points)%>%left_join(start)%>%left_join(step)
# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

files <- grep(pattern = "Rdata", list.files( ,full.names = T) ,value=T)
i <- 1
myls <- list()
for(i in 1:length(files)){
  
  load(files[i])
  ff <- data.table(percent = unlist(res), total_bet=unlist(total_bet))
  
  ff[, returns:=total_bet*percent]
  ff[, hodl := hodl]
  ff[, date := random_date]
  ff <- cbind(ff,params)
  
  ff <- ff[total_bet!=0]
  
  setorder(ff, -returns)
  
  ff <- head(ff, 1)
  myls[[i]] <- ff
}
tt <- rbindlist(myls)
View(tt)
sum(tt$returns)
