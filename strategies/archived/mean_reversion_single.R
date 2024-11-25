rm(list=ls())

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
pair <- "BTCUSD"
# pair <- "SHIBEUR"
# Path to save results
data_path <- "Code/Parameter_optim/Data"

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")


ticks <- c(5)
units <- c("minutes")
intervals <- paste(ticks, units, sep = " ")


df <- trades_to_OHLC(pair = pair,
                     interval = intervals,
                     from_date = "2024-08-01",
                     to_date = "2024-09-01",
                     date_subset = T)
df1 <- df[[1]]
candles(df1)


bb <- as.data.frame(TTR:::BBands(HLC = df1[, .(high, low, close)], n=200, maType=EMA, sd=4)) 
setDT(bb)
df1 <- cbind(df1, bb[, .(dn, up)])
df1[, EMA:= EMA(close, 200)]

df1[close>EMA & close>up, position := "short"]
df1[close<EMA & close<dn, position := "long"]


candles(df1)+
  geom_line(data=df1, aes(x=interval, y= dn), colour="blue")+
  geom_line(data=df1, aes(x=interval, y= up), colour="blue")+
  geom_line(data=df1, aes(x=interval, y= EMA), colour="black")+
  geom_point(data=df1[position=="long"], aes(x=interval, y= close), colour="green")+
  geom_point(data=df1[position=="short"], aes(x=interval, y= close), colour="red")

