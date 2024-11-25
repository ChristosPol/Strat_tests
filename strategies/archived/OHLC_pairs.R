rm(list = ls())
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
interval <- 60
wick_dive <- 10
each_usd <- 5
n_orders <- 8 
grid <- c(0.025,0.05,0.075,0.1, 0.125, 0.15, 0.2, 0.25)

url <- paste0("https://api.kraken.com/0/public/AssetPairs")
tb <- jsonlite::fromJSON(url)
all_pairs <- names(tb$result)
all_pairs <- data.table(PAIR = all_pairs, CUR=str_sub(all_pairs,start = -3))
all_pairs <- all_pairs[CUR%in%c("USD")]
i <- 1
df <- list()
for (i in 1:length(all_pairs$PAIR)){
  
    df[[i]] <- simple_OHLC(interval = interval, pair = all_pairs$PAIR[i])
    df[[i]][, pair := all_pairs$PAIR[i]]
  Sys.sleep(1)
  print(i)
}

all_ohlc <- rbindlist(df)
all_ohlc[, candle := ifelse(close>open, "green", "red")]
all_ohlc[is.na(candle), candle := "black"]

all_ohlc[candle == "green", wick_diff := open-low]
all_ohlc[candle == "red", wick_diff := close-low]
all_ohlc[candle == "green", wick_diff_per := round((wick_diff/open)*100, 3)]
all_ohlc[candle == "red", wick_diff_per := round((wick_diff/close)*100, 3)]
# boxplot(all_ohlc$wick_diff_per)
# quantile(all_ohlc$wick_diff_per)
# table(all_ohlc$wick_diff_per>10)
all_ohlc[wick_diff_per >=wick_dive, flag_enter := TRUE]
all_ohlc[, vol_usd := volume*close]
all_ohlc[flag_enter == T, .(pair, wick_diff_per)]
nrow(all_ohlc[flag_enter== T])/nrow(all_ohlc)*100
