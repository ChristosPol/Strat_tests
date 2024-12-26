rm(list=ls())
gc()
# Source functions
library(profvis)
library(runner)
library(Rfast)
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
library(stringr)
# .rs.restartR()
url <- paste0("https://api.kraken.com/0/public/AssetPairs")
tb <- jsonlite::fromJSON(url)

margin_pairs <- unlist(lapply(lapply(tb$result, "[[", "leverage_buy"), function(x) length(x)>0))
margin_pairs <- names(margin_pairs[margin_pairs == T])
margin_pairs <- margin_pairs[str_sub(margin_pairs,start = -3) == "USD"]

# [1] "AAVEUSD"   "ADAUSD"    "ALGOUSD"   "APEUSD"    "APTUSD"    "ARBUSD"    "ATOMUSD"  
# [8] "AVAXUSD"   "AXSUSD"    "BATUSD"    "BCHUSD"    "BLURUSD"   "BONKUSD"   "CHZUSD"   
# [15] "COMPUSD"   "CRVUSD"    "DAIUSD"    "DASHUSD"   "DOTUSD"    "EIGENUSD"  "EOSUSD"   
# [22] "FILUSD"    "FLOWUSD"   "FLRUSD"    "FTMUSD"    "GALAUSD"   "GRTUSD"    "ICPUSD"   
# [29] "INJUSD"    "JUPUSD"    "KAVAUSD"   "KEEPUSD"   "KSMUSD"    "LDOUSD"    "LINKUSD"  
# [36] "LRCUSD"    "MANAUSD"   "MEWUSD"    "MINAUSD"   "NANOUSD"   "NEARUSD"   "OMGUSD"   
# [43] "OPUSD"     "PAXGUSD"   "PEPEUSD"   "POLUSD"    "POPCATUSD" "RENDERUSD" "RUNEUSD"  
# [50] "SANDUSD"   "SCUSD"     "SEIUSD"    "SHIBUSD"   "SNXUSD"    "SOLUSD"    "STRKUSD"  
# [57] "STXUSD"    "SUIUSD"    "TAOUSD"    "TIAUSD"    "TONUSD"    "TRXUSD"    "TURBOUSD" 
# [64] "UNIUSD"    "USDCUSD"   "USDTZUSD"  "WIFUSD"    "XDGUSD"    "XETCZUSD"  "XETHZUSD" 
# [71] "XLTCZUS

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Choose which unix time to use for pulling data
# Choose from ["start_of_time", "manually", "latest_available"]
unix_time <- "manually"

# Choose any pair to pull
pair <- "CRVUSD"
# pair <- "SHIBEUR"
# Path to save results
data_path <- "Data"

# Create pair directory
dir.create(paste(data_path, pair, sep ="/"), showWarnings = T)

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

# Select initial id based on unix_time arg
initial_id <- select_period(unix_time,  diff_time = 100)

# Pull historical trades since initial id from epoch time
hist_trades_pair(sleep = 1, hist_id = initial_id, pair = pair)
