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

url <- paste0("https://api.kraken.com/0/public/AssetPairs")
tb <- jsonlite::fromJSON(url)


all_pairs <- unlist(lapply(lapply(tb$result, "[[", "altname"), function(x) length(x)>0))
all_pairs <- names(all_pairs)
all_pairs <- all_pairs[str_sub(all_pairs,start = -3) == "USD"]
all_pairs <- all_pairs[!all_pairs %in%c("DAIUSD", "JUPUSD", "PAXGUSD","USDCUSD","ZGBPZUSD","ZEURZUSD", "USDTZUSD")]

for(i in 88:length(all_pairs)){
  
  # Source functions
  path_source <- "Source"
  files.sources = list.files(path_source, full.names = T)
  sapply(files.sources, source)
  
  # Choose which unix time to use for pulling data
  # Choose from ["start_of_time", "manually", "latest_available"]
  unix_time <- "manually"
  
  # Choose any pair to pull
  pair <- all_pairs[i]
  # pair <- "SHIBEUR"
  # Path to save results
  data_path <- "Data_since_1"
  
  # Create pair directory
  dir.create(paste(data_path, pair, sep ="/"), showWarnings = T)
  
  # Fix path
  pair_data_results <- paste(data_path, pair, sep ="/")
  
  # Select initial id based on unix_time arg
  initial_id <- select_period(unix_time,  diff_time = 365*5)
  
  # Pull historical trades since initial id from epoch time
  hist_trades_pair(sleep = 1, hist_id = initial_id, pair = pair)
  
}

margin_pairs <- unlist(lapply(lapply(tb$result, "[[", "leverage_buy"), function(x) length(x)>0))
margin_pairs <- names(margin_pairs[margin_pairs == T])
margin_pairs <- margin_pairs[str_sub(margin_pairs,start = -3) == "USD"]