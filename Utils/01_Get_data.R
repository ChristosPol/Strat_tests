# Preamble ---------------------------------------------------------------------
# screen -S pullingETH R

rm(list = ls())

# .rs.restartR()
url <- paste0("https://api.kraken.com/0/public/AssetPairs")
tb <- jsonlite::fromJSON(url)

margin_pairs <- unlist(lapply(lapply(tb$result, "[[", "leverage_buy"), function(x) length(x)>0))
margin_pairs <- names(margin_pairs[margin_pairs == T])
margin_pairs <- margin_pairs[str_sub(margin_pairs,start = -3) == "USD"]

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Choose which unix time to use for pulling data
# Choose from ["start_of_time", "manually", "latest_available"]
unix_time <- "manually"

# Choose any pair to pull
pair <- "ARBUSD"
# pair <- "SHIBEUR"
# Path to save results
data_path <- "Code/Parameter_optim/Data"

# Create pair directory
dir.create(paste(data_path, pair, sep ="/"), showWarnings = T)

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

# Select initial id based on unix_time arg
initial_id <- select_period(unix_time,  diff_time = 365)

# Pull historical trades since initial id from epoch time
hist_trades_pair(sleep = 1, hist_id = initial_id, pair = pair)
