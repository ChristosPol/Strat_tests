gc()
# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
pair <- "SOLUSD"
# pair <- "SHIBEUR"
# Path to save results
data_path <- "Code/Parameter_optim/Data"

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

# Load data
ticks <- c(60)
units <- c("minutes")
intervals <- paste(ticks, units, sep = " ") 

df <- trades_to_OHLC(pair = pair,
                     interval = intervals,
                     from_date = "2023-03-27",
                     to_date = "2023-03-30",
                     date_subset = F)
df1 <- df[[1]]
# candles(df1)
df1[, date:=as.Date(interval)]
gc()
# Fix vector of dates
# Fix vector of dates
dates_vector <- unique(as.Date(df1$interval))
dates_vector <- sample(x = dates_vector, 300, replace = F)



# get the datasets
dataframes <- lapply(dates_vector, function(x){
  df <- df1[date >=x & date < x+(days(7))]
  return(df)
})


pdf(paste0("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Grid/results/", "weeks_simple_vis.pdf"), onefile = TRUE)
for (x in 1:length(dataframes)){
  df1 <- dataframes[[x]]
  plot <- candles(df1)
  print(plot)
}
dev.off()
