rm(list=ls())
gc()
# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

load("~/Repositories/Private/Strat_tests/metrics_13Jan25.Rdata")
selected

res <- list()
i <- 5
for(i in 1:length(selected)){
  test <- fund_list_pair_tt[pair %in%selected[1:i]]  
  exceeded_funds_bool <- test[, list(sum_funds=sum(funds_pair)), by = list(interval_enter, param_concatenated)]
  exceeded_funds_bool[, exceeded_funds := any(sum_funds<(-funds)), by = param_concatenated]
  exceeded_funds_bool <- unique(exceeded_funds_bool[, .(param_concatenated, exceeded_funds)])
  tmp <- metrics_pair[pair %in% selected[1:i], .(param_concatenated, percent, mean_hodl)]
  tmp <- tmp[, list(mean_hodl = mean(mean_hodl), sum_percent = sum(percent)), by = param_concatenated]
  exceeded_funds_bool <- merge(exceeded_funds_bool,tmp , by = "param_concatenated", all.x = T)
  exceeded_funds_bool[, n_pairs := length(selected[1:i])]
  res[[i]] <- copy(exceeded_funds_bool)
  rm(exceeded_funds_bool)
  rm(tmp)
}

i <- 2
binded <- rbindlist(res)
View(binded)
