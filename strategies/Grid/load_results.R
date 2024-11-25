rm(list=ls())
gc()
# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)


# load("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Grid/results/60_minutes/")
files <- list.files("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Grid/results/60_minutes/", full.names = T)
names <- gsub("60_minutes_|.Rdata","", list.files("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Grid/results/60_minutes/"))
i <- 1
res <- list()
for (i in 1:length(files)){
  load(files[i])
  res[[i]] <- rbindlist(daily_res)
  res[[i]][ , concatenated := paste(splits_reset,exit,start,maxim,sl,n_trades , sep = "_")]
  res[[i]][, res_usd := quote_res-total_bet]
  res[[i]][is.nan(res_usd), res_usd := 0]
  res[[i]][is.nan(percent), percent := 0]
  res[[i]][, coin := names[i]]
  # res[[i]] <- res[[i]][order(-percent)][, head(.SD, 50), by = day]
  rm(daily_res)
} 

results <- rbindlist(res)
results[, beat_hodl := ifelse(percent > hodl, T, F)]
results[is.na(res_usd), res_usd :=0]
results[is.na(percent), percent:=0]

metrics <- results[, list(aver_percent=mean(percent),
                          mean_bet=mean(total_bet),
                          mean_aver_quote=mean(res_usd),
                          sum_bet = sum(total_bet),
                          sum_quote = sum(res_usd),
                          entered = sum(total_bet>0),
                          positive_percent = sum(percent>0),
                          beat_hodl = sum(beat_hodl, na.rm = T)), by=.(concatenated, coin)]
setorder(metrics, -aver_percent)

View(results[concatenated == "1_0.2_0.02_0.2_0.06_20"])

sort(table(res[[10]]$exit))
sort(table(res[[10]]$start))
sort(table(res[[10]]$maxim))
sort(table(res[[2]]$n_trades))
x <- 1
dd <- res[[10]]
metr <- "percent"
p1 <- ggplot(data=dd, aes(x = day, y= get(metr), colour = maxim))+
  geom_point(size =3)
p1
p2 <- ggplot(data=dd, aes(x = day, y= get(metr), colour = n_trades))+
  geom_point(size =3)
p2
p3 <- ggplot(data=dd, aes(x = day, y= get(metr), colour = start))+
  geom_point(size =3)
p3
p4 <- ggplot(data=dd, aes(x = day, y= get(metr), colour = exit))+
  geom_point(size =3)
p4
p5 <- ggplot(data=dd, aes(x = day, y= get(metr), colour = sl))+
  geom_point(size =3)
p5

metrics <- list()


ggplot(data=res[[1]], aes(x = day, y= res_usd, colour = maxim))+
  geom_point(size =3)

# Need to adjust for how much hodl percentage are the returns, returns need to be relevant, related
for(x in 1:length(res)){
  
  d <- copy(res[[x]])
  d[is.nan(percent),  percent:=0]
  d[is.nan(total_bet),  total_bet:=0]
  d[is.nan(quote_res),  quote_res:=0]
  d[ , concatenated := paste(splits_reset,exit,start,maxim,sl,n_trades , sep = "_")]
  setorder(d, concatenated,day)
  data_unique <- d[ , .SD[1], by = .(concatenated, day)]
  
  
  
  metrics[[x]] <- data_unique[,   list(aver_percent=mean(percent),
                                  mean_bet=mean(total_bet),
                                  mean_aver_quote=mean(quote_res-total_bet),
                                  sum_bet = sum(total_bet),
                                  sum_quote = sum(quote_res-total_bet),
                                  entered = sum(total_bet>0),
                                  positive_percent = sum(percent>0)), by=.(concatenated)]
  

  }

ggplot(data=metrics, aes(x=aver_percent, y= mean_aver_quote))+
  geom_point(colour ="darkred")
