rm(list=ls())
gc()
# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# load("~/Repositories/Private/Strat_tests/results/60_minutes_XXBTZUSD_break_out_v01_BTC_more_params.Rdata")
files <- list.files("~/Repositories/Private/Strat_tests/results/results_break/", full.names = T)
names <- gsub("60_minutes_|.Rdata|_break_out_v01","", list.files("~/Repositories/Private/Strat_tests/results/results_break"))

i <- 1
res <- list()
for (i in 1:length(files)){
  load(files[i])
  res[[i]] <- copy(results_exp)
  
  # res[[i]] <- res[[i]][order(-percent)][, head(.SD, 50), by = day]
  rm(results_exp)
} 
library(viridis)
results <- rbindlist(res)

View(results)

results <- results_exp

ggplot(data=results, aes(x = sum_per, y =win_rate, colour = pair))+
  geom_point(alpha=0.5, size =2)+
  geom_hline(yintercept = 0.5)+
  geom_vline(xintercept = 0.0)+theme_bw()


ggplot(data=results[sum_per>0], aes(x = sum_per, y =win_rate, colour = pair))+
  geom_point(size =2)+
  geom_hline(yintercept = 0.5)+
  geom_vline(xintercept = 0.0)+theme_bw()

ggplot(data=results[sum_per>2 & win_rate>=0.6], aes(x = sum_per, y =win_rate, colour = pair))+
  geom_point(size =2)+
  geom_hline(yintercept = 0.5)+
  geom_vline(xintercept = 0.0)+theme_bw()

View(results[sum_per>2 & win_rate>=0.6])


ggplot(data=results[pair=="POLUSD"], aes(x = tp, y =sl))+
         geom_jitter(aes(size=sum_per))
       

p <- unique(results$pair)
pairs <- 1

pdf("output_plots.pdf", width = 8, height = 6)
for(pairs in 1:length(p)){
  
  tmp <- results[pair==p[pairs] ]
  tmp[sum_per <=0, sum_per := 0]
  
  # tmp[, category := cut_interval(sum_per, n = 3)]  # Divides into 4 equal-width intervals
  # 
  # tmp[, category := case_when(
  #   sum_per < 0 ~ "Negative",
  #   sum_per >= 0 & sum_per < 0.05 ~ "[0, 0.05]",
  #   sum_per >= 0.05 & sum_per < 0.15 ~ "[0.05, 0.15]",
  #   sum_per >= 0.15 & sum_per < 0.30 ~ "[0.15, 0.30]",
  #   sum_per >= 0.30 & sum_per < 0.50 ~ "[0.30, 0.50]",
  #   sum_per >= 0.50 & sum_per < 0.80 ~ "[0.50, 0.80]",
  #   sum_per >= 0.80 & sum_per < 1 ~ "[0.80, 1]",
  #   sum_per >= 1 & sum_per < 1.20 ~ "[1, 1.20]",
  #   sum_per >= 1.20 & sum_per < 1.50 ~ "[1.20, 1.50]",
  #   sum_per >= 1.50 & sum_per < 1.80 ~ "[1.50, 1.80]",
  #   sum_per >= 1.80 & sum_per < 2 ~ "[1.80, 2]",
  #   sum_per >= 2 ~ "More than 2"
  # )]
  
  # annot <- results[pair==p[pairs]]
  # setorder(annot, -sum_per)
  # annot <- annot[sum_per >0]
  
  
  plot_out1 <- ggplot(data=tmp, aes(x = tp, y = sl, fill = sum_per)) +
    geom_tile(colour="black")+
    # geom_tile(data = annot, aes(x = tp, y = sl), 
    #           fill = "red", alpha = 0.7, inherit.aes = FALSE)+
    facet_grid(med_num~bar+exc_num)+
    # scale_fill_brewer(palette = "RdYlBu")+
    # scale_fill_viridis_d(option = "A")+
    scale_fill_viridis(option="magma")+
    ggtitle(paste0(p[pairs], " ", "percent") ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
  print(plot_out1)
  
  plot_out2 <- ggplot(data=tmp, aes(x = tp, y = sl, fill = win_rate)) +
    geom_tile(colour="black")+
    # geom_tile(data = annot, aes(x = tp, y = sl), 
    #           fill = "red", alpha = 0.7, inherit.aes = FALSE)+
    facet_grid(med_num~bar+exc_num)+
    # scale_fill_brewer(palette = "RdYlBu")+
    # scale_fill_viridis_d(option = "A")+
    scale_fill_viridis(option="magma")+
    ggtitle(paste0(p[pairs], " ", "win_rate") ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
  print(plot_out2)
  
}
dev.off()

results[, beat_hodl := ifelse(total_percent > hodl, T, F)]

View(unique(results[, .(max(total_percent), hodl), by =pair]))

# results[is.na(quote_res), res_usd :=0]
# results[is.na(percent), percent:=0]

metrics <- results[, list(aver_percent=mean(percent),
                          mean_bet=mean(total_bet),<
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
