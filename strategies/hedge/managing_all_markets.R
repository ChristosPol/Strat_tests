csv_path <- paste0("/Users/christospolysopoulos/Repositories/Private/QFL_Act/Data/trading_table.csv")
orders <- read_csv(csv_path,col_types = cols())
summ <- orders %>% group_by(PAIR, STATUS_BUY) %>% summarise(n=n())
summ <- summ %>% group_by(PAIR)%>% mutate(tot = sum(n))
av <- summ%>% mutate(per = n/tot*100)%>%filter(STATUS_BUY =="CLOSED")%>% select(per)
mean(av$per)



init <- 7000
target_profit <- 7000
target_per <- target_profit/init
for_one_pair <- target_per/(8*0.3)





each_bet <- data.frame(bet=seq(5, 40, 5), flag=1)
enter_rate <- data.frame(rate=c(0.3), flag=1) # for example
n_bets <- data.frame(nbets=seq(5, 30, by=1), flag=1)
sim <- left_join(each_bet, enter_rate)%>%left_join(n_bets)
i <- 6
res <- c()
for(i in 1:nrow(sim)){
  pairs <- init/(sim$bet[i]*sim$nbets[i])
  total_bets <- sim$nbets[i]*pairs
  sucessful <- total_bets*sim$rate[i]
  res[i] <- target_profit/(sucessful*sim$bet[i]) 
  print(i)
}

plot(res)

sim$res <- res
View(sim)



init <- 7000


target_profit <- data.frame(target=seq(500, 7000,500), flag=1)
# enter_rate <- data.frame(rate=seq(0.1, 0.9, 0.1), flag=1) # for example
n_bets <- data.frame(nbets=seq(8, 30, by=1), flag=1)
amount_bets <- data.frame(amount=seq(5, 30, by=5), flag=1)

sim <- left_join(target_profit, n_bets)%>%left_join(amount_bets)
#%>%left_join(enter_rate)
sim$amount_per_pair <- sim$nbets*sim$amount
sim$avail_pairs <- floor(init/sim$amount_per_pair)
i <- 156
for_the_pair_USD <- c()
for_the_pair_per <- c()
for_the_bet <- c()
res <- c()
for(i in 1:nrow(sim)){
  target <- sim$target[i]
  for_the_pair_USD[i] <- target/(sim$avail_pairs[i])
  for_the_pair_per[i] <- (for_the_pair_USD[i]) /target
  # for_the_bet[i] <- for_the_pair[i]/sim$nbets[i]
}
sim$for_the_pair_USD <- for_the_pair_USD
sim$for_the_pair_per <- for_the_pair_per

# sim$for_the_bet <- for_the_bet*100
View(sim)


