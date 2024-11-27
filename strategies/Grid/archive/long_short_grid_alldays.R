rm(list=ls())

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
ticks <- c(5)
units <- c("minutes")
intervals <- paste(ticks, units, sep = " ")

df <- trades_to_OHLC(pair = pair,
                     interval = intervals,
                     from_date = "2023-03-27",
                     to_date = "2023-03-30",
                     date_subset = F)
df1 <- df[[1]]
# Fix vector of dates
dates_vector <- unique(as.Date(df1$interval))

dates_vector <- dates_vector[-1] 

# Set parameters table
bars_reset <- data.table(bars_reset=c(96, 192),flag=1)
exit_points <- data.table(exit=c(0.008, seq(0.01,0.02 ,0.01)), flag=1)
start <-data.table(start= c(0.005,0.01, 0.015, 0.02),flag=1)
maxim <- data.table(maxim = c(0.06),flag=1)
n_trades <- data.table(n_trades=seq(10, 40, 10),flag=1)
params <- left_join(bars_reset,exit_points)%>%left_join(start)%>%left_join(maxim)%>%left_join(n_trades)

# For trade Ids
all_chars <- c(LETTERS, 0:9)
str_len <- 20

bet <- 5
day_results <- list()
x <- 1
for (x in 1:length(dates_vector)){
  df <- trades_to_OHLC(pair = pair,
                       interval = intervals,
                       from_date = dates_vector[x],
                       to_date = dates_vector[x],
                       date_subset = T)
  df1 <- df[[1]]
  
  res <- list()
  total_bet <- list()
  h <- 55
  for (h in 1:nrow(params)){
    first_price <-  df1[1, close]
    step <- (params$maxim[h] - params$start[h])/(params$n_trades[h])
    
    # Calculate short grid
    short_grid <- data.table(batch = sample(x = 1:10E5,1),
                             grid = first_price*(1+seq(params$start[h], params$maxim[h], by=step)),
                             init_price = first_price,
                             status_enter="open",
                             status_exit = "open")
    short_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
    short_grid[, interval_enter :=POSIXct()]
    short_grid[, interval_exit :=POSIXct()]
    short_grid[, position := "short"]
    short_grid[, exits:=min(short_grid[, grid])*(1-params$exit[h])]
    setorder(short_grid, -grid)
    
    # Calculate long grid
    long_grid <- data.table(batch = sample(x = 1:10E5,1),
                            grid = first_price*(1-seq(params$start[h], params$maxim[h], by=step)),
                            init_price = first_price,
                            status_enter="open",
                            status_exit = "open")
    long_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
    long_grid[, interval_enter :=POSIXct()]
    long_grid[, interval_exit :=POSIXct()]
    long_grid[, position := "long"]
    long_grid[, exits:=max(short_grid[, grid])*(1+params$exit[h])]
    setorder(long_grid, -grid)
    
    grid <- rbind(short_grid, long_grid)
    
    
    counter <- 0
    i <- 2
    save_batch <- list()
    for (i in 2:nrow(df1)){
      
      low <- df1[i, low]
      high <- df1[i, high]
      int <- df1[i, interval]
      close <-df1[i, close]
      grid[, current:=close]
      current <- unique(grid$current)
      counter <- counter+1
      
      # update grid after x bars
      
      if(counter > params$bars_reset[h]){
        
        counter <- 0
        save_batch <- append(save_batch, list(grid))
        to_add <- grid[status_enter =="closed" & status_exit =="open"]
        
        # Calculate short grid
        short_grid <- data.table(batch = sample(x = 1:10E5,1),
                                 grid = current*(1+seq(params$start[h], params$maxim[h], by=step)),
                                 init_price = first_price,
                                 status_enter="open",
                                 status_exit = "open")
        short_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
        short_grid[, interval_enter :=POSIXct()]
        short_grid[, interval_exit :=POSIXct()]
        short_grid[, position := "short"]
        short_grid[, exits:=min(short_grid[, grid])*(1-params$exit[h])]
        short_grid[, current:=close]
        setorder(short_grid, -grid)
        
        # Calculate long grid
        long_grid <- data.table(batch = sample(x = 1:10E5,1),
                                grid = current*(1-seq(params$start[h], params$maxim[h], by=step)),
                                init_price = first_price,
                                status_enter="open",
                                status_exit = "open")
        long_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
        long_grid[, interval_enter :=POSIXct()]
        long_grid[, interval_exit :=POSIXct()]
        long_grid[, position := "long"]
        long_grid[, exits:=max(short_grid[, grid])*(1+params$exit[h])]
        long_grid[, current:=close]
        setorder(long_grid, -grid)
        
        grid <- rbind(short_grid, long_grid)
        grid <- rbind(grid, to_add)
      }
      
      
      
      # Enter long
      if (any(low <grid[status_enter=="open" & position=="long", grid])  ){
        grid[status_enter=="open" & current < grid& position=="long", `:=`(status_enter ="closed")]
        if( any(grid[status_enter=="closed"& position=="long",   is.na(interval_enter)])  ){
          grid[status_enter=="closed" & is.na(interval_enter)& position=="long", interval_enter:=int]
        }
      }
      
      # Exit long
      if (any(high >grid[status_enter=="closed" & position == "long", exits])  ){
        grid[status_enter=="closed" & high >exits& position == "long", `:=`(status_exit ="closed")]
        if(any(grid[status_exit=="closed"& position == "long",   is.na(interval_exit)])  ){
          grid[status_exit=="closed" & is.na(interval_exit)& position == "long", interval_exit:=int]
        }
        
      }
      
      # Enter short
      if (any(high <grid[status_enter=="open" & position=="short", grid])  ){
        grid[status_enter=="open" & high > grid & position=="short", `:=`(status_enter ="closed")]
        if( any(grid[status_enter=="closed"& position=="short",   is.na(interval_enter)])  ){
          grid[status_enter=="closed" & is.na(interval_enter) & position=="short", interval_enter:=int]
        }
      }
      
      # Exit short
      if (any(low <grid[status_enter=="closed" & position == "short", exits])  ){
        grid[status_enter=="closed" & low <exits& position == "short", `:=`(status_exit ="closed")]
        if(any(grid[status_exit=="closed"& position == "short",   is.na(interval_exit)])  ){
          grid[status_exit=="closed" & is.na(interval_exit)& position == "short", interval_exit:=int]
        }
        
      }
      
    }
    
    d <- rbindlist(save_batch)
    dd <- d[, tail(.SD, 1L),  by = c("batch", "trade_id")]
    dd[, final_exit := ifelse(status_enter =="closed" & status_exit =="closed", exits, tail(df1[, close], 1))]
    dd <- dd[status_enter!="open"]
    dd[, percent := (final_exit-grid)/grid]
    dd[position == "short", percent:=-1*percent]
    dd[, quote_res_no_fees := bet+bet*percent]
    dd[, quote_res_clean := quote_res_no_fees-(0.65/100)*quote_res_no_fees]
    dd[, bet := bet]
    
    res[[h]] <-(sum(dd$quote_res_clean)-sum(dd$bet))/(sum(dd$bet))
    total_bet[[h]] <- sum(dd$bet)
    print(paste0("Entering with ", total_bet[[h]], " USD", ", Returns: ", res[[h]], " USD returns: ", res[[h]]*total_bet[[h]]))
    print(h)
    
    day_res <- cbind(params, percent=unlist(res), tot_bet=unlist(total_bet))
    day_res[, date:=dates_vector[x]]
    
  }
  hodl <- (tail(df1[, close], 1)-head(df1[, close], 1))/head(df1[, close], 1)
  day_res[, hodl := hodl]
  day_results[[x]] <- day_res
  print(x)
}

# Save
save(day_results, file=paste0("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Grid/results/", paste0(ticks,"_", units,"_", pair), ".Rdata"))

