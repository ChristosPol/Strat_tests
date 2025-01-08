# Description ------------------------------------------------------------------
# Scheduler for trading at the end of each candle and not intra-candle
library(cronR)

# Path of live trading Rscript
path <- "/Users/christos.polysopoulos/Repositories/QFL_Bot/Code/02_Send_sell_orders.R"

# Command
cmd <- cron_rscript(path)

# add frequency and intervals
cron_add(cmd, frequency = 'hourly', id = 'Live trading', description = 'Live trading')

# cron_add(cmd,  frequency = '*/15 * * * *', id = 'Live trading', description = 'Live trading')
# Check all jobs
cron_ls()

# Stop Job
cron_clear(ask = FALSE)
cron_rm(id = "Live trading")