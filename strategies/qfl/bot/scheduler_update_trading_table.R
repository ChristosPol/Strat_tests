# Description ------------------------------------------------------------------
# Scheduler for trading at the end of each candle and not intra-candle
library(cronR)

# Path of live trading Rscript
path <- ""

# Command
cmd <- cron_rscript(path)

# add frequency and intervals
cron_add(cmd, frequency = 'hourly', id = 'update_table', description = 'Updates trading table')

# Check all jobs
cron_ls()

# Stop Job
cron_clear(ask = FALSE)
cron_rm(id = "update_table")