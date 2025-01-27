# Description ------------------------------------------------------------------
# Scheduler for trading at the end of each candle and not intra-candle
library(cronR)

# Path of live trading Rscript
path <- "/Users/christospolysopoulos/Repositories/Private/Strat_tests/strategies/qfl/bot/01_setup_grid.R"

# Command
cmd <- cron_rscript(path)

# add frequency and intervals
# cron_add(command = cmd, frequency = 'custom', at = '30 15 */3 * *', id = 'setup_grid', description = 'Live trading every 72 hours at 3:30 PM')

cron_add(command=cmd, frequency='minutely', id='setup_grid', description='Live trading every 5 minutes')


# Check all jobs
cron_ls()

# Stop Job
cron_clear(ask = FALSE)
cron_rm(id = "setup_grid")

getwd()
