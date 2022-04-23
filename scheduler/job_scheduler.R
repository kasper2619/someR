# libs
library(cronR)

cmd <- cron_rscript("/home/kasper/someR/scheduler/control_script.R")
cron_add(cmd, frequency = '0 */6 * * *', id = 'job1', description = 'Every 6th hour')

#cron_add(cmd, frequency = 'daily', id = 'job12', at = '09:37', days_of_week = c(1,2,3,4,5,6,7))
cron_ls()
cron_clear()

Sys.time()

# this can be
