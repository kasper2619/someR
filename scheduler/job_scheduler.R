# libs
library(cronR)

cmd <- cron_rscript("/home/kasper/someR/scheduler/control_script.R")
#cron_add(cmd, frequency = '0 */1 * * *', id = 'job1', description = 'Every 1st hour')

#cron_add(cmd, frequency = 'daily', id = 'Twitter 6:00', at = '04:30', days_of_week = c(1,2,3,4,5,6,7))
cron_add(cmd, frequency = 'daily', id = 'Twitter 7:00', at = '05:30', days_of_week = c(1,2,3,4,5,6,7))
#cron_add(cmd, frequency = 'daily', id = 'Twitter 8:00', at = '06:30', days_of_week = c(1,2,3,4,5,6,7))
#cron_add(cmd, frequency = 'daily', id = 'Twitter 9:00', at = '07:30', days_of_week = c(1,2,3,4,5,6,7))
#cron_add(cmd, frequency = 'daily', id = 'Twitter 10:00', at = '08:30', days_of_week = c(1,2,3,4,5,6,7))
cron_add(cmd, frequency = 'daily', id = 'Twitter 11:00', at = '09:30', days_of_week = c(1,2,3,4,5,6,7))
#cron_add(cmd, frequency = 'daily', id = 'Twitter 12:00', at = '10:30', days_of_week = c(1,2,3,4,5,6,7))

cron_add(cmd, frequency = 'daily', id = 'Twitter 16:00', at = '14:30', days_of_week = c(1,2,3,4,5,6,7))
#cron_add(cmd, frequency = 'daily', id = 'Twitter 17:00', at = '15:30', days_of_week = c(1,2,3,4,5,6,7))
#cron_add(cmd, frequency = 'daily', id = 'Twitter 18:00', at = '16:30', days_of_week = c(1,2,3,4,5,6,7))
#cron_add(cmd, frequency = 'daily', id = 'Twitter 19:00', at = '17:30', days_of_week = c(1,2,3,4,5,6,7))
cron_add(cmd, frequency = 'daily', id = 'Twitter 20:00', at = '18:30', days_of_week = c(1,2,3,4,5,6,7))
#cron_add(cmd, frequency = 'daily', id = 'Twitter 21:00', at = '19:30', days_of_week = c(1,2,3,4,5,6,7))
#cron_add(cmd, frequency = 'daily', id = 'Twitter 22:00', at = '20:00', days_of_week = c(1,2,3,4,5,6,7))
#cron_add(cmd, frequency = 'daily', id = 'Twitter 23:00', at = '21:30', days_of_week = c(1,2,3,4,5,6,7))
#cron_add(cmd, frequency = 'daily', id = 'Twitter 00:00', at = '22:30', days_of_week = c(1,2,3,4,5,6,7))

cron_ls()
cron_clear()

Sys.time()

# this can be
