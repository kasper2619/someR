# libs
library(cronR)

cmd <- cron_rscript("/home/kasper/someR/projects/folketinget/engine/get_twitter_user_timeline.R")
cron_add(cmd, frequency = 'daily', id = 'job10', at = '23:59', days_of_week = c(1,2,3,4,5,6,7))
cron_ls()
cron_clear()

# this can be
