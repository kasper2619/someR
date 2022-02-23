# libs
library(cronR)


f <- system.file(package = "cronR", "extdata", "get_twitter_user_timeline.R")

f <- system.file(package = "cronR", "extdata", "helloworld.R")



cmd <- cron_rscript(f)
cron_add(cmd, frequency = 'daily', id = 'job7', at = '15:18', days_of_week = c(1,2,3,4,5,6,7))
cron_ls()


cron_clear(ask = TRUE, user = "")
