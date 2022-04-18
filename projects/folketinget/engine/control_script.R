# script that controls the flow

# 1: download twets based on excel list in data folder
source("/home/kasper/someR/projects/folketinget/engine/get_twitter_user_timeline.R")

# 2: derive central statisctics
source("/home/kasper/someR/projects/folketinget/engine/calculate_stats.R")
