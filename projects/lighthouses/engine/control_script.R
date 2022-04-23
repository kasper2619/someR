# script that controls the flow

# 1: update master data
source("/home/kasper/someR/projects/lighthouses/engine/get_user_info.R")

# 2: download tweets based on excel list in data folder
source("/home/kasper/someR/projects/lighthouses/engine/get_twitter_user_timeline.R")

# 3: derive central statisctics
source("/home/kasper/someR/projects/lighthouses/engine/calculate_stats.R")


