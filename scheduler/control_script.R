# script that controls the flow

### DKPOL ###
print("# 1: update master data")
source("/home/kasper/someR/projects/folketinget/engine/get_user_info.R")
source("/home/kasper/someR/projects/folketinget/engine/get_user_info_new.R")

print("# 2: download twets based on excel list in data folder")
source("/home/kasper/someR/projects/folketinget/engine/get_twitter_user_timeline.R")

print("# 3: derive central statisctics")
source("/home/kasper/someR/projects/folketinget/engine/calculate_stats.R")

### LIGHTHOUSES ###
# 1: update master data
source("/home/kasper/someR/projects/lighthouses/engine/get_user_info.R")

# 2: download tweets based on excel list in data folder
source("/home/kasper/someR/projects/lighthouses/engine/get_twitter_user_timeline.R")

# 3: derive central statisctics from timeline
source("/home/kasper/someR/projects/lighthouses/engine/calculate_stats.R")

# 4: derive central stats from master data
#source("/home/kasper/someR/projects/lighthouses/engine/calculate_stats.R")

