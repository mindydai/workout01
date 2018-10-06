##################################################
## title: NBA players and their data
## description: Creates csv data file containing required variables
## inputs: 
## outputs:
##################################################

library(readr)
library(dplyr)
nba <- read_csv("/Users/mindydai/hw-stat133/workout1/data/nba2018.csv", 
                col_names=TRUE)
nba$experience <- replace(nba$experience, nba$experience == 'R', 0)
nba$experience <- as.integer(nba$experience)
nba$salary <- nba$salary / 1000000
nba$position <- factor(nba$position)
levels(nba$position) <- c('center', 'power_fwd', 'point_guard', 
                  'small_fwd', 'shoot_guard')
nba <- mutate(nba, missed_fg = nba$field_goals_atts - nba$field_goals, missed_ft = nba$points1_atts - nba$points1, rebounds = nba$off_rebounds + nba$def_rebounds)
efficiency <- (nba$points + nba$rebounds + nba$assists + nba$steals + nba$blocks - nba$missed_fg - nba$missed_ft - nba$turnovers) / nba$games
nba <- mutate(nba, efficiency = efficiency)

sink("../output/efficiency-summary.txt")
y <- summary(nba$efficiency)
sink()



