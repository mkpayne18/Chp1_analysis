#-------------------------------------------------------------------------------
# Project title: Revealing stream attractiveness to stray hatchery-origin chum
# salmon in Southeast Alaska (MS thesis chapter 1)

# Molly K Payne

# Purpose of this script: Create tables for thesis/manuscript

# Last updated: May 23, 2022
#-------------------------------------------------------------------------------
require(tidyverse)
require(tidyr)

#source("scripts/04_model_diagnostics.R") #NOTE that this will take several min-
#utes to run. If you don't have it loaded already, you can instead just load the
#needed objects from previous scripts:
mean_bm1_pred <- readRDS("output/mean_pred_and_obs_vals.rds")
bm1_pred <- readRDS("output/pred_and_obs_vals_unaveraged.rds")
stray_dat_scaled <- readRDS("output/stray_dat_scaled.rds")
bm1 <- readRDS("output/best_model.rds")




#1. Predicted, observed vals and number of surveys table =======================
#This is currently table 2 in the manuscript
tab2 <- stray_dat_scaled %>% group_by(StreamName) %>%
  summarise(across(Avg_number_strays, c(min,max)), across(Number_surveys, sum))

tab2a <- left_join(tab2, mean_bm1_pred)

#Add number of years surveyed column
yrs_surveyed <- stray_dat_scaled %>% group_by(StreamName) %>%
  summarise(Number_yrs_surveyed = n())
tab2b <- left_join(tab2a, yrs_surveyed)


#Add average chum salmon abundance from dead counts (natural-origin + stray):
survey_data8 <- readRDS("output/survey_data8.rds") #from dead_count_analysis.R
#first find mean by year
avg_dead <- survey_data8 %>% group_by(StreamName, Year) %>%
  summarise(Average_dead_count = mean(DeadCount))
#now find averages overall for each stream
avg_dead_by_stream <- avg_dead %>% group_by(StreamName) %>%
  summarise(Average_dead = mean(Average_dead_count))
tab2c <- left_join(tab2b, avg_dead_by_stream, by = "StreamName")


#Tailor the df:
tab2c <- tab2c[,c(1,5,6,2,3,8,4,7)]
name_update <- c("Mean predicted attractiveness index",
                 "Mean observed attractiveness index",
                 "Minimum observed number of strays", "Maximum observed number of strays",
                 "Average abundance", "Total number of surveys",
                 "Number of years surveyed")
tab2c <- tab2c %>% rename_at(2:8, ~name_update)
head(tab2c) #there are many decimal places! Cut those down
tab2c[,c(2,3,6)] <- sapply(tab2c[,c(2,3,6)], function(x) round(x, 1))


#Export
# write.csv(tab2c, "figs/tab2_pred_obs.csv")


#Note that there are other tables included in the manuscript, however, those
#tables are easily made just by typing in the values, rather than creating a .csv
#file within R






