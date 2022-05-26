#this is for making table 2 in the paper (# of fish released by subregion and 
#hatchery- average and range)

library(dplyr)

#first get the total released by each hatchery in each year, because hatcheries
#made multiple releases within a year in many cases
Releas_by_yr <- H_releas_num_RAWdat %>%
  group_by(YearReleased, LocationFacilityOrWildStock) %>%
  summarise(Sum = sum(TotalReleased))
View(Releas_by_yr)

#then summarise
Releases_by_H <- Releas_by_yr %>%
  group_by(LocationFacilityOrWildStock) %>%
  summarise(Mean = mean(Sum), Min = min(Sum), Max = max(Sum))
View(Releases_by_H)
b <- Releases_by_H[ , c(2:4)]/1000000
Releases_by_H <- cbind.data.frame(Releases_by_H[ , 1], b)

setwd("~/Documents/CHUM THESIS/Manuscript/Figures/Table_2")
write.csv(Releases_by_H, "Releases_by_H.csv")
