#Append fishery harvest data to master dataset
SEAK_Fishery_Harvest_Data <-
  read_csv("~/Documents/CHUM THESIS/Data Sources/Fishery_Harvest_Data/SEAK_Fishery_Harvest_Data.csv", 
           +     col_types = cols(Subregion = col_skip()))
harv1 <- SEAK_Fishery_Harvest_Data %>% pivot_longer(cols = c(X2008, X2009, X2010, X2011, X2013,
                                                             X2014, X2015, X2017, X2018, X2019))
View(harv1)
harv1$name[harv1$name == "X2008"] <- 2008
harv1$name[harv1$name == "X2009"] <- 2009
harv1$name[harv1$name == "X2010"] <- 2010
harv1$name[harv1$name == "X2011"] <- 2011
harv1$name[harv1$name == "X2013"] <- 2013
harv1$name[harv1$name == "X2014"] <- 2014
harv1$name[harv1$name == "X2015"] <- 2015
harv1$name[harv1$name == "X2017"] <- 2017
harv1$name[harv1$name == "X2018"] <- 2018
harv1$name[harv1$name == "X2019"] <- 2019
harv1 <- harv1 %>% rename(Year = name)
harv1 <- harv1 %>% rename(Fishery_harvest = value)
Master_dataset$Year <- sapply(Master_dataset$Year, as.character)
Master_dataset <- left_join(Master_dataset, harv1, by = c("StreamName", "Year"))
View(Master_dataset)

#Append conspecific density and abundance to the master dataset:
#Abundance
Conspecific_abund <-
  read_csv("~/Documents/CHUM THESIS/Data Sources/Cons_Dens&Abundance/Conspecific_abund.csv")
Ab1 <- Conspecific_abund %>% pivot_longer(cols = c(X2008, X2009, X2010, X2011, X2013,
                                     X2014, X2015, X2017, X2018, X2019))
View(Ab1)
Ab1$name[Ab1$name == "X2008"] <- 2008
Ab1$name[Ab1$name == "X2009"] <- 2009
Ab1$name[Ab1$name == "X2010"] <- 2010
Ab1$name[Ab1$name == "X2011"] <- 2011
Ab1$name[Ab1$name == "X2013"] <- 2013
Ab1$name[Ab1$name == "X2014"] <- 2014
Ab1$name[Ab1$name == "X2015"] <- 2015
Ab1$name[Ab1$name == "X2017"] <- 2017
Ab1$name[Ab1$name == "X2018"] <- 2018
Ab1$name[Ab1$name == "X2019"] <- 2019
Ab1 <- Ab1 %>% rename(Year = name)
Ab1 <- Ab1 %>% rename(Cons_Abundance = value)
Master_dataset$Year <- sapply(Master_dataset$Year, as.character)
Master_dataset <- left_join(Master_dataset, Ab1, by = c("StreamName", "Year"))
View(Master_dataset)

#density
Conspecifc_dens <-
  read_csv("~/Documents/CHUM THESIS/Data Sources/Cons_Dens&Abundance/Conspecifc_dens.csv")
Den1 <- Conspecifc_dens %>% pivot_longer(cols = c(X2008, X2009, X2010, X2011, X2013,
                                                 X2014, X2015, X2017, X2018, X2019))
View(Den1)
Den1$name[Den1$name == "X2008"] <- 2008
Den1$name[Den1$name == "X2009"] <- 2009
Den1$name[Den1$name == "X2010"] <- 2010
Den1$name[Den1$name == "X2011"] <- 2011
Den1$name[Den1$name == "X2013"] <- 2013
Den1$name[Den1$name == "X2014"] <- 2014
Den1$name[Den1$name == "X2015"] <- 2015
Den1$name[Den1$name == "X2017"] <- 2017
Den1$name[Den1$name == "X2018"] <- 2018
Den1$name[Den1$name == "X2019"] <- 2019
Den1 <- Den1 %>% rename(Year = name)
Den1 <- Den1 %>% rename(Cons_Density = value)
Master_dataset$Year <- sapply(Master_dataset$Year, as.character)
Master_dataset <- left_join(Master_dataset, Den1, by = c("StreamName", "Year"))
View(Master_dataset)

#Append pink density and abundance to the master dataset:
#Abundance
Ab2 <- Pink_abundance %>% pivot_longer(cols = c(X2008, X2009, X2010, X2011, X2013,
                                                X2014, X2015, X2017, X2018, X2019))
View(Ab2)
Ab2$name[Ab2$name == "X2008"] <- 2008
Ab2$name[Ab2$name == "X2009"] <- 2009
Ab2$name[Ab2$name == "X2010"] <- 2010
Ab2$name[Ab2$name == "X2011"] <- 2011
Ab2$name[Ab2$name == "X2013"] <- 2013
Ab2$name[Ab2$name == "X2014"] <- 2014
Ab2$name[Ab2$name == "X2015"] <- 2015
Ab2$name[Ab2$name == "X2017"] <- 2017
Ab2$name[Ab2$name == "X2018"] <- 2018
Ab2$name[Ab2$name == "X2019"] <- 2019
Ab2 <- Ab2 %>% rename(Year = name)
Ab2 <- Ab2 %>% rename(Pink_Abundance = value)
Master_dataset$Year <- sapply(Master_dataset$Year, as.character)
Master_dataset <- left_join(Master_dataset, Ab2, by = c("StreamName", "Year"))
View(Master_dataset)

#density
Den2 <- Pink_density %>% pivot_longer(cols = c(X2008, X2009, X2010, X2011, X2013,
                                                  X2014, X2015, X2017, X2018, X2019))
View(Den2)
Den2$name[Den2$name == "X2008"] <- 2008
Den2$name[Den2$name == "X2009"] <- 2009
Den2$name[Den2$name == "X2010"] <- 2010
Den2$name[Den2$name == "X2011"] <- 2011
Den2$name[Den2$name == "X2013"] <- 2013
Den2$name[Den2$name == "X2014"] <- 2014
Den2$name[Den2$name == "X2015"] <- 2015
Den2$name[Den2$name == "X2017"] <- 2017
Den2$name[Den2$name == "X2018"] <- 2018
Den2$name[Den2$name == "X2019"] <- 2019
Den2 <- Den2 %>% rename(Year = name)
Den2 <- Den2 %>% rename(Pink_Density = value)
Master_dataset$Year <- sapply(Master_dataset$Year, as.character)
Master_dataset <- left_join(Master_dataset, Den2, by = c("StreamName", "Year"))
View(Master_dataset)




releas <- read_csv("WMA_releas_by_yr2.csv")

#Append releases within 40km to master dataset:
releas <- WMA_releas_by_yr %>% pivot_longer(cols = c(X2008, X2009, X2010, X2011, X2013,
                                                 X2014, X2015, X2017, X2018, X2019))

Master_dataset$Year <- sapply(Master_dataset$Year, as.character)
releas$Year <- sapply(releas$Year, as.character)
Master_dataset <- left_join(Master_dataset, releas, by = c("StreamName", "Year"))
View(Master_dataset)

#Append distance to nearest hatchery and nearest release site to master dataset
#nearest hatchery
Dist_nearest_H <- read_csv("~/Documents/CHUM THESIS/Data Sources/Dist_Nearest_H&Release/Dist_nearest_H.csv")
View(Dist_nearest_H)
nearest_H <- Dist_nearest_H %>% pivot_longer(cols = c(X2008, X2009, X2010, X2011, X2013,
                                                     X2014, X2015, X2017, X2018, X2019))
View(nearest_H)
nearest_H$name[nearest_H$name == "X2008"] <- 2008
nearest_H$name[nearest_H$name == "X2009"] <- 2009
nearest_H$name[nearest_H$name == "X2010"] <- 2010
nearest_H$name[nearest_H$name == "X2011"] <- 2011
nearest_H$name[nearest_H$name == "X2013"] <- 2013
nearest_H$name[nearest_H$name == "X2014"] <- 2014
nearest_H$name[nearest_H$name == "X2015"] <- 2015
nearest_H$name[nearest_H$name == "X2017"] <- 2017
nearest_H$name[nearest_H$name == "X2018"] <- 2018
nearest_H$name[nearest_H$name == "X2019"] <- 2019
nearest_H <- nearest_H %>% rename(Year = name)
nearest_H <- nearest_H %>% rename(Dist_nearest_H = value)
Master_dataset$Year <- sapply(Master_dataset$Year, as.character)
Master_dataset <- left_join(Master_dataset, nearest_H, by = c("StreamName", "Year"))
View(Master_dataset)

#nearest release site (which may or may not include the hatchery)
nearest_R <- Dist_nearest_Releas %>% pivot_longer(cols = c(X2008, X2009, X2010, X2011, X2013,
                                                           X2014, X2015, X2017, X2018, X2019))
View(nearest_R)
nearest_R$name[nearest_R$name == "X2008"] <- 2008
nearest_R$name[nearest_R$name == "X2009"] <- 2009
nearest_R$name[nearest_R$name == "X2010"] <- 2010
nearest_R$name[nearest_R$name == "X2011"] <- 2011
nearest_R$name[nearest_R$name == "X2013"] <- 2013
nearest_R$name[nearest_R$name == "X2014"] <- 2014
nearest_R$name[nearest_R$name == "X2015"] <- 2015
nearest_R$name[nearest_R$name == "X2017"] <- 2017
nearest_R$name[nearest_R$name == "X2018"] <- 2018
nearest_R$name[nearest_R$name == "X2019"] <- 2019
nearest_R <- nearest_R %>% rename(Year = name)
nearest_R <- nearest_R %>% rename(Dist_nearest_R = value)
Master_dataset$Year <- sapply(Master_dataset$Year, as.character)
Master_dataset <- left_join(Master_dataset, nearest_R, by = c("StreamName", "Year"))
View(Master_dataset)

#Calculate straying index denominator and attach to master dataset
p <- Master_dataset %>% group_by(Year) %>% summarise(Total_strays =
                                                       sum(`Number H fish`))
View(p)
z <- left_join(Master_dataset, p, by = 'Year')
View(z) #this is your new master dataset, still need to add stream hydro  
Stream_hydro <- read_csv("Stream_hydro.csv")
View(Stream_hydro)
y <- left_join(z, Stream_hydro, by = "StreamName")
View(y)
#Export
setwd("~/Documents/CHUM THESIS/Data Sources/Model_matrices")
write.csv(y, "Master_dataset.csv")

###########  add % lake area by watershed predictor data  ######################
lake_A_by_WShed <-
  read_csv("~/Documents/CHUM THESIS/Data Sources/Stream_Hydrology/lake_A_by_WShed.csv")
Master_dataset <-
  read_csv("~/Documents/CHUM THESIS/Data Sources/Model_matrices/Master_dataset.csv")
library(dplyr)
f <- inner_join(Master_dataset, lake_A_by_WShed, by = "StreamName")
View(f)
f <- f %>% rename("%lake_wetL_area_Wshed" = OpenFrozenRiparianWetland)
#Export
setwd("~/Documents/CHUM THESIS/Data Sources/Model_matrices")
write.csv(f, "Master_dataset.csv")

######  add flow data from Sergeant et al. 2020 dataset  ######################
Master_dataset <- read_csv("~/Documents/CHUM THESIS/MODEL/Master_dataset.csv")
View(Master_dataset)

Flow_dat <- read_csv("Documents/CHUM THESIS/Data Sources/Stream_Hydro_Flow/Flow_dat.csv", 
                     col_types = cols(Watershed_ID = col_skip(), 
                                      Watershed_km2 = col_skip(), Stream_spans_2_Wsheds = col_skip(), 
                                      Scenario_A = col_skip(), Scenario_B = col_skip(), 
                                      Notes = col_skip()))
View(Flow_dat)
Master_dataset2 <- inner_join(Master_dataset, Flow_dat, by = "StreamName")
setwd("~/Documents/CHUM THESIS/MODEL")
write.csv(Master_dataset2, "Master_dataset2.csv")
