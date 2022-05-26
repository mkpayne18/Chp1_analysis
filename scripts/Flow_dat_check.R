setwd("~/Documents/CHUM_THESIS/Analysis")
Master_dataset <- read.csv("Master_dataset.csv")
View(Master_dataset)

Master_dataset$Year <- factor(Master_dataset$Year)
str(Master_dataset)
#remove Chilkat River, Disappearance Creek, Black River, Saook Bay West Head in
#2010 only, and Herman Creek from analysis. These creeks were sampled either 
#really late bc they are fall-run (Chilkat + Dis.Creek), or early (before 7/20).
delete <- c("Chilkat River", "Disappearance Creek", "Black River", "Herman Creek")
f <- Master_dataset[!(Master_dataset$StreamName %in% delete),]
#only delete 2010 for Saook Bay West Head, not 2011:
f <- f[!(f$Year == "2010" & f$StreamName == "Saook Bay West Head"), ]
f$WMA_Releases_by_Yr[is.na(f$WMA_Releases_by_Yr)] <- 0
f <- f[complete.cases(f), ]
View(f)

#From flow data section:
Flow_dat <- read_csv("~/Documents/CHUM_THESIS/Data Sources/Stream_Hydro_Flow/Flow_dat.csv")
library(tidyverse)

#Link flow data accuracy information (scenario A = accurate vs scenario B) to 
#model dataset, filter out inaccurate streams, and rerun model 
f_scenA <- left_join(f, Flow_dat, by = "StreamName")
sum(f_scenA$mean_flow.x - f_scenA$mean_flow.y) #0, all are matching
sum(f_scenA$CV_flow.x - f_scenA$CV_flow.y) #0
f_accFlow <- f_scenA %>% drop_na(Scenario_A) #only keep scenario A streams
View(f_accFlow)

#Rerun model
#1. Check for collinearity
library(Hmisc)
stray_vars <- as.matrix(f_accFlow[ , c(10:19)])  # Save variables as a matrix 
(COR <- rcorr(stray_vars, type = "spearman"))
#Similar conclusions as in Model_fitting2.R; remove Cons_Density and Pink_Density
#as well as Dist_nearest_H and Dist_nearest_R. Some multicollineairty between 
#Pink_Abundance/Cons_Abundance and CV_flow (-0.51 and -0.52) but I will leave in
#since -0.51 and -0.52 are right on the line and were included previously in model



#2. Transform covariates as necessary
hist(f_accFlow$Fishery_harvest)
hist(f_accFlow$Cons_Abundance) #log transform
hist(f_accFlow$Pink_Abundance) #log(x + 1) transform (add 1 bc of 0s in this column)
hist(f_accFlow$WMA_Releases_by_Yr) #log(x + 1) transform (not much better but what can
#you do?)
hist(f_accFlow$mean_flow.x) #log transform
hist(f_accFlow$CV_flow.x)

#log + 1 transform 
trans_dat <- f_accFlow %>% mutate(Pink_Abundance = log(Pink_Abundance + 1),
                          WMA_Releases_by_Yr = log(WMA_Releases_by_Yr + 1))

trans_dat <- trans_dat %>% mutate(Cons_Abundance = log(Cons_Abundance),
                                  mean_flow.x = log(mean_flow.x))
View(trans_dat)



#3. Scale continuous covariates
scaled_data <- trans_dat %>% mutate(Fishery_harvest = scale(Fishery_harvest,
                                                            center = T, scale = T))
scaled_data$Cons_Abundance <-
  scale(scaled_data$Cons_Abundance, center = TRUE, scale = TRUE)
scaled_data$Pink_Abundance <-
  scale(scaled_data$Pink_Abundance, center = TRUE, scale = TRUE)
scaled_data$WMA_Releases_by_Yr <-
  scale(scaled_data$WMA_Releases_by_Yr, center = TRUE, scale = TRUE)
scaled_data$mean_flow.x <-
  scale(scaled_data$mean_flow.x, center = TRUE, scale = TRUE)
scaled_data$CV_flow.x <-
  scale(scaled_data$CV_flow.x, center = TRUE, scale = TRUE)
View(scaled_data)

#round response to integer
scaled_data$Avg_number_strays <- round(scaled_data$Avg_number_strays)



#4. Fit best model (as determined antes is Model_fitting2.R)
library(lme4)
acc_flow_mod <- glmer.nb(Avg_number_strays ~ (1|Year) + Cons_Abundance +
                           Pink_Abundance + WMA_Releases_by_Yr + mean_flow.x +
                           CV_flow.x + I(CV_flow.x^2), data = scaled_data)
summary(acc_flow_mod)
#Comparing to the results of the model fit with the full dataset, including 
#scenario B streams, there are some slight changes in magnitude when the model
#includes only the highly accurate flow streams, but the conclusions do not change.
#E.g., CV_flow and CV_flow^2 are still highly positively correlated with the 
#response. mean_flow is weakly negatively correlated with the response as before