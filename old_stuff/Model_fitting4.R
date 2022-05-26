# Franz Mueter suggested I change my response variable to be the total number of hatchery strays detected in individual stream sampling events, rather than the average number across a season (year). While this would allow to me effectively deal with variation in survey effort, it would make my response variable non-independent and I don't have temporally finer scale Cons_Abundance and Pink_Abundance data to match with different points in the season. This script was me starting to do this analysis but abandoning it later

#1. Create and format dataset using survey date-specific response variable #####
HW_data <- read.csv("2008-2019_HW_Data copy.csv")
HW_data$Year <- as.factor(HW_data$Year)
HW_data$SurveyDate <- as.factor(HW_data$SurveyDate)
str(HW_data)
#remove unnecessary columns
HW_data <- HW_data[ , -c(5,8:16,19,21:23)]

#Determine number of H fish in each sampling event in each stream
library(dplyr)
HW_data2 <- HW_data %>% group_by(Year, StreamName, SurveyDate) %>%
  summarise(Number_H_fish = sum(From_H))
View(HW_data2)

#Verify correct number of H fish
Master_dataset <- read.csv("Master_dataset.csv")
Master_dataset$Year <- as.factor(Master_dataset$Year)
check_HW2 <- HW_data2 %>% group_by(Year, StreamName) %>% summarise(sum(Number_H_fish))
u <- left_join(Master_dataset, check_HW2, by = c("Year", "StreamName"))
sum(u$Number_H_fish-u$`sum(Number_H_fish)`) #looks good. Continue using HW_data2

#add covariate data to number of H fish per survey response variable dataset
new_df <- left_join(HW_data2, Master_dataset, by = c("Year", "StreamName"))
View(new_df)
new_df <- new_df[ , -c(7:9)]
new_df <- new_df[ , c(5,1:4, 6:18)]
colnames(new_df)[5] <- "Number_H_fish"
colnames(new_df)[7] <- "Number_surveys"


### Tailor dataset for modeling
str(new_df)

#remove Chilkat River, Disappearance Creek, Black River, Saook Bay West Head in
#2010 only, and Herman Creek from analysis. These creeks were sampled either 
#really late bc they are fall-run (Chilkat + Dis.Creek), or early (before 7/20).
delete <- c("Chilkat River", "Disappearance Creek", "Black River", "Herman Creek")
stray_dat <- new_df[!(new_df$StreamName %in% delete),]
#only delete 2010 for Saook Bay West Head, not 2011:
stray_dat <- stray_dat[!(stray_dat$Year == "2010" & stray_dat$StreamName == "Saook Bay West Head"), ]
#If you do choose to keep these creeks in the future, I'm only deleting them here
#in R. They still exist in the original .csv 

#Change NA values in WMA_Releases_by_Yr to 0
stray_dat$WMA_Releases_by_Yr[is.na(stray_dat$WMA_Releases_by_Yr)] <- 0

#remove rows containing NA values (9 rows, 3 streams). As I find out later and 
#after reading about missing values in Zuur, it is best to remove the rows con-
#taining NA values since there will be many issues with model validation later
#on if you don't 
stray_dat <- stray_dat[complete.cases(stray_dat), ]
stray_dat <- as.data.frame(stray_dat)
rownames(stray_dat) <- 1:nrow(stray_dat)

#add Julian day column
require(date)
v <- as.date(stray_dat$SurveyDate)
w <- as.POSIXct(v, format = "%d%b%y")
Julian <- format(w, "%j")
class(Julian)
stray_dat <- cbind.data.frame(stray_dat, Julian)




#2. Exploratory data analysis ##################################################
#2.1. Check distribution of response variable ==================================
hist(stray_dat$Number_H_fish, breaks = 50, main =
       "Histogram of Number of Strays per Survey (Model Response)",
     xlab = "Average # of hatchery-origin strays") #very right-skewed
boxplot(stray_dat$Number_H_fish) #high outliers
response_table <- as.data.frame(table(stray_dat$Number_H_fish))
head(response_table) #49 zeroes
176/length(stray_dat$Year) #28% of the data, maybe zero-inflation?? Check
#to see if model under-predicts the number of 0s later on

#Conclusion: Response data is going to be modeled with Poisson or Negative Bin-
#omial distribution with log-link due to heavy right-skew and because it is count
#data. Plot the LOG of the response against explanatory variables to evaluate 
#potential relationships. 


#2.2. Check for correlations between explanatory variables =====================
library(corrgram) #visually assess first
#I'm not totally sure if I need to check for correlations between Julian day and
#other covariates. First I'll see if this is even an issue:
stray_dat$Julian <- as.numeric(stray_dat$Julian)
corrgram(stray_dat[ , c(9:19)]) #evidence of several strong correlations

library(Hmisc)
stray_vars <- as.matrix(stray_dat[ , c(9:19)])  # Save variables as a matrix 
(COR <- rcorr(stray_vars, type = "spearman"))
#Julian day not correlated with any other covariates more than 0.3, so convert
#back to factor and don't worry about it
stray_dat$Julian <- as.factor(stray_dat$Julian)

#or alternatively, from base R:
cor(f[ , c(10:19)], method = "spearman") #you should use spearman, not pearson
#correlation coefs because your data may not be normal
COR$r   # Matrix of correlations
COR$P   # Matrix of p-values (which correlations are significant)
#Cons_Abundance and Cons_Density are highly correlated (0.89)
#Cons_Abundance and Pink_Abundance correlated (0.56)
#Cons_Abundance and CV_flow correlated (-0.50)
#Pink_Abundance and Pink_Density are highly correlated (0.88)
#Pink_Abundance and CV_flow are correlated (-0.59)
#Dist_nearest_R and WMA_Releases_by_Yr are highly correlated (-0.84)
#mean_flow and WMA_Releases_by_Yr are correlated (-0.54)
#CV_flow and WMA_Releases_by_Yr are correlated (0.53)
#Dist_nearest_R and mean_flow are correlated (0.70)
#Dist_nearest_R and CV_flow are correlated (-0.60)
#mean_flow and CV_flow are correlated (-0.53)

print(COR$P <= 0.05) #all of the above mentioned correlations are significant

#I will remove Cons_Density and Pink_Density from the model since they are the 
#less important of their correlated sets (mostly bc I don't have a lot of con-
#fidence in the density denominator data (area)). I will also remove Dist_
#nearest_H and Dist_nearest_R since they are correlated with WMA_Releases_by_Yr 
#and each other. WMA_Releases_by_Yr will remain because it provides the most info
f <- f[ , -c(12,14,16,17)]