#In this script, I am trying to find if there is any potential bias in my response
#variable whereby streams with larger dead counts have proportionally fewer fish
#sampled and then collating dead counts for all surveys to deal with the bias
library(dplyr)
library(lubridate)
library(lme4)
library(ggplot2)

### See the Rmd files in the "dead_count_additional_notes" folder within the
#"scripts" folder for 1) a guide walking you through how survey_data was tailored
#in this script, and for 2) an important note (mentioned at the end of this
#script) about adjusting the proportion sampled to 1 when this proportion was >
#1. For complete description of why this script even exists, check out Bayes_intro
#&Model_devo.docx.

### Read in data
survey_data <- read.csv("data/survey_data.csv") #directly downloaded from hatcherywild.org,
#where the NumberofSpecimens column indicates how many samples were collected and
#the dead count indicates the number of total dead, or the number of new dead
#specimens sampled that day + previously sampled fish (unfortunately they do not
#differentiate between the two types of dead). Sometimes the number of specimens
#is greater than the DeadCount (typically early season) bc surveyors would kill 
#and sample live, post-spawn fish, which were counted as "Live" instead of dead

#Remove streams that are not included in analysis
delete2 <- c("Hidden Inlet", "North Arm Creek") 
survey_data2 <- survey_data[!(survey_data$StreamName %in% delete2),]
rownames(survey_data2) <- 1:nrow(survey_data2)





#1. Check for bias within individual surveys ###################################
#### Create proportion sampled plots by survey ####
### Plot total fish sampled ~ total dead count (sampled + unsampled dead)
plot(NumberofSpecimens ~ DeadCount, survey_data2)
abline(0,1, col = "red") #there is clear evidence of bias, whereby a proportionally
#smaller amount of the total # of dead in the stream is sampled when the dead 
#count is higher. This could bias your detected # of strays lower than the true
#value in streams with higher dead counts



### Plot proportion of fish sampled (sampled/total dead) ~ total dead count
survey_data2$Proportion_sampled <- survey_data2$NumberofSpecimens/survey_data2$DeadCount
plot(Proportion_sampled ~ DeadCount, survey_data2) #hard to see. Filter out prop.
#sampled >= 1
prop_1 <- survey_data2 %>% filter(Proportion_sampled <= 1)
plot(Proportion_sampled ~ DeadCount, prop_1) #confirms my above conclusion in 
#previous plot
saveRDS(prop_1, "output/prop_1.rds")


##ggplot version of first plot for manuscript
#exclude the streams that have live fish sampled (i.e., NumberofSpecimens > 
#DeadCount), which are contained in the prop_1 dataframe
plot(NumberofSpecimens ~ DeadCount, prop_1) #base R version

bias_plot <- ggplot() +
  geom_point(data = prop_1, aes(x = DeadCount, y = NumberofSpecimens)) +
  geom_abline() +
  labs(y = "Number of carcasses sampled",
       x = "Total number of carcasses in stream") +
  theme_bw() +
  theme(text = element_text(family="Times New Roman", size = 14)) +
  theme(axis.text = element_text(size = 14))
bias_plot

#Export as high-res figure
tiff("figs/supplemental/bias_plot.tiff", width = 8, height = 7, pointsize = 12,
     units = 'in',
     res = 300)
bias_plot #graph that you want to export
dev.off( )





#2. Check for bias within a season (sum the survey counts) #####################
#### Create proportion sampled plots by season ####
survey_data2$SurveyDate <- as.Date(survey_data2$SurveyDate, format = "%m/%d/%y")
survey_data2$Year <- as.factor(format(survey_data2$SurveyDate, format="%Y"))
survey_season <- survey_data2 %>% group_by(StreamName, Year) %>%
  summarise(across(.cols = c(DeadCount, NumberofSpecimens), sum))
head(survey_season, 50) #Note lots of NA values in DeadCount column
survey_season$Proportion_sampled <- survey_season$NumberofSpecimens/survey_season$DeadCount


plot(NumberofSpecimens ~ DeadCount, survey_season, main = "By season")
plot(Proportion_sampled ~ DeadCount, survey_season, main = "By season") #same 
#conclusion as above, where streams that have more dead throughout the season
#have a smaller proportion of the total dead sampled




#3. Is total dead count correlated with conspecific abundance in a stream? #####
#AliveCount almost certainly is, but what about dead count?
#This might cause confounding results in your model if they are correlated
Master_dataset <- read.csv("data/Master_dataset.csv")
Master_dataset$Year <- as.factor(Master_dataset$Year)
#use survey_season df from above, as this will indicate the total dead count by
#season for a given stream
dead_vs_consA <- left_join(survey_season, Master_dataset, by = c("StreamName", "Year"))
plot(DeadCount ~ Cons_Abundance, dead_vs_consA)
cor.test(dead_vs_consA$DeadCount, dead_vs_consA$Cons_Abundance)
#there doesn't appear to be much correlation




#4. Dead count NAs #############################################################
#Are there NA values for any dead counts, which would make it challenging for 
#you to adjust for sampling bias?
sum(is.na(survey_data2$DeadCount)) #78
length(survey_data2$DeadCount) #544
#78 surveys have missing dead counts for whatever reason. See if you can find out
#why
deadNA <- survey_data2 %>% filter(is.na(DeadCount))
print(deadNA[,c(1:3)]) #Sawmill Creek in 2015 makes up a large swath of the missing
#dead counts. I would imagine you might be able to get these from McConnell et al.
#2015, since the authors of that paper were surveying there everyday 



#4.1. Update Sawmill Creek 2015 DeadCount NAs ==================================
#Change to the carcasses recovered column in table 1 of McConnell et al. 2015,
#as this approximates total dead
saw15 <- survey_data2 %>%
  filter(StreamName == "Sawmill Creek" & Year == "2015") %>%
  arrange(SurveyDate) #this last part (arrange(...)) orders the dates sequentially
#Create object (saw15_deads) that gives the "carcasses recovered" column from 
#Table 1 in McConnell et al. 2018, making sure that the dates line up with those
#in saw15 object order (which are ordered sequentially by date):
saw15_deads <- c(1,4,11,16,32,48,19,44,24,42,32,22,32,41,19,22,26,24,37,36,65,0)
#note that 7/21 and 8/7 surveys show up in the saw15 survey data, but not in Mc-
#Connell et al. 2018. I estimated these values as halfway between the dead counts
#on either side. E.g., 32 (the 5th value in the vector above) is the 7/21/15 survey
#estimated dead count and is the number halfway between 16 and 48. 22 (the 16th
#value) is the 8/7/15 survey estimated dead count

#Input into dataframe!
saw15$DeadCount <- saw15_deads

#Now add additional surveys from McConnell et al. that were not included in the
#saw15 survey data (7/6-7/8, 7/10, 7/14, 7/16, and 8/20)
#Create this dataset containing additional surveys (n = 7):
awc <- rep("115-20-10520", 7)
strmnm <- rep("Sawmill Creek", 7)
new_saw_dates <- c("2015-07-06", "2015-07-07", "2015-07-08", "2015-07-10",
                         "2015-07-14", "2015-07-16", "2015-08-20")
mtrx <- matrix(ncol = length(saw15) - 3,#subtract the 3 columns you created above
               nrow = 7)
additional_saw15 <- cbind.data.frame(awc, strmnm, new_saw_dates, mtrx)
names(additional_saw15) <- names(saw15)
zero <- rep(0,7)
additional_saw15$DeadCount <- zero
additional_saw15$NumberofSpecimens <- zero
additional_saw15$NumberStrays <- zero
additional_saw15$Year <- rep("2015", 7)
Sawmill_2015 <- rbind.data.frame(saw15, additional_saw15)
str(Sawmill_2015)

#Incorporate Sawmill_2015 object (corrected dead counts) into full survey_data2
interim <- survey_data2 %>% filter(StreamName != "Sawmill Creek" | Year != 2015)
survey_data3 <- rbind.data.frame(interim, Sawmill_2015) #should have 7 more rows
#than survey_data2 at this point in the code because you added 7 surveys 
length(survey_data2$SurveyDate) #544
length(survey_data3$SurveyDate) #551



#4.2. Interpolate other dead counts if possible ================================
### After taking care of 2015 Sawmill dead counts, check other NA dead counts:
deadNA2 <- survey_data3 %>% filter(is.na(DeadCount))
length(deadNA2$SurveyDate) #58
length(deadNA$SurveyDate) #78 ## there were 22 total Sawmill 2015 surveys in 
#original survey_data2 (and survey_data for that matter) df, and 2 of them did
#not have NA values in the DeadCount column. In the above section, I overwrote
#those two non-NA DeadCounts with their matching values in the McConnell et al.
#table 1 so that I would have all Sawmill 2015 (n = 22) surveys together. Bc there
#are 2 non-NA DeadCounts for Sawmill 2015 in survey_data2, this means that once
#you input the Sawmill 2015 dead counts that were missing, there are only 20 fewer
#NA values in survey_data3 as opposed to survey_data2, instead of 22 fewer (22 =
#total number of original Sawmill 2015 surveys)

#Said another way^^, length(deadNA2$SurveyDate) should = 58
length(deadNA2$SurveyDate) #58


print(deadNA2[,c(1:3)]) #no more Sawmill 2015
#As for the others, you can delete Admiralty Creek 2019-07-18, because this was
#a training exercise at the Sitka Sound Science Center tent (see "Comments" column)
survey_data3 <- survey_data3 %>%
  filter(StreamName != "Admiralty Creek" | SurveyDate != "2019-07-18")
deadNA2 <- deadNA2 %>%
  filter(StreamName != "Admiralty Creek" | SurveyDate != "2019-07-18")

#For other dead counts (n = 57), recall from the Alaska Hatchery Research Project
#methods section for 2013-2015 streams that there was a season threshold of 384
#fish, beyond which the stream would typically cease to be sampled. This means
#that any stream that had a season total sampled < 384 would have had insufficient
#dead to sample beyond that number. Hence, you can probably safely assume that
#those streams had dead count = specimen count (i.e., they were able to sample
#everything)
#Use survey_season df from section 2 above. This will give you total sampled by
#stream by season:
eval_1315_deads <- left_join(deadNA2, survey_season, by = c("StreamName", "Year"))
#filter out the 2017-2019 years because the 384 fish threshold did not apply then
eval_1315_deads <- eval_1315_deads %>% filter(Year %in% c("2013", "2014", "2015"))

no_longer_NA_1315 <- eval_1315_deads %>%
  mutate(DeadCount.x = ifelse(NumberofSpecimens.y <= 384,
                              NumberofSpecimens.x, NA))
#remove excess columns^^ from the no_longer_NA_1315 df and replace their counter-
#part rows in survey_data3 full dataset. I.e., input interpolated dead counts into
#the 2013-2015 stream survey rows that are missing dead counts, using the dead
#counts you interpolated in the no_longer_NA_1315 object
interpolated_1315_deads <- no_longer_NA_1315[,-c(30:32)]
interpolated_1315_deads <- interpolated_1315_deads %>%
  rename(DeadCount = DeadCount.x,
         NumberofSpecimens = NumberofSpecimens.x,
         Proportion_sampled = Proportion_sampled.x) 

#remove the corresponding rows from survey_data3 full dataset
remove1315NA_survey_data <- survey_data3 %>%
  filter(!is.na(DeadCount) | !Year %in% c("2013", "2014", "2015"))

#Now add the interpolated 2013-2015 rows to the full survey data
survey_data4 <- rbind.data.frame(remove1315NA_survey_data, interpolated_1315_deads)


### Check your work:
check2 <- anti_join(survey_data2, survey_data4,
                    by = c("StreamName", "SurveyDate")) #should contain the 1
#Admiralty Creek "survey" in the science center tent
check2 <- anti_join(survey_data4, survey_data2,
                    by = c("StreamName", "SurveyDate")) #should contain the 7 
#additional Sawmill Creek 2015 surveys 
check2 <- anti_join(survey_data4, survey_data2) #this should include the 29 total
#Sawmill Creek 2015 surveys (22 in original data + 7 new) and the 2013-2015 
#surveys for which you interpolated the dead counts. Looks like it does! Double
#check:
sum(!is.na(interpolated_1315_deads$DeadCount)) +
  length(Sawmill_2015$DeadCount) #56
length(check2$DeadCount) #56
sum(!is.na(check2$DeadCount)) #56
sum(is.na(check2$DeadCount)) #0 hooray!

sum(is.na(survey_data4$DeadCount)) #30
deadNA3 <- survey_data4[is.na(survey_data4$DeadCount),] #all looks good

### Here is your final dataset for the moment, containing the most complete 
#dead count data possible for 2013-2015 years:
survey_data4
#Note that there are a few remaining missing 2013-2015 dead counts that will need
#to be modeled, and the missing 2017-2019 dead counts will need to be dealt with
#separately





#5. Dead counts for 2017-2019 ##################################################
fitness_1719 <- survey_data4 %>% filter(Year %in% c("2017", "2018", "2019"))
#"fitness" refers to the fitness streams (Fish, Sawmill, Prospect, Admiralty)

#5.1. Deal with NAs ============================================================
sum(is.na(fitness_1719$NumberofSpecimens)) #complete data
sum(is.na(fitness_1719$DeadCount)) #12
length(fitness_1719$DeadCount) #there are only 12 missing dead counts in all of
#2017-2019. Go through each individually and see if you can't provide a best 
#estimate for each

## The first 2 NAs are high water days where a count was not possible and the total
#number of specimens was small. In the 2013-2015 dead counts, most of the high
#flow days have small dead counts coinciding with small # of specimens. Therefore
#I would assign this NA the same total dead count as the # of specimens to be 
#consistent
fitness_1719[85,18] <- 13
fitness_1719[87,18] <- 1

#For the next 2 NAs, the surveys are earlier season (8/2 and 8/3) and the Alive
#Count = TotalCount. Normally TotalCount = AliveCount + DeadCount. Given this and
#the fact that the NumberofSpecimens = 0 for these two, I think you can safely
#assume that DeadCount = 0
fitness_1719[94,18] <- 0
fitness_1719[95,18] <- 0

#Similar situation^^ for the next two NAs
fitness_1719[105,18] <- 0
fitness_1719[106,18] <- 0

#The next NA is Fish Creek 8/3/17, where the # of specimens = 214 and the comments
#suggest that there were a lot of dead (and no rain/high waters). I would input
#the # of dead as the mean of the # of dead from the closest survey dates, which
#were 1 and 3 days away, respectively
(264+289)/2 #277
fitness_1719[114,18] <- 277

#similarly, no rain or high waters, just missing dead counts for some reason. I 
#would input the mean of the two proximate survey's dead counts, which were 2
#and 3 days away, respectively
(431+496)/2 #464
fitness_1719[117,18] <- 464

#Unsure what to make of next NA (Fish Creek 8/20/17) but proceeding dead counts
#(except for high water day immediately before) are high and the number of
#specimens on 8/20 = 111. I would assume that the # of specimens = dead count
#in this case
fitness_1719[122,18] <- 111

#NA for Sawmill Creek 8/4/17. No indication of high waters. Dead counts of 2 
#proximate surveys (2 days before and after) are high, as is the number of spec-
#imens on 8/4/17. Assume that dead count = mean of 2 previous surveys' dead counts
(486+438)/2 #462
fitness_1719[168,18] <- 462

#" " for the next 2 NAs
(853+855)/2 #854
fitness_1719[171,18] <- 854

(498+336)/2 #417
fitness_1719[175,18] <- 417

#That should be all of the NAs. Check your work:
sum(is.na(fitness_1719$DeadCount)) #0
old_fitness_1719 <- survey_data4 %>% filter(Year %in% c("2017", "2018", "2019"))
nomatch_fit1719 <- anti_join(old_fitness_1719, fitness_1719) #looks good (should
#be 12 that did not match bc they were originally NA)




#5.2. Determine true dead counts for 2017-2019 streams =========================
#Unlike 2013-2015 surveys, 2017-2019 surveys are conducted either every day or
#every other day, so you need to account for the excess dead in each DeadCount

#Do this by subtracting out the previous day's dead count from each dead count.
#The previous day's dead count should include all dead that are remaining from
#the previous set of days, as well as the new specimens being sampled that day.
#Only subtract each previous day's dead count if the previous day's dead count
#is less than the current day dead count. Current day dead counts that are smaller
#are all likely to be new dead, as the old, already-sampled carcasses would have
#been washed out or otherwise disappeared (that's why there would be fewer total
#dead compared to previous day)

#Create "helper" column (the amount to subtract from the dead count)
fitness_1719$subtract <- rep(NA, length(fitness_1719$AdfgStreamCode))
#Create your estimated dead counts column to input your subtracted counts
fitness_1719$Estimated_dead <- rep(NA, length(fitness_1719$AdfgStreamCode))

subtract_col_fxn <- function(vec){
  vec2 <- c(0, vec) #offsets each vector of dead counts by 1 position
  vec3 <- vec2[-length(vec2)] #removes final val from each vector
  return(vec3)
} #fxn offsets the dead counts by 1 position, allowing you to easily subtract the 
#previous day's dead count from the current dead count by putting the previous
#day's dead count in another column

final_deads_1719 <- fitness_1719 %>% group_by(StreamName, Year) %>%
  mutate(subtract = subtract_col_fxn(DeadCount)) %>%
  mutate(Estimated_dead = ifelse(DeadCount > subtract,
                                 DeadCount - subtract, DeadCount)) #as noted ab-
#ove, only subtract the previous day's dead count from the current day's dead
#count if previous day < current day dead count. If this condition is not met, 
#then set DeadCount = DeadCount (bc we assume these are new dead)



#5.3. Tailor final_deads_1719 and combine with 2013-2015 survey data ===========
final_deads_1719 <- final_deads_1719[,-30] #remove the "subtract" column
final_deads_1719$DeadCount <- final_deads_1719$Estimated_dead
final_deads_1719 <- final_deads_1719[,-30] #remove estimated dead column, as these
#numbers are now in the dead count col

survey_data5 <- survey_data4 %>% filter(!Year %in% c("2017", "2018", "2019"))
survey_data5 <- rbind.data.frame(survey_data5, final_deads_1719)
sum(is.na(survey_data5$DeadCount)) #18. This is what there should be, given
#survey_data4 had 30 NAs and we took care of 12 of them in 2017-2019 surveys




#5.4. 2013-2015 fitness streams adjust dead count ==============================
#The 4 fitness streams (Fish, Sawmill, Prospect, and Admiralty Creek) are also 
#sampled more often than the other, non-fitness streams in 2013-2015, which
#means those DeadCounts will also have excessive dead that you need to deal with
fitness_1315 <- survey_data5 %>%
  filter(Year %in% c("2013", "2014", "2015"),
         StreamName %in% c("Fish Creek", "Admiralty Creek", "Sawmill Creek",
                           "Prospect Creek"))
#OK never mind, looking at these data^^ it appears that only in 2014 were the
#fitness streams sampled extremely frequently. Otherwise these streams were sam-
#pled quite far apart in time (often weeks) in 2013 and 2015, meaning all the 
#dead would be new dead. In 2015, Sawmill was sampled almost everyday (McConnell
#et al. 2018), but the carcasses were not previously sampled (i.e., new dead)
#So..
rm(fitness_1315)
fitness_14 <- survey_data5 %>% #only worry about 2014 fitness streams then
  filter(Year %in% c("2014"),
         StreamName %in% c("Fish Creek", "Admiralty Creek", "Sawmill Creek",
                           "Prospect Creek"))


#Create "helper" column (the amount to subtract from the dead count)
fitness_14$subtract <- rep(NA, length(fitness_14$AdfgStreamCode))
#Create your estimated dead counts column to input your subtracted counts
fitness_14$Estimated_dead <- rep(NA, length(fitness_14$AdfgStreamCode))

subtract_col_fxn #use same function from section 5.2 above

fitness_deads_14 <- fitness_14 %>% group_by(StreamName, Year) %>%
  mutate(subtract = subtract_col_fxn(DeadCount)) %>%
  mutate(Estimated_dead = ifelse(DeadCount > subtract,
                                 DeadCount - subtract, DeadCount))


### Replace 2014 fitness stream rows with these^^ in full df:
remove_fitness_14 <- survey_data5 %>%
  filter(!Year %in% c("2014") |
         !StreamName %in% c("Fish Creek", "Admiralty Creek", "Sawmill Creek",
                           "Prospect Creek"))

fitness_deads_14 <- fitness_deads_14[,-30] #remove the "subtract" column
fitness_deads_14$DeadCount <- fitness_deads_14$Estimated_dead
fitness_deads_14 <- fitness_deads_14[,-30] #remove estimated dead column
survey_data6 <- rbind.data.frame(remove_fitness_14, fitness_deads_14)








#6. Final part! Estimate dead counts for 2008-2011 data ########################
HW_data <- read.csv("data/2008_2019_HW_Data_copy.csv")
HW_data$Year <- as.factor(HW_data$Year)
#Filter the 2008-2011 years only
HW_data_811 <- HW_data %>% filter(Year %in% c("2008", "2009", "2010", "2011"))
#these data^^ are for individual fish. Summarize by survey!
HW_data_811$SurveyDate <- as.Date(HW_data_811$SurveyDate, format = "%m/%d/%y")
HW_by_survey <- HW_data_811 %>% group_by(StreamName, SurveyDate) %>%
  summarise(NumberofSpecimens = length(From_H), NumberStrays = sum(From_H))
HW_by_survey <- as.data.frame(HW_by_survey)


#6.1. Fit linear model =========================================================
#Use linear model to predict missing 2008-2011 dead counts based on the number
#of specimens

#Fit linear model using 2013-2015 data only that does not include the fitness
#streams (Fish, Sawmill, Prospect, Admiralty) since the fitness streams were
#sampled more often than 2013-2015 non-fitness streams and all 2008-2011 streams.
#Non-fitness 2013-2015 streams will likely have different dead counts within an
#individual survey compared to the fitness streams and will therefore be more
#representative of a 2008-2011 stream
non_fitness_1315 <- survey_data6 %>% filter(!StreamName %in% c("Fish Creek",
                                                               "Admiralty Creek",
                                                               "Sawmill Creek",
                                                               "Prospect Creek"))
plot(DeadCount ~ NumberofSpecimens, non_fitness_1315)
lm_dead <- lm(DeadCount ~ NumberofSpecimens, non_fitness_1315, na.action = na.omit)
abline(lm_dead)
summary(lm_dead) #slope is 1.4, which means that larger specimen counts increas-
#ingly correspond to dead counts that are so large that they can't all be sampled.
#This matches the observed data (see on plot), whereby the smaller number of
#specimens means that there generally weren't that many fish and the technicians
#were able to get them all


#6.2. Predict dead counts for 2008-2011 streams ================================
pred_deads_811 <- as.data.frame(predict(lm_dead, HW_by_survey))
deads_811 <- cbind.data.frame(HW_by_survey, pred_deads_811)
colnames(deads_811)[5] <- "DeadCount"
#Note that there are a handful of negative dead counts that need to be corrected
deads_811$DeadCount[deads_811$DeadCount < 0] <- 0





#6.3. Add 2008-2011 dead counts to full dataframe ==============================
mtrx <- as.data.frame(matrix(nrow = length(deads_811$StreamName),
                             ncol = 23))
mtrx_deads_811 <- cbind.data.frame(rep(NA, length(deads_811$StreamName)),
                                          deads_811, mtrx)
mtrx_deads2 <- mtrx_deads_811[,c(1:3,7:20,6,21,22,4,5,23:29)]
names(mtrx_deads2) <- names(survey_data6)
#Fill in year column
mtrx_deads2$SurveyDate <- as.Date(mtrx_deads2$SurveyDate, format = "%m/%d/%y")
mtrx_deads2$Year <- as.factor(format(mtrx_deads2$SurveyDate, format="%Y"))

#Combine into final dataframe
survey_data7 <- rbind.data.frame(survey_data6, mtrx_deads2)
survey_data7$DeadCount <- round(survey_data7$DeadCount)






#7. Predict 2013-2015 missing dead counts ######################################
sum(is.na(survey_data7$DeadCount)) #18 remaining dead counts, all in 2013-2015

#Predict these missing dead counts with the model you created
survey_data8 <- survey_data7 %>%
  mutate(DeadCount = ifelse(is.na(DeadCount),
                              round((NumberofSpecimens*coef(lm_dead)[2])+coef(lm_dead)[1]),
                              DeadCount))
sum(is.na(survey_data8$DeadCount))
sum(is.na(survey_data8$NumberofSpecimens))
survey_data8[survey_data8$DeadCount < 0,] #the model predicted 1 dead count to
#be < 0. Set this = 0
survey_data8$DeadCount[survey_data8$DeadCount < 0] <- 0


### Your final dataset that contains all of the dead counts to use for rescaling
#your model response variable is survey_data8
survey_data8








#8. Rescale your model response variable #######################################
NumberStrays_NA <- survey_data8 %>%
  filter(is.na(NumberStrays)) #These should all be zeros (bc NumberofSpecimens =
#0), so change them to zeros
survey_data8$NumberStrays[is.na(survey_data8$NumberStrays)] <- 0


#8.1. Survey data (survey_data8) final tailoring ===============================
### Calculate proportion sampled as the NumberofSpecimens/DeadCount, since we're 
#assuming based on all the work in sections 1-7 above that the dead should mostly
#be new dead
survey_data8$Proportion_sampled <-
  survey_data8$NumberofSpecimens/survey_data8$DeadCount
sum(is.na(survey_data8$Proportion_sampled))
prop_sampled_NA <- survey_data8 %>% filter(is.na(Proportion_sampled)) #these NAs
#should be 1s because NumberofSpecimens = 0 and DeadCount = 0, therefore the prop.
#sampled would be 1
survey_data8$Proportion_sampled[is.na(survey_data8$Proportion_sampled)] <- 1
sum(is.na(survey_data8$Proportion_sampled)) #0
survey_data8$Proportion_sampled[survey_data8$Proportion_sampled < 0] #none

#Are there many proportions sampled > 1?
length(survey_data8$Proportion_sampled[survey_data8$Proportion_sampled > 1]) #306,
#so actually yes
length(survey_data2$Proportion_sampled[survey_data2$Proportion_sampled > 1]) #275,
#but this doesn't differ much from the raw data (before you did any modeling or
#updating of values)


#Looking at the data, there are some rows where the NumberofSpecimens is actually
#greater than the TotalCount (Alive + Dead), which doesn't really make any sense.
#But, this was true in the raw data (i.e., it wasn't anything I did):
length(survey_data2[survey_data2$NumberofSpecimens > survey_data2$TotalCount,])
#raw data^^ total = 29
length(survey_data8[survey_data8$NumberofSpecimens > survey_data8$TotalCount,])
#my final data^^, total = 29
#So, I'm not sure what to make of this. I will assume my specimen counts are 
#accurate and set all proportions sampled > 1 equal to 1, as per C. Cunningham's
#suggestion

survey_data8$Proportion_sampled[survey_data8$Proportion_sampled > 1] <- 1
#Final check:
na_or_below_0 <- function(column){
  sum(is.na(column))
  length(column[column < 0])
}
na_or_below_0(survey_data8$DeadCount)
na_or_below_0(survey_data8$NumberofSpecimens)
na_or_below_0(survey_data8$NumberStrays)



#8.2. Calculate effective number of hatchery fish! =============================
#Effective number of hatchery fish = # hatchery strays / proportion sampled
survey_data8$Effective_number_strays <-
  survey_data8$NumberStrays/survey_data8$Proportion_sampled
#Are there any NAs?
sum(is.na(survey_data8$Effective_number_strays)) #yes, 7 of them
effective_straysNA <- survey_data8 %>% filter(is.na(Effective_number_strays))
#these^^ streams all have 0 hatchery strays, and a proportion sampled = 0 as well.
#The dead counts for these streams ranges from 1-11 individuals. Maybe these are
#some previously sampled dead, and that's why 0 fish were sampled? Hopefully 1-11 
#fish is the max error associated with any of my dead counts
#Since the number of strays for these 7 NAs = 0, then the effective number of 
#strays would also = 0
survey_data8$Effective_number_strays[is.na(survey_data8$Effective_number_strays)] <- 0
sum(is.na(survey_data8$Effective_number_strays)) #none
survey_data8$Effective_number_strays[survey_data8$Effective_number_strays < 0] #none



#8.3. Calculate strays by season ===============================================
new_response_var <- survey_data8 %>% group_by(StreamName, Year) %>%
  summarise(Total_effective_strays = sum(Effective_number_strays),
    Number_of_surveys = length(SurveyDate))

#### VERY IMPORTANT NOTE ####
#The total effective number of strays column for a given stream-year in new_
#response_var will not always equal the number of hatchery strays / proportion
#sampled for a stream-year because you changed the set the proportions sampled
#that were greater than 1 equal to 1 instead. See docs/effective_number_strays_
#note.Rmd for more info and an example

sum(is.na(new_response_var$Total_effective_strays)) #0
sum(is.na(new_response_var$Number_of_surveys)) #0

#Calculate average (effective) number of strays (dividing by # of surveys):
new_response_var$Avg_number_strays <-
  new_response_var$Total_effective_strays/new_response_var$Number_of_surveys

### Compare to dataset used previously to determine the response variable
head(Master_dataset)
Master_subset <- Master_dataset[,c(2:5,8,9)]
colnames(Master_subset)[5] <- "old_Number_surveys"
colnames(Master_subset)[6] <- "old_Avg_strays"
compare_response <- left_join(new_response_var, Master_subset,
                              by = c("StreamName", "Year"))
#Does the number of surveys differ markedly between any of Number_of_surveys used
#to calculate the response variable previously? (previously = before we incorp-
#orated the dead count in any way)
compare_response$diff <-
  compare_response$Number_of_surveys - compare_response$old_Number_surveys
#There are two stream-years for which there are significantly more surveys
#in the new response variable dataset compared to the older version. These are
#1) Admiralty Creek in 2018, where the newer dataset downloaded from the stream
#survey reports section includes additional early season surveys where there were
#no specimens detected, and 2) Sawmill Creek 2015, where I added 7 more surveys
#that were listed in McConnell et al. 2018, but not in any AHRP data. Most other
#streams differ little or not at all in # of surveys

#How does the previous average # of strays differ from the effective average # of
#strays?
plot(compare_response$old_Avg_strays ~ compare_response$Avg_number_strays)
abline(0,1, col = "red")
#does not appear to differ drastically at smaller sample sizes. At larger sample
#sizes, the new (effective) number of strays is larger than the previous Avg_strays
#bc we've inflated many of those observations where proportionally less of the
#total dead was sampled



#Use the Avg_number_strays column from
new_response_var #to update your model response variable
saveRDS(new_response_var, file = "output/new_response_var.rds") #save this object
#for use in chapter 2 and in making chp1 supplemental figures. Make sure to move 
#a copy of it into chapter 2 folder

#Also save survey_data8. You will need it for some supplementary figures
saveRDS(survey_data8, "output/survey_data8.rds")






