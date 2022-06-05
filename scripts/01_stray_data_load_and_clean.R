#-------------------------------------------------------------------------------
# Project title: Revealing stream attractiveness to stray hatchery-origin chum
# salmon in Southeast Alaska (MS thesis chapter 1)

# Molly K Payne

# Purpose of this script: Load and clean straying data

# Last updated: May 18, 2022
#-------------------------------------------------------------------------------
require(tidyverse)
require(dplyr)

#Read in data containing response variable (number of strays in a stream and
#year) and covariate data
Master_dataset <- read.csv("data/Master_dataset.csv")
head(Master_dataset)
#Year is a factor
Master_dataset$Year <- factor(Master_dataset$Year)
str(Master_dataset)


#Remove Chilkat River, Disappearance Creek, Black River, Saook Bay West Head in
#2010 only, and Herman Creek from analysis. These creeks were sampled either 
#really late bc they are fall-run (Chilkat + Dis.Creek), or early (before 7/20).
delete <- c("Chilkat River", "Disappearance Creek", "Black River", "Herman Creek")
stray_dat <- Master_dataset[!(Master_dataset$StreamName %in% delete),]
#only delete 2010 for Saook Bay West Head, not 2011:
stray_dat <- stray_dat[!(stray_dat$Year == "2010" &
                           stray_dat$StreamName == "Saook Bay West Head"), ]


#Change NA values in WMA_Releases_by_Yr to 0 (these may be considered 0s because
#you've effectively "sampled" within 40km of a stream and found no releases, so
#it is not missing data)
stray_dat$WMA_Releases_by_Yr[is.na(stray_dat$WMA_Releases_by_Yr)] <- 0


#remove rows containing NA values (9 rows, 3 streams). As I find out later and 
#after reading about missing values in Zuur, it is best to remove the rows con-
#taining NA values since there will be many issues with model validation later
#on if you don't 
stray_dat <- stray_dat[complete.cases(stray_dat), ]
rownames(stray_dat) <- 1:nrow(stray_dat)
colnames(stray_dat)[8] <- "Number_surveys"



#In May 2022, I'm updating my model response variable to be the average EFFECTIVE
#number of strays, which takes into account bias in the response variable. Streams
#with larger dead counts have proportionally fewer fish sampled, so the # of strays
#detected would be biased low in these streams. I've dealt with this by calcul-
#ating the average effective number of hatchery strays in the dead_count_analysis.R
#script. Check out this script and Bayes_intro&Model_devo.docx for more info.
#Note in section 8.3 of the dead counts script that the two versions of the re-
#sponse variable don't differ greatly

#Replace Avg_number_strays with effective average number of strays. Avg effective
#strays can be found in the Avg_number_strays column of new_response_var object
#created in dead_count_analysis.R:

#First read in the new response variable data object containing the effective
#number of strays (created in dead_counts_analysis.R):
new_response_var <- readRDS("output/new_response_var.rds")
colnames(stray_dat)[8] <- "old_Number_surveys"
colnames(stray_dat)[9] <- "old_Avg_strays"
stray_dat2 <- left_join(stray_dat, new_response_var, by = c("StreamName", "Year"))
stray_dat3 <- stray_dat2[,c(1:3,20:22,10:19)] #remove some really old cols that
#I'm no longer using for anything
colnames(stray_dat3)[5] <- "Number_surveys"
stray_dat <- stray_dat3 #overwrite your former dataset
#^^In case you can't remember and/or don't want to go dig through Bayes_intro doc
#or dead_count_analysis.R script, Total_effective_strays is the sum of the eff-
#ective number of strays from each individual survey in a stream in a year (and
#effective strays = raw # of strays / proportion of total dead count sampled; it
#accounts for bias in the response variable). The Avg_number_strays column is now
#the Total_effective_strays / Number_surveys

#Remove un-needed items
rm(stray_dat2)
rm(stray_dat3)
rm(new_response_var)
rm(delete)
rm(Master_dataset)

### Final output from this script:
stray_dat #dataset to use for modeling. Read into EDA script next

#Send this object over to chapter 2 as well
saveRDS(stray_dat,
        file = "~/Documents/CHUM_THESIS/Chp2_analysis/output/stray_dat.rds")

