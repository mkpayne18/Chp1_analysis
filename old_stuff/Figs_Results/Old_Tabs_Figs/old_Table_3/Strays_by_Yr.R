#This script creates part of the data for table 3, which gives the total number
#of strays detected in the subregion by year, averaged over the number of surveys.
#I calculated this by summing the Avg_number_strays column in my model dataset

#I sum the average number of strays from stream-year, meaning I first divide each
#stream-year's count of strays by the number of surveys that occurred in that 
#stream and year, and then I sum those numbers for each year. This instead of 
#summing the total strays detected in a year across all streams and then dividing
#by the total number of surveys across all streams because this would give a 
#different result. I think the first method is correct because that lines up each
#stream with its respective number of surveys, therefore not unfairly penalizing
#streams which weren't sampled often. That is to say, streams that were sampled 
#a lot will be divided by a larger number (as they should be), instead of having
#all streams just be divided by a lump sum

c <- f %>% group_by(Year) %>% summarise(Sum = sum(Avg_number_strays))
setwd("~/Documents/CHUM THESIS/Manuscript/Figures/Table_3")
write.csv(c, "Strays_by_Yr.csv")
