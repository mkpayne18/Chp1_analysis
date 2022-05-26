setwd("~/Documents/CHUM_THESIS/Analysis/Figs_Results/Suppl_Materials/Wt_vs_Avg_pHOS")
library(ggplot2)
library(tidyverse)
library(dplyr)
pHOS_differences <- read_excel("pHOS_differences.xlsx")

pHOS_long <- pHOS_differences %>% pivot_longer(!StreamName, names_to = "Year",
                                               values_to = "pHOS_difference")
pHOS_long <- data.frame(pHOS_long)
pHOS_long$Year[pHOS_long$Year == "X2013"] <- "2013"
pHOS_long$Year[pHOS_long$Year == "X2014"] <- "2014"
pHOS_long$Year[pHOS_long$Year == "X2015"] <- "2015"

#dot plot with categorical x-axis (each of the streams) and 3 points for each
#stream showing the difference in pHOS between Josephson et al. 2021 and my data
#for the 3 years in question (2013-2015)
p <- ggplot(pHOS_long, aes(x = StreamName, y = pHOS_difference, shape = Year)) +
  xlab("Stream Name") + ylab("pHOS Difference") +
  theme(axis.text.x = element_text(size = 13, angle = 90)) + geom_point(size = 5) +
  scale_shape_manual(values=c(2, 5, 19)) +
  theme(axis.text.y = element_text(size = 15)) +
  theme(axis.title = element_text(size = 16)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.key = element_rect(fill = NA, color = NA)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 16)) +
  theme(text=element_text(family="Times New Roman"))
p

#Export figure in high res
tiff("pHOSdiff.tiff", width = 8.5, height = 6.3, pointsize = 12, units = 'in', res = 300)
p #insert graph that you want to export
dev.off( ) #now the displayed graphs are saved to a file with the above file name



#t-test compare pHOS values for each year
X2013 <- read_csv("2013.csv")
?t.test
t.test(X2013$pHOS_Josephson, X2013$pHOS_MyDat) #p = 0.95; fail to reject null
#Josephson et al. 2021 and my data's pHOS values are not significantly different
#in 2013

X2014 <- read_csv("2014.csv")
t.test(X2014$pHOS_Josephson, X2014$pHOS_MyDat) #p = 0.85; fail to reject null
#Josephson et al. 2021 and my data's pHOS values are not significantly different
#in 2014

X2015 <- read_csv("2015.csv")
t.test(X2015$pHOS_Josephson, X2015$pHOS_MyDat) #p = 0.96; fail to reject null
#Josephson et al. 2021 and my data's pHOS values are not significantly different
#in 2015
