library(dplyr)
library(stats)

load("Mod_fit3.RData") #not loading anything, also not returning an error
load("Manu_figs_objects.RData") #works and appears to include all Mod_fit3.RData
#objects. This is probably because I load Mod_fit3.RData within Manu_figs.R script

#Anyway
head(f_update) #this is your master dataset that you used for modeling
#Stream location data is here:
StreamPoints <- read.csv("~/Documents/CHUM_THESIS/Data Sources/StreamPoints.csv")
StreamPoints2 <- StreamPoints %>% select(5:7)

dat_for_pca <- left_join(f_update, StreamPoints2, by = "StreamName")

?stats::prcomp
lat_long_pca <- prcomp(dat_for_pca[,c(16:17)], center = T, scale = T)#run PCA on
#latitude and longitude. Note that it is common practice to standardize (center
#and scale) continuous variables that you put into PCA. That doesn't seem to matter
#very much here bc I tried with and without standardizing and the trends between
#PC1 and my variables were the same
lat_long_pca$rotation #shows the PCA loadings. I.e., how much weight does each
#variable (lat or long) contribute to each PC
#Longitude is positively correlated with PC1, which means longitude increases as
#PC1 increases. Lat is negatively correlated with PC1, so as PC1 increases, lat
#decreases
PC1 <- lat_long_pca$x[,1] #gives the values of your first PC. I.e., your original 
#data values projected onto your first PC


### Explore potential relationships between your lat-long pc1 and your model
#explanatory variables
plot(dat_for_pca$Cons_Abundance ~ PC1, ylab = "Conspecific abundance")
abline(lm(dat_for_pca$Cons_Abundance ~ PC1), lty = 2)

plot(dat_for_pca$WMA_Releases_by_Yr ~ PC1, ylab = "No. fish released within 40km")
abline(lm(dat_for_pca$WMA_Releases_by_Yr ~ PC1), lty = 2)

plot(dat_for_pca$CV_flow ~ PC1, ylab = "CV of flow")
abline(lm(dat_for_pca$CV_flow ~ PC1), lty = 2)


