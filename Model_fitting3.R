#This script is the final official script for the model fitting process for the 
#model that was ultimately analyzed and described in my manuscript for chapter 1.
#See "Folder_Guide.Rmd" for more info on what the different Model_fittingX.R"
#files are


#Also, an important note: The model was ultimately fit with the removal of an 
#outlier. So, beyond section 1 where I read in the data (immediately below), you
#may skip ahead to section 7 to see the fitting process for the final model I 
#used in the manuscript. Sections 2-6 are retained so you may see my whole process
#from start to finish
library(tidyverse)
library(dplyr)
library(corrgram)
library(Hmisc)
library(lme4)
library(MuMIn)
library(MASS)
library(lattice)
library(performance)
library(caret)


#1. Read in and tailor the data as necessary ###################################
Master_dataset <- read.csv("Master_dataset.csv")
head(Master_dataset)
#Year is a factor
Master_dataset$Year <- factor(Master_dataset$Year)
str(Master_dataset)

#remove Chilkat River, Disappearance Creek, Black River, Saook Bay West Head in
#2010 only, and Herman Creek from analysis. These creeks were sampled either 
#really late bc they are fall-run (Chilkat + Dis.Creek), or early (before 7/20).
delete <- c("Chilkat River", "Disappearance Creek", "Black River", "Herman Creek")
f <- Master_dataset[!(Master_dataset$StreamName %in% delete),]
#only delete 2010 for Saook Bay West Head, not 2011:
f <- f[!(f$Year == "2010" & f$StreamName == "Saook Bay West Head"), ]
#If you do choose to keep these creeks in the future, I'm only deleting them here
#in R. They are still in the original .csv Master_dataset. Note that the Total_
#strays_by_subregion and Total_strays_all_SEAK will differ slightly now because
#those columns were calculated in excel and include the strays from the creeks
#I just deleted^^. So for 2009, if you sum on Number_H_fish, it will give you 411,
#when the Total_strays_all_SEAK column reads 414 for 2009. In 2010, there were 
#557 total strays in SEAK, but with the removal of the above creeks, there are now
#548:
sum(Master_dataset$Number_H_fish[Master_dataset$Year == "2010"]) #557
sum(f$Number_H_fish[f$Year == "2010"]) #548
#Keep this in mind in case you are ever using the total_strays columns for anything

#Change NA values in WMA_Releases_by_Yr to 0
f$WMA_Releases_by_Yr[is.na(f$WMA_Releases_by_Yr)] <- 0

#remove rows containing NA values (9 rows, 3 streams). As I find out later and 
#after reading about missing values in Zuur, it is best to remove the rows con-
#taining NA values since there will be many issues with model validation later
#on if you don't 
f <- f[complete.cases(f), ]
rownames(f) <- 1:nrow(f)
colnames(f)[8] <- "Number_surveys"


### Note that the final version of this model ended up excluding an outlier (you 
#can see why I did that in section 5.6-6). To use/see that final model version,
#skip ahead to section 7 of this script. It includes all the necessary data-
#tailoring steps that this section 1 above here includes as well



#2. Exploratory data analysis ##################################################
#2.1. Check distribution of response variable ==================================
f$Avg_number_strays <- round(f$Avg_number_strays)
hist(f$Avg_number_strays, breaks = 50, main =
       "Histogram of Average # of Strays (Model Response)",
     xlab = "Average # of hatchery-origin strays") #very right-skewed
boxplot(f$Avg_number_strays) #several high outliers
response_table <- as.data.frame(table(f$Avg_number_strays))
head(response_table) #49 zeroes
49/length(f$Year) #28% of the data, maybe zero-inflation?? Check
#to see if model under-predicts the number of 0s later on

#Conclusion: Response data is going to be modeled with Poisson or Negative Bin-
#omial distribution with log-link due to heavy right-skew and because it is count
#data. Plot the LOG of the response against explanatory variables to evaluate 
#potential relationships. 



#2.2. Check for correlations between explanatory variables =====================
library(corrgram) #visually assess first
corrgram(f[ , c(10:19)]) #evidence of several strong correlations

library(Hmisc)
stray_vars <- as.matrix(f[ , c(10:19)])  # Save variables as a matrix 
(COR <- rcorr(stray_vars, type = "spearman"))
#or alternatively, from base R:
cor(f[ , c(10:19)], method = "spearman") #you should use spearman, not pearson
#correlation coefs because your data may not be normal
COR$r   # Matrix of correlations
COR$P   # Matrix of p-values (which correlations are significant)
#Cons_Abundance and Cons_Density are highly correlated (0.89)
#Cons_Abundance and Pink_Abundance possibly correlated (0.46)
#Pink_Abundance and Pink_Density are highly correlated (0.88)
#Dist_nearest_H and Dist_nearest_R are highly correlated (0.69)
#Dist_nearest_H and WMA_Releases_by_Yr are highly correlated (-0.55)
#Dist_nearest_R and WMA_Releases_by_Yr are highly correlated (-0.80)
#Dist_nearest_R and mean_flow are highly correlated (0.52)
#mean_flow and WMA_Releases_by_Yr possibly correlated (-0.456)
#mean_flow and Dist_nearest_R are correlated (0.52)
#CV_flow and Pink_Abundance possibly correlated (-0.46)
#CV_flow borderline with Cons_Abundance, Cons_Density, and Pink_Density
#(-0.36-0.41 correlation)
print(COR$P <= 0.05) #all of the above mentioned correlations are significant

#I will remove Cons_Density and Pink_Density from the model since they are the 
#less important of their correlated sets (mostly bc I don't have a lot of con-
#fidence in the density denominator data (area)). I will also remove Dist_
#nearest_H and Dist_nearest_R since they are correlated with WMA_Releases_by_Yr 
#and each other. WMA_Releases_by_Yr will remain because it provides the most info
f <- f[ , -c(12,14,16,17)]


#2.3. Response~explanatory relationships (ln(response) ~ predictor)) ===========
par(mfrow=c(2,3))
for(i in names(f)[10:15]) {
  x <- f[,i]
  y <- log(f$Avg_number_strays + 1)	#add 1 because you have zeroes
  plot(x, y, xlab=i, ylab="ln(Avg (obs) # of strays)")
  title(i)
}
#Fishery_harvest direct increase, Cons_Abundance & Pink_Abundance inverse rel-
#ationship with response, WMA_Releases_by_Yr increasing, possibly quadratic
#Hard to tell for mean_flow, CV_flow increases with response. Possibility of out-
#liers and quadratic relationships so check for both


### Check for outliers
#Pink_Abundance
outl_P <-influence.measures(lm(log(f$Avg_number_strays + 1) ~ f$Pink_Abundance))
4/length(f$Avg_number_strays) #extreme leverage values will be greater than 0.0226
p1 <- as.data.frame(outl_P[[1]])
length(which(p1$cook.d > 0.0226)) #6 total Pink_Abundance outliers

#Does a log transformation help?
outl_P2 <-influence.measures(lm(log(f$Avg_number_strays + 1) ~ log(f$Pink_Abundance + 1)))
p2 <- as.data.frame(outl_P2[[1]])
length(which(p2$cook.d > 0.0226)) #9 outliers, log transformation does not improve
#outlier situation

#Does a square root transformation help?
outl_P3 <-influence.measures(lm(log(f$Avg_number_strays + 1) ~ sqrt(f$Pink_Abundance)))
p3 <- as.data.frame(outl_P3[[1]])
length(which(p3$cook.d > 0.0226)) #7 outliers, does not help


#mean_flow
outl_M <-influence.measures(lm(log(f$Avg_number_strays + 1) ~ f$mean_flow))
m1 <- as.data.frame(outl_M[[1]])
length(which(m1$cook.d > 0.0226)) #4 outliers

#Does a log transformation help?
outl_M2 <-influence.measures(lm(log(f$Avg_number_strays + 1) ~ log(f$mean_flow)))
m2 <- as.data.frame(outl_M2[[1]])
length(which(m2$cook.d > 0.0226)) #10 outliers after transformation

#Does a square root transformation help?
outl_M3 <-influence.measures(lm(log(f$Avg_number_strays + 1) ~ sqrt(f$mean_flow)))
m3 <- as.data.frame(outl_M3[[1]])
length(which(m3$cook.d > 0.0226)) #9 outliers, transformation does not help


#Pink_Abundance and mean_flow have several outliers. Log and square root trans-
#formations do not help. These points are part of large river systems as well as
#a few streams that did not have any pink salmon in certain years, so they are 
#not errors. I will leave these points in the dataset, leave Pink_Abundance and
#mean_flow untransformed, and revisit the model with these points removed later 
#on


#2.4. Explore distributions of explanatory variables ===========================
for (i in c(10:15)) {
  x <- f[,i]
  hist(x, main = colnames(f)[i])
  print(shapiro.test(x))
} 
#Cons_Abundance, Pink_Abundance, WMA_Releases_by_Yr, and mean_flow are heavily
#right-skewed. Fishery_harvest and CV_flow look closer to a normal distribution
#but are not officially. Leave predictors untransformed for now as it is not 
#strictly necessary and will facilitate easier model interpretation


#2.5. Are there differences between years? =====================================
par(mfrow=c(1,1))
plot(f$Avg_number_strays ~ f$Year) #some differences between years for raw response
plot(log(f$Avg_number_strays + 1) ~ f$Year) #even more so for the log response
summary(aov(log(f$Avg_number_strays + 1) ~ f$Year)) #differences significant be-
#tween years

### Variation in years for explanatory variables 
par(mfrow=c(2,2))
for(i in names(f)[10:15]) {
  x <- f$Year
  y <- f[,i]
  plot(x, y, xlab="Year")
  title(i)
} #clear differences between years for all variables except maybe Cons_Abundance,
#Pink_Abundance, and mean_flow. Year should be included as a random effect as
#expected. Ignore CV_flow and mean_flow plots since their data does not vary
#over time



### Variation in response ~ covariate relationships by year 
#Consider whether you might need random slopes
library(lattice)
for (i in names(f)[10:15]) {
  print(xyplot(log(Avg_number_strays+1) ~ f[,i]|Year, data = f, type = c("p","r"),
               xlab = i))
}
#Some relationships vary greatly across years, but I think this has to do with
#variation in which sites were included and not inherent difference in the slope
#of the relationship(?). However, given that the response does differ signif-
#icantly by year, I should include year as a random intercept



#2.6. Are observations spatially and temporally independent? ===================
### Not a lot of (visual + subjective) evidence for temporal dependence looking
#at the boxplots (run two for loops above). Remember to check for temporal auto-
#correlation in the residuals later on

### Check for spatial autocorrelation in the response
#I have multiple years of data with considerable variation between years, so I
#will average the # of strays across years to check for spatial autocorrelation

#Create table of average of Avg_number_strays and link coordinates to it
StreamPoints <- read.csv("~/Documents/CHUM_THESIS/Data Sources/StreamPoints.csv")
spat <- inner_join(f, StreamPoints, by = "StreamName")
spatio_sum <- spat %>% group_by(StreamName) %>% summarise(Number_Strays =
                                                            mean(Avg_number_strays))
spatio_sum2 <- merge(spatio_sum, spat[ , c("StreamName", "Latitude", "Longitude")],
                     by = "StreamName", all.x = T) #attach coordinates
spatio_sum3 <- unique(spatio_sum2) #the above created a row for each year for 
#each stream, so remove the duplicate rows
#export in case you want for later reference:
setwd("~/Documents/CHUM_THESIS/Manuscript/Suppl_Materials/Spatial_AutoC")
write.csv(spatio_sum3, "Number_strays_by_Yr_w_coords.csv")

#Check for spatial autocorrelation using geodesic distances following the example
#of https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/ 
library(geosphere)
dist_mat <- distm(cbind(spatio_sum3$Longitude, spatio_sum3$Latitude)) #I double-
#checked some of these distances on google earth and besides being on different
#scales, they look good. For example, point 1 and point 2 (Admiralty and Amber 
#Creeks, respectively) are ~95km away from one another, and the distm() function
#returned 94878.92 as the distance between them (in meters it looks like)
dist_mat <- dist_mat/100000 #this step was to get distances on the same scale as
#was done in the example I'm following, which is 0.949 for two points (stream 
#mouths) that are 94.9km away from each other, as in the case of Admiralty and 
#Amber Creek. I don't really understand why the scaling like this (tenths of a
#km?) but I did what the example did so that I could produce similar resultados
dist_mat_inv <- 1/dist_mat
diag(dist_mat_inv) <- 0

library(ape)
Moran.I(spatio_sum3$Number_Strays, dist_mat_inv) #p = 0.308, fail to reject 
#null = no spatial autocorrelation occurring 


### Section 2 - EDA conclusions and actions:
# Response is count data and heavily right-skewed -> use Poisson or NB distribution
#when modeling

# Several variables were collinear. In cases where collinearity was > 0.5 between
#two variables, one of them was removed because I want to be able to interpret 
#coefficients, not just make predictions. Fishery_harvest, Cons_Abundance, Pink_
#Abundance, WMA_Releases_by_Yr, mean_flow, and CV_flow remain

# There might be(?) some non-linear relationships between certain covariates
#and the response. It was hard to tell visually what relationships there might
#be. Still, be sure to test out higher order model terms

# There are significant differences between years for most variables, especially 
#the response. Include YEAR as a random effect (intercept), mainly because I would
#like to make predictions beyond my study streams and generalize covariate info
#over multiple years. No random slopes

# No evidence of spatial autocorrelation in the response. Probably no temporal
#autocorrelation either but double-check it in your residuals later on



#3. Scale covariates ###########################################################
### Scale across all years because you are trying to make inferences across time,
#not by year (OK because you will have year as a random effect)
m <- apply(f[ , c(10:15)], 2, scale.default)
f_scaled <- cbind.data.frame(f[ , c(1:9)], m)
head(f_scaled) #Note that scaling is required. I tried fitting glmer.nb earlier 
#on without scaling and the model would not converge and returned several warnings



#4. Model fitting ##############################################################
#4.1. Check for overdispersion in the response (likely) ========================
### Determine mean and variance of evenly split chunks of data
chunk2 <- function(x,n) split(x, cut(seq_along(log(f$Avg_number_strays+1)), n,
                                     labels = FALSE))
f_split <- chunk2(sort(log(f$Avg_number_strays+1)), 17) #split into 17 groups
f_means <- lapply(f_split, mean)
f_means <- data.frame(matrix(unlist(f_means), nrow=length(f_means), byrow=F))
colnames(f_means)[1] <- "means"
f_means <- unlist(f_means)
f_variances <- lapply(f_split, var)
f_variances <- data.frame(matrix(unlist(f_variances), nrow=length(f_variances),
                                 byrow=F))
colnames(f_variances)[1] <- "variances"
f_variances <- unlist(f_variances)
#How do mean and variance relate to one another?
plot(f_means, f_variances)
fit <- lm(f_variances ~ f_means + I(f_means^2))
curve(predict(fit, newdata = data.frame(f_means=x)), add=T)
#Variance substantially greater than the mean at high values. In fact, the quad-
#ratic relationship of the lm fit above (variance ~ mean^2) seems to approximate
#the relationship well minus one outlier
#CONCLUSION: Use negative binomial probability distribution to fit model due to 
#apparent overdispersion


#4.2. Fit global model with negative binomial distribution =====================
#Curry says you should fit and compare models using REML = FALSE. Based on google
#search, I believe the default for glmer.nb does not use REML
#Global model:
stray_1 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                      Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                      mean_flow + CV_flow, data = f_scaled)
#if you need more iterations, include this after your data argument"
#control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) 
AICc(stray_1, REML = F) #958.07


#4.3. Check for higher order terms =============================================
### Possible quadratic relationships:
for(i in c(10:15)){
  mod <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                    Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                    mean_flow + CV_flow + I(f_scaled[ , i]^2), data = f_scaled)
  print(AICc(mod, REML = F))
} #Pink_Abundance, WMA_Releases_by_Yr, and CV_flow quadratic terms improve the 
#model fit, though it is by < 2 AICc points for Pink_Abundance
stray_1.1 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                        Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                        mean_flow + CV_flow + I(WMA_Releases_by_Yr^2),
                      data = f_scaled)
summary(stray_1.1) #WMA_Releases_by_Yr is almost perfectly correlated with its
#squared term (-0.934), so it should be excluded from the model

stray_1.2 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                        Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                        mean_flow + CV_flow + I(CV_flow^2), data = f_scaled)
lmtest::lrtest(stray_1, stray_1.2) #CV_flow^2 improves model fit
summary(stray_1.2) #no issues with collinearity. Keep CV_flow^2 term 
AICc(stray_1.2) #893.61


### Possible interactions:
stray_2 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                      Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                      mean_flow + CV_flow + I(CV_flow^2) +
                      WMA_Releases_by_Yr:mean_flow, data = f_scaled)
AICc(stray_1.2) #current best AICc = 893.61
AICc(stray_2)
#Fishery_harvest:Cons_Abundance = 895.84
#Fishery_harvest:Pink_Abundance = 895.59
#Cons_Abundance:Pink_Abundance = 893.21
#Cons_Abundance:mean_flow = 894.04
#Cons_Abundance:CV_flow = 895.86
#Pink_Abundance:mean_flow = 894.95
#Pink_Abundance:CV_flow = 893.20
#WMA_Releases_by_Yr:Cons_Abundance = 893.83
#WMA_Releases_by_Yr:Pink_Abundance = 895.79
#WMA_Releases_by_Yr:mean_flow = 891.72
#WMA_Releases_by_Yr:CV_flow = 895.84
lmtest::lrtest(stray_1.2, stray_2) #the greatest reduction in AICc was with the 
#WMA_Releases_by_Yr:mean_flow interaction and passes the LRT
summary(stray_2) #However, WMA_Releases_by_Yr:mean_flow has collinearity 
#issues. Do not include it in the model

#Current best model:
stray_1.2
summary(stray_1.2) #no correlations between variables > 0.4
car::vif(stray_1.2) #all are < 1.6


### Preliminary diagnostics of global model
par(mfrow=c(1,1))
plot(stray_1.2) #model tends to overpredict slightly 
par(mfrow=c(1,1))
plot(residuals(stray_1.2, type = "deviance") ~ fitted(stray_1.2))
hist(residuals(stray_1.2, type = "deviance"))
hist(residuals(stray_1.2, type = "pearson"))
qqnorm(residuals(stray_1.2, type = "deviance"))
qqnorm(residuals(stray_1.2, type = "pearson"))

par(mfrow=c(2,3))
resid_cov <- function(mod){
  for(i in c(10:15)){ 
    plot(residuals(mod, type = "deviance") ~ f_scaled[ , i]) #change residual
    #type back and forth between "deviance" and "pearson" depending on what you
    #want to see
    title(colnames(f_scaled)[i])
  }
}
resid_cov(stray_1.2)
#acceptable enough to proceed, though note there seems to be one major outlier
# on qqnorm plots that you may need to look into further


summary(stray_1.2) 
#mean_flow is HIGHLY insignificant (p = 0.93). Remove from future versions of 
#model
AICc(stray_1.2, REML = F) #893.61

stray_2 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                      Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                      CV_flow + I(CV_flow^2), data = f_scaled)
summary(stray_2) #no correlations > 0.4
car::vif(stray_2) #all are < 1.5
AICc(stray_2, REML = F) #891.37



#4.4. Check for spurious relationships before final dredge =====================
#I.e., do any of the response~covariate relationships differ when fit individually
#compared to the full model fit?
harvest <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest, data = f_scaled)
Cons_A <- glmer.nb(Avg_number_strays ~ (1|Year) + Cons_Abundance, data = f_scaled)
Pink_A <- glmer.nb(Avg_number_strays ~ (1|Year) + Pink_Abundance, data = f_scaled)
WMA <- glmer.nb(Avg_number_strays ~ (1|Year) + WMA_Releases_by_Yr, data = f_scaled)
CV_f <- glmer.nb(Avg_number_strays ~ (1|Year) + CV_flow + I(CV_flow^2), data = f_scaled)
#Convergence warnings were issued for several of these models, but the relationships
#within them seem to visually match their expectations based on EDA:
print(coef(harvest)); print(coef(Cons_A)); print(coef(Pink_A)); print(coef(WMA));
print(coef(CV_f))
par(mfrow=c(2,2))
for (i in c(10:13, 15)) {
  x <- f_scaled[,i]
  y <- log(f_scaled$Avg_number_strays+1)
  plot(x, y)
  title(colnames(f_scaled)[i])
}


#Compare to overall model fit
summary(stray_2)
summary(harvest) #0.10 (p = 0.45) in stray_2, 0.32 (p = 0.07) in individual mod
summary(Cons_A) #-0.31 (p = 0.02) in stray_2, -0.77 (p = 0) in individual mod
summary(Pink_A) #0.16 (p = 0.19) in stray_2, -0.52 (p = 0) in individual mod
summary(WMA) #0.39 (p = 0) in stray_2, 1.07 (p = 0) in individual mod
summary(CV_f) #0.44 + 0.70^2 (p = 0) in stray_2, 0.48 + 0.77^2 (p = 0) in ind. mod
#Pink_Abundance is totally spurious! Magnitude and sign change greatly between 
#models. Furthermore, stray_2 coef estimate for Pink_Abundance is positive when
#it appears that Pink_Abundance is negatively correlated with response in plot
#CONCLUSION: Remove Pink_Abundance from model. Other covariates OK

stray_3 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                      Cons_Abundance + WMA_Releases_by_Yr + CV_flow +
                      I(CV_flow^2), data = f_scaled)#, control =
                      #glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1e6)))
AICc(stray_3, REML = F) #890.93
car::vif(stray_3) #OK
summary(stray_3) #Cons_Abundance-CV_flow correlation = 0.404, all others < 0.4


#4.5. Dredge model (MuMIn::dredge()) ===========================================
options(na.action = "na.fail") #there shouldn't be any NAs, but you need to run 
#this line anyway to use MuMIn:dredge() or it will get angry
sapply(f_scaled, function(x) sum(is.na(x))) #confirmed, no NAs
straymod_dredge <- dredge(stray_3, rank = "AICc")
#I'm getting convergence issue warnings, even with the new optimizer and increase
#in max iterations in glmerControl above. But when I check the coefficient esti-
#mates in head(straymod_dredge) below, they are the close to the same as in the
#summary(stray_3) output, which did not have convergence issues. So, I think it
#is fine to proceed(?)

head(straymod_dredge)
#The top two models account for 78% of the total model weight, so I will average
#the results from those two and disregard the remaining models (I never actually
#end up doing the model averaging for these two models below bc I find in section
#6 that there is an outlier worth removing, therefore I fit new models using an
#updated dataset in section 7. If you do end up returning to these two models here
#do NOT average their predictions Just use bm1. See note in section 7.3 for why
#this is)
bm1 <- glmer.nb(Avg_number_strays ~ (1|Year) + Cons_Abundance + WMA_Releases_by_Yr
                + CV_flow + I(CV_flow^2), data = f_scaled) #'bm' for "best model"
bm2 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest + Cons_Abundance +
                  WMA_Releases_by_Yr + CV_flow + I(CV_flow^2), data = f_scaled)
#Note that creating new model fits (bm1, bm2) is not exactly equivalent to extrac-
#ting the model coefficients from the MuMIn::dredge() object. I created new model
#objects so that I could make predictions from the model separately, and then 
#average the predictions while leaving the coefficients alone. When comparing 
#coefs in summary(bmX) to the equivalent model in straymod_dredge, I find that 
#the coefficient estimates only differ at the 4th decimal place (if they even do
#differ), so I think my approach is acceptable




#5. Model diagnostics ##########################################################
#5.1. Compare to null model ====================================================
AICc(bm1, bm2, glm.nb(Avg_number_strays ~ 1, data = f_scaled)) #null model is 
#significantly worse
AICc(bm1, bm2, glmer.nb(Avg_number_strays ~ (1|Year), data = f_scaled)) #also true
#with Year as random intercept

lmtest::lrtest(bm1, glmer.nb(Avg_number_strays ~ (1|Year), data = f_scaled))


#5.2. Residual diagnostics =====================================================
### (Deviance) residuals ~ fitted values plot 
par(mfrow=c(1,1))
plot(residuals(bm1, type = "deviance") ~ fitted(bm1), main = "Model #1-outlier included")
plot(residuals(bm2, type = "deviance") ~ fitted(bm2), main = "Model #2-outlier included")

### For manuscript: ggplot2 version which compares models with and w/o outlier
outlieryes <- data.frame(deviance_resid = residuals(bm1, type = "deviance"),
                        pred = fitted(bm1))
dev_outlieryes <- ggplot(outlieryes, aes(pred, deviance_resid)) + geom_point() +
  xlab("Model predicted values") + ylab("Deviance residuals") + theme_bw()
dev_outlieryes


#Pearson residuals
plot(residuals(bm1, type = "pearson") ~ fitted(bm1), main = "Model #1-outlier included")
plot(residuals(bm2, type = "pearson") ~ fitted(bm2), main = "Model #2-outlier included")
#In both the deviance and pearson residuals, it seems that there are some larger
#residuals at smaller fitted values (i.e., cone shape). However, there are so
#few of the high extreme values (>50 strays) that it is hard to say whether there
#is actually less spread at this level or if there just isn't enough data. The 
#best way to evaluate your model will be to cross validate it and evaluate pred-
#iction error


### Normality of the residuals
par(mfrow=c(1,2))
hist(residuals(bm1, type = "deviance"), main = "Model #1-outlier included")
hist(residuals(bm2, type = "deviance"), main = "Model #2-outlier included")

#Some slight right-skew in histograms
qqnorm(residuals(bm1, type = "deviance"), main = "Model #1-outlier included")
qqnorm(residuals(bm2, type = "deviance"), main = "Model #2-outlier included")
qqnorm(residuals(bm1, type = "pearson"), main = "Model #1-outlier included")
qqnorm(residuals(bm2, type = "pearson"), main = "Model #2-outlier included")


### Residuals ~ predictor variables
par(mfrow=c(2,2))
resid_cov <- function(mod, dat){
  for(i in c(10,11,13,15)){ #add column 10 for bm2
    plot(residuals(mod, type = "deviance") ~ dat[ , i])
    title(colnames(dat)[i])
  }
}
resid_cov(bm1, f_scaled) 
par(mfrow=c(2,2)) #need to run again each time
resid_cov(bm2, f_scaled)
#no obvious patterns


### Evaluate leverage
ggplot(data.frame(lev = hatvalues(bm1),
                  deviance = residuals(bm1, type = "deviance")),
       aes(x = lev, y = deviance)) + geom_point() + theme_bw()
ggplot(data.frame(lev = hatvalues(bm2),
                  deviance = residuals(bm2, type = "deviance")),
       aes(x = lev, y = deviance)) + geom_point() + theme_bw()
#The high leverage points do not have extreme residual values. Though note the 
#warning that outputs with the hatvalues function



#5.3. Random effect diagnostics ================================================
#Random effect should be normally distributed
qqmath(lme4::ranef(bm1)) #qqmath from library(lattice)
qqmath(lme4::ranef(bm2)) 
#All look approximately normally distributed

#Intraclass correlation coefficient
icc(bm1) #icc from library(performance)
icc(bm2)
#none are = 0, indicating that random effects are necessary 



#5.4. Check for temporal autocorrelation =======================================
par(mfrow=c(1,1))
temp <- function(mod, dat){
  E <- residuals(mod, type = "deviance")
  I1 <- !is.na(dat$Avg_number_strays)
  Efull <- vector(length = length(dat$Avg_number_strays))
  Efull <- NA
  Efull[I1] <- E
  acf(Efull, na.action = na.pass)
}
temp(bm1, f_scaled)
temp(bm2, f_scaled) #no evidence of temporal autocorrelation



#5.5. Cross-validate each model ================================================
### Calculate the mean absolute error for each model
mean_mae <- vector(length = 10)
mae_bm1 <- vector(length = 500)
#Note that the prediction accuracy varies greatly depending on how large the obs.
#value is (much greater error for large values because there are fewer of them).
#Observe this by excluding the largest observed values in the model fitting with
#the sub_f_scaled dataset
sub_f_scaled <- f_scaled[f_scaled$Avg_number_strays < 20,]
#Best model (bm1):
for(j in 1:10){
  for (i in 1:500) {
    train <- sub_f_scaled %>% #change data between f_scaled and sub_f_scaled
      group_by(Year) %>% mutate(group = sample(n())/n() > 0.7) #0.7 for a 70-30
    #cross-validation
    
    splitdf <- split(train, train$group)
    training <- splitdf[["FALSE"]]
    test <- splitdf[["TRUE"]]
    
    predictions <- bm1 %>% predict(test)
    #remember to exp() bc the glmer.nb output is in log space
    exp.pred <- exp(predictions)
    mae <- MAE(exp.pred, test$Avg_number_strays, na.rm = TRUE)
    mae_bm1[i] <- mae
  }
  mean_mae[j] <- mean(mae_bm1)
}
mean_mae
mean(mean_mae) #for f_scaled (full dataset including large obs. values), the mean
#MAE is 7.23. For sub_f_scaled, it is only 4.31



#5.6. Examine predicted values =================================================
bm1_pred <- as.data.frame(fitted(bm1))
bm1_pred <- cbind.data.frame(f_scaled$Year, f_scaled$StreamName, bm1_pred,
                                f_scaled$Avg_number_strays)
new_names <- c("Year", "StreamName", "Predicted", "Observed")
bm1_pred <- bm1_pred %>% rename_at(1:4, ~ new_names)
plot(Observed ~ Predicted, data = bm1_pred, main = "Model #1-outlier included")
plot(Observed ~ Predicted, data = bm1_pred, xlim=range(0,50),
     main = "Model #1-outlier included, lim x-range")
abline(0,1)
abline(lm(Observed ~ Predicted, data = bm1_pred), lty = 2)

#ggplot version for manuscript:
lm_yes <-lm(Observed ~ Predicted, data = bm1_pred)
summary(lm_yes)
OP_outlieryes <- ggplot(bm1_pred, aes(Predicted, Observed)) + geom_point() +
  geom_abline(slope = 0.624, intercept = 2.62) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed") + theme_bw()
OP_outlieryes

#Export as high-res figure
tiff("OP_outlieryes.tiff", width = 7, height = 6, pointsize = 12, units = 'in', res = 300)
OP_outlieryes #graph that you want to export
dev.off( ) 
#OR combine with same plot that has the outlier removed below (section 7.4)

#Averages by stream
mean_bm1_pred <- bm1_pred %>% group_by(StreamName) %>%
  summarise(Mean_pred_strays = mean(Predicted), Mean_obs_strays = mean(Observed))
mean_bm1_pred
plot(Mean_obs_strays ~ Mean_pred_strays, data = mean_bm1_pred,
     main = "Mean predictions by stream for Model 1-outlier included")
plot(Mean_obs_strays ~ Mean_pred_strays, data = mean_bm1_pred,
     main = "Mean predictions by stream for Model 1-outlier included (lim x-range)",
     xlim=range(0,20))
abline(0,1)
abline(lm(Mean_obs_strays ~ Mean_pred_strays, data = mean_bm1_pred), col = "red")


#Are there outliers?
#In section 2.3 (EDA), outliers were identified for Pink_Abundance and mean_flow
#covariates, neither of which remain in the final model. However, there is an 
#apparent outlier on the qqnorm plot that may be having some influence on the fit
out <- cooks.distance(influence(bm1, obs = T))
class(out)
out <- as.vector(out)
which(out > 0.2) #162 is a big outlier. Remove and refit
identify(qqnorm(residuals(bm1, type = "pearson"), main = "Model #1 pearson residuals"))
#confirmed by qqnorm plot





#6. Compare models fit without outliers ########################################
stray_4 <- glmer.nb(Avg_number_strays ~ (1|Year) + Cons_Abundance +
                      WMA_Releases_by_Yr + CV_flow + I(CV_flow^2), data =
                      f_scaled[-162,])
summary(stray_4)
AICc(stray_4, REML = F) #869.79

summary(bm1)
AICc(bm1, REML = F) #888.85, very similar models but better AICc for model with
#outlier removed. Cons_Abundance is no longer significant without outlier

### Diagnostics
qqnorm(residuals(stray_4, type = "deviance"), main = "outlier removed-deviance")
qqnorm(residuals(stray_4, type = "pearson"), main = "outlier removed-pearson")


#See if it improved predictions
stray_4_pred <- as.data.frame(fitted(stray_4))
stray_4_pred <- cbind.data.frame(f_scaled[-162,]$Year, f_scaled[-162,]$StreamName, 
                                 stray_4_pred, f_scaled[-162,]$Avg_number_strays)
stray_4_pred <- stray_4_pred %>% rename_at(1:4, ~ new_names)
plot(Observed ~ Predicted, data = stray_4_pred, main = "outlier-removed")
plot(Observed ~ Predicted, data = stray_4_pred, xlim=range(0,20), ylim=range(0,50),
     main = "outlier-removed, lim x-range")
abline(0,1)
abline(lm(Observed ~ Predicted, data = stray_4_pred), col = "red")
#Seems to predict slightly better without outlier

### Cross-validate top model without outlier
mean_mae_no_outlier <- vector(length = 10)
mae_stray_4 <- vector(length = 500)
#Note that the prediction accuracy varies greatly depending on how large the obs.
#value is (much greater error for large values because there are fewer of them).
#Observe this by excluding the largest observed values in the model fitting with
#the sub_f_scaled dataset (which will also exclude the outlier)
sub_f_scaled <- f_scaled[f_scaled$Avg_number_strays < 20,]
#Best model (bm1):
for(j in 1:10){
  for (i in 1:500) {
    train <- sub_f_scaled %>% #change data between f_scaled[-162,] and sub_f_scaled
      group_by(Year) %>% mutate(group = sample(n())/n() > 0.7) #0.7 for a 70-30
    #cross-validation
    
    splitdf <- split(train, train$group)
    training <- splitdf[["FALSE"]]
    test <- splitdf[["TRUE"]]
    
    predictions <- stray_4 %>% predict(test)
    #remember to exp() bc the glmer.nb output is in log space
    exp.pred <- exp(predictions)
    mae <- MAE(exp.pred, test$Avg_number_strays, na.rm = TRUE)
    mae_stray_4[i] <- mae
  }
  mean_mae_no_outlier[j] <- mean(mae_stray_4)
}
mean_mae_no_outlier
mean(mean_mae_no_outlier) #for f_scaled (full dataset including large obs. values),
#the mean MAE is 6.64. For sub_f_scaled, it is only 3.87

#CONCLUSION: The model makes predictions significantly better with the removal of
#the outlier in row 162 (Ketchikan Creek 2010). While I do not believe this point
#to be an error, removing greatly improves the model's predictive power, which is
#the ultimate goal for this particular model. Therefore, I will proceed with re-
#fitting the model without this outlier





#7. Refit model without outlier (Ketchikan Creek 2010) #########################
#7.1 EDA =======================================================================
### Create dataframe
delete <- c("Chilkat River", "Disappearance Creek", "Black River", "Herman Creek")
f_update <- Master_dataset[!(Master_dataset$StreamName %in% delete),]
#only delete 2010 for Saook Bay West Head, not 2011:
f_update <- f_update[!(f_update$Year == "2010" &
                         f_update$StreamName == "Saook Bay West Head"), ]
f_update$WMA_Releases_by_Yr[is.na(f_update$WMA_Releases_by_Yr)] <- 0
f_update <- f_update[complete.cases(f_update), ]
rownames(f_update) <- 1:nrow(f_update)
f_update <- f_update[-162,]
rownames(f_update) <- 1:nrow(f_update)
colnames(f_update)[8] <- "Number_surveys"

### Examine response variable
hist(f_update$Avg_number_strays, breaks = 50)
boxplot(f_update$Avg_number_strays)
#round the response variable to the nearest integer
f_update$Avg_number_strays <- round(f_update$Avg_number_strays)


### Check for correlations
library(corrgram) #visually assess first
corrgram(f_update[ , c(10:19)])

library(Hmisc)
stray_vars2 <- as.matrix(f_update[ , c(10:19)])  # Save variables as a matrix 
COR2 <- rcorr(stray_vars2, type = "spearman")
COR2$r #Same conclusions as before. Remove Cons_Density, Pink_Density, Dist_
#nearest_H, and Dist_nearest_R due to collinearity
f_update <- f_update[ , -c(12,14,16,17)]

### Response-explanatory variable relationships
par(mfrow=c(1,2))
for(i in names(f_update)[10:15]) {
  x <- f_update[,i]
  y <- log(f_update$Avg_number_strays+1)	#add 1 because you have zeroes
  plot(x, y, xlab=i, ylab="ln(# of strays)")
  title(i)
} #looks the same as previously


### Differences between years
plot(f_update$Avg_number_strays ~ f_update$Year) #some differences between years
#for raw response
plot(log(f_update$Avg_number_strays + 1) ~ f_update$Year) #even more so for the
#log response
summary(aov(log(f_update$Avg_number_strays + 1) ~ f_update$Year)) #differences
#significant between years

for (i in names(f_update)[10:15]) {
  print(xyplot(Avg_number_strays ~ f_update[,i]|Year, data = f_update,
               type = c("p","r"), xlab = i))
} #Relationships generally appear consistent with those visually identifiable in
#the for loop ~20 lines above

#EDA Conclusions for model fit without Ketchikan Creek in 2010: Just the same as
#before; I removed collinear variables, leaving Fishery_harvest, Cons_Abundance,
#Pink_Abundance, WMA_Releases_by_Yr, mean_flow, and CV_flow to be tested in the
#model. I will fit the data to a negative binomial distribution with a log-link
#and include year as a random effect



#7.2 Scale covariates ==========================================================
n <- apply(f_update[ , c(10:15)], 2, scale.default)
fu_scaled <- cbind.data.frame(f_update[ , c(1:5,8:9)], n)
head(fu_scaled)



#7.3. Fit model ================================================================
#Global model:
stray_1u <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                      Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                      mean_flow + CV_flow, data = fu_scaled)
AICc(stray_1u, REML = F) #922.22

### Check for higher order terms
# Possible quadratic relationships:
for(i in c(8:13)){
  mod <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                    Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                    mean_flow + CV_flow + I(fu_scaled[ , i]^2), data = fu_scaled)
  print(AICc(mod, REML = F))
}

stray_1.1u <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                         Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                         mean_flow + CV_flow + I(Fishery_harvest^2),
                       data = fu_scaled)
AICc(stray_1.1u, REML = F) #920.42
summary(stray_1.1u) #no collinearity issues
lmtest::lrtest(stray_1u, stray_1.1u) #Fishery_harvest^2 significantly improves fit

stray_1.2u <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                         Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                         mean_flow + CV_flow + I(Pink_Abundance^2),
                       data = fu_scaled)
AICc(stray_1.2u, REML = F) #914.46
summary(stray_1.2u) #Pink_Abundance highly collinear with squared term. Do not
#include squared term

stray_1.3u <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                         Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                         mean_flow + CV_flow + I(WMA_Releases_by_Yr^2),
                       data = fu_scaled)
AICc(stray_1.3u, REML = F) #912.48
summary(stray_1.3u) #WMA_Releases_by_Yr almost perfectly collinear with squared
#term. Do not include squared term in model

stray_1.4u <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                         Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                         mean_flow + CV_flow + I(CV_flow^2),
                       data = fu_scaled)
AICc(stray_1.4u, REML = F) #872.84
summary(stray_1.4u) #no collinearity issues

#Fishery_harvest^2 and CV_flow^2 both improve model fit separately, but they may
#not together. Check to see:
stray_1.5u <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                         Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                         mean_flow + CV_flow + I(Fishery_harvest^2) +
                         I(CV_flow^2), data = fu_scaled)
AICc(stray_1.5u, REML = F) #874.60
summary(stray_1.5u) #no collinearity issues
#Compare Fishery_harvest^2, CV_flow^2, and Fishery_harvest^2 + CV_flow^2 models:
AICc(stray_1.1u, stray_1.4u, stray_1.5u, REML = F)
lmtest::lrtest(stray_1.4u, stray_1.5u) #the simpler model is better, that is,
#the model with only CV_flow^2 term (stray_1.4u)


# Possible interactions:
stray_2u <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                         Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                         mean_flow + CV_flow + I(CV_flow^2) +
                       WMA_Releases_by_Yr:mean_flow, data = fu_scaled)
AICc(stray_1.4u, REML = F) #current best = 872.84
AICc(stray_2u, REML = F)
#Fishery_harvest:Cons_Abundance = 875.10
#Fishery_harvest:Pink_Abundance = 875.11
#Cons_Abundance:Pink_Abundance = 873.87
#Cons_Abundance:mean_flow = 872.16
#Cons_Abundance:CV_flow = 873.47
#Pink_Abundance:mean_flow = 873.48
#Pink_Abundance:CV_flow = 875.00
#WMA_Releases_by_Yr:Cons_Abundance = 872.20
#WMA_Releases_by_Yr:Pink_Abundance = 874.98
#WMA_Releases_by_Yr:mean_flow = 870.17
#WMA_Releases_by_Yr:CV_flow = 875.12
summary(stray_2u) #the only model that improved the fit by more than 0.5 AICc 
#points was the one that included WMA_Releases_by_Yr:mean_flow interaction. How-
#ever, this interaction term is perfectly collinear with WMA_Releases_by_Yr and
#mean_flow, so I will exclude it. 
#CONCLUSION: No interaction terms needed


### Preliminary diagnostics of global model
par(mfrow=c(1,1))
plot(stray_1.4u) #model tends to overpredict slightly 
plot(residuals(stray_1.4u, type = "deviance") ~ fitted(stray_1.4u))
hist(residuals(stray_1.4u, type = "deviance"))
hist(residuals(stray_1.4u, type = "pearson"))
qqnorm(residuals(stray_1.4u, type = "deviance"))
qqnorm(residuals(stray_1.4u, type = "pearson"))

par(mfrow=c(2,3))
resid_cov <- function(mod){
  for(i in c(8:13)){ 
    plot(residuals(mod, type = "deviance") ~ fu_scaled[ , i]) #change residual
    #type back and forth between "deviance" and "pearson" depending on what you
    #want to see
    title(colnames(fu_scaled)[i])
  }
}
resid_cov(stray_1.4u)
#acceptable enough to proceed I think 

summary(stray_1.4u) 
#Fishery_harvest and mean_flow HIGHLY insignificant (p > 0.8). Remove from future
#versions of model
AICc(stray_1.4u, REML = F) #872.84

stray_2u <- glmer.nb(Avg_number_strays ~ (1|Year) + Cons_Abundance +
                       Pink_Abundance + WMA_Releases_by_Yr + CV_flow +
                       I(CV_flow^2), data = fu_scaled)
summary(stray_2u) #no correlations > 0.5, but Pink_Abundance and CV_flow cor = 0.45
car::vif(stray_2u) #all are <= 1.5
AICc(stray_2u, REML = F) #868.51


### Check for spurious relationships before final dredge 
#I.e., do any of the response~covariate relationships differ when fit individually
#compared to the full model fit?
Cons_Au <- glmer.nb(Avg_number_strays ~ (1|Year) + Cons_Abundance, data = fu_scaled)
Pink_Au <- glmer.nb(Avg_number_strays ~ (1|Year) + Pink_Abundance, data = fu_scaled)
WMAu <- glmer.nb(Avg_number_strays ~ (1|Year) + WMA_Releases_by_Yr, data = fu_scaled)
CV_fu <- glmer.nb(Avg_number_strays ~ (1|Year) + CV_flow + I(CV_flow^2), data = fu_scaled)
#Convergence warnings were issued for several of these models, but the relationships
#within them seem to visually match their expectations based on EDA:
print(coef(Cons_Au)); print(coef(Pink_Au)); print(coef(WMAu)); print(coef(CV_fu))
par(mfrow=c(2,2))
for (i in c(9:11,13)) {
  x <- fu_scaled[,i]
  y <- log(fu_scaled$Avg_number_strays+1)
  plot(x, y)
  title(colnames(fu_scaled)[i])
}

#Compare to overall model fit
summary(stray_2u)
summary(Cons_Au) #-0.23 (p = 0.053) in stray_2u, -0.77 (p = 0) in individual mod
summary(Pink_Au) #0.21 (p = 0.07) in stray_2u, -0.52 (p = 0) in individual mod
summary(WMAu) #0.46 (p = 0) in stray_2u, 1.09 (p = 0) in individual mod
summary(CV_fu) #0.59 + 0.60^2 (p = 0) in stray_2u, 0.54 + 0.72^2 (p = 0) in ind. mod
#Pink_Abundance is totally spurious! Magnitude and sign change greatly between 
#models. Furthermore, stray_2u coef estimate for Pink_Abundance is positive when
#it appears that Pink_Abundance is negatively correlated with response in plot
#CONCLUSION: Remove Pink_Abundance from model. Other covariates OK

stray_3u <- glmer.nb(Avg_number_strays ~ (1|Year) + Cons_Abundance +
                       WMA_Releases_by_Yr + CV_flow + I(CV_flow^2),
                     data = fu_scaled)
AICc(stray_3u, REML = F) #869.79
car::vif(stray_3u) #OK
summary(stray_3u) #Cons_Abundance:CV_flow correlation = 0.390, all others < 0.4



### Dredge model (MuMIn::dredge()) =============================================
options(na.action = "na.fail") #there shouldn't be any NAs, but you need to run 
#this line anyway to use MuMIn:dredge()
sapply(fu_scaled, function(x) sum(is.na(x))) #confirmed, no NAs
straymod_dredge_u <- dredge(stray_3u, rank = "AICc")
head(straymod_dredge_u) #I'm getting convergence warnings, but I don't know which
#models they are for in the dredged set. The coefficient estimates for the top
#model match those in stray_3u (which did not have convergence issues), so I'm
#not going to worry about it

#The top two models account for 100% of the total model weight (does that have
#to do with convergence issues?). I will average the results from those two and 
#disregard the remaining models
bm1u <- glmer.nb(Avg_number_strays ~ (1|Year) + Cons_Abundance + WMA_Releases_by_Yr
                + CV_flow + I(CV_flow^2), data = fu_scaled) #'bm' for "best model"
bm2u <- glmer.nb(Avg_number_strays ~ (1|Year) + WMA_Releases_by_Yr + CV_flow +
                   I(CV_flow^2), data = fu_scaled)
#Note that creating new model fits (bm1u and bm2u) is not exactly equivalent to
#extracting the model coefficients from the MuMIn::dredge() object. I created new
#model objects so that I could make predictions from the model separately, and 
#then average the predictions while leaving the coefficients alone. When comparing 
#coefs in summary(bmXu) to the equivalent model in straymod_dredge_u, I find that 
#the coefficient estimates only differ at the 3rd decimal place (if they even do
#differ), so I think my approach is acceptable


### UPDATE 3/22/22 ###
#I will not average the two models' predictions anymore, based on coauthor sugg-
#estion to not go to the trouble of doing so bc the top model accounts for most
#of the weight and had a slightly higher AICc despite having an additionl covar-
#iate. Therefore, you should be able to explain patterns in straying OK with
#just the top model. However, I retain the second model (bm2u) because it will
#be needed for chapter 2 analysis




#7.4. Model diagnostics ########################################################
### Compare to null model
AICc(bm1u, bm2u, glm.nb(Avg_number_strays ~ 1, data = fu_scaled)) #null model is 
#significantly worse
AICc(bm1u, bm2u, glmer.nb(Avg_number_strays ~ (1|Year), data = fu_scaled)) #also
#true with Year as random intercept. Note that I think the true null model is this
#one; the one that includes the random effect

lmtest::lrtest(bm1u, glmer.nb(Avg_number_strays ~ (1|Year), data = fu_scaled))


### Residual diagnostics
### (Deviance) residuals ~ fitted values plot 
par(mfrow=c(1,1))
plot(residuals(bm1u, type = "deviance") ~ fitted(bm1u),
     main = "Model #1-outlier removed")
plot(residuals(bm2u, type = "deviance") ~ fitted(bm2u),
     main = "Model #2-outlier removed")


### For manuscript: ggplot2 version of deviance residuals
outlierno <- data.frame(deviance_resid = residuals(bm1u, type = "deviance"),
                        pred = fitted(bm1u))
dev_outlierno <- ggplot(outlierno, aes(pred, deviance_resid)) + geom_point() +
  xlab("Model predicted values") + ylab("Deviance residuals") + theme_bw()
dev_outlierno

### Only in previous version of manuscript where I was wanting to compare deviance
#residuals for pre- and post-outlier removal models. Peter recommended against
#Combine with deviance ~ fitted values plot from section 5.2:
#deviance_plot <- ggpubr::ggarrange(dev_outlieryes, dev_outlierno)
#Export as high-res figure
#tiff("deviance_plot.tiff", width = 8, height = 5, pointsize = 12, units = 'in', res = 300)
#deviance_plot #graph that you want to export
#dev.off( )


#Pearson residuals
plot(residuals(bm1u, type = "pearson") ~ fitted(bm1u), main = "Model #1-outlier removed")
plot(residuals(bm2u, type = "pearson") ~ fitted(bm2u), main = "Model #2-outlier removed")
#In both the deviance and pearson residuals, it seems that there are some larger
#residuals at smaller fitted values (i.e., cone shape). However, there are so
#few of the high extreme values (>50 strays) that it is hard to say whether there
#is actually less spread at this level or if there just isn't enough data. The 
#best way to evaluate your model will be to cross validate it and evaluate pred-
#iction error


### Normality of the residuals
par(mfrow=c(1,2))
hist(residuals(bm1u, type = "deviance"), main = "Model #1-outlier removed")
hist(residuals(bm2u, type = "deviance"), main = "Model #2-outlier removed")

#Some slight right-skew in histograms
qqnorm(residuals(bm1u, type = "deviance"), main = "Model #1-outlier removed")
qqnorm(residuals(bm2u, type = "deviance"), main = "Model #2-outlier removed")
qqnorm(residuals(bm1u, type = "pearson"), main = "Model #1-outlier removed")
qqnorm(residuals(bm2u, type = "pearson"), main = "Model #2-outlier removed")


### Residuals ~ predictor variables
par(mfrow=c(2,2))
resid_cov <- function(mod, dat){
  for(i in c(9,11,13)){ #remove column 9 for bm2u
    plot(residuals(mod, type = "deviance") ~ dat[ , i])
    title(colnames(dat)[i])
  }
}
resid_cov(bm1u, fu_scaled) 
par(mfrow=c(1,2))
resid_cov(bm2u, fu_scaled)
#no obvious patterns


### Evaluate leverage
ggplot(data.frame(lev = hatvalues(bm1u),
                  deviance = residuals(bm1u, type = "deviance")),
       aes(x = lev, y = deviance)) + geom_point() + theme_bw()
ggplot(data.frame(lev = hatvalues(bm2u),
                  deviance = residuals(bm2u, type = "deviance")),
       aes(x = lev, y = deviance)) + geom_point() + theme_bw()
#The high leverage points do not have extreme residual values. Though note the 
#warning that outputs with the hatvalues function


### Random effect diagnostics 
#Random effect should be normally distributed
qqmath(lme4::ranef(bm1u)) 
qqmath(lme4::ranef(bm2u)) 
#All look approximately normally distributed

#Intraclass correlation coefficient
icc(bm1u)
icc(bm2u)
#none are = 0, indicating that random effects are necessary 




### Check for temporal autocorrelation ###
par(mfrow=c(1,1))
temp <- function(mod, dat){
  E <- residuals(mod, type = "deviance")
  I1 <- !is.na(dat$Avg_number_strays)
  Efull <- vector(length = length(dat$Avg_number_strays))
  Efull <- NA
  Efull[I1] <- E
  acf(Efull, na.action = na.pass)
}
temp(bm1u, fu_scaled)
temp(bm2u, fu_scaled) #no evidence of temporal autocorrelation




### Cross validate the model ###
cross_val <- function(dat, mod, mae_vec, mean_mae_vec){
  for(j in 1:10){
    for (i in 1:500) {
      train <- dat %>% 
        group_by(Year) %>% mutate(group = sample(n())/n() > 0.7) #0.7 for a 70-30
      #cross-validation
      
      splitdf <- split(train, train$group)
      training <- splitdf[["FALSE"]]
      test <- splitdf[["TRUE"]]
      
      predictions <- mod %>% predict(test) 
      exp.pred <- exp(predictions)
      mae <- MAE(exp.pred, test$Avg_number_strays, na.rm = TRUE)
      mae_vec[i] <- mae
    }
    mean_mae_vec[j] <- mean(mae_vec)
  }
  return(mean(mean_mae_vec))
}

#Note that the prediction accuracy varies greatly depending on how large the obs.
#value is (much greater error for large values because there are fewer of them).
#Observe this by excluding the largest observed values in the model fitting with
#the sub_fu_scaled dataset vs full fu_scaled dataset
sub_fu_scaled <- fu_scaled[fu_scaled$Avg_number_strays < 20,]

#Top model 
mean_mae_bm1u <- vector(length = 10)
mae_bm1u <- vector(length = 500)
cross_val(fu_scaled, bm1u, mae_bm1u, mean_mae_bm1u) #for fu_scaled (full dataset
#including large obs. values), the mean MAE is 6.27
cross_val(sub_fu_scaled, bm1u, mae_bm1u, mean_mae_bm1u) #for sub_fu_scaled, the
#mean MAE is 3.86 

#Second best model
mean_mae_bm2u <- vector(length = 10)
mae_bm2u <- vector(length = 500)
cross_val(fu_scaled, bm2u, mae_bm2u, mean_mae_bm2u) #for fu_scaled (full dataset
#including large obs. values), the mean MAE is 6.44
cross_val(sub_fu_scaled, bm2u, mae_bm2u, mean_mae_bm2u) #for sub_fu_scaled, the
#mean MAE is 3.90


#Null model 
null_mod <- glmer.nb(Avg_number_strays ~ (1|Year), data = fu_scaled,
                     control=glmerControl(optCtrl=list(maxfun=20000)))
mean_mae_null <- vector(length = 10)
mae_null <- vector(length = 500)
cross_val(fu_scaled, null_mod, mae_null, mean_mae_null) #for fu_scaled (full dataset
#including large obs. values), the mean MAE is 9.93
cross_val(sub_fu_scaled, null_mod, mae_null, mean_mae_null) #for sub_fu_scaled,
#the mean MAE is 5.65




### Examine predicted values 
#ONLY MAKE PREDICTIONS FOR TOP MODEL, do not model average (3/22/22 update as per
#co-author suggestion)
bm1u_pred <- as.data.frame(fitted(bm1u))
bm1u_pred <- cbind.data.frame(fu_scaled$Year, fu_scaled$StreamName, bm1u_pred,
                             fu_scaled$Avg_number_strays)
new_names <- c("Year", "StreamName", "Predicted", "Observed")
bm1u_pred <- bm1u_pred %>% rename_at(1:4, ~ new_names)
plot(Observed ~ Predicted, data = bm1u_pred, main = "Model #1-outlier removed")
plot(Observed ~ Predicted, data = bm1u_pred, xlim=range(0,50),
     main = "Model #1-outlier removed, lim x-range")
abline(0,1)
abline(lm(Observed ~ Predicted, data = bm1u_pred), col = "red")

#ggplot version for manuscript
lm_no <-lm(Observed ~ Predicted, data = bm1u_pred) #"no" for no outlier
summary(lm_no)
OP_outlierno <- ggplot(bm1u_pred, aes(Predicted, Observed)) + geom_point() +
  #co-author Megan says to not include regression and 1:1 lines on plot
  #geom_abline(slope = 0.751, intercept = 1.63) +
  #geom_abline(slope = 1, intercept = 1, linetype = "dashed") +
  theme_bw() +
  theme(text=element_text(family="Times New Roman", size=12)) +
  theme(axis.text=element_text(size=10))
OP_outlierno2 <- OP_outlierno + coord_cartesian(clip = "off")

### No longer am visually comparing obs ~ pred for pre- and post-outlier removal
#in my project:
#obs_pred_plot <- ggpubr::ggarrange(OP_outlieryes, OP_outlierno) #combine with 
#same figure which excludes the outlier (section 5.4 above)
#obs_pred_plot

#Export as high-res figure
tiff("obs_pred_plot.tiff", width = 6, height = 4, pointsize = 12, units = 'in', res = 300)
OP_outlierno #graph that you want to export
dev.off( ) 


#Averages by stream
mean_bm1u_pred <- bm1u_pred %>% group_by(StreamName) %>%
  summarise(Mean_pred_strays = mean(Predicted), Mean_obs_strays = mean(Observed))
mean_bm1u_pred
plot(Mean_obs_strays ~ Mean_pred_strays, data = mean_bm1u_pred,
     main = "Mean predictions by stream for Model #1-outlier removed")
plot(Mean_obs_strays ~ Mean_pred_strays, data = mean_bm1u_pred,
     main = "Mean predictions by stream for Model #1-outlier removed (lim x-range)",
     xlim=range(0,20))
abline(0,1)
abline(lm(Mean_obs_strays ~ Mean_pred_strays, data = mean_bm1u_pred), col = "red")



### Pseudo-R2 (coefficient of determination, or prop. variance explained) ###

#At the request of co-authors and an AFS meeting audience member, I'm calculating
#the pseudo-R^2 value for the model and trying to calculate partial R2 for the 
#covariates. Neither is well-defined for mixed effects models. See project log,
#Bayes_intro&Model_dev doc, and Nakagawa 2017 paper
MuMIn::r.squaredGLMM(bm1u, null = null_mod) #null_mod defined above in cross-val
#as glmer.nb(Avg_strays ~ (1|Year))
MuMIn::r.squaredGLMM(bm2u, null = null_mod)
?r.squaredGLMM #use trigamma R^2 estimates. R2m is the marginal R^2, which gives
#the variance explained by the fixed effects only. R2c is the conditional R^2,
#which gives the variance explained by the entire model (FE and RE together)

#Also helpful and important, the residual deviance vs null deviance:
summary(bm1u) #855.1
summary(null_mod) #1008.7


### In response to AFS talk judge who asked you to report the partial R2 for ind.
#covariates:

#Determine partial R^2s for your covariates. Not sure if this is best approach,
#though. Also report change in deviance and, by extension, AICc, with removal of 
#individual covariates model without Cons_Abundance
#Full model
summary(bm1u)$devcomp #deviance = 855.1
r.squaredGLMM(bm1u, null = null_mod) #marginal R2 = 0.40, conditional = 0.53
AICc(bm1u) #869.79


no_ConsA <- update(bm1u, .~. -Cons_Abundance)
summary(no_ConsA)$devcomp #deviance = 858.4
r.squaredGLMM(no_ConsA, null = null_mod) #marginal R2 = 0.38, conditional = 0.52
AICc(no_ConsA) #870.85

no_WMA <- update(bm1u, .~. -WMA_Releases_by_Yr)
summary(no_WMA)$devcomp #880.36
r.squaredGLMM(no_WMA, null = null_mod) #marginal R2 = 0.37, conditional = 0.52
AICc(no_WMA) #892.85

no_CVf <- glmer.nb(Avg_number_strays ~ (1 | Year) + Cons_Abundance +
                     WMA_Releases_by_Yr, data = fu_scaled)
summary(no_CVf)$devcomp #943.95
r.squaredGLMM(no_CVf, null = null_mod) #marginal R2 = 0.18, conditional = 0.23
AICc(no_CVf) #954.30



### Justify use of NB model over poisson ###
pois_mod <- glmer(Avg_number_strays ~ (1|Year) + Cons_Abundance +
                    WMA_Releases_by_Yr + CV_flow + I(CV_flow^2),
                  data = fu_scaled, family = "poisson")
AICc(bm1u, pois_mod)






save.image("Mod_fit3.RData")
load("Mod_fit3.RData")
