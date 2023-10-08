#-------------------------------------------------------------------------------
# Project title: Revealing stream attractiveness to stray hatchery-origin chum
# salmon in Southeast Alaska (MS thesis chapter 1)

# Molly K Payne

# Purpose of this script: Exploratory data analysis of response variable and 
# covariates prior to modeling

# Last updated: May 18, 2022 #test commit with new PAT created on ORNRC computer
#-------------------------------------------------------------------------------
require(corrgram)
require(Hmisc)
require(geosphere)
require(ape)

source("scripts/01_stray_data_load_and_clean.R") #Note that this will load pack-
#ages associated with this script (in this case tidyverse and dplyr) in addition
#to objects created within

#Model data (response + covariates) from stray_data_load_and_clean.R:
stray_dat




#1. Examine distribution of response variable ==================================
stray_dat$Avg_number_strays <- round(stray_dat$Avg_number_strays)
hist(stray_dat$Avg_number_strays, breaks = 50, main =
       "Histogram of average # of strays (model response)",
     xlab = "Average # of hatchery-origin strays") #very right-skewed
boxplot(stray_dat$Avg_number_strays) #several high outliers

#Are there a lot of zeros?
response_table <- as.data.frame(table(stray_dat$Avg_number_strays))
head(response_table) #43 zeroes
43/length(stray_dat$Year) #24% of the data, maybe zero-inflation?? Check
#to see if model under-predicts the number of 0s later on

#Conclusion: Response data is going to be modeled with Poisson or Negative Bin-
#omial distribution with log-link due to heavy right-skew and because it is count
#data. Plot the LOG of the response against explanatory variables to evaluate 
#potential relationships. 





#2. Check for correlations between explanatory variables =======================
#Visually assess first, using corrgram::corrgram()
corrgram(stray_dat[ , c(7:16)]) #evidence of several strong correlations

#Quantitatively check for highly correlated (spearman's rho > 0.5) variables:
stray_vars <- as.matrix(stray_dat[ , c(7:16)])  # Save variables as a matrix 
(COR <- Hmisc::rcorr(stray_vars, type = "spearman")) #you should use spearman,
#not pearson correlation coefs bc not all covariates are normally distributed

#Alternative method using base R:
cor(stray_dat[ , c(7:16)], method = "spearman") 

#Examine matrix of spearman's correlation coefficients
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
stray_dat <- stray_dat[ , -c(9,11,13,14)]

#Send correlation matrix to output folder as a table so you can include in supp-
#lementary material of paper
corr_matrix <- round(COR$r, 3)
write.csv(corr_matrix, "figs/supplemental/corr_matrix.csv")


#Remove unneeded objects
rm(response_table)
rm(stray_vars)




#3. Response~explanatory relationships (ln(response) ~ predictor)) =============
#Response should be log transformed for these plots because you are going to be
#using a negative binomial error distribution with a log link
par(mfrow=c(2,3))
for(i in names(stray_dat)[7:12]) {
  x <- stray_dat[,i]
  y <- log(stray_dat$Avg_number_strays + 1)	#add 1 because you have zeroes
  plot(x, y, xlab=i, ylab="ln(Avg (obs) # of strays)")
  title(i)
}
par(mfrow=c(1,1))
#Fishery_harvest direct increase, Cons_Abundance & Pink_Abundance inverse rel-
#ationship with response, WMA_Releases_by_Yr increasing, possibly quadratic
#Hard to tell for mean_flow, CV_flow increases with response. Possibility of out-
#liers for Pink_Abundance and mean_flow so check for them


### Check for outliers ###
#Pink_Abundance
outl_P <- influence.measures(lm(log(stray_dat$Avg_number_strays + 1) ~
                                  stray_dat$Pink_Abundance))
4/length(stray_dat$Avg_number_strays) #extreme leverage values will be greater
#than 0.0226
p1 <- as.data.frame(outl_P[[1]])
length(which(p1$cook.d > 0.0226)) #7 total Pink_Abundance outliers

#Does a log transformation help?
outl_P2 <- influence.measures(lm(log(stray_dat$Avg_number_strays + 1) ~
                                   log(stray_dat$Pink_Abundance + 1)))
p2 <- as.data.frame(outl_P2[[1]])
length(which(p2$cook.d > 0.0226)) #11 outliers, log transformation does not im-
#prove outlier situation

#Does a square root transformation help?
outl_P3 <- influence.measures(lm(log(stray_dat$Avg_number_strays + 1) ~
                          sqrt(stray_dat$Pink_Abundance)))
p3 <- as.data.frame(outl_P3[[1]])
length(which(p3$cook.d > 0.0226)) #8 outliers, does not help


#mean_flow
outl_M <- influence.measures(lm(log(stray_dat$Avg_number_strays + 1) ~
                                  stray_dat$mean_flow))
m1 <- as.data.frame(outl_M[[1]])
length(which(m1$cook.d > 0.0226)) #4 outliers

#Does a log transformation help?
outl_M2 <- influence.measures(lm(log(stray_dat$Avg_number_strays + 1) ~
                                   log(stray_dat$mean_flow)))
m2 <- as.data.frame(outl_M2[[1]])
length(which(m2$cook.d > 0.0226)) #12 outliers after transformation

#Does a square root transformation help?
outl_M3 <- influence.measures(lm(log(stray_dat$Avg_number_strays + 1) ~
                                   sqrt(stray_dat$mean_flow)))
m3 <- as.data.frame(outl_M3[[1]])
length(which(m3$cook.d > 0.0226)) #8 outliers, transformation does not help


#Pink_Abundance and mean_flow have several outliers. Log and square root trans-
#formations do not help. These points are part of large river systems as well as
#a few streams that did not have any pink salmon in certain years, so they are 
#not errors. I will leave these points in the dataset, leave Pink_Abundance and
#mean_flow untransformed, and revisit the model with these points removed later 
#on if Pink_abundance and mean_flow do end up staying in the model


#Remove unneeded items
rm(outl_P, outl_P2, outl_P3, outl_M, outl_M2, outl_M3, p1, p2, p3, m1,
            m2, m3)





#4. Explore distributions of explanatory variables =============================
par(mfrow=c(2,3))
for (i in c(7:12)) {
  x <- stray_dat[,i]
  hist(x, main = colnames(stray_dat)[i])
  print(shapiro.test(x))
} 
par(mfrow=c(1,1))
#Cons_Abundance, Pink_Abundance, WMA_Releases_by_Yr, and mean_flow are heavily
#right-skewed. Fishery_harvest and CV_flow look closer to a normal distribution
#but are not officially. Leave predictors untransformed for now as it is not 
#strictly necessary and will facilitate easier model interpretation





#5. Are there differences between years? =======================================
plot(stray_dat$Avg_number_strays ~ stray_dat$Year) #some differences between
#years for raw response
plot(log(stray_dat$Avg_number_strays + 1) ~ stray_dat$Year) #even more so for
#the log response
summary(aov(log(stray_dat$Avg_number_strays + 1) ~ stray_dat$Year)) #differences
#significant between years

### Variation in years for explanatory variables 
par(mfrow=c(2,2))
for(i in names(stray_dat)[7:12]) {
  x <- stray_dat$Year
  y <- stray_dat[,i]
  plot(x, y, xlab="Year")
  title(i)
} #clear differences between years for all variables except maybe Cons_Abundance,
#Pink_Abundance, and mean_flow. Year should be included as a random effect as
#expected. Ignore CV_flow and mean_flow plots since their data does not vary
#over time
par(mfrow=c(1,1))




### Variation in response ~ covariate relationships by year 
#Consider whether you might need random slopes
#xyplot is from library(lattice)
for (i in names(stray_dat)[7:12]) {
  print(xyplot(log(Avg_number_strays + 1) ~ stray_dat[,i]|Year,
               data = stray_dat,
               type = c("p","r"),
               xlab = i))
}
rm(i, x, y)
#Some relationships vary greatly across years, but I think this has to do with
#variation in which sites were included and not inherent difference in the slope
#of the relationship(?). However, given that the response does differ signif-
#icantly by year, I should include year as a random intercept. Test for better
#model fit with and w/o year random intercept in your model development script






#6. Check for overdispersion in the response (likely) ========================
### Determine mean and variance of evenly split chunks of data ###
chunk2 <- function(x,n) split(x, cut(seq_along(log(stray_dat$Avg_number_strays+1)),
                                     n, labels = FALSE))
f_split <- chunk2(sort(log(stray_dat$Avg_number_strays + 1)), 17) #split into 17
#groups so that you can have approximately 10 observations within each
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
plot(f_means, f_variances, main = "Response variable variance vs mean")
fit <- lm(f_variances ~ f_means + I(f_means^2))
curve(predict(fit, newdata = data.frame(f_means=x)), add=T)
#Variance substantially greater than the mean at high values. In fact, the quad-
#ratic relationship of the lm fit above (variance ~ mean^2) seems to approximate
#the relationship quite well
#CONCLUSION: Use negative binomial probability distribution to fit model due to 
#apparent overdispersion

#Remove unneeded items
rm(f_split, fit, f_means, f_variances, chunk2)






#7. Check for spatial autocorrelation in the response variable =================
#I have multiple years of data with considerable variation between years, so I
#will average the # of strays across years to check for spatial autocorrelation

#Create table of average of Avg_number_strays and link coordinates to it
StreamPoints <- read.csv("data/StreamPoints.csv")
spat <- inner_join(stray_dat, StreamPoints, by = "StreamName")
spatio_sum <- spat %>% group_by(StreamName) %>% summarise(Number_Strays =
                                                            mean(Avg_number_strays))
spatio_sum2 <- merge(spatio_sum, spat[ , c("StreamName", "Latitude", "Longitude")],
                     by = "StreamName", all.x = T) #attach coordinates
spatio_sum3 <- unique(spatio_sum2) #the above created a row for each year for 
#each stream, so remove the duplicate rows

#Check for spatial autocorrelation using geodesic distances following the example
#of https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/ 
#Using geosphere::distm()
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


ape::Moran.I(spatio_sum3$Number_Strays, dist_mat_inv) #p = 0.34, fail to reject 
#null = no spatial autocorrelation occurring 


#Remove uneeded objects
rm(spat, spatio_sum, spatio_sum2, spatio_sum3, dist_mat, dist_mat_inv)



### EDA conclusions and actions ###
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






