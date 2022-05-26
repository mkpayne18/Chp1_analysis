#-------------------------------------------------------------------------------
# Project title: Revealing stream attractiveness to stray hatchery-origin chum
# salmon in Southeast Alaska (MS thesis chapter 1)

# Molly K Payne

# Purpose of this script: Fit generalized linear mixed effects model w/ negative
# binomial error distribution to predict the average (effective) number of hatch-
# ery strays in a stream and year

# Last updated: May 19, 2022
#-------------------------------------------------------------------------------
require(lme4)
require(MuMIn)
require(car)

source("scripts/02_EDA.R")
stray_dat #cleaned-up modeling dataset containing response variable and covariates



#1. Scale model covariates =====================================================
#Subtract the mean then divide by the standard deviation of each covariate data
#vector (can use scale.default() function in base R):
m <- apply(stray_dat[ , c(7:12)], 2, scale.default)
stray_dat_scaled <- cbind.data.frame(stray_dat[ , c(1:6)], m)
head(stray_dat_scaled) #Note that scaling is required. I tried fitting glmer.nb
#earlier on without scaling and the model would not converge and returned several
#warnings
rm(m)

#Save scaled df for future use (in making figs + tables)
saveRDS(stray_dat_scaled, "output/stray_dat_scaled.rds")





#2. Fit global model with negative binomial distribution =======================
#Curry says you should fit and compare models using REML = FALSE. Based on google
#search, I believe the default for glmer.nb does not use REML
#Global model:
NB_mod1 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                      Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                      mean_flow + CV_flow, data = stray_dat_scaled)
#if you need more iterations, include this after your data argument"
#control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) 
AICc(NB_mod1, REML = F) #1035.29





#3. Check for higher order terms ===============================================
#3a. Possible quadratic relationships ==========================================
mtrx <- as.data.frame(matrix(ncol = 2, nrow = 6))
names(mtrx) <- c("Covariate^2", "AICc")
for(i in c(7:12)){
  mod <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                    Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                    mean_flow + CV_flow + I(stray_dat_scaled[ , i]^2),
                  data = stray_dat_scaled)
  mtrx[i-6, 1] <- colnames(stray_dat_scaled)[i] #subtract 6 from your index i so
  #that you can start filling AICc values in the 1st row of 'mtrx' not the 7th
  mtrx[i-6, 2] <- AICc(mod) 
}

#Keep quadratic terms that reduce the AICc below that of the global model:
mtrx <- mtrx[!mtrx$AICc > AICc(NB_mod1),]
#Pink_Abundance, WMA_Releases_by_Yr, and CV_flow

### For attempting another day; fitting the models individually with the quadratic
#terms that reduce the AICc. The lapply function below works but it doesn't 
#correctly name the quadratic variable in a way that is helpful
#names.use <- stray_dat_scaled %>% select(any_of(mtrx$`Covariate^2`)) #this fxn
#identifies the cols in stray_dat_scaled which reduce the AICc when they are in-
#cluded as a quadratic term

#mod_list <- lapply(1:length(mtrx$AICc), function(x) 
#  glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest + Cons_Abundance +
#             Pink_Abundance + WMA_Releases_by_Yr + mean_flow + CV_flow +
#             I(stray_dat_subset[,x]^2), data = stray_dat_scaled))

### Fit and examine models which reduce the AICc by containing a quadratic term:
#Pink_Abundance
summary(glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                   Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                   mean_flow + CV_flow + I(Pink_Abundance^2),
                 data = stray_dat_scaled)) #Pink_Abundance collinear (rho = -0.8)
#with it's quadratic term, therefore not necessary to include quadratic term

summary(glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                   Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                   mean_flow + CV_flow + I(WMA_Releases_by_Yr^2),
                 data = stray_dat_scaled)) #WMA_Releases_by_Yr collinear (rho =
#-0.9) with quadratic term, therefore not necessary to include quadratic term

summary(glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                   Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                   mean_flow + CV_flow + I(CV_flow^2),
                 data = stray_dat_scaled)) #no collinearity issues, keep CV_flow
#quadratic term

NB_mod2 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                      Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                      mean_flow + CV_flow + I(CV_flow^2),
                    data = stray_dat_scaled)

#Remove unneeded items
rm(mod, mtrx, NB_mod1, i)





#3b. Possible (and biologically plausible) interactions ========================
XNB_mod2 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                      Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                      mean_flow + CV_flow + I(CV_flow^2) +
                      WMA_Releases_by_Yr:Cons_Abundance, data = stray_dat_scaled)
AICc(NB_mod2) #current best AICc = 955.77
AICc(XNB_mod2)
#Fishery_harvest:Cons_Abundance = 957.98
#Fishery_harvest:Pink_Abundance = 957.32
#Cons_Abundance:Pink_Abundance = 956.31
#Cons_Abundance:mean_flow = 957.07
#Cons_Abundance:CV_flow = 958.00
#Pink_Abundance:mean_flow = 956.09
#Pink_Abundance:CV_flow = 955.56 ####
#WMA_Releases_by_Yr:Cons_Abundance = 954.11 ####
#WMA_Releases_by_Yr:Pink_Abundance = 957.72
#WMA_Releases_by_Yr:mean_flow = 953.02 ####
#WMA_Releases_by_Yr:CV_flow = 957.59
lmtest::lrtest(NB_mod2, XNB_mod2) #the greatest reduction in AICc was with the 
#WMA_Releases_by_Yr:mean_flow interaction and passes the LRT. WMA_Releases_by_Yr:
#Cons_Abundance also passes the LRT
summary(XNB_mod2) #However, WMA_Releases_by_Yr:mean_flow has collinearity 
#issues. Do not include it in the model. But you should include Cons_Abundance:
#WMA_Releases_by_Yr interaction because it improves model fit and does not have
#collinearity issues


####

#### For the sake of getting your thesis done on time, ignore the Cons_Abundance:
#WMA_Releases_by_Yr interaction for now (until the manuscript stage). Come back
#and include it again later on 

#Global model
summary(NB_mod2)





#4. Preliminary diagnostics of global model ====================================
par(mfrow=c(1,1))
plot(NB_mod2) #model tends to overpredict slightly 
#Deviance residuals
plot(residuals(NB_mod2, type = "deviance") ~ fitted(NB_mod2))
hist(residuals(NB_mod2, type = "deviance"))
qqnorm(residuals(NB_mod2, type = "deviance"))

#Pearson residuals
hist(residuals(NB_mod2, type = "pearson"))
qqnorm(residuals(NB_mod2, type = "pearson"))

#Covariate residuals
par(mfrow=c(2,3))
resid_cov <- function(mod){
  for(i in c(7:12)){ 
    plot(residuals(mod, type = "deviance") ~ stray_dat_scaled[ , i]) #change
    #residual type back and forth between "deviance" and "pearson" depending on
    #what you want to see
    title(colnames(stray_dat_scaled)[i])
  }
}
resid_cov(NB_mod2)
#acceptable enough to proceed
par(mfrow=c(1,1))

#Check variance inflation factors
car::vif(NB_mod2) #all are < 1.6 (cause for concern only if > 2)





#5. Check for spurious relationships before final dredge =======================
#I.e., do any of the response~covariate relationships differ when fit individually
#compared to the full model fit? I'm checking for this because Pink_Abundance
#looks like it should have a negative relationship with the response variable
#based on EDA plotting, but summary(NB_mod2) shows a positive relationship

#View EDA relationships again:
par(mfrow=c(2,2))
for (i in c(7:12)) { 
  x <- stray_dat_scaled[,i]
  y <- log(stray_dat_scaled$Avg_number_strays+1)
  plot(x, y)
  title(colnames(stray_dat_scaled)[i])
}

harvest <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest,
                    data = stray_dat_scaled)
Cons_A <- glmer.nb(Avg_number_strays ~ (1|Year) + Cons_Abundance,
                   data = stray_dat_scaled)
Pink_A <- glmer.nb(Avg_number_strays ~ (1|Year) + Pink_Abundance,
                   data = stray_dat_scaled)
WMA <- glmer.nb(Avg_number_strays ~ (1|Year) + WMA_Releases_by_Yr,
                data = stray_dat_scaled)
mean_f <- glmer.nb(Avg_number_strays ~ (1|Year) + mean_flow,
                   data = stray_dat_scaled)
CV_f <- glmer.nb(Avg_number_strays ~ (1|Year) + CV_flow + I(CV_flow^2),
                 data = stray_dat_scaled)
#Convergence warnings were issued for several of these models, but the relationships
#within them seem to visually match their expectations based on EDA:


#Compare to overall model fit
summary(NB_mod2)
summary(harvest) #0.12 (p = 0.42) in NB_mod2, 0.33 (p = 0.06) in individual mod
summary(Cons_A) #-0.21 (p = 0.10) in NB_mod2, -0.69 (p = 0.72) in individual mod
summary(Pink_A) #0.16 (p = 0.21) in NB_mod2, -0.51 (p = 0.82) in individual mod
summary(WMA) #0.35 (p = 0) in NB_mod2, 1.17 (p = 0) in individual mod
summary(mean_f) #-0.05 (p = 0.52) in NB_mod2, -0.22 (p = 0.98) in individual mod
summary(CV_f) #0.44 + 0.81^2 (p = 0) in NB_mod2, 0.48 + 0.87^2 (p = 0) in ind. mod
#Pink_Abundance is totally spurious! Magnitude and sign change greatly between 
#models. Furthermore, NB_mod2 coef estimate for Pink_Abundance is positive when
#it appears that Pink_Abundance is negatively correlated with response in plot
#CONCLUSION: Remove Pink_Abundance from model. Other covariates OK

NB_mod3 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                      Cons_Abundance + WMA_Releases_by_Yr + #no more Pink_Abundance
                      mean_flow + CV_flow + I(CV_flow^2),
                    data = stray_dat_scaled)
AICc(NB_mod3) #955.18
car::vif(NB_mod3) #OK
summary(NB_mod3)






#6. Dredge model (MuMIn::dredge()) =============================================
options(na.action = "na.fail") #there shouldn't be any NAs, but you need to run 
#this line anyway to use MuMIn:dredge() or it will get angry
sapply(stray_dat_scaled, function(x) sum(is.na(x))) #indeed, no NAs
NBmod_dredge <- dredge(NB_mod3, rank = "AICc")
#I'm getting convergence issue warnings, even with the new optimizer and increase
#in max iterations in glmerControl above. But when I check the coefficient esti-
#mates in head(NBmod_dredge) below, they are the close to the same as in the
#summary(NB_mod3) output, which did not have convergence issues. So, I think it
#is fine to proceed(?)

head(NBmod_dredge)
saveRDS(NBmod_dredge, "output/NBmod_dredge.rds")


#The top two models account for 53.7% of the total model weight, so I will average
#the results from those two and disregard the remaining models
bm1 <- glmer.nb(Avg_number_strays ~ (1|Year) + Cons_Abundance + WMA_Releases_by_Yr
                + CV_flow + I(CV_flow^2), data = stray_dat_scaled) #'bm' for "best model"
bm2 <- glmer.nb(Avg_number_strays ~ (1|Year) +
                  WMA_Releases_by_Yr + CV_flow + I(CV_flow^2),
                data = stray_dat_scaled)
#Note that creating new model fits (bm1, bm2) is not exactly equivalent to extrac-
#ting the model coefficients from the MuMIn::dredge() object. I created new model
#objects so that I could make predictions from the model separately, and then 
#average the predictions while leaving the coefficients alone. When comparing 
#coefs in summary(bmX) to the equivalent model in straymod_dredge, I find that 
#the coefficient estimates only differ at the 4th decimal place (if they even do
#differ), so I think my approach is acceptable


#Save final model object as .rds for future use if necessary:
saveRDS(bm1, file = "output/best_model.rds")


#Remove unneeded objects from this script
rm(Cons_A, CV_f, harvest, mean_f, NB_mod2, Pink_A, WMA, XNB_mod2, i, x, y)



