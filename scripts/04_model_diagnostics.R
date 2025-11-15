#-------------------------------------------------------------------------------
# Project title: Revealing stream attractiveness to stray hatchery-origin chum
# salmon in Southeast Alaska (MS thesis chapter 1)

# Molly K Payne

# Purpose of this script: Assess diagnostics for best model fit 

# Last updated: November 13, 2025
#-------------------------------------------------------------------------------
require(lmtest)
require(lattice)
require(performance)
require(Metrics)

source("scripts/03_fit_NB_model.R")

bm1 #final model to predict stream attractiveness to hatchery strays



#1. Compare to null model ======================================================
null_model <- glmer.nb(Avg_number_strays ~ (1|Year), data = stray_dat_scaled,
                       control = glmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun = 1e8))) #null
#model is failing to converge still, not sure what to do about that
AICc(bm1, null_model) #null model is worse
lrtest(bm1, null_model) #complex model is better





#2. Residual diagnostics =======================================================
### (Deviance) residuals ~ fitted values plot 
par(mfrow=c(1,1))
plot(residuals(bm1, type = "deviance") ~ fitted(bm1),
     main = "Deviance residuals ~ fitted values")

#Pearson residuals ~ fitted values plot
plot(residuals(bm1, type = "pearson") ~ fitted(bm1),
     main = "Pearson residuals ~ fitted values")



### Normality of the residuals
hist(residuals(bm1, type = "deviance"), main = "Hist. of deviance residuals")

#Quantile-quantile plots
qqnorm(residuals(bm1, type = "deviance"),
       main = "qqnorm plot for deviance residuals")
qqnorm(residuals(bm1, type = "pearson"),
       main = "qqnorm plot for pearson residuals")



### Residuals ~ predictor variables
par(mfrow=c(2,2))
resid_cov #function from previous script that plots deviance residuals against
#predictors
resid_cov(bm1)



### Evaluate leverage
ggplot(data.frame(lev = hatvalues(bm1),
                  deviance = residuals(bm1, type = "deviance")),
       aes(x = lev, y = deviance)) + geom_point() +
  ggtitle("Deviance residuals vs leverage") + theme_bw()
#The high leverage points do not have extreme residual values. Though note the 
#warning that outputs with the hatvalues function that the hat matrix may not
#make sense for GLMMs






#3. Random effect diagnostics ==================================================
#Random effect should be normally distributed
qqmath(lme4::ranef(bm1)) #qqmath from library(lattice)

#Intraclass correlation coefficient
icc(bm1) #icc from library(performance)
#none are = 0, indicating that random effects are necessary 






#4. Check for temporal autocorrelation =========================================
par(mfrow=c(1,1))
temp <- function(mod){
  E <- residuals(mod, type = "deviance")
  I1 <- !is.na(stray_dat_scaled$Avg_number_strays)
  Efull <- vector(length = length(stray_dat_scaled$Avg_number_strays))
  Efull <- NA
  Efull[I1] <- E
  acf(Efull, na.action = na.pass)
}
temp(bm1) #no evidence of temporal autocorrelation






#5. Cross-validate each model ==================================================
### Calculate the mean absolute error for each model
cross_val <- function(dat, mod, mae_vec, mean_mae_vec){
  for(j in 1:10){
    for (i in 1:500) {
      train <- dat %>% 
        group_by(Year) %>% mutate(group = sample(n())/n() > 0.8) #0.8 for an 80-20
      #cross-validation
      
      #split data into training (80%) and test (20%) datasets
      splitdf <- split(train, train$group)
      training <- splitdf[["FALSE"]]
      test <- splitdf[["TRUE"]]
      
      #train (fit) the model on the training set
      mod2 <- glmer.nb(formula(mod), data = training)
      
      #validate on the test set, i.e., see how well mod predicts 'new' (test) data
      predictions <- mod2 %>% predict(test) 
      exp.pred <- exp(predictions)
      mae <- mae(exp.pred, test$Avg_number_strays)#, na.rm = TRUE)
      mae_vec[i] <- mae
    }
    mean_mae_vec[j] <- mean(mae_vec)
  }
  return(mean(mean_mae_vec))
}

## Model predicts small vals (avg strays < 40) more accurately:
#included previously to show that model predicts smaller vals more accurately, but
#after correcting cross validation approach to fit with training set and validate
#with test set (previously I had incorrectly trained the model with the full OG
#dataset and didn't notice my mistake), the model training can no longer converge
# stray_dat_sub <- stray_dat_scaled[stray_dat_scaled$Avg_number_strays < 40,]

#Top model 
mean_mae_bm1 <- vector(length = 10)
mae_bm1 <- vector(length = 500)
cross_val(stray_dat_scaled, bm1, mae_bm1, mean_mae_bm1) #mean MAE = 12.51
#30 ish warnings about convergence issues each time function is run. 30 out of
#500 iterations is OK

#Second best model, only for reporting purposes:
mean_mae_bm2 <- vector(length = 10)
mae_bm2 <- vector(length = 500)
cross_val(stray_dat_scaled, bm2, mae_bm2, mean_mae_bm2) #mean MAE = 11.99
#also 30 ish warnings about convergence

#Null model - this no longer converges when you remove 20% of the data, after
#correcting cross validation function
# mean_mae_null <- vector(length = 10)
# mae_null <- vector(length = 500)
# cross_val(stray_dat_scaled, null_model, mae_null, mean_mae_null)

#calculate mae for full dataset instead:
mae(fitted(null_model), stray_dat_scaled$Avg_number_strays) #14.603

#model with CV flow removed
#refit model with optimizer for convergence:
# no_CVflow <- glmer.nb(Avg_number_strays ~ (1|Year) + WMA_Releases_by_Yr,
#                       data = stray_dat_scaled,
#                       control = glmerControl(optimizer="bobyqa",
#                                             optCtrl = list(maxfun = 1e8)))
# mean_mae_noCV <- vector(length = 10)
# mae_noCV <- vector(length = 500)
# cross_val(stray_dat_scaled, no_CVflow, mae_noCV, mean_mae_noCV) #dang, also
#doesn't converge

#calculate for full dataset instead then, similar to null model
mae(fitted(no_CVflow), stray_dat_scaled$Avg_number_strays) #22.4
summary(no_CVflow) #2 fixed effects (WMA_releases + intercept) + theta (dispersion
#parm) + random effect variance = 4 df for this model


#Remove unneeded objects
rm(mae_bm1, mae_null, mean_mae_bm1, mean_mae_null)





#6. Examine predicted values ===================================================
bm1_pred <- as.data.frame(fitted(bm1))
bm1_pred <- cbind.data.frame(stray_dat_scaled$Year, stray_dat_scaled$StreamName,
                             bm1_pred, stray_dat_scaled$Avg_number_strays)
new_names <- c("Year", "StreamName", "Predicted", "Observed")
bm1_pred <- bm1_pred %>% rename_at(1:4, ~ new_names)
plot(Observed ~ Predicted, data = bm1_pred)
abline(0,1)
abline(lm(Observed ~ Predicted, data = bm1_pred), lty = 2) 

#Reduced x-range (to show smaller predicted values)
plot(Observed ~ Predicted, data = bm1_pred, xlim=range(0,50),
     main = "Model #1, lim x-range")
abline(0,1)
abline(lm(Observed ~ Predicted, data = bm1_pred), lty = 2)

rm(new_names)



### Averages by stream
mean_bm1_pred <- bm1_pred %>% group_by(StreamName) %>%
  summarise(Mean_pred_strays = mean(Predicted), Mean_obs_strays = mean(Observed))
mean_bm1_pred
plot(Mean_obs_strays ~ Mean_pred_strays, data = mean_bm1_pred,
     main = "Mean predictions by stream")
abline(0,1)
abline(lm(Mean_obs_strays ~ Mean_pred_strays, data = mean_bm1_pred), col = "red")

#Reduced x-range (to show smaller predicted values)
plot(Mean_obs_strays ~ Mean_pred_strays, data = mean_bm1_pred,
     main = "Mean predictions by stream (lim x-range)",
     xlim=range(0,40))
abline(0,1)
abline(lm(Mean_obs_strays ~ Mean_pred_strays, data = mean_bm1_pred), col = "red")


#Save predicted and observed vals for future use
saveRDS(bm1_pred, file = "output/pred_and_obs_vals_unaveraged.rds")
saveRDS(mean_bm1_pred, file = "output/mean_pred_and_obs_vals.rds")
#Also send to chapter 2 output
# saveRDS(mean_bm1_pred,
#         file = "~/Documents/CHUM_THESIS/Chp2_analysis/output/mean_bm1_pred.rds")






#7. Check for outliers =========================================================
#In the EDA script, outliers were identified for Pink_Abundance and mean_flow
#covariates, neither of which remain in the final model. However, I will still 
#check for other outliers
out <- cooks.distance(influence(bm1))
class(out)
out <- as.data.frame(out)
out$out <- round(out$out, 2)
which(out$out > 0.2) #outliers are records 42 and 162

#Refit model w/o outliers and compare
no_out_dat <- stray_dat_scaled[-c(42, 162),]
no_out <- glmer.nb(Avg_number_strays ~ (1|Year) + WMA_Releases_by_Yr +
                     CV_flow + I(CV_flow^2), data = no_out_dat)
AICc(bm1)
AICc(no_out) #much lower AICc

#cross validate:
mean_mae_out <- vector(length = 10)
mae_out <- vector(length = 500)
cross_val(no_out_dat, no_out, mae_out, mean_mae_out) #the mod with outliers re-
#moved predicts better

summary(bm1)
summary(no_out) #conclusions do not change

#Compare predictions of attractiveness:
no_out_pred <- as.data.frame(fitted(no_out))
no_out_pred <- cbind.data.frame(no_out_dat$Year, no_out_dat$StreamName,
                             no_out_pred, no_out_dat$Avg_number_strays)
new_names <- c("Year", "StreamName", "Predicted", "Observed")
no_out_pred <- no_out_pred %>% rename_at(1:4, ~ new_names)

mean_out_pred <- no_out_pred %>% group_by(StreamName) %>%
  summarise(Mean_pred_strays = mean(Predicted),
            Mean_obs_strays = mean(Observed)) %>% arrange(desc(Mean_pred_strays))
mean_bm1_pred <- mean_bm1_pred %>% arrange(desc(Mean_pred_strays))

head(mean_out_pred, n = 10)
head(mean_bm1_pred, n = 10) #not much difference
tail(mean_out_pred, n = 10)
tail(mean_bm1_pred, n = 10) #no difference

## This seems like a grey area to me. The mod w/o outliers clearly makes more
#accurate predictions, but the conclusions around the covariates do not change
#and the relative attractiveness of streams doesn't appreciably deviate either.
#I do not believe the outlier observed values of attractiveness to be errors, so
#I will leave them in place.


rm(out, no_out_dat, no_out_pred, no_out, mean_out_pred)






#8. Calculate pseudo-R^2 for final model =======================================
MuMIn::r.squaredGLMM(bm1, null = null_model)
MuMIn::r.squaredGLMM(bm2, null = null_model) #also for mod2
MuMIn::r.squaredGLMM(no_CVflow, null = null_model)
?r.squaredGLMM #use trigamma R^2 estimates. R2m is the marginal R^2, which gives
#the variance explained by the fixed effects only. R2c is the conditional R^2,
#which gives the variance explained by the entire model (FE and RE together)






#9. Evaluate possible Poisson model ============================================
### Justify use of NB model over poisson ###
pois_mod <- glmer(Avg_number_strays ~ (1|Year) +
                    WMA_Releases_by_Yr + CV_flow + I(CV_flow^2),
                  data = stray_dat_scaled, family = "poisson")
AICc(bm1, pois_mod)



