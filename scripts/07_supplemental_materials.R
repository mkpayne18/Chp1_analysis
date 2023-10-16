#-------------------------------------------------------------------------------
# Project title: Revealing stream attractiveness to stray hatchery-origin chum
# salmon in Southeast Alaska (MS thesis chapter 1)

# Molly K Payne

# Purpose of this script: Create supplemental tables and figs for thesis/manuscript

# Last updated: May 23, 2022
#-------------------------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(dplyr)



#### FIGURES ###################################################################
#1. Bias in response variable graph ============================================
prop_1 <- readRDS("output/prop_1.rds") #contains total dead counts and number of
#fish sampled for all streams where the ratio number sampled/dead count was < 1,
#which was the majority of streams. The ratio was only ever > 1 if live fish were
#sampled early in the season

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
# tiff("figs/supplemental/bias_plot.tiff", width = 8, height = 7, pointsize = 12,
#      units = 'in',
#      res = 300)
# bias_plot #graph that you want to export
# dev.off( )






#2. Observed vs model predicted indices (not averaged) =========================
bm1_pred <- readRDS("output/pred_and_obs_vals_unaveraged.rds")

obs_pred_plot <- ggplot(bm1_pred, aes(Predicted, Observed)) + geom_point() +
  geom_abline() +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        plot.margin = margin(10, 12, 10, 10))
obs_pred_plot2 <- obs_pred_plot + coord_cartesian(clip = "off")
obs_pred_plot2

#Export
# tiff("figs/supplemental/obs_pred_plot.tiff", width = 8, height = 7, pointsize = 12,
#      units = 'in',
#      res = 300)
# obs_pred_plot2
# dev.off( )






#3. Model residuals (deviance + pearson) plots =================================
bm1 <- readRDS("output/best_model.rds")

dev_resid <- data.frame(deviance_resid = residuals(bm1, type = "deviance"),
                        pred = fitted(bm1))
dev_resid_plot <- ggplot(dev_resid, aes(pred, deviance_resid)) + geom_point() +
  xlab("Model predicted values") + ylab("Deviance residuals") + theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.margin = margin(8, 8, 10, 8))
dev_resid_plot



pea_resid <- data.frame(pearson_resid = residuals(bm1, type = "pearson"),
                        pred = fitted(bm1))
pea_resid_plot <- ggplot(pea_resid, aes(pred, pearson_resid)) + geom_point() +
  xlab("Model predicted values") + ylab("Pearson residuals") + theme_bw() +
  theme(text = element_text(family= "Times New Roman", size= 14),
        plot.margin = margin(8, 8, 10, 8))
pea_resid_plot


resid_plots <- ggarrange(dev_resid_plot, pea_resid_plot, ncol = 2)
resid_plots

#Export as high-res figure
# tiff("figs/supplemental/resid_plots.tiff", width = 8, height = 4,
#      pointsize = 12, units = 'in',
#      res = 300)
# resid_plots 
# dev.off( ) 







#### TABLES ####################################################################
### The first supplemental table is a correlation matrix for all the covariates.
#I created this directly from the correlation matrix in section 2 of 02_EDA.R.
#It should already be in your figs/supplemental folder


#1. AICc and log(Li) for all candidate mods table ==============================
NBmod_dredge <- readRDS("output/NBmod_dredge.rds")
head(NBmod_dredge)
AICc_tab <- as.data.frame(head(NBmod_dredge, 5))
AICc_tab2 <- AICc_tab[,c(2:9,11,12)]
AICc_tab2 <- sapply(AICc_tab2, function(x) round(x, 2))

# write.csv(AICc_tab2, "figs/supplemental/AICc_logLi_table.csv")





#2. Mod pred and obs attractiveness indices complete table =====================
#You already created a version of this in 06_tables.R. Read in that table and 
#add stream locations

tab2 <- read.csv("figs/tab2_pred_obs.csv")
StreamPoints <- read.csv("data/StreamPoints.csv")
pred_obs_supp <- left_join(tab2, StreamPoints, by = "StreamName")
pred_obs_supp <- pred_obs_supp[,c(2,14,15,3:6,8)]

# write.csv(pred_obs_supp, "figs/supplemental/pred_obs_table.csv")






#3. Individual stream-year info ================================================
new_response_var <- readRDS("output/new_response_var.rds") #created in dead_
#counts_analysis.R, gives the total effective # of strays, the # of surveys,
#and the average # of strays calculated from both of those, which I used as the
#updated model response variable as of May 2022
survey_data8 <- readRDS("output/survey_data8.rds")  #also from dead_counts_
#analysis.R, contains the dead counts used to calculate new_response_var
deads_by_yr <- survey_data8 %>% group_by(StreamName, Year) %>%
  summarise(Total_dead = sum(DeadCount),
            Number_H_fish = sum(NumberStrays),
            Total_fish_sampled = sum(NumberofSpecimens))
tables5 <- left_join(new_response_var, deads_by_yr, by = c("StreamName", "Year"))

sapply(deads_by_yr, function(x) sum(is.na(x)))

tables5 <- tables5[,c(2,1,8,7,6,3:5)]
colnames(tables5)[2] <- "Stream name"      
colnames(tables5)[3] <- "Total fish sampled"
colnames(tables5)[4] <- "Number of hatchery fish"
colnames(tables5)[5] <- "Dead count"
colnames(tables5)[6] <- "Effective number of hatchery strays"
colnames(tables5)[7] <- "Number of surveys"
colnames(tables5)[8] <- "Attractiveness index (average effective number of strays)"

#Reduce the number of digits
tables5$`Effective number of hatchery strays` <-
  round(tables5$`Effective number of hatchery strays`, 1)
tables5$`Attractiveness index (average effective number of strays)` <-
  round(tables5$`Attractiveness index (average effective number of strays)`, 1)

#NOTE that the effective number of strays col does not always = number of
#hatchery fish / proportion sampled (where proportion sampled = total fish
#sampled / dead count). This is bc for some individual surveys, the proportion
#sampled was greater than 1, but was set equal to 1 in these specific instances. 
#Hence, effective number of strays in the model and here in this table will be
#slightly greater than would be expected. See docs/effective_number_strays_note.
#Rmd for more info

# write.csv(tables5, "figs/supplemental/stream_year_info.csv")







