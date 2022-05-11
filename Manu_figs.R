#Note that some figures and tables made more sense to create in Model_fitting3.R,
#so if you can't find the code for a figure you expected to find here then check
#that script
library(ggplot2)
library(ggmap)
library(ggspatial)
library(ggsn)
library(ggpubr)


setwd("~/Documents/CHUM_THESIS/Analysis")
Master_dataset <- read.csv("Master_dataset.csv")
#Year is a factor variable!
Master_dataset$Year <- factor(Master_dataset$Year)



#Figure 1. Observed number of strays + hatchery locations map ##################
################################################################################
load("Mod_fit3.RData") #load data that has mean attractiveness index by year for
#each stream
mean_bm1u_pred
#dataframe that has coordinates for each stream:
coords2 <- read_csv("Figs_Results/Suppl_Materials/Spatial_AutoC/Number_strays_by_Yr_w_coords.csv")

######
#from previously when testing for spatial autocorrelation; gives average of the 
#average number of strays for each streams across all years sampled:
Avg_strays_by_Yr_w_coords <-
  read_csv("~/Documents/CHUM THESIS/Manuscript/Spatial_AutoC/Avg_strays_by_Yr_w_coords.csv")
#remove Fish Creek-Portland Canal
Avg_strays_by_Yr_w_coords <-
  Avg_strays_by_Yr_w_coords[!Avg_strays_by_Yr_w_coords$StreamName ==
                              "Ketchikan Creek", ]
####

#include number of times a stream was surveyed on figure: 
surv <- Master_dataset %>% group_by(StreamName) %>%
  summarise(Number_of_Surveys = sum(Number.of.surveys))
Avg_strays_by_Yr_w_coords <- left_join(coords2, surv, by = "StreamName")
Avg_strays_by_Yr_w_coords <- left_join(Avg_strays_by_Yr_w_coords, mean_bm1u_pred,
                                       by = "StreamName")


#read in hatchery locations
Hatchery_Locations <- read_csv("~/Documents/CHUM_THESIS/Data Sources/Hatchery_Locations.csv")

#remove Wally Noerenberg and Nitinat River hatcherys
Hatchery_Locations <- Hatchery_Locations[c(1:12), ]


myMap <- get_stamenmap(location <- c(-137, 54.5, -130, 59.5), zoom = 6,
                       maptype = "terrain-background", crop = TRUE)
ggmap(myMap)
fig1 <- ggmap(myMap) + geom_point(aes(x = Longitude, y = Latitude,
                                      size = Mean_obs_strays,
                                      fill = Number_of_Surveys),
                                  colour = "black", pch = 21,
                                  data = Avg_strays_by_Yr_w_coords) +
  scale_size_continuous(range = c(2, 10)) +
  xlab("Longitude") + ylab("Latitude") + theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.key=element_blank()) + labs(size = "Mean Observed Index",
                                           fill = "Number of Surveys") +
  theme(text=element_text(family="Times New Roman", size=12)) +
  ggspatial::annotation_north_arrow( #direction arrow code chunk
    location = "bl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "Times New Roman")) +
  scalebar(x.min = -136.9, x.max = -134.9, y.min = 54.75,
           y.max = 54.95, dist = 50, dist_unit = "km",
           transform = T, height = 0.4, st.dist = 0.6,
           st.size = 4)
fig1

#to add hatchery locations to the map AND have them be included in the legend:
H_llocs <- rep("Hatchery Locations", 12)
Hatchery_Locations <- cbind.data.frame(Hatchery_Locations, H_llocs)

fig1a <- fig1 + geom_point(data = Hatchery_Locations, aes(x = Longitude,
                                                          y = Latitude, col=H_llocs),
                           shape = 24, size = 4, fill = "darkred") + labs(col = "") +
  scale_color_manual(values = "black") 
fig1a #note that if you don't want hatchery locations to be on the legend (just 
#mention it in figure caption instead), use the following code instead:
fig1 + geom_point(data = Hatchery_Locations, aes(x = Longitude, y = Latitude),
                  shape = 24, size= 4, fill = "darkred", colour = "black")

#create inset map:
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
world <- ne_countries(scale='medium',returnclass = 'sf')
usa_can <- subset(world, admin == "United States of America" | admin == "Canada")
alaska <- ggplot(data = usa_can) +
  geom_sf(fill = "grey") +
  coord_sf(crs = st_crs(3467), xlim = c(-1000000, 1800000), ylim = c(250000, 
                                                                     2500000),
           expand = FALSE, datum = NA) + geom_rect(aes(xmin = 900000,
                                                       xmax = 1550000,
                                                       ymin = 700000,
                                                       ymax = 1270000),
                                                   fill = "transparent",
                                                   color = "black", size = 1.5) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) #this last plot.margin part re-
#moves the white margin that shows up around the plot when you insert it as an 
#inset map
alaska #nice!

#add to original figure
fig1b <- fig1a + inset(grob = ggplotGrob(alaska), xmin = -133, xmax = -130,
                       ymin = 58.2, ymax = 59.55) 
fig1b



#IN GRAYSCALE:
fig1_bw <- ggmap(myMap) + geom_point(aes(x = Longitude, y = Latitude,
                                         size = Number_of_Surveys,
                                         fill = Average_Strays),
                                     colour = "black", pch = 21,
                                     data = Avg_strays_by_Yr_w_coords) +
  scale_size_continuous(range = c(2, 10)) +
  geom_point(data = Hatchery_Locations, aes(x = Longitude, y = Latitude),
             shape = 24, size = 4, fill = "white", colour = "black") +
  xlab("Longitude") + ylab("Latitude") + theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.key=element_blank()) + labs(size = "# of Surveys") +
  labs(fill = "Average # of Strays") +
  ggspatial::annotation_north_arrow( #direction arrow code chunk
    location = "bl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "Times New Roman")) +
  scalebar(x.min = -136.9, x.max = -134.9, y.min = 54.75,
           y.max = 54.95, dist = 50, dist_unit = "km",
           transform = T, height = 0.4, st.dist = 0.6,
           st.size = 4)
fig1_bw

fig1a_bw <-fig1_bw + scale_fill_gradientn(colours = c("grey72", "grey50", "grey38",
                                                      "grey15", "grey10"),
                                          values = c(0, 0.1, 0.2, 0.5, 1))
#breaks = c(10, 30, 50, 80))

#add hatchery locations 
H_llocs <- rep("Hatchery Locations", 12) #if you didn't already create above for
#the color version
Hatchery_Locations <- cbind.data.frame(Hatchery_Locations, H_llocs) #if you didn't
#already do above

fig1b_bw <- fig1a_bw + geom_point(data = Hatchery_Locations, aes(x = Longitude,
                                                                 y = Latitude, col=H_llocs),
                                  shape = 24, size = 4, fill = "white") +
  labs(col = "") + scale_color_manual(values = "black")

#add inset map
fig1c_bw <- fig1b_bw + inset(grob = ggplotGrob(alaska), xmin = -132.5,
                             xmax = -130, ymin = 58.2, ymax = 59.75) 



#Export as high-res figure
setwd("~/Documents/CHUM_THESIS/Analysis/Figs_Results")
tiff("fig1.tiff", width = 7, height = 6, pointsize = 12, units = 'in', res = 300)
fig1b
dev.off( ) #now the displayed graphs are saved to a file with the above file name






#Figure 2. Average # of strays ~ distance to nearest release site graph ########
################################################################################
View(Master_dataset)
plot(Avg_number_strays ~ Dist_nearest_R, data = Master_dataset)
Dist_nearest_Releas <-
  read_csv("~/Documents/CHUM_THESIS/Data Sources/Dist_Nearest_H&Release/Dist_nearest_Releas.csv")
R_type <- left_join(Master_dataset, Dist_nearest_Releas, by = "StreamName")
R_type <- as.data.frame(R_type)
sum(is.na(R_type$Release_site_type))

#find row with max avg number of strays for each stream; only for streams with 
#at least 2 observations (that way there is something to take the max of AND we
#have slightly more confidence in designating the stream as attractive or not)
R_type2 <- R_type %>% group_by(StreamName) %>% filter(Avg_number_strays == max(Avg_number_strays))
#Note that this^^ returns multiple rows if the max value is the same for any 
#streams (e.g., Little Goose Creek). Remove duplicate rows
R_type2 <- R_type2[!duplicated(R_type2$StreamName),]
###Since this might come up, NOTE that the higher avg_number_strays values are
#not influenced by higher number of surveys (sort R_type2$Avg_number_strays from
#largest to smallest and look at the number of surveys column) 


fig3 <- ggplot(data = R_type2, aes(x = Dist_nearest_R, y = Avg_number_strays,
                                   shape = Release_site_type)) + geom_point(size = 3.5) +
  theme_classic() + scale_shape_manual(values = c(16, 2)) +
  xlab("Distance to the Nearest Release Site (KM)") +
  ylab("Average Number of Hatchery Strays") + labs(shape = "Release Site Type") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman", size=12))
fig3

#briefly quantitatively compare the average number of hatchery strays within 40km
#of release for on-site vs remote release sites
R_type40km <- R_type2[R_type2$Dist_nearest_R <= 40, ]
onsite <- R_type40km$Avg_number_strays[R_type40km$Release_site_type == "On-site"]
Remote <- R_type40km$Avg_number_strays[R_type40km$Release_site_type == "Remote"]
t.test(onsite, Remote) #streams near hatchery on-site releases averaged 19.3 strays
#while remote site-proximate streams averaged 38.6 strays. p = 0.34 (not significant)

#Export as high-res figure
tiff("fig3.tiff", width = 7, height = 5, pointsize = 12, units = 'in', res = 300)
fig3 #graph that you want to export
dev.off( ) #now the displayed graphs are saved to a file with the above file name





#Figure 3. Covariate effects plots #############################################
# Cons_Abundance
library(effects)
effects_Cons <- effects::effect(term = "Cons_Abundance", mod = bm1u, xlevels = 10)
summary(effects_Cons)
x_Cons <- as.data.frame(effects_Cons)

#there is probably a better way to do this, but here is my method for now:
#un-scale the data
mean(f_update$Cons_Abundance, na.rm = T) #3034
sd(f_update$Cons_Abundance, na.rm = T) #4286
x_Cons$Cons_Abundance <- (x_Cons$Cons_Abundance * 4286) + 3034

#reduce range of observed data points
trunc_Avg_strays <- f_update[f_update$Avg_number_strays < 10,]
#create plot
Cons_plot <- ggplot() +
  geom_line(data = x_Cons, aes(x = Cons_Abundance, y=fit)) +
  geom_ribbon(data= x_Cons,
              aes(x = Cons_Abundance, ymin = lower, ymax = upper),
              alpha= 0.3, fill="grey70") +
  xlab("Chum Salmon Abundance") +
  ylab("Attractiveness Index") + theme_classic() +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) #+
#geom_point(data = trunc_Avg_strays, aes(x = Cons_Abundance,
#y = Avg_number_strays)) #+ ylim(0, 20)
Cons_plot 


# WMA_Releases_by_Yr
effects_WMA <- effects::effect(term = "WMA_Releases_by_Yr", mod = bm1u, xlevels = 10)
summary(effects_WMA)
x_WMA <- as.data.frame(effects_WMA)

#there is probably a better way to do this, but here is my method for now:
#un-scale the data
mean(f_update$WMA_Releases_by_Yr, na.rm = T) #16.3
sd(f_update$WMA_Releases_by_Yr, na.rm = T) #27.6
x_WMA$WMA_Releases_by_Yr <- (x_WMA$WMA_Releases_by_Yr * 27.6) + 16.3

WMA_plot <- ggplot() +
  geom_line(data = x_WMA, aes(x = WMA_Releases_by_Yr, y=fit)) +
  geom_ribbon(data= x_WMA,
              aes(x = WMA_Releases_by_Yr, ymin = lower, ymax = upper),
              alpha= 0.3, fill="grey70") +
  xlab("Number of Fish Released within 40KM") +
  ylab("Attractiveness Index") + theme_classic() +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) #+
#geom_point(data = trunc_Avg_strays, aes(x = WMA_Releases_by_Yr,
#y = Avg_number_strays))#+ ylim(0, 20)
WMA_plot



# CV_flow
effects_CV_flow <- effects::effect(term = "CV_flow", mod = bm1u, xlevels = 10)
summary(effects_CV_flow)
x_CV_flow <- as.data.frame(effects_CV_flow)

#there is probably a better way to do this, but here is my method for now:
#un-scale the data
mean(f_update$CV_flow, na.rm = T) #0.53
sd(f_update$CV_flow, na.rm = T) #0.057
x_CV_flow$CV_flow <- (x_CV_flow$CV_flow * 0.057) + 0.53

CVflow_plot <- ggplot() +
  geom_line(data = x_CV_flow, aes(x = CV_flow, y=fit)) +
  geom_ribbon(data= x_CV_flow,
              aes(x = CV_flow, ymin = lower, ymax = upper),
              alpha= 0.3, fill="grey70") +
  xlab("CV of Streamflow") +
  ylab("ln(Average Predicted # of Strays)") + theme_classic() +
  ylab("Attractiveness Index") + theme_classic() +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  xlim(0.42, 0.61) + ylim(0, 20)
CVflow_plot #gives you a warning about 2 removed rows; that is because of the 
#x-limit I set in the line above


all_effects_plot <- ggarrange(WMA_plot + rremove("ylab"),
                              Cons_plot + rremove("ylab"),
                              CVflow_plot + rremove("ylab"))
all_effects_plot2 <- annotate_figure(all_effects_plot,
                                     left = text_grob("Predicted Attractiveness Index",
                                                      size = 15,
                                                      family = "Times", rot = 90)) #if you are
#wondering, "rot = 90" rotates the y-axis label 90 degrees so that it is vertical
all_effects_plot2


#export high-res figure
getwd()
tiff('effects_plots.tiff', width = 8, height = 6.3, pointsize = 12,
     units = 'in', res = 300)
all_effects_plot2
dev.off()


strays_range <- fu_scaled %>% group_by(StreamName) %>%
  summarise_at(vars(Avg_number_strays), list(mean, min, max))
View(strays_range)






#Table 3: pred and observed values #############################################
tab3.1 <- fu_scaled %>% group_by(StreamName) %>%
  summarise(across(Avg_number_strays, c(min,max)), across(Number_surveys, sum))
#from Model_fitting3.R:
head(mean_bm1u_pred)
tab3 <- left_join(tab3.1, mean_bm1u_pred)
tab3 <- tab3[,c(1,5,6,2:4)]
name_update <- c("Mean predicted attractiveness index",
                 "Mean observed attractiveness index",
                 "Minimum observed number of strays", "Maximum observed number of strays",
                 "Total number of surveys")
tab3 <- tab3 %>% rename_at(2:6, ~name_update)
tab3 <- tab3[order(tab3$`Mean predicted attractiveness index`, decreasing = T),]
PAR <- 1:56
tab3 <- cbind.data.frame(tab3, PAR)
colnames(tab3)[7] <- "Predicted attractivenes rank"
#add location data in case you need it
head(Avg_strays_by_Yr_w_coords) #has location data by stream
tab3 <- left_join(tab3, Avg_strays_by_Yr_w_coords, by = "StreamName")
tab3 <- tab3[,c(1:7,10,11)]
getwd()
write.csv(tab3, "pred_obs_table.csv")





#Table S2: avg pred & obs values for all streams with lat/long #################
#Lat & long data:
StreamPoints <- read_csv("~/Documents/CHUM_THESIS/Data Sources/StreamPoints.csv")
#Avg pred & obs table (alternatively you could use mean_bm1u_pred if Model_fitting3.R
#objects are loaded)
pred_obs_table <- read_csv("Figs_Results/pred_obs_table.csv")
tabs2 <- left_join(StreamPoints, pred_obs_table, by = 'StreamName')
#check it:
X_tabs2 <- anti_join(StreamPoints, pred_obs_table, by = 'StreamName') #looks good,
#all of those creeks were excluded from the model 

tabs2 <- tabs2[complete.cases(tabs2),]
tabs2 <- tabs2[,-c(1:4,8:10)]
write.csv(tabs2, "Table_S2.csv")


#old Table 3 stuff #############################################################
u <- X2008_2019_HW_Data %>% group_by(`Hatchery of Origin`) %>%
  summarise(Sum = sum(From_H))
View(u)

v <- Releases_site_year %>% group_by(YearReleased) %>%
  summarise(Sum = sum(SUM_Releases_in_millions))
View(v)




#Table S3: Individual stream survey info #######################################
tabs3 <- fu_scaled[,c(2:7)]
head(tabs3)
write.csv(tabs3, "Table_S3.csv")



#Supplementary supplementary no zeroinfl plot ##################################
#This plot is going into my model log document (Bayes_intrdo&Model_dev.docx), not
#my manuscript (for now) in case someone wishes to see justification for why I 
#did not account for zero-inflation in my model and why it honestly isn't really
#necessary
zeros <- bm1u_pred %>% filter(Observed == 0) #all predictions for each stream
#year and their corresponding observed values. bm1u_pred object comes from
#Mod_fit3.Rdata file
plot(zeros$Observed ~ zeros$Predicted)
hist(zeros$Predicted, breaks = 20)





#Supplementary correlation matrix table (or is it a figure?) ###################
library(ggcorrplot)
COR #from Model_fitting3.R objects
ggcorrplot(COR$r, lab = T) +
  theme(text=element_text(family="Times New Roman", size=12)) #hmm, doesn't seem
#like you can change the font of the numbers within the corrplot, only the labels.
#I would leave it be for now and see if the grad school notices. You can always
#come back and make a correlation matrix using some other package

#Export as high-res figure
setwd("~/Documents/CHUM_THESIS/Analysis/Figs_Results")
tiff("corr_matrix.tiff", width = 7, height = 6, pointsize = 12, units = 'in', res = 300)
ggcorrplot(COR$r, lab = T) +
  theme(text=element_text(family="Times New Roman", size=12)) #graph that you want to export
dev.off( ) #now the displayed graphs are saved to a file with the above file name






#Supplementary model residuals plots ###########################################
dev_resid <- data.frame(deviance_resid = residuals(bm1u, type = "deviance"),
                        pred = fitted(bm1u))
dev_resid_plot <- ggplot(dev_resid, aes(pred, deviance_resid)) + geom_point() +
  xlab("Model predicted values") + ylab("Deviance residuals") + theme_bw() +
  theme(text=element_text(family="Times New Roman", size=14),
        plot.margin = margin(8, 8, 10, 8))
dev_resid_plot



pea_resid <- data.frame(pearson_resid = residuals(bm1u, type = "pearson"),
                        pred = fitted(bm1u))
pea_resid_plot <- ggplot(pea_resid, aes(pred, pearson_resid)) + geom_point() +
  xlab("Model predicted values") + ylab("Pearson residuals") + theme_bw() +
  theme(text=element_text(family="Times New Roman", size=14),
        plot.margin = margin(8, 8, 10, 8))
pea_resid_plot


resid_plots <- ggarrange(dev_resid_plot, pea_resid_plot, ncol = 2)
resid_plots

#Export as high-res figure
tiff("resid_plots.tiff", width = 8, height = 4, pointsize = 12, units = 'in',
     res = 300)
resid_plots #graph that you want to export
dev.off( ) #now the displayed graphs are saved to a file with the above file name





#Stream pHOS averages from Josephson et al. 2021 ###############################
#At the start of page 7 in the manuscript (methods section), I talk about the 
#pHOS of SEAK streams from the findings of Josephson et al. 2021. In the edits
#of chum_v5, Curry said that I should report the stream specific average pHOS
#range, instead of the range of all pHOS values. Here is where I calculate that:

#Using 2013-2015 vals from chapter 1 data (bc that is the data that Josephson et
#al. 2021 used)
X2013_2015 <- Master_dataset %>% filter(Year %in% c("2013", "2014", "2015"))
X2013_2015 <- X2013_2015[,c(1:5)]
X2013_2015$pHOS <- X2013_2015$Number_H_fish/X2013_2015$Total_fish_sampled
pHOS_mean_by_stream <- X2013_2015 %>% group_by(StreamName) %>%
  summarise(Mean_pHOS = mean(pHOS))
split2 <- split(pHOS_mean_by_stream, pHOS_mean_by_stream$Mean_pHOS < 0.05)
length(split2[[1]]$Mean_pHOS) #the pHOS streams that are > 0.05; 10 total
mean(split2[[1]]$Mean_pHOS)
range(split2[[1]]$Mean_pHOS) #report in paper, along with mean in line above





#Observed ~ predicted values plot ##############################################
#Can also be found near end of section 7.4 of Model_fitting3.R because I was using
#this plot during analysis to evaluate predictions
lm_no <-lm(Observed ~ Predicted, data = bm1u_pred) #"no" for no outlier, which
#doesn't matter anymore bc I'm not graphically comparing pre- and post-outlier
#models anymore. I left the lm name the same though so you could find the matching
#code in Model_fitting3.R script
summary(lm_no)
OP_outlierno <- ggplot(bm1u_pred, aes(Predicted, Observed)) + geom_point() +
  #co-author Megan says to not include regression and 1:1 lines on plot
  #geom_abline(slope = 0.751, intercept = 1.63) +
  #geom_abline(slope = 1, intercept = 1, linetype = "dashed") +
  theme_bw() +
  theme(text=element_text(family="Times New Roman", size=14),
        plot.margin = margin(10, 12, 10, 10))
OP_outlierno2 <- OP_outlierno + coord_cartesian(clip = "off")
OP_outlierno2


#Export as high-res figure
tiff("obs_pred_plot.tiff", width = 6, height = 4, pointsize = 12, units = 'in',
     res = 300)
OP_outlierno2 #graph that you want to export
dev.off( ) 






#CV_flow plot ##################################################################
load("CVflow_side.Rdata")
flow_plus$class_1 <- as.factor(flow_plus$class_1) #"class" needs to be a factor
#reorder it to show same watershed types next to each other on boxplot:
flow_plus$class_1 <- factor(flow_plus$class_1, levels = c("0", "3", "10",
                                                          "1", "2", "4", "5",
                                                          "8", "6"))

flow_plot <- ggplot(flow_plus) +
  geom_boxplot(aes(x = class_1, y = CV_flow)) +
  labs(x = "Watershed Type", y = "CV of Streamflow") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 16)) +
  #theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(labels = c("0" = "Rain-0", "1" = "Snow-1", "2" = "Snow-2",
                              "3" = "Rain-3", "4" = "Snow-4", "5" = "Snow-5",
                              "6" = "Glacier-6", "8" = "Rain-snow-8",
                              "10" = "Rain-10"))
flow_plot

#Export as high-res figure
tiff("CVflow_side_plot.tiff", width = 8.5, height = 4.2, pointsize = 12, units = 'in',
     res = 300)
flow_plot #graph that you want to export
dev.off( ) 





save.image("Manu_figs_objects.RData")
load("Manu_figs_objects.RData")
