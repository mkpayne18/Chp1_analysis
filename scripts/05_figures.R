#-------------------------------------------------------------------------------
# Project title: Revealing stream attractiveness to stray hatchery-origin chum
# salmon in Southeast Alaska (MS thesis chapter 1)

# Molly K Payne

# Purpose of this script: Create figures for thesis/manuscript

# Last updated: May 23, 2022
#-------------------------------------------------------------------------------
require(tidyverse)
require(ggplot2)
require(ggmap)
require(ggspatial)
require(ggsn)
require(ggpubr)
require(effects)
require(sf)
require(rnaturalearth)
require(rnaturalearthdata)
#require(rgeos)##

# source("scripts/04_model_diagnostics.R") #NOTE that this will take several min-
#utes to run. If you don't have it loaded already, you can instead just load the
#needed objects from previous scripts:
bm1 <- readRDS("output/best_model.rds")
mean_bm1_pred <- readRDS("output/mean_pred_and_obs_vals.rds")
StreamPoints <- read.csv("data/StreamPoints.csv")
stray_dat <- readRDS("output/stray_dat.rds")
stray_dat_scaled <- readRDS("output/stray_dat_scaled.rds")


#1. Observed # of strays + hatchery locations map ==============================
### This is currently figure 1 of the manuscript

#1.1. Data tailoring for map ===================================================
bm1 #top model developed and selected from scripts 01-04. This was the most
#parsimonious model with 2 AICc of the lowest AICc.
mean_bm1_pred #contains mean predicted and observed values for each stream
StreamPoints #contains lat and long for each stream
stray_dat #un-scaled (but clean) covariate data which includes the updated response
#var, average effective # of strays
stray_dat_scaled #scaled (z-score standardized vals for data used to fit model)


### Create dataframe for mapping:
data_for_map <- left_join(mean_bm1_pred, StreamPoints, by = "StreamName")
data_for_map <- data_for_map[,c(1,8,9,2,3)]

#Include number of times a stream was surveyed on figure: 
surv <- stray_dat_scaled %>% group_by(StreamName) %>%
  summarise(Number_of_Surveys = sum(Number_surveys))
data_for_map <- left_join(data_for_map, surv, by = "StreamName")

#Include hatchery locations (on map, not in dataframe)
Hatchery_Locations <- read.csv("data/Hatchery_Locations.csv")

#remove Wally Noerenberg and Nitinat River hatcheries
Hatchery_Locations <- Hatchery_Locations %>% slice(1:12) %>%
  mutate(H_llocs = "Hatchery locations")



#1.2. Create the map ===========================================================
### SEAK area basemap:
myMap <- get_stadiamap(location <- c(-137, 54.5, -130, 59.5), zoom = 9,
                       maptype = "stamen_terrain", crop = TRUE)
ggmap(myMap)


### Map with data:
fig1 <- ggmap(myMap) + geom_point(aes(x = Longitude, y = Latitude,
                                      size = Mean_obs_strays,
                                      fill = Number_of_Surveys),
                                  colour = "black", pch = 21,
                                  data = data_for_map) +
  scale_size_continuous(range = c(2, 10)) +
  xlab("Longitude") + ylab("Latitude") + theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13),
        legend.text = element_text(size = 11.5),
        legend.title = element_text(size = 14),
        legend.key=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) +
  labs(size = "Mean observed index", fill = "Number of surveys") +
  theme(text = element_text(family = "Times New Roman", size = 12)) +
  ggspatial::annotation_north_arrow( #direction arrow code chunk
    location = "bl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "Times New Roman")) +
  scalebar(x.min = -137.15, x.max = -135.15, y.min = 54.75,
           y.max = 54.95, dist = 50, dist_unit = "km",
           transform = T, height = 0.4, st.dist = 0.6,
           st.size = 4)

fig1


#Add in the hatcheries:
fig1a <- fig1 + geom_point(data = Hatchery_Locations, aes(x = Longitude,
                                                          y = Latitude, col=H_llocs),
                           shape = 24, size = 4, fill = "darkred") + labs(col = "") +
  scale_color_manual(values = "black") 
fig1a


### Create inset map of Alaska, then add to figure
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
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        panel.border = element_rect(colour = "black", fill=NA)) #this last
#plot.margin part removes the white margin that shows up around the plot when you 
#insert it as an inset map
alaska #nice!


#Final map (include inset map):
fig1b <- fig1a + inset(grob = ggplotGrob(alaska), xmin = -133, xmax = -130,
                       ymin = 58.2, ymax = 59.55) 
fig1b

#Export as high-res figure
# tiff("figs/fig1.tiff", width = 18, height = 18, pointsize = 12, units = 'cm',
#      res = 600)
# fig1b
# dev.off()

#Remove unneeded objects
rm(surv, usa_can, world)






#2. Number surveys by site and yr ==============================================
#This is currently figure 2 of the manuscript
freq <- stray_dat %>%
  select(Year, StreamName, Number_surveys) %>%
  mutate(Year = as.factor(Year))

fr <- ggplot(freq, aes(Year, StreamName, color = Number_surveys,
                       size = Number_surveys)) +
  geom_point(alpha = 0.75) + theme_bw() +
  labs(y = "Stream name", color = "Number of
surveys", size = "Number of
surveys") +
  scale_size_continuous(range  = c(0, 3), 
                        limits = c(0, 32), 
                        breaks = c(0, 5, 10, 15, 20, 25)) +
  theme(text = element_text(family= "Times New Roman", size = 5.3),
        legend.key.size = unit(0.3, "cm"))
fr

#Export
# tiff("figs/survey_freq.tiff", width = 8.5, height = 8.5,
#      pointsize = 12, units = 'cm',
#      res = 600)
# fr
# dev.off()





#3. Avg # strays vs distance to nearest release site graph =====================
#This is currently figure 3 of the manuscript

#3.1. Data tailoring ===========================================================
stray_dat

plot(Avg_number_strays ~ Dist_nearest_R, data = stray_dat)
Dist_nearest_Releas <- read.csv("data/Dist_nearest_Releas.csv")
R_type <- left_join(stray_dat, Dist_nearest_Releas, by = "StreamName")
R_type <- as.data.frame(R_type)
sum(is.na(R_type$Release_site_type))

#Find row with max avg number of strays for each stream
R_type2 <- R_type %>% group_by(StreamName) %>% filter(Avg_number_strays == max(Avg_number_strays))
#Note that this^^ returns multiple rows if the max value is the same for any 
#streams (e.g., Little Goose Creek). Remove duplicate rows
R_type2 <- R_type2[!duplicated(R_type2$StreamName),]
###Since this might come up, NOTE that the higher avg_number_strays values are
#not influenced by higher number of surveys (sort R_type2$Avg_number_strays from
#largest to smallest and look at the number of surveys column) 



#3.2. Create figure ============================================================
fig3 <- ggplot(data = R_type2, aes(x = Dist_nearest_R, y = Avg_number_strays,
                                   shape = Release_site_type)) + geom_point(size = 1.8) +
  theme_classic() + scale_shape_manual(values = c(16, 2)) +
  xlab("Distance to the nearest release site (km)") +
  ylab("Average number of hatchery strays") + labs(shape = "Release site type") +
  theme(axis.text = element_text(size =6),
        axis.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        text=element_text(family="Times New Roman"),
        axis.line = element_line(linewidth = 0.2),
        legend.box.spacing = unit(0, "pt"))
fig3
#briefly quantitatively compare the average number of hatchery strays within 40km
#of release for on-site vs remote release sites
R_type40km <- R_type2[R_type2$Dist_nearest_R <= 40, ]
onsite <- R_type40km$Avg_number_strays[R_type40km$Release_site_type == "On-site"]
Remote <- R_type40km$Avg_number_strays[R_type40km$Release_site_type == "Remote"]
t.test(onsite, Remote) #streams near hatchery on-site releases averaged 21.6 strays
#while remote site-proximate streams averaged 71.8 strays. p = 0.21 (not significant)

#Export as high-res figure
# tiff("figs/fig3.tiff", width = 8.5, height = 7, pointsize = 12, units = 'cm',
#      res = 600)
# fig3
# dev.off()


#Remove unneeded items
rm(R_type, R_type2)







#4. Covariate effect plots =====================================================
#This is currently figure 3 of the manuscript/thesis

#Create function to determine model effects vector for each covariate:
create_eff <- function(mod_term){
  #Get vector of effects for each covariate:
  term <- deparse(substitute(mod_term)) #this line allows the fxn to
  #put an argument into something that should have quotes ("term" in the 
  #effect function below)
  eff <- effects::effect(term = term, mod = bm1, xlevels = 10)
  summary(eff)
  x <- as.data.frame(eff)
}

### Now create an individual effect plot for each covariate:


#4.1. Conspecific abundance effect plot ========================================
#not included in top model
# x_Cons <- create_eff(Cons_Abundance)
# #there's probably a better way to do this, but here is my method for now:
# #un-scale the data
# mean(stray_dat$Cons_Abundance, na.rm = T) #3021
# sd(stray_dat$Cons_Abundance, na.rm = T) #4277
# x_Cons$Cons_Abundance <- (x_Cons$Cons_Abundance * 4277) + 3021
# 
# 
# #Create plot
# Cons_plot <- ggplot() +
#   geom_line(data = x_Cons, aes(x = Cons_Abundance, y=fit)) +
#   geom_ribbon(data = x_Cons,
#               aes(x = Cons_Abundance, ymin = lower, ymax = upper),
#               alpha= 0.3, fill="grey70") +
#   xlab("Chum salmon abundance") +
#   ylab("Attractiveness index") + theme_classic() +
#   theme(axis.title = element_text(size = 11)) +
#   theme(axis.text = element_text(size = 11)) +
#   theme(text=element_text(family="Times New Roman")) #+
# #geom_point(data = trunc_Avg_strays, aes(x = Cons_Abundance,
# #y = Avg_number_strays)) #+ ylim(0, 20)
# Cons_plot 



#4.2. Number of fish released within 40km plot =================================
x_WMA <- create_eff(WMA_Releases_by_Yr)

#Un-scale the data
mean(stray_dat$WMA_Releases_by_Yr, na.rm = T) #16.3
sd(stray_dat$WMA_Releases_by_Yr, na.rm = T) #27.5
x_WMA$WMA_Releases_by_Yr <- (x_WMA$WMA_Releases_by_Yr * 27.5) + 16.3

#Create plot
WMA_plot <- ggplot() +
  geom_line(data = x_WMA, aes(x = WMA_Releases_by_Yr, y = fit)) +
  geom_ribbon(data = x_WMA,
              aes(x = WMA_Releases_by_Yr, ymin = lower, ymax = upper),
              alpha= 0.3, fill="grey70") +
  xlab("Number of fish released within 40 km") +
  ylab("Predicted attractiveness index") + theme_classic() +
  theme(axis.title = element_text(size = 13)) +
  theme(axis.text = element_text(size = 11.5)) +
  theme(text=element_text(family="Times New Roman"),
        plot.margin = margin(5,5,10-4)) #+
#geom_point(data = trunc_Avg_strays, aes(x = WMA_Releases_by_Yr,
#y = Avg_number_strays))#+ ylim(0, 20)
WMA_plot


#4.3. Coefficient of variation of streamflow plot ==============================
x_CV_flow <- create_eff(CV_flow)

#Un-scale the data
mean(stray_dat$CV_flow, na.rm = T) #0.53
sd(stray_dat$CV_flow, na.rm = T) #0.057
x_CV_flow$CV_flow <- (x_CV_flow$CV_flow * 0.057) + 0.53

#Create plot
CVflow_plot <- ggplot() +
  geom_line(data = x_CV_flow, aes(x = CV_flow, y = fit)) +
  geom_ribbon(data = x_CV_flow,
              aes(x = CV_flow, ymin = lower, ymax = upper),
              alpha= 0.3, fill="grey70") +
  xlab("CV of streamflow") +
  ylab("ln(predicted attractiveness index)") + theme_classic() +
  ylab("") + theme_classic() +
  theme(axis.title = element_text(size = 13)) +
  theme(axis.text = element_text(size = 11.5)) +
  theme(text=element_text(family="Times New Roman")) +
  xlim(0.395, 0.61) + ylim(0, 99)
CVflow_plot #gives you a warning about 3 removed rows; that is because of the 
#x-limit I set in the line above



#4.4. Combine all effects plots into one fig ===================================
all_effects_plot <- ggarrange(WMA_plot, CVflow_plot,
                              ncol = 2, labels = c("a)", "b)"),
                              label.x = -0.01, label.y = 0.99,
                              font.label = list(size = 11, family = "Times"))

#Export
# tiff('figs/effects_plots.tiff', width = 18, height = 9, pointsize = 12,
#      units = 'cm', res = 600)
# all_effects_plot2
# dev.off()







#5. CV flow artifact plot ======================================================
#Read in df containing flow + Sergeant et al. 2020 variables (created in CV_
#flow_side_analysis.R)
flow_plus <- readRDS("output/flow_plus.rds")

### Data tailoring
flow_plus$class_1 <- as.factor(flow_plus$class_1) #"class" needs to be a factor
#reorder it to show same watershed types next to each other on boxplot:
flow_plus$class_1 <- factor(flow_plus$class_1, levels = c("0", "3", "10", "8",
                                                          "1", "2", "4", "5", "6"))
#See Sergeant et al. 2020 for more in-depth explanation, but class_1 in a nutshell
#is the watershed type associated with each CV_flow value assigned to each stream.
#Page 8 of Sergeant et al. 2020 shows what the different watershed types are
#associated with each class # (e.g., class_1 = 8 is a rain-snow watershed)

flow_plot <- ggplot(flow_plus) +
  geom_boxplot(aes(x = class_1, y = CV_flow),
               linewidth = 0.3,
               outlier.size = 0.5) +
  labs(x = "Watershed type", y = "CV of streamflow") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 7)) +
  #theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(labels = c("0" = "Rain-0", "1" = "Snow-1", "2" = "Snow-2",
                              "3" = "Rain-3", "4" = "Snow-4", "5" = "Snow-5",
                              "6" = "Glacier-6", "8" = "Rain-snow-8",
                              "10" = "Rain-10")) +
  rotate_x_text(angle = 35)
flow_plot

#Export as high-res figure
# tiff("figs/CVflow_side_plot.tiff", width = 8.5, height = 5, pointsize = 12,
#      units = 'cm', res = 600)
# flow_plot
# dev.off()



