#Note that some figures and tables made more sense to create in Model_fitting3.R,
#so if you can't find the code for a figure you expected to find here then check
#that script 


setwd("~/Documents/CHUM_THESIS/Analysis")
Master_dataset <- read.csv("Master_dataset.csv")
#Year is a factor variable!
Master_dataset$Year <- factor(Master_dataset$Year)




#Asynchronous metapopulation dynamics figure ###################################
#Create the dataframe
Year <- as.factor(c(2013:2015, 2013:2015))
Type <- as.factor(c(rep("(Average) Hatchery strays",3), rep("Natural origin", 3)))
Whitewater <- c(4,2,17,2210,365,2009)
Sister <- c(4,8,6,8186,7858,3993)
Crawfish <- c(6,2,2,4138,3036,6908)
Marten <- c(2,0,6,7626,473,5037)
metapop <- cbind.data.frame(Year, Type, Whitewater, Sister, Crawfish, Marten)
names_new <- c("Whitewater Creek", "Sister Lake SE Head", "W Crawfish NE Arm Hd",
               "Marten River")
metapop <- metapop %>% rename_at(3:6, ~names_new)

metapop2 <- metapop %>% pivot_longer(c(3:6), names_to = "Stream")
colnames(metapop2)[4] <- "Abundance"


#Create the figure
temporal_var <- ggplot(metapop2, aes(Year, Abundance, group = Stream, col = Stream)) +
  geom_line() + geom_point() + facet_wrap(~Type, scales = "free", ncol = 1) +
  theme_bw() + scale_color_brewer(palette = "Paired")

#export high-res figure
tiff('temporal_var.tiff', width = 8, height = 6.3, pointsize = 12,
     units = 'in', res = 300)
temporal_var
dev.off()


#Figure 1. Observed number of strays + hatchery locations map ##################
################################################################################

#from previously when testing for spatial autocorrelation; gives average of the 
#average number of strays for each streams across all years sampled:
Avg_strays_by_Yr_w_coords <-
  read_csv("~/Documents/CHUM THESIS/Manuscript/Spatial_AutoC/Avg_strays_by_Yr_w_coords.csv")
#remove Fish Creek-Portland Canal
Avg_strays_by_Yr_w_coords <-
  Avg_strays_by_Yr_w_coords[!Avg_strays_by_Yr_w_coords$StreamName ==
                              "Fish Creek-Portland Canal", ]

#include number of times a stream was surveyed on figure: 
surv <- Master_dataset %>% group_by(StreamName) %>%
  summarise(Number_of_Surveys = sum(Number.of.surveys))
Avg_strays_by_Yr_w_coords <- left_join(Avg_strays_by_Yr_w_coords, surv, by = "StreamName")


#read in hatchery locations
Hatchery_Locations <- read_csv("~/Documents/CHUM THESIS/Data Sources/Hatchery_Locations.csv")

#remove Wally Noerenberg and Nitinat River hatcherys
Hatchery_Locations <- Hatchery_Locations[c(1:12), ]

library(ggmap)
myMap <- get_stamenmap(location <- c(-137, 54.5, -130, 59.5), zoom = 6,
                       maptype = "terrain-background", color = "bw", crop = TRUE)
ggmap(myMap)
fig1 <- ggmap(myMap) + geom_point(aes(x = Longitude, y = Latitude,
                                      size = Number_of_Surveys,
                                      fill = Average_Strays),
                                  colour = "black", pch = 21,
                                  data = Avg_strays_by_Yr_w_coords) +
  scale_size_continuous(range = c(2, 10)) +
  xlab("Longitude") + ylab("Latitude") + theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.key=element_blank()) + labs(size = "# of Surveys") +
  labs(fill = "Average # of Strays")

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
fig1b <- fig1a + inset(grob = ggplotGrob(alaska), xmin = -132.75, xmax = -129.75,
                       ymin = 58.2, ymax = 59.55) 

#add scale bar and north arrow
library(ggsn)
fig1c <- fig1b + scalebar(x.min = -137.2, x.max = -135.2, y.min = 54.8, y.max = 55, 
                          dist = 50, dist_unit = "km", transform = T, height = 0.5,
                          st.dist = 0.6, st.size = 5)
north2(fig1c, x = 0.2, y = 0.22, symbol = 3)



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
  labs(fill = "Average # of Strays")
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
#add scalebar
fig1d_bw <- fig1c_bw + scalebar(x.min = -137, x.max = -135, y.min = 54.8,
                                y.max = 55, dist = 50, dist_unit = "km",
                                transform = T, height = 0.5, st.dist = 0.6,
                                st.size = 5)
#add north arrow
north2(fig1d_bw, x = 0.2, y = 0.22, symbol = 3)


#Export as high-res figure
setwd("~/Documents/CHUM THESIS/Manuscript/Figures")
tiff("fig1.tiff", width = 7, height = 6, pointsize = 12, units = 'in', res = 300)
north2(fig1c, x = 0.2, y = 0.22, symbol = 3) #graph that you want to export
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


fig4 <- ggplot(data = R_type2, aes(x = Dist_nearest_R, y = Avg_number_strays,
                                   shape = Release_site_type)) + geom_point(size = 3.5) +
  theme_classic() + scale_shape_manual(values = c(16, 2)) +
  xlab("Distance to the Nearest Release Site (KM)") +
  ylab("Average Number of Hatchery Strays") + labs(shape = "Release Site Type") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14))
fig4

#briefly quantitatively compare the average number of hatchery strays within 40km
#of release for on-site vs remote release sites
R_type40km <- R_type2[R_type2$Dist_nearest_R <= 40, ]
onsite <- R_type40km$Avg_number_strays[R_type40km$Release_site_type == "On-site"]
Remote <- R_type40km$Avg_number_strays[R_type40km$Release_site_type == "Remote"]
t.test(onsite, Remote) #streams near hatchery on-site releases averaged 19.3 strays
#while remote site-proximate streams averaged 38.6 strays. p = 0.34 (not significant)

#Export as high-res figure
tiff("fig4.tiff", width = 7, height = 6, pointsize = 12, units = 'in', res = 300)
fig4 #graph that you want to export
dev.off( ) #now the displayed graphs are saved to a file with the above file name



#Figure 3. Across-year variability in # of strays ##############################
################################################################################
#Use dataset "f", which is what I used for modeling (it has a few streams filtered
#out of it that I didn't want to include, see Model_fitting2.R for more info):
delete <- c("Chilkat River", "Disappearance Creek", "Black River", "Herman Creek")
f <- Master_dataset[!(Master_dataset$StreamName %in% delete),]
#only delete 2010 for Saook Bay West Head, not 2011:
f <- f[!(f$Year == "2010" & f$StreamName == "Saook Bay West Head"), ]
#make year a factor so it will plot as discrete years
f$Year <- as.factor(f$Year)

#Let's use Fish Creek, Sister Lake SE Head, and Kadashan River because they all
#have at least 4 years of data collection and are quite variable. Sister Lake SE
#Head in particular is variable in attractiveness ranking over time as well as #
#of strays
Fish <- f[f$StreamName == "Fish Creek", c(1:3, 9)]
Sister <- f[f$StreamName == "Sister Lake SE Head", c(1:3, 9)]
Kad <- f[f$StreamName == "Kadashan River", c(1:3, 9)]

#add missing data years and rbind to data series for each creek
View(Fish) #missing 2008 and 2011
Fish_nas <- data.frame("Subregion" = rep("NSE Inside", 2),
                       "Year" = c(2008, 2011),
                       "StreamName" = rep("Fish Creek", 2),
                       "Avg_number_strays" = rep(NA, 2))
Fish <- rbind.data.frame(Fish, Fish_nas)

View(Sister) #missing 2009-2011 and 2017-2019
Sister_nas <- data.frame("Subregion" = rep("NSE Inside", 6),
                         "Year" = c(2009:2011, 2017:2019),
                         "StreamName" = rep("Sister Lake SE Head", 6),
                         "Avg_number_strays" = rep(NA, 6))
Sister <- rbind.data.frame(Sister, Sister_nas)

View(Kad) #missing 2008, and 2017-2019
Kad_nas <- data.frame("Subregion" = rep("NSE Inside", 4),
                      "Year" = c(2008, 2017:2019),
                      "StreamName" = rep("Kadashan River", 4),
                      "Avg_number_strays" = rep(NA, 4))
Kad <- rbind.data.frame(Kad, Kad_nas)

var_strms <- rbind.data.frame(Fish, Sister, Kad)
var_strms$StreamName <- factor(var_strms$StreamName, levels = c("Fish Creek",
                                                                "Sister Lake SE Head",
                                                                "Kadashan River"))
var_strms$Avg_number_strays[var_strms$Avg_number_strays == 0] <- 0.03 #I'm 
#adding a small amount here to the stream surveys where 0 strays were detected 
#(but a survey still occurred). That way on the graph, a tiny bar will appear to
#indicate "0 strays detected" and differentiate those stream-years from stream-yrs
#where surveys didn't occur

View(var_strms)
p <- ggplot() + geom_col(data = var_strms, aes(x = Year, y = Avg_number_strays)) +
  facet_wrap(~StreamName, scales = "free", nrow = 3, ncol = 1) +
  ylab("Average Number of Strays") + theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 10)) 
p #looks good

#Export as high-res figure
tiff("fig3.tiff", width = 7, height = 6, pointsize = 12, units = 'in', res = 300)
p #graph that you want to export
dev.off( ) #now the displayed graphs are saved to a file with the above file name




#effects plot code can be found in Model_fitting3.R because I made those directly
#from datasets contained within that script file


#Table 3: pred and observed values #############################################
tab3.1 <- fu_scaled %>% group_by(StreamName) %>%
  summarise(across(Avg_number_strays, c(min,max)), across(Number_surveys, sum))
#from Model_fitting3.R:
head(mean_bm1u_pred)
tab3 <- left_join(tab3.1, mean_bm1u_pred)
tab3 <- tab3[,c(1,5,6,2:4)]
name_update <- c("Mean predicted number of strays", "Mean observed number of strays",
                 "Minimum observed number of strays", "Maximum observed number of strays",
                 "Total number of surveys")
tab3 <- tab3 %>% rename_at(2:6, ~name_update)
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

#Table S3 stuff ################################################################
#Using the dataframe "f" from Model_fitting3.R:
w <- f[ , c(1:5, 8)]
View(w)
setwd("~/Documents/CHUM THESIS/Manuscript/Figures")
write.csv(w, "Table_S3.csv")





save.image("Manu_figs_objects.R")
load("Manu_figs_objects.R")