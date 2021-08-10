#Molly K. Payne
#Stray Model Code 2020-2021
#If not otherwise cited, my sources of information which guided my modeling 
#process were Zuur et al. 2009, lots of stack exchange, and some help from Ben
#Bolker (http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html)


#1. Read in and tailor the data as necessary ===================================
setwd("~/Documents/CHUM_THESIS/Analysis")
Master_dataset <- read.csv("Master_dataset.csv")
View(Master_dataset)
#Stream_hydro and year are categorical variables!
#Master_dataset$Hydrology_Type <- factor(Master_dataset$Hydrology_Type)
#Hydrology_Type^^ is no longer included in the model, see Hydro_methods_detail for 
#more information


Master_dataset$Year <- factor(Master_dataset$Year)
str(Master_dataset)
#note, variables are NOT scaled (z-scored at the moment)


#remove Chilkat River, Disappearance Creek, Black River, Saook Bay West Head in
#2010 only, and Herman Creek from analysis. These creeks were sampled either 
#really late bc they are fall-run (Chilkat + Dis.Creek), or early (before 7/20).
delete <- c("Chilkat River", "Disappearance Creek", "Black River", "Herman Creek")
f <- Master_dataset[!(Master_dataset$StreamName %in% delete),]
#only delete 2010 for Saook Bay West Head, not 2011:
f <- f[!(f$Year == "2010" & f$StreamName == "Saook Bay West Head"), ]
#If you do choose to keep these creeks in the future, I'm only deleting them here
#in R. They are still in the original excel Master_dataset. Note that the Total_
#strays_by_subregion and Total_stray_all_SEAK will differ slightly now because
#those columns were calculated in excel and include the strays from the creeks
#I just deleted^^. So for 2009, if you sum on Number_H_fish, it will give you 411,
#when the Total_strays_all_SEAK column reads 414 for 2009. In 2010, there were 
#557 total strays in SEAK, but with the removal of the above creeks, there are now
#548:
sum(Master_dataset$Number_H_fish[Master_dataset$Year == "2010"]) #557
sum(f$Number_H_fish[f$Year == "2010"]) #548
#Keep this in mind in case you are ever using the total_strays columns for anything
View(f)

#Update 2/8/21 -> I'm changing the NA values in WMA_Releases_by_Yr to 0s, because
#I don't consider the data to be missing where there aren't release sites within
#40 km of a stream. Rather, the 40km radii were "sampled" and did not 
#come up with any fish being released in the vicinity
f$WMA_Releases_by_Yr[is.na(f$WMA_Releases_by_Yr)] <- 0

#some quick visualization/exploration ----
?boxplot
#response variable:
boxplot(Avg_number_strays ~ Year, data = f) #clearly some differences in years,
#even within years where sampling pressure was more equivalent
#explanatory variables
boxplot(Fishery_harvest ~ Year, data = f) #high in 2013
boxplot(Cons_Abundance ~ Year, data = f)
boxplot(Pink_Abundance ~ Year, data = f)
boxplot(WMA_Releases_by_Yr ~ Year, data = f) #higher in 2017-2019, which makes sense
boxplot(Dist_nearest_H ~ Year, data = f) #significantly higher in 2008
boxplot(Dist_nearest_R ~ Year, data = f) #high in 2008, low in 2017-2019
boxplot(mean_flow ~ Year, data = f, na.action = na.omit)
boxplot(CV_flow ~ Year, data = f, na.action = na.omit) #high in 2017-2019 ----

pairs(f[ , c(9:19)]) #collinearity likely between Dist_nearest_H and Dist_nearest_R
#also between the abundance and density predictors for each respective species

boxplot(WMA_Releases_by_Yr ~ Subregion, data = f) #more releases within 40km
#of streams in NSEI
boxplot(Avg_number_strays ~ Subregion, data = f) #but doesn't overwhelmingly show
#up in the response

#remove rows containing NA values (9 rows, 3 streams). As I find out later and 
#after reading about missing values in Zuur, it is best to remove the rows con-
#taining NA values since there will be many issues with model validation later
#on if you don't 
f <- f[complete.cases(f), ]
View(f)




#1.5. Check for spatial autocorrelation ========================================
#I have multiple years of data with considerable variation between years, so I
#will average the # of strays across years to check for spatial autocorrelation

#Create table of average of Avg_number_strays and link coordinates to it
StreamPoints <- read_csv("~/Documents/CHUM THESIS/Data Sources/StreamPoints.csv")
spat <- inner_join(Master_dataset, StreamPoints, by = "StreamName")
spatio_sum <- spat %>% group_by(StreamName) %>% summarise(Average_Strays =
                                                            mean(Avg_number_strays))
View(spatio_sum)
spatio_sum2 <- merge(spatio_sum, spat[ , c("StreamName", "Latitude", "Longitude")],
                     by = "StreamName", all.x = T) #attach coordinates
spatio_sum3 <- unique(spatio_sum2) #the above created a row for each year for 
#each stream, so remove the duplicate rows
View(spatio_sum3) #looks good
#export in case you want for later reference:
setwd("~/Documents/CHUM THESIS/Manuscript/Spatial_AutoC")
write.csv(spatio_sum3, "Avg_strays_by_Yr_w_coords.csv")


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
Moran.I(spatio_sum3$Average_Strays, dist_mat_inv) #p = 0.38, fail to reject 
#null = no spatial autocorrelation occurring 

#Average # of strays mapped to visualize (I now know nonexistent) spatial
#autocorrelation:
#NOTE 6/16/21: This figure may not actually be necessary since Fig1 of the main 
#paper shows the same thing
library(ggmap)
myMap <- get_stamenmap(location <- c(-137.5, 54.5, -129.75, 59.5), zoom = 6,
                       maptype = "terrain-background", color = "bw", crop = TRUE)
ggmap(myMap) #see map
ptmap <- ggmap(myMap) + geom_point(aes(x = Longitude, y = Latitude,
                                       size = Average_Strays),
                                   data = spatio_sum3) +
  xlab("Longitude") + ylab("Latitude") + theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.key=element_blank()) + labs(size = "Average # Strays") 
  #scale_fill_manual(values = "white")
ptmap

#add inset map
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
alaska

ptmap1 <- ptmap + inset(grob = ggplotGrob(alaska), xmin = -133, xmax = -129.65,
                       ymin = 58.2, ymax = 59.5) 
library(ggsn)
ptmap2 <- ptmap1 + scalebar(x.min = -137, x.max = -135, y.min = 54.8, y.max = 55, 
                          dist = 50, dist_unit = "km", transform = T, height = 0.5,
                          st.dist = 0.6, st.size = 5)
north2(ptmap2, x = 0.22, y = 0.22, symbol = 3)

#Export as high-res figure
setwd("~/Documents/CHUM_THESIS/Manuscript/Spatial_AutoC")
tiff("spati_auto_map.tiff", width = 7, height = 6, pointsize = 12, units = 'in', res = 300)
north2(ptmap2, x = 0.22, y = 0.22, symbol = 3) #graph that you want to export
dev.off( ) #now the displayed graphs are saved to a file with the above file name







#2. Check for collinearity =====================================================
#check for correlation between covariates
?cor #cor() default gives Pearson's correlation coefficients
cor(f[ , c(10:19)])
#Cons_Abundance and Cons_Density are highly correlated (0.7+)
#Pink_Abundance and Pink_Density are highly correlated (0.7+)
#Dist_nearest_H and Dist_nearest_R are highly correlated (0.7+)
#Dist_nearest_H and WMA_Releases_by_Yr are highly correlated (-0.56)
#Dist_nearest_R and WMA_Releases_by_Yr are highly correlated (0.7+)
#CV_flow and Pink_Density highly are correlated (-0.48), CV_flow borderline with 
#Cons_Abundance, Cons_Density, and Pink_Abundance (-0.30-0.39 correlation)

#I will remove Cons_Density and Pink_Density from the model since they are the 
#less important of the correlated sets (mostly bc I don't have a lot of con-
#fidence in the density denominator data (area))

#I will also remove Dist_nearest_R from consideration because it is highly corr-
#elated with Dist_nearest_H and WMA_Releases_by_Yr, and because it seems like 
#WMA_Releases_by_Yr captures Dist_nearest_R effect relatively well on its own:
plot(f$Dist_nearest_R ~ f$WMA_Releases_by_Yr)
#Dist_nearest_H and WMA_Releases_by_Yr are not as highly correlated, I will leave
#both for now and address again later when fitting the model 

plot(f$CV_flow ~f$Cons_Abundance)
plot(f$CV_flow ~f$Pink_Abundance)

#percent_lake_wetL_A was also removed from consideration because I determined 
#that its result would be interpreted essentially the same as that of CV_flow





#3. Check contingency table for the categorical covariate ======================
#Hydrology_Type no longer being tested in the model. See Hydro_methods_detail for
#more information

#table(f$Hydrology_Type)
#heavily skewed towards snow-fed streams (141) and no glacial streams now with 
#the earlier deletion of the Chilkat River (1/13/20). Rain-snow only 10, but I 
#don't want to delete this category because then this covariate will only have
#two levels to compare






#4. Transform continuous covariates as necessary ===============================
#variables that are log(x + 1) transformed vs just log transformed have 1 added
#to them because there are zeros in their columns (can't take log of zero)

hist(f$Fishery_harvest)
hist(f$Cons_Abundance) #log transform
hist(f$Pink_Abundance) #log(x + 1) transform (add 1 bc of 0s in this column)
hist(f$WMA_Releases_by_Yr) #log(x + 1) transform (not much better but what can
#you do?)
hist(f$Dist_nearest_H)
hist(f$mean_flow) #log transform
hist(f$CV_flow)

#log + 1 transform 
trans_dat <- f %>% mutate(Pink_Abundance = log(Pink_Abundance + 1),
                          WMA_Releases_by_Yr = log(WMA_Releases_by_Yr + 1))
         
trans_dat <- trans_dat %>% mutate(Cons_Abundance = log(Cons_Abundance),
                                  mean_flow = log(mean_flow))
View(trans_dat)
#also check for NAs in these columns using is.na()
apply(trans_dat[ , c(9:19)], MARGIN = 2, FUN = is.na)#this prints out T/F vals :/
NAs <- trans_dat[rowSums(is.na(trans_dat)) > 0, ] #easier format to see NA rows
View(NAs) #looks good (there are none)



#5. Scale continuous covariates ================================================

#On scaling (Z-scoring) your continuous covariates: You should z-score within 
#years across streams for time-independent covariates like mean_flow and across 
#years and streams for time-varying covariates like Cons_Abundance. This is bc 
#your unit of replication is at the stream level (year is RE)

#Update (5/26/21): I don't think this is correct anymore bc comparing time-
#invariant covariates within a year can change their values substantially depend-
#ing on how many other streams are included in the model in that year. See model
#devo doc for more explanation. I will now instead scale all covariates across 
#year and stream

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
} #for scaling within year

#time-independent predictors
#scaled_data <- trans_dat %>% group_by(Year) %>%
 # mutate(mean_flow = scale_this(mean_flow),
     #    CV_flow = scale_this(CV_flow))

#time varying predictors; scale across years and streams:
scaled_data <- apply(X = trans_dat[ , c(10:11, 13, 15, 18:19)], 2,
                     scale(center = T, scale = T)) #not working

#do individually by covariate (column) instead
scaled_data <- trans_dat %>% mutate(Fishery_harvest = scale(Fishery_harvest,
                                                            center = T, scale = T))
scaled_data$Cons_Abundance <-
  scale(scaled_data$Cons_Abundance, center = TRUE, scale = TRUE)
scaled_data$Pink_Abundance <-
  scale(scaled_data$Pink_Abundance, center = TRUE, scale = TRUE)
scaled_data$WMA_Releases_by_Yr <-
  scale(scaled_data$WMA_Releases_by_Yr, center = TRUE, scale = TRUE)
scaled_data$Dist_nearest_H <-
  scale(scaled_data$Dist_nearest_H, center = TRUE, scale = TRUE)
scaled_data$mean_flow <-
  scale(scaled_data$mean_flow, center = TRUE, scale = TRUE)
scaled_data$CV_flow <-
  scale(scaled_data$CV_flow, center = TRUE, scale = TRUE)
View(scaled_data)







#6. Check distribution/transform response variable =============================

#Consider rounding response instead of transforming (bc you have decimals) and
#then fitting to a poisson or negative binomial (if there is overdispersion) to 
#properly fit right-skewed data
scaled_data$Avg_number_strays <- round(scaled_data$Avg_number_strays)
#I don't expect this to affect interpretation. 0.4 fish getting rounded to 0 is
#still less attractive than a stream with 1 or more fish. And I think it is re-
#quired to be able to use poisson or negative binomial families/functions


#response (Avg_number_strays) is not transformed at this point in time
hist(scaled_data$Avg_number_strays, col = "blue") #very right-skewed, definitely
#not normal
ggqqplot(scaled_data$Avg_number_strays) #not normal
shapiro.test(scaled_data$Avg_number_strays) #p < 0.0001, data is significantly 
#different from normal distribution

hist(log(scaled_data$Avg_number_strays + 1), col = "blue") #maybe normal?
ggqqplot(log(scaled_data$Avg_number_strays + 1)) #not normal
shapiro.test(log(scaled_data$Avg_number_strays + 1)) #p < 0.0001, data is 
#significantly different from normal distribution. That being said, I would apply
#this transformation to squash some of the variance associated with all of the 
#values <5 that are causing so many problems
scaled_data$Avg_number_strays <- log(scaled_data$Avg_number_strays + 1) #WAIT, 
#this won't work bc you now have non-integer response. And I wouldn't round them
#after log(x + 1) transforming bc the range of values is only 0-6 now, effectively
#removing most of the variation in your response

#CONLUSION: Round the response to the nearest integer but otherwise leave un-
#transformed



#Need to check for overdispersion with poisson first. You can do this by fitting
#the model and checking the ratio of residual deviance to degrees of freedom
library(lme4)
summary(glmer(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                Dist_nearest_H + mean_flow + CV_flow +
                Dist_nearest_H:CV_flow, family = "poisson",
              data = scaled_data))
#Residual deviance is 1818.1 for 168 degrees of freedom for a ratio of 10.8 in
#mod w/o interactions indicating severe overdispersion (the ratio should be 1). 
#Source of information:
#https://biometry.github.io/APES/LectureNotes/2016-JAGS/Overdispersion/OverdispersionJAGS.pdf

#Hilbe 2007 (book called "Negative Binomial Regression") recommends constructing
#required interactions (pg. 157) to deal with overdispersion or apparent overdis.
#the interactions I added iteratively to the model above included:
#Cons_Abundance:Pink_Abundance -> new ratio = 1818.1/167 = 10.9
#Cons_Abundance:mean_flow -> new ratio = 1812.6/167 = 10.9
#Cons_Abundance:CV_flow -> new ratio = 1749.8/167 = 10.8
#Pink_A with both flow variables not worth reporting, ratio was >10.7
#Dist_nearest_H:mean_flow = 1738.2/167 = 10.4
#Dist_nearest_H:CV_flow = 1765.0/167 = 10.6

#It is also recommended (by Hilbe 2007) to check for and deal with zero-inflation
#as this may address overdispersion. My attempts at modeling a zero-inflated
#mixed effects model are below

#1) 
install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")
library(glmmADMB)

attempt1 <- glmmadmb(Avg_number_strays ~ (1|Year) + Cons_Abundance +
                       Pink_Abundance + WMA_Releases_by_Yr + Dist_nearest_H +
                       CV_flow, data = scaled_data, zeroInflation = TRUE,
                     family = "nbinom") #error that I don't understand

#2)
install.packages("GLMMadaptive")
library(GLMMadaptive)
?mixed_model
attempt2 <- mixed_model(Avg_number_strays ~ Cons_Abundance + Pink_Abundance +
                          WMA_Releases_by_Yr + Dist_nearest_H + CV_flow,
                        random = ~ 1 | Year, data = scaled_data,
            family = zi.negative.binomial()) #this function requires arguments
#for zi_fixed and zi_random and I don't understand what those are. There is a bit
#more description here but I'm still confused: 
#https://drizopoulos.github.io/GLMMadaptive/articles/ZeroInflated_and_TwoPart_Models.html

#3) 
library(glmmTMB)
attempt3 <- glmmTMB(Avg_number_strays ~ (1|Year) + Cons_Abundance + CV_flow +
          Dist_nearest_H + Pink_Abundance + I(Pink_Abundance^2) +
            WMA_Releases_by_Yr + Cons_Abundance:CV_flow, data = scaled_data,
          family = nbinom1, ziformula = ~.) #note that this is a quasi-poisson
#(nbinom1), not negbin.
#I couldn't get a negative binomial (nbinom2) to work. Many, many warnings and 
#does not give any SEs, z-vals, or p-vals
#3/9/21 -> move on from this -Peter. The model that does not account for zero-
#inflation fits the data quite well, so you can justify it to reviewers


#Unless I can figure out 1), 2), or 3) above^^, it seems that accounting for zero-
#inflation and including a random effect are incompatible (2/5/21)

#Final thing I will try before ignoring zero-inflation or making year a FE:
#Fit a mixed model using the Tweedie distribution from the cpglmm package, which
#is designed for count data with many 0s. It is technically the "Tweedie compound
#Poisson distribution", so still check for overdispersion after bc of Poisson
install.packages('cpglmm')
#says I need to update R...
#still not available after updating to the latest version (4.0.4)?
av <- available.packages(filters=list())
av[av[, "Package"] == 'cpglmm', ] #my license restricts use apparently. I give up.
#I will not be trying the Tweedie distribution (2/5/21)






#7. Fit model ==================================================================
#Fit NB mixed effects model without accounting for zero-inflation
stray_1 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                      Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                      Dist_nearest_H + mean_flow + CV_flow, data = scaled_data)
summary(stray_1) 
#also try 
broom::tidy(stray_1)
#residual deviance is 892.9 for 167 degrees of freedom, for a ratio of 5.3. That
#is still overdispersed. Peter suggests continuing on to see if it predicts OK

#Hydrology_TypeSnow is highly correlated with Hydro_TypeRain, the model intercept,
#and mean_flow (from previous model dataset which included Hydro_type predictor,
#see Hydro_methods_detail.docx for more information)
#Dist_nearest_H HIGHLY correlated with WMA_releases (0.579) and CV_flow (-0.544)
#Some correlation between CV_flow and Cons_Abundance (0.373) and WMA_Releases_by_Yr
#(-0.372), respectively. But, both are well under 0.5 correlation so I will leave
#them


#NOTE: Since your n/k < 40, where n is the # of observations and k is the # of
#parameters in the global (fullest) model including the RE (i.e., n/k = 177/10 = 
#17.7), you need to calculate the AICc, instead of the AIC, which accounts for 
#small sample sizes
library(MuMIn)
AICc(stray_1) #914.2

##############
#No longer applicable bc Hydro_type isn't in model anymore. I kept the following
#8 lines in case you volver a incluir Hydro_type in a future version:
#MuMIn:dredge won't consider the categorical covariate (Hydrology_Type), so
#remove it from the full model and compare:
#stray_1.5 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                      #Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                      #Dist_nearest_H + mean_flow + CV_flow, data = scaled_data)
#summary(stray_1.5) #no significant change in coef or significance from stray_1
#AICc(stray_1.5) #960.1
#Since the AICc differs by almost 4 points and Hydrology_Type has some collin-
#earity issues with other covariates (described ~18 lines above), I will leave 
#Hydrology_Type out of the model
#################


#Dist_nearest_H + WMA_Releases_by_Yr are highly correlated (0.579), as are Dist_
#nearest_H + CV_flow (-0.544), so remove Dist_nearest_H from model
stray_2 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                      Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                      mean_flow + CV_flow, data = scaled_data)
summary(stray_2) #all correlations are now <0.35
AICc(stray_2) #916.3


# Check for complex regressors (higher order terms and interactions) ###########
#higher order terms first ######################################################
stray_3 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                      Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                      mean_flow + CV_flow + I(CV_flow^2), data = scaled_data)
AICc(stray_2) #916.3 (w/o interactions)
AICc(stray_3)
summary(stray_3)
#I(Fishery_harvest^2) AICc: 918.6, SE: 0.122, p = 0.89
#I(Cons_Abundance^2) AICc: 918.6, SE: 0.080, p: 0.84
#I(Pink_Abundance^2) AICc: 915.1, SE: 0.150, p: 0.06
#I(WMA_Releases_by_Yr^2) AICc: 911.2, SE: 0.27, p: 0.01 #improvement
#I(mean_flow^2) AICc: 915.4, SE: 0.059, p: 0.09
#I(CV_flow^2) AICc: 870.6, SE: 0.081, p: 7.14e-13 #improvement
#I(CV_flow^2)+I(WMA_Releases_by_Yr^2) AICc: 869.6 #some improvement from
#CV_flow^2 alone
plot(Avg_number_strays ~ CV_flow, data = scaled_data) #increases exponentially @
#higher values
plot(Avg_number_strays ~ WMA_Releases_by_Yr, data = scaled_data) #on average there
#is higher # of strays at higher WMA_Releases_by_Yr. The quadratic term lowered 
#the AICc because there is a cluster of 0-20 # of strays at the highest WMA_Releas.

#Burnham and Anderson 2002 write that models differing by 0-2 AIC have essentially
#the same amount of support. The best model here contains WMA_Releases and CV_flow
#as quadratic terms, but the model containing both of those is only 1 AIC point
#better than the model with CV_flow^2 only
#CONCLUSION: Keep model with CV_flow^2, but not not WMA_Releases_by_Yr^2 to avoid
#extra terms/fewer df/additional model complication



#possible interactions #########################################################
stray_4 <- glmer.nb(Avg_number_strays ~ (1|Year) + Fishery_harvest +
                      Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
                      mean_flow + CV_flow + I(CV_flow^2) +
                      WMA_Releases_by_Yr:Cons_Abundance, data = scaled_data)
#remember the AICc for the current best model w/o interactions:
AICc(stray_3) #870.6
AICc(stray_4)
summary(stray_4)
#Fishery_harvest:Cons_Abundance AICc: 872.3 SE: 0.090, 0.46
#Cons_Abundance:Pink_Abundance AICc: 872.8, SE: 0.139, p: 0.72
#Cons_Abundance:mean_flow AICc: 871.4, SE: 0.101, p: 0.23 
#Cons_Abundance:CV_flow AICc: 868.3, SE: 0.102, p: 0.03 #improvement
#Pink_Abundance:mean_flow AICc: 872.9, SE: 0.120, p: 0.86
#Pink_Abundance:CV_flow AICc: 870.3, SE: 0.153, p: 0.11
#WMA_Releases_by_Yr:Pink_Abundance AICc: 872.9, SE: 0.147, p: 0.94
#WMA_Releases_by_Yr:Cons_Abundance AICc: 870.3, SE: 0.104, p: 0.105
#The Cons_Abundance:CV_flow interaction improved the model fit by 2.3 AICc 
#points, but the model is struggling to converge at this point with so many 
#terms. Given that the improvement with this interaction term was small, I will
#leave it out
#CONCLUSION: NO INTERACTIONS NEEDED

summary(stray_4) #CV_flow^2 correlated with intercept (-0.424). Leave in for now
#and see if it remains after (MuMIn) dredging

#check for multicollinearity before proceeding
car::vif(stray_3) #all vif are <2


save.image(file = "Model_fitting2_objects.Rdata")
load("~/Documents/CHUM_THESIS/Analysis/Model_fitting2_objects.Rdata")






#7.5. Model averaging (if necessary) and final model selection =================

#Run current global model (stray_3) through MuMIn::dredge() to decide final model
#you may need to run the following command to get MiMIn::dredge() to work:
options(na.action = "na.fail") #prevent fitting models to different-sized datasets

straymod_dredge <- dredge(stray_3, rank = "AICc")
straymod_dredge #Burnham and Anderson 2002 (pg 70) write that delta AIC differences
#of 0-2 models all have substantial support, so I will keep and average my models
#which have delta AICc <= 2 when model averaging (those are the top 2 models,
#which collectively include 0.592, or 59% of the weight).

#Numerous papers caution against averaging the estimated regression coefficients
#based on AIC weights due to issues with multicollinearity between predictors 
#(hard to avoid in observational studies), chief among them is Cade 2015 (Ecology). 
#A better alternative is to average the predictions (rather than the coefficient
#estimates) of the top candidate models, hereby leaving the estimated coefficients
#alone. That is what I will do to make predictions here, using the top 3 models
#from straymod_dredge
bm1 <- glmer.nb(Avg_number_strays ~ (1|Year) + Cons_Abundance + Pink_Abundance +
                  WMA_Releases_by_Yr + mean_flow + CV_flow + I(CV_flow^2),
                data = scaled_data) #"bm" for "best model", accounts for 30% of 
#total weight
summary(bm1) #I(CV_flow^2) -0.421 correlation with intercept
#849.4/168 = 5.1 overdispersion ratio
car::vif(bm1) #all are <2

bm2 <- glmer.nb(Avg_number_strays ~ (1|Year) + Pink_Abundance +
                  WMA_Releases_by_Yr + mean_flow + CV_flow + I(CV_flow^2),
                data = scaled_data) #accounts for 28% of total weight 
summary(bm2) #I(CV_flow^2) -0.401 correlation with intercept
#851.8/169 = 5.0 overdispersion ratio 
car::vif(bm2) #all are <2

Mod_1_pred <- predict(bm1)
Mod_2_pred <- predict(bm2)
Stray_model_predictions <- data.frame(Mod_1_pred, Mod_2_pred)
#give more weight to bm1 predictions when model-averaging!
#bm1 weight:
0.309/0.592 #0.522

#bm2 weight:
0.283/0.592 #0.478

Stray_model_predictions$pred_log_strays <-
  (Stray_model_predictions$Mod_1_pred * 0.522) +
  (Stray_model_predictions$Mod_2_pred * 0.478)

Stray_model_predictions$predicted_strays <- exp(Stray_model_predictions$pred_log_strays)
View(Stray_model_predictions)


#It doesn't seem like dredge() gives the null model for comparison, which strikes 
#me as strange. Maybe that has to do with the RE. So anyway, I will compare to
#the null model here:
library(MASS)
null_mod <- glm.nb(Avg_number_strays ~ 1, data = scaled_data)
AICc(glm.nb(null_mod)) #1029.9; null model is significantly worse!







#8. Diagnostics for the best model so far (bm1) ================================
#I think it will be easiest to report diagnostics for just one of the models (the
#best one) but gives predictions from model averages of the top two
qqnorm(residuals(bm1)) #can you use qqnorm to check glmer.nb? #2/20/21, I 
#believe yes because you want the residuals to be normally distributed. This can
#also be checked with a simple histogram of the residuals:
hist(residuals(bm1)) #skewed slightly to the right
mean(residuals(bm1)) #-0.31
sd(residuals(bm1)) #0.95, close to normal
#guide to interpreting qqplot: https://stats.stackexchange.com/questions/348438/qq-plot-and-x-y-line


#ALSO PLOT RESIDUALS AGAINST EACH EXPLANATORY VARIABLE (PG 129 ZUUR)
plot(scaled_data$Cons_Abundance ~ residuals(bm1)) #shotgun blast
plot(scaled_data$Pink_Abundance ~ residuals(bm1)) #shotgun blast
plot(scaled_data$WMA_Releases_by_Yr ~ residuals(bm1)) #shotgun blast
plot(scaled_data$mean_flow ~ residuals(bm1)) #shotgun blast
plot(scaled_data$CV_flow ~ residuals(bm1)) #shotgun blast

#random effect normally distributed:
library(lattice)
library(GLMMadaptive)
qqmath(lme4::ranef(bm1)) #approx. normal



#Check for temporal autocorrelation:
E <- residuals(bm1, type = "deviance")
#Note: I was really confused about which type of residual to use here. I followed
#the example given on pages 146-147 in Zuur, which said to use type = "normalized"
#in the residuals argument, but that returned an error for me and said I could 
#only use “deviance”, “pearson”, “working”, “response”, or "partial" residuals. 
#This website was somewhat helpful:
#https://www.datascienceblog.net/post/machine-learning/interpreting_generalized_linear_models/
#but I still wasn't sure which type of residuals to use. I've gone with "deviance"
#for now because that seemed like the right way to go from the website
I1 <- !is.na(scaled_data$Avg_number_strays)
Efull <- vector(length = length(scaled_data$Avg_number_strays))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
    main = "Auto-correlation plot for residuals: bm1") #no temporal autocorr.




#residuals vs fitted values plot
plot(bm1, main = "Top Model") #is there a cone shape in this plot?

#plot observed vs predicted values
#remember to use the averaged predictions, not just predictions from bm1
plot(scaled_data$Avg_number_strays ~ Stray_model_predictions$predicted_strays)
q <- lm(scaled_data$Avg_number_strays ~ Stray_model_predictions$predicted_strays)
summary(q) #ratio of observed to predicted: 0.53 (model under-predicts more than 
#it over-predicts on average)
abline(q, col = "red")
abline(0, 1)



#manually check some predictions
dat <- data.frame(Year = scaled_data$Year, Stream = scaled_data$StreamName,
                  Observed = scaled_data$Avg_number_strays,
           Predicted = Stray_model_predictions$predicted_strays) 
options(scipen = 5) #removes scientific notation format
View(dat)
#Export this as a table to inlcude in supplemental material
setwd("~/Documents/CHUM_THESIS/Analysis/Manuscript/Figures")
write.csv(dat, "Table_S4.csv")


#to predict a single new value (yikes):
library(merTools)
scaled_data2[,"predValue"]<-NA
for (i in 1:length(scaled_data2[,1])){
  newdat.strayMod<-data.frame(Cons_Abundance=scaled_data2[i,"Cons_Abundance"],Pink_Abundance=scaled_data2[i,"Pink_Abundance"],
                              WMA_Releases_by_Yr= scaled_data2[i,"WMA_Releases_by_Yr"],
                              Dist_nearest_H=scaled_data2[i,"Dist_nearest_H"],
                              CV_flow=scaled_data2[i,"CV_flow"],Year=scaled_data2[i,"Year"],
                              Sum_avg_strays_by_subregion=scaled_data2[i,"Sum_avg_strays_by_subregion"])
  pStray<-predictInterval(merMod = stray_4, newdata = newdat.strayMod, 
                          level = 0.95, n.sims = 1000,
                          stat = "median", which = "all", 
                          include.resid.var = FALSE)
  scaled_data2[i,"predValue"]<-as.vector((pStray[,2][which(pStray[,1]=="combined")]))# c
} #okay nevermind, not working apparently. Just going to trust that the fitted 
#values line up correctly in dat above



#look at extreme residuals
dat$Residuals <- dat$Observed - dat$Predicted
#So there are a few extreme residuals, but on average the model correctly predicts
#attractive and unattractive streams, see here:
mean(dat$Observed) #8.86
mean(dat$Predicted[dat$Observed >= 8.86]) #will tell me avg predicted # of strays
#in which the observed # of strays was above the average:
#43.21
mean(dat$Predicted[dat$Observed < 8.86]) #will tell me avg predicted # of strays
#in which the observed # of strays was below the average:
#2.98


#How do the averages compare for each stream, as opposed to individual years 
#above in "dat"?
mean_strays <- dat %>% group_by(Stream) %>% summarise(Average_Obs = mean(Observed),
                                                      Average_Pred = mean(Predicted),
                                                      Max_Obs = max(Observed))
View(mean_strays)
#add total number of surveys column to include in table for manuscript
num_surv <- f %>% group_by(StreamName) %>%
  summarise(Total_surveys = sum(Number.of.surveys))
?left_join
colnames(num_surv)[1] <- "Stream"
mean_strays <- left_join(mean_strays, num_surv, by = "Stream")

setwd("~/Documents/CHUM_THESIS/Analysis") #this should already be set within your
#project hopefully
write.csv(mean_strays, "Avg_pred&obs.csv")

plot(Average_Obs ~ Average_Pred, data =  mean_strays)
p <- lm(Average_Obs ~ Average_Pred, data =  mean_strays)
summary(p) #slope = 0.78
abline(p, col = "red")
abline(0,1)






#8.5. Investigate possible causes of model mis-predictions =====================

#Model accurately predicts unattractive streams. That is, for the bottom 29 
#least attractive streams (all of which had <=4 avg strays observed), the model
#predicted <2.7 strays. Similarly, for the most attractive streams, which had
#14-75 observed avg strays, the model predicted 5-92 avg strays. <- These numbers
#which define "attractive" and "unattractive" are based on my own perception of
#how I could most accurately classify the majority of the streams, ensuring that
#streams with 0-2 strays are "unattractive" and the whopper streams with 15+
#average strays are "attractive"

#So to conclude, model predictions are therefore accurate in terms of the # of 
#strays predicted for unattractive streams and generally inaccurate for moderately
#or attractive streams, but it does adequately predict which streams are attrac-
#tive even if the # of strays predicted is incorrect


###Quantitatively identify model mis-predictions by splitting mod pred. # of strays
#into unattractive (LOW), neither un- nor attractive (MED), and attractive streams
#(HIGH) based on the above conclusions on predicted model results and then compare
#to observed # of strays
sum(mean_strays$Average_Pred <= 2.7) #29 pred. unattractive streams (<2.7 strays
#predicted, or...
29/57 #51% of the total streams

sum(mean_strays$Average_Pred > 5.1) #16 pred. attractive streams, which account for
16/57 #the top 28% of the streams
57 - (29 + 16) #leaving 12 streams that are possibly attractive in the middle, or
12/57 #21% of the dataset
#So, 51-69-100 should be the dataset separation, where 51% of the streams are un-
#attractive, 16% are in the middle, and 28% are attractive

?quantile
quant_pred <- quantile(mean_strays$Average_Pred, c(0, 0.51, 0.69, 1)) #confirms
#the above


###Assign model-predicted attractive/medium/unattractive classifications:
mean_strays$Pred_Rating <- NA
mean_strays$Pred_Rating[mean_strays$Average_Pred <= 2.7] <- "LOW"
mean_strays$Pred_Rating[mean_strays$Average_Pred > 2.7 &
                          mean_strays$Average_Pred <= 5.1] <- "MED"
mean_strays$Pred_Rating[mean_strays$Average_Pred > 5.1] <- "HIGH"


###Which streams were incorrectly predicted as attractive/medium/unattractive by
#the model?
#incorrectly pred.unattractive streams:
ifelse(mean_strays$Pred_Rating == "LOW" & mean_strays$Average_Obs > 4, #4 obs.
       #strays is the maximum within the 2.7 or less predicted strays threshold
       #for unattractiveness
       mean_strays$StreamName, NA) #Returns NA. All pred.unattractive streams are
#truly unattractive

#incorrect medium-attractive streams
ifelse(mean_strays$Pred_Rating == "MED" & mean_strays$Average_Obs <= 4 &
         mean_strays$Average_Obs < 8, mean_strays$StreamName, NA) #Swan Cove 
#Creek, Harding River, W Crawfish NE Arm Hd, Ford Arm Creek, Carroll Creek, Sag-
#inaw Bay S Head, and Staney Creek are all predicted to be "medium-attractive"
#but are really unattractive. Since the line between med-attractive and unattr-
#active is blurry, I won't sweat these mis-predictions

#incorrectly pred.attractive streams
ifelse(mean_strays$Pred_Rating == "HIGH" & mean_strays$Average_Obs < 8, #all of 
       #the extremely attractive streams have pred. # of strays of at least 5,
       #which coincides with #obs. # of strays of approx. 8 (based on the highest
       #value in the "MED" group, which is 7.7)
       mean_strays$StreamName, NA)
#Admiralty Creek, Camp Coogan, Chaik Bay Creek, Saginaw Creek, Whale Bay Great 
#Arm Head, Ralphs Creek, and Harris River are all predicted to be attractive,
#but really are med- or unattractive.
#Of those, Admiralty Creek and Ralphs Creek had at least 1 year in which they
#were attractive (=>8 obs. strays), it was just that their overall average was
#lower than the "attractive" cutoff. So, those 2 creeks may actually be attractive
#in reality

###So, why then are Camp Coogan, Chaik Bay Creek, Saginaw Creek, Whale Bay Great
#Arm Head, and Harris River predicted to be attractive when they really aren't?
#Let's try to find out:
mean_strays$Classification <- NA
mean_strays$Classification[mean_strays$Average_Obs >= 8] <- "Attractive"
mean_strays$Classification[mean_strays$Pred_Rating == "HIGH" &
                             mean_strays$Average_Obs < 8] <- "Pred.Attractive"
#^^for streams that were predicted to be attractive but aren't^^
mean_strays$Classification[mean_strays$Pred_Rating == "LOW"] <- "Unattractive"
mean_strays$Classification[is.na(mean_strays$Classification)] <- "Medium"

scaled_data_subset <- scaled_data[ , c(3,11,13,15,18,19)] #relevant data columns
#from final model (Cons_A, Pink_A, WMA_Releases_by_Yr, mean_flow, and CV_flow)
mean_dat <- scaled_data_subset %>% group_by(StreamName) %>% summarise_all(mean)
colnames(mean_strays)[1] <- "StreamName" #change name from "Stream" for joining
att_streams_dat <- left_join(mean_strays, mean_dat, by = "StreamName")
View(att_streams_dat)


###See if the pred.attractive streams have more in common with the attractive or
#unattractive streams:
qplot(StreamName, Cons_Abundance, colour = Classification, data = att_streams_dat)
#no obvious trend of attractive and pred.attractive streams having Cons_Abundance
#in common
qplot(StreamName, Pink_Abundance, colour = Classification, data = att_streams_dat)
#Unattractive streams have slightly lower pink abundance, pred.attractive streams
#more similar to attractive streams in having higher pink abundance
qplot(StreamName, WMA_Releases_by_Yr, colour = Classification, data = att_streams_dat)
#some pred.attractive streams have high WMA_Releases_by_Year like attractive
#streams do. No unattractive streams have high WMA_Releases_by_Yr
qplot(StreamName, mean_flow, colour = Classification, data = att_streams_dat) #no
#obvious trend
qplot(StreamName, CV_flow, colour = Classification, data = att_streams_dat) #no 
#obvious trend

#CONCLUSIONS: While I can't draw definitive conclusions from this analysis about
#why the model incorrectly predicted some streams to be attractive which actually
#weren't, I can consider some possibilities. The 2nd and 3rd qplots above reveal
#that the pred.attractive streams trend towards the higher Pink_Abundance and WMA_
#Releases_by_Yr characteristic of attractive streams, while they otherwise don't 
#stand out for the other 3 variables. This most likely was what resulted in their
#incorrect "attractive" rating as predicted by the model, which tells me that 
#there are probably other important factors needed to in order to assess stream
#attractiveness which I did not include.
#It is also possible that the pred.attractive streams maybe were (are) attractive
#in some year in which they were not sampled, since except for Chaik Bay Creek,
#they were all only sampled 1-2 times ever. This argument may not hold as much
#weight though given many other streams were sampled 1-2 times which the model
#did accurately predict. All in all, this model seems to have performed quite
#well considering the circumstances of data limitation











#9. Model cross-validation =====================================================
install.packages('caret') 
library(caret) #necessary functions not showing up. Stack exchange person sug-
#ests additional measures:
install.packages("lme4", dependencies = TRUE)
library(lme4)
methods(sigma)
install.packages("pbkrtest", dependencies = TRUE) 
library('caret') #okay cool that seems to have worked

data.ctrl <- trainControl(method = "cv", number = 5)
?train
model_caret <- train(Avg_number_strays ~ (1|Year) + Pink_Abundance + 
                       WMA_Releases_by_Yr + Dist_nearest_H + CV_flow,
                     data = scaled_data, trControl = data.ctrl,                       
                     method = "glm.nb", na.action = na.pass) #okay so I guess you
#can't really do this when you have a random effect in your model


#Manual cross validation instead!
library(dplyr)
train <- scaled_data %>%
  group_by(Year) %>% mutate(group = sample(n())/n() > 0.73) #giving this argument
#a value of 0.7 (to split the data set into 70% and 30% chunks) doesn't quite 
#give the 70-30 split (130:56 rows) of my data set, so I bumped it up to 0.73,
#which gives 131:55 rows for the training:test data set ratio
splitdf <- split(train, train$group)
training <- splitdf[["FALSE"]]
View(training)
test <- splitdf[["TRUE"]]
View(test) #okay great, this works

model_train <- glmer.nb(Avg_number_strays ~ (1 | Year) + Cons_Abundance +
                          Pink_Abundance + WMA_Releases_by_Yr + mean_flow +
                          CV_flow + I(CV_flow^2), data = training)
predictions <- model_train %>% predict(test) #remember to exp() bc the glmer.nb
#output is in log space. Using fitted() doesn't work in this specific instance
exp.pred <- exp(predictions)
RMSE(exp.pred, test$Avg_number_strays, na.rm = TRUE)
#RMSE is the square root of the average of the squared differences between pred
#and obs



#Try it in a for loop to get higher # of iterations
#First, output the RMSE:
rmse_output100 <- vector(length = 100)
for (i in 1:100) {
  train <- scaled_data %>%
    group_by(Year) %>% mutate(group = sample(n())/n() > 0.73)
  
  splitdf <- split(train, train$group)
  training <- splitdf[["FALSE"]]
  test <- splitdf[["TRUE"]]
  
  model_train <- glmer.nb(Avg_number_strays ~ (1 | Year) + Cons_Abundance +
                            Pink_Abundance + WMA_Releases_by_Yr + mean_flow +
                            CV_flow + I(CV_flow^2), data = training,
                          glmerControl(optCtrl = list(maxfun = 1e07)))
  predictions <- model_train %>% predict(test)
  #remember to exp() bc the glmer.nb output is in log space
  exp.pred <- exp(predictions)
  rmse <- RMSE(exp.pred, test$Avg_number_strays, na.rm = TRUE)
  print(rmse)
  
  rmse_output100[i] <- rmse
}
View(rmse_output100)
mean(rmse_output100) #22.18

#Question of whether I should use RMSE (root mean squared error) as the cross-V
#performance metric or MAE (mean absolute error). The key difference is that RMSE
#squares the errors before averaging and then taking the sqrt, so by squaring the
#errors it gives more weight to large errors

#MAE in contrast is less sensitive to large outliers. My large outliers are the
#really attractive streams which the model under-predicted or alternatively over-
#predicted in years in which those attractive streams weren't as attractive (ex-
#cept for Glen Creek, which is consistently predicted to be attractive when it 
#really isn't). Yet the model still predicted those as attractive as it should 
#have. So, I am okay with those errors and therefore would be more inclined to 
#use MAE as my performance metric since it doesn't problematize large outliers
#as much

#Sources:
#1) https://medium.com/human-in-a-machine-world/mae-and-rmse-which-metric-is-better-e60ac3bde13d
#2) http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/




#repeat for MAE:
mae_output100 <- vector(length = 100)
for (i in 1:100) {
  train <- scaled_data %>%
    group_by(Year) %>% mutate(group = sample(n())/n() > 0.73)
  
  splitdf <- split(train, train$group)
  training <- splitdf[["FALSE"]]
  test <- splitdf[["TRUE"]]
  
  model_train <- glmer.nb(Avg_number_strays ~ (1 | Year) + Cons_Abundance +
                            Pink_Abundance + WMA_Releases_by_Yr + mean_flow +
                            CV_flow + I(CV_flow^2), data = training,
                          glmerControl(optCtrl = list(maxfun = 1e08)))
  predictions <- model_train %>% predict(test)
  #remember to exp() bc the glmer.nb output is in log space
  exp.pred <- exp(predictions)
  mae <- MAE(exp.pred, test$Avg_number_strays, na.rm = TRUE)
  print(mae)
  
  mae_output100[i] <- mae
}
View(mae_output100)
mean(mae_output100) #7.96 = mean absolute error





#10. Interpret glmer.nb model results ==========================================
library(sjPlot)
coef_table <-sjPlot::plot_model(bm1, type = "est", transform = NULL, #transform
                                #= NULL leaves coef estimates unexponentiated fyi
                   pred.type = "fe",
                   mdrt.values = "all", order.terms = c(3, 1, 2, 4, 5, 6),
                   title = "", axis.title = c("Covariate Effect", ""),
                   #axis.labels =  c("CV of River Discharge^2",
                                    #"CV of River Discharge",
                                    #"Mean River Discharge",
                                    #"Pink Salmon Abundance",
                                    #"Conspecific Abundance",
                                    #"Number of Fish Released within 40km"),
                   axis.lim = c(-1,1), se = T, value.size = 4, show.values = T,
                   #se = T means you are now showing the standard errors around
                   #your coef estimates, not the confidence intervals
                   vline.color = "grey", value.offset = 0.3) + theme_bw() +
  theme(axis.text=element_text(size=12, face = "bold")) +
  theme(axis.title=element_text(size=13))
#gives graphical representation of effect sizes for all covariates similar to
#figure 2 in Westley et al. 2015
coef_table
#Update 7/12/21: This figure is now going to be an alternative to the response ~
#covariate graphs which actually show the relationships in question. The code for 
#those is below

#Alternative method using the broom package
broom::tidy(bm1) #gives you a nice tibble with just the coefficient estimates. 
#You can input this into a ggplot and then do all of the editing with ggplot
#controls that you are familiar with
ggplot(data = tidy(bm1)) +
  geom_point(aes(x = term,
                 y = estimate)) +
  geom_errorbar(aes(x = term,
                    ymin = estimate - std.error,
                    ymax = estimate + std.error))

#Export as high-res figure
setwd("~/Documents/CHUM THESIS/Manuscript/Figures")
tiff("fig4_alt.tiff", width = 7, height = 6, pointsize = 12, units = 'in', res = 300)
coef_table #graph that you want to export
dev.off( ) #now the displayed graphs are saved to a file with the above file name



tab_model(bm1, transform = NULL, show.est = TRUE)
#gives table of exp(coef), their CIs, and p-values, as well as info on the random
#effect in HTML format. Similar to the plot_model function above, transform = NULL
#will leave the model coef un-exponentiated 
#More info here if you want to use this table:
#http://rstudio-pubs-static.s3.amazonaws.com/402202_895e36c1b1ef49868e570aa58cb1ef9b.html



#####  SIDEBAR on understanding the random effect/intercepts  ##################
#Random effects are normally distributed. The intercept given in glmer output
#(using summary() command) is the  intercept when the mean random deviation is 
#equal to zero (bc RE is normally distributed). This is illustrated below:

#run the following command
coef(stray_2)$Year #this gives you the model coef for each year, and more useful,
#the random intercept for each year. (I think) the average of all the random in-
#tercepts given approximately = the intercept of the model (that which is given
#in summary(stray_4)). See how:
colMeans(coef(stray_4)$Year) #for (Intercept), the average is 1.265, which is v
#close to the intercept given in summary(stray_4), so I think they are the same
#thing
summary(stray_4) #intercept = 1.258
#So, in conclusion, THE INTERCEPT GIVEN IN THE GLMER SUMMARY IS THE AVERAGE OF 
#THE RANDOM INTERCEPTS FOR EACH YEAR. This is also suggested in Zuur et al. 2009
#page 108

#This really confused me for a while because if you run the ranef() function, from
#either lme4 or GLMMadaptive (doesn't matter they return the same thing), it gives
#a column of intercept values that do NOT match those given in coef(stray_4)$Year 
#for the random intercepts for each year. I learned (thank you to this site:
#https://biologyforfun.wordpress.com/2017/04/03/interpreting-random-effects-in-linear-mixed-effect-models/)
#that ranef() gives the estimated deviations for the random intercepts. That is 
#to say, you would add/subtract the deviation from the overall intercept to get
#the intercept for each year.
#For example, the overall model summary (summary(stray_4)) intercept is 1.26. If
#we run ranef(stray_4)
ranef(stray_4)
#then we get -1.101 for 2008, 0.197 for 2009, 0.735 for 2010, and so on. If you
#want the random intercept for 2008, subtract 1.101 from the overall intercept 
#(which is 1.26 remember, from summary(stray_4) -> the intercept term)
#1.26-1.101 = 0.159, which will also equal that given for the year 2008 in the 
#command from above:
coef(stray_4)$Year
#As another example, to get the random intercept for 2009 from ranef(), ADD (bc
#the sign is positive)  0.197 to the overall average intercept: 1.26+0.197=1.46

#Hope that helps! I would just recommend running the command coef(stray_4)$Year
#to get the random intercepts for each year and just ignore ranef() altogether

#Also potentially helpful:
#https://stats.stackexchange.com/questions/132971/in-what-sense-is-the-interpretation-of-coefficients-in-a-glmm-subject-specific
################################################################################



#Model effect plots for (approx) un-scaled data --------------------------------
#These are plotting the transformed data (for the covariates that were trans-
#formed). I'm still not 100% on the consensus of this but it's what this one SE
#thread recommended:
#https://stats.stackexchange.com/questions/115804/plotting-raw-data-but-running-statistics-on-log-transformed-data/169354
#I looked at some papers in my zotero library and it seems to be standard to plot
#the ln(covariate) as opposed to the raw covariate, for example Bailey and Moore
#2020. CONCLUSION: Plot un-scaled but still transformed data


#WMA_Releases_by_Yr
effects_WMA <- effects::effect(term = "WMA_Releases_by_Yr", mod = bm1, xlevels = 10)
#xlevels = 10 is used to specify how many effects you want included (the default
#is 5). By effects I mean what is the model response based on the given value of
#(scaled) WMA_Releases_by_Yr. You can also use xlevels to set specific values of
#WMA_Releases_by_Yr, like so: xlevels = list(x1=c(2, 4.5, 7)). See the document-
#ation to learn more
summary(effects_WMA)
x_WMA <- as.data.frame(effects_WMA)

#there is probably a better way to do this, but here is my method for now:
#un-scale the data
mean(trans_dat$WMA_Releases_by_Yr, na.rm = T) #1.22
sd(trans_dat$WMA_Releases_by_Yr, na.rm = T) #1.81
x_WMA$WMA_Releases_by_Yr <- (x_WMA$WMA_Releases_by_Yr * 1.81) + 1.22

WMA_plot <- ggplot() +
  geom_line(data = x_WMA, aes(x = WMA_Releases_by_Yr, y=fit), color="blue") +
  geom_ribbon(data= x_WMA,
              aes(x = WMA_Releases_by_Yr, ymin = lower, ymax = upper),
              alpha= 0.3, fill="grey70") +
  xlab("ln(Number of Fish Released within 40KM + 1)") +
  ylab("ln(Average # Number of Strays)") + theme_classic() + ylim(0, 20)
WMAp <- WMA_plot + #theme(axis.title = element_text(size=10)) +
  theme(axis.title.y = element_blank()) + #make the y-axis blank bc you are going
  #to make a shared y-axis label for your grid.arranged figure containing the 
  #plots for all 5 covariates
  theme(axis.text = element_text(size = 9)) #+ geom_point(data = trans_dat,
                                                          #aes(x = WMA_Releases_by_Yr,
                                                              #y = log(Avg_number_strays)))

WMAp

#Cons_Abundance
effects_Cons <- effects::effect(term = "Cons_Abundance", mod = bm1, xlevels = 10)
summary(effects_Cons)
x_Cons <- as.data.frame(effects_Cons)

#there is probably a better way to do this, but here is my method for now:
#un-scale the data
mean(trans_dat$Cons_Abundance, na.rm = T) #7.27
sd(trans_dat$Cons_Abundance, na.rm = T) #1.25
x_Cons$Cons_Abundance <- (x_Cons$Cons_Abundance * 1.25) + 7.27

Cons_plot <- ggplot() +
  geom_line(data = x_Cons, aes(x = Cons_Abundance, y=fit), color="red") +
  geom_ribbon(data= x_Cons,
              aes(x = Cons_Abundance, ymin = lower, ymax = upper),
              alpha= 0.3, fill="grey70") +
  xlab("ln(Chum Salmon Abundance)") +
  ylab("ln(Average # Number of Strays)") + theme_classic() + ylim(0, 20)
CAp <- Cons_plot + #theme(axis.title = element_text(size=10)) +
  theme(axis.title.y = element_blank()) + #make the y-axis blank bc you are going
  #to make a shared y-axis label for your grid.arranged figure containing the 
  #plots for all 5 covariates
  theme(axis.text = element_text(size = 9)) 
CAp


#Pink_Abundance
effects_Pink <- effects::effect(term = "Pink_Abundance", mod = bm1, xlevels = 10)
summary(effects_Pink)
x_Pink <- as.data.frame(effects_Pink)

#there is probably a better way to do this, but here is my method for now:
#un-scale the data
mean(trans_dat$Pink_Abundance, na.rm = T) #9.82
sd(trans_dat$Pink_Abundance, na.rm = T) #1.80
x_Pink$Pink_Abundance <- (x_Pink$Pink_Abundance * 1.80) + 9.82 

Pink_plot <- ggplot() +
  geom_line(data = x_Pink, aes(x = Pink_Abundance, y=fit), color="blue") +
  geom_ribbon(data= x_Pink,
              aes(x = Pink_Abundance, ymin = lower, ymax = upper),
              alpha= 0.3, fill="grey70") +
  xlab("ln(Pink Salmon Abundance + 1)") +
  ylab("ln(Average # Number of Strays)") + theme_classic() + ylim(0, 20)
PAp <- Pink_plot + #theme(axis.title = element_text(size=10)) +
  theme(axis.title.y = element_blank()) + #make the y-axis blank bc you are going
  #to make a shared y-axis label for your grid.arranged figure containing the 
  #plots for all 5 covariates
  theme(axis.text = element_text(size = 9))
PAp


#mean_flow
effects_mean_flow <- effects::effect(term = "mean_flow", mod = bm1, xlevels = 10)
summary(effects_mean_flow)
x_mean_flow <- as.data.frame(effects_mean_flow)

#there is probably a better way to do this, but here is my method for now:
#un-scale the data
mean(trans_dat$mean_flow, na.rm = T) #1.77
sd(trans_dat$mean_flow, na.rm = T) #1.04
x_mean_flow$mean_flow <- (x_mean_flow$mean_flow * 1.04) + 1.77
x_mean_flow <- x_mean_flow[c(3:10), ] #remove first two rows because they contain
#negative values

meanflow_plot <- ggplot() +
  geom_line(data = x_mean_flow, aes(x = mean_flow, y=fit), color="red") +
  geom_ribbon(data= x_mean_flow,
              aes(x = mean_flow, ymin = lower, ymax = upper),
              alpha= 0.3, fill="grey70") + 
  xlab("ln(Mean River Discharge)") +
  ylab("ln(Average Predicted # of Strays)") + theme_classic() + ylim(0, 20)
MFp <- meanflow_plot + #theme(axis.title = element_text(size=10)) +
  theme(axis.title.y = element_blank()) + #make the y-axis blank bc you are going
  #to make a shared y-axis label for your grid.arranged figure containing the 
  #plots for all 5 covariates
  theme(axis.text = element_text(size = 9))
MFp


#CV_flow
effects_CV_flow <- effects::effect(term = "CV_flow", mod = bm1, xlevels = 10)
summary(effects_CV_flow)
x_CV_flow <- as.data.frame(effects_CV_flow)

#there is probably a better way to do this, but here is my method for now:
#un-scale the data
mean(trans_dat$CV_flow, na.rm = T) #0.53
sd(trans_dat$CV_flow, na.rm = T) #0.057
x_CV_flow$CV_flow <- (x_CV_flow$CV_flow * 0.057) + 0.53

CVflow_plot <- ggplot() +
  geom_line(data = x_CV_flow, aes(x = CV_flow, y=fit), color="blue") +
  geom_ribbon(data= x_CV_flow,
              aes(x = CV_flow, ymin = lower, ymax = upper),
              alpha= 0.3, fill="grey70") +
  xlab("CV of River Discharge") +
  ylab("ln(Average Predicted # of Strays)") + theme_classic() +
  xlim(0.42, 0.61) + ylim(0, 20)
CVp <- CVflow_plot + #theme(axis.title = element_text(size=10)) +
  theme(axis.title.y = element_blank()) + #make the y-axis blank bc you are going
  #to make a shared y-axis label for your grid.arranged figure containing the 
  #plots for all 5 covariates
  theme(axis.text = element_text(size = 9))
CVp

#thank you to Laura Mudge github page for the guidance on this part^^ and on the
#very first plot that compares effect sizes. See the page here:
#https://lmudge13.github.io/sample_code/mixed_effects.html

#arrange the above plots all into one figure for your paper:
library(gridExtra)
plotlist <- list(WMAp, CAp, PAp, MFp, CVp)
cov_plots <- grid.arrange(grobs = plotlist, ncol = 2,
                          left = textGrob("ln(Average Predicted # of Strays)",
                                          rot = 90, vjust = 1,
                                          gp = gpar(cex = 1.15)))
ggsave(filename = "fig4.tiff", plot = cov_plots)#Because the export as .tiff steps
#below don't work for this one for some reason 
#Export as high-res figure
setwd("~/Documents/CHUM THESIS/Manuscript/Figures")
tiff("fig4.tiff", width = 7, height = 6, pointsize = 12, units = 'in', res = 300)
cov_plots #graph that you want to export
dev.off( ) #now the displayed graphs are saved to a file with the above file name



#Model effect plots (for scaled data) ------------------------------------------
#To plot them all at once (not recommended):
plot(allEffects(stray_4))

?effect_plot
#a nice ggplot2 + jtools version:
coef(stray_4)$Year #to see random intercepts for each year
effect_plot(stray_4, Cons_Abundance, interval = T) +
  geom_abline(intercept = 0.33, slope = -0.12) #this second argument was intended
#to try and plot a parallel line showing one of the other random intercepts. It
#failed. I will plot the rest of the effects plots without for now
#Note: this function gives a little warning about how the confint bands are an
#experimental feature for mixed effects model which only reflect the variance
#about the fixed effect.

effect_plot(stray_4, Dist_nearest_H, interval = T, #outcome.scale = "link",
            #un-comment out outcome.scale = "link" if you want to see the 
            #effect linearly, that is, the y-axis will be the log-response
            #that the model output gives, not the exp() values
            colors = "red") +
  xlab("Distance to the Nearest Hatchery") +
  ylab("Average Predicted Number of Hatchery Strays") + theme_classic() +
  theme(axis.text = element_text(size = 10.5), axis.title = element_text(size = 12))

effect_plot(stray_4, WMA_Releases_by_Yr, interval = T,
            colors = "blue") +
  xlab("Number of Fish Released within 40km") +
  ylab("Average Predicted Number of Hatchery Strays") + theme_classic() +
  theme(axis.text = element_text(size = 10.5), axis.title = element_text(size = 12))

effect_plot(stray_4, Cons_Abundance, interval = T,
            colors = "red") +
  xlab("Chum Salmon Abundance") +
  ylab("Average Predicted Number of Hatchery Strays") + theme_classic() +
  theme(axis.text = element_text(size = 10.5), axis.title = element_text(size = 12))

effect_plot(stray_4, Pink_Abundance, interval = T,
            colors = "blue") +
  xlab("Pink Salmon Abundance") +
  ylab("Average Predicted Number of Hatchery Strays") + theme_classic() +
  theme(axis.text = element_text(size = 10.5), axis.title = element_text(size = 12))

effect_plot(stray_5, CV_flow, interval = T,
            colors = "blue") +
  xlab("CV of River Discharge") +
  ylab("Average Predicted Number of Hatchery Strays") + theme_classic() +
  theme(axis.text = element_text(size = 10.5), axis.title = element_text(size = 12))











#11. Fit with brms ==============================================================
install.packages('brms')
library(brms)
?brm
brm1 <- brm(Avg_number_strays ~ (1|Year) + Sum_avg_strays_by_subregion +
      Cons_Abundance + Pink_Abundance + WMA_Releases_by_Yr +
      Dist_nearest_H + CV_flow, data = scaled_data,
    family = "negbinomial") #working now for some reason on day 2. #coef estimates
#are very close to what stray_4 predicted!
predict(brm1)
plot(brm1)
plot(residuals(brm1) ~ predict(brm1)) #that doesn't look great


brm2 <- brm(Avg_number_strays ~ (1|Year) + Sum_avg_strays_by_subregion +
              Hydrology_Type + Fishery_harvest + Cons_Abundance +
              Pink_Abundance + WMA_Releases_by_Yr + Dist_nearest_H +
              mean_flow + CV_flow, data = scaled_data,
            family = "negbinomial") 
summary(brm2)
plot(brm2)
plot(residuals(brm2) ~ predict(brm2), main = "brm2: Full Model") #also does not
#look great


unloadNamespace("glmmTMB")
install.packages("glmmTMB", type = "source") #error
install.packages('TMB', type = 'source') #error
install.packages("Matrix") #error, f*ck 
devtools::install_github("glmmTMB/glmmTMB",sub="glmmTMB")
library(glmmTMB)