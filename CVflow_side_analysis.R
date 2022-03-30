
#Contains all watershed data in addition to CV_flow
CSergeant_OG_dat <-
  read_csv("~/Documents/CHUM_THESIS/Data Sources/Stream_Hydro_Flow/CSergeant_OG_dat.csv")
colnames(CSergeant_OG_dat)[2] <- "Watershed_ID2"
CSergeant_OG_dat$Watershed_ID2 <- as.character(CSergeant_OG_dat$Watershed_ID2)

#Contains flow data only. Specifically (its purpose here) is that it contains 
#your chapter 1 study streams and their associated watershed ID so that you can
#link your study streams to the additional data in CSergeant_OG_dat
Flow_dat <- read_csv("~/Documents/CHUM_THESIS/Data Sources/Stream_Hydro_Flow/Flow_dat.csv")


### Some streams have 2 watersheds assigned to them, so the CSergeant_OG_dat data
#will not join to them. Add a new column and assign a single watershed to each 
#stream (the same Wshed for streams that only had 1 Wshed, and the Wshed with
#the higher CV_flow when there are 2 Wsheds)
Flow_dat$Watershed_ID2 <- Flow_dat$Watershed_ID
Flow_dat <- Flow_dat[,c(1,2,10,3:9)]

#Update values in Watershed_ID2 column
Flow_dat[5,3] <- "8607"
Flow_dat[12,3] <- "9513"
Flow_dat[13,3] <- "9965"
Flow_dat[14,3] <- "12335"
Flow_dat[16,3] <- "9644"
Flow_dat[17,3] <- "10682"
Flow_dat[18,3] <- "11011"
Flow_dat[20,3] <- "11382"
Flow_dat[21,3] <- "8069"
Flow_dat[37,3] <- "6873"
Flow_dat[42,3] <- "8058"
Flow_dat[44,3] <- "8634"
Flow_dat[54,3] <- "12790"
Flow_dat$Watershed_ID2 <- as.character(Flow_dat$Watershed_ID2)


#join the dataframes
flow_plus <- left_join(Flow_dat, CSergeant_OG_dat, by = "Watershed_ID2") #this 
#contains all 64 streams. Remove the ones that you didn't end up modeling

flow_plus <- flow_plus[!(flow_plus$StreamName %in% delete),]
flow_plus <- flow_plus %>% filter(!StreamName == "Fish Creek-Portland Canal")
flow_plus <- flow_plus %>% filter(!StreamName == "Ketchikan Creek")
flow_plus <- flow_plus %>% filter(!StreamName == "Hidden Inlet")
flow_plus <- flow_plus %>% filter(!StreamName == "North Arm Creek")

#See if CV_flow might be correlated with other variables. Specifically, if low 
#and high CVs line up with specific values of other variables
plot(CV ~ area_sqkm, data = flow_plus) #no apparent trends for high and low CVs
plot(CV ~ max_m, data = flow_plus) #no
plot(CV ~ mean_m, data = flow_plus) #no, maybe for low CVs, but not for high
plot(CV ~ BareSoilRockImperv, data = flow_plus) #no
plot(CV ~ Forest, data = flow_plus) #no
plot(CV ~ GrassShrub, data = flow_plus) #no
plot(CV ~ OpenFrozenRiparianWetland, data = flow_plus) #no
plot(CV ~ PermSnowIce, data = flow_plus) #no
plot(CV ~ class_1, data = flow_plus) #higher CVs most clearly associated with
#snow-1, snow-2, and glacier-6 classes (see page 6 of Sergeant et al. 2020 for
#the classes identified by class_1 column)



save.image("CVflow_side.Rdata")
load("CVflow_side.Rdata")

