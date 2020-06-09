setwd("//ahmct-065/teams/PMRF/Amir")
library(data.table)
library(dplyr)
library(tidyr)
library(gdata)

CHP.df=fread(file="./bin/Final Datasets/CHP.csv", sep=",", header=TRUE)
CHP.df=CHP.df[, c("CASE_ID", "ACCIDENT_YEAR", "COLLISION_DATE", "COLLISION_TIME", "DAY_OF_WEEK", "POPULATION", "PRIMARY_RD",
                  "SECONDARY_RD", "DIRECTION", "WEATHER_1", "WEATHER_2", "STATE_HWY_IND", "CALTRANS_COUNTY", 
                  "CALTRANS_DISTRICT", "STATE_ROUTE", "ROUTE_SUFFIX", "POSTMILE_PREFIX", "POSTMILE", "LOCATION_TYPE",
                  "RAMP_INTERSECTION", "SIDE_OF_HWY", "COLLISION_SEVERITY", "NUMBER_KILLED", "NUMBER_INJURED", "PARTY_COUNT",
                  "PRIMARY_COLL_FACTOR", "PCF_VIOL_CATEGORY", "ROAD_SURFACE", "ROAD_COND_1", "ROAD_COND_2", "LIGHTING",
                  "CONTROL_DEVICE", "CHP_ROAD_TYPE", "LATITUDE", "LONGITUDE")]

CHP.df=CHP.df %>% distinct()
CHP.df[CHP.df==""]=NA
###################
CHP.df$SIDE_OF_HWY[is.na(CHP.df$SIDE_OF_HWY)]=CHP.df$DIRECTION[is.na(CHP.df$SIDE_OF_HWY)]
CHP.df=CHP.df[which(!is.na(CHP.df$SIDE_OF_HWY)),]

CHP.df_pm.to.odom=fread(file="./bin/CHP_All(pm)+odom_output.csv", sep=",", header=TRUE)
#CHP.df_pm.to.odom=fread(file="./bin/odom_output.csv", sep=",", header=TRUE)
colnames(CHP.df_pm.to.odom)
CHP.df_pm.to.odom=CHP.df_pm.to.odom %>% distinct()


CHP.df_geo.to.odom_pm=fread(file="./bin/CHP_All(coord.)+odom_pm_output.csv", sep=",", header=TRUE)
#CHP.df_geo.to.odom_pm=fread(file="./bin/geo_output.csv", sep=",", header=TRUE)
colnames(CHP.df_geo.to.odom_pm)
CHP.df_geo.to.odom_pm=CHP.df_geo.to.odom_pm %>% distinct()
########################################################################
CHP.df=cbind.data.frame(CHP.df, Odometer=NA)

CHP.df$Odometer[which(CHP.df$SIDE_OF_HWY=="L" & is.na(CHP.df$Odometer))]=
  CHP.df_pm.to.odom$Odometer_Left[match(CHP.df$CASE_ID[which(CHP.df$SIDE_OF_HWY=="L" & is.na(CHP.df$Odometer))], CHP.df_pm.to.odom$CASE_ID)]

CHP.df$Odometer[which(CHP.df$SIDE_OF_HWY=="R" & is.na(CHP.df$Odometer))]=
  CHP.df_pm.to.odom$Odometer_Right[match(CHP.df$CASE_ID[which(CHP.df$SIDE_OF_HWY=="R" & is.na(CHP.df$Odometer))], CHP.df_pm.to.odom$CASE_ID)]

CHP.df$Odometer[which(CHP.df$SIDE_OF_HWY=="L" & is.na(CHP.df$Odometer))]=
  CHP.df_geo.to.odom_pm$Odometer_Left[match(CHP.df$CASE_ID[which(CHP.df$SIDE_OF_HWY=="L" & is.na(CHP.df$Odometer))], CHP.df_geo.to.odom_pm$CASE_ID)]

CHP.df$Odometer[which(CHP.df$SIDE_OF_HWY=="R" & is.na(CHP.df$Odometer))]=
  CHP.df_geo.to.odom_pm$Odometer_Right[match(CHP.df$CASE_ID[which(CHP.df$SIDE_OF_HWY=="R" & is.na(CHP.df$Odometer))], CHP.df_geo.to.odom_pm$CASE_ID)]

########################################################################
CHP.df$Odometer=as.numeric(CHP.df$Odometer) #53946 rows with no odometer

CHP.df$Odometer[which(CHP.df$SIDE_OF_HWY=="L" & is.na(CHP.df$Odometer))]=
  CHP.df_pm.to.odom$Odometer_Right[match(CHP.df$CASE_ID[which(CHP.df$SIDE_OF_HWY=="L" & is.na(CHP.df$Odometer))], CHP.df_pm.to.odom$CASE_ID)]

CHP.df$Odometer[which(CHP.df$SIDE_OF_HWY=="R" & is.na(CHP.df$Odometer))]=
  CHP.df_pm.to.odom$Odometer_Left[match(CHP.df$CASE_ID[which(CHP.df$SIDE_OF_HWY=="R" & is.na(CHP.df$Odometer))], CHP.df_pm.to.odom$CASE_ID)]

CHP.df$Odometer[which(CHP.df$SIDE_OF_HWY=="L" & is.na(CHP.df$Odometer))]=
  CHP.df_geo.to.odom_pm$Odometer_Right[match(CHP.df$CASE_ID[which(CHP.df$SIDE_OF_HWY=="L" & is.na(CHP.df$Odometer))], CHP.df_geo.to.odom_pm$CASE_ID)]

CHP.df$Odometer[which(CHP.df$SIDE_OF_HWY=="R" & is.na(CHP.df$Odometer))]=
  CHP.df_geo.to.odom_pm$Odometer_Left[match(CHP.df$CASE_ID[which(CHP.df$SIDE_OF_HWY=="R" & is.na(CHP.df$Odometer))], CHP.df_geo.to.odom_pm$CASE_ID)]

CHP.df$Odometer=as.numeric(CHP.df$Odometer)
########################################################################
CHP.df$LATITUDE=as.numeric(CHP.df$LATITUDE)

CHP.df$LATITUDE[which(CHP.df$SIDE_OF_HWY=="R" & is.na(CHP.df$LATITUDE))]=
  CHP.df_pm.to.odom$Latitude_Right[match(CHP.df$CASE_ID[which(CHP.df$SIDE_OF_HWY=="R" & is.na(CHP.df$LATITUDE))], CHP.df_pm.to.odom$CASE_ID)]

CHP.df$LATITUDE[which(CHP.df$SIDE_OF_HWY=="L" & is.na(CHP.df$LATITUDE))]=
  CHP.df_pm.to.odom$Latitude_Left[match(CHP.df$CASE_ID[which(CHP.df$SIDE_OF_HWY=="L" & is.na(CHP.df$LATITUDE))], CHP.df_pm.to.odom$CASE_ID)]

########################################################################
CHP.df$LONGITUDE=as.numeric(CHP.df$LONGITUDE)

CHP.df$LONGITUDE[which(CHP.df$SIDE_OF_HWY=="R" & is.na(CHP.df$LONGITUDE))]=
  CHP.df_pm.to.odom$Longitude_Right[match(CHP.df$CASE_ID[which(CHP.df$SIDE_OF_HWY=="R" & is.na(CHP.df$LONGITUDE))], CHP.df_pm.to.odom$CASE_ID)]

CHP.df$LONGITUDE[which(CHP.df$SIDE_OF_HWY=="L" & is.na(CHP.df$LONGITUDE))]=
  CHP.df_pm.to.odom$Longitude_Left[match(CHP.df$CASE_ID[which(CHP.df$SIDE_OF_HWY=="L" & is.na(CHP.df$LONGITUDE))], CHP.df_pm.to.odom$CASE_ID)]

CHP.df$LONGITUDE=as.numeric(CHP.df$LONGITUDE)
CHP.df$LONGITUDE[which(CHP.df$LONGITUDE > 0)]=CHP.df$LONGITUDE[which(CHP.df$LONGITUDE > 0)]*(-1)
########################################################################
fwrite(CHP.df, file = "./bin/Final Datasets/CHP.csv", sep=",", append = FALSE)