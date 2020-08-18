setwd("//ahmct-065/teams/PMRF/Amir")

library(data.table)
library(dplyr)
library(tidyr)
library(anytime)
library(lubridate)

county_abbr=fread(file = "COunty_Abbr_PeMS.code.csv", sep=",", header = TRUE)
LEMO.df=fread(file="./bin/Final Datasets/LEMO_WorkOrder+odom.csv", sep=",", header=TRUE)

LEMO.df[LEMO.df==""]=NA
#add work_length to LEMO.df
LEMO.df=cbind(LEMO.df, work_length=rowMeans(cbind(abs(LEMO.df$to.odom.R-LEMO.df$from.odom.R), 
                                                  abs(LEMO.df$to.odom.L-LEMO.df$from.odom.L)), 
                                            na.rm = TRUE))
#drop columns
LEMO.df=LEMO.df[,c("Work Order No", "Workdate", "Activity", "Activity Description", "Dist", 
                   "Hours.sum", "beginCounty", "endCounty", "rID", "rSuffix", "work_length")]

#reduce begin and end county columns to one column
LEMO.df$beginCounty=county_abbr$ABBREV.[match(LEMO.df$beginCounty, county_abbr$`PEMS code`)]
LEMO.df$endCounty=county_abbr$ABBREV.[match(LEMO.df$endCounty, county_abbr$`PEMS code`)]
LEMO.df$beginCounty=ifelse(LEMO.df$beginCounty==LEMO.df$endCounty,
                               LEMO.df$beginCounty,
                               paste(LEMO.df$beginCounty, LEMO.df$endCounty, sep = "-"))
LEMO.df=LEMO.df[,-c("endCounty")]

#reduce rID and rSuffix columns to one column
LEMO.df$rID=ifelse(is.na(LEMO.df$rSuffix), LEMO.df$rID, paste0(LEMO.df$rID, LEMO.df$rSuffix, sep = ""))
LEMO.df$rID=gsub("SB", "S", LEMO.df$rID)
LEMO.df$rID=gsub("C", "", LEMO.df$rID)
LEMO.df=LEMO.df[,-c("rSuffix")]

#add unique ID column
LEMO.df=cbind(LEMO_ID=seq.int(nrow(LEMO.df)), LEMO.df)

#rename columns
colnames(LEMO.df)=c("LEMO_ID", "wono", "work_date", "activity", "activity_descr", "district", 
                    "work_duration", "county", "route", "work_length")

#######################################################################################################
#######################################################################################################
################################### match with closure ################################################
closure_matches.df=fread(file = "./bin/Final Datasets/LEMO_ID.match.closure.csv", sep=",", header = TRUE)

#drop columns
closure_matches.df=closure_matches.df[,-c("wono", "activity", "work_date")]

LEMO_merged.df=merge(LEMO.df, closure_matches.df, by="LEMO_ID")
LEMO_merged.df[LEMO_merged.df==""]=NA

rm(LEMO.df, closure_matches.df)
#######################################################################################################
#######################################################################################################
################################### match with cleanRouteFile #########################################
clRtFile_matches.df=fread(file = "./bin/Final Datasets/LEMO_ID.match.CleanRouteFile.csv", sep =",", header = TRUE)
clRtFile.df=fread(file = "HighwayElementMarkers+odom_coord.csv", sep=",", header=TRUE)
clRtFile_matches.df=merge(clRtFile_matches.df, clRtFile.df, by="THY_ID", all.x = TRUE)
clRtFile_matches.df[clRtFile_matches.df==""]=NA
#drop columns
clRtFile_matches.df=clRtFile_matches.df[, -c("THY_ID", "THY_DISTRICT_CODE", "THY_COUNTY_CODE", 
                                             "THY_ROUTE_NAME", "THY_ROUTE_SUFFIX_CODE", "route", 
                                             "THY_PM_PREFIX_CODE", "THY_BEGIN_PM_AMT", "begin_pm", 
                                             "THY_END_PM_AMT", "end_pm", "THY_ELEMENT_ID", "THY_BEGIN_OFFSET_AMT", 
                                             "THY_END_OFFSET_AMT", "THY_BEGIN_DATE", "THY_END_DATE", 
                                             "THY_CREATE_DATE", "THY_CREATE_USER_NAME", "THY_SEG_ORDER_ID", 
                                             "THY_PM_SUFFIX_CODE", "THY_LENGTH_MILES_AMT", "THY_LEFT_ROAD_EFF_DATE", 
                                             "THY_LT_THROUGH_LANES_AMT", "THY_LT_SPEC_FEATURES_CODE", 
                                             "THY_LT_SPEC_FEATURES_DESC", "THY_LT_O_SHD_TOT_WIDTH_AMT", 
                                             "THY_LT_O_SHD_TRT_WIDTH_AMT", "THY_LT_I_SHD_TOT_WIDTH_AMT", 
                                             "THY_LT_I_SHD_TRT_WIDTH_AMT", "THY_LT_SIG_CHG_IND", "THY_MEDIAN_EFF_DATE",
                                             "THY_CURB_LANDSCAPE_CODE", "THY_CURB_LANDSCAPE_DESC", 
                                             "THY_MEDIAN_WIDTH_AMT", "THY_MEDIAN_WIDTH_VAR_CODE", 
                                             "THY_MEDIAN_SIG_CHG_IND", "THY_M_ROADWAY_USE_CODE", 
                                             "THY_M_ROADWAY_USE_DESC", "THY_RIGHT_ROAD_EFF_DATE", 
                                             "THY_RT_THROUGH_LANES_AMT", "THY_RT_SPEC_FEATURES_CODE",
                                             "THY_RT_SPEC_FEATURES_DESC", "THY_RT_I_SHD_TOT_WIDTH_AMT", 
                                             "THY_RT_I_SHD_TRT_WIDTH_AMT", "THY_RT_O_SHD_TOT_WIDTH_AMT",
                                             "THY_RT_O_SHD_TRT_WIDTH_AMT", "THY_RT_SIG_CHG_IND", "THY_CITY_CODE", 
                                             "THY_ACCESS_EFF_DATE", "THY_ACCESS_SIG_CHG_IND", 
                                             "THY_NON_ADD_CODE", "THY_PROFILE_CODE", "THY_CHANGE_PER_MILE_AMT", 
                                             "THY_LANDMARK_SHORT_DESC", "THY_LAST_SIG_CHG_DATE", "THY_RECORD_DATE", 
                                             "THY_UPDATE_DATE", "THY_UPDATE_USER_NAME", "THY_MAINT_SVC_LVL_CODE",
                                             "THY_EQUATE_CODE", "THY_BREAK_DESC", "THY_TOLL_FOREST_CODE", 
                                             "THY_TOLL_FOREST_DESC", "THY_NATIONAL_LANDS_CODE", 
                                             "THY_NATIONAL_LANDS_DESC", "THY_SCENIC_FREEWAY_CODE", 
                                             "THY_SCENIC_FREEWAY_DESC", "THY_EXTRACT_DATE", "begin_Odometer_Left", 
                                             "begin_Odometer_Right", "begin_Longitude_Left", "begin_Latitude_Left", 
                                             "begin_Longitude_Right", "begin_Latitude_Right", "end_Odometer_Left", 
                                             "end_Odometer_Right", "end_Longitude_Left", "end_Latitude_Left", 
                                             "end_Longitude_Right", "end_Latitude_Right")]

#reduce left and right columns to one columns
clRtFile_matches.df$THY_LT_SURF_TYPE_CODE=ifelse(clRtFile_matches.df$THY_LT_SURF_TYPE_CODE==
                                                   clRtFile_matches.df$THY_RT_SURF_TYPE_CODE,
                                                 clRtFile_matches.df$THY_LT_SURF_TYPE_CODE, NA)
clRtFile_matches.df$THY_LT_SURF_TYPE_DESC=ifelse(clRtFile_matches.df$THY_LT_SURF_TYPE_DESC==
                                                   clRtFile_matches.df$THY_RT_SURF_TYPE_DESC,
                                                 clRtFile_matches.df$THY_LT_SURF_TYPE_DESC, NA)
clRtFile_matches.df$THY_LT_LANES_AMT=ifelse(clRtFile_matches.df$THY_LT_LANES_AMT==
                                              clRtFile_matches.df$THY_RT_LANES_AMT,
                                            clRtFile_matches.df$THY_LT_LANES_AMT, NA)
clRtFile_matches.df$THY_LT_ROADWAY_USE_CODE=ifelse(clRtFile_matches.df$THY_LT_ROADWAY_USE_CODE==
                                                     clRtFile_matches.df$THY_RT_ROADWAY_USE_CODE,
                                                   clRtFile_matches.df$THY_LT_ROADWAY_USE_CODE, NA)
clRtFile_matches.df$THY_LT_ROADWAY_USE_DESC=ifelse(clRtFile_matches.df$THY_LT_ROADWAY_USE_DESC==
                                                     clRtFile_matches.df$THY_RT_ROADWAY_USE_DESC,
                                                   clRtFile_matches.df$THY_LT_ROADWAY_USE_DESC, NA)
clRtFile_matches.df$THY_LT_TRAV_WAY_WIDTH_AMT=ifelse(clRtFile_matches.df$THY_LT_TRAV_WAY_WIDTH_AMT==
                                                       clRtFile_matches.df$THY_RT_TRAV_WAY_WIDTH_AMT,
                                                     clRtFile_matches.df$THY_LT_TRAV_WAY_WIDTH_AMT, NA)

clRtFile_matches.df=clRtFile_matches.df[,-c("THY_RT_SURF_TYPE_CODE", "THY_RT_SURF_TYPE_DESC", 
                                            "THY_RT_LANES_AMT", "THY_RT_ROADWAY_USE_CODE", 
                                            "THY_RT_ROADWAY_USE_DESC", "THY_RT_TRAV_WAY_WIDTH_AMT")]

#rename columns
colnames(clRtFile_matches.df)=c("LEMO_ID", "surface_type", "surface_type_descr", "num_lanes", "road_use",
                                "road_use_descr", "road_width", "median_type", "median_type_descr", 
                                "barrier_type", "barrier_type_descr", "hwy_group", "hwy_group_descr", 
                                "access_type", "access_type_descr", "terrain_type", "terrain_type_descr", 
                                "road_speed", "road_adt", "population_code")

LEMO_merged.df=merge(LEMO.merge.Clousre, clRtFile_matches.df, by="LEMO_ID")
rm(clRtFile.df, clRtFile_matches.df)

#######################################################################################################
#######################################################################################################
######################################### match with AADT #############################################

AADT.matches=fread(file = "./bin/Final Datasets/LEMO_ID.match.AADT.csv", sep=",", header = TRUE)
AADT.matches[AADT.matches==""]=NA
AADT.matches=AADT.matches[, c("ID", "R.back_peak_hour", "R.back_aadt", "R.ahead_peak_hour", "R.ahead_aadt", 
                              "L.back_peak_hour", "L.back_aadt", "L.ahead_peak_hour", "L.ahead_aadt")]

#take average of each cell
AADT.matches$R.back_peak_hour=lapply(AADT.matches$R.back_peak_hour, function(x) 
  mean(scan(text=x, sep = ",", quiet = TRUE), na.rm = TRUE))

AADT.matches$R.back_aadt=lapply(AADT.matches$R.back_aadt, function(x) 
  mean(scan(text=x, sep = ",", quiet = TRUE), na.rm = TRUE))

AADT.matches$R.ahead_peak_hour=lapply(AADT.matches$R.ahead_peak_hour, function(x) 
  mean(scan(text=x, sep = ",", quiet = TRUE), na.rm = TRUE))

AADT.matches$R.ahead_aadt=lapply(AADT.matches$R.ahead_aadt, function(x) 
  mean(scan(text=x, sep = ",", quiet = TRUE), na.rm = TRUE))

AADT.matches$L.back_peak_hour=lapply(AADT.matches$L.back_peak_hour, function(x) 
  mean(scan(text=x, sep = ",", quiet = TRUE), na.rm = TRUE))

AADT.matches$L.back_aadt=lapply(AADT.matches$L.back_aadt, function(x) 
  mean(scan(text=x, sep = ",", quiet = TRUE), na.rm = TRUE))

AADT.matches$L.ahead_peak_hour=lapply(AADT.matches$L.ahead_peak_hour, function(x) 
  mean(scan(text=x, sep = ",", quiet = TRUE), na.rm = TRUE))

AADT.matches$L.ahead_aadt=lapply(AADT.matches$L.ahead_aadt, function(x) 
  mean(scan(text=x, sep = ",", quiet = TRUE), na.rm = TRUE))

#convert to numerics
AADT.matches$R.back_peak_hour=as.numeric(AADT.matches$R.back_peak_hour)
AADT.matches$R.back_aadt=as.numeric(AADT.matches$R.back_aadt)
AADT.matches$R.ahead_peak_hour=as.numeric(AADT.matches$R.ahead_peak_hour)
AADT.matches$R.ahead_aadt=as.numeric(AADT.matches$R.ahead_aadt)
AADT.matches$L.back_peak_hour=as.numeric(AADT.matches$L.back_peak_hour)
AADT.matches$L.back_aadt=as.numeric(AADT.matches$L.back_aadt)
AADT.matches$L.ahead_peak_hour=as.numeric(AADT.matches$L.ahead_peak_hour)
AADT.matches$L.ahead_aadt=as.numeric(AADT.matches$L.ahead_aadt)

#take rowWise averages
AADT.matches=cbind.data.frame(AADT.matches, peak_hour_aadt=rowMeans(AADT.matches[,c("R.back_peak_hour",
                                                                                    "R.ahead_peak_hour",
                                                                                    "L.back_peak_hour", 
                                                                                    "L.ahead_peak_hour")],
                                                                    na.rm = TRUE))
AADT.matches=cbind.data.frame(AADT.matches, aadt=rowMeans(AADT.matches[,c("R.back_aadt", 
                                                                          "R.ahead_aadt", 
                                                                          "L.back_aadt", 
                                                                          "L.ahead_aadt")], 
                                                          na.rm = TRUE))
#drop columns and rename
AADT.matches=AADT.matches[,c("ID", "peak_hour_aadt", "aadt")]
colnames(AADT.matches)=c("LEMO_ID", "peak_aadt", "aadt")

LEMO_merged.df=merge(LEMO_merged.df, AADT.matches, by="LEMO_ID")
rm(AADT.matches)

#######################################################################################################
#######################################################################################################
#################################### match with TRUCK aadt ############################################

TRUCK.matches=fread(file = "./bin/Final Datasets/LEMO_ID.match.TRUCK_AADT.csv", sep = ",", header=TRUE)
TRUCK.matches[TRUCK.matches==""]=NA

#take average of each cell
TRUCK.matches$R.ahead_truck_aadt=lapply(TRUCK.matches$R.ahead_truck_aadt, function(x) 
  mean(scan(text=x, sep = ",", quiet = TRUE), na.rm = TRUE))
TRUCK.matches$L.ahead_truck_aadt=lapply(TRUCK.matches$L.ahead_truck_aadt, function(x) 
  mean(scan(text=x, sep = ",", quiet = TRUE), na.rm = TRUE))

#convert to numerics
TRUCK.matches$R.ahead_truck_aadt=as.numeric(TRUCK.matches$R.ahead_truck_aadt)
TRUCK.matches$L.ahead_truck_aadt=as.numeric(TRUCK.matches$L.ahead_truck_aadt)

#take average rowWise
TRUCK.matches=cbind(TRUCK.matches, truck_aadt=rowMeans(TRUCK.matches[,2:3], na.rm = TRUE))

#drop columns
TRUCK.matches=TRUCK.matches[,-c("R.ahead_truck_aadt", "L.ahead_truck_aadt")]
colnames(TRUCK.matches)=c("LEMO_ID", "truck_aadt")

LEMO_merged.df=merge(LEMO_merged.df, TRUCK.matches, by="LEMO_ID")
rm(TRUCK.matches)

#######################################################################################################
#######################################################################################################
##################################### match with Densities ############################################

density.matches11_12=fread(file = "./bin/Final Datasets/LEMO_ID.match.density_2011-2012.csv", sep = ",",
                           header = TRUE)

density.matches11_12$density=as.numeric(density.matches11_12$density)
colnames(density.matches11_12)=c("LEMO_ID", "collision_density11_12")

LEMO_merged.df=merge(LEMO_merged.df, density.matches11_12, by="LEMO_ID")

density.matches11_18=fread(file = "./bin/Final Datasets/LEMO_ID.match.density_2011-2018.csv", sep = ",", 
                           header = TRUE)

density.matches11_18$density=as.numeric(density.matches11_18$density)
colnames(density.matches11_18)=c("LEMO_ID", "collision_density11_18")

LEMO_merged.df=merge(LEMO_merged.df, density.matches11_18, by="LEMO_ID")
rm(density.matches11_12, density.matches11_18)

#######################################################################################################
#######################################################################################################
################################### match with closure time ###########################################

LCS.df=fread(file = "./bin/Final Datasets/LCS.csv", sep = ",", header = TRUE)
LCS.df[LCS.df==""]=NA

true_start_time=LCS.df$StartTime
true_start_time[which(!is.na(LCS.df$Time1097))]=LCS.df$Time1097[which(!is.na(LCS.df$Time1097))]

true_end_time=LCS.df$EndTime
true_end_time[which(!is.na(LCS.df$Time1098))]=LCS.df$Time1098[which(!is.na(LCS.df$Time1098))]
true_end_time[which(!is.na(LCS.df$Time1022))]=LCS.df$Time1022[which(!is.na(LCS.df$Time1022))]


true_start_date=LCS.df$StartDate
true_start_date[which(!is.na(LCS.df$Date1097))]=LCS.df$Date1097[which(!is.na(LCS.df$Date1097))]

true_end_date=LCS.df$EndDate
true_end_date[which(!is.na(LCS.df$Date1098))]=LCS.df$Date1098[which(!is.na(LCS.df$Date1098))]
true_end_date[which(!is.na(LCS.df$Date1022))]=LCS.df$Date1022[which(!is.na(LCS.df$Date1022))]

LCS.df=cbind.data.frame(LCS.df, closure_start_date=true_start_date, closure_start_time=true_start_time, 
                        closure_end_date=true_end_date, closure_end_time=true_end_time)

LEMO_merged.df=merge(LEMO_merged.df[,-c("closure_start_time", "closure_end_time")], LCS.df[,c("DB ID", "closure_start_date", "closure_start_time", 
                                               "closure_end_date", "closure_end_time")], by.x="closure_id", 
                     by.y="DB ID", all.x = TRUE)
rm(LCS.df)
#######################################################################################################
#######################################################################################################
#################################### match with Collisions ############################################

collision_matches=fread(file = "./bin/Final Datasets/LEMO_ID.match.CHP.csv", sep=",", header = TRUE)
collision_matches$COLLISION_DATE=as.character(collision_matches$COLLISION_DATE)

CHP.df=fread(file = "./bin/Final Datasets/CHP.csv", sep=",", header = TRUE)
CHP.df$COLLISION_DATE=anydate(CHP.df$COLLISION_DATE)

collision_matches=merge(collision_matches, CHP.df, by="CASE_ID")
collision_matches=collision_matches[,-c("COLLISION_DATE.y", "STATE_ROUTE.y", "SIDE_OF_HWY.y", "Odometer.y")]
names(collision_matches)[3:6]=c("STATE_ROUTE", "COLLISION_DATE", "Odometer", "SIDE_OF_HWY")
names(collision_matches)[2]="LEMO_ID"

collision_matches=collision_matches[,c("LEMO_ID", "CASE_ID", "COLLISION_DATE", "COLLISION_TIME", "DAY_OF_WEEK", "POPULATION", 
                                       "WEATHER_1", "WEATHER_2", "LOCATION_TYPE", "RAMP_INTERSECTION", 
                                       "COLLISION_SEVERITY", "NUMBER_KILLED", "NUMBER_INJURED", "PARTY_COUNT", 
                                       "PRIMARY_COLL_FACTOR", "PCF_VIOL_CATEGORY", "ROAD_SURFACE", "ROAD_COND_1", 
                                       "ROAD_COND_2", "LIGHTING", "CONTROL_DEVICE", "CHP_ROAD_TYPE")]

colnames(collision_matches)=c("LEMO_ID", "collision_id", "collision_date", "collision_time", "collision_day", "collision_pop_cod", 
                              "collision_weather_cond_1", "collision_weather_cond_2", "collision_location_type", 
                              "collision_ramp_intersection", "collision_severity", "collision_num_killed", 
                              "collision_num_injured", "collision_party_count", "collision_prime_factor", 
                              "collision_violation_cat", "collision_surface_cond", "collision_road_cond_1", 
                              "collision_road_cond_2", "collision_lighting_cond", "collision_control_device",
                              "collision_road_type")
collision_matches$collision_time=paste(collision_matches$collision_time %/% 100, 
                                       collision_matches$collision_time %% 100, sep=":")


collision_matches_byRoadCond=collision_matches[which(collision_matches$collision_road_cond_1=="D" | 
                                                     collision_matches$collision_road_cond_2=="D")]

LEMO_merged_CHP.df=merge(LEMO_merged.df, collision_matches_byRoadCond, by="LEMO_ID", all.x = TRUE)
fwrite(LEMO_merged_CHP.df, file = "./bin/LEMO_CHP.by.roadCond.csv", sep=",", append = FALSE)

#LEMO_merged.df$closure_start_time=as.numeric(format(strptime(LEMO_merged.df$closure_start_time, "%H:%M:%S"), '%H'))
#LEMO_merged.df$closure_end_time=as.numeric(format(strptime(LEMO_merged.df$closure_end_time, "%H:%M:%S"), '%H'))

collision_matches_byRoadCond_byClosureTime=collision_matches[which((
                                                                    collision_matches$collision_road_cond_1=="D" | 
                                                                    collision_matches$collision_road_cond_2=="D"
                                                                    ) | 
                                                                   (collision_matches$LEMO_ID %in% LEMO_merged.df$LEMO_ID[which(!is.na(LEMO_merged.df$closure_id))]
                                                                   ))]

LEMO_merged_CHP.df=merge(LEMO_merged.df, collision_matches_byRoadCond_byClosureTime, by="LEMO_ID", all.x = TRUE)
`%notwithin%`=Negate(`%within%`)
LEMO_merged_CHP.df[which(!is.na(LEMO_merged_CHP.df$closure_id) & 
                         !is.na(LEMO_merged_CHP.df$collision_id) & 
                         ymd_hm(paste(LEMO_merged_CHP.df$collision_date, LEMO_merged_CHP.df$collision_time, sep=" ")) %notwithin%
                             interval(
                                      ymd_hms(paste(LEMO_merged_CHP.df$closure_start_date, LEMO_merged_CHP.df$closure_start_time, sep=" ")), 
                                      ymd_hms(paste(LEMO_merged_CHP.df$closure_end_date, LEMO_merged_CHP.df$closure_end_time, sep=" "))
                                     )), 
                   grep("collision", names(LEMO_merged_CHP.df))]=NA
LEMO_merged_CHP.df = LEMO_merged_CHP.df %>% distinct()
fwrite(LEMO_merged_CHP.df, file = "./bin/LEMO_CHP.by.roadCond_closureTime.csv", sep=",", append = FALSE)

collision_matches_by_workOrderDate=collision_matches[which((
                                                            collision_matches$collision_road_cond_1=="D" | 
                                                            collision_matches$collision_road_cond_2=="D") | 
                                                           (as.POSIXlt(collision_matches$collision_time, format="%H:%M") >= as.POSIXlt("08:00", format="%H:%M") & 
                                                            as.POSIXlt(collision_matches$collision_time, format="%H:%M") <= as.POSIXlt("18:00", format="%H:%M"))),]
LEMO_merged_CHP.df=merge(LEMO_merged.df, collision_matches_by_workOrderDate, by="LEMO_ID", all.x = TRUE)
LEMO_merged_CHP.df = LEMO_merged_CHP.df %>% distinct()
fwrite(LEMO_merged_CHP.df, file = "./bin/LEMO_CHP.by.workOrderDate.csv", sep=",", append = FALSE)