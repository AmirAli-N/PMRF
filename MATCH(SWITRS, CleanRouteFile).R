library(data.table)
library(tidyr)
library(dplyr)
#set working directory
setwd("C:/Users/anasr/Desktop")

hwMarker.df=fread(file="HighwayMarkers-SW_2018-03-19_output.csv", sep=",", header=TRUE)
CHP.df=fread(file="CHP_All.csv", sep=",", header=TRUE)

#this function orders the clean route file based on begin_odometer, and finds the chp case odometer in thoses intervals
find_odom_interval=function(alignment, odometer, Marker.df){
  ind=NA
  if (alignment=="L"){
    Marker.df$begin_Odometer_Left=as.numeric(Marker.df$begin_Odometer_Left)
    Marker.df=Marker.df[!is.na(Marker.df$begin_Odometer_Left),]
    Marker.df=setDT(Marker.df)[order(Marker.df$begin_Odometer_Left),,]
    ind=findInterval(odometer, Marker.df$begin_Odometer_Left, rightmost.closed = TRUE)
  }
  if (alignment=="R"){
    Marker.df$begin_Odometer_Right=as.numeric(Marker.df$begin_Odometer_Right)
    Marker.df=Marker.df[!is.na(Marker.df$begin_Odometer_Right),]
    Marker.df=setDT(Marker.df)[order(Marker.df$begin_Odometer_Right),,]
    ind=findInterval(odometer, Marker.df$begin_Odometer_Right, rightmost.closed = TRUE)
  }
  if (!is.na(ind) & ind!=0){
    return (as.data.frame(Marker.df[ind,]))
  } else if (!is.na(ind) & ind==0){
    return(as.data.frame(Marker.df[1,]))
  } else{
    df=data.frame(matrix(NA, nrow=1, ncol=ncol(Marker.df)))
    colnames(df)=colnames(Marker.df)
    return(df)
  }
}

res=data.frame(matrix(NA, nrow = 0, ncol = (length(CHP.df)+length(hwMarker.df))))
colnames(res)=c(colnames(CHP.df), colnames(hwMarker.df))

for (i in 1:length(CHP.df$CASE_ID)){
  filter_hw=hwMarker.df[which(
                              hwMarker.df$route==CHP.df$STATE_ROUTE[i] &
                              hwMarker.df$THY_COUNTY_CODE==CHP.df$CALTRANS_COUNTY[i]),]
  
  if (CHP.df$SIDE_OF_HWY[i]=="R" & !is.na(CHP.df$Odometer_Right[i])){
    res=rbind(res, 
              cbind.data.frame(CHP.df[i,],
                               find_odom_interval(alignment = CHP.df$SIDE_OF_HWY[i],
                                                  odometer = CHP.df$Odometer_Right[i],
                                                  Marker.df = filter_hw)))
  } else if (CHP.df$SIDE_OF_HWY[i]=="L" & !is.na(CHP.df$Odometer_Left[i])){
    res=rbind(res, 
              cbind.data.frame(CHP.df[i,],
                               find_odom_interval(alignment = CHP.df$SIDE_OF_HWY[i], 
                                                  odometer = CHP.df$Odometer_Left[i], 
                                                  Marker.df = filter_hw)))
  } else{
    df=data.frame(matrix(NA, nrow=1, ncol = (length(CHP.df)+length(hwMarker.df))))
    colnames(df)=c(colnames(CHP.df), colnames(hwMarker.df))
    res=rbind(res, df)
  }
  if (i%%50000==0){
    print(i)
    fwrite(res, file="Accident.csv", sep=",", append=FALSE)
  }
}
fwrite(res, file="Accident.csv", sep=",", append=FALSE)
df=res
rm(res)
######cleanUp the joined data base and add missing coordinates

df$DIRECTION[df$DIRECTION==""]=NA
df$WEATHER_1[df$WEATHER_1=="-"]=NA
df$WEATHER_2[df$WEATHER_2=="-"]=NA
#THY_COUNTY_CODE=CALTRRANS_COUNTY
df=df[, -c("THY_COUNTY_CODE")]
#NOTE some districts in THY_DISTRICT_CODE do not match CALTRANS_DISTRICT: 1481 entries
df$ROUTE_SUFFIX[which(df$ROUTE_SUFFIX=="-" | 
                      df$ROUTE_SUFFIX=="" | 
                      df$ROUTE_SUFFIX=="B")]=NA
df$POSTMILE_PREFIX[which(df$POSTMILE_PREFIX=="-" |
                         df$POSTMILE_PREFIX=="")]=NA
df$POSTMILE=gsub("([[:digit:]])([[:alpha:]])", "\\1 \\2", df$POSTMILE)
df=separate(df, col = "POSTMILE", into=c("POSTMILE", "POSTMILE_SUFFIX"), sep=" ", remove = TRUE, convert = TRUE, extra = "drop")
df$LOCATION_TYPE[df$LOCATION_TYPE==""]=NA
df$RAMP_INTERSECTION[which(df$RAMP_INTERSECTION=="" |
                            df$RAMP_INTERSECTION=="-")]=NA
df$PRIMARY_COLL_FACTOR[df$PRIMARY_COLL_FACTOR=="-"]=NA
df$PCF_VIOL_CATEGORY=as.numeric(df$PCF_VIOL_CATEGORY)
df$ROAD_SURFACE[df$ROAD_SURFACE=="-"]=NA
df$ROAD_COND_1[df$ROAD_COND_1=="-"]=NA
df$ROAD_COND_2[df$ROAD_COND_2=="-"]=NA
df$LIGHTING[df$LIGHTING=="-"]=NA
df$CONTROL_DEVICE[df$CONTROL_DEVICE=="-"]=NA
#save the odometer for the side of hwy
df$Odometer_Left=as.numeric(df$Odometer_Left)
df$Odometer_Right=as.numeric(df$Odometer_Right)

df$Odometer_Left[which(df$SIDE_OF_HWY=="R" & !is.na(df$Odometer_Right))]=df$Odometer_Right[which(df$SIDE_OF_HWY=="R" & !is.na(df$Odometer_Right))]
df$Odometer_Left[which(df$SIDE_OF_HWY=="L" & is.na(df$Odometer_Left))]=df$Odometer_Right[which(df$SIDE_OF_HWY=="L" & is.na(df$Odometer_Left))]
names(df)[names(df)=="Odometer_Left"]="Odometer"
df=df[, -c("Odometer_Right")]

#THY_ROUTE_NAME=STATE_ROUTE
df=df[, -c("THY_ID",
           "THY_ROUTE_NAME",
           "THY_ROUTE_SUFFIX_CODE",
           "route",
           "THY_PM_PREFIX_CODE",
           "THY_PM_SUFFIX_CODE",
           "THY_BEGIN_PM_AMT",
           "begin_pm",
           "THY_END_PM_AMT",
           "end_pm",
           "THY_ELEMENT_ID",
           "THY_BEGIN_OFFSET_AMT",
           "THY_END_OFFSET_AMT",
           "THY_BEGIN_DATE",
           "THY_END_DATE",
           "THY_CREATE_DATE",
           "THY_CREATE_USER_NAME",
           "THY_SEG_ORDER_ID",
           "THY_LENGTH_MILES_AMT")]
#save left or right information for the side of hwy
df=setDF(df)
df[which(df$SIDE_OF_HWY=="R"), grep("_LT_", names(df), ignore.case = FALSE)]=df[which(df$SIDE_OF_HWY=="R"), grep("_RT_", names(df), ignore.case = FALSE)]

right.col=grep("_RT_", names(df), ignore.case = FALSE)
df=as.data.frame(df)[,!names(df)%in% names(df)[right.col]]
colnames(df)=gsub("_LT_","_",colnames(df))
df=as.data.frame(df)[,!names(df)%in%c("THY_LEFT_ROAD_EFF_DATE",
          "THY_RIGHT_ROAD_EFF_DATE",
          "THY_ACCESS_EFF_DATE",
          "THY_RECORD_DATE",
          "THY_UPDATE_DATE",
          "THY_UPDATE_USER_NAME",
          "THY_MAINT_SVC_LVL_CODE",
          "THY_EQUATE_CODE",
          "THY_BREAK_DESC",
          "THY_EXTRACT_DATE",
          "begin_Odometer_Left",
          "begin_Odometer_Right",
          "begin_Longitude_Left",
          "begin_Latitude_Left",
          "begin_Longitude_Right",
          "begin_Latitude_Right",
          "end_Odometer_Left",
          "end_Odometer_Right",
          "end_Longitude_Left",
          "end_Latitude_Left",
          "end_Longitude_Right",
          "end_Latitude_Right")]

df=setDT(df)
df[df$THY_SURF_TYPE_CODE==""]=NA
df$THY_SURF_TYPE_DESC=gsub("  ", " - ", df$THY_SURF_TYPE_DESC)
df$THY_ROADWAY_USE_CODE[df$THY_ROADWAY_USE_CODE==""]=NA
df$THY_ROADWAY_USE_DESC[df$THY_ROADWAY_USE_DESC==""]=NA
df$THY_M_ROADWAY_USE_CODE[df$THY_M_ROADWAY_USE_CODE==""]=NA
df$THY_M_ROADWAY_USE_DESC[df$THY_M_ROADWAY_USE_DESC==""]=NA
df$THY_SPEC_FEATURES_DESC[df$THY_SPEC_FEATURES_DESC==""]=NA
df$THY_SIG_CHG_IND[which(df$THY_SIG_CHG_IND=="" |
                         df$THY_SIG_CHG_IND=="*")]=NA

df=df[,-c("THY_MEDIAN_EFF_DATE")]
df$THY_MEDIAN_TYPE_DESC[which(df$THY_MEDIAN_TYPE_DESC==""|
                              df$THY_MEDIAN_TYPE_DESC=="Unknown")]=NA
df$THY_MEDIAN_BARRIER_CODE[df$THY_MEDIAN_BARRIER_CODE==""]=NA
df$THY_MEDIAN_BARRIER_DESC[df$THY_MEDIAN_BARRIER_DESC==""]=NA
df$THY_MEDIAN_SIG_CHG_IND[which(df$THY_MEDIAN_SIG_CHG_IND=="" |
                            df$THY_MEDIAN_SIG_CHG_IND=="*")]=NA
df$THY_CITY_CODE[df$THY_CITY_CODE==""]=NA
df=df[,-c("THY_HIGHWAY_ACCESS_DESC")]
df$THY_ACCESS_SIG_CHG_IND[df$THY_ACCESS_SIG_CHG_IND==""]=NA
df$THY_ADT_AMT=as.numeric(gsub(",","",df$THY_ADT_AMT))
df$THY_CHANGE_PER_MILE_AMT=as.numeric(gsub(",","",df$THY_CHANGE_PER_MILE_AMT))
df=df[,-c("THY_LANDMARK_SHORT_DESC")]
df=df[,-c("THY_POPULATION_CODE")]
df=df[,-c("THY_LAST_SIG_CHG_DATE")]
df=df[,-c("THY_TOLL_FOREST_CODE", "THY_TOLL_FOREST_DESC")]
df=df[,-c("THY_NATIONAL_LANDS_CODE", "THY_NATIONAL_LANDS_DESC")]
df=df[,-c("THY_SCENIC_FREEWAY_CODE","THY_SCENIC_FREEWAY_DESC")]

#fix missing coordinates
coord.df=fread(file="CHP_All(pm)+odom_output.csv", sep=",", header=TRUE)

df$LATITUDE[which(is.na(df$LATITUDE) & df$SIDE_OF_HWY=="R")]=
  coord.df$Latitude_Right[match(df$CASE_ID[which(is.na(df$LATITUDE) & df$SIDE_OF_HWY=="R")], coord.df$CASE_ID)]
df$LONGITUDE[which(is.na(df$LONGITUDE) & df$SIDE_OF_HWY=="R")]=
  coord.df$Longitude_Right[match(df$CASE_ID[which(is.na(df$LONGITUDE) & df$SIDE_OF_HWY=="R")], coord.df$CASE_ID)]

df$LATITUDE[which(is.na(df$LATITUDE) & df$SIDE_OF_HWY=="L")]=
  coord.df$Latitude_Left[match(df$CASE_ID[which(is.na(df$LATITUDE) & df$SIDE_OF_HWY=="L")], coord.df$CASE_ID)]
df$LONGITUDE[which(is.na(df$LONGITUDE) & df$SIDE_OF_HWY=="L")]=
  coord.df$Longitude_Left[match(df$CASE_ID[which(is.na(df$LONGITUDE) & df$SIDE_OF_HWY=="L")], coord.df$CASE_ID)]

df$LONGITUDE=as.numeric(df$LONGITUDE)
df$LATITUDE=as.numeric(df$LATITUDE)

df$LONGITUDE[which(!is.na(df$LONGITUDE) & df$LONGITUDE>0)]=df$LONGITUDE[which(!is.na(df$LONGITUDE) & df$LONGITUDE>0)]*(-1)
df=df[!is.na(df$CASE_ID),]
fwrite(df, file="CHP_CleanRouteFeatures.cleaned.csv", sep=",", append=FALSE)