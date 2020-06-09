library(data.table)
library(tidyr)
library(dplyr)
#set working directory
setwd("//ahmct-065/teams/PMRF/Amir")

LEMO_WorkOrder.df=fread(file = "./bin/LEMO_WorkOrder+odom.csv", sep=",", header = TRUE)
county_abbr=fread(file="County_Abbr_PeMS.code.csv", sep=",", header = TRUE)
hwMarker.df=hwMarker.df=fread(file="HighwayElementMarkers+odom_coord.csv", sep=",", header=TRUE)
LEMO_WorkOrder.df[LEMO_WorkOrder.df==""]=NA
hwMarker.df[hwMarker.df==""]=NA
hwMarker.df$begin_Odometer_Left=as.numeric(hwMarker.df$begin_Odometer_Left)
hwMarker.df$begin_Odometer_Right=as.numeric(hwMarker.df$begin_Odometer_Right)

hwMarker.df$begin_Odometer_Left[which(!is.na(hwMarker.df$THY_PM_SUFFIX_CODE) & 
                                      hwMarker.df$THY_PM_SUFFIX_CODE=="R" &
                                      !is.na(hwMarker.df$begin_Odometer_Right))]=
  hwMarker.df$begin_Odometer_Right[which(!is.na(hwMarker.df$THY_PM_SUFFIX_CODE) & 
                                           hwMarker.df$THY_PM_SUFFIX_CODE=="R" &
                                           !is.na(hwMarker.df$begin_Odometer_Right))]

hwMarker.df$begin_Odometer_Left[which(is.na(hwMarker.df$begin_Odometer_Left))]=
  hwMarker.df$begin_Odometer_Right[which(is.na(hwMarker.df$begin_Odometer_Left))]

names(hwMarker.df)[names(hwMarker.df)=="begin_Odometer_Left"]="Odometer"
hwMarker.df=hwMarker.df[,-c("begin_Odometer_Right")]
tempMarker.df=hwMarker.df[,c("THY_ID", "THY_COUNTY_CODE", "THY_ROUTE_NAME", "THY_ROUTE_SUFFIX_CODE", "Odometer")]
tempMarker.df$THY_COUNTY_CODE=county_abbr$`PEMS code`[match(tempMarker.df$THY_COUNTY_CODE, county_abbr$ABBREV.)]

tempLEMO.df=LEMO_WorkOrder.df[,c("ID",
                                 "beginCounty", "rID", "rSuffix",
                                 "from.odom.R", "from.odom.L")]
tempLEMO.df$rSuffix[tempLEMO.df$rSuffix=="SB" | tempLEMO.df$rSuffix=="C"]=NA


tempLEMO.df$from.odom.R[which(is.na(tempLEMO.df$from.odom.R))]=
  tempLEMO.df$from.odom.L[which(is.na(tempLEMO.df$from.odom.R))]

find_odom_interval=function(odom, Marker.df){
  ind=NA
  Marker.df=Marker.df[!is.na(Marker.df$Odometer),]
  Marker.df=setDT(Marker.df)[order(Marker.df$Odometer),,]
  ind=findInterval(odom, Marker.df$Odometer, rightmost.closed = TRUE)
  
  if (!is.na(ind) & ind!=0){
    return (Marker.df[ind,"THY_ID"])
  } else if (!is.na(ind) & ind==0){
    return (Marker.df[1,"THY_ID"])
  } else{
    return(NA)
  }
}

res=data.frame(matrix(NA, nrow = 0, ncol = 2))
colnames(res)=c("LEMO_ID", "THY_ID")

for (i in 1:dim(tempLEMO.df)[1]){
  filter_hw=data.frame()
  if(!is.na(tempLEMO.df$rSuffix[i])){
    filter_hw=tempMarker.df[which(tempMarker.df$THY_ROUTE_NAME==tempLEMO.df$rID[i] &
                                  tempMarker.df$THY_ROUTE_SUFFIX_CODE==tempLEMO.df$rSuffix[i] & 
                                  tempMarker.df$THY_COUNTY_CODE==tempLEMO.df$beginCounty[i]),]
  } else{
    filter_hw=tempMarker.df[which(tempMarker.df$THY_ROUTE_NAME==tempLEMO.df$rID[i] & 
                                  tempMarker.df$THY_COUNTY_CODE==tempLEMO.df$beginCounty[i]),]
  }
  
  if(dim(filter_hw)[1]>0 & !is.na(tempLEMO.df$from.odom.R[i])){
    res=rbind(res, cbind.data.frame(LEMO_ID=tempLEMO.df$ID[i],
                                    find_odom_interval(odom = tempLEMO.df$from.odom.R[i],
                                                              Marker.df = filter_hw)
                                    )
             )
  } else{
    res=rbind(res, cbind.data.frame(LEMO_ID=tempLEMO.df$ID[i], THY_ID=NA))
  }
  
  if (i%%100000==0){
    print(i)
    fwrite(res, file="LEMO_ID+THY_ID.csv", sep=",", append=FALSE)
  }
}
fwrite(res, file="LEMO_ID+THY_ID.csv", sep=",", append=FALSE)