setwd("//ahmct-065/teams/PMRF/Amir")
library(data.table)
df=fread(file="./bin/all_CHP.csv", sep=",", header=TRUE)
df=df[, c("CASE_ID", "ACCIDENT_YEAR", "COLLISION_DATE", "COLLISION_TIME", "DAY_OF_WEEK", "POPULATION", "PRIMARY_RD",
         "SECONDARY_RD", "DIRECTION", "WEATHER_1", "WEATHER_2", "STATE_HWY_IND", "CALTRANS_COUNTY", "CALTRANS_DISTRICT",
         "STATE_ROUTE", "ROUTE_SUFFIX", "POSTMILE_PREFIX", "POSTMILE", "LOCATION_TYPE", "RAMP_INTERSECTION", "SIDE_OF_HWY",
         "COLLISION_SEVERITY", "NUMBER_KILLED", "NUMBER_INJURED", "PARTY_COUNT", "PRIMARY_COLL_FACTOR", "PCF_VIOL_CATEGORY",
         "ROAD_SURFACE", "ROAD_COND_1", "ROAD_COND_2", "LIGHTING", "CONTROL_DEVICE", "CHP_ROAD_TYPE", "LATITUDE", "LONGITUDE")]

df.to.odom=df[, c("CASE_ID", "CALTRANS_COUNTY", "STATE_ROUTE", "ROUTE_SUFFIX", "POSTMILE_PREFIX", "POSTMILE")]
df.to.odom$ROUTE_SUFFIX[df.to.odom$ROUTE_SUFFIX=="-" |
                        df.to.odom$ROUTE_SUFFIX=="" |
                        df.to.odom$ROUTE_SUFFIX=="B"]=""

df.to.odom$POSTMILE_PREFIX[df.to.odom$POSTMILE_PREFIX=="-" |
                           df.to.odom$POSTMILE_PREFIX==""]=""
df.to.odom$STATE_ROUTE=paste(df.to.odom$STATE_ROUTE, df.to.odom$ROUTE_SUFFIX, sep="")
df.to.odom$POSTMILE=paste(df.to.odom$POSTMILE_PREFIX, df.to.odom$POSTMILE, sep="")
df.to.odom=df.to.odom[,c("CASE_ID", "CALTRANS_COUNTY", "STATE_ROUTE", "POSTMILE")]

no.pm=df[which(df.to.odom$CALTRANS_COUNTY==""), c("CASE_ID", "LATITUDE", "LONGITUDE")]
df.to.odom=df.to.odom[-c(which(df.to.odom$CALTRANS_COUNTY=="")),]

no.pm.geo=no.pm[which(!is.na(no.pm$LATITUDE)),]
no.pm.geo=no.pm[which(!is.na(no.pm$LONGITUDE)),]

setwd("C:/Users/anasr/Desktop")
fwrite(df.to.odom, file="odom.csv", sep=",", append=FALSE)
fwrite(no.pm.geo, file="geo.csv", sep=",", append=FALSE)
