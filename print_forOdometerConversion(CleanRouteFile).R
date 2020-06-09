library(data.table)
#####################################################################################
#####################################################################################
#### Assumption: HighwayMarkers mark the start and end of a route in each county ####
df=fread(file="HighwayMarkers-SW_2018-03-19.csv", sep=",", header=TRUE)
County_Abbr_code=fread(file="./PeMS/County_Abbr_PeMS.code.csv", sep=",", header=TRUE)
df=df[,.(county_stPM=min(THY_BEGIN_PM_AMT),
         county_enPM=max(THY_END_PM_AMT)),
      by=.(THY_ROUTE_NAME, THY_COUNTY_CODE)]
df$THY_COUNTY_CODE=County_Abbr_code$`PEMS code`[match(df$THY_COUNTY_CODE, County_Abbr_code$ABBREV.)]
fwrite(df, file="Highway_countyPM.csv", sep=",", append=FALSE)

#####################################################################################
#####################################################################################
####### Assumption: Odometer mark the start and end of a route in each county #######
df=fread(file="OdometerMarkers.csv", sep=",", header=TRUE)
df=df[order(route, odometer, postmile_suffix), .(countyStartOdometer=min(odometer),
                                countyEndOdometer=max(odometer),
                                countyStartLat=min(latitude),
                                countyEndLat=max(latitude),
                                countyStartLon=min(longitude),
                                countyEndLon=max(longitude),
                                countyStartPM=min(postmile),
                                countyEndPM=max(postmile)), by=.(route, county_code, postmile_suffix)]
df$county_code=County_Abbr_code$`PEMS code`[match(df$county_code, County_Abbr_code$ABBREV.)]
fwrite(df, file="Odometer_countyPM.csv", sep=",", append=FALSE)

