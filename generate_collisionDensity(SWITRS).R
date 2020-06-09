setwd("//ahmct-065/teams/PMRF/Amir")
library(data.table)
library(dplyr)
library(tidyr)

CHP.df=fread(file = "./bin/Final Datasets/CHP.csv", sep=",", header=TRUE)
CHP.df[CHP.df==""]=NA
#CHP.df$SIDE_OF_HWY[which(is.na(CHP.df$SIDE_OF_HWY))]=CHP.df$DIRECTION[which(is.na(CHP.df$SIDE_OF_HWY))]
CHP.df$SIDE_OF_HWY[which(is.na(CHP.df$SIDE_OF_HWY))]="R"
CHP.df$Odometer=as.numeric(CHP.df$Odometer)

#LEMO.df=fread(file = "./bin/Final Datasets/LEMO_WorkOrder+odom.csv", sep=",", header=TRUE)

RouteMarker.df=fread(file="./RouteMarker.odometer.csv", sep = ",", header = TRUE)
#RouteMarker_bins.df=setDT(RouteMarker.df)[order(route, start), .(segments=seq.maker(start, end)), by=.(route, county_code, start, end)]
#fwrite(RouteMarker_bins.df, file = "RouteMarker_bins.csv", sep=",", append=FALSE)

seq.maker=function(start, end){
  if ((end-start)%%2==0){
    return(seq(start, end, 2))
  } else{
    return(c(seq(start, end, 2), end))
  }
}

bin.df=as.data.frame(matrix(NA, nrow = 0, ncol = 5))
colnames(bin.df)=c("CASE_ID", "COUNTY_ROUTE_ID", "BIN_ID", "bin_start", "bin_end")
#bins=apply(RouteMarker.df, 1, function(x) seq(x[3], x[4], 2))

for (i in 1:length(CHP.df$CASE_ID)){
  county=CHP.df$CALTRANS_COUNTY[i]
  if (is.na(county)){
    next
  }
  route_id=CHP.df$STATE_ROUTE[i]
  if (is.na(route_id)){
    next
  }
  odom=NA
  if (!is.na(CHP.df$Odometer[i])){
    odom=CHP.df$Odometer[i]
  } else{
    next
  }
  
  filter_routeMarker.df=RouteMarker.df[which(RouteMarker.df$county_code==county & RouteMarker.df$route==route_id),]
  ind=findInterval(odom, filter_routeMarker.df$start, rightmost.closed = TRUE)
  if (ind==0){
    ind=1
    temp_bins=seq(filter_routeMarker.df$start[ind], filter_routeMarker.df$end[ind], 2)
    route_county_id=which(RouteMarker.df$route==route_id &
                          RouteMarker.df$county_code==county & 
                          RouteMarker.df$start==filter_routeMarker.df$start[ind])
    bin_id=findInterval(odom, temp_bins, rightmost.closed = TRUE)
    if (bin_id==0){
      bin_id=1
    }
    bin_start=temp_bins[bin_id]
    bin_end=temp_bins[bin_id+1]
    #not sure what to do if a chp odometer is found before the minimum odometer listed for the route!!
    #for now, count it as if it belongs to the first bin
    bin.df=rbind(bin.df, data.frame(CASE_ID=CHP.df$CASE_ID[i],
                                    ROUTE_COUNTY_ID=route_county_id,
                                    BIN_ID=bin_id,
                                    bin_start=bin_start,
                                    bin_end=bin_end))
  } else{
    if (filter_routeMarker.df$end[ind]-filter_routeMarker.df$start[ind]>2){
      temp_bins=seq(filter_routeMarker.df$start[ind], filter_routeMarker.df$end[ind], 2)
      route_county_id=which(RouteMarker.df$route==route_id &
                            RouteMarker.df$county_code==county & 
                            RouteMarker.df$start==filter_routeMarker.df$start[ind])
      bin_id=findInterval(odom, temp_bins, rightmost.closed = TRUE)
      if (bin_id==0){
        bin_id=1
      }
      bin_start=temp_bins[bin_id]
      bin_end=temp_bins[bin_id+1]
      bin.df=rbind(bin.df, data.frame(CASE_ID=CHP.df$CASE_ID[i],
                                      ROUTE_COUNTY_ID=route_county_id,
                                      BIN_ID=bin_id,
                                      bin_start=bin_start,
                                      bin_end=bin_end))
    } else{
      route_county_id=which(RouteMarker.df$route==route_id &
                            RouteMarker.df$county_code==county & 
                            RouteMarker.df$start==filter_routeMarker.df$start[ind])
      bin_id=1
      bin.df=rbind(bin.df, data.frame(CASE_ID=CHP.df$CASE_ID[i],
                                      ROUTE_COUNTY_ID=route_county_id,
                                      BIN_ID=bin_id,
                                      bin_start=filter_routeMarker.df$start[ind],
                                      bin_end=filter_routeMarker.df$end[ind]))
    }
  }
  if (i %% 100000==0){
    print(i)
    fwrite(bin.df, file = "./bin/collision_segments.csv", sep=",", append=TRUE)
  }
}
bin.df=cbind.data.frame(bin.df, route=RouteMarker.df$route[match(bin.df$ROUTE_COUNTY_ID, RouteMarker.df$route_county_id)], 
                        county=RouteMarker.df$county_code[match(bin.df$ROUTE_COUNTY_ID, RouteMarker.df$route_county_id)])
fwrite(bin.df, file = "./bin/collision_segments.csv", sep=",", append=TRUE)

freq_table=setDT(bin.df)[order(route, county, bin_start), .(freq=length(unique(CASE_ID))), by=.(route, county, bin_start, bin_end)]
fwrite(freq_table, file="./bin/collision_density.csv", sep=",", append = FALSE)