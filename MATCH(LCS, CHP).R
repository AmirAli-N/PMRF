#matching closure data with the collision data
setwd("Z:/PMRF/Amir")

library(data.table)
library(lubridate)
library(anytime)
library(tidyr)
library(stringi)

LCS.df=fread(file = "./bin/Final Datasets/LCS.csv", sep=",", header = TRUE)
LCS.df[LCS.df==""]=NA
CHP.df=fread(file = "./bin/Final Datasets/CHP_CleanRouteFile.csv", sep=",", header=TRUE)
CHP.df[CHP.df==""]=NA
county_abbr=fread(file = "County_Abbr_PeMS.code.csv", sep = ",", header = TRUE)

#filter approved closures, and collisions between 2013-2018
LCS.df=LCS.df[LCS.df$Status=="Approved",]
CHP.df=CHP.df[!CHP.df$ACCIDENT_YEAR %in% c(2011, 2012),]

#process start and end date/time for each closure
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
rm(true_end_date, true_end_time, true_start_date, true_start_time)

#remove unneccessary columns
LCS.temp=LCS.df[, c("DB ID", "FwyID", "FwyDir", 
                    "Begin County", "Begin State PM", "End County", "End State PM", 
                    "closure_start_date", "closure_start_time", "closure_end_date", "closure_end_time")]
CHP.temp=CHP.df[, c("CASE_ID", "ACCIDENT_YEAR", "COLLISION_DATE", "COLLISION_TIME", 
                  "DIRECTION", "SIDE_OF_HWY", "CALTRANS_COUNTY", "STATE_ROUTE", "POSTMILE")]

#format date time in LCS and CHP
LCS.temp$closure_start_date=anydate(LCS.temp$closure_start_date)
LCS.temp$closure_end_date=anydate(LCS.temp$closure_end_date)
LCS.temp=LCS.temp %>% unite(col = "start_date_time", c(closure_start_date, closure_start_time), 
                            sep = " ", remove = TRUE) %>%
                      unite(col = "end_date_time", c(closure_end_date, closure_end_time),
                            sep = " ", remove = TRUE)

CHP.temp$COLLISION_DATE=anydate(CHP.temp$COLLISION_DATE)
CHP.temp$COLLISION_TIME=paste(stri_pad(CHP.temp$COLLISION_TIME %/% 100, 2, pad="0"),
                              stri_pad(CHP.temp$COLLISION_TIME %% 100, 2, pad="0"), sep=":")
CHP.temp=CHP.temp %>% unite(col = "date_time", c(COLLISION_DATE, COLLISION_TIME), 
                            sep = " ", remove = TRUE)
#remove na
LCS.temp=na.omit(LCS.temp)
CHP.temp=na.omit(CHP.temp)

#change county abbreviation to code
CHP.temp$CALTRANS_COUNTY=county_abbr$`PEMS code`[match(CHP.temp$CALTRANS_COUNTY, county_abbr$ABBREV.)]

#build the result matrix
res=data.table(matrix(NA, nrow = 0, ncol=2))
colnames(res)=c("DB_ID", "CASE_ID")

#match every collision
pm.tol=0.25
for (i in 1:dim(CHP.temp)[1]){
#for (i in 1:1000){
  #mathc the route
  temp.closure=LCS.temp[LCS.temp$FwyID==CHP.temp$STATE_ROUTE[i],]
  if (dim(temp.closure)[1]==0){
    next
  }
  #match the side of road
  temp.closure=temp.closure[temp.closure$FwyDir==CHP.temp$SIDE_OF_HWY[i],]
  if (dim(temp.closure)[1]==0){
    next
  }
  #match the county
  temp.closure=temp.closure[temp.closure$`Begin County`==CHP.temp$CALTRANS_COUNTY[i] |
                            temp.closure$`End County`==CHP.temp$CALTRANS_COUNTY[i],]
  if (dim(temp.closure)[1]==0){
    next
  }
  #match postmile
  temp.closure=temp.closure[temp.closure$`Begin State PM`-pm.tol<=CHP.temp$POSTMILE[i] & 
                            temp.closure$`End State PM`+pm.tol>=CHP.temp$POSTMILE[i],]
  if (dim(temp.closure)[1]==0){
    next
  }
  #match the date_time
  temp.closure=temp.closure[
    as.POSIXct(CHP.temp$date_time[i]) %within% interval(as.POSIXct(temp.closure$start_date_time),
                                                        as.POSIXct(temp.closure$end_date_time)),]
  if (dim(temp.closure)[1]==0){
    next
  }else{
    res=rbind(res, cbind("DB_ID"=temp.closure$`DB ID`, "CASE_ID"=CHP.temp$CASE_ID[i]))
  }
  if (i%%100000==0){
    fwrite(res, './bin/LCS_ID.match.CHP.csv', sep=",", append = FALSE)
    print(i)
  }
}

fwrite(res, './bin/LCS_ID.match.CHP.csv', sep=",", append = FALSE)