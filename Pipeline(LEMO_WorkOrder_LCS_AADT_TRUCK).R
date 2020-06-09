setwd("//ahmct-065/teams/PMRF/Amir")
library(data.table)
library(lubridate)
library(RQuantLib)
library(anytime)
#### read workorders
WorkOrder.df=fread(file="./WorkOrder/WorkOrder-2013.csv", sep=",", header=TRUE)
LEMO.df=fread(file="./LEMO/LEMO-2013.csv", sep=",", header = TRUE)
LCS.df=fread(file="./PeMS/Lane Closure System/LCS-2013.csv", sep=",", header=TRUE)
county_abbr=fread(file="County_Abbr_PeMS.code.csv", sep=",", header=TRUE)
aadt.df=fread("./bin/aadt-2013.csv", sep=",", header=TRUE)
aadt.df$County=county_abbr$`PEMS code`[match(aadt.df$County, county_abbr$ABBREV.)]
aadt.df[aadt.df==""]=NA
truck.df=fread("./bin/truck-2013.csv", sep=",", header = TRUE)
truck.df$County=county_abbr$`PEMS code`[match(truck.df$County, county_abbr$ABBREV.)]
truck.df[truck.df==""]=NA

#### LEMO.merge.WorkOrder_LCS.print.func
###### cleans up WorkOrder, LEMO, and LCS databases
###### merges LEMO and WorkOder dataframes
###### prints .csv postmiles for converting to odometer values
source("./Codes/LEMO.merge.WorkOrder.print+LCS.print.R")
res=LEMO.merge.WorkOrder_LCS.print.func(LEMO.df, WorkOrder.df, LCS.df)
LEMO_WorkOrder.df=res$LEMO_WorkOrder.df
LCS.df=res$LCS.df

#### use submit_pm_queries.py and pm_odom_query.py to convert postmiles to queries

#### LEMO_WorkOrder+LCS.bind.odom.func
##### read odometer values
##### match odometers with LEMO_WorkOrder and LCS data.frames
source("./Codes/LEMO_WorkOrder+LCS.bind.odom.R")
res=LEMO_WorkOrder_LCS.bind.odom.func(LEMO_WorkOrder.df, LCS.df, county_abbr)
LEMO_WorkOrder.df=res$LEMO_WorkOrder.df
LCS.df=res$LCS.df

#### match LEMO_WorkOrder with LCS
##### identifies date interval of the closure using the LCS_timeInterval.func
##### for each work order in LEMO_WorkOrder.df, LCS.df is searched for matching closures using the LCS_matchPM.func
##### date and postmile information could be matched with tolerance
##### LCS_LEMO.func filters the LCS.df with the route information in LEMO_WorkOrder.df
source("./Codes/LEMO_WorkOrder.match.LCS.R")
#date.tol=1
#pm.tol=0.25
LCS.df[LCS.df==""]=NA
LEMO_WorkOrder.df[LEMO_WorkOrder.df==""]=NA

#figure out the date interval from start date, end date, cone placement date, cone pickup date, and cancel date
closure_date_interval.df=LCS_timeInterval.func(LCS.df$StartDate, LCS.df$EndDate, LCS.df$Date1097, LCS.df$Date1098,
                                               LCS.df$Date1022, LCS.df$`DB ID`)
#bind the LCS.df with start and end date for each closure
LCS.df=cbind.data.frame(LCS.df, true_start=closure_date_interval.df$true_start, true_end=closure_date_interval.df$true_end)

tempLCS.df=filter_LCS.func(LCS.df)
tempLEMO.df=filter_LEMO.func(LEMO_WorkOrder.df)

tempLCS.df=tempLCS.df[,c("DB ID", "true_start", "true_end", "FwyID", "begin.odom.R", "end.odom.R", "begin.odom.L", "end.odom.L", "FwyDir")]
tempLEMO.df=tempLEMO.df[,c("Work Order No", "Workdate", "rID", "from.odom.R", "to.odom.R", "from.odom.L", "to.odom.L")]

res=data.table(matrix(NA, nrow = 0, ncol=4))
colnames(res)=c("WONo", "work_date", "DB_ID", "coverage")

#for (i in 900001:dim(tempLEMO.df)[1]){
for (i in 1618774:1978158){
  temp_closure.df=tempLCS.df[which(tempLCS.df$FwyID==tempLEMO.df$rID[i]),]
  temp_closure.df=filter_closureDate.func(work_date=tempLEMO.df$Workdate[i], closure.df=temp_closure.df)
  
  if (dim(temp_closure.df)[1]==0){
    res=rbind(res, cbind.data.frame("WONo"=tempLEMO.df$`Work Order No`[i], 
                                    "work_date"=LEMO_WorkOrder.df$Workdate[i],
                                    "DB_ID"=NA,
                                    "coverage"=NA))
  } else{
    res=rbind(res, LCS_matchPM.func(workOrder.df=tempLEMO.df[i,], closure.df=temp_closure.df))
  }
  if (i%%50000==0){
    print(i)
    #to while running
    fwrite(res, file="./bin/WONO_DB.ID_matches.csv", sep=",", append=FALSE)
  }
}
fwrite(res, file="./bin/WONO_DB.ID_matches.csv", sep=",", append=FALSE)
matched_closures.df=drop_na(res)
matched_closures.df=distinct(matched_closures.df)

#filter LCS or LEMO for IDs in matched_closures.df
LCS_filter.df=LCS.df[which(LCS.df$`DB ID` %in% matched_closures.df$DB_ID),]
#merge LEMO_WorkOrder.df data frame with matching closures
LEMO_WorkOrder_Closure.df=merge(LEMO_WorkOrder.df, matched_closures.df, 
                                by.x=c("Work Order No", "Workdate"), by.y=c("WONo", "work_date"))
LEMO_WorkOrder_Closure.df=merge(LEMO_WorkOrder_Closure.df, LCS_filter.df, 
                                by.x="DB_ID", by.y="DB ID")
#remove duplicates
LEMO_WorkOrder_Closure.df=distinct(LEMO_WorkOrder_Closure.df)
#write the LEMO_WorkOrder_Closure to file
fwrite(LEMO_WorkOrder_Closure.df, file="./bin/LEMO_WorkOrder_LCS.csv", sep=",", append=FALSE)
#####match LEMO_WorkOrder_Closure with AADT and TRUCK data
######
######
source("./Codes/LEMO_WorkOrder_LCS.match.AADT+TRUCK.R")

LEMO_WorkOrder.df=fread(file="./bin/LEMO_WorkOrder+odom.csv", sep=",", header=TRUE)
LEMO_WorkOrder.df=cbind(LEMO_WorkOrder.df, ID=seq.int(nrow(LEMO_WorkOrder.df)))
LEMO_WorkOrder.df[LEMO_WorkOrder.df==""]=NA

county_abbr=fread(file="County_Abbr_PeMS.code.csv", sep=",", header=TRUE)
aadt.df=fread("./bin/2013-2018_aadt.csv", sep=",", header=TRUE)
aadt.df$CNTY=county_abbr$`PEMS code`[match(aadt.df$CNTY, county_abbr$ABBREV.)]
aadt.df[aadt.df==""]=NA

tempLEMO.df=LEMO_WorkOrder.df[,c("ID", "Workdate",
                           "beginCounty", "fPMmiles",
                           "endCounty", "tPMmiles",
                           "rID", "rSuffix",
                           "from.odom.R", "to.odom.R", "from.odom.L", "to.odom.L")]


res=data.table(matrix(NA, nrow = 0, ncol=13))
colnames(res)=c("ID",
                "R.back_peak_hour", 
                "R.back_peak_month",
                "R.back_aadt",
                "R.ahead_peak_hour",
                "R.ahead_peak_month",
                "R.ahead_aadt",
                "L.back_peak_hour",
                "L.back_peak_month",
                "L.back_aadt",
                "L.ahead_peak_hour",
                "L.ahead_peak_month",
                "L.ahead_aadt")

#for (i in 1:dim(tempLEMO.df)[1]){
for(i in 1:500000){
  temp_year=year(anytime(tempLEMO.df$Workdate[i]))
  temp_aadt=setDF(aadt.df)[which(aadt.df$Year==temp_year),]
  res=rbind(res, cbind.data.frame("ID"=tempLEMO.df$ID[i],  AADT_match.func(
                                                              beginCounty=tempLEMO.df$beginCounty[i],
                                                              beginPM=tempLEMO.df$fPMmiles[i],
                                                              endCounty=tempLEMO.df$tPMmiles[i],
                                                              endPM=tempLEMO.df$endCounty[i],
                                                              route=tempLEMO.df$rID[i],
                                                              rtsfx=tempLEMO.df$rSuffix[i],
                                                              from_odom.R=tempLEMO.df$from.odom.R[i],
                                                              to_odom.R=tempLEMO.df$to.odom.R[i],
                                                              from_odom.L=tempLEMO.df$from.odom.L[i],
                                                              to_odom.L=tempLEMO.df$to.odom.L[i],
                                                              temp_aadt
                                                           )
                                  )
            )
  if (i%%100000==0){
    fwrite(res, './bin/LEMO_ID.match.AADT.csv', sep=",", append = FALSE)
    print(i)
  }
}

fwrite(res, './bin/LEMO_ID.match.AADT.csv', sep=",", append = FALSE)
LEMO_WorkOrder.df=merge(LEMO_WorkOrder.df, res, by="ID")
fwrite(res, '/bin/LEMO_WorkOrder_AADT.csv', sep=",", append = FALSE)


truck.df=fread("./bin/2013-2018_truck.csv", sep=",", header = TRUE)
truck.df$CNTY=county_abbr$`PEMS code`[match(truck.df$CNTY, county_abbr$ABBREV.)]
truck.df[truck.df==""]=NA
truck.df$Odometer_Left=as.numeric(truck.df$Odometer_Left)
truck.df$Odometer_Right=as.numeric(truck.df$Odometer_Right)

tempLEMO.df=LEMO_WorkOrder.df[,c("ID", "Workdate",
                                 "beginCounty", "fPMmiles",
                                 "endCounty", "tPMmiles",
                                 "rID", "rSuffix",
                                 "from.odom.R", "to.odom.R", "from.odom.L", "to.odom.L")]


res=data.table(matrix(NA, nrow = 0, ncol=3))
colnames(res)=c("ID",
                "R.ahead_truck_aadt",
                "L.ahead_truck_aadt")

for (i in 1:dim(tempLEMO.df)[1]){
  #temp_year=year(anytime(tempLEMO.df$Workdate[i]))
  #temp_truck=setDF(truck.df)[which(truck.df$Year==temp_year),]
  res=rbind(res, cbind.data.frame("ID"=tempLEMO.df$ID[i], TRUCK_match.func(
                                                                            beginCounty=tempLEMO.df$beginCounty[i],
                                                                            beginPM=tempLEMO.df$fPMmiles[i],
                                                                            endCounty=tempLEMO.df$endCounty[i],
                                                                            endPM=tempLEMO.df$tPMmiles[i],
                                                                            route=tempLEMO.df$rID[i],
                                                                            rtsfx=tempLEMO.df$rSuffix[i],
                                                                            from_odom.R=tempLEMO.df$from.odom.R[i],
                                                                            to_odom.R=tempLEMO.df$to.odom.R[i],
                                                                            from_odom.L=tempLEMO.df$from.odom.L[i],
                                                                            to_odom.L=tempLEMO.df$to.odom.L[i],
                                                                            truck.df
                                                                          )
                                )
           )
  if (i%%100000==0){
    print(i)
  }
}

fwrite(res, file="./bin/LEMO_ID.match.TRUCK_AADT.csv", sep=",", append = FALSE)
LEMO_WorkOrder.df=merge(LEMO_WorkOrder.df, res, by="ID")
fwrite(res, file="./bin/LEMO_WorkOrder_TRUCK.csv", sep=",", append = FALSE)

#####match everything together, i.e., LEMO_WorkOrder_LCS_AADT_TRUCK
######
######
LEMO_LCS_AADT.df=merge(LEMO_WorkOrder_Closure.df, LEMO_WorkOrder.df[,c(1,2,3,13, 33:52)], by=c("Work Order No", "Activity", "Workdate", "Crew"))
colnames(LEMO_LCS_AADT.df)[99:118]=paste(colnames(LEMO_LCS_AADT.df)[99:118], "LEMO", sep=".")
LEMO_LCS_AADT.df=merge(LEMO_LCS_AADT.df, LCS.df[,c(57, 64:83)], by="DB ID", all.x = TRUE)
colnames(LEMO_LCS_AADT.df)[119:138]=paste(colnames(LEMO_LCS_AADT.df)[119:138], "LCS", sep=".")
fwrite(LEMO_LCS_AADT.df, file="./bin/LEMO_LCS_AADT.csv", sep=",", append = TRUE)



#####Summarize the 2013 data set
######
######
df=LEMO_LCS_AADT.df[,c("Work Order No", "Activity", "Activity Description", "Workdate", 
                       "Dist", "rID", "rSuffix", 
                       "beginCounty", "fPMprefix", "fPMmiles", "from.odom.R", "from.odom.L", 
                       "endCounty", "tPMprefix", "tPMmiles", "to.odom.R", "to.odom.L",
                       "Hours.sum", "Labors.sum", "Equipment.sum", "Material.sum", "LEM.sum",
                       "matchType", "alignment","coveredLength", "DB ID", "FwyID", "FwyDir",
                       "Begin County", "Begin Abs PM", "Begin State PM", "begin.odom.R", "begin.odom.L",
                       "End County", "End Abs PM", "End State PM", "end.odom.R", "end.odom.L",
                       "StartDate", "Date1097", "true_start", "EndDate", "Date1098", "Date1022", "true_end", "Request Date",
                       "Length", "Status", "Work Type", "Duration", "Planned Duration", "Reported Duration",
                       "Type", "Facility", "Closure Lanes", "Total Lanes",
                       "R.back_aadt.LEMO", "R.ahead_aadt.LEMO", "L.back_aadt.LEMO", "L.ahead_aadt.LEMO",
                       "R.back_truck_aadt.LEMO", "R.ahead_truck_aadt.LEMO", "L.back_truck_aadt.LEMO", "L.ahead_truck_aadt.LEMO",
                       "R.back_aadt.LCS", "R.ahead_aadt.LCS", "L.back_aadt.LCS", "L.ahead_aadt.LCS",
                       "R.back_truck_aadt.LCS", "R.ahead_truck_aadt.LCS", "L.back_truck_aadt.LCS", "L.ahead_truck_aadt.LCS")]


df$R.back_aadt.LEMO=gsub(",", "", df$R.back_aadt.LEMO)
df$R.ahead_aadt.LEMO=gsub(",", "", df$R.ahead_aadt.LEMO)
df$L.back_aadt.LEMO=gsub(",", "", df$L.back_aadt.LEMO)
df$L.ahead_aadt.LEMO=gsub(",", "", df$L.ahead_aadt.LEMO)
df$R.back_truck_aadt.LEMO=gsub(",", "", df$R.back_truck_aadt.LEMO)
df$R.ahead_truck_aadt.LEMO=gsub(",", "", df$R.ahead_truck_aadt.LEMO)
df$L.back_truck_aadt.LEMO=gsub(",", "", df$L.back_truck_aadt.LEMO)
df$L.ahead_truck_aadt.LEMO=gsub(",", "", df$L.ahead_truck_aadt.LEMO)

df$R.back_aadt.LCS=gsub(",", "", df$R.back_aadt.LCS)
df$R.ahead_aadt.LCS=gsub(",", "", df$R.ahead_aadt.LCS)
df$L.back_aadt.LCS=gsub(",", "", df$L.back_aadt.LCS)
df$L.ahead_aadt.LCS=gsub(",", "", df$L.ahead_aadt.LCS)
df$R.back_truck_aadt.LCS=gsub(",", "", df$R.back_truck_aadt.LCS)
df$R.ahead_truck_aadt.LCS=gsub(",", "", df$R.ahead_truck_aadt.LCS)
df$L.back_truck_aadt.LCS=gsub(",", "", df$L.back_truck_aadt.LCS)
df$L.ahead_truck_aadt.LCS=gsub(",", "", df$L.ahead_truck_aadt.LCS)

df$R.back_aadt.LEMO=sapply(strsplit(as.character(df$R.back_aadt.LEMO), " "), 
                                               function(x) mean(as.numeric(x), na.rm=TRUE))
df$R.ahead_aadt.LEMO=sapply(strsplit(as.character(df$R.ahead_aadt.LEMO), " "), 
                                                function(x) mean(as.numeric(x), na.rm=TRUE))
df$R.back_truck_aadt.LEMO=sapply(strsplit(as.character(df$R.back_truck_aadt.LEMO), " "), 
                                                     function(x) mean(as.numeric(x), na.rm=TRUE))
df$R.ahead_truck_aadt.LEMO=sapply(strsplit(as.character(df$R.ahead_truck_aadt.LEMO), " "), 
                                                      function(x) mean(as.numeric(x), na.rm=TRUE))
df$L.back_aadt.LEMO=sapply(strsplit(as.character(df$L.back_aadt.LEMO), " "), 
                                               function(x) mean(as.numeric(x), na.rm=TRUE))
df$L.ahead_aadt.LEMO=sapply(strsplit(as.character(df$L.ahead_aadt.LEMO), " "), 
                                                function(x) mean(as.numeric(x), na.rm=TRUE))
df$L.back_truck_aadt.LEMO=sapply(strsplit(as.character(df$L.back_truck_aadt.LEMO), " "), 
                                                     function(x) mean(as.numeric(x), na.rm=TRUE))
df$L.ahead_truck_aadt.LEMO=sapply(strsplit(as.character(df$L.ahead_truck_aadt.LEMO), " "), 
                                                      function(x) mean(as.numeric(x), na.rm=TRUE))

df$R.back_aadt.LCS=sapply(strsplit(as.character(df$R.back_aadt.LCS), " "), 
                                              function(x) mean(as.numeric(x), na.rm=TRUE))
df$R.ahead_aadt.LCS=sapply(strsplit(as.character(df$R.ahead_aadt.LCS), " "), 
                                               function(x) mean(as.numeric(x), na.rm=TRUE))
df$R.back_truck_aadt.LCS=sapply(strsplit(as.character(df$R.back_truck_aadt.LCS), " "), 
                                                    function(x) mean(as.numeric(x), na.rm=TRUE))
df$R.ahead_truck_aadt.LCS=sapply(strsplit(as.character(df$R.ahead_truck_aadt.LCS), " "), 
                                                     function(x) mean(as.numeric(x), na.rm=TRUE))
df$L.back_aadt.LCS=sapply(strsplit(as.character(df$L.back_aadt.LCS), " "), 
                                              function(x) mean(as.numeric(x), na.rm=TRUE))
df$L.ahead_aadt.LCS=sapply(strsplit(as.character(df$L.ahead_aadt.LCS), " "), 
                                               function(x) mean(as.numeric(x), na.rm=TRUE))
df$L.back_truck_aadt.LCS=sapply(strsplit(as.character(df$L.back_truck_aadt.LCS), " "), 
                                                    function(x) mean(as.numeric(x), na.rm=TRUE))
df$L.ahead_truck_aadt.LCS=sapply(strsplit(as.character(df$L.ahead_truck_aadt.LCS), " "), 
                                                     function(x) mean(as.numeric(x), na.rm=TRUE))

fwrite(df, file="./bin/LEMO_LCS_AADT.summary.csv", sep=",", append = TRUE)