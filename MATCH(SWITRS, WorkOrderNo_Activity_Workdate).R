setwd("//ahmct-065/teams/PMRF/Amir")

library(data.table)
library(lubridate)
library(anytime)
library(purrr)

LEMO_WorkOrder.df=fread(file="./bin/Final Datasets/LEMO_WorkOrder+odom.csv", sep=",", header=TRUE)
LEMO_WorkOrder.df=cbind(LEMO_WorkOrder.df, ID=seq.int(nrow(LEMO_WorkOrder.df)))
tempLEMO.df=LEMO_WorkOrder.df[,c("Dist", "rID", "Workdate", "from.odom.R", "to.odom.R", "from.odom.L", "to.odom.L", "ID")]
tempLEMO.df[tempLEMO.df==""]=NA

CHP.df=fread(file="./bin/Final Datasets/CHP.csv", sep=",", header=TRUE)
CHP.df=CHP.df[-c(which(CHP.df$ACCIDENT_YEAR==2011 | CHP.df$ACCIDENT_YEAR==2012)),]
routes=regmatches(CHP.df$PRIMARY_RD[which(is.na(CHP.df$STATE_ROUTE))], 
                  gregexpr("[[:digit:]]+", 
                           CHP.df$PRIMARY_RD[which(is.na(CHP.df$STATE_ROUTE))]
                           )
                  )
routes=map(routes, 1)
routes[sapply(routes, is.null)]=NA
routes=unlist(routes)

CHP.df$STATE_ROUTE[which(is.na(CHP.df$STATE_ROUTE))]=routes

routes=regmatches(CHP.df$SECONDARY_RD[which(is.na(CHP.df$STATE_ROUTE))], 
                  gregexpr("[[:digit:]]+", 
                           CHP.df$SECONDARY_RD[which(is.na(CHP.df$STATE_ROUTE))]
                          )
                  )
routes=map(routes, 1)
routes[sapply(routes, is.null)]=NA
routes=unlist(routes)

CHP.df$STATE_ROUTE[which(is.na(CHP.df$STATE_ROUTE))]=routes

tempCHP=CHP.df[,c("CASE_ID", "STATE_ROUTE", "COLLISION_DATE", "Odometer", "SIDE_OF_HWY")]
tempCHP[tempCHP==""]=NA
tempCHP=tempCHP[which(!is.na(CHP.df$Odometer) & !is.na(CHP.df$STATE_ROUTE)),]
tempCHP$COLLISION_DATE=anydate(tempCHP$COLLISION_DATE)

collision_match_ID.df=tempCHP[FALSE,]
collision_match_ID.df=cbind.data.frame("ID"=integer(), collision_match_ID.df)

pm.tol=0.25
for (i in 1500001:dim(tempLEMO.df)[1]){
  #filter.chp=tempCHP[which(tempCHP$CALTRANS_DISTRICT==tempLEMO.df$Dist[i]),]
  filter.chp=tempCHP[which(tempCHP$STATE_ROUTE==tempLEMO.df$rID[i]),]
  filter.chp=filter.chp[which(filter.chp$COLLISION_DATE==tempLEMO.df$Workdate[i]),]
  
  filter.chp=filter.chp[which((filter.chp$Odometer>=tempLEMO.df$from.odom.R[i]-pm.tol & 
                               filter.chp$Odometer<=tempLEMO.df$to.odom.R[i]+pm.tol &
                               (filter.chp$SIDE_OF_HWY=="R" | is.na(filter.chp$SIDE_OF_HWY))) |
                              
                               (filter.chp$Odometer>=tempLEMO.df$from.odom.L[i]-pm.tol &
                               filter.chp$Odometer<=tempLEMO.df$to.odom.L[i]+pm.tol &
                               (filter.chp$SIDE_OF_HWY=="L" | is.na(filter.chp$SIDE_OF_HWY)))),]
  if (dim(filter.chp)[1]!=0){
    collision_match_ID.df=rbind(collision_match_ID.df, cbind.data.frame("ID"=tempLEMO.df$ID[i], filter.chp), use.names=TRUE)
  }
  if (i%%100000==0){
    print(i)
    fwrite(collision_match_ID.df, file="./bin/LEMO_ID.matches.CHP.csv", sep=",", append=FALSE)
  }
}
fwrite(collision_match_ID.df, file="./bin/LEMO_ID.matches.CHP.csv", sep=",", append=FALSE)
