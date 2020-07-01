library(data.table)
library(dplyr)
library(tidyr)

setwd("Z:/PMRF/Amir/LEMO")
df=fread(file = "LEMO-2013_2018.csv", sep=",", header = TRUE)
colnames(df)

LEMO=setDT(df)[order(`Work Order No`, Workdate, Activity), 
      c(.(Duration=sum(Hours)),
        .(LaborCost=sum(Labor)), 
        .(MaterialCost=sum(Material)),
        .(EquipmentCost=sum(Equipment)), 
        .(totCost=sum(`LEM Total`))),
      by=.(`Work Order No`, Workdate, Activity)]
LEMO=na.omit(LEMO)
LEMO=LEMO%>%distinct()

setwd("Z:/PMRF/Amir/WorkOrder")
df=fread(file = "WorkOrder-2013_2018.csv", sep=",", header = TRUE)
colnames(df)

WorkOrder=na.omit(df, cols = "Wono")
WorkOrder[WorkOrder==""]=NA
WorkOrder=WorkOrder[-which(is.na(WorkOrder$`From PM`) & is.na(WorkOrder$`To PM`)),]
WorkOrder=WorkOrder %>%distinct()

setwd("Z:/PMRF/Amir/bin/Final Datasets")
df=fread(file="LEMO_WorkOrder.csv", sep=",", header = TRUE)
LEMO_WorkOrder=na.omit(df, cols = c("Workdate", "Activity", "rID"))

setwd("Z:/PMRF/Amir/PeMS/Lane Closure System")
df=fread(file="LCS-2013_2018.csv", sep=",", header = TRUE)
colnames(df)
df[df==""]=NA

LCS=na.omit(df, cols = c("DB ID", "Fwy-Dir", "Begin County", "End County",
                         "Begin State PM", "End State PM"))
LCS=LCS[LCS$Status=="Approved"]
LCS=LCS[which((!is.na(LCS$`Start Date`) | !is.na(LCS$`Status 1097 Date`)) & 
        (!is.na(LCS$`End Date`) | !is.na(LCS$`Status 1098 Date`) | 
         !is.na(LCS$`Status 1022 Date`))),]

setwd("Z:/PMRF/Amir/bin/Final Datasets")
df=fread(file="LEMO_ID.match.closure.csv", sep=",", header = TRUE)
df[df==""]=NA

setwd("Z:/PMRF/Amir/bin/Final Datasets")
df=fread(file = "AADT.csv", sep = ",", header = TRUE)
df=fread(file = "TRUCK_AADT.csv", sep = ",", header = TRUE)

setwd("Z:/PMRF/Amir")
df=fread(file = "HighwayElementMarkers+odom_coord.csv", sep=",", header = TRUE)
colnames(df)

setwd("Z:/PMRF/Amir/bin/Final Datasets")
df=fread(file = "SWITRS.csv", sep = ",", header = TRUE)
df=df[df$ACCIDENT_YEAR %in% c(2013:2018)]
df[df==""]=NA
SWITRS=df

df=fread(file = "CHP.csv", sep = ",", header = TRUE)

setwd("Z:/PMRF/Amir/bin/Final Datasets")
df=fread(file = "CHP_density.csv", sep = ",", header = TRUE)

setwd("Z:/PMRF/Amir/bin/")
df=fread(file = "LEMO_CHP.by.roadCond.csv", sep=",", header = TRUE)