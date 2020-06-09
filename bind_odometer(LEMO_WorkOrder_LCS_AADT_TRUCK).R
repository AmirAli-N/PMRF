library(data.table)
library(tidyr)
library(dplyr)
#####################################################################################################################
#####################################################################################################################
###################### attach odometer values to LEMO_WorkOrder, LCS, and AADT dataframes ###########################
setwd("G:/My Drive/WorkingDesk/PM-to-ODOM")
LEMO_from_odom.df=fread(file="LEMO-from.csv", sep=",", header=TRUE)
LEMO_to_odom.df=fread(file="LEMO-to.csv", sep=",", header=TRUE)
LCS_from_odom.df=fread(file="LCS-from.csv", sep=",", header=TRUE)
LCS_to_odom.df=fread(file="LCS-to.csv", sep=",", header=TRUE)
AADT_odom.df=fread(file="aadt_odom.csv", sep=",", header=TRUE)
TRUCK_odom.df=fread(file="truck_odom.csv", sep = ",", header=TRUE)

LEMO_WorkOrder.df=cbind(LEMO_WorkOrder.df, from.odom_R=LEMO_from_odom.df$R, from_odom_L=LEMO_from_odom.df$L)
LEMO_WorkOrder.df=cbind(LEMO_WorkOrder.df, to.odom_R=LEMO_to_odom.df$R, to.odom_L=LEMO_to_odom.df$L)

LCS.df=cbind(LCS.df, from.odom_R=LCS_from_odom.df$R, from.odom_L=LCS_from_odom.df$L)
LCS.df=cbind(LCS.df, to.odom_R=LCS_to_odom.df$R, to.odom_L=LCS_to_odom.df$L)

aadt.df=cbind(aadt.df, R.odom=AADT_odom.df$R, L.odom=AADT_odom.df$L)
aadt.df[aadt.df==""]=NA
aadt.df$R.odom[which(aadt.df$pmsfx=="L")]=NA
aadt.df$L.odom[which(aadt.df$pmsfx=="R")]=NA
aadt.df=aadt.df[!duplicated(aadt.df)]
fwrite(aadt.df, file="aadt-2013.csv", sep=",", append=FALSE)

truck.df=cbind(truck.df, R.odom=TRUCK_odom.df$R, L.odom=TRUCK_odom.df$L)
truck.df[truck.df==""]=NA
truck.df=truck.df[,-c("Description", "aadt.total", "truck.tot.aadt.2", "truck.tot.aadt.3", "truck.tot.aadt.4", "truck.tot.aadt.5+",
             "truck.per.tot.vehicle.2", "truck.per.tot.vehicle.3", "truck.per.tot.vehicle.4", "truck.per.tot.vehicle.5+",
             "EAL-2way", "Year")]

truck.df=pivot_wider(truck.df, names_from = Leg, values_from = c(truck.aadt.total, truck.per.tot.vehicle), names_sep = " ", 
                 values_fill = list(truck.aadt.total=NA, truck.per.tot.vehicle=NA), 
                 values_fn = list(truck.aadt.total=mean, truck.per.tot.vehicle=mean))

truck.df$`truck.aadt.total A`[is.na(truck.df$`truck.aadt.total A`) & !is.na(truck.df$`truck.aadt.total O`)]=truck.df$`truck.aadt.total O`[!is.na(truck.df$`truck.aadt.total O`) & is.na(truck.df$`truck.aadt.total A`)]
truck.df$`truck.aadt.total B`[is.na(truck.df$`truck.aadt.total B`) & !is.na(truck.df$`truck.aadt.total O`)]=truck.df$`truck.aadt.total O`[!is.na(truck.df$`truck.aadt.total O`) & is.na(truck.df$`truck.aadt.total B`)]
truck.df$`truck.aadt.total B`[is.na(truck.df$`truck.aadt.total B`) & !is.na(truck.df$`truck.aadt.total X`)]=truck.df$`truck.aadt.total X`[!is.na(truck.df$`truck.aadt.total X`) & is.na(truck.df$`truck.aadt.total B`)]
truck.df$`truck.aadt.total A`[is.na(truck.df$`truck.aadt.total A`) & !is.na(truck.df$`truck.aadt.total X`)]=truck.df$`truck.aadt.total X`[!is.na(truck.df$`truck.aadt.total X`) & is.na(truck.df$`truck.aadt.total A`)]

truck.df$`truck.per.tot.vehicle A`[is.na(truck.df$`truck.per.tot.vehicle A`) & !is.na(truck.df$`truck.per.tot.vehicle O`)]=truck.df$`truck.per.tot.vehicle O`[!is.na(truck.df$`truck.per.tot.vehicle O`) & is.na(truck.df$`truck.per.tot.vehicle A`)]
truck.df$`truck.per.tot.vehicle B`[is.na(truck.df$`truck.per.tot.vehicle B`) & !is.na(truck.df$`truck.per.tot.vehicle O`)]=truck.df$`truck.per.tot.vehicle O`[!is.na(truck.df$`truck.per.tot.vehicle O`) & is.na(truck.df$`truck.per.tot.vehicle B`)]
truck.df$`truck.per.tot.vehicle A`[is.na(truck.df$`truck.per.tot.vehicle A`) & !is.na(truck.df$`truck.per.tot.vehicle X`)]=truck.df$`truck.per.tot.vehicle X`[!is.na(truck.df$`truck.per.tot.vehicle X`) & is.na(truck.df$`truck.per.tot.vehicle A`)]
truck.df$`truck.per.tot.vehicle B`[is.na(truck.df$`truck.per.tot.vehicle B`) & !is.na(truck.df$`truck.per.tot.vehicle X`)]=truck.df$`truck.per.tot.vehicle X`[!is.na(truck.df$`truck.per.tot.vehicle X`) & is.na(truck.df$`truck.per.tot.vehicle B`)]

truck.df=truck.df[,-c(11, 12, 15, 16)]
fwrite(truck.df, file="truck-2013.csv", sep=",", append=FALSE)