setwd("//ahmct-065/teams/PMRF/Amir")

library(data.table)
library(dplyr)
library(tidyr)

county_abbr=fread(file = "COunty_Abbr_PeMS.code.csv", sep=",", header = TRUE)

#clean up the LEMO_Closure matches
LEMO_LCS.df=fread(file = "./bin/Final Datasets/LEMO_WorkOrder_LCS.csv", sep=",", header = TRUE)
LEMO_LCS.df[LEMO_LCS.df==""]=NA
LEMO_LCS.df=LEMO_LCS.df[,c("Work Order No", "Workdate", "Activity", "Activity Description", "Dist", "beginCounty", "endCounty", "rID", "rSuffix", 
                           "Hours.sum", "DB_ID", "coverage", "Length", "Status", "Work Type", "Duration", "Cozeep Mazeep", "Has Detour Map", "Type",
                           "Facility", "Closure Lanes", "Total Lanes")]

#reduce begin and end county to one column
LEMO_LCS.df$beginCounty=county_abbr$ABBREV.[match(LEMO_LCS.df$beginCounty, county_abbr$`PEMS code`)]
LEMO_LCS.df$endCounty=county_abbr$ABBREV.[match(LEMO_LCS.df$endCounty, county_abbr$`PEMS code`)]
LEMO_LCS.df$beginCounty=ifelse(LEMO_LCS.df$beginCounty==LEMO_LCS.df$endCounty,
                               LEMO_LCS.df$beginCounty,
                               paste(LEMO_LCS.df$beginCounty, LEMO_LCS.df$endCounty, sep = "-"))
LEMO_LCS.df=LEMO_LCS.df[,-c("endCounty")]

#reduce rID and rSuffix to one column
LEMO_LCS.df$rID=ifelse(is.na(LEMO_LCS.df$rSuffix),
                       LEMO_LCS.df$rID,
                       paste0(LEMO_LCS.df$rID, LEMO_LCS.df$rSuffix, sep = ""))
LEMO_LCS.df$rID=gsub("SB", "S", LEMO_LCS.df$rID)
LEMO_LCS.df$rID=gsub("C", "", LEMO_LCS.df$rID)
LEMO_LCS.df=LEMO_LCS.df[,-c("rSuffix")]

#rename columns
colnames(LEMO_LCS.df)=c("wono", "work_date", "activity", "activity_descr", "district", "county", "route", "work_duration",
                        "closure_id", "closure_coverage", "closure_length", "closure_status", "closure_workType", "closure_duration", 
                        "closure_cozeepMazeep", "closure_detour", "closure_type", "closure_facility", "closure_lanes", "total_lanes")

fwrite(LEMO_LCS.df, file = "./bin/Final Datasets/LEMO_LCS.summary.csv", sep=",", append = FALSE)


LEMO.df=fread(file="./bin/Final Datasets/LEMO_WorkOrder+odom.csv", sep=",", header=TRUE)

#add work order length to LEMO.df
LEMO.df=cbind(LEMO.df, work_length=rowMeans(cbind(abs(LEMO.df$to.odom.R-LEMO.df$from.odom.R), abs(LEMO.df$to.odom.L-LEMO.df$from.odom.L)), na.rm = TRUE))
LEMO.df=cbind(LEMO.df, LEMO_ID=seq.int(nrow(LEMO.df)))

res=as.data.frame(matrix(NA, nrow = 0, ncol = 13))
colnames(res)=c("LEMO_ID", "closure_id", "closure_coverage", "closure_length", "closure_status", "closure_workType", "closure_duration", 
                "closure_cozeepMazeep", "closure_detour", "closure_type", "closure_facility", "closure_lanes", "total_lanes")

temp_LEMO=LEMO.df[,c("LEMO_ID", "Work Order No", "Workdate", "Activity")]
colnames(temp_LEMO)=c("LEMO_ID", "wono", "work_date", "activity")
temp_match=LEMO_LCS.df[,c("wono", "work_date", "activity", "closure_id", "closure_coverage", "closure_length", "closure_status", "closure_workType",
                          "closure_duration", "closure_cozeepMazeep", "closure_detour", "closure_type", "closure_facility", "closure_la^nes", "total_lanes")]
temp_match=temp_match %>% group_by(wono, work_date, activity) %>% slice(which(abs(closure_coverage)==max(abs(closure_coverage)))[1])

res=merge(temp_LEMO, temp_match, by=c("wono", "work_date", "activity"), all.x = TRUE)

fwrite(res, file = "./bin/LEMO_ID.match.closure.csv", sep=",", append = FALSE)
