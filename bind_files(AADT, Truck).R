library(data.table)
library(tidyr)
library(dplyr)
#############################################################################################################
#############################################################################################################
########################################## Read AADT files ##################################################
setwd("//ahmct-065/teams/PMRF/Amir/Traffic Volume/AADT")
files_list=list.files(pattern="*.csv")
files=lapply(files_list, function(x) fread(x, sep=",", header = TRUE))

files[[1]]=files[[1]][-c(which(files[[1]]$DISTRICT=="Dist")),]
files[[1]]=cbind(files[[1]], Year=2013)
files[[2]]=cbind(files[[2]], Year=2014)
files[[3]]=cbind(files[[3]], Year=2015)
files[[4]]=cbind(files[[4]], Year=2016)
files[[5]]=cbind(files[[5]], Year=2017)
files[[6]]=cbind(files[[6]], Year=2018)

aadt.df=rbindlist(files, use.names = TRUE, fill=TRUE)
aadt.df=aadt.df[!duplicated(aadt.df)]
aadt.df[aadt.df==""]=NA
aadt.df=aadt.df[-c(which(aadt.df$DISTRICT=="Dist")),]
setwd("//ahmct-065/teams/PMRF/Amir/")
fwrite(aadt.df[,c("Year", "RTE", "RTE_SFX", "CNTY", "PM_PFX", "PM", "PM_SFX", "DESCRIPTION", "BACK_PEAK_HOUR", "BACK_PEAK_MADT", "BACK_AADT", 
                  "AHEAD_PEAK_HOUR", "AHEAD_PEAK_MADT", "AHEAD_AADT")], file="./bin/pm_aadt.csv", append=FALSE)
rm(files)
#############################################################################################################
#############################################################################################################
########################################### Read Truck AADT #################################################
setwd("//ahmct-065/teams/PMRF/Amir/Traffic Volume/Truck")
files_list=list.files(pattern="*sm.csv")

files=lapply(files_list, function(x) fread(x, sep=",", header=FALSE, skip=5, col.names = c("RTE", "DIST",
                                           "CNTY", "PM", "LEG", "DESCRIPTION", "AADT_TOTAL", "TRUCK_AADT")))
files[[1]]=cbind(files[[1]], Year=2013)
files[[2]]=cbind(files[[2]], Year=2014)
files[[3]]=cbind(files[[3]], Year=2015)
files[[4]]=cbind(files[[4]], Year=2016)
files[[5]]=cbind(files[[5]], Year=2017)
files[[6]]=cbind(files[[6]], Year=2018)

truck.df=rbindlist(files, use.names = TRUE, fill=TRUE)
truck.df$PM=gsub("([[:alpha:]]*)([0-9.]*)", "\\1 \\2", truck.df$PM)
truck.df=separate(truck.df, col="PM", into=c("PM_PFX", "PM"), 
                  sep=" ", remove=TRUE, convert = TRUE, extra="merge", fill = "left")

truck.df$RTE=gsub("([0-9]*)([[:alpha:]]*)", "\\1 \\2", truck.df$RTE)
truck.df=separate(truck.df, col="RTE", into=c("RTE", "RTE_SFX"), 
                  sep=" ", remove=TRUE, convert = TRUE, extra="merge", fill = "right")
truck.df$RTE_SFX[truck.df$RTE_SFX==""]=NA
fwrite(truck.df[,c("CNTY", "RTE", "RTE_SFX", "PM_PFX", "PM", "AADT_TOTAL", "TRUCK_AADT")], file="pm_truck.csv", append=FALSE)
