library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("//ahmct-065/teams/PMRF/Amir/CHP/All collision data")
files_lst=list.files(pattern="*.csv")
files=lapply(files_lst, fread)
df=rbindlist(files, use.names = TRUE, fill = TRUE)
df=df[which(df$STATE_HWY_IND=="Y"),]
setwd("//ahmct-065/teams/PMRF/Amir/bin/Final Datasets")
fwrite(df, file="SWITRS.csv", sep=",", append=FALSE)
