library(data.table)
setwd("//ahmct-065/teams/PMRF/Amir/WorkOrder")
files_lst=list.files(pattern="*.csv")
files=lapply(files_lst, fread)
df=rbindlist(files, use.names = TRUE, fill = TRUE)
fwrite(df, file="WorkOrder-2013_2018.csv", sep=",", append=FALSE)