library(data.table)
setwd("//ahmct-065/teams/PMRF/Amir/PeMS/Lane Closure System/")
for(i in 2013:2018){
    file.names=list.files(path=paste("./", i, "/.csv files", sep=""), pattern="*.csv")
    files=lapply(file.names, function(x) fread(file=paste("./", i, "/.csv files/", x, sep=""), header=TRUE))
    df=rbindlist(files, use.names=TRUE, fill=TRUE)
    fwrite(df, file=paste("./LCS-", i, ".csv", sep=""), sep=",", append=FALSE)    
}
file.names=list.files(pattern="*.csv")
files=lapply(file.names[-7], function(x) fread(file=x, header=TRUE))
df=rbindlist(files, use.names = TRUE, fill=TRUE)
fwrite(df, file="LCS-2013_2018.csv", append = FALSE, sep=',')
