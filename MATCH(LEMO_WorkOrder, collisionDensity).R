setwd("//ahmct-065/teams/PMRF/Amir")

library(data.table)
library(dplyr)
library(tidyr)

LEMO.df=fread(file = "./bin/Final Datasets/LEMO_WorkOrder+odom.csv", sep=",", header=TRUE)
LEMO.df[LEMO.df==""]=NA
LEMO.df=cbind(LEMO.df, LEMO_ID=seq.int(nrow(LEMO.df)))
temp_LEMO=LEMO.df[,c("LEMO_ID", "rID", "rSuffix", "beginCounty", "endCounty", "from.odom.R", 
                     "to.odom.R", "from.odom.L", "to.odom.L")]
temp_LEMO$from.odom.L=as.numeric(temp_LEMO$from.odom.L)
temp_LEMO$from.odom.R=as.numeric(temp_LEMO$from.odom.R)
temp_LEMO$to.odom.L=as.numeric(temp_LEMO$to.odom.L)
temp_LEMO$to.odom.R=as.numeric(temp_LEMO$to.odom.R)
rm(LEMO.df)

collision_density=fread(file = "./bin/Final Datasets/CHP_density_2011-2012.csv", sep=",", header = TRUE)
collision_density[collision_density==""]=NA
collision_density=setDT(collision_density)[order(route, bin_start, bin_end), ,]

res=data.frame(matrix(NA, nrow=0, ncol=2))
colnames(res)=c("LEMO_ID", "density")

for (i in 1:length(temp_LEMO$LEMO_ID)){
  route_id=temp_LEMO$rID[i]
  if (!is.na(temp_LEMO$rSuffix[i])){
    route_id=paste0(temp_LEMO$rID[i], temp_LEMO$rSuffix[i], sep="")
  }
  filtered_density=collision_density[which(collision_density$route==route_id),]
  if (dim(filtered_density)[1]==0){
    res=rbind(res, cbind.data.frame(LEMO_ID=temp_LEMO$LEMO_ID[i], density=0))
    next
  }
  
  ind_begin=NA
  ind_end=NA
  density=NA
  
  if (!is.na(temp_LEMO$from.odom.L[i]) & !is.na(temp_LEMO$to.odom.L[i])){
    ind_begin=findInterval(temp_LEMO$from.odom.L[i], filtered_density$bin_start, rightmost.closed = TRUE)
    ind_end=findInterval(temp_LEMO$to.odom.L[i], filtered_density$bin_start, rightmost.closed = TRUE)
    
    if (!is.na(ind_begin) & !is.na(ind_end)){
      density=filtered_density$freq[ind_begin:ind_end]
    }
  }
  
  if (!is.na(temp_LEMO$from.odom.R[i]) & !is.na(temp_LEMO$to.odom.R[i])){
    ind_begin=findInterval(temp_LEMO$from.odom.R[i], filtered_density$bin_start, rightmost.closed = TRUE)
    ind_end=findInterval(temp_LEMO$to.odom.R[i], filtered_density$bin_start, rightmost.closed = TRUE)
    
    if (!is.na(ind_begin) & !is.na(ind_end)){
      density=c(density, filtered_density$freq[ind_begin:ind_end])
    }
  }
  
  density=mean(density, na.rm = TRUE)
  res=rbind(res, cbind.data.frame(LEMO_ID=temp_LEMO$LEMO_ID[i], density=density))
  
  if(i%%100000==0){
    print(i)
    fwrite(res, file = "./bin/LEMO_ID.match.density_2011-2012.csv", sep=",", append = FALSE)
  }
}
fwrite(res, file = "./bin/LEMO_ID.match.density_2011-2012.csv", sep=",", append = FALSE)
