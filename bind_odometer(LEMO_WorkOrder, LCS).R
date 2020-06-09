LEMO_WorkOrder_LCS.bind.odom.func=function(LEMO_WorkOrder.df, LCS.df, 
                                       county_abbr){
  setwd("//ahmct-065/teams/PMRF/Amir")
  library(data.table)
  #read odometer values for LEMO_WorkOrder and LCS data sets
  LEMO_from.df=fread("./bin/LEMO-from.csv", sep=",", header=TRUE)
  LEMO_from.df[LEMO_from.df==""]=NA
  LEMO_to.df=fread("./bin/LEMO-to.csv", sep=",", header=TRUE)
  LEMO_to.df[LEMO_to.df==""]=NA
  LCS_from.df=fread("./bin/LCS-begin.csv", sep=",", header=TRUE)
  LCS_from.df[LCS_from.df==""]=NA
  LCS_to.df=fread("./bin/LCS-end.csv", sep=",", header=TRUE)
  LCS_to.df[LCS_to.df==""]=NA
  LCS.df=LCS.df[which(LCS.df$Status=="Approved"),]
  LCS.df=LCS.df[which(!is.na(LCS.df$`Begin County`)),]
  LCS.df=LCS.df[which(!is.na(LCS.df$`End County`)),]
  LCS.df=LCS.df[which(!is.na(LCS.df$FwyID)),]
  print("Odometer values read...")
  
  #rename columns for correct identification when matching
  col.num=dim(LEMO_from.df)[2]
  colnames(LEMO_from.df)[(col.num-1):col.num]=c("from.odom.R", "from.odom.L")
  colnames(LEMO_to.df)[(col.num-1):col.num]=c("to.odom.R", "to.odom.L")
  col.num=dim(LCS_from.df)[2]
  colnames(LCS_from.df)[(col.num-1):col.num]=c("begin.odom.R", "begin.odom.L")
  colnames(LCS_to.df)[(col.num-1):col.num]=c("end.odom.R", "end.odom.L")
  print("column names are changed...")
  
  #change county abbreviations to PeMS county codes
  LEMO_from.df$County=county_abbr$`PEMS code`[match(LEMO_from.df$County, county_abbr$ABBREV.)]
  LEMO_to.df$County=county_abbr$`PEMS code`[match(LEMO_to.df$County, county_abbr$ABBREV.)]
  LCS_from.df$County=county_abbr$`PEMS code`[match(LCS_from.df$County, county_abbr$ABBREV.)]
  LCS_to.df$County=county_abbr$`PEMS code`[match(LCS_to.df$County, county_abbr$ABBREV.)]
  print("county abbreviation changes to PeMS code...")
  
  #bind odom values with LEMO and LCS data sets
  LEMO_WorkOrder.df=cbind.data.frame(LEMO_WorkOrder.df, from.odom.R=LEMO_from.df$from.odom.R, 
                                     from.odom.L=LEMO_from.df$from.odom.L,
                                     to.odom.R=LEMO_to.df$to.odom.R,
                                     to.odom.L=LEMO_to.df$to.odom.L)
  LCS.df=cbind.data.frame(LCS.df, begin.odom.R=LCS_from.df$begin.odom.R,
                          begin.odom.L=LCS_from.df$begin.odom.L,
                          end.odom.R=LCS_to.df$end.odom.R,
                          end.odom.L=LCS_to.df$end.odom.L)
  print("bind odom with LEMO_WorkOrder and LCS...")
  return(list("LEMO_WorkOrder.df"=LEMO_WorkOrder.df,
              "LCS.df"=LCS.df))
}
