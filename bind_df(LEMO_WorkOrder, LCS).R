LEMO.merge.WorkOrder_LCS.print.func=function(LEMO.df, WorkOrder.df, LCS.df){
  library(data.table)
  library(tidyr)
  library(dplyr)
  #############################################################################################################
  #############################################################################################################
  ########################### Read files LEMO, WorkORder, and Lane Closure csv files ##########################
  #setwd("G:/My Drive/WorkingDesk/PM-to-ODOM")
  #LEMO.df=fread(file="LEMO-2013-01-01_02-01.csv", sep=',', header=TRUE)
  #WorkOrder.df=fread(file="WorkOrder-2013-01-01_02-01.csv", sep=',', header=TRUE)
  #LCS.df=fread(file="LCS-2013-01-01_02-01.csv",sep = ',', header=TRUE)
  county_abbr=fread(file="County_Abbr_PeMS.code.csv", sep=",", header=TRUE)

  ##############################################################################################################
  ##############################################################################################################
  ################################### clean up the LEMO data frame #############################################
  #date conversions
  LEMO.df$Workdate=as.Date(strptime(LEMO.df$Workdate, format="%d-%b-%Y"), format="%Y-%m-%d")
  LEMO.df$Compdttm=as.Date(strptime(LEMO.df$Compdttm, format="%d-%b-%Y"), format="%Y-%m-%d")

  #collapse the LEMO data set in order of Work order no, complete date and work date
  collapsed_LEMO.df=LEMO.df[order(`Work Order No`, as.Date(Compdttm), as.Date(Workdate)),,]
  #collapse the LEMO data set grouping by work order, complete date, take the sum of hours and costs
  collapsed_LEMO.df=collapsed_LEMO.df[order(`Work Order No`, as.Date(Workdate))
                                      , .(Compdttm=Compdttm
                                          , Hours.sum=sum(as.numeric(Hours))
                                          , Labors.sum=sum(as.numeric(Labor))
                                          , Equipment.sum=sum(as.numeric(Equipment))
                                          , Material.sum=sum(as.numeric(Material))
                                          , LEM.sum=sum(as.numeric(`LEM Total`))),
                                      by=.(`Work Order No`, Workdate, Dist, Region, Unit, Activity)]
  collapsed_LEMO.df=collapsed_LEMO.df[!is.na(collapsed_LEMO.df$`Work Order No`),]
  print("LEMO collapsed...")
  #collapse the LEMO data set by removing duplicates
  collapsed_LEMO.df=collapsed_LEMO.df[!duplicated(collapsed_LEMO.df)]
  #change the work order number to numeric type
  collapsed_LEMO.df$`Work Order No`=as.numeric(collapsed_LEMO.df$`Work Order No`)
  print("LEMO cleaned up...")
  ##############################################################################################################
  ##############################################################################################################
  ################################### clean up WorkOrder data frame ############################################
  #unify the column name for work order number
  colnames(WorkOrder.df)[1]='Work Order No'
  WorkOrder.df[WorkOrder.df=="" | WorkOrder.df==" "]=NA
  WorkOrder.df=WorkOrder.df[!is.na(WorkOrder.df$`Work Order No`),]
  WorkOrder.df$`Work Order No`=as.numeric(WorkOrder.df$`Work Order No`)
  WorkOrder.df=WorkOrder.df[!duplicated(WorkOrder.df)]
  print("WorkOrder cleaned up...")
  ################################################################################################################
  ################################################################################################################
  ##################################### merge LEMO with WorkOrder and clean up ###################################
  #merge LEMO and WorkOrder data set by Work Order No and Activity
  LEMO_WorkOrder.df=merge(collapsed_LEMO.df, WorkOrder.df, by = c("Work Order No", "Activity"), all.y = TRUE)
  print("LEMO merged with WorkOrder...")
  #Replace empty strings with NA for post mile columns 
  LEMO_WorkOrder.df$`From PM`[LEMO_WorkOrder.df$`From PM`==""]=NA
  LEMO_WorkOrder.df$`To PM`[LEMO_WorkOrder.df$`To PM`==""]=NA
  #remove rows that do not have post mile information
  LEMO_WorkOrder.df=na.omit(LEMO_WorkOrder.df, cols = c("From PM", "To PM", "IMMS Unit ID"), invert = FALSE)
  print("NAs removed...")
  #apparently, IMMS Unit ID refers to District-County-Route for some activities
  #separate district, county, and route into different columns
  LEMO_WorkOrder.df=separate(LEMO_WorkOrder.df, col="IMMS Unit ID", into=c("rDistrict", "rCounty", "rID"), 
                            sep="-", remove=TRUE, convert = TRUE)

  #NOTE: rCounty sometimes refers to two counties separated by '/' such as NEV/PLA
  #sepatate rCounty into begin and end county for those who are separated by '/'
  LEMO_WorkOrder.df=separate(LEMO_WorkOrder.df, col="rCounty", into=c("beginCounty", "endCounty"),
                            sep="/", remove=TRUE, extra="drop", fill="right")
  #if only one county is listed in the rCounty column, use it as the begin and the end county
  LEMO_WorkOrder.df$endCounty[is.na(LEMO_WorkOrder.df$endCounty)]=
    LEMO_WorkOrder.df$beginCounty[is.na(LEMO_WorkOrder.df$endCounty)]

  #NOTE: rID sometimes includes values such as '080A'
  #separate rID into a letter part and a number part with space as the separator
  LEMO_WorkOrder.df$rID=gsub("([0-9]*)([[:alpha:]]*)", "\\1 \\2", LEMO_WorkOrder.df$rID)
  #separate the rID column into two columns named rID (numeric), and rSuffix (string)
  LEMO_WorkOrder.df=separate(LEMO_WorkOrder.df, col="rID", into=c("rID", "rSuffix"), 
                            sep=" ", remove=TRUE, convert = TRUE, extra="merge")
  #replace empty strings in rSuffix with NA
  LEMO_WorkOrder.df$rSuffix[LEMO_WorkOrder.df$rSuffix==""]=NA

  #separate From PM column to a string part and numeric miles
  LEMO_WorkOrder.df=separate(LEMO_WorkOrder.df, col="From PM", into=c("fPMstr", "fPMmiles"),
                            sep=" ", remove=TRUE, convert = TRUE, extra = "merge")
  #split the string part to PM prefix and PM
  LEMO_WorkOrder.df$fPMstr=gsub("PM", " PM", LEMO_WorkOrder.df$fPMstr)
  #separate the prefix and the PM into separate columns
  LEMO_WorkOrder.df=separate(LEMO_WorkOrder.df, col="fPMstr", into=c("fPMprefix", "fPM"),
                            sep=" ", remove=TRUE, convert = TRUE, extra="merge", fill="left")
  #replace empty strings with NA
  LEMO_WorkOrder.df$fPMprefix[LEMO_WorkOrder.df$fPMprefix==""]=NA
  #NOTE: fPM sometimes include MILES instead of PM

  #separate To PM column to a string part and numeric miles (like above)
  LEMO_WorkOrder.df=separate(LEMO_WorkOrder.df, col="To PM", into=c("tPMstr", "tPMmiles"),
                            sep=" ", remove=TRUE, convert = TRUE, extra="merge")
  LEMO_WorkOrder.df$tPMstr=gsub("PM", " PM", LEMO_WorkOrder.df$tPMstr)
  LEMO_WorkOrder.df=separate(LEMO_WorkOrder.df, col="tPMstr", into=c("tPMprefix", "tPM"),
                            sep=" ", remove=TRUE, convert = TRUE, extra="merge", fill="left")
  LEMO_WorkOrder.df$tPMprefix[LEMO_WorkOrder.df$tPMprefix==""]=NA
  #NOTE: fPM sometimes include MILES instead of PM
  #NOTE: rSuffix sometimes includes A, SA, and UA. Only U and S values are valid. Assuming SA=S, UA=U, and A=NA
  LEMO_WorkOrder.df$rSuffix[LEMO_WorkOrder.df$rSuffix=="A" | LEMO_WorkOrder.df$rSuffix=="B"]=NA
  LEMO_WorkOrder.df$rSuffix[LEMO_WorkOrder.df$rSuffix=="SA"]="S"
  LEMO_WorkOrder.df$rSuffix[LEMO_WorkOrder.df$rSuffix=="UA"]="U"
  print("LEMO_WorkOrder parsed counties, routes, route suffix, postmile prefix, and postmiles...")
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  #print from postmile and to postmile information to .csv file for use in the pm_odometer_query.py
  #make a copy to rename the columns
  df=LEMO_WorkOrder.df[,c("beginCounty", "rID", "rSuffix", "fPMprefix", "fPMmiles")]
  colnames(df)=c("County", "rID", "rSuffix", "PMprefix", "PM")
  fwrite(df, file="./bin/fromPM.csv", sep=",", append=FALSE)

  df=LEMO_WorkOrder.df[,c("endCounty", "rID", "rSuffix", "tPMprefix", "tPMmiles")]
  colnames(df)=c("County", "rID", "rSuffix", "PMprefix", "PM")
  fwrite(df, file = "./bin/toPM.csv", sep=",", append=FALSE)

  print("LEMO_WorkOrder toPM and fromPM files printed...")
  #####################################################################################################################
  ##############change abbreviations for county into codes used by the PeMS Lane Closure System in LCS.df##############
  LEMO_WorkOrder.df$beginCounty=county_abbr$`PEMS code`[match(LEMO_WorkOrder.df$beginCounty, county_abbr$ABBREV.)]
  LEMO_WorkOrder.df$endCounty=county_abbr$`PEMS code`[match(LEMO_WorkOrder.df$endCounty, county_abbr$ABBREV.)]
  print("LEMO_WorkOrder county abbrev. converted to PeMS codes...")
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  ########################################### clean up the Lane Closure file ##########################################
  #replace empty strings with NA
  LCS.df[LCS.df==""]=NA
  print("LCS cleaned up...")
  #separate the Fwy-Dir into an ID part and a letter part corresponding to the direction of traffic
  LCS.df=separate(LCS.df, col="Fwy-Dir", into=c("FwyID", "FwyDir"),
                  sep="-", remove = TRUE, convert = TRUE, extra="drop")
  #separate the FwyID inot a letter part corresponding to the route ID (rID in LEMO_WorkOrder.df) and a letter part
  LCS.df$FwyID=gsub("([[:alpha:]]*)([0-9]*)", "\\1 \\2", LCS.df$FwyID)
  #separate the FwyID column into two columns reflecting the above separation
  LCS.df=separate(LCS.df, col="FwyID", into=c("FwyName", "FwyID"),
                  sep=" ", remove = TRUE, convert = TRUE, extra="drop")
  print("LCS parsed route name, route ID, and alignment...")

  #separate date and time columns for Start Date, End Data, Status 1097 Date, Status 1098 Date, and Status 1022 Date
  LCS.df=separate(LCS.df, col="Start Date", into=c("StartDate", "StartTime"),
                  sep=" ", remove=TRUE, convert = TRUE, extra = "drop")
  LCS.df=separate(LCS.df, col="End Date", into=c("EndDate", "EndTime"),
                  sep=" ", remove=TRUE, convert = TRUE, extra = "drop")
  LCS.df=separate(LCS.df, col="Status 1097 Date", into=c("Date1097", "Time1097"),
                  sep=" ", remove=TRUE, convert = TRUE, extra = "drop")
  LCS.df=separate(LCS.df, col="Status 1098 Date", into=c("Date1098", "Time1098"),
                  sep=" ", remove=TRUE, convert = TRUE, extra = "drop")
  LCS.df=separate(LCS.df, col="Status 1022 Date", into=c("Date1022", "Time1022"),
                  sep=" ", remove=TRUE, convert = TRUE, extra = "drop")

  print("LCS parsed dates...")
  #substitute FwyDir with postmile suffix values, R for E and N, L for S and W
  LCS.df$FwyDir=gsub("N", "R", LCS.df$FwyDir)
  LCS.df$FwyDir=gsub("E", "R", LCS.df$FwyDir)
  LCS.df$FwyDir=gsub("S", "L", LCS.df$FwyDir)
  LCS.df$FwyDir=gsub("W", "L", LCS.df$FwyDir)
  #####################################################################################################################
  #####################################################################################################################
  #####################################################################################################################
  df=LCS.df[which(LCS.df$Status=="Approved"),]
  df=df[which(!is.na(df$`Begin County`)),]
  df=df[which(!is.na(df$`End County`)),]
  df=df[which(!is.na(df$FwyID)),]
  
  df=df[,c("DB ID", "Begin County", "FwyID", "Begin State PM")]
  df$`Begin County`=county_abbr$ABBREV.[match(df$`Begin County`, county_abbr$`PEMS code`)]
  colnames(df)=c("ID", "County", "rID", "PM")
  fwrite(df, file="./bin/BeginLane.csv", sep=",", append=FALSE)
  
  df=LCS.df[which(LCS.df$Status=="Approved"),]
  df=df[which(!is.na(df$`Begin County`)),]
  df=df[which(!is.na(df$`End County`)),]
  df=df[which(!is.na(df$FwyID)),]
  
  df=df[,c("DB ID", "End County", "FwyID", "End State PM")]
  df$`End County`=county_abbr$ABBREV.[match(df$`End County`, county_abbr$`PEMS code`)]
  colnames(df)=c("ID", "County", "rID", "PM")
  fwrite(df, file="./bin/EndLane.csv", sep=",", append=FALSE)

  print("LCS BeginLane and EndLane printed")
  #####################################################################################################################
  ################################# convert caltrans postmiles to absolute ############################################
  ##define a function for evaluating absolute begin and end postmiles
  #abs_PM.func=function(PM, route_ID, county){
  #  county_PM.df=fread(file="Highway_countyPM.csv", sep=",", header=TRUE)
  #  odometer_countyPM=fread(file="Odometer_countyPM.csv", sep=",", header=TRUE)
  #  #set initial absolute postmile value to caltrans postmile
  #  abs_PM=as.numeric(PM)
  #  #filter the county_PM.df by route ID
  #  temp_county_PM.df=county_PM.df[which(county_PM.df$THY_ROUTE_NAME==as.numeric(route_ID)),]
  #  if(dim(temp_county_PM.df)[1]>1){#if the route passes multiple counties
  #    #identify the order of the county of interest in the route sequence of counties
  #    order_of_beginCounty=which(temp_county_PM.df$THY_COUNTY_CODE==as.numeric(county))
  #    if (order_of_beginCounty > 1){#if the county of interest is not the first county
  #      #absolute postmiles is the value of caltrans postmile plus the lenght of the route in all previous counties
  #      abs_PM=abs_PM+sum(temp_county_PM.df$county_enPM[1:(order_of_beginCounty-1)]) 
  #    }
  #  }
  #  return(abs_PM)
  #}
  ##add the absolute begin postmile value to the data set
  #LEMO_WorkOrder.df=as.data.frame(cbind(LEMO_WorkOrder.df, 
                                  #absBeginPM=apply(LEMO_WorkOrder.df, 1, 
                                                  #function(x) abs_PM.func(x["fPMmiles"], x["rID"], x["beginCounty"]))))
  ##add the absolute end postmile value to the data set
  #LEMO_WorkOrder.df=as.data.frame(cbind(LEMO_WorkOrder.df, 
                                  #absEndPM=apply(LEMO_WorkOrder.df, 1, 
                                                  #function(x) abs_PM.func(x["tPMmiles"], x["rID"], x["endCounty"]))))
  return(list("LEMO_WorkOrder.df"=LEMO_WorkOrder.df, "LCS.df"=LCS.df))
}