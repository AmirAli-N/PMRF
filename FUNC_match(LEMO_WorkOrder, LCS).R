LCS_timeInterval.func=function(start_date, end_date, conePlace_date, conePick_date, cancel_date, ID){
  library(lubridate)
  library(RQuantLib)
  library(anytime)
  int_start.date=start_date #set initial value of the start of interval to requested start date
  int_end.date=end_date #set initial value of the end of interval to predicted end date
  
  #if first cone placement date is provided update the start date is equal to cone place date
  int_start.date[which(!is.na(conePlace_date) &
                       as.Date(anydate(conePlace_date))>=as.Date("2013-01-01") &
                       as.Date(anydate(conePlace_date))<=as.Date("2019-01-01"))]=conePlace_date[which(!is.na(conePlace_date) &
                                                                                                      as.Date(anydate(conePlace_date))>=as.Date("2013-01-01") &
                                                                                                      as.Date(anydate(conePlace_date))<=as.Date("2019-01-01"))]
  #if last cone pick up is provide, uce cone pick up date instead of end date
  int_end.date[which(!is.na(conePick_date) &
                      as.Date(anydate(conePick_date))>=as.Date("2013-01-01") &
                      as.Date(anydate(conePick_date))<=as.Date("2019-01-01"))]=conePick_date[which(!is.na(conePick_date) &
                                                                                                   as.Date(anydate(conePick_date))>=as.Date("2013-01-01") &
                                                                                                   as.Date(anydate(conePick_date))<=as.Date("2019-01-01"))]
  ##if cancel date is provided, use cancel date instead of end date
  int_end.date[which(!is.na(cancel_date) &
                     as.Date(anydate(cancel_date))>=as.Date("2013-01-01") &
                     as.Date(anydate(cancel_date))<=as.Date("2019-01-01"))]=cancel_date[which(!is.na(cancel_date) &
                                                                                              as.Date(anydate(cancel_date))>=as.Date("2013-01-01") &
                                                                                              as.Date(anydate(cancel_date))<=as.Date("2019-01-01"))]
  ##evaluate start and end date by considering a tolerance for business days
  ##advance of RQuantLib moves the date by tolera-ce considering only business days
  #tol_start=as.character(advance(calendar = "UnitedStates", dates=as.Date(int_start.date), -tol, 0))
  #tol_end=as.character(advance(calendar = "UnitedStates", dates=as.Date(int_end.date), tol, 0))
  #return(as.data.frame(cbind(true_start=int_start.date, true_end=int_end.date,
  #                           tol_start=tol_start, tol_end=tol_end)))
  return(as.data.frame(cbind(true_start=int_start.date, true_end=int_end.date)))
}

filter_LEMO.func=function(LEMO_WorkOrder.df){
  library(anytime)
  #filter LEMO_WorkOrder.df for empty work order numbers
                              # not convertible to date format workdates
                              # non numeric route numbers
                              # non numeric odometers, and not having start and end odometer for each alignment
  LEMO_WorkOrder.df=LEMO_WorkOrder.df[which(!is.na(LEMO_WorkOrder.df$`Work Order No`)),]
  LEMO_WorkOrder.df=LEMO_WorkOrder.df[which(!is.na(anydate(LEMO_WorkOrder.df$Workdate, calcUnique = TRUE))),]
  LEMO_WorkOrder.df=LEMO_WorkOrder.df[which(!is.na(as.numeric(LEMO_WorkOrder.df$rID))),]
  LEMO_WorkOrder.df=LEMO_WorkOrder.df[which((!is.na(as.numeric(LEMO_WorkOrder.df$from.odom.R)) &
                                             !is.na(as.numeric(LEMO_WorkOrder.df$to.odom.R))) |
                                            (!is.na(as.numeric(LEMO_WorkOrder.df$from.odom.L)) &
                                             !is.na(as.numeric(LEMO_WorkOrder.df$to.odom.L)))),]
  return(LEMO_WorkOrder.df)
}

filter_LCS.func=function(LCS.df){
  library(anytime)
  #filter LCS.df for empty closure ID
                    #not convertible to date format start and end dates
                    #non numeric freeway ID
                    #non numeric odometers, and not having start and end odometer for each alignment
  LCS.df=LCS.df[which(!is.na(LCS.df$`DB ID`)),]
  LCS.df=LCS.df[which(!is.na(anydate(LCS.df$true_start, calcUnique = TRUE)) & !is.na(anydate(LCS.df$true_end, calcUnique = TRUE))),]
  LCS.df=LCS.df[which(!is.na(as.numeric(LCS.df$FwyID))),]
  LCS.df=LCS.df[which((!is.na(as.numeric(LCS.df$begin.odom.R)) & 
                       !is.na(as.numeric(LCS.df$end.odom.R))) | 
                      (!is.na(as.numeric(LCS.df$begin.odom.L)) &
                       !is.na(as.numeric(LCS.df$end.odom.L)))),]
  
  return(LCS.df)
}

filter_closureDate.func=function(work_date, closure.df){
  library(lubridate)
  library(RQuantLib)
  library(anytime)
  #filter the closure data for work_dates that match the date range of a closure
  closure.df=closure.df[which(as.Date(anydate(work_date))%within%
                                interval(as.Date(anydate(closure.df$true_start)), as.Date(anydate(closure.df$true_end)))),]
  return(closure.df)
}

##############new funtion to match odometer values
##############does not match postmiles
##############should add later

LCS_matchPM.func=function(workOrder.df, closure.df){
  
  #divide closure.df to two data.frames with right and left alignment odometer
  closure_R.df=closure.df[which(!is.na(closure.df$begin.odom.R) & !is.na(closure.df$end.odom.R) & closure.df$FwyDir=="R"),]
  closure_L.df=closure.df[which(!is.na(closure.df$begin.odom.L) & !is.na(closure.df$end.odom.L) & closure.df$FwyDir=="L"),]
  
  #check for odometer intersection in the right alignment
  if (!is.na(workOrder.df$from.odom.R) & !is.na(workOrder.df$to.odom.R)){
    closure_R.df=closure_R.df[-c(which(
      (workOrder.df$from.odom.R > closure_R.df$end.odom.R) |
        (closure_R.df$begin.odom.R > workOrder.df$to.odom.R)
    )),]
    #remove the left alignment odometers, and assign a generic column name for odometer values
    closure_R.df=closure_R.df[,-c("begin.odom.L", "end.odom.L")]
    colnames(closure_R.df)=c("DB ID", "true_start", "true_end", "FwyID", "begin.odom", "end.odom", "FwyDir")
  } else{
    closure_R.df=closure_R.df[,-c("begin.odom.L", "end.odom.L")]
    colnames(closure_R.df)=c("DB ID", "true_start", "true_end", "FwyID", "begin.odom", "end.odom", "FwyDir")
    closure_R.df=closure_R.df[0,]
  }
  
  
  #check for odometer intersection in the left alignment
  if(!is.na(workOrder.df$from.odom.L) & !is.na(workOrder.df$to.odom.L)){
    closure_L.df=closure_L.df[-c(which(
      (workOrder.df$from.odom.L > closure_L.df$end.odom.L) |
        (closure_L.df$begin.odom.L > workOrder.df$to.odom.L)
    )),]
    #remove the right alignment odometers, and assign a generic column name for odometer values
    closure_L.df=closure_L.df[,-c("begin.odom.L", "end.odom.L")]
    colnames(closure_L.df)=c("DB ID", "true_start", "true_end", "FwyID", "begin.odom", "end.odom", "FwyDir")
  } else{
    closure_L.df=closure_L.df[,-c("begin.odom.L", "end.odom.L")]
    colnames(closure_L.df)=c("DB ID", "true_start", "true_end", "FwyID", "begin.odom", "end.odom", "FwyDir")
    closure_L.df=closure_L.df[0,]
  }
  
  
  #if no intersection is found
  if (dim(closure_R.df)[1]==0 & dim(closure_L.df)[1]==0){
    return(data.table(cbind.data.frame("WONo"=workOrder.df$`Work Order No`,
                                       "work_date"=workOrder.df$Workdate,
                                       "DB_ID"=NA,
                                       "coverage"=NA)))
  } else{
    #combine left and right alignment matching closures
    closure.df=rbind.data.frame(closure_R.df, closure_L.df)
  
    #the following column name generation is neccessary because every work order has two sets of odometer for each alignment
    
    #generate workOrder from.odom.<alignment> column names corresponding with closure alignments
    #evaluate the begininig of the matching closure intersection
    from.name=paste("from.odom.", closure.df$FwyDir, sep="")
    begin.intersection=unlist(lapply(1:length(from.name), function(x) max(closure.df$begin.odom[x], workOrder.df[[from.name[x]]])))
    
    #generate workOrder to.odom.<alignment> column names corresponding with closure alignments
    #evaluate the begininig of the matching closure intersection
    to.name=paste("to.odom.", closure.df$FwyDir, sep="")
    end.intersection=unlist(lapply(1:length(to.name), function(x) min(closure.df$end.odom[x], workOrder.df[[to.name[x]]])))
    
    #if the length of workorder is zero, coverage is 100 percent
    if ((!is.na(workOrder.df$from.odom.R) & !is.na(workOrder.df$to.odom.R) & workOrder.df$from.odom.R-workOrder.df$to.odom.R==0) | 
        (!is.na(workOrder.df$from.odom.L) & !is.na(workOrder.df$to.odom.L) & workOrder.df$from.odom.L-workOrder.df$to.odom.L==0)){
      return(cbind.data.frame("WONo"=workOrder.df$`Work Order No`,
                              "work_date"=workOrder.df$Workdate,
                              "DB_ID"=closure.df$`DB ID`,
                              "coverage"=1))
    } else{
      #we do not know the alignment of workorder, so the coverage is the average of coverage for left and right alignment
      coverage_R=(end.intersection-begin.intersection)/(workOrder.df$to.odom.R-workOrder.df$from.odom.R)
      coverage_L=(end.intersection-begin.intersection)/(workOrder.df$to.odom.L-workOrder.df$from.odom.L)
      return(cbind.data.frame("WONo"=workOrder.df$`Work Order No`,
                              "work_date"=workOrder.df$Workdate,
                              "DB_ID"=closure.df$`DB ID`,
                              "coverage"=rowMeans(cbind(coverage_R, coverage_L), na.rm=TRUE)))
    }
  }
}

##############old function mathcing odometers, and postmiles
##############very slow!!

# LCS_matchPM.func=function(WONo, work_date, beginCounty, endCounty, route_ID, begin_PM, end_PM,
#                           from.odom_R, from.odom_L, to.odom_R, to.odom_L, Lane_Closure.df, tol, match.type){

#   naodom_Lane_Closure.df=Lane_Closure.df[c(which(is.na(Lane_Closure.df$begin.odom.R) | is.na(Lane_Closure.df$begin.odom.L) |
#                                                  is.na(Lane_Closure.df$end.odom.R) | is.na(Lane_Closure.df$end.odom.L))),]
# 	Lane_Closure.df=Lane_Closure.df[-c(which(is.na(Lane_Closure.df$begin.odom.R) | is.na(Lane_Closure.df$begin.odom.L) |
# 	                                          is.na(Lane_Closure.df$end.odom.R) | is.na(Lane_Closure.df$end.odom.L))),]
# 	
# 	if (!is.na(from.odom_R) & !is.na(from.odom_L) & !is.na(to.odom_R) & !is.na(to.odom_L)){
# 	  
#         Lane_Closure_R.df=Lane_Closure.df[-c(which(
# 												(from.odom_R > Lane_Closure.df$end.odom.R) |
# 												(Lane_Closure.df$begin.odom.R > to.odom_R)
# 											  )),]
#         Lane_Closure_L.df=Lane_Closure.df[-c(which(
# 												(from.odom_L > Lane_Closure.df$end.odom.L) |
# 												(Lane_Closure.df$begin.odom.L > to.odom_L)
# 											  )),]
#       
#         if ((dim(Lane_Closure_R.df)[1]==0) & (dim(Lane_Closure_L.df)[1]==0)){
#           return.df=as.data.frame(cbind.data.frame("WONo"=WONo, "workDate"=work_date, 
#                                                    "matchType"=paste("No", match.type, sep=" ")))
#         }
#         
#         if ((dim(Lane_Closure_R.df)[1]==0) & (dim(Lane_Closure_L.df)[1]!=0)){
#           covered_length_L=NA
#           if (from.odom_L-to.odom_L==0){
#             covered_length_L=0
#           } else{
#             begin_intersection_L=pmax(Lane_Closure_L.df$begin.odom.L, from.odom_L)
#             end_intersection_L=pmin(Lane_Closure_L.df$end.odom.L, to.odom_L)
#             covered_length_L=(end_intersection_L-begin_intersection_L)/(from.odom_L-to.odom_L)   
#           }
#           return.df=as.data.frame(cbind.data.frame("WONo"=WONo, "workDate"=work_date, "matchType"=match.type, 
#                                                 "alignment"="L", "coveredLength"=covered_length_L, Lane_Closure_L.df))
#         }
#         
#         if ((dim(Lane_Closure_L.df)[1]==0) & (dim(Lane_Closure_R.df)[1]!=0)){
#           covered_length_R=NA
#           if (from.odom_R-to.odom_R==0){ 
#             covered_length_R=0
#           } else{
#             end_intersection_R=pmin(Lane_Closure_R.df$end.odom.R, to.odom_R)
#             begin_intersection_R=pmax(Lane_Closure_R.df$begin.odom.R, from.odom_R) #assume for now it is a vector
#             covered_length_R=(end_intersection_R-begin_intersection_R)/(from.odom_R-to.odom_R)
#           }
#           return.df=as.data.frame(cbind.data.frame("WONo"=WONo, "workDate"=work_date, "matchType"=match.type, 
#                                                 "alignment"="R", "coveredLength"=covered_length_R, 
#                                                 Lane_Closure_R.df))
#         }
#         
#         if ((dim(Lane_Closure_L.df)[1]!=0) & (dim(Lane_Closure_R.df)[1]!=0)){
#           covered_length_R=NA
#           covered_length_L=NA
#           if (from.odom_R-to.odom_R==0){ 
#             covered_length_R=0
#           } else{
#             end_intersection_R=pmin(Lane_Closure_R.df$end.odom.R, to.odom_R)
#             begin_intersection_R=pmax(Lane_Closure_R.df$begin.odom.R, from.odom_R) #assume for now it is a vector
#             covered_length_R=(end_intersection_R-begin_intersection_R)/(from.odom_R-to.odom_R)
#           }
#           if (from.odom_L-to.odom_L==0){
#             covered_length_L=0
#           } else{
#             begin_intersection_L=pmax(Lane_Closure_L.df$begin.odom.L, from.odom_L)
#             end_intersection_L=pmin(Lane_Closure_L.df$end.odom.L, to.odom_L)
#             covered_length_L=(end_intersection_L-begin_intersection_L)/(from.odom_L-to.odom_L)   
#           }
#           return.df=as.data.frame(cbind.data.frame("WONo"=WONo, "workDate"=work_date, "matchType"=match.type, 
#                                                 rbind.data.frame(cbind.data.frame("alignment"="R", "coveredLength"=covered_length_R, Lane_Closure_R.df), 
#                                                                  cbind.data.frame("alignment"="L", "coveredLength"=covered_length_L, Lane_Closure_L.df))
#           ))
#         }
#   } else {
#         Lane_Closure.df=Lane_Closure.df[-c(which(
#                                             (Lane_Closure.df$`Begin County`!=beginCounty) & (Lane_Closure.df$`End County`!=endCounty)
#                                           )),]
#         if (dim(Lane_Closure.df)[1]==0){
#             return.df=as.data.frame(cbind.data.frame("WONo"=WONo, "workDate"=work_date, 
#                                             "matchType"=paste("No", match.type, sep=" ")))
#         } else{
#             Lane_Closure.df=Lane_Closure.df[-c(which(
#                                                         (begin_PM > Lane_Closure.df$`End State PM`) &
#                                                         (Lane_Closure.df$`Begin State PM` > end_PM) 
#                                                )),]
#             if (dim(Lane_Closure.df)[1]==0){
#                 return.df=as.data.frame(cbind.data.frame("WONo"=WONo, "workDate"=work_date, 
#                                             "matchType"=paste("No", match.type, sep=" ")))
#             } else{
#                 begin_intersection_PM=pmax(Lane_Closure.df$`Begin State PM`, begin_PM) #assume for now it is a vector
#                 end_intersection_PM=pmin(Lane_Closure.df$`End State PM`, end_PM)
#                 covered_length=0
#                 if (end_PM-begin_PM==0){
#                     covered_length=0
#                 } else{
#                     covered_length=(end_intersection_PM-begin_intersection_PM)/(end_PM-begin_PM)
#                 }
#                 return.df=as.data.frame(cbind.data.frame("WONo"=WONo, "workDate"=work_date, "matchType"=match.type, 
#                         "alignment"=NA, "coveredLength"=covered_length, Lane_Closure.df))
#             }
#         }
#       }
# 	if(dim(naodom_Lane_Closure.df)[1]!=0){
# 	  naodom_Lane_Closure.df=naodom_Lane_Closure.df[-c(which(
# 	    (naodom_Lane_Closure.df$`Begin County`!=beginCounty) & (naodom_Lane_Closure.df$`End County`!=endCounty)
# 	  )),]
# 	  if (dim(naodom_Lane_Closure.df)[1]==0){
# 	    if (return.df$matchType=="No exact/partial match"){
# 	      return.df=as.data.frame(cbind.data.frame("WONo"=WONo, "workDate"=work_date, 
# 	                                               "matchType"=paste("No", match.type, sep=" ")))
# 	    }
# 	  } else{
# 	    naodom_Lane_Closure.df=naodom_Lane_Closure.df[-c(which(
# 	      (begin_PM > naodom_Lane_Closure.df$`End State PM`) &
# 	      (naodom_Lane_Closure.df$`Begin State PM` > end_PM) 
# 	    )),]
# 	    if (dim(naodom_Lane_Closure.df)[1]==0){
# 	      if (return.df$matchType=="No exact/partial match"){
# 	        return.df=as.data.frame(cbind.data.frame("WONo"=WONo, "workDate"=work_date, 
# 	                                                 "matchType"=paste("No", match.type, sep=" ")))
# 	      }
# 	    } else{
# 	      begin_intersection_PM=pmax(naodom_Lane_Closure.df$`Begin State PM`, begin_PM) #assume for now it is a vector
# 	      end_intersection_PM=pmin(naodom_Lane_Closure.df$`End State PM`, end_PM)
# 	      covered_length=0
# 	      if (end_PM-begin_PM==0){
# 	        covered_length=0
# 	      } else{
# 	        covered_length=(end_intersection_PM-begin_intersection_PM)/(end_PM-begin_PM)
# 	      }
# 	      if (return.df$matchType=="No exact/partial match"){
# 	        return.df=as.data.frame(cbind.data.frame("WONo"=WONo, "workDate"=work_date, "matchType"=match.type, 
# 	                                                 "alignment"=NA, "coveredLength"=covered_length, naodom_Lane_Closure.df))
# 	      } else{
# 	        return.df=rbind.data.frame(return.df, cbind.data.frame("WONo"=WONo, "workDate"=work_date, "matchType"=match.type, 
# 	                                                               "alignment"=NA, "coveredLength"=covered_length, naodom_Lane_Closure.df))
# 	      }
# 	    }
# 	  }
# 	}
# 	return(return.df)
# }