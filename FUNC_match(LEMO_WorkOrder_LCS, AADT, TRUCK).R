##########################################################################################################
##########################################################################################################
####################################### join LEMO_WorkOrder/LCS with AADT ################################
AADT_match.func=function(beginCounty, beginPM, endCounty, endPM, 
                         route, rtsfx, from_odom.R, to_odom.R, from_odom.L, to_odom.L, temp_aadt){
  #filter the AADT data.frame for route
  route_seq.df=temp_aadt[which(temp_aadt$RTE==route),]
  
  #if the given route has a route suffix, filter for it
  if (!is.na(rtsfx)){
    route_seq.df=route_seq.df[which(route_seq.df$RTE_SFX==rtsfx),]
  }else{#if no suffix is given, filter rows that have nor route suffix
    route_seq.df=route_seq.df[is.na(route_seq.df$RTE_SFX),]
  }
  
  #filter route_seq.df with the right/left alingment for postmile suffix = alignment or NA and corresponding odometer
  route_seq_R.df=route_seq.df[which((route_seq.df$PM_PFX=="R" | is.na(route_seq.df$PM_PFX)) & !is.na(route_seq.df$Odometer_Right)),]
  route_seq_L.df=route_seq.df[which((route_seq.df$PM_PFX=="L" | is.na(route_seq.df$PM_PFX)) & !is.na(route_seq.df$Odometer_Left)),]
  #order the route_seq data.frames by odometer values
  #this is because of the find interval function which only works over non-decreasing sequences
  route_seq_R.df=route_seq_R.df[order(route_seq_R.df$Odometer_Right),]
  route_seq_L.df=route_seq_L.df[order(route_seq_L.df$Odometer_Left),]
  
  
  #introduce empty vectors for each feature of the AADT database
  back_peak_hour.R=c()
  back_peak_month.R=c()
  back_aadt.R=c()
  ahead_peak_hour.R=c()
  ahead_peak_month.R=c()
  ahead_aadt.R=c()
  
  back_peak_hour.L=c()
  back_peak_month.L=c()
  back_aadt.L=c()
  ahead_peak_hour.L=c()
  ahead_peak_month.L=c()
  ahead_aadt.L=c()
  
  #### check to see if any matching routes are found
  #### or to see if enough information is provided to find a matching
  #### e.g., if no odometer value is available, and one og postmile or county values are missing
  if (dim(route_seq.df)[1]==0 | 
      (is.na(from_odom.R) & (is.na(beginPM) | (is.na(beginCounty)))) |
      (is.na(from_odom.L) & (is.na(beginPM) | (is.na(beginCounty)))) | 
      (is.na(to_odom.R) & (is.na(endPM) | (is.na(endCounty)))) |
      (is.na(to_odom.L) & (is.na(endPM) | (is.na(endCounty))))
      ){
        return (#if no match was found, return NA
                cbind.data.frame("R.back_peak_hour"=NA, 
                             "R.back_peak_month"=NA,
                             "R.back_aadt"=NA,
                             "R.ahead_peak_hour"=NA,
                             "R.ahead_peak_month"=NA,
                             "R.ahead_aadt"=NA,
                             "L.back_peak_hour"=NA,
                             "L.back_peak_month"=NA,
                             "L.back_aadt"=NA,
                             "L.ahead_peak_hour"=NA,
                             "L.ahead_peak_month"=NA,
                             "L.ahead_aadt"=NA)
                )
  #### when some matching routes are found in AADT
  } else{
    begin.ind=NA #identifies the begining of a work order or closure in the sequence of AADTs reported
    end.ind=NA #identifies the end of a work order or closure in the sequence of AADTs reported
    
    #### first, the right alingment, pmsfx=R is matched 
    #### when odometer value is given for the begininig
    if (!is.na(from_odom.R)){
      #find the section of the road in AADT that contains the begininig of work order or closure
      begin.ind=findInterval(from_odom.R, route_seq_R.df$Odometer_Right, rightmost.closed = FALSE)
      ####some times the given odometer value is before the first interval
      ####return NAs for workorder or closures that are located before Caltrans's control
      if (begin.ind==0){
        # return (cbind.data.frame("R.back_peak_hour"=NA, 
        #                          "R.back_peak_month"=NA,
        #                          "R.back_aadt"=NA,
        #                          "R.ahead_peak_hour"=NA,
        #                          "R.ahead_peak_month"=NA,
        #                          "R.ahead_aadt"=NA,
        #                          "L.back_peak_hour"=NA,
        #                          "L.back_peak_month"=NA,
        #                          "L.back_aadt"=NA,
        #                          "L.ahead_peak_hour"=NA,
        #                          "L.ahead_peak_month"=NA,
        #                          "L.ahead_aadt"=NA)
        #         )
        return(cbind.data.frame("R.back_peak_hour"=toString(back_peak_hour.R), 
                                "R.back_peak_month"=toString(back_peak_month.R),
                                "R.back_aadt"=toString(back_aadt.R),
                                "R.ahead_peak_hour"=toString(ahead_peak_hour.R),
                                "R.ahead_peak_month"=toString(ahead_peak_month.R),
                                "R.ahead_aadt"=toString(ahead_aadt.R),
                                "L.back_peak_hour"=toString(back_peak_hour.L),
                                "L.back_peak_month"=toString(back_peak_month.L),
                                "L.back_aadt"=toString(back_aadt.L),
                                "L.ahead_peak_hour"=toString(ahead_peak_hour.L),
                                "L.ahead_peak_month"=toString(ahead_peak_month.L),
                                "L.ahead_aadt"=toString(ahead_aadt.L)))
      }
      ####some times the given odometer exactly matches one row in the AADT data frame
      ####for these case, return AADT traffic data as is
      if (from_odom.R==route_seq_R.df$Odometer_Right[begin.ind]){
        back_peak_hour.R=c(back_peak_hour.R, route_seq_R.df$BACK_PEAK_HOUR[begin.ind])
        back_peak_month.R=c(back_peak_month.R, route_seq_R.df$BACK_PEAK_MADT[begin.ind])
        back_aadt.R=c(back_aadt.R, route_seq_R.df$BACK_AADT[begin.ind])
        ahead_peak_hour.R=c(ahead_peak_hour.R, route_seq_R.df$AHEAD_PEAK_HOUR[begin.ind])
        ahead_peak_month.R=c(ahead_peak_month.R, route_seq_R.df$AHEAD_PEAK_MADT[begin.ind])
        ahead_aadt.R=c(ahead_aadt.R, route_seq_R.df$AHEAD_AADT[begin.ind])
      } else {
        ####for the majority that are somewhere in the interval, the back traffic is equal to the ahead traffic
        #####of the begining of the interval and vice versa
        back_peak_hour.R=c(back_peak_hour.R, route_seq_R.df$AHEAD_PEAK_HOUR[begin.ind])
        back_peak_month.R=c(back_peak_month.R, route_seq_R.df$AHEAD_PEAK_MADT[begin.ind])
        back_aadt.R=c(back_aadt.R, route_seq_R.df$AHEAD_AADT[begin.ind])
        ahead_peak_hour.R=c(ahead_peak_hour.R, route_seq_R.df$BACK_PEAK_HOUR[begin.ind+1])
        ahead_peak_month.R=c(ahead_peak_month.R, route_seq_R.df$BACK_PEAK_MADT[begin.ind+1])
        ahead_aadt.R=c(ahead_aadt.R, route_seq_R.df$BACK_AADT[begin.ind+1])
      }
    #### if given odometer values for workorders or closures are missing
    #### consider postmile informations
    } else{
      #### note that both begin county and begin PM are required
      if (!is.na(beginCounty) & !is.na(beginPM)){
        #filter route_seq_R for county and order it based on postmile values
        route_seqPM.df=route_seq_R.df[which(route_seq_R.df$CNTY==beginCounty),]
        route_seqPM.df=route_seqPM.df[order(route_seqPM.df$PM),]
        #find the section of the road in AADT that contains the begininig of work order or closure
        begin.ind=findInterval(beginPM, route_seqPM.df$PM, rightmost.closed = FALSE)
        ####some times the given odometer value is before the first interval
        ####return NAs for workorder or closures that are located before Caltrans's control
        if (begin.ind==0){
          # return (cbind.data.frame("R.back_peak_hour"=NA, 
          #                          "R.back_peak_month"=NA,
          #                          "R.back_aadt"=NA,
          #                          "R.ahead_peak_hour"=NA,
          #                          "R.ahead_peak_month"=NA,
          #                          "R.ahead_aadt"=NA,
          #                          "L.back_peak_hour"=NA,
          #                          "L.back_peak_month"=NA,
          #                          "L.back_aadt"=NA,
          #                          "L.ahead_peak_hour"=NA,
          #                          "L.ahead_peak_month"=NA,
          #                          "L.ahead_aadt"=NA))
          return(cbind.data.frame("R.back_peak_hour"=toString(back_peak_hour.R), 
                                  "R.back_peak_month"=toString(back_peak_month.R),
                                  "R.back_aadt"=toString(back_aadt.R),
                                  "R.ahead_peak_hour"=toString(ahead_peak_hour.R),
                                  "R.ahead_peak_month"=toString(ahead_peak_month.R),
                                  "R.ahead_aadt"=toString(ahead_aadt.R),
                                  "L.back_peak_hour"=toString(back_peak_hour.L),
                                  "L.back_peak_month"=toString(back_peak_month.L),
                                  "L.back_aadt"=toString(back_aadt.L),
                                  "L.ahead_peak_hour"=toString(ahead_peak_hour.L),
                                  "L.ahead_peak_month"=toString(ahead_peak_month.L),
                                  "L.ahead_aadt"=toString(ahead_aadt.L)))
        }
        ####some time the given odometer exactly matches one row in the AADT data frame
        ####for these case, return AADT traffic data as is
        if (beginPM==route_seqPM.df$PM[begin.ind]){
          back_peak_hour.R=c(back_peak_hour.R, route_seqPM.df$BACK_PEAK_HOUR[begin.ind])
          back_peak_month.R=c(back_peak_month.R, route_seqPM.df$BACK_PEAK_MADT[begin.ind])
          back_aadt.R=c(back_aadt.R, route_seqPM.df$BACK_AADT[begin.ind])
          ahead_peak_hour.R=c(ahead_peak_hour.R, route_seqPM.df$AHEAD_PEAK_HOUR[begin.ind])
          ahead_peak_month.R=c(ahead_peak_month.R, route_seqPM.df$AHEAD_PEAK_MADT[begin.ind])
          ahead_aadt.R=c(ahead_aadt.R, route_seqPM.df$AHEAD_AADT[begin.ind])
        } else {
          ####for the majority that are somewhere in the interval, the back traffic is equal to the ahead traffic
          #####of the begining of the interval and vice versa
          back_peak_hour.R=c(back_peak_hour.R, route_seqPM.df$AHEAD_PEAK_HOUR[begin.ind])
          back_peak_month.R=c(back_peak_month.R, route_seqPM.df$AHEAD_PEAK_MADT[begin.ind])
          back_aadt.R=c(back_aadt.R, route_seqPM.df$AHEAD_AADT[begin.ind])
          ahead_peak_hour.R=c(ahead_peak_hour.R, route_seqPM.df$BACK_PEAK_HOUR[begin.ind+1])
          ahead_peak_month.R=c(ahead_peak_month.R, route_seqPM.df$BACK_PEAK_MADT[begin.ind+1])
          ahead_aadt.R=c(ahead_aadt.R, route_seqPM.df$BACK_AADT[begin.ind+1])
        }
      }
    }

    #### when odometer value is given for the ending
    if(!is.na(to_odom.R)){
      ####find which section of the road contains the end of work order or closure
      end.ind=findInterval(to_odom.R, route_seq_R.df$Odometer_Right, rightmost.closed = FALSE)
      ####some times the given odometer value is before the first interval
      ####return NAs for workorder or closures that are located before Caltrans's control
      if (end.ind==0){
        # return (cbind.data.frame("R.back_peak_hour"=NA, 
        #                          "R.back_peak_month"=NA,
        #                          "R.back_aadt"=NA,
        #                          "R.ahead_peak_hour"=NA,
        #                          "R.ahead_peak_month"=NA,
        #                          "R.ahead_aadt"=NA,
        #                          "L.back_peak_hour"=NA,
        #                          "L.back_peak_month"=NA,
        #                          "L.back_aadt"=NA,
        #                          "L.ahead_peak_hour"=NA,
        #                          "L.ahead_peak_month"=NA,
        #                          "L.ahead_aadt"=NA))
        return(cbind.data.frame("R.back_peak_hour"=toString(back_peak_hour.R), 
                                "R.back_peak_month"=toString(back_peak_month.R),
                                "R.back_aadt"=toString(back_aadt.R),
                                "R.ahead_peak_hour"=toString(ahead_peak_hour.R),
                                "R.ahead_peak_month"=toString(ahead_peak_month.R),
                                "R.ahead_aadt"=toString(ahead_aadt.R),
                                "L.back_peak_hour"=toString(back_peak_hour.L),
                                "L.back_peak_month"=toString(back_peak_month.L),
                                "L.back_aadt"=toString(back_aadt.L),
                                "L.ahead_peak_hour"=toString(ahead_peak_hour.L),
                                "L.ahead_peak_month"=toString(ahead_peak_month.L),
                                "L.ahead_aadt"=toString(ahead_aadt.L)))
      }
      #### if end.ind >= begin.ind+1, return all the aadt information in between
      if (begin.ind < end.ind){
        for (i in (begin.ind+1):end.ind){
          back_peak_hour.R=c(back_peak_hour.R, route_seq_R.df$BACK_PEAK_HOUR[i])
          back_peak_month.R=c(back_peak_month.R, route_seq_R.df$BACK_PEAK_MADT[i])
          back_aadt.R=c(back_aadt.R, route_seq_R.df$BACK_AADT[i])
          ahead_peak_hour.R=c(ahead_peak_hour.R, route_seq_R.df$AHEAD_PEAK_HOUR[i])
          ahead_peak_month.R=c(ahead_peak_month.R, route_seq_R.df$AHEAD_PEAK_MADT[i])
          ahead_aadt.R=c(ahead_aadt.R, route_seq_R.df$AHEAD_AADT[i])
        }
      }
      ####if the end is located after the begining of the lest interval
      if (to_odom.R > route_seq_R.df$Odometer_Right[end.ind]){
        ####when the section that contains end.ind is also the last section of the road
        if (length(route_seq_R.df$Odometer_Right)==end.ind){
          back_peak_hour.R=c(back_peak_hour.R, route_seq_R.df$AHEAD_PEAK_HOUR[end.ind])
          back_peak_month.R=c(back_peak_month.R, route_seq_R.df$AHEAD_PEAK_MADT[end.ind])
          back_aadt.R=c(back_aadt.R, route_seq_R.df$AHEAD_AADT[end.ind])
          ahead_peak_hour.R=c(ahead_peak_hour.R, NA)
          ahead_peak_month.R=c(ahead_peak_month.R, NA)
          ahead_aadt.R=c(ahead_aadt.R, NA)
        } else{
          #### when the route is continued after the ending interval
          back_peak_hour.R=c(back_peak_hour.R, route_seq_R.df$AHEAD_PEAK_HOUR[end.ind])
          back_peak_month.R=c(back_peak_month.R, route_seq_R.df$AHEAD_PEAK_MADT[end.ind])
          back_aadt.R=c(back_aadt.R, route_seq_R.df$AHEAD_AADT[end.ind])
          ahead_peak_hour.R=c(ahead_peak_hour.R, route_seq_R.df$BACK_PEAK_HOUR[end.ind+1])
          ahead_peak_month.R=c(ahead_peak_month.R, route_seq_R.df$BACK_PEAK_MADT[end.ind+1])
          ahead_aadt.R=c(ahead_aadt.R, route_seq_R.df$BACK_AADT[end.ind+1])
        }
      }
    #### when odometer values are NA 
    #### note that both county and end postmile are required
    } else {
      if (!is.na(endCounty) & !is.na(endPM)){
        #filter route_seq_R for county and order based on postmile values
        route_seqPM.df=route_seq_R.df[which(route_seq_R.df$CNTY==endCounty),]
        route_seqPM.df=route_seqPM.df[order(route_seqPM.df$PM),]
        ####find which section of the road contains the end of work order or closure
        end.ind=findInterval(endPM, route_seqPM.df$PM, rightmost.closed = FALSE)
        ####some times the given odometer value is before the first interval
        ####return NAs for workorder or closures that are located before Caltrans's control
        if (end.ind==0){
          # return (cbind.data.frame("R.back_peak_hour"=NA, 
          #                          "R.back_peak_month"=NA,
          #                          "R.back_aadt"=NA,
          #                          "R.ahead_peak_hour"=NA,
          #                          "R.ahead_peak_month"=NA,
          #                          "R.ahead_aadt"=NA,
          #                          "L.back_peak_hour"=NA,
          #                          "L.back_peak_month"=NA,
          #                          "L.back_aadt"=NA,
          #                          "L.ahead_peak_hour"=NA,
          #                          "L.ahead_peak_month"=NA,
          #                          "L.ahead_aadt"=NA))
          return(cbind.data.frame("R.back_peak_hour"=toString(back_peak_hour.R), 
                                  "R.back_peak_month"=toString(back_peak_month.R),
                                  "R.back_aadt"=toString(back_aadt.R),
                                  "R.ahead_peak_hour"=toString(ahead_peak_hour.R),
                                  "R.ahead_peak_month"=toString(ahead_peak_month.R),
                                  "R.ahead_aadt"=toString(ahead_aadt.R),
                                  "L.back_peak_hour"=toString(back_peak_hour.L),
                                  "L.back_peak_month"=toString(back_peak_month.L),
                                  "L.back_aadt"=toString(back_aadt.L),
                                  "L.ahead_peak_hour"=toString(ahead_peak_hour.L),
                                  "L.ahead_peak_month"=toString(ahead_peak_month.L),
                                  "L.ahead_aadt"=toString(ahead_aadt.L)))
        }
        ####some time the given odometer exactly matches one row in the AADT data frame
        if (begin.ind < end.ind){
          for (i in (begin.ind+1):end.ind){
            back_peak_hour.R=c(back_peak_hour.R, route_seqPM.df$BACK_PEAK_HOUR[i])
            back_peak_month.R=c(back_peak_month.R, route_seqPM.df$BACK_PEAK_MADT[i])
            back_aadt.R=c(back_aadt.R, route_seqPM.df$BACK_AADT[i])
            ahead_peak_hour.R=c(ahead_peak_hour.R, route_seqPM.df$AHEAD_PEAK_HOUR[i])
            ahead_peak_month.R=c(ahead_peak_month.R, route_seqPM.df$AHEAD_PEAK_MADT[i])
            ahead_aadt.R=c(ahead_aadt.R, route_seqPM.df$AHEAD_AADT[i])
          }
        }
        #####if the end is located after the begining of the lest interval
        if (endPM > route_seqPM.df$PM[end.ind]){
          ####when the end.ind marks the last interval of the route
          if (length(route_seqPM.df$PM)==end.ind){
            back_peak_hour.R=c(back_peak_hour.R, route_seqPM.df$AHEAD_PEAK_HOUR[end.ind])
            back_peak_month.R=c(back_peak_month.R, route_seqPM.df$AHEAD_PEAK_MADT[end.ind])
            back_aadt.R=c(back_aadt.R, route_seqPM.df$AHEAD_AADT[end.ind])
            ahead_peak_hour.R=c(ahead_peak_hour.R, NA)
            ahead_peak_month.R=c(ahead_peak_month.R, NA)
            ahead_aadt.R=c(ahead_aadt.R, NA)
          } else{
            ####when the route is continued after the ending interval
            back_peak_hour.R=c(back_peak_hour.R, route_seqPM.df$AHEAD_PEAK_HOUR[end.ind])
            back_peak_month.R=c(back_peak_month.R, route_seqPM.df$AHEAD_PEAK_MADT[end.ind])
            back_aadt.R=c(back_aadt.R, route_seqPM.df$AHEAD_AADT[end.ind])
            ahead_peak_hour.R=c(ahead_peak_hour.R, route_seqPM.df$BACK_PEAK_HOUR[end.ind+1])
            ahead_peak_month.R=c(ahead_peak_month.R, route_seqPM.df$BACK_PEAK_MADT[end.ind+1])
            ahead_aadt.R=c(ahead_aadt.R, route_seqPM.df$BACK_AADT[end.ind+1])
          }
        }
      }
    }

    
#################### This section is repeating the above operation for the left alignment ###############
    if (!is.na(from_odom.L)){
      begin.ind=findInterval(from_odom.L, route_seq_L.df$Odometer_Left, rightmost.closed = FALSE)
      if (begin.ind==0){
        # return (cbind.data.frame("R.back_peak_hour"=NA, 
        #                          "R.back_peak_month"=NA,
        #                          "R.back_aadt"=NA,
        #                          "R.ahead_peak_hour"=NA,
        #                          "R.ahead_peak_month"=NA,
        #                          "R.ahead_aadt"=NA,
        #                          "L.back_peak_hour"=NA,
        #                          "L.back_peak_month"=NA,
        #                          "L.back_aadt"=NA,
        #                          "L.ahead_peak_hour"=NA,
        #                          "L.ahead_peak_month"=NA,
        #                          "L.ahead_aadt"=NA))
        return(cbind.data.frame("R.back_peak_hour"=toString(back_peak_hour.R), 
                                "R.back_peak_month"=toString(back_peak_month.R),
                                "R.back_aadt"=toString(back_aadt.R),
                                "R.ahead_peak_hour"=toString(ahead_peak_hour.R),
                                "R.ahead_peak_month"=toString(ahead_peak_month.R),
                                "R.ahead_aadt"=toString(ahead_aadt.R),
                                "L.back_peak_hour"=toString(back_peak_hour.L),
                                "L.back_peak_month"=toString(back_peak_month.L),
                                "L.back_aadt"=toString(back_aadt.L),
                                "L.ahead_peak_hour"=toString(ahead_peak_hour.L),
                                "L.ahead_peak_month"=toString(ahead_peak_month.L),
                                "L.ahead_aadt"=toString(ahead_aadt.L)))
      }
      if (from_odom.L==route_seq_L.df$Odometer_Left[begin.ind]){
        back_peak_hour.L=c(back_peak_hour.L, route_seq_L.df$BACK_PEAK_HOUR[begin.ind])
        back_peak_month.L=c(back_peak_month.L, route_seq_L.df$BACK_PEAK_MADT[begin.ind])
        back_aadt.L=c(back_aadt.L, route_seq_L.df$BACK_AADT[begin.ind])
        ahead_peak_hour.L=c(ahead_peak_hour.L, route_seq_L.df$AHEAD_PEAK_HOUR[begin.ind])
        ahead_peak_month.L=c(ahead_peak_month.L, route_seq_L.df$AHEAD_PEAK_MADT[begin.ind])
        ahead_aadt.L=c(ahead_aadt.L, route_seq_L.df$AHEAD_AADT[begin.ind])
      } else {
        #for the majority that are somewhere in the interval, the back traffic is equal to the ahead traffic
        #of the begining of the interval and the ahead traffic is equal to the back traffic of the end of the
        #interval
        back_peak_hour.L=c(back_peak_hour.L, route_seq_L.df$AHEAD_PEAK_HOUR[begin.ind])
        back_peak_month.L=c(back_peak_month.L, route_seq_L.df$AHEAD_PEAK_MADT[begin.ind])
        back_aadt.L=c(back_aadt.L, route_seq_L.df$AHEAD_AADT[begin.ind])
        ahead_peak_hour.L=c(ahead_peak_hour.L, route_seq_L.df$BACK_PEAK_HOUR[begin.ind+1])
        ahead_peak_month.L=c(ahead_peak_month.L, route_seq_L.df$BACK_PEAK_MADT[begin.ind+1])
        ahead_aadt.L=c(ahead_aadt.L, route_seq_L.df$BACK_AADT[begin.ind+1])
      }
    } else{
      if (!is.na(beginCounty) & !is.na(beginPM)){
        route_seqPM.df=route_seq_L.df[which(route_seq_L.df$County==beginCounty),]
        route_seqPM.df=route_seqPM.df[order(route_seqPM.df$PM),]
        begin.ind=findInterval(beginPM, route_seqPM.df$PM, rightmost.closed = FALSE)
        if (begin.ind==0){
          # return (cbind.data.frame("R.back_peak_hour"=NA, 
          #                          "R.back_peak_month"=NA,
          #                          "R.back_aadt"=NA,
          #                          "R.ahead_peak_hour"=NA,
          #                          "R.ahead_peak_month"=NA,
          #                          "R.ahead_aadt"=NA,
          #                          "L.back_peak_hour"=NA,
          #                          "L.back_peak_month"=NA,
          #                          "L.back_aadt"=NA,
          #                          "L.ahead_peak_hour"=NA,
          #                          "L.ahead_peak_month"=NA,
          #                          "L.ahead_aadt"=NA))
          return(cbind.data.frame("R.back_peak_hour"=toString(back_peak_hour.R), 
                                  "R.back_peak_month"=toString(back_peak_month.R),
                                  "R.back_aadt"=toString(back_aadt.R),
                                  "R.ahead_peak_hour"=toString(ahead_peak_hour.R),
                                  "R.ahead_peak_month"=toString(ahead_peak_month.R),
                                  "R.ahead_aadt"=toString(ahead_aadt.R),
                                  "L.back_peak_hour"=toString(back_peak_hour.L),
                                  "L.back_peak_month"=toString(back_peak_month.L),
                                  "L.back_aadt"=toString(back_aadt.L),
                                  "L.ahead_peak_hour"=toString(ahead_peak_hour.L),
                                  "L.ahead_peak_month"=toString(ahead_peak_month.L),
                                  "L.ahead_aadt"=toString(ahead_aadt.L)))
        }
        #some time the given odometer exactly matches one row in the AADT data frame
        if (beginPM==route_seqPM.df$PM[begin.ind]){
          back_peak_hour.L=c(back_peak_hour.L, route_seqPM.df$BACK_PEAK_HOUR[begin.ind])
          back_peak_month.L=c(back_peak_month.L, route_seqPM.df$BACK_PEAK_MADT[begin.ind])
          back_aadt.L=c(back_aadt.L, route_seqPM.df$BACK_AADT[begin.ind])
          ahead_peak_hour.L=c(ahead_peak_hour.L, route_seqPM.df$AHEAD_PEAK_HOUR[begin.ind])
          ahead_peak_month.L=c(ahead_peak_month.L, route_seqPM.df$AHEAD_PEAK_MADT[begin.ind])
          ahead_aadt.L=c(ahead_aadt.L, route_seqPM.df$AHEAD_AADT[begin.ind])
        } else {
          #for the majority that are somewhere in the interval, the back traffic is equal to the ahead traffic
          #of the begining of the interval and the ahead traffic is equal to the back traffic of the end of the
          #interval
          back_peak_hour.L=c(back_peak_hour.L, route_seqPM.df$AHEAD_PEAK_HOUR[begin.ind])
          back_peak_month.L=c(back_peak_month.L, route_seqPM.df$AHEAD_PEAK_MADT[begin.ind])
          back_aadt.L=c(back_aadt.L, route_seq_L.df$AHEAD_AADT[begin.ind])
          ahead_peak_hour.L=c(ahead_peak_hour.L, route_seqPM.df$BACK_PEAK_HOUR[begin.ind+1])
          ahead_peak_month.L=c(ahead_peak_month.L, route_seqPM.df$BACK_PEAK_MADT[begin.ind+1])
          ahead_aadt.L=c(ahead_aadt.L, route_seqPM.df$BACK_AADT[begin.ind+1])
        }
      }
    }
    if(!is.na(to_odom.L)){
      #find the interval of to_odom.L in sequence of odometer values
      end.ind=findInterval(to_odom.L, route_seq_L.df$Odometer_Left, rightmost.closed = FALSE)
      #some times the given odometer value is before the first interval
      if (end.ind==0){
        # return (cbind.data.frame("R.back_peak_hour"=NA, 
        #                          "R.back_peak_month"=NA,
        #                          "R.back_aadt"=NA,
        #                          "R.ahead_peak_hour"=NA,
        #                          "R.ahead_peak_month"=NA,
        #                          "R.ahead_aadt"=NA,
        #                          "L.back_peak_hour"=NA,
        #                          "L.back_peak_month"=NA,
        #                          "L.back_aadt"=NA,
        #                          "L.ahead_peak_hour"=NA,
        #                          "L.ahead_peak_month"=NA,
        #                          "L.ahead_aadt"=NA))
        return(cbind.data.frame("R.back_peak_hour"=toString(back_peak_hour.R), 
                                "R.back_peak_month"=toString(back_peak_month.R),
                                "R.back_aadt"=toString(back_aadt.R),
                                "R.ahead_peak_hour"=toString(ahead_peak_hour.R),
                                "R.ahead_peak_month"=toString(ahead_peak_month.R),
                                "R.ahead_aadt"=toString(ahead_aadt.R),
                                "L.back_peak_hour"=toString(back_peak_hour.L),
                                "L.back_peak_month"=toString(back_peak_month.L),
                                "L.back_aadt"=toString(back_aadt.L),
                                "L.ahead_peak_hour"=toString(ahead_peak_hour.L),
                                "L.ahead_peak_month"=toString(ahead_peak_month.L),
                                "L.ahead_aadt"=toString(ahead_aadt.L)))
      }
      # if end.ind >= begin.ind+1, return all the aadt information in between
      if (begin.ind < end.ind){
        for (i in (begin.ind+1):end.ind){
          back_peak_hour.L=c(back_peak_hour.L, route_seq_L.df$BACK_PEAK_HOUR[i])
          back_peak_month.L=c(back_peak_month.L, route_seq_L.df$BACK_PEAK_MADT[i])
          back_aadt.L=c(back_aadt.L, route_seq_L.df$BACK_AADT[i])
          ahead_peak_hour.L=c(ahead_peak_hour.L, route_seq_L.df$AHEAD_PEAK_HOUR[i])
          ahead_peak_month.L=c(ahead_peak_month.L, route_seq_L.df$AHEAD_PEAK_MADT[i])
          ahead_aadt.L=c(ahead_aadt.L, route_seq_L.df$AHEAD_AADT[i])
        }
      }
      #if the end is located after the begining of the lest interval
      if (to_odom.L > route_seq_L.df$Odometer_Left[end.ind]){
        #when the end.ind marks the last interval of the route
        if (length(route_seq_L.df$Odometer_Left)==end.ind){
          back_peak_hour.L=c(back_peak_hour.L, route_seq_L.df$AHEAD_PEAK_HOUR[end.ind])
          back_peak_month.L=c(back_peak_month.L, route_seq_L.df$AHEAD_PEAK_MADT[end.ind])
          back_aadt.L=c(back_aadt.L, route_seq_L.df$AHEAD_AADT[end.ind])
          ahead_peak_hour.L=c(ahead_peak_hour.L, NA)
          ahead_peak_month.L=c(ahead_peak_month.L, NA)
          ahead_aadt.L=c(ahead_aadt.L, NA)
        } else{
          #when the route is continued after the ending interval
          back_peak_hour.L=c(back_peak_hour.L, route_seq_L.df$AHEAD_PEAK_HOUR[end.ind])
          back_peak_month.L=c(back_peak_month.L, route_seq_L.df$AHEAD_PEAK_MADT[end.ind])
          back_aadt.L=c(back_aadt.L, route_seq_L.df$AHEAD_AADT[end.ind])
          ahead_peak_hour.L=c(ahead_peak_hour.L, route_seq_L.df$BACK_PEAK_HOUR[end.ind+1])
          ahead_peak_month.L=c(ahead_peak_month.L, route_seq_L.df$BACK_PEAK_MADT[end.ind+1])
          ahead_aadt.L=c(ahead_aadt.L, route_seq_L.df$BACK_AADT[end.ind+1])
        }
      }
    } else {
      if (!is.na(endCounty) & !is.na(endPM)){
        route_seqPM.df=route_seq_L.df[which(route_seq_L.df$County==endCounty),]
        route_seqPM.df=route_seqPM.df[order(route_seqPM.df$PM),]
        end.ind=findInterval(endPM, route_seqPM.df$PM, rightmost.closed = FALSE)
        #this may be required for later use in this if body
        if (end.ind==0){
          # return (cbind.data.frame("R.back_peak_hour"=NA, 
          #                          "R.back_peak_month"=NA,
          #                          "R.back_aadt"=NA,
          #                          "R.ahead_peak_hour"=NA,
          #                          "R.ahead_peak_month"=NA,
          #                          "R.ahead_aadt"=NA,
          #                          "L.back_peak_hour"=NA,
          #                          "L.back_peak_month"=NA,
          #                          "L.back_aadt"=NA,
          #                          "L.ahead_peak_hour"=NA,
          #                          "L.ahead_peak_month"=NA,
          #                          "L.ahead_aadt"=NA))
          return(cbind.data.frame("R.back_peak_hour"=toString(back_peak_hour.R), 
                                  "R.back_peak_month"=toString(back_peak_month.R),
                                  "R.back_aadt"=toString(back_aadt.R),
                                  "R.ahead_peak_hour"=toString(ahead_peak_hour.R),
                                  "R.ahead_peak_month"=toString(ahead_peak_month.R),
                                  "R.ahead_aadt"=toString(ahead_aadt.R),
                                  "L.back_peak_hour"=toString(back_peak_hour.L),
                                  "L.back_peak_month"=toString(back_peak_month.L),
                                  "L.back_aadt"=toString(back_aadt.L),
                                  "L.ahead_peak_hour"=toString(ahead_peak_hour.L),
                                  "L.ahead_peak_month"=toString(ahead_peak_month.L),
                                  "L.ahead_aadt"=toString(ahead_aadt.L)))
        }
        #some time the given odometer exactly matches one row in the AADT data frame
        if (begin.ind < end.ind){
          for (i in (begin.ind+1):end.ind){
            back_peak_hour.L=c(back_peak_hour.L, route_seqPM.df$BACK_PEAK_HOUR[i])
            back_peak_month.L=c(back_peak_month.L, route_seqPM.df$BACK_PEAK_MADT[i])
            back_aadt.L=c(back_aadt.L, route_seqPM.df$BACK_AADT[i])
            ahead_peak_hour.L=c(ahead_peak_hour.L, route_seqPM.df$AHEAD_PEAK_HOUR[i])
            ahead_peak_month.L=c(ahead_peak_month.L, route_seqPM.df$AHEAD_PEAK_MADT[i])
            ahead_aadt.L=c(ahead_aadt.L, route_seqPM.df$AHEAD_AADT[i])
          }
        }
        #if the end is located after the begining of the lest interval
        if (endPM > route_seqPM.df$PM[end.ind]){
          #when the end.ind marks the last interval of the route
          if (length(route_seqPM.df$PM)==end.ind){
            back_peak_hour.L=c(back_peak_hour.L, route_seqPM.df$AHEAD_PEAK_HOUR[end.ind])
            back_peak_month.L=c(back_peak_month.L, route_seqPM.df$AHEAD_PEAK_MADT[end.ind])
            back_aadt.L=c(back_aadt.L, route_seqPM.df$AHEAD_AADT[end.ind])
            ahead_peak_hour.L=c(ahead_peak_hour.L, NA)
            ahead_peak_month.L=c(ahead_peak_month.L, NA)
            ahead_aadt.L=c(ahead_aadt.L, NA)
          } else{
            #when the route is continued after the ending interval
            back_peak_hour.L=c(back_peak_hour.L, route_seqPM.df$AHEAD_PEAK_HOUR[end.ind])
            back_peak_month.L=c(back_peak_month.L, route_seqPM.df$AHEAD_PEAK_MADT[end.ind])
            back_aadt.L=c(back_aadt.L, route_seqPM.df$AHEAD_AADT[end.ind])
            ahead_peak_hour.L=c(ahead_peak_hour.L, route_seqPM.df$BACK_PEAK_HOUR[end.ind+1])
            ahead_peak_month.L=c(ahead_peak_month.L, route_seqPM.df$BACK_PEAK_MADT[end.ind+1])
            ahead_aadt.L=c(ahead_aadt.L, route_seqPM.df$BACK_AADT[end.ind+1])
          }
        }
      }
    }
  }
  return(cbind.data.frame("R.back_peak_hour"=toString(back_peak_hour.R), 
               "R.back_peak_month"=toString(back_peak_month.R),
               "R.back_aadt"=toString(back_aadt.R),
               "R.ahead_peak_hour"=toString(ahead_peak_hour.R),
               "R.ahead_peak_month"=toString(ahead_peak_month.R),
               "R.ahead_aadt"=toString(ahead_aadt.R),
               "L.back_peak_hour"=toString(back_peak_hour.L),
               "L.back_peak_month"=toString(back_peak_month.L),
               "L.back_aadt"=toString(back_aadt.L),
               "L.ahead_peak_hour"=toString(ahead_peak_hour.L),
               "L.ahead_peak_month"=toString(ahead_peak_month.L),
               "L.ahead_aadt"=toString(ahead_aadt.L)))
}
##########################################################################################################
##########################################################################################################
###################################### join LEMO_WorkOrder/LCS with TRUCK ################################

####this function is also similar to the AADT_match.func
####the only difference is the number of TRUCK data features
TRUCK_match.func=function(beginCounty, beginPM, endCounty, endPM, 
                          route, rtsfx, from_odom.R, to_odom.R, from_odom.L, to_odom.L, temp_truck){
  #filter the AADT data.frame for route
  route_seq.df=temp_truck[which(temp_tr???uck$RTE==route),]
  
  #if the given route has a route suffix, filter for it
  if (!is.na(rtsfx)){
    route_seq.df=route_seq.df[which(route_seq.df$RTE_SFX==rtsfx),]
  }else{#if no suffix is given, filter rows that have nor route suffix
    route_seq.df=route_seq.df[is.na(route_seq.df$RTE_SFX),]
  }
  
  #filter route_seq.df with the right/left alingment for postmile suffix = alignment or NA and corresponding odometer
  route_seq_R.df=route_seq.df[which(!is.na(route_seq.df$Odometer_Right)),]
  route_seq_L.df=route_seq.df[which(!is.na(route_seq.df$Odometer_Left)),]
  #order the route_seq data.frames by odometer values
  #this is because of the find interval function which only works over non-decreasing sequences
  route_seq_R.df=route_seq_R.df[order(route_seq_R.df$Odometer_Right),]
  route_seq_L.df=route_seq_L.df[order(route_seq_L.df$Odometer_Left),]
  
  
  #introduce empty vectors for each feature of the AADT database
  #back_truck_aadt.R=c()
  #back_truck_percent.R=c()
  ahead_truck_aadt.R=c()
  #ahead_truck_percent.R=c()
  
  #back_truck_aadt.L=c()
  #back_truck_percent.L=c()
  ahead_truck_aadt.L=c()
  #ahead_truck_percent.L=c()
  if (dim(route_seq.df)[1]==0 |
      (is.na(from_odom.R) & (is.na(beginPM) | (is.na(beginCounty)))) |
      (is.na(from_odom.L) & (is.na(beginPM) | (is.na(beginCounty)))) |
      (is.na(to_odom.R) & (is.na(endPM) | (is.na(endCounty)))) |
      (is.na(to_odom.L) & (is.na(endPM) | (is.na(endCounty))))
  ){
    return (cbind.data.frame("R.ahead_truck_aadt"=NA,
                             "L.ahead_truck_aadt"=NA))
  } else{
    begin.ind=NA
    end.ind=NA

    if (!is.na(from_odom.R)){
      #find the interval of from_odom.R in sequence of odometer values 
      begin.ind=findInterval(from_odom.R, route_seq_R.df$Odometer_Right, rightmost.closed = FALSE)
      #some times the given odometer value is before the first interval
      if (begin.ind==0){
        return(cbind.data.frame("R.ahead_truck_aadt"=toString(ahead_truck_aadt.R),
                                "L.ahead_truck_aadt"=toString(ahead_truck_aadt.L)))
      }
      #some time the given odometer exactly matches one row in the AADT data frame
      if (from_odom.R==route_seq_R.df$Odometer_Right[begin.ind]){
        ahead_truck_aadt.R=c(ahead_truck_aadt.R, route_seq_R.df$TRUCK_AADT[begin.ind])
      } else {
        #for the majority that are somewhere in the interval, the back traffic is equal to the ahead traffic
        #of the begining of the interval and the ahead traffic is equal to the back traffic of the end of the
        #interval
        ahead_truck_aadt.R=c(ahead_truck_aadt.R, route_seq_R.df$TRUCK_AADT[begin.ind+1])
      }
    } else{
      if (!is.na(beginCounty) & !is.na(beginPM)){
        route_seqPM.df=route_seq_R.df[which(route_seq_R.df$CNTY==beginCounty),]
        route_seqPM.df=route_seqPM.df[order(route_seqPM.df$PM),]
        begin.ind=findInterval(beginPM, route_seqPM.df$PM, rightmost.closed = FALSE)
        if (begin.ind==0){
          return(cbind.data.frame("R.ahead_truck_aadt"=toString(ahead_truck_aadt.R),
                                  "L.ahead_truck_aadt"=toString(ahead_truck_aadt.L)))
        }
        #some time the given odometer exactly matches one row in the AADT data frame
        if (beginPM==route_seqPM.df$PM[begin.ind]){
          ahead_truck_aadt.R=c(ahead_truck_aadt.R, route_seqPM.df$TRUCK_AADT[begin.ind])
        } else {
          #for the majority that are somewhere in the interval, the back traffic is equal to the ahead traffic
          #of the begining of the interval and the ahead traffic is equal to the back traffic of the end of the
          #interval
          ahead_truck_aadt.R=c(ahead_truck_aadt.R, route_seqPM.df$TRUCK_AADT[begin.ind+1])
        }
      }
    }
    if(!is.na(to_odom.R)){
      #find the interval of to_odom.R in sequence of odometer values
      end.ind=findInterval(to_odom.R, route_seq_R.df$Odometer_Right, rightmost.closed = FALSE)
      #some times the given odometer value is before the first interval
      if (end.ind==0){
        return(cbind.data.frame("R.ahead_truck_aadt"=toString(ahead_truck_aadt.R),
                                "L.ahead_truck_aadt"=toString(ahead_truck_aadt.L)))
      }
      # if end.ind >= begin.ind+1, return all the aadt information in between
      if (begin.ind < end.ind){
        for (i in (begin.ind+1):end.ind){
          ahead_truck_aadt.R=c(ahead_truck_aadt.R, route_seq_R.df$TRUCK_AADT[i])
        }
      }
      #if the end is located after the begining of the lest interval
      if (to_odom.R > route_seq_R.df$Odometer_Right[end.ind]){
        #when the end.ind marks the last interval of the route
        if (length(route_seq_R.df$Odometer_Right)==end.ind){
          ahead_truck_aadt.R=c(ahead_truck_aadt.R, NA)
        } else{
          #when the route is continued after the ending interval
          ahead_truck_aadt.R=c(ahead_truck_aadt.R, route_seq_R.df$TRUCK_AADT[end.ind+1])
        }
      }
    } else {
      if (!is.na(endCounty) & !is.na(endPM)){
        route_seqPM.df=route_seq_R.df[which(route_seq_R.df$CNTY==endCounty),]
        route_seqPM.df=route_seqPM.df[order(route_seqPM.df$PM),]
        end.ind=findInterval(endPM, route_seqPM.df$PM, rightmost.closed = FALSE)
        #this may be required for later use in this if body
        if (end.ind==0){
          return(cbind.data.frame("R.ahead_truck_aadt"=toString(ahead_truck_aadt.R),
                                  "L.ahead_truck_aadt"=toString(ahead_truck_aadt.L)))
        }
        #some time the given odometer exactly matches one row in the AADT data frame
        if (begin.ind < end.ind){
          for (i in (begin.ind+1):end.ind){
            ahead_truck_aadt.R=c(ahead_truck_aadt.R, route_seqPM.df$TRUCK_AADT[i])
          }
        }
        #if the end is located after the begining of the lest interval
        if (endPM > route_seqPM.df$PM[end.ind]){
          #when the end.ind marks the last interval of the route
          if (length(route_seqPM.df$PM)==end.ind){
            ahead_truck_aadt.R=c(ahead_truck_aadt.R, NA)
          } else{
            #when the route is continued after the ending interval
            ahead_truck_aadt.R=c(ahead_truck_aadt.R, route_seqPM.df$`truck.aadt.total B`[end.ind+1])
          }
        }
      }
    }
    if (!is.na(from_odom.L)){
      #find the interval of from_odom.L in sequence of odometer values 
      begin.ind=findInterval(from_odom.L, route_seq_L.df$Odometer_Left, rightmost.closed = FALSE)
      #some times the given odometer value is before the first interval
      if (begin.ind==0){
        return(cbind.data.frame("R.ahead_truck_aadt"=toString(ahead_truck_aadt.R),
                                "L.ahead_truck_aadt"=toString(ahead_truck_aadt.L)))
      }
      #some time the given odometer exactly matches one row in the AADT data frame
      if (from_odom.L==route_seq_L.df$Odometer_Left[begin.ind]){
        ahead_truck_aadt.L=c(ahead_truck_aadt.L, route_seq_L.df$TRUCK_AADT[begin.ind])
      } else {
        #for the majority that are somewhere in the interval, the back traffic is equal to the ahead traffic
        #of the begining of the interval and the ahead traffic is equal to the back traffic of the end of the
        #interval
        ahead_truck_aadt.L=c(ahead_truck_aadt.L, route_seq_L.df$TRUCK_AADT[begin.ind+1])
      }
    } else{
      if (!is.na(beginCounty) & !is.na(beginPM)){
        route_seqPM.df=route_seq_L.df[which(route_seq_L.df$County==beginCounty),]
        route_seqPM.df=route_seqPM.df[order(route_seqPM.df$PM),]
        begin.ind=findInterval(beginPM, route_seqPM.df$PM, rightmost.closed = FALSE)
        if (begin.ind==0){
          return(cbind.data.frame("R.ahead_truck_aadt"=toString(ahead_truck_aadt.R),
                                  "L.ahead_truck_aadt"=toString(ahead_truck_aadt.L)))
        }
        #some time the given odometer exactly matches one row in the AADT data frame
        if (beginPM==route_seqPM.df$PM[begin.ind]){
          ahead_truck_aadt.L=c(ahead_truck_aadt.L, route_seqPM.df$TRUCK_AADT[begin.ind])
        } else {
          #for the majority that are somewhere in the interval, the back traffic is equal to the ahead traffic
          #of the begining of the interval and the ahead traffic is equal to the back traffic of the end of the
          #interval
          ahead_truck_aadt.L=c(ahead_truck_aadt.L, route_seqPM.df$TRUCK_AADT[begin.ind+1])
        }
      }
    }
    if(!is.na(to_odom.L)){
      #find the interval of to_odom.L in sequence of odometer values
      end.ind=findInterval(to_odom.L, route_seq_L.df$Odometer_Left, rightmost.closed = FALSE)
      #some times the given odometer value is before the first interval
      if (end.ind==0){
        return(cbind.data.frame("R.ahead_truck_aadt"=toString(ahead_truck_aadt.R),
                                "L.ahead_truck_aadt"=toString(ahead_truck_aadt.L)))
      }
      # if end.ind >= begin.ind+1, return all the aadt information in between
      if (begin.ind < end.ind){
        for (i in (begin.ind+1):end.ind){
          ahead_truck_aadt.L=c(ahead_truck_aadt.L, route_seq_L.df$TRUCK_AADT[i])
        }
      }
      #if the end is located after the begining of the lest interval
      if (to_odom.L > route_seq_L.df$Odometer_Left[end.ind]){
        #when the end.ind marks the last interval of the route
        if (length(route_seq_L.df$Odometer_Left)==end.ind){
          ahead_truck_aadt.L=c(ahead_truck_aadt.L, NA)
        } else{
          #when the route is continued after the ending interval
          ahead_truck_aadt.L=c(ahead_truck_aadt.L, route_seq_L.df$TRUCK_AADT[end.ind+1])
        }
      }
    } else {
      if (!is.na(endCounty) & !is.na(endPM)){
        route_seqPM.df=route_seq_L.df[which(route_seq_L.df$County==endCounty),]
        route_seqPM.df=route_seqPM.df[order(route_seqPM.df$PM),]
        end.ind=findInterval(endPM, route_seqPM.df$PM, rightmost.closed = FALSE)
        #this may be required for later use in this if body
        if (end.ind==0){
          return(cbind.data.frame("R.ahead_truck_aadt"=toString(ahead_truck_aadt.R),
                                  "L.ahead_truck_aadt"=toString(ahead_truck_aadt.L)))
        }
        #some time the given odometer exactly matches one row in the AADT data frame
        if (begin.ind < end.ind){
          for (i in (begin.ind+1):end.ind){
            ahead_truck_aadt.L=c(ahead_truck_aadt.L, route_seqPM.df$TRUCK_AADT[i])
          }
        }
        #if the end is located after the begining of the lest interval
        if (endPM > route_seqPM.df$PM[end.ind]){
          #when the end.ind marks the last interval of the route
          if (length(route_seqPM.df$PM)==end.ind){
            ahead_truck_aadt.L=c(ahead_truck_aadt.L, NA)
          } else{
            #when the route is continued after the ending interval
           ahead_truck_aadt.L=c(ahead_truck_aadt.L, route_seqPM.df$TRUCK_AADT[end.ind+1])
          }
        }
      }
    }
  }
  return(cbind.data.frame("R.ahead_truck_aadt"=toString(ahead_truck_aadt.R),
                          "L.ahead_truck_aadt"=toString(ahead_truck_aadt.L)))
}
##########################################################################################################
##########################################################################################################
##########################################################################################################
#NOTE:
#check for route_ID
#check for odom 
#it should be between two points: x < odom < y
#average aadt= ((odom - x) / (y-x)) * aadt(y) + ((y - odom) / (y-x)) * aadt(x)
#if odom if found exactly, aadt(odom) = aadt(x)