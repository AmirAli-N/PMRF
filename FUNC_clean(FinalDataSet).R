cleanUp_Dataset=function(df, cols){
  
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(anytime)
  
  df=select(df, cols)
  final_cols=c()
  
  if ("work_date" %in% cols){
    df=cbind.data.frame(df, work_day=wday(anydate(df$work_date), label = TRUE, abbr = TRUE))
    df$work_day=factor(df$work_day, ordered = FALSE)
    df$work_date=month(anydate(df$work_date), label = TRUE, abbr = TRUE)
    names(df)[names(df)=="work_date"]="work_month"
    df$work_month=factor(df$work_month, ordered = FALSE)
    final_cols=c(final_cols, "work_month", "work_day")
  }
  
  if ("activity" %in% cols){
    unique(df$activity)
    df$activity=as.factor(df$activity)
    final_cols=c(final_cols, "activity")
  }
  
  if ("district" %in% cols){
    unique(df$district)
    df$district=as.factor(df$district)
    final_cols=c(final_cols, "district")
  }
  
  if ("county" %in% cols){
    unique(df$county)
    df$county=as.factor(df$county)
    final_cols=c(final_cols, "county")
  }
  
  if ("route" %in% cols){
    unique(df$route)
    df$route=as.factor(df$route)
    final_cols=c(final_cols, "route")
  }
  
  if ("work_duration" %in% cols){
    df$work_duration=as.numeric(df$work_duration)
    final_cols=c(final_cols, "work_duration")
  }
  
  if ("work_length" %in% cols){
    df$work_length=as.numeric(df$work_length)
    final_cols=c(final_cols, "work_length")
  }
  
  if ("surface_type" %in% cols){
    unique(df$surface_type)
    df$surface_type=as.factor(df$surface_type)
    final_cols=c(final_cols, "surface_type")
  }
  
  if ("num_lanes" %in% cols){
    unique(df$num_lanes)
    df$num_lanes=as.factor(df$num_lanes)
    final_cols=c(final_cols, "num_lanes")
  }
  
  if ("road_use" %in% cols){
    unique(df$road_use)
    df$road_use=as.factor(df$road_use)
    final_cols=c(final_cols, "road_use")
  }
  
  if ("road_width" %in% cols){
    unique(df$road_width)
    df$road_width=as.numeric(df$road_width)
    final_cols=c(final_cols, "road_width")
  }
  
  if ("median_type" %in% cols){
    unique(df$median_type)
    df$median_type=as.factor(df$median_type)
    final_cols=c(final_cols, "median_type")
  }
  
  if ("barrier_type" %in% cols){
    unique(df$barrier_type)
    df$barrier_type=as.factor(df$barrier_type)
    final_cols=c(final_cols, "barrier_type")
  }
  
  if ("hwy_group" %in% cols){
    unique(df$hwy_group)
    df$hwy_group=as.factor(df$hwy_group)
    final_cols=c(final_cols, "hwy_group")
  }
  
  if ("access_type" %in% cols){
    unique(df$access_type)
    df$access_type=as.factor(df$access_type)
    final_cols=c(final_cols, "access_type")
  }
  
  if ("terrain_type" %in% cols){
    unique(df$terrain_type)
    df$terrain_type=as.factor(df$terrain_type)
    final_cols=c(final_cols, "terrain_type")
  }
  
  if ("road_speed" %in% cols){
    unique(df$road_speed)
    df$road_speed=as.numeric(df$road_speed)
    final_cols=c(final_cols, "road_speed")
  }
  
  if ("road_adt" %in% cols){
    unique(df$road_adt)
    df$road_adt=as.numeric(gsub(",", "", df$road_adt))
    final_cols=c(final_cols, "road_adt")
  }
  
  if ("population_code" %in% cols){
    unique(df$population_code)
    df$population_code[which(!is.na(df$population_code) & df$population_code!="B" & df$population_code!="U" & df$population_code!="R")]=NA
    df$population_code=as.factor(df$population_code)
    final_cols=c(final_cols, "population_code")
  }
  
  if ("peak_aadt" %in% cols){
    df$peak_aadt=as.numeric(df$peak_aadt)
    final_cols=c(final_cols, "peak_aadt")
  }
  
  if ("aadt" %in% cols){
    df$aadt=as.numeric(df$aadt)
    final_cols=c(final_cols, "aadt")
  }
  
  if ("truck_aadt" %in% cols){
    df$truck_aadt=as.numeric(df$truck_aadt)
    final_cols=c(final_cols, "truck_aadt")
  }
  
  if ("collision_density11_12" %in% cols){
    df$collision_density11_12=as.numeric(df$collision_density11_12)
    final_cols=c(final_cols, "collision_density11_12")
  }
  
  if ("closure_id" %in% cols){
    unique(df$closure_id)
    df$closure_id=ifelse(is.na(df$closure_id), 0, 1)
    df$closure_id=as.factor(df$closure_id)
    final_cols=c(final_cols, "closure_id")
  }
  
  if ("closure_coverage" %in% cols){
    #df$closure_coverage[which(is.na(df$closure_coverage))]=0
    df$closure_coverage=as.numeric(df$closure_coverage)
    final_cols=c(final_cols, "closure_coverage")
  }
  
  if ("closure_length" %in% cols){
    #df$closure_length[which(is.na(df$closure_length))]=0
    df$closure_length=as.numeric(df$closure_length)
    final_cols=c(final_cols, "closure_length")
  }
  
  if ("closure_workType" %in% cols){
    unique(df$closure_workType)
    df$closure_workType=as.factor(df$closure_workType)
    final_cols=c(final_cols, "closure_workType")
  }
  
  if ("closure_duration" %in% cols){
    unique(df$closure_duration)
    df$closure_duration=as.factor(df$closure_duration)
    final_cols=c(final_cols, "closure_duration")
  }
  
  if ("closure_cozeepMazeep" %in% cols){
    unique(df$closure_cozeepMazeep)
    df$closure_cozeepMazeep=as.factor(df$closure_cozeepMazeep)
    final_cols=c(final_cols, "closure_cozeepMazeep")
  }
  
  if ("closure_detour" %in% cols){
    unique(df$closure_detour)
    df$closure_detour=as.factor(df$closure_detour)
    final_cols=c(final_cols, "closure_detour")
  }
  
  if ("closure_type" %in% cols){
    unique(df$closure_type)
    df$closure_type=as.factor(df$closure_type)
    final_cols=c(final_cols, "closure_type")
  }
  
  if ("closure_facility" %in% cols){
    unique(df$closure_facility)
    df$closure_facility=as.factor(df$closure_facility)
    final_cols=c(final_cols, "closure_facility")
  }
  
  if ("closure_lanes" %in% cols){
    unique(df$closure_lanes)
    #df$closure_lanes[which(is.na(df$closure_lanes))]=0
    df$closure_lanes=as.factor(df$closure_lanes)
    final_cols=c(final_cols, "closure_lanes")
  }
  
  if ("collision_id" %in% cols){
    df$collision_id=ifelse(is.na(df$collision_id), 0, 1)
    df$collision_id=as.factor(df$collision_id)
    final_cols=c(final_cols, "collision_id")
  }
  
  if ("collision_time" %in% cols){
    unique(df$collision_time)
    df$collision_time[which(!is.na(df$collision_time))]=as.numeric(gsub(":", "", df$collision_time[which(!is.na(df$collision_time))])) %/% 100
    df$collision_time=as.factor(df$collision_time)
    final_cols=c(final_cols, "collision_time")
  }
  
  if ("collision_day" %in% cols){
    unique(df$collision_day)
    df$collision_day=as.factor(df$collision_day)
    final_cols=c(final_cols, "collision_day")
  }
  
  if ("collision_weather_cond_1" %in% cols){
    unique(df$collision_weather_cond_1)
    df$collision_weather_cond_1[df$collision_weather_cond_1=="-"]=NA
    df$collision_weather_cond_1=as.factor(df$collision_weather_cond_1)
    final_cols=c(final_cols, "collision_weather_cond_1")
  }
  
  if ("collision_weather_cond_2" %in% cols){
    unique(df$collision_weather_cond_2)
    df$collision_weather_cond_2[df$collision_weather_cond_2=="-"]=NA
    df$collision_weather_cond_2=as.factor(df$collision_weather_cond_2)
    final_cols=c(final_cols, "collision_weather_cond_2")
  }
  
  if ("collision_location_type" %in% cols){
    unique(df$collision_location_type)
    df$collision_location_type=as.factor(df$collision_location_type)
    final_cols=c(final_cols, "collision_location_type")
  }
  
  if ("collision_ramp_intersection" %in% cols){
    unique(df$collision_ramp_intersection)
    df$collision_ramp_intersection[df$collision_ramp_intersection=="-"]=NA
    df$collision_ramp_intersection=as.factor(df$collision_ramp_intersection)
    final_cols=c(final_cols, "collision_ramp_intersection")
  }
  
  if ("collision_severity" %in% cols){
    unique(df$collision_severity)
    df$collision_severity=as.factor(df$collision_severity)
    final_cols=c(final_cols, "collision_severity")
  }
  
  if ("collision_num_killed" %in% cols){
    df$collision_num_killed=as.factor(df$collision_num_killed)
    final_cols=c(final_cols, "collision_num_killed")
  }
  
  if ("collision_num_injured" %in% cols){
    df$collision_num_injured=as.factor(df$collision_num_injured)
    final_cols=c(final_cols, "collision_num_injured")
  }
    
  if ("collision_party_count" %in% cols){  
    df$collision_party_count=as.factor(df$collision_party_count)
    final_cols=c(final_cols, "collision_party_count")
  }
  
  if ("collision_prime_factor" %in% cols){
    unique(df$collision_prime_factor)
    df$collision_prime_factor[df$collision_prime_factor=="-"]=NA
    df$collision_prime_factor=as.factor(df$collision_prime_factor)
    final_cols=c(final_cols, "collision_prime_factor")
  }
  
  if ("collision_violation_cat" %in% cols){
    unique(df$collision_violation_cat)
    df$collision_violation_cat[df$collision_violation_cat=="-"]=NA
    df$collision_violation_cat=as.numeric(df$collision_violation_cat)
    df$collision_violation_cat=as.factor(df$collision_violation_cat)
    final_cols=c(final_cols, "collision_violation_cat")
  }
  
  if ("collision_surface_cond" %in% cols){
    unique(df$collision_surface_cond)
    df$collision_surface_cond[df$collision_surface_cond=="-"]=NA
    df$collision_surface_cond=as.factor(df$collision_surface_cond)
    final_cols=c(final_cols, "collision_surface_cond")
  }
  
  if ("collision_road_cond_1" %in% cols){
    unique(df$collision_road_cond_1)
    df$collision_road_cond_1[df$collision_road_cond_1=="-"]=NA
    df$collision_road_cond_1=as.factor(df$collision_road_cond_1)
    final_cols=c(final_cols, "collision_road_cond_1")
  }
  
  if ("collision_road_cond_2" %in% cols){
    unique(df$collision_road_cond_2)
    df$collision_road_cond_2[df$collision_road_cond_2=="-"]=NA
    df$collision_road_cond_2=as.factor(df$collision_road_cond_2)
    final_cols=c(final_cols, "collision_road_cond_2")
  }
  if ("collision_lighting_cond" %in% cols){
    unique(df$collision_lighting_cond)
    df$collision_lighting_cond[df$collision_lighting_cond=="-"]=NA
    df$collision_lighting_cond=as.factor(df$collision_lighting_cond)
    final_cols=c(final_cols, "collision_lighting_cond")
  }
  
  if ("collision_control_device" %in% cols){
    unique(df$collision_control_device)
    df$collision_control_device[df$collision_control_device=="-"]=NA
    df$collision_control_device=as.factor(df$collision_control_device)
    final_cols=c(final_cols, "collision_control_device")
  }
  
  if ("collision_road_type" %in% cols){
    unique(df$collision_road_type)
    df$collision_road_type[df$collision_road_type=="-"]=NA
    df$collision_road_type=as.factor(df$collision_road_type)
    final_cols=c(final_cols, "collision_road_type")
  }
  
  #re-arrange columns
  df=select(df, final_cols)
  return(df)
}
