#create test data
setwd("//ahmct-065/teams/PMRF/Amir/")

library(data.table)
library(dplyr)
library(tidyr)
library(caret)
library(anytime)
library(e1071)


set.seed(123)

#df=fread(file="./bin/LEMO_CHP.by.roadCond.csv", sep=",", header=TRUE)
#df=fread(file="./bin/LEMO_CHP.by.roadCond_closureTime.csv", sep=",", header=TRUE)
df=fread(file="./bin/LEMO_CHP.by.roadCond_workOrderDate.csv", sep=",", header=TRUE)
df[df==""]=NA

colnames(df)
selected_cols=c("work_date", "activity", "district", "county", "route", "work_duration", "work_length", 
                "closure_id", "closure_coverage", "closure_length", "closure_workType", "closure_duration", "closure_cozeepMazeep", 
                "closure_detour", "closure_type", "closure_facility", "closure_lanes",
                "surface_type", "num_lanes", "road_use", "road_width", "median_type", "barrier_type", "hwy_group", "access_type", 
                "terrain_type", "road_speed", "road_adt", "population_code", "peak_aadt", "aadt", "truck_aadt", "collision_density11_12", "collision_id", 
                "collision_time", "collision_day", "collision_weather_cond_1", "collision_weather_cond_2", "collision_location_type", 
                "collision_ramp_intersection", "collision_severity", "collision_num_killed", "collision_num_injured", "collision_party_count", 
                "collision_prime_factor", "collision_violation_cat", "collision_surface_cond", "collision_road_cond_1", "collision_road_cond_2", 
                "collision_lighting_cond", "collision_control_device", "collision_road_type")


source("./Codes/FUNC_clean(FinalDataSet).R")
df=cleanUp_Dataset(df, selected_cols)

#check clean up process
df %>% str

#filter rows for a complete data set, in that, no features except collision and closure features should be missing
df=na.omit(setDT(df), cols = c("work_month", "work_day", "district", "county", "route", "activity", "work_duration", "work_length", 
                        "surface_type", "num_lanes", "road_use", "road_width", "median_type", "barrier_type", "hwy_group", 
                        "access_type", "terrain_type", "road_speed", "road_adt", "population_code", 
                        "peak_aadt", "aadt", "truck_aadt", "collision_density11_12"))

#######################################################################################################################################
####FOR MULTINOMIAL REGRESSION ONLY####
###for three classes
#unique(df$collision_severity)
#df$collision_severity[df$collision_severity %in% c(1, 2, 3, 4)]=2 #for symptomatic injury or fatality
#df$collision_severity[df$collision_severity==0]=1                 #for PDO
#df$collision_severity[is.na(df$collision_severity)]=0             #for no collision
#df$collision_severity=droplevels(df$collision_severity)
#unique(df$collision_severity)

unique(df$collision_severity)
df$collision_severity=factor(df$collision_severity, levels = c(levels(df$collision_severity), "F"))
df$collision_severity[df$collision_severity %in% c(1, 2)]="F" #for severe injury or fatality
df$collision_severity[df$collision_severity %in% c(3, 4)]=2 #for visible injury and complaint of injury
df$collision_severity[df$collision_severity==0]=1           #for PDO
df$collision_severity[is.na(df$collision_severity)]=0       #for no collision
df$collision_severity[df$collision_severity =="F"]="3"
df$collision_severity=droplevels(df$collision_severity)
unique(df$collision_severity)
########################################################################################################################################
#split data
train.ind=createDataPartition(df$collision_severity, times = 1, p=0.7, list = FALSE)
testing.df=df[-train.ind, ]

testing.cat.df=testing.df %>% select_if(is.factor)
`isnot.factor` = Negate(`is.factor`)
testing.num.df=testing.df %>% select_if(isnot.factor)

#drop collision and closure columns, some of NA variabels can be translated to 0-1 categories or numerics
testing.cat.df=testing.cat.df[,-c("closure_workType", "closure_duration", "closure_type", "closure_facility")]
testing.cat.df$closure_cozeepMazeep=ifelse(is.na(testing.cat.df$closure_cozeepMazeep), 0, 1)
testing.cat.df$closure_detour=ifelse(is.na(testing.cat.df$closure_detour), 0, 1)

testing.num.df=testing.num.df[,-c("closure_lanes")]
testing.num.df$closure_coverage[is.na(testing.num.df$closure_coverage)]=0
testing.num.df$closure_coverage=abs(testing.num.df$closure_coverage)
testing.num.df$closure_length[is.na(testing.num.df$closure_length)]=0

#testing.cat.df=testing.cat.df[,-c("collision_time", "collision_day", "collision_weather_cond_1", "collision_weather_cond_2", 
#                                  "collision_location_type", "collision_ramp_intersection", "collision_severity", "collision_prime_factor", 
#                                  "collision_violation_cat", "collision_surface_cond", "collision_road_cond_1", "collision_road_cond_2", 
#                                  "collision_lighting_cond", "collision_control_device", "collision_road_type")]

#########################################################################################################################################
####FOR MULTINOMIAL REGRESSION ONLY####
testing.cat.df=testing.cat.df[,-c("collision_time", "collision_day", "collision_weather_cond_1", "collision_weather_cond_2", 
                                  "collision_location_type", "collision_ramp_intersection", "collision_prime_factor", 
                                  "collision_violation_cat", "collision_surface_cond", "collision_road_cond_1", "collision_road_cond_2", 
                                  "collision_lighting_cond", "collision_control_device", "collision_road_type")]

y_test=unlist(testing.cat.df[,"collision_severity"])
testing.cat.df=setDF(testing.cat.df)[, !colnames(testing.cat.df) %in% c("collision_severity")]
#########################################################################################################################################

testing.num.df=testing.num.df[,-c("collision_num_killed", "collision_num_injured", "collision_party_count")]

#dummy variables
dummy.mod=dummyVars(~., data = testing.cat.df, fullRank = TRUE)
testing.cat.df=predict(dummy.mod, newdata = testing.cat.df)

#preprocess numeric variables
preprocess.mod=preProcess(testing.num.df, method = c("center", "scale"), rangeBounds = c(0, 1))
testing.num.df=predict(preprocess.mod, testing.num.df)
testing.num.df=data.matrix(testing.num.df)

#join the two matrix for more preprocessing
#testing.df=cbind.data.frame(testing.cat.df, testing.num.df)
testing.df=cbind.data.frame(testing.cat.df, testing.num.df, "collision_severity"=y_test)

fwrite(testing.df, file = "./bin/test(severity4class)_by.roadCondition_workOrderDate.csv", sep=",", append = FALSE)
#fwrite(testing.df, file = "./bin/test(severity4class)_by.roadCondition_closureTime.csv", sep=",", append = FALSE)
rm(testing.cat.df, testing.num.df)
