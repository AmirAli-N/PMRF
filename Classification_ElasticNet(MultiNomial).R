setwd("//ahmct-065/teams/PMRF/Amir/")

library(data.table)
library(dplyr)
library(tidyr)
library(caret)
library(anytime)
library(e1071)
library(DMwR)
library(glmnet)
library(doParallel)

set.seed(123)

df=fread(file="./bin/LEMO_CHP.by.roadCond_workOrderDate.csv", sep=",", header=TRUE)
#df=fread(file="./bin/LEMO_CHP.by.roadCond_closureTime.csv", sep=",", header=TRUE)
#df=fread(file="./bin/LEMO_CHP.by.roadCond.csv", sep=",", header=TRUE)
df[df==""]=NA

#select features
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


#cleanUp features and convert to type
source("./Codes/FUNC_clean(FinalDataSet).R")
df=cleanUp_Dataset(df, selected_cols)

#check clean up process
df %>% str

#filter rows for a complete data set, in that, no features except collision and closure features should be missing
df=na.omit(setDT(df), cols = c("work_month", "work_day", "district", "county", "route", "activity", "work_duration", "work_length", 
                        "surface_type", "num_lanes", "road_use", "road_width", "median_type", "barrier_type", "hwy_group", 
                        "access_type", "terrain_type", "road_speed", "road_adt", "population_code", 
                        "peak_aadt", "aadt", "truck_aadt", "collision_density11_12"))

####FOR MULTINOMIAL REGRESSION ONLY####
unique(df$collision_severity)
df$collision_severity[df$collision_severity %in% c(1, 2, 3, 4)]=2 #for symptomatic injury or fatality
df$collision_severity[df$collision_severity==0]=1 #for PDO
df$collision_severity[is.na(df$collision_severity)]=0 #for no collision
df$collision_severity=droplevels(df$collision_severity)
unique(df$collision_severity)

#check and plot the proportion of response variable classes
length(which(df$collision_id==1))/length(df$collision_id)
ggplot(data=df, aes(x=collision_severity, fill=collision_severity))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"), legend.position = "none")+
  ylab("count")+
  xlab("collision severity")+
  labs(fill="collision")

#SMOTE balanced sampling
balanced01.df=SMOTE(collision_severity~., 
                    data = droplevels.data.frame(df[which(df$collision_severity %in% c(0,1)),]), 
                    perc.over = 200, perc.under = 200, k = 5)
balanced02.df=SMOTE(collision_severity~., 
                    data = droplevels.data.frame(df[which(df$collision_severity %in% c(0,2)),]), 
                    perc.over = 200, perc.under = 200, k = 5)

balanced.df=rbind(balanced01.df, balanced02.df)
balanced.df=balanced.df %>% distinct()

#create training and testing splits
train.ind=createDataPartition(balanced.df$collision_severity, times = 1, p=0.7, list = FALSE)
training.df=balanced.df[train.ind, ]
testing.df=balanced.df[-train.ind, ]

#check and plot the proportion of response variable classes
length(which(balanced.df$collision_id==1))/length(balanced.df$collision_id)
ggplot(data=training.df, aes(x=collision_severity, fill=collision_severity))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"), legend.position = "none")+
  ylab("count")+
  xlab("collision id")+
  labs(fill="collision")

#process the balanced data set for categorical and numerical variables
balanced.cat.df=training.df %>% select_if(is.factor)
`isnot.factor` = Negate(`is.factor`)
balanced.num.df=training.df %>% select_if(isnot.factor)

#drop collision and closure columns, some of NA variabels can be translated to 0-1 categories or numerics
balanced.cat.df=balanced.cat.df[,-c("closure_workType", "closure_duration", "closure_type", "closure_facility")]
balanced.cat.df$closure_cozeepMazeep=ifelse(is.na(balanced.cat.df$closure_cozeepMazeep), 0, 1)
balanced.cat.df$closure_detour=ifelse(is.na(balanced.cat.df$closure_detour), 0, 1)

balanced.num.df=balanced.num.df[,-c("closure_lanes")]
balanced.num.df$closure_coverage[is.na(balanced.num.df$closure_coverage)]=0
balanced.num.df$closure_coverage=abs(balanced.num.df$closure_coverage)
balanced.num.df$closure_length[is.na(balanced.num.df$closure_length)]=0

balanced.cat.df=balanced.cat.df[,-c("collision_time", "collision_day", "collision_weather_cond_1", "collision_weather_cond_2", 
                                    "collision_location_type", "collision_ramp_intersection", "collision_prime_factor", 
                                    "collision_violation_cat", "collision_surface_cond", "collision_road_cond_1", "collision_road_cond_2", 
                                    "collision_lighting_cond", "collision_control_device", "collision_road_type")]

balanced.num.df=balanced.num.df[,-c("collision_num_killed", "collision_num_injured", "collision_party_count")]

#take the response vector
y=unlist(balanced.cat.df[,"collision_severity"])
balanced.cat.df=setDF(balanced.cat.df)[,!colnames(balanced.cat.df)%in% c("collision_severity")]

#convert categorical variables to dummy binaries
dummy.mod=dummyVars(~., data = balanced.cat.df, fullRank = TRUE, drop2nd=TRUE)
balanced.cat.df=predict(dummy.mod, newdata = balanced.cat.df)

#preprocess numeric variables and center+scale them to range 0-1
preprocess.mod=preProcess(balanced.num.df, method = c("center", "scale"), rangeBounds = c(0, 1))
balanced.num.df=predict(preprocess.mod, balanced.num.df)
balanced.num.df=data.matrix(balanced.num.df)

#join the two matrix for more preprocessing
training.df=cbind(balanced.cat.df, balanced.num.df)
rm(balanced.cat.df, balanced.num.df, balanced.df)

#remove near zero variance
nzv=nearZeroVar(training.df)
training.df=training.df[, -nzv]

#remove multicollinearity
descrCor=cor(training.df)
highlyCorDescr=findCorrelation(descrCor, cutoff = .75)
training.df=training.df[, -highlyCorDescr]

#remove linear dependencies
comboInfo=findLinearCombos(training.df)
if (length(comboInfo$remove) > 0) {
  training.df=training.df[, -comboInfo$remove] 
}

#check the remaining variables
colnames(training.df)
if("collision_id.1" %in% colnames(training.df)){
  training.df=training.df[,!colnames(training.df) %in% c("collision_id.1")]
}
###################################################################################################################
###################################################################################################################
####################################################################################################### Elastic net
y=as.numeric(as.character(y))
training.df=as(as.matrix(training.df), "dgCMatrix")

#### for imabalanced data ##############################
#evaluate the weight of each class in response variable
#sumwpos=sum(y==1)
#sumwneg=sum(y==0)
#weights=ifelse(y==0, 1, sumwneg/sumwpos)
#elastic.mod=cv.glmnet(x=dtrain, y=y, family="binomial", weights=weights, nfolds=5, type.logistic="modified.Newton", type.measure="auc", trace.it = 1)
########################################################

## using the glmnet library

n_cores=detectCores()
my_cluster=makeCluster(n_cores)
registerDoParallel(my_cluster)

training.df=sparse.model.matrix(y~.-1, data = data.frame(training.df))
elastic.mod=cv.glmnet(x=training.df, y=y, family="multinomial", nfolds=5, type.multinomial="grouped",
                      type.logistic="modified.Newton", type.measure="deviance", trace.it = 1, parallel = TRUE)
stopCluster(my_cluster)
plot(elastic.mod)
coefficients(elastic.mod, elastic.mod$lambda.min)

## using the caret library
#preprocess.df=cbind.data.frame(y, preprocess.df)
#trCtrl=trainControl(method = "repeatedcv", index=index, repeats = 1, search = "random", verboseIter = TRUE)
#elastic.mod=train(as.factor(y)~., data=balanced.df, method="glmnet", tuneLength=25, trControl=trCtrl)
#coef(elastic.mod$finalModel, elastic.mod$bestTune$lambda)

####################################################################################################################
####################################################################################################################
######################################################################################################### Prediction

#testing.df=fread(file="./bin/test(severity3class)_by.roadCondition_closureTime.csv", sep=",", header=TRUE)
testing.df=fread(file="./bin/test(severity4class)_by.roadCondition_workOrderDate.csv", sep=",", header=TRUE)
y_test=unlist(testing.df[,"collision_severity"])

test.matrix=setDF(testing.df)[, names(testing.df) %in% colnames(training.df)]
test.matrix=data.matrix(test.matrix)

predicted.net=predict(elastic.mod, test.matrix, s=elastic.mod$lambda.min, type="class")
confusionMatrix(as.factor(predicted.net), as.factor(y_test))