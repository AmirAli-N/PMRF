setwd("//ahmct-065/teams/PMRF/Amir/")

library(data.table)
library(dplyr)
library(tidyr)
library(caret)
library(anytime)
library(e1071)
library(DMwR)
library(glmnet)

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
source("./Codes/FUNC(Dataset CleanUp).R")
df=cleanUp_Dataset(df, selected_cols)

#check clean up process
df %>% str

#filter rows for a complete data set, in that, no features except collision and closure features should be missing
df=na.omit(setDT(df), cols = c("work_month", "work_day", "district", "county", "route", "activity", "work_duration", "work_length", 
                        "surface_type", "num_lanes", "road_use", "road_width", "median_type", "barrier_type", "hwy_group", 
                        "access_type", "terrain_type", "road_speed", "road_adt", "population_code", 
                        "peak_aadt", "aadt", "truck_aadt", "collision_density11_12"))

#check and plot the proportion of response variable classes
length(which(df$collision_id==1))/length(df$collision_id)
ggplot(data=df, aes(x=collision_id, fill=collision_id))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"), legend.position = "none")+
  ylab("count")+
  xlab("collision id")+
  labs(fill="collision")

#create training and testing splits
train.ind=createDataPartition(df$collision_id, times = 1, p=0.7, list = FALSE)
training.df=df[train.ind, ]
testing.df=df[-train.ind, ]

#SMOTE balanced sampling
balanced.df=SMOTE(collision_id~., data = training.df, perc.over = 200, perc.under = 200, k = 5)

###################################################
##ONLY FOR LEMO_CHP.by.roadCond_workOrderDate.csv##
##no need to balance
#balanced.df=training.df
###################################################

#check and plot the proportion of response variable classes
length(which(balanced.df$collision_id==1))/length(balanced.df$collision_id)
ggplot(data=balanced.df, aes(x=collision_id, fill=collision_id))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"), legend.position = "none")+
  ylab("count")+
  xlab("collision id")+
  labs(fill="collision")

#process the balanced data set for categorical and numerical variables
balanced.cat.df=balanced.df %>% select_if(is.factor)
`isnot.factor` = Negate(`is.factor`)
balanced.num.df=balanced.df %>% select_if(isnot.factor)

#drop collision and closure columns, some of NA variabels can be translated to 0-1 categories or numerics
balanced.cat.df=balanced.cat.df[,-c("closure_workType", "closure_duration", "closure_type", "closure_facility")]
balanced.cat.df$closure_cozeepMazeep=ifelse(is.na(balanced.cat.df$closure_cozeepMazeep), 0, 1)
balanced.cat.df$closure_detour=ifelse(is.na(balanced.cat.df$closure_detour), 0, 1)

balanced.num.df=balanced.num.df[,-c("closure_lanes")]
balanced.num.df$closure_coverage[is.na(balanced.num.df$closure_coverage)]=0
balanced.num.df$closure_coverage=abs(balanced.num.df$closure_coverage)
balanced.num.df$closure_length[is.na(balanced.num.df$closure_length)]=0

balanced.cat.df=balanced.cat.df[,-c("collision_time", "collision_day", "collision_weather_cond_1", "collision_weather_cond_2", 
                                    "collision_location_type", "collision_ramp_intersection", "collision_severity", "collision_prime_factor", 
                                    "collision_violation_cat", "collision_surface_cond", "collision_road_cond_1", "collision_road_cond_2", 
                                    "collision_lighting_cond", "collision_control_device", "collision_road_type")]

balanced.num.df=balanced.num.df[,-c("collision_num_killed", "collision_num_injured", "collision_party_count")]

#convert categorical variables to dummy binaries
balanced.cat.df$collision_id=as.factor(balanced.cat.df$collision_id)
dummy.mod=dummyVars(~., data = balanced.cat.df, fullRank = TRUE, drop2nd=TRUE)
balanced.cat.df=predict(dummy.mod, newdata = balanced.cat.df)

#preprocess numeric variables and center+scale them to range 0-1
preprocess.mod=preProcess(balanced.num.df, method = c("center", "scale"), rangeBounds = c(0, 1))
balanced.num.df=predict(preprocess.mod, balanced.num.df)
balanced.num.df=data.matrix(balanced.num.df)

#join the two matrix for more preprocessing
preprocess.df=cbind(balanced.cat.df, balanced.num.df)
y=preprocess.df[,"collision_id.1"]
preprocess.df=preprocess.df[,!colnames(preprocess.df) %in% c("collision_id.1")]
#rm(balanced.cat.df, balanced.num.df)

#remove near zero variance
nzv=nearZeroVar(preprocess.df)
preprocess.df=preprocess.df[, -nzv]

#remove multicollinearity
descrCor=cor(preprocess.df)
highlyCorDescr=findCorrelation(descrCor, cutoff = .75)
preprocess.df=preprocess.df[, -highlyCorDescr]

#remove linear dependencies
comboInfo=findLinearCombos(preprocess.df)
if (length(comboInfo$remove) > 0) {
  preprocess.df=preprocess.df[, -comboInfo$remove] 
}
###################################################################################################################
###################################################################################################################
####################################################################################################### Elastic net
y=as.numeric(as.character(y))

#### for imabalanced data ##############################
#evaluate the weight of each class in response variable
#sumwpos=sum(y==1)
#sumwneg=sum(y==0)
#weights=ifelse(y==0, 1, sumwneg/sumwpos)
#elastic.mod=cv.glmnet(x=dtrain, y=y, family="binomial", weights=weights, nfolds=5, type.logistic="modified.Newton", type.measure="auc", trace.it = 1)
########################################################

## using the glmnet library
elastic.mod=cv.glmnet(x=preprocess.df, y=y, family="binomial", nfolds=5, type.logistic="modified.Newton", type.measure="auc", trace.it = 1)
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
testing.df=fread(file="./bin/test_by.roadCondition_workOrderDate.csv", sep=",", header=TRUE)
names(testing.df)[which(names(testing.df)=="collision_id.1")]="y"

## for glmnet predict method
test.matrix=setDF(testing.df)[, names(testing.df) %in% colnames(preprocess.df)]
test.matrix=data.matrix(test.matrix)
predicted.net=predict(elastic.mod, test.matrix, s=glm.mod$lambda.min)
predicted.net=as.numeric(predicted.net > 0.5)
confusionMatrix(as.factor(predicted.net), as.factor(testing.df$y), positive = "1")

## for caret predict method
#predicted.net=predict(elastic.mod, testing.df)
#confusionMatrix(as.factor(predicted.net), as.factor(testing.df$y), positive = "1")