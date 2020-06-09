setwd("//ahmct-065/teams/PMRF/Amir/")

library(data.table)
library(dplyr)
library(tidyr)
library(caret)
library(anytime)
library(e1071)
library(DMwR)
library(glmnet)

library(xgboost)
library(DiagrammeR)
library(ggplot2)

library(Ckmeans.1d.dp)
library(devtools)
library(xgboostExplainer)


set.seed(123)

df=fread(file="./bin/LEMO_CHP.by.roadCond_workOrderDate.csv", sep=",", header=TRUE)
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

#clean up the selected features and convert to type
source("./Codes/FUNC(Dataset CleanUp).R")
df=cleanUp_Dataset(df, selected_cols)

#check clean up process
df %>% str

#filter rows for a complete data set, in that, no features except collision and closure features should be missing
df=na.omit(setDT(df), cols = c("work_month", "work_day", "district", "county", "route", "activity", "work_duration", "work_length", 
                               "surface_type", "num_lanes", "road_use", "road_width", "median_type", "barrier_type", "hwy_group", 
                               "access_type", "terrain_type", "road_speed", "road_adt", "population_code", 
                               "peak_aadt", "aadt", "truck_aadt", "collision_density11_12"))

#check and plot response variable class
length(which(df$collision_id==1))/length(df$collision_id)
ggplot(data=df, aes(x=collision_id, fill=collision_id))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"), legend.position = "none")+
  ylab("Count")+
  xlab("Class collision")

#create training and testing data set
train.ind=createDataPartition(df$collision_id, times = 1, p=0.7, list = FALSE)
training.df=df[train.ind, ]
testing.df=df[-train.ind, ]
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
#drop collision and closure columns, some of NA variabels can be translated to 0-1 categories or numerics
temp.df=training.df[,-c("closure_workType", "closure_duration", "closure_type", "closure_facility")]
temp.df$closure_cozeepMazeep=ifelse(is.na(temp.df$closure_cozeepMazeep), 0, 1)
temp.df$closure_detour=ifelse(is.na(temp.df$closure_detour), 0, 1)

temp.df=temp.df[,-c("closure_lanes")]
temp.df$closure_coverage[is.na(temp.df$closure_coverage)]=0
temp.df$closure_coverage=abs(temp.df$closure_coverage)
temp.df$closure_length[is.na(temp.df$closure_length)]=0

temp.df=temp.df[,-c("collision_time", "collision_day", "collision_weather_cond_1", "collision_weather_cond_2", 
                                    "collision_location_type", "collision_ramp_intersection", "collision_severity", "collision_prime_factor", 
                                    "collision_violation_cat", "collision_surface_cond", "collision_road_cond_1", "collision_road_cond_2", 
                                    "collision_lighting_cond", "collision_control_device", "collision_road_type")]

temp.df=temp.df[,-c("collision_num_killed", "collision_num_injured", "collision_party_count")]

test.df=testing.df[,-c("closure_workType", "closure_duration", "closure_type", "closure_facility")]
test.df$closure_cozeepMazeep=ifelse(is.na(test.df$closure_cozeepMazeep), 0, 1)
test.df$closure_detour=ifelse(is.na(test.df$closure_detour), 0, 1)

test.df=test.df[,-c("closure_lanes")]
test.df$closure_coverage[is.na(test.df$closure_coverage)]=0
test.df$closure_coverage=abs(test.df$closure_coverage)
test.df$closure_length[is.na(test.df$closure_length)]=0

test.df=test.df[,-c("collision_time", "collision_day", "collision_weather_cond_1", "collision_weather_cond_2", 
                    "collision_location_type", "collision_ramp_intersection", "collision_severity", "collision_prime_factor", 
                    "collision_violation_cat", "collision_surface_cond", "collision_road_cond_1", "collision_road_cond_2", 
                    "collision_lighting_cond", "collision_control_device", "collision_road_type")]

test.df=test.df[,-c("collision_num_killed", "collision_num_injured", "collision_party_count")]
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
#prepare sparse matrices for xgboost
dtest=sparse.model.matrix(collision_id~.-1, data = data.frame(test.df))
dtrain=sparse.model.matrix(collision_id~.-1, temp.df)
label=as.numeric(as.character(training.df$collision_id))

#evaluate the weight of each class in response variable
sumwpos=sum(label==1)
sumwneg=sum(label==0)

#train the xgboost model
xgb.mod=xgboost(data = dtrain, label = label, max.depth=10, eta=0.1, nthread=3, scale_pos_weight=sumwneg/sumwpos,
                eval_metric="auc", nrounds=100, objective="binary:logistic")

#evaluate and plot feature importance
importance=xgb.importance(feature_names = colnames(dtrain), model = xgb.mod)
(gg=xgb.ggplot.importance(importance_matrix = importance[1:20,]))
gg+theme(plot.title = element_text(angle = 0, size=24, face = "bold"),
         axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
         axis.title.x = element_text(size = 20, face="bold"),
         axis.text.y = element_text(size=14),
         axis.title.y = element_text(size=20, face = "bold"), legend.position = "none")+
  xlab("Features")+
  ylab("Average relative contribution to the loss reduction gained when using a feature")+
  ggtitle("Feature Importance")

#predict the test data
temp.predict=predict(xgb.mod, dtest)
temp.predict=as.numeric(temp.predict > 0.5)
confusionMatrix(as.factor(temp.predict), as.factor(testing.df$collision_id), positive = "1")

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
#fit glm to the top features
featureSet=importance$Feature[1:30]
x=dtrain[, colnames(dtrain) %in% featureSet]
weights=ifelse(label==0, 1, sumwneg/sumwpos)

#glm.mod=cv.glmnet(x=x, y=label, family="binomial", weights=weights, nfolds=5, type.logistic="modified.Newton", type.measure="auc", trace.it = 1)
glm.mod=cv.glmnet(x=x, y=label, family="binomial", nfolds=5, type.logistic="modified.Newton", type.measure="auc", trace.it = 1)
plot(glm.mod)
coefficients(glm.mod, glm.mod$lambda.min)
temp.predict=predict(glm.mod, newx = dtest[, colnames(dtrain) %in% featureSet], s=glm.mod$lambda.min)
temp.predict=as.numeric(temp.predict > 0.5)
confusionMatrix(as.factor(temp.predict), as.factor(testing.df$collision_id), positive = "1")
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
#interpret each feature
dmtrain=xgb.DMatrix(data = dtrain, label=label)
dmtest=xgb.DMatrix(data=dtest, label=as.numeric(as.character(testing.df$collision_id)))

explainer = buildExplainer(xgb.mod, dmtrain, type="binary", base_score = 0.5, trees_idx = NULL)
pred.breakdown = explainPredictions(xgb.mod, explainer, dmtest)

cat('Breakdown Complete','\n')
weights = rowSums(pred.breakdown)
pred.xgb = 1/(1+exp(-weights))
cat(max(temp.predict-pred.xgb),'\n')

idx_to_get = as.integer(802)

test.df[idx_to_get, ]
showWaterfall(xgb.mod, explainer, dmtest, data.matrix(test.df) ,idx_to_get, type = "binary")
####### IMPACT AGAINST VARIABLE VALUE
plot(test.df[,closure_id], pred.breakdown[,closure_id1], cex=0.4, pch=16, 
     xlab = "Closure class", ylab = "closure class impact on log-odds")
plot(test.df[,work_length], pred.breakdown[,work_length], cex=0.4, pch=16, 
     xlab = "work length", ylab = "work length (miles) impact on log-odds")
plot(test.df[,collision_density11_12], pred.breakdown[,collision_density11_12], cex=0.4, pch=16, 
     xlab = "Collision density", ylab = "Collision density impact on log-odds")
plot(test.df[,truck_aadt], pred.breakdown[,truck_aadt], cex=0.4, pch=16, 
     xlab = "Collision density", ylab = "Collision density impact on log-odds")
plot(test.df[,road_adt], pred.breakdown[,road_adt], cex=0.4, pch=16, 
     xlab = "Road_adt", ylab = "Road adt impact on log-odds")
plot(test.df[,peak_aadt], pred.breakdown[,peak_adt], cex=0.4, pch=16, 
     xlab = "Road_adt", ylab = "Road adt impact on log-odds")

#cr <- colorRamp(c("blue", "red"))
#plot(test.df[,last_evaluation], pred.breakdown[,last_evaluation], col = rgb(cr(round(test.df[,satisfaction_level])), max=255), cex=0.4, pch=16, xlab = "Last evaluation", ylab = "Last evaluation impact on log-odds")