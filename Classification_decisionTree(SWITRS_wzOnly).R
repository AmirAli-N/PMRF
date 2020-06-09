library(data.table)
library(caret)
library(dplyr)
library(ROSE)
library(DMwR)
library(rpart.plot)

CHP_summary.df=CHP.df[,c("CASE_ID", "COLLISION_TIME", "DAY_OF_WEEK", "POPULATION", "WEATHER_1", "CALTRANS_COUNTY",
                         "CALTRANS_DISTRICT", "STATE_ROUTE", "LOCATION_TYPE", "RAMP_INTERSECTION", "COLLISION_SEVERITY",
                         "PRIMARY_COLL_FACTOR", "PCF_CODE_OF_VIOL", "PCF_VIOL_CATEGORY", "ROAD_SURFACE", "ROAD_COND_1",
                         "LIGHTING", "CONTROL_DEVICE", "PARTY_COUNT")]

CHP_summary.df$COLLISION_SEVERITY[CHP_summary.df$COLLISION_SEVERITY==0]="not severe or fatal"
CHP_summary.df$COLLISION_SEVERITY[CHP_summary.df$COLLISION_SEVERITY==1]="severe or fatal"
CHP_summary.df$COLLISION_SEVERITY[CHP_summary.df$COLLISION_SEVERITY==2]="severe or fatal"
CHP_summary.df$COLLISION_SEVERITY[CHP_summary.df$COLLISION_SEVERITY==3]="not severe or fatal"
CHP_summary.df$COLLISION_SEVERITY[CHP_summary.df$COLLISION_SEVERITY==4]="not severe or fatal"

CHP_summary.df$CASE_ID=as.factor(CHP_summary.df$CASE_ID)
CHP_summary.df$COLLISION_TIME=as.numeric(CHP_summary.df$COLLISION_TIME)
CHP_summary.df$DAY_OF_WEEK=as.factor(CHP_summary.df$DAY_OF_WEEK)
CHP_summary.df$POPULATION=as.factor(CHP_summary.df$POPULATION)
CHP_summary.df$WEATHER_1=as.factor(CHP_summary.df$WEATHER_1)
CHP_summary.df$CALTRANS_COUNTY=as.factor(CHP_summary.df$CALTRANS_COUNTY)
CHP_summary.df$CALTRANS_DISTRICT=as.factor(CHP_summary.df$CALTRANS_DISTRICT)
CHP_summary.df$STATE_ROUTE=as.factor(CHP_summary.df$STATE_ROUTE)
CHP_summary.df$LOCATION_TYPE=as.factor(CHP_summary.df$LOCATION_TYPE)
CHP_summary.df$RAMP_INTERSECTION=as.factor(CHP_summary.df$RAMP_INTERSECTION)
CHP_summary.df$COLLISION_SEVERITY=as.factor(CHP_summary.df$COLLISION_SEVERITY)
CHP_summary.df$PRIMARY_COLL_FACTOR=as.factor(CHP_summary.df$PRIMARY_COLL_FACTOR)
CHP_summary.df$PCF_CODE_OF_VIOL=as.factor(CHP_summary.df$PCF_CODE_OF_VIOL)
CHP_summary.df$PCF_VIOL_CATEGORY=as.factor(CHP_summary.df$PCF_VIOL_CATEGORY)
CHP_summary.df$ROAD_SURFACE=as.factor(CHP_summary.df$ROAD_SURFACE)
CHP_summary.df$ROAD_COND_1=as.factor(CHP_summary.df$ROAD_COND_1)
CHP_summary.df$LIGHTING=as.factor(CHP_summary.df$LIGHTING)
CHP_summary.df$CONTROL_DEVICE=as.factor(CHP_summary.df$CONTROL_DEVICE)
CHP_summary.df$PARTY_COUNT=as.factor(CHP_summary.df$PARTY_COUNT)

CHP_summary.df=CHP_summary.df[!duplicated(CHP_summary.df)]

CHP_summary.df %>% str

train.ind=createDataPartition(as.factor(CHP_summary.df$COLLISION_SEVERITY), p=0.7, list=FALSE, times=1)

#CHP_training.df=CHP_summary.df[train.ind,]
#CHP_training.df=ROSE(COLLISION_SEVERITY~., data = CHP_summary.df, p=0.5)$data
CHP_training.df=SMOTE(COLLISION_SEVERITY~., data = CHP_summary.df, perc.over=100, k=5, perc.under=200)

fit.control=trainControl(method = "cv", number=2)
tree.control=rpart::rpart.control(minsplit = 2, cp=0.01)

tree.fit=train(COLLISION_SEVERITY~COLLISION_TIME+DAY_OF_WEEK+POPULATION+WEATHER_1+CALTRANS_COUNTY+CALTRANS_DISTRICT+
               STATE_ROUTE+LOCATION_TYPE+RAMP_INTERSECTION+ROAD_SURFACE+LIGHTING+CONTROL_DEVICE,
               data = CHP_training.df, method="rpart", trControl=fit.control)

CHP_testing.df=CHP_summary.df[-train.ind,]
pred.tree=predict(tree.fit, newdata = CHP_testing.df)
confusionMatrix(pred.tree, CHP_testing.df$COLLISION_SEVERITY)

rpart.plot(tree.fit$finalModel)

######################################################
######################################################
######################################################
library(randomForest)

train.ind=createDataPartition(as.factor(CHP_summary.df$COLLISION_SEVERITY), p=0.7, list=FALSE, times=1)
#CHP_training.df=ROSE(COLLISION_SEVERITY~., data = CHP_summary.df, p=0.5)$data
CHP_training.df=SMOTE(COLLISION_SEVERITY~., data = CHP_summary.df, perc.over=100, k=5, perc.under=200)

fit.control=trainControl(method="repeatedcv", number = 10, repeats = 3)
mtry=sqrt(ncol(CHP_training.df))
tunegrid=expand.grid(.mtry=mtry)

rf.fit=train(COLLISION_SEVERITY~COLLISION_TIME+DAY_OF_WEEK+POPULATION+WEATHER_1+CALTRANS_COUNTY+CALTRANS_DISTRICT+
                 STATE_ROUTE+LOCATION_TYPE+RAMP_INTERSECTION+ROAD_SURFACE+LIGHTING+CONTROL_DEVICE,
               data = CHP_training.df, method="rf", trControl=fit.control)

CHP_testing.df=CHP_summary.df[-train.ind,]
pred.tree=predict(rf.fit, newdata = CHP_testing.df)
confusionMatrix(pred.tree, CHP_testing.df$COLLISION_SEVERITY)

varImpPlot(rf.fit$finalModel)