setwd("//ahmct-065/teams/PMRF/Amir/")

library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(caret)
library(anytime)
library(e1071)
library(DMwR)
library(glmnet)
library(glmnetcr)
library(hrbrthemes)
library(viridis)
library(ggrepel)
library(forcats)
library(magrittr)
library(ordinal)

set.seed(123)

#df=fread(file="./bin/LEMO_CHP.by.roadCond_workOrderDate.csv", sep=",", header=TRUE)
#df=fread(file="./bin/LEMO_CHP.by.roadCond_closureTime.csv", sep=",", header=TRUE)
df=fread(file="./bin/LEMO_CHP.by.roadCond.csv", sep=",", header=TRUE)
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
source("./Codes/PMRF-Data analysis/FUNC_clean(FinalDataSet).R")
df=cleanUp_Dataset(df, selected_cols)

#check clean up process
df %>% str

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

#for closure columns, some of NA variabels can be translated to 0-1 categories or numerics
df$closure_cozeepMazeep=ifelse(is.na(df$closure_cozeepMazeep), 0, 1)
df$closure_detour=ifelse(is.na(df$closure_detour), 0, 1)
df=df[,-c("collision_time", "collision_day", "collision_weather_cond_1", 
          "collision_weather_cond_2", "collision_location_type", 
          "collision_ramp_intersection", "collision_prime_factor", 
          "collision_violation_cat", "collision_surface_cond", 
          "collision_road_cond_1", "collision_road_cond_2", 
          "collision_lighting_cond", "collision_control_device", 
          "collision_road_type", "collision_num_killed", 
          "collision_num_injured", "collision_party_count")]

df$closure_coverage[is.na(df$closure_coverage)]=0
df$closure_coverage=abs(df$closure_coverage)
df$closure_length[is.na(df$closure_length)]=0

#split train and test
train.ind=createDataPartition(df$collision_id, times = 1, p=0.7, list = FALSE)
train.df=df[train.ind, ]
test.df=df[-train.ind]

#balance the data set
bal_train.df=SMOTE(collision_id~., data = train.df, perc.over = 1000, perc.under = 200, k=5)

ggplot(data=bal_train.df, aes(x=collision_id, fill=collision_id))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"), legend.position = "none")+
  ylab("count")+
  xlab("collision id")+
  labs(fill="collision")

ggplot(data=bal_train.df, aes(x=collision_severity, fill=collision_severity))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"), legend.position = "none")+
  ylab("count")+
  xlab("collision id")+
  labs(fill="collision")

#response variables
x_train=bal_train.df[, -c("collision_id", "collision_severity")]
x_test=test.df[, -c("collision_id", "collision_severity")]

y_train=bal_train.df[, c("collision_id", "collision_severity")]
y_test=test.df[, c("collision_id", "collision_severity")]

#generating dummies for categorical variables, and dimension reduction
x_train$closure_lanes=fct_explicit_na(x_train$closure_lanes, na_level = "N/A")
x_train$closure_facility=fct_explicit_na(x_train$closure_facility, na_level = "N/A")
x_train$closure_type=fct_explicit_na(x_train$closure_type, na_level = "N/A")
x_train$closure_duration=fct_explicit_na(x_train$closure_duration, na_level = "N/A")
x_train$closure_workType=fct_explicit_na(x_train$closure_workType, na_level = "N/A")

x_test$closure_lanes=fct_explicit_na(x_test$closure_lanes, na_level = "N/A")
x_test$closure_facility=fct_explicit_na(x_test$closure_facility, na_level = "N/A")
x_test$closure_type=fct_explicit_na(x_test$closure_type, na_level = "N/A")
x_test$closure_duration=fct_explicit_na(x_test$closure_duration, na_level = "N/A")
x_test$closure_workType=fct_explicit_na(x_test$closure_workType, na_level = "N/A")

dummy.mod=dummyVars(~., data = x_train, fullRank = TRUE, drop2nd=TRUE)
x_train=predict(dummy.mod, newdata = x_train)
dummy.mod=dummyVars(~., data = x_test, fullRank = TRUE, drop2nd=TRUE)
x_test=predict(dummy.mod, newdata = x_test)

#remove near zero variance
nzv=nearZeroVar(x_train)
x_train=x_train[, -nzv]
x_test=x_test[, -nzv]

#remove multicollinearity
descrCor=cor(x_train)
highlyCorDescr=findCorrelation(descrCor, cutoff = .75)
x_train=x_train[, -highlyCorDescr]
x_test=x_test[, -highlyCorDescr]

#remove linear dependencies
comboInfo=findLinearCombos(x_train)
if (length(comboInfo$remove) > 0) {
  x_train=x_train[, -comboInfo$remove]
  x_test=x_test[, -comboInfo$remove]
}

#scale
x_train=apply(x_train, 2, function(col) reshape::rescaler(col, type="range"))
x_test=apply(x_test, 2, function(col) reshape::rescaler(col, type="range"))

#model fitting
y_class=y_train$collision_severity
y_class[y_class==1 | y_class==2 | y_class==3 | y_class==4]=2
y_class[y_class==0]=1
y_class[is.na(y_class)]=0
y_class=droplevels.factor(y_class)

w_sum=length(y_class)
w_0=length(which(y_class==2))/w_sum
w_1=length(which(y_class==1))/w_sum
w_2=length(which(y_class==0))/w_sum
w=rep(0, length(y_class))
w[which(y_class==0)]=w_0
w[which(y_class==1)]=w_1
w[which(y_class==2)]=w_2

y_test_class=y_test$collision_severity
y_test_class[y_test_class==1 | y_test_class==2 | y_test_class==3 | y_test_class==4]=2
y_test_class[y_test_class==0]=1
y_test_class[is.na(y_test_class)]=0
y_test_class=droplevels.factor(y_test_class)

x_train=as.matrix(x_train, "dgCMatrix")
ord.mod=glmnetcr(x_train, 
                 y_class,
                 weights = w,
                 method = "backward", 
                 standardize = TRUE, 
                 type.logistic = "modified.Newton", 
                 trace.it=1,
                 pmax = 100,
                 dfmax = 100,
                 alpha = 1,
                 maxit = 200)

ord.link=clm(y_class~. , data = data.frame(x_train), weights = w,
             link = "logit", doFit = TRUE)

print(ord.mod)
best.fit=select.glmnetcr(ord.mod, which = "BIC")
coef(ord.mod, s=best.fit)

pred=predict(ord.mod, newx = x_test)
pred=predict(ord.link, newdata=test, type="class")
confusionMatrix(as.factor(pred$class[, best.fit]), as.factor(y_test_class))
confusionMatrix(as.factor(pred$fit), as.factor(y_test_class))
                