library(data.table)
library(dplyr)
library(tidyr)

#read the files
df.length.duration=fread(file = "act.by.length+duration.csv", sep=",", header = TRUE)
df.cost=fread(file="act.by.cost.csv", sep=",", header = TRUE)
df.access.prop=fread(file = "act.by.access.prop.csv", sep=",", header = TRUE)
df.cl.prop=fread(file="act.by.closure.prop.csv", sep=",", header = TRUE)
df.crew.prop=fread(file = "act.by.crew.prop.csv", sep=",", header = TRUE)



df.final=merge(df.length.duration, df.cost, by=c("activity", "activity_descr"))
df.final=merge(df.final, df.access.prop, by=c("activity", "activity_descr"))
df.final=cbind.data.frame(df.final,
                          prop_clousre=df.cl.prop$closure_prop[match(df.final$activity, 
                                                                     df.cl.prop$activity)])
df.final=cbind.data.frame(df.final,
                          crew_score=df.crew.prop$crew.score[match(df.final$activity, 
                                                                   df.crew.prop$activity)])

df.final=df.final %>% mutate(overall.score=4.66*prop_clousre+
                               4.47*crew_score+
                               4.41*access_score+
                               4.25*mean_duration+
                               4.01*mean_length+
                               3.92*cost.mean)

df.final=df.final %>% mutate(overall.score1=4.66*prop_clousre+
                               #4.47*crew_score+
                               4.41*access_score+
                               4.25*mean_duration+
                               4.01*mean_length+
                               3.92*cost.mean)

df.final=setDT(df.final)[order(-overall.score),,]
df.final=setDT(df.final)[order(-overall.score1),,]
fwrite(df.final, file = "activity.by.difficulty.csv", sep=",", append = FALSE)