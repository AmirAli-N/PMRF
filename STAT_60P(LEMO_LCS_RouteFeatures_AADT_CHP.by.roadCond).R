library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(ggrepel)
library(ggforce)
library(stringr)
library(reshape2)

setwd("Z:/PMRF/Amir/bin/Final Datasets")
df=fread(file="LEMO_LCS_RouteFeauter_AADT_CHP.by.roadCond.csv", sep=",", header = TRUE)
df=na.omit(df, cols = c("work_date", "activity"))
df[df==""]=NA

############################
## activity by number of closure
############################
df.temp=df[,c("wono", "activity", "activity_descr", "closure_id")]
df.temp=setDT(df.temp)[order(activity), .(freqC=length(unique(closure_id)),
                                          freqW=length(unique(wono))), by=.(activity)]
setwd("G:/My Drive/WorkingDesk/CalTrans-60% report")
fwrite(df.temp, file = "act.by.closure.csv", sep=",", append = FALSE)

df.temp=cbind.data.frame(df.temp, family=substring(df.temp$activity, 1, 1))

df.temp=setDT(df.temp)[order(family), .(freqC=sum(freqC),
                                        freqW=sum(freqW)), by=.(family)]



ggplot(na.omit(df.temp), aes(x=family, y=freqC, fill=family))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(angle= 0, size = 18, family = "Century Gothic", 
                                   vjust = 0, hjust = 0.5),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 18, family = "Century Gothic"),
        axis.text.y = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic"))+
  xlab("IMMS family grouping")+
  ylab("Number of lane closures")

ggplot(na.omit(df.temp), aes(x=family, y=freqC/freqW, fill=family))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(angle= 0, size = 18, family = "Century Gothic", 
                                   vjust = 0, hjust = 0.5),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 18, family = "Century Gothic"),
        axis.text.y = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic"))+
  xlab("IMMS family grouping")+
  ylab("Fraction of work orders with lane closure")

############################
## activity by number of collision
############################
df.temp=df[,c("wono", "activity", "collision_id", "collision_severity")]
df.temp$collision_severity[df.temp$collision_severity==0]="PDO"
df.temp$collision_severity[df.temp$collision_severity==1]="Fatality or symptomatic injury"
df.temp$collision_severity[df.temp$collision_severity==2]="Fatality or symptomatic injury"
df.temp$collision_severity[df.temp$collision_severity==3]="Fatality or symptomatic injury"
df.temp$collision_severity[df.temp$collision_severity==4]="Fatality or symptomatic injury"
df.temp=setDT(df.temp)[order(activity), .(freqC=length(unique(collision_id)),
                                          freqW=length(unique(wono))), 
                       by=.(activity, collision_severity)]

df.temp=df.temp%>%group_by(activity)%>%mutate(pct=round(freqC/sum(freqC), 2))
df.temp$pct[df.temp$collision_severity=="PDO"]=""
df.temp=na.omit(df.temp)

setwd("G:/My Drive/WorkingDesk/CalTrans-60% report")
fwrite(df.temp, file = "act.by.collision.csv", sep=",", append = FALSE)

df.temp=cbind.data.frame(df.temp, family=substring(df.temp$activity, 1, 1))

df.temp=setDT(df.temp)[order(family, collision_severity), .(freqC=sum(freqC),
                                        freqW=sum(freqW)), by=.(family, collision_severity)]

df.temp$collision_severity=str_wrap(df.temp$collision_severity, width = 20)
df.temp=df.temp%>%group_by(family)%>%mutate(pct=round(freqC/sum(freqC), 2))
df.temp$pct[df.temp$collision_severity=="PDO"]=""
df.temp=na.omit(df.temp)

ggplot(df.temp, aes(x=family, y=freqC, fill=collision_severity))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center')+
  theme(axis.text.x = element_text(angle= 0, size = 18, family = "Century Gothic", 
                                   vjust = 0, hjust = 0.5),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 18, family = "Century Gothic"),
        axis.text.y = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic"),
        legend.text = element_text(size = 18, family = "Century Gothic"),
        legend.title = element_text(size = 18, family = "Century Gothic"))+
  xlab("IMMS family grouping")+
  ylab("Number of collisions")+
  labs(fill="Collision severity")
  #geom_text(aes(label=pct), family="Century Gothic", size="4", hjust="top")

ggplot(df.temp, aes(x=family, y=freqC/freqW, fill=family))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(angle= 0, size = 18, family = "Century Gothic", 
                                   vjust = 0, hjust = 0.5),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 18, family = "Century Gothic"),
        axis.text.y = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic"))+
  xlab("IMMS family grouping")+
  ylab("Fraction of work orders with lane closure")

#############################
## lane closure by number of collision
############################
df.temp=df[,c("wono", "closure_id", "collision_id")]
df.temp$closure_id=ifelse(is.na(df.temp$closure_id), 0, 1)
df.temp$collision_id=ifelse(is.na(df.temp$collision_id), 0, 1)

df.temp=df.temp[,.(count=length(wono)), by=.(closure_id, collision_id)]

ggplot(df.temp, aes(x=closure_id, y=count, fill=factor(collision_id)))+
  geom_bar(position="stack", stat = "identity")

df.temp=df.temp[collision_id!=0,]
ggplot(df.temp, aes(x=closure_id, y=count, fill=closure_id))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic", hjust = 0.5, color = "black"),
        axis.title.x = element_blank(),
        axis.line.x = element_line(size=1.2),
        axis.line.y = element_line(size=1.2),
        axis.text.y = element_text(size = 18, family = "Century Gothic", color = "black"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic",
                                    color = "black"))+
  scale_x_continuous(breaks=c(0, 1), labels=c("Work orders w/o \nlane closure",
                            "Work orders with \nlane closure"))+
  ylab("Number of collisions")
############################
## work lenght by number of collision
############################

df.temp=df[, c("collision_id", "work_length")]
df.temp$collision_id=ifelse(is.na(df$collision_id), "0", "1")

ggplot(df.temp, aes(x=collision_id, y=work_length, fill=collision_id))+
  geom_violin(trim = TRUE, scale = "area", na.rm = TRUE)+
  scale_y_continuous(limits = c(0, 100))+
  scale_x_discrete(labels=c("No collision", "Collision"))+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic", color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18, family = "Century Gothic", color = "black"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, 
                                    family = "Century Gothic", color = "black"),
        axis.line.x = element_line(size=1.2),
        axis.line.y = element_line(size = 1.2))+
  ylab("Work length")

############################
## collision by collision density
############################

df.temp=df[, c("collision_id", "collision_density11_12")]
df.temp$collision_id=ifelse(is.na(df$collision_id), "0", "1")

ggplot(df.temp, aes(x=collision_id, y=collision_density11_12, fill=collision_id))+
  geom_violin(trim = TRUE, scale = "area", na.rm = TRUE)+
  scale_y_continuous(limits = c(0, 450))+
  scale_x_discrete(labels=c("No collision", "Collision"))+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic", color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18, family = "Century Gothic", color = "black"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, 
                                    family = "Century Gothic", color = "black"),
        axis.line.x = element_line(size=1.2),
        axis.line.y = element_line(size = 1.2))+
  ylab("Collision density")

############################
## collision by truck aadt
############################
df.temp=df[, c("collision_id", "truck_aadt")]
df.temp$collision_id=ifelse(is.na(df$collision_id), "0", "1")

ggplot(df.temp, aes(x=collision_id, y=truck_aadt, fill=collision_id))+
  geom_violin(trim = TRUE, scale = "area", na.rm = TRUE)+
  #scale_y_continuous(limits = c(0, 450))+
  scale_x_discrete(labels=c("No collision", "Collision"))+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic", color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18, family = "Century Gothic", color = "black"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, 
                                    family = "Century Gothic", color = "black"),
        axis.line.x = element_line(size=1.2),
        axis.line.y = element_line(size = 1.2))+
  ylab("Truck AADT")

############################
## activity by proportion of closure
############################
df.temp=df[,c("wono", "activity", "activity_descr", "closure_id")]
df.temp.cl=df.temp[which(!is.na(df.temp$closure_id)),]
df.temp.cl=setDT(df.temp.cl)[order(activity), 
                             .(freqC=length(unique(wono))), 
                             by=.(activity, activity_descr)]

df.temp.w=setDT(df.temp)[order(activity), 
                         .(freqW=length(unique(wono))), 
                         by=.(activity, activity_descr)]

df.fin=merge(df.temp.w, df.temp.cl, by="activity", all.x = TRUE)
df.fin=df.fin[which(df.fin$activity_descr.x==df.fin$activity_descr.y),]
df.fin=df.fin[which(!is.na(df.fin$activity)),]
df.fin$freqC[is.na(df.fin$freqC)]=0
df.fin=cbind.data.frame(df.fin, prop=df.fin$freqC/df.fin$freqW)
df.fin=setDT(df.fin)[order(-prop),,]
df.fin=df.fin[,-4]
fwrite(df.fin, file = "act.by.closure.prop.csv", sep=",", append=FALSE)

df.cl.prop=df.fin
names(df.cl.prop)[5]="closure_prop"
rm(df.fin, df.temp.cl, df.temp.w)
############################
## activity by number of crew
############################
df.temp=df[,c("wono", "activity", "activity_descr")]
df.temp=setDT(df.temp)[order(activity), 
                       .(freqW=length(unique(wono))), 
                       by=.(activity, activity_descr)]

df.crew=fread(file="activity+crew.csv", sep=",", header = TRUE)

df.temp=cbind.data.frame(df.temp, avg.crew=df.crew$avg[match(df.temp$activity, df.crew$ACTIVITY)])
df.temp=cbind.data.frame(df.temp, crew.score=df.temp$avg.crew/df.temp$freqW)

df.temp=setDT(df.temp)[order(-freqW, activity),,]
df.temp=df.temp %>% distinct(activity, .keep_all = TRUE)
df.temp=df.temp[!is.na(df.temp$activity),]

df.temp=setDT(df.temp)[order(-crew.score),,]
df.temp=df.temp[!which(startsWith(df.temp$activity, "H")),]

df.crew.prop=df.temp
df.crew.prop$crew.score=rescaler(df.crew.prop$crew.score, type = "range")
fwrite(df.crew.prop, file = "act.by.crew.prop.csv", sep=",", append=FALSE)
rm(df.crew)

############################
## activity by access type
############################
df.temp=df[,c("wono", "activity", "activity_descr", "access_type")]
df.temp=setDT(df.temp)[order(activity),
                       .(freqW=length(unique(wono))),
                       by=.(activity, activity_descr, access_type)]
df.temp=df.temp[!which(is.na(df.temp$freqW)),]
df.temp=df.temp[!which(is.na(df.temp$access_type)),]
df.temp=setDT(df.temp)[order(-freqW, activity),,]
df.temp=df.temp %>% distinct(activity, access_type, .keep_all = TRUE)

df.temp=df.temp %>% group_by(activity) %>% mutate(prop=freqW/sum(freqW))
df.temp=setDT(df.temp)[order(activity, -freqW),,]

df.temp$access_type[df.temp$access_type=="E"]=4
df.temp$access_type[df.temp$access_type=="C"]=3
df.temp$access_type[df.temp$access_type=="F"]=2
df.temp$access_type[df.temp$access_type=="S"]=1

df.score=df.temp %>% group_by(activity) %>% mutate(score=sum(prop*as.numeric(access_type)))
df.score$score=rescaler(df.score$score, type="range")
df.score=df.score %>% distinct(activity, activity_descr, score, .keep_all = FALSE)

fwrite(df.score, file = "act.by.access.prop.csv", sep=",", append = FALSE)
df.access.prop=df.score
names(df.access.prop)[3]="access_score"
rm(df.score)
##################################
## activity by duration and length
##################################
df.temp=df[,c("wono", "activity", "activity_descr", "work_duration", "work_length")]
df.temp = df.temp %>% group_by(activity) %>% mutate(mean_duration=mean(work_duration, na.rm = TRUE),
                                                    mean_length=mean(work_length, na.rm = TRUE))
df.temp = df.temp %>% distinct(activity, activity_descr, mean_duration, mean_length, .keep_all = FALSE)
df.temp = df.temp %>% distinct(activity, .keep_all = TRUE)

df.temp$mean_duration=rescaler(df.temp$mean_duration, type="range")
df.temp$mean_length=rescaler(df.temp$mean_length, type="range")

fwrite(df.temp, file="act.by.length+duration.csv", sep = ",", append = FALSE)
df.length.duration=df.temp

