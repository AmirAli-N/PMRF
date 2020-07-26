library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(ggrepel)
library(ggforce)
library(stringr)

setwd("Z:/PMRF/Amir/bin/Final Datasets")
df=fread(file="LEMO_LCS_RouteFeauter_AADT_CHP.by.roadCond.csv", sep=",", header = TRUE)
df=na.omit(df, cols = c("work_date", "activity"))
df[df==""]=NA

############################
## activity by number of closure
############################
df.temp=df[,c("wono", "activity", "closure_id")]
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

############################
## work lenght by number of collision
############################

df.temp=df[, c("collision_id", "work_length")]
df.temp$collision_id=ifelse(is.na(df$collision_id), "0", "1")

ggplot(df.temp, aes(x=collision_id, y=work_length, fill=collision_id))+
  geom_violin(trim = TRUE, scale = "width", na.rm = TRUE)+
  scale_y_continuous(limits = c(0, 100))+
  scale_x_discrete(labels=c("No collision", "Collision"))+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic", color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18, family = "Century Gothic", color = "black"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, 
                                    family = "Century Gothic", color = "black"))+
  ylab("Work length")

############################
## collision by collision density
############################

df.temp=df[, c("collision_id", "collision_density11_12")]
df.temp$collision_id=ifelse(is.na(df$collision_id), "0", "1")

ggplot(df.temp, aes(x=collision_id, y=collision_density11_12, fill=collision_id))+
  geom_violin(trim = TRUE, scale = "width", na.rm = TRUE)+
  scale_y_continuous(limits = c(0, 450))+
  scale_x_discrete(labels=c("No collision", "Collision"))+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic", color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18, family = "Century Gothic", color = "black"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, 
                                    family = "Century Gothic", color = "black"))+
  ylab("Collision density")