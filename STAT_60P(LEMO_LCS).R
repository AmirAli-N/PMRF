library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(ggrepel)
library(ggforce)

setwd("Z:/PMRF/Amir/bin/Final Datasets")
df=fread(file="LEMO_LCS.summary.csv", sep=",", header = TRUE)
df=na.omit(df, cols = c("Workdate", "Activity"))
df[df==""]=NA
############################
## activity by number of closure
############################
df.temp=setDT(df)[order(activity), .(freq=length(unique(wono))), by=.(activity, activity_descr)]
setwd("G:/My Drive/WorkingDesk/CalTrans-60% report")
fwrite(df.temp, file = "act.by.closure.csv", sep=",", append = FALSE)
############################
## family by number of closure
############################
df.temp=cbind(df.temp, family=substring(df.temp$activity, 1, 1))
df.temp=setDT(df.temp)[order(family), .(freq=sum(freq)), by=.(family)]

ggplot(df.temp, aes(x=family, y=freq, fill=family))+
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
############################
## family by closure type
############################
df.temp=setDT(df)[order(activity), .(freq=length(unique(wono))), by=.(activity, closure_type)]
df.temp=cbind.data.frame(df.temp, family=substring(df.temp$activity, 1, 1))
setwd("G:/My Drive/WorkingDesk/CalTrans-60% report")
fwrite(df.temp, file = "act.by.closure_type.csv", sep=",", append = FALSE)

df.temp$closure_type=factor(df.temp$closure_type, levels = c("Full", "Lane", "One-Way", "Moving", NA))
df.temp=setDT(df.temp)[order(family, closure_type), .(freq=sum(freq)), by=.(family, closure_type)]
df.temp=df.temp%>%group_by(family)%>%mutate(pct=round(freq/sum(freq), 2))
#df.label=df.temp%>%group_by(closure_type)%>%top_n(n=1, wt=pct)

ggplot(df.temp, aes(x=family, y=closure_type, size=freq, fill=closure_type))+
  geom_point(alpha=0.5, shape=21)+
  scale_size(range = c(1, 30))+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic"),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 18, family = "Century Gothic"),
        axis.text.y = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic"))+
  xlab("IMMS family grouping")+
  ylab("Closure type")
############################
## family by closure length
############################
df.temp=df[,c("wono", "activity", "closure_length")]
df.temp=cbind.data.frame(df.temp, family=substring(df.temp$activity, 1, 1))

ggplot(df.temp, aes(x=family, y=closure_length, fill=family))+
  geom_violin(trim = TRUE, scale = "width", na.rm = TRUE)+
  scale_y_continuous(limits = c(0, 15))+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic"),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 18, family = "Century Gothic"),
        axis.text.y = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic"))+
  xlab("IMMS family grouping")+
  ylab("Closure length (mile)")
############################
## family by closure duration
############################
df.temp=df[,c("wono", "activity", "closure_duration")]
df.temp=cbind.data.frame(df.temp, family=substring(df.temp$activity, 1, 1))
df.temp=setDT(df.temp)[order(family, closure_duration), .(freq=length(unique(wono))), 
                       by=.(family, closure_duration)]
#df.temp=df.temp%>%group_by(family)%>%mutate(pct=round(freq/sum(freq),2))

ggplot(df.temp, aes(x=family, y=closure_duration, size=freq, fill=closure_duration))+
  geom_point(alpha=0.5, shape=21)+
  scale_size(range = c(3, 30))+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic"),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 18, family = "Century Gothic"),
        axis.text.y = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic"))+
  xlab("IMMS family grouping")+
  ylab("Closure duration")
############################
## family by closure facility
############################
df.temp=df[,c("wono", "activity", "closure_facility")]
df.temp=cbind.data.frame(df.temp, family=substring(df.temp$activity, 1, 1))
df.temp=setDT(df.temp)[order(family, closure_facility), .(freq=length(unique(wono))), 
                       by=.(family, closure_facility)]
#df.temp=df.temp%>%group_by(family)%>%mutate(pct=round(freq/sum(freq),2))
#df.label=df.temp%>%group_by(family)%>%top_n(n=2, wt=pct)

ggplot(df.temp, aes(x=family, y=closure_facility, size=freq, fill=closure_facility))+
  geom_point(alpha=0.5, shape=21)+
  scale_size(range = c(3, 30))+
  #geom_text(size=5, data=df.label, aes(x=family, y=closure_facility, label=pct), 
  #          family="Century Gothic", vjust=-2.6)+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic"),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 18, family = "Century Gothic"),
        axis.text.y = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic"))+
  xlab("IMMS family grouping")+
  ylab("Closure facility")

############################
## family by closure lanes
############################
df.temp=df[,c("wono", "activity", "closure_lanes")]
df.temp=cbind.data.frame(df.temp, family=substring(df.temp$activity, 1, 1))

ggplot(na.omit(df.temp), aes(x=family, y=closure_lanes, fill=family))+
  geom_violin(trim = TRUE, scale = "width", na.rm = TRUE)+
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1), labels = seq(0, 10, 1))+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic"),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 18, family = "Century Gothic"),
        axis.text.y = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic"),
        panel.grid.minor.y = element_blank())+
  xlab("IMMS family grouping")+
  ylab("Closed lanes")
