library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(ggforce)
library(lubridate)
library(anytime)
library(forcats)
library(egg)
library(stringr)

######################################################################################################
#month
temp.df=df
temp.df$collision_date=month(anydate(temp.df$collision_date), label = TRUE, abbr = TRUE)
temp.df=setDT(temp.df)[order(collision_date, collision_severity), .(count=length(unique(collision_id))),
                       by=.(collision_date, collision_severity)]

temp.df1=temp.df

temp.df$collision_severity[temp.df$collision_severity==0]="PDO"
temp.df$collision_severity[temp.df$collision_severity==1]="Fatal"
temp.df$collision_severity[temp.df$collision_severity==2]="Severe injury"
temp.df$collision_severity[temp.df$collision_severity==3]="Visible injury"
temp.df$collision_severity[temp.df$collision_severity==4]="Complaint of pain"

temp.df1$collision_severity=as.character(temp.df1$collision_severity)
temp.df1$collision_severity[which(temp.df1$collision_severity==0)]="PDO"
temp.df1$collision_severity[which(temp.df1$collision_severity==1 | 
                                    temp.df1$collision_severity==2 | 
                                    temp.df1$collision_severity==3 |
                                    temp.df1$collision_severity==4)]="Fatality or symptomatic Injury"
temp.df1=na.omit(temp.df1)
temp.df1$collision_severity=str_wrap(temp.df1$collision_severity, width = 20)

p.main=ggplot(data=temp.df1, aes(x=collision_date, y=count, fill=collision_severity))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center') +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=18, family = "Century Gothic"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18, family = "Century Gothic"),
        axis.title.y = element_text(size=18, family = "Century Gothic"),
        legend.title = element_text(size=18, family = "Century Gothic"),
        legend.text = element_text(size=18, family = "Century Gothic"))+
  ylab("Number of collisions")+
  labs(fill="Collision severity")

temp.df=na.omit(temp.df)
temp.df$collision_severity=factor(temp.df$collision_severity, levels=c("PDO", 
                                                                       "Complaint of pain", 
                                                                       "Visible injury", 
                                                                       "Severe injury", 
                                                                       "Fatal"))
p.severity=ggplot(data=temp.df, aes(x=collision_date, y=count, fill=collision_severity))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 18, family = "Century Gothic"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=18, family = "Century Gothic"),
        axis.text.y = element_text(size=18, family = "Century Gothic"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=18, family = "Century Gothic"),
        legend.title = element_text(size=18, family = "Century Gothic"),
        legend.text = element_text(size=18, family = "Century Gothic"))+
  ylab("Number of collisions")+
  labs(fill="Collision severity")+
  facet_wrap(~collision_severity, ncol=2, scales = "free")+
  theme(strip.text.x = element_text(size = 18, family = "Century Gothic"),
        strip.text.y = element_text(size = 18, family = "Century Gothic"))
  

ggarrange(p.main, p.severity, heights = c(2, 4))
######################################################################################################
#week day
temp.df=df
temp.df=setDT(temp.df)[order(collision_day, collision_severity), .(count=length(unique(collision_id))), 
                     by=.(collision_day, collision_severity)]
temp.df$collision_day[temp.df$collision_day==1]="Mon"
temp.df$collision_day[temp.df$collision_day==2]="Tue"
temp.df$collision_day[temp.df$collision_day==3]="Wed"
temp.df$collision_day[temp.df$collision_day==4]="Thu"
temp.df$collision_day[temp.df$collision_day==5]="Fri"
temp.df$collision_day[temp.df$collision_day==6]="Sat"
temp.df$collision_day[temp.df$collision_day==7]="Sun"
temp.df$collision_day=factor(temp.df$collision_day, levels = c("Mon", "Tue", "Wed", 
                                                               "Thu", "Fri", "Sat", "Sun"))

temp.df1=temp.df

temp.df$collision_severity[temp.df$collision_severity==0]="PDO"
temp.df$collision_severity[temp.df$collision_severity==1]="Fatal"
temp.df$collision_severity[temp.df$collision_severity==2]="Severe injury"
temp.df$collision_severity[temp.df$collision_severity==3]="Visible injury"
temp.df$collision_severity[temp.df$collision_severity==4]="Complaint of pain"

temp.df1$collision_severity=as.character(temp.df1$collision_severity)
temp.df1$collision_severity[which(temp.df1$collision_severity==0)]="PDO"
temp.df1$collision_severity[which(temp.df1$collision_severity==1 | 
                                    temp.df1$collision_severity==2 | 
                                    temp.df1$collision_severity==3 |
                                    temp.df1$collision_severity==4)]="Fatality or symptomatic injury"

temp.df1=na.omit(temp.df1)
temp.df1$collision_severity=str_wrap(temp.df1$collision_severity, width = 20)
p.main=ggplot(data=temp.df1, aes(x=collision_day, y=count, fill=collision_severity))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center') +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=18, family = "Century Gothic"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18, family = "Century Gothic"),
        axis.title.y = element_text(size=18, family = "Century Gothic"),
        legend.title = element_text(size=18, family = "Century Gothic"),
        legend.text = element_text(size=18, family = "Century Gothic"))+
  ylab("Number of collisions")+
  labs(fill="Collision severity")

temp.df=na.omit(temp.df)
temp.df$collision_severity=factor(temp.df$collision_severity, levels=c("PDO", 
                                                                       "Complaint of pain", 
                                                                       "Visible injury", 
                                                                       "Severe injury", 
                                                                       "Fatal"))
p.severity=ggplot(data=temp.df, aes(x=collision_day, y=count, fill=collision_severity))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 18, family = "Century Gothic"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=18, family = "Century Gothic"),
        axis.text.y = element_text(size=18, family = "Century Gothic"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=18, family = "Century Gothic"),
        legend.title = element_text(size=18, family = "Century Gothic"),
        legend.text = element_text(size=18, family = "Century Gothic"))+
  ylab("Number of collisions")+
  labs(fill="Collision severity")+
  facet_wrap(~collision_severity, ncol=2, scales = "free")+
  theme(strip.text.x = element_text(size = 18, family = "Century Gothic"),
        strip.text.y = element_text(size = 18, family = "Century Gothic"))

ggarrange(p.main, p.severity, heights = c(2, 4))
######################################################################################################
#Location type (ramp, intersectiom)
temp.df=setDT(df)

temp.df$collision_ramp_intersection[temp.df$collision_ramp_intersection==1]="Ramp exit"
temp.df$collision_ramp_intersection[temp.df$collision_ramp_intersection==2]="Mid ramp"
temp.df$collision_ramp_intersection[temp.df$collision_ramp_intersection==3]="Ramp entry"
temp.df$collision_ramp_intersection[temp.df$collision_ramp_intersection==4]="within 100 ft. of ramp"
temp.df$collision_ramp_intersection[temp.df$collision_ramp_intersection==5]="Intersection"
temp.df$collision_ramp_intersection[temp.df$collision_ramp_intersection==6]="within 250 ft. of intersection"
temp.df$collision_ramp_intersection[temp.df$collision_ramp_intersection==7]="Highway"
temp.df$collision_ramp_intersection[temp.df$collision_ramp_intersection==8]="Not state highway"
temp.df$collision_ramp_intersection[temp.df$collision_ramp_intersection=="-"]="Not stated"
temp.df$collision_ramp_intersection[temp.df$collision_ramp_intersection==""]="Missing value"

temp.df$collision_ramp_intersection=factor(temp.df$collision_ramp_intersection, 
                                           levels = c("Ramp exit", "Mid ramp", "Ramp entry",
                                                      "within 100 ft. of ramp", "Intersection",
                                                      "within 250 ft. of intersection",
                                                      "Highway", "Not state highway",
                                                      "Not stated", "Missing value"))

temp.df=setDT(temp.df)[order(collision_ramp_intersection, collision_severity), 
                       .(count=length(unique(collision_id))), 
                       by=.(collision_ramp_intersection, collision_severity)]

temp.df$collision_ramp_intersection=str_wrap(temp.df$collision_ramp_intersection, width = 12)
temp.df1=temp.df

temp.df$collision_severity[temp.df$collision_severity==0]="PDO"
temp.df$collision_severity[temp.df$collision_severity==1]="Fatal"
temp.df$collision_severity[temp.df$collision_severity==2]="Severe injury"
temp.df$collision_severity[temp.df$collision_severity==3]="Visible injury"
temp.df$collision_severity[temp.df$collision_severity==4]="Complaint of pain"

temp.df1$collision_severity=as.character(temp.df1$collision_severity)
temp.df1$collision_severity[which(temp.df1$collision_severity==0)]="PDO"
temp.df1$collision_severity[which(temp.df1$collision_severity==1 | 
                                    temp.df1$collision_severity==2 | 
                                    temp.df1$collision_severity==3 |
                                    temp.df1$collision_severity==4)]="Fatality or symptomatic injury"

temp.df1=na.omit(temp.df1)
temp.df1$collision_severity=str_wrap(temp.df1$collision_severity, width = 20)
temp.df1=temp.df1[-c(which(temp.df1$collision_ramp_intersection=="Not stated" | 
                           temp.df1$collision_ramp_intersection=="Missing value")),]
p.main=ggplot(data=temp.df1, aes(x=collision_ramp_intersection, y=count, fill=collision_severity))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center') +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=18, family = "Century Gothic"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 20, 0, 0), size=18, family = "Century Gothic"),
        legend.title = element_text(size=18, family = "Century Gothic"),
        legend.text = element_text(size=18, family = "Century Gothic"))+
  ylab("Number of collisions")+
  labs(fill="Collision severity")

temp.df=temp.df[-c(which(temp.df$collision_ramp_intersection=="Not stated" | 
                           temp.df$collision_ramp_intersection=="Missing value")),]
temp.df=na.omit(temp.df)
temp.df$collision_severity=factor(temp.df$collision_severity, levels=c("PDO", 
                                                                       "Complaint of pain", 
                                                                       "Visible injury", 
                                                                       "Severe injury", 
                                                                       "Fatal"))

p.severity=ggplot(data=temp.df, aes(x=collision_ramp_intersection, y=count, fill=collision_severity))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center') +
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 18, family = "Century Gothic"),
        axis.text.x = element_text(angle = 35, hjust = 1, vjust=1, size=18, family = "Century Gothic"),
        axis.text.y = element_text(size=18, family = "Century Gothic"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin=margin(0, 20, 0, 0), size=18, family = "Century Gothic"),
        legend.title = element_text(size=18, family = "Century Gothic"),
        legend.text = element_text(size=18, family = "Century Gothic"),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ylab("Number of collisions")+
  labs(fill="Collision severity")+
  facet_wrap(~collision_severity, ncol=2, scales = "free")+
  theme(strip.text.x = element_text(size = 18, family = "Century Gothic"),
        strip.text.y = element_text(size = 18, family = "Century Gothic"))

ggarrange(p.main, p.severity, heights = c(2, 8))
