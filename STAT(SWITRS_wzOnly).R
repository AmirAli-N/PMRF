library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(plotly)
library(chron)
library(forcats)
library(scales)
library(stringr)

setwd("//ahmct-065/teams/PMRF/Amir")
CHP.df=fread(file="./bin/CHP_CleanRouteFeatures.cleaned.csv", sep=",", header = TRUE)
#######################################
####################################### time vs. severity
#######################################

temp.chp=CHP.df
temp.chp=CHP.df[which(CHP.df$ROAD_COND_1=="D" | CHP.df$ROAD_COND_2=="D"),]

time.label=c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00",
             "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00",
             "22:00", "23:00", "24:00")

temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==0]="PDO"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

temp.chp=temp.chp%>%group_by(COLLISION_TIME=COLLISION_TIME%/%100, COLLISION_SEVERITY)%>%summarise(count=n())%>%mutate(pct=round(count/sum(count),2))
#temp.chp=temp.chp[order(COLLISION_TIME, COLLISION_SEVERITY), .(len.id=length(unique(CASE_ID))),
#                  by=.(COLLISION_TIME%/%100, COLLISION_SEVERITY)]

cutoff=quantile(temp.chp$count[which(temp.chp$COLLISION_SEVERITY=="Fatality or symptomatic injury")])[4]
temp.chp$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.chp$COLLISION_SEVERITY)

ggplot(data=temp.chp, aes(x=COLLISION_TIME, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  #geom_hline(yintercept = cutoff, linetype="dashed", color="blue")+
  geom_text(aes(label=pct), size=6, vjust=0, angle=0, hjust=0.5)+
  scale_x_continuous(limits=c(-1, 26), breaks=seq(0, 24, 1), labels = time.label)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID")+
  xlab("Time of the day")+
  labs(fill="Collision Severity")
#######################################
####################################### time vs. day
#######################################
temp.chp=CHP.df
temp.chp=CHP.df[which(CHP.df$ROAD_COND_1=="D" | CHP.df$ROAD_COND_2=="D"),]

temp.chp=temp.chp[order(COLLISION_TIME, DAY_OF_WEEK), .(len.id=length(unique(CASE_ID))), 
                  by=.(COLLISION_TIME%/%100, DAY_OF_WEEK)]

temp.chp$DAY_OF_WEEK[temp.chp$DAY_OF_WEEK==1]="1-Mon"
temp.chp$DAY_OF_WEEK[temp.chp$DAY_OF_WEEK==2]="2-Tue"
temp.chp$DAY_OF_WEEK[temp.chp$DAY_OF_WEEK==3]="3-Wed"
temp.chp$DAY_OF_WEEK[temp.chp$DAY_OF_WEEK==4]="4-Thu"
temp.chp$DAY_OF_WEEK[temp.chp$DAY_OF_WEEK==5]="5-Fri"
temp.chp$DAY_OF_WEEK[temp.chp$DAY_OF_WEEK==6]="6-Sat"
temp.chp$DAY_OF_WEEK[temp.chp$DAY_OF_WEEK==7]="7-Sun"

quantile(temp.chp$len.id)

ggplot(data=temp.chp, aes(x=COLLISION_TIME, y=DAY_OF_WEEK, size=len.id)) +
  geom_point(shape=21, alpha=0.75, aes(fill=cut(len.id, c(5, 128, 195, 274, 388))))+
  scale_colour_manual(name="Count CASE_ID", values=c("[5, 128]"="black",
                                                     "(128, 195]"="yellow",
                                                     "(195, 274]"="green",
                                                     "(274, 388]"="blue"),
                      labels=c("[5, 128]", "(128, 195]", "(195, 274]", "(274, 388]"))+
  scale_size(range = c(0.01, 30))+
  scale_x_continuous(limits=c(-1, 26), breaks=seq(0, 24, 1), labels = time.label)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(angle = 0, vjust = 0.5, size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  guides(fill=guide_legend(override.aes = list(size=10)))+
  ylab("Day of the week")+
  xlab("Time of the day")+
  labs(size="Count case ID", fill="Color cutoffs")
###############################################
############################################### population vs. severity
###############################################
temp.chp=CHP.df
temp.chp=CHP.df[which(CHP.df$ROAD_COND_1=="D" | CHP.df$ROAD_COND_2=="D"),]

temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==0]="PDO"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

temp.chp$POPULATION[temp.chp$POPULATION==1]="1-Less than 2500"
temp.chp$POPULATION[temp.chp$POPULATION==2]="2-2500-10000"
temp.chp$POPULATION[temp.chp$POPULATION==3]="3-10000-25000"
temp.chp$POPULATION[temp.chp$POPULATION==4]="4-25000-50000"
temp.chp$POPULATION[temp.chp$POPULATION==5]="5-50000-100000"
temp.chp$POPULATION[temp.chp$POPULATION==6]="6-100000-250000"
temp.chp$POPULATION[temp.chp$POPULATION==7]="7-Over 250000"
temp.chp$POPULATION[temp.chp$POPULATION==9]="9-Rural or unincorporated"

temp.chp = temp.chp %>% group_by(POPULATION, COLLISION_SEVERITY) %>% summarise(count=n()) %>% mutate(pct=round(count/sum(count),2))
temp.chp$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.chp$COLLISION_SEVERITY)

#temp.chp=temp.chp[order(POPULATION, COLLISION_SEVERITY), .(len.id=length(unique(CASE_ID))),
#                  by=.(COLLISION_SEVERITY, POPULATION)]

ggplot(data=temp.chp, aes(x=POPULATION, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_text_repel(aes(label=pct), size=6, vjust=0, angle=0, hjust=0.5)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID")+
  xlab("Population")+
  labs(fill="Collision Severity")
#######################################
####################################### weather vs. severity
#######################################
temp.chp=CHP.df
temp.chp=CHP.df[which(CHP.df$ROAD_COND_1=="D" | CHP.df$ROAD_COND_2=="D"),]

temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==0]="PDO"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

temp.chp$WEATHER_1[temp.chp$WEATHER_1=="A"]="Clear"
temp.chp$WEATHER_1[temp.chp$WEATHER_1=="B"]="Cloudy"
temp.chp$WEATHER_1[temp.chp$WEATHER_1=="C"]="Raining"
temp.chp$WEATHER_1[temp.chp$WEATHER_1=="D"]="Snowing"
temp.chp$WEATHER_1[temp.chp$WEATHER_1=="E"]="Fog"
temp.chp$WEATHER_1[temp.chp$WEATHER_1=="F"]="Other"
temp.chp$WEATHER_1[temp.chp$WEATHER_1=="G"]="Wind"
temp.chp$WEATHER_1[temp.chp$WEATHER_1=="-"]="Not stated"

temp.chp$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.chp$COLLISION_SEVERITY)

temp.chp=temp.chp%>%group_by(WEATHER_1, COLLISION_SEVERITY)%>%summarise(count=n())%>%mutate(pct=round(count/sum(count),2))
#=temp.chp[order(WEATHER_1, COLLISION_SEVERITY), .(len.id=length(unique(CASE_ID))),
#                  by=.(WEATHER_1, COLLISION_SEVERITY)]

ggplot(data=temp.chp, aes(x=WEATHER_1, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_text_repel(aes(label=pct), size=6, vjust=0, angle=0, hjust=0.5)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID")+
  xlab("Weather condition")+
  labs(fill="Collision Severity")
#######################################
####################################### lighting vs. severity
#######################################
temp.chp=CHP.df
temp.chp=CHP.df[which(CHP.df$ROAD_COND_1=="D" | CHP.df$ROAD_COND_2=="D"),]

temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==0]="PDO"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

temp.chp$LIGHTING[temp.chp$LIGHTING=="A"]="Daylight"
temp.chp$LIGHTING[temp.chp$LIGHTING=="B"]="Dusk"
temp.chp$LIGHTING[temp.chp$LIGHTING=="C"]="Dark - Street light"
temp.chp$LIGHTING[temp.chp$LIGHTING=="D"]="Dark - No street light"
temp.chp$LIGHTING[temp.chp$LIGHTING=="E"]="Dark - Not functioning light"
temp.chp$LIGHTING[temp.chp$LIGHTING=="-"]="Not stated"

temp.chp$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.chp$COLLISION_SEVERITY)
temp.chp$LIGHTING=gsub(" - ", " - \n", temp.chp$LIGHTING)

temp.chp=temp.chp%>%group_by(LIGHTING, COLLISION_SEVERITY)%>%summarise(count=n())%>%mutate(pct=round(count/sum(count),2))
#temp.chp=temp.chp[order(LIGHTING, COLLISION_SEVERITY), .(len.id=length(unique(CASE_ID))),
#                  by=.(LIGHTING, COLLISION_SEVERITY)]

ggplot(data=temp.chp, aes(x=LIGHTING, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_text_repel(aes(label=pct), size=6, vjust=0, angle=0, hjust=0.5)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID")+
  xlab("Lighting condition")+
  labs(fill="Collision Severity")
#######################################
####################################### weather vs. lighting vs. severity
#######################################
temp.chp=CHP.df
temp.chp=CHP.df[which(CHP.df$ROAD_COND_1=="D" | CHP.df$ROAD_COND_2=="D"),]

temp.chp$WEATHER_1[temp.chp$WEATHER_1=="A"]="Clear"
temp.chp$WEATHER_1[temp.chp$WEATHER_1=="B"]="Cloudy"
temp.chp$WEATHER_1[temp.chp$WEATHER_1=="C"]="Raining"
temp.chp$WEATHER_1[temp.chp$WEATHER_1=="D"]="Snowing"
temp.chp$WEATHER_1[temp.chp$WEATHER_1=="E"]="Fog"
temp.chp$WEATHER_1[temp.chp$WEATHER_1=="F"]="Other"
temp.chp$WEATHER_1[temp.chp$WEATHER_1=="G"]="Wind"
temp.chp$WEATHER_1[temp.chp$WEATHER_1=="-"]="Not stated"

temp.chp$LIGHTING[temp.chp$LIGHTING=="A"]="Daylight"
temp.chp$LIGHTING[temp.chp$LIGHTING=="B"]="Dusk"
temp.chp$LIGHTING[temp.chp$LIGHTING=="C"]="Dark - Street light"
temp.chp$LIGHTING[temp.chp$LIGHTING=="D"]="Dark - No street light"
temp.chp$LIGHTING[temp.chp$LIGHTING=="E"]="Dark - Not functioning light"
temp.chp$LIGHTING[temp.chp$LIGHTING=="-"]="Not stated"

temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==0]="PDO"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

temp.chp$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.chp$COLLISION_SEVERITY)
temp.chp$LIGHTING=gsub(" - ", " - \n", temp.chp$LIGHTING)

temp.chp=temp.chp%>%group_by(WEATHER_1, LIGHTING, COLLISION_SEVERITY)%>%summarise(count=n())%>%mutate(pct=round(count/sum(count),2))
temp.chp=setDT(temp.chp)[order(WEATHER_1, LIGHTING, -COLLISION_SEVERITY),,]

#temp.chp=temp.chp[order(WEATHER_1, LIGHTING, COLLISION_SEVERITY), .(len.id=length(unique(CASE_ID))), 
#                  by=.(WEATHER_1, LIGHTING, COLLISION_SEVERITY)]

ggplot(data=temp.chp, aes(x=LIGHTING, y=WEATHER_1, size=count, fill=forcats::fct_rev(COLLISION_SEVERITY))) +
  geom_point(shape=21, alpha=0.75)+
  scale_size(range = c(2.5, 50))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(angle = 0, vjust = 0.5, size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  guides(fill=guide_legend(override.aes = list(size=10)))+
  ylab("Weather condition")+
  xlab("Lighting")+
  labs(size="Count case ID", fill="Collision severity")
#######################################
####################################### surface vs. severity
#######################################
temp.chp=CHP.df
temp.chp=CHP.df[which(CHP.df$ROAD_COND_1=="D" | CHP.df$ROAD_COND_2=="D"),]

temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==0]="PDO"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

temp.chp$ROAD_SURFACE[temp.chp$ROAD_SURFACE=="A"]="Dry"
temp.chp$ROAD_SURFACE[temp.chp$ROAD_SURFACE=="B"]="Wet"
temp.chp$ROAD_SURFACE[temp.chp$ROAD_SURFACE=="C"]="Snowy/Icy"
temp.chp$ROAD_SURFACE[temp.chp$ROAD_SURFACE=="D"]="Slippery"
temp.chp$ROAD_SURFACE[temp.chp$ROAD_SURFACE=="-"]="Not stated"

temp.chp$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.chp$COLLISION_SEVERITY)

temp.chp=temp.chp%>%group_by(ROAD_SURFACE, COLLISION_SEVERITY)%>%summarise(count=n())%>%mutate(pct=round(count/sum(count),2))
#temp.chp=temp.chp[order(ROAD_SURFACE, COLLISION_SEVERITY), .(len.id=length(unique(CASE_ID))), 
#                  by=.(ROAD_SURFACE, COLLISION_SEVERITY)]

ggplot(data=temp.chp, aes(x=ROAD_SURFACE, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_text_repel(aes(label=pct), size=6, vjust=0, angle=0, hjust=0.5)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID")+
  xlab("Road surface condition")+
  labs(fill="Collision Severity")
#######################################
####################################### violation cat vs. severity
#######################################
temp.chp=CHP.df
temp.chp=CHP.df[which(CHP.df$ROAD_COND_1=="D" | CHP.df$ROAD_COND_2=="D"),]

temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==0]="PDO"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="01" |
                             temp.chp$PCF_VIOL_CATEGORY=="1"]="DUI"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="02" |
                             temp.chp$PCF_VIOL_CATEGORY=="2"]="Impeding traffic"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="03" |
                             temp.chp$PCF_VIOL_CATEGORY=="3"]="Unsafe speed"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="04" |
                             temp.chp$PCF_VIOL_CATEGORY=="4"]="Following closely"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="05" |
                             temp.chp$PCF_VIOL_CATEGORY=="5"]="Wrong side"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="06" |
                             temp.chp$PCF_VIOL_CATEGORY=="6"]="Improper passing"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="07" |
                             temp.chp$PCF_VIOL_CATEGORY=="7"]="Unsafe lane change"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="08" |
                             temp.chp$PCF_VIOL_CATEGORY=="8"]="Improper turning"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="09" |
                             temp.chp$PCF_VIOL_CATEGORY=="9"]="Aut. ROW"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="10"]="Ped. ROW"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="11"]="Ped. violation"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="12"]="Traffic signal"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="13"]="Bad parking"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="14"]="Lights"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="15"]="Brakes"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="16"]="Other equip."
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="17"]="Other hazard"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="18"]="Other than driver"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="19"]="19"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="20"]="20"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="21"]="Unsafe starting"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="22"]="Other improper driving"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="23"]="Other DUI"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="24"]="Sleep"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="00" |
                             temp.chp$PCF_VIOL_CATEGORY=="0"]="Unknown"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="-"]="Not stated"

temp.chp=temp.chp%>%group_by(PCF_VIOL_CATEGORY, COLLISION_SEVERITY)%>%summarise(count=n())%>%mutate(pct=round(count/sum(count),2))
temp.chp$PCF_VIOL_CATEGORY=str_wrap(temp.chp$PCF_VIOL_CATEGORY, width = 10)
temp.chp$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.chp$COLLISION_SEVERITY)

#temp.chp=temp.chp[order(PCF_VIOL_CATEGORY, COLLISION_SEVERITY), .(len.id=length(unique(CASE_ID))),
#                  by=.(PCF_VIOL_CATEGORY, COLLISION_SEVERITY)]

ggplot(data=temp.chp, aes(x=PCF_VIOL_CATEGORY, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_text_repel(aes(label=pct), size=6, vjust=0, angle=0, hjust=0.5)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID")+
  xlab("Violation category")+
  labs(fill="Collision Severity")
#######################################
####################################### violation cat vs. ramp/intersection vs. severity
#######################################
temp.chp=CHP.df
temp.chp=CHP.df[which(CHP.df$ROAD_COND_1=="D" | CHP.df$ROAD_COND_2=="D"),]

temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==0]="PDO"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="01" |
                             temp.chp$PCF_VIOL_CATEGORY=="1"]="DUI"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="02" |
                             temp.chp$PCF_VIOL_CATEGORY=="2"]="Impeding traffic"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="03" |
                             temp.chp$PCF_VIOL_CATEGORY=="3"]="Unsafe speed"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="04" |
                             temp.chp$PCF_VIOL_CATEGORY=="4"]="Following closely"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="05" |
                             temp.chp$PCF_VIOL_CATEGORY=="5"]="Wrong side"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="06" |
                             temp.chp$PCF_VIOL_CATEGORY=="6"]="Improper passing"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="07" |
                             temp.chp$PCF_VIOL_CATEGORY=="7"]="Unsafe lane change"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="08" |
                             temp.chp$PCF_VIOL_CATEGORY=="8"]="Improper turning"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="09" |
                             temp.chp$PCF_VIOL_CATEGORY=="9"]="Aut. ROW"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="10"]="Ped. ROW"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="11"]="Ped. violation"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="12"]="Traffic signal"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="13"]="Bad parking"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="14"]="Lights"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="15"]="Brakes"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="16"]="Other equip."
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="17"]="Other hazard"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="18"]="Other than driver"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="19"]="19"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="20"]="20"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="21"]="Unsafe starting"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="22"]="Other improper driving"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="23"]="Other DUI"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="24"]="Sleep"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="00" |
                             temp.chp$PCF_VIOL_CATEGORY=="0"]="Unknown"
temp.chp$PCF_VIOL_CATEGORY[temp.chp$PCF_VIOL_CATEGORY=="-"]="Not stated"

temp.chp$RAMP_INTERSECTION[temp.chp$RAMP_INTERSECTION==1]="Ramp exit"
temp.chp$RAMP_INTERSECTION[temp.chp$RAMP_INTERSECTION==2]="Mid ramp"
temp.chp$RAMP_INTERSECTION[temp.chp$RAMP_INTERSECTION==3]="Ramp entry"
temp.chp$RAMP_INTERSECTION[temp.chp$RAMP_INTERSECTION==4]="within 100 ft. of ramp"
temp.chp$RAMP_INTERSECTION[temp.chp$RAMP_INTERSECTION==5]="Intersection"
temp.chp$RAMP_INTERSECTION[temp.chp$RAMP_INTERSECTION==6]="within 250 ft. of intersection"
temp.chp$RAMP_INTERSECTION[temp.chp$RAMP_INTERSECTION==7]="Highway"
temp.chp$RAMP_INTERSECTION[temp.chp$RAMP_INTERSECTION==8]="Not state highway"
temp.chp$RAMP_INTERSECTION[temp.chp$RAMP_INTERSECTION=="-"]="Not stated"

temp.chp=temp.chp%>%group_by(PCF_VIOL_CATEGORY, RAMP_INTERSECTION, COLLISION_SEVERITY)%>%summarise(count=n())%>%
  mutate(pct=round(count/sum(count),2))
#temp.chp=temp.chp[order(PCF_VIOL_CATEGORY, RAMP_INTERSECTION, COLLISION_SEVERITY), .(len.id=length(unique(CASE_ID))),
#                  by=.(PCF_VIOL_CATEGORY, RAMP_INTERSECTION, COLLISION_SEVERITY)]

temp.chp$PCF_VIOL_CATEGORY=str_wrap(temp.chp$PCF_VIOL_CATEGORY, width = 10)
temp.chp$RAMP_INTERSECTION=str_wrap(temp.chp$RAMP_INTERSECTION, width = 12)
temp.chp$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.chp$COLLISION_SEVERITY)
temp.chp=setDT(temp.chp)[order(PCF_VIOL_CATEGORY, RAMP_INTERSECTION, -COLLISION_SEVERITY),,]

ggplot(data=temp.chp, aes(x=PCF_VIOL_CATEGORY, y=RAMP_INTERSECTION, size=count, fill=forcats::fct_rev(COLLISION_SEVERITY))) +
  geom_point(shape=21, alpha=0.75)+
  scale_size(range = c(2.5, 50))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(angle = 0, vjust = 0.5, size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  guides(fill=guide_legend(override.aes = list(size=10)))+
  ylab("Ramp/Intersection")+
  xlab("Violation category")+
  labs(size="Count case ID", fill="Collision severity")