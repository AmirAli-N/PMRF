library(data.table)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggrepel)
library(stringr)
setwd("//ahmct-065/teams/PMRF/Amir")

hwMarker=fread(file="HighwayMarkers-SW_2018-03-19.csv", sep=",", header = TRUE)
df=fread(file="./CHP/CHP_CleanRouteFeatures.cleaned.csv", sep=",", header=TRUE)

####################################
#################################### by surface_type_desc
####################################
temp.df=df
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==0]="PDO"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

surface_miles_LT=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_LT_SURF_TYPE_DESC)]
colnames(surface_miles_LT)=c("THY_SURF_TYPE_DESC", "miles")
surface_miles_RT=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_RT_SURF_TYPE_DESC)]
colnames(surface_miles_RT)=c("THY_SURF_TYPE_DESC", "miles")
surface_miles=bind_rows(surface_miles_LT, surface_miles_RT) %>%
  group_by(THY_SURF_TYPE_DESC) %>%
  summarise_each(funs(sum))
surface_miles$THY_SURF_TYPE_DESC=gsub("  ", " - ", surface_miles$THY_SURF_TYPE_DESC)

temp.df=setDT(temp.df)[, .(freq=length(unique(CASE_ID))), by=.(THY_SURF_TYPE_DESC, COLLISION_SEVERITY)]
temp.df$freq=temp.df$freq/surface_miles$miles[match(temp.df$THY_SURF_TYPE_DESC, surface_miles$THY_SURF_TYPE_DESC)]
temp.df=temp.df %>%
  group_by(THY_SURF_TYPE_DESC, COLLISION_SEVERITY) %>% summarise(count=freq) %>%
  mutate(pct=round(count/sum(count), 2))

temp.df$THY_SURF_TYPE_DESC=gsub(" - ", " - \n", temp.df$THY_SURF_TYPE_DESC)
temp.df$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.df$COLLISION_SEVERITY)

ggplot(data=temp.df, aes(x=THY_SURF_TYPE_DESC, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_text_repel(aes(label=pct), size=6, vjust=0, angle=0, hjust=0.5)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID per mile")+
  xlab("Surface type description")+
  labs(fill="Collision Severity")
####################################
#################################### by surface_type_desc vs. road_surface
####################################
temp.df=df
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==0]="PDO"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE=="A"]="Dry"
temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE=="B"]="Wet"
temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE=="C"]="Snowy/Icy"
temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE=="D"]="Slippery"
temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE==""]="Not stated"

temp.df$THY_SURF_TYPE_DESC=gsub(" - ", " - \n", temp.df$THY_SURF_TYPE_DESC)
temp.df$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.df$COLLISION_SEVERITY)

temp.df=temp.df%>%group_by(THY_SURF_TYPE_DESC, ROAD_SURFACE, COLLISION_SEVERITY)%>%
  summarise(count=n())%>%mutate(pct=round(count/sum(count),2))

temp.df=setDT(temp.df)[order(THY_SURF_TYPE_DESC, ROAD_SURFACE, -COLLISION_SEVERITY),,]

ggplot(data=temp.df, aes(x=THY_SURF_TYPE_DESC, y=ROAD_SURFACE, size=count, fill=forcats::fct_rev(COLLISION_SEVERITY))) +
  geom_point(shape=21, alpha=0.75)+
  scale_size(range = c(2.5, 100))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(angle = 0, vjust = 0.5, size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  guides(size=FALSE)+
  ylab("Road surface condition")+
  xlab("Surface type description")+
  labs(fill="Collision Severity")
####################################
#################################### by median_type
####################################
temp.df=df
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==0]="PDO"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

surface_miles=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_MEDIAN_TYPE_DESC)]

temp.df=setDT(temp.df)[, .(freq=length(unique(CASE_ID))), by=.(THY_MEDIAN_TYPE_DESC, COLLISION_SEVERITY)]
temp.df$freq=temp.df$freq/surface_miles$miles[match(temp.df$THY_MEDIAN_TYPE_DESC, surface_miles$THY_MEDIAN_TYPE_DESC)]
temp.df=temp.df %>%
  group_by(THY_MEDIAN_TYPE_DESC, COLLISION_SEVERITY) %>% summarise(count=freq) %>%
  mutate(pct=round(count/sum(count), 2))

temp.df$THY_MEDIAN_TYPE_DESC=gsub(" - ", " - \n", temp.df$THY_MEDIAN_TYPE_DESC)
temp.df$THY_MEDIAN_TYPE_DESC=gsub(" or ", " or \n", temp.df$THY_MEDIAN_TYPE_DESC)
temp.df$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.df$COLLISION_SEVERITY)

ggplot(data=temp.df, aes(x=THY_MEDIAN_TYPE_DESC, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_text_repel(aes(label=pct), size=6, vjust=0, angle=0, hjust=0)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID per mile")+
  xlab("Median type description")+
  labs(fill="Collision Severity")
####################################
#################################### by median_type vs. road_surface
####################################
temp.df=df
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==0]="PDO"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE=="A"]="Dry"
temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE=="B"]="Wet"
temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE=="C"]="Snowy/Icy"
temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE=="D"]="Slippery"
temp.df$ROAD_SURFACE[temp.df$ROAD_SURFACE==""]="Not stated"

temp.df=temp.df%>%group_by(THY_MEDIAN_TYPE_DESC, ROAD_SURFACE, COLLISION_SEVERITY)%>%
  summarise(count=n())%>%mutate(pct=round(count/sum(count),2))
temp.df=setDT(temp.df)[order(THY_MEDIAN_TYPE_DESC, ROAD_SURFACE, -COLLISION_SEVERITY),,]

temp.df$THY_MEDIAN_TYPE_DESC=gsub(" - ", " - \n", temp.df$THY_MEDIAN_TYPE_DESC)
temp.df$THY_MEDIAN_TYPE_DESC=gsub(" or ", " or \n", temp.df$THY_MEDIAN_TYPE_DESC)
temp.df$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.df$COLLISION_SEVERITY)

ggplot(data=temp.df, aes(x=THY_MEDIAN_TYPE_DESC, y=ROAD_SURFACE, size=count, fill=forcats::fct_rev(COLLISION_SEVERITY))) +
  geom_point(shape=21, alpha=0.75)+
  scale_size(range = c(2.5, 100))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(angle = 0, vjust = 0.5, size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  guides(size=FALSE)+
  ylab("Road surface condition")+
  xlab("Median type description")+
  labs(fill="Collision Severity")
####################################
#################################### by group_code_desc
####################################
temp.df=df
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==0]="PDO"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

surface_miles=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_HIGHWAY_GROUP_DESC)]

temp.df=setDT(temp.df)[, .(freq=length(unique(CASE_ID))), by=.(THY_HIGHWAY_GROUP_DESC, COLLISION_SEVERITY)]
temp.df$freq=temp.df$freq/surface_miles$miles[match(temp.df$THY_HIGHWAY_GROUP_DESC, surface_miles$THY_HIGHWAY_GROUP_DESC)]
temp.df=temp.df %>%
  group_by(THY_HIGHWAY_GROUP_DESC, COLLISION_SEVERITY) %>% summarise(count=freq) %>%
  mutate(pct=round(count/sum(count), 2))

temp.df$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.df$COLLISION_SEVERITY)

ggplot(data=temp.df, aes(x=THY_HIGHWAY_GROUP_DESC, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_text_repel(aes(label=pct), size=6, vjust=0, angle=0, hjust=0)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID per mile")+
  xlab("Roadway division description")+
  labs(fill="Collision Severity")
####################################
#################################### by number of lanes
####################################
temp.df=df
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==0]="PDO"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

surface_miles_LT=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_LT_LANES_AMT)]
colnames(surface_miles_LT)=c("THY_LANES_AMT", "miles")
surface_miles_RT=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_RT_LANES_AMT)]
colnames(surface_miles_RT)=c("THY_LANES_AMT", "miles")
surface_miles=bind_rows(surface_miles_LT, surface_miles_RT) %>%
  group_by(THY_LANES_AMT) %>%
  summarise_each(funs(sum))

temp.df=setDT(temp.df)[, .(freq=length(unique(CASE_ID))), by=.(THY_LANES_AMT, COLLISION_SEVERITY)]
temp.df$freq=temp.df$freq/surface_miles$miles[match(temp.df$THY_LANES_AMT, surface_miles$THY_LANES_AMT)]
temp.df=temp.df %>%
  group_by(THY_LANES_AMT, COLLISION_SEVERITY) %>% summarise(count=freq) %>%
  mutate(pct=round(count/sum(count), 2))

temp.df$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.df$COLLISION_SEVERITY)

ggplot(data=temp.df, aes(x=THY_LANES_AMT, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_text_repel(aes(label=pct), size=6, vjust=0.2, angle=0, hjust=0.5)+
  scale_x_continuous(limits=c(0, 20), breaks=seq(0, 20, 1), labels = seq(0, 20, 1))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID per mile")+
  xlab("No. of Lanes")+
  labs(fill="Collision Severity")
####################################
#################################### by roadway use
####################################
temp.df=df
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==0]="PDO"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

surface_miles_LT=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_LT_ROADWAY_USE_DESC)]
colnames(surface_miles_LT)=c("THY_ROADWAY_USE_DESC", "miles")
surface_miles_RT=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_RT_ROADWAY_USE_DESC)]
colnames(surface_miles_RT)=c("THY_ROADWAY_USE_DESC", "miles")
surface_miles=bind_rows(surface_miles_LT, surface_miles_RT) %>%
  group_by(THY_ROADWAY_USE_DESC) %>%
  summarise_each(funs(sum))

temp.df=setDT(temp.df)[, .(freq=length(unique(CASE_ID))), by=.(THY_ROADWAY_USE_DESC, COLLISION_SEVERITY)]
temp.df$freq=temp.df$freq/surface_miles$miles[match(temp.df$THY_ROADWAY_USE_DESC, surface_miles$THY_ROADWAY_USE_DESC)]
temp.df=temp.df %>%
  group_by(THY_ROADWAY_USE_DESC, COLLISION_SEVERITY) %>% summarise(count=freq) %>%
  mutate(pct=round(count/sum(count), 2))

temp.df$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.df$COLLISION_SEVERITY)
temp.df$THY_ROADWAY_USE_DESC=str_wrap(temp.df$THY_ROADWAY_USE_DESC, width = 15)

ggplot(data=temp.df, aes(x=THY_ROADWAY_USE_DESC, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_text_repel(aes(label=pct), size=6, vjust=0.2, angle=0, hjust=0.5)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID per mile")+
  xlab("Roadway use description")+
  labs(fill="Collision Severity")
####################################
#################################### by barrier type
####################################
temp.df=df
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==0]="PDO"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

surface_miles=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_MEDIAN_BARRIER_DESC)]

temp.df=setDT(temp.df)[, .(freq=length(unique(CASE_ID))), by=.(THY_MEDIAN_BARRIER_DESC, COLLISION_SEVERITY)]
temp.df$freq=temp.df$freq/surface_miles$miles[match(temp.df$THY_MEDIAN_BARRIER_DESC, surface_miles$THY_MEDIAN_BARRIER_DESC)]
temp.df=temp.df %>%
  group_by(THY_MEDIAN_BARRIER_DESC, COLLISION_SEVERITY) %>% summarise(count=freq) %>%
  mutate(pct=round(count/sum(count), 2))

temp.df$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.df$COLLISION_SEVERITY)
temp.df$THY_MEDIAN_BARRIER_DESC=str_wrap(temp.df$THY_MEDIAN_BARRIER_DESC, width = 12)

ggplot(data=temp.df, aes(x=THY_MEDIAN_BARRIER_DESC, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_text_repel(aes(label=pct), size=6, vjust=0.2, angle=0, hjust=0.5)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID per mile")+
  xlab("Barrier type description")+
  labs(fill="Collision Severity")
####################################
#################################### by terrain
####################################
temp.df=df
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==0]="PDO"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

surface_miles=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_TERRAIN_DESC)]

temp.df=setDT(temp.df)[, .(freq=length(unique(CASE_ID))), by=.(THY_TERRAIN_DESC, COLLISION_SEVERITY)]
temp.df$freq=temp.df$freq/surface_miles$miles[match(temp.df$THY_TERRAIN_DESC, surface_miles$THY_TERRAIN_DESC)]
temp.df=temp.df %>%
  group_by(THY_TERRAIN_DESC, COLLISION_SEVERITY) %>% summarise(count=freq) %>%
  mutate(pct=round(count/sum(count), 2))

temp.df$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.df$COLLISION_SEVERITY)

ggplot(data=temp.df, aes(x=THY_TERRAIN_DESC, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_text_repel(aes(label=pct), size=6, vjust=0.2, angle=0, hjust=0.5)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID per mile")+
  xlab("Terrain")+
  labs(fill="Collision Severity")
####################################
#################################### by design speed
####################################
temp.df=df
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==0]="PDO"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

surface_miles=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_DESIGN_SPEED_AMT)]

temp.df=setDT(temp.df)[, .(freq=length(unique(CASE_ID))), by=.(THY_DESIGN_SPEED_AMT, COLLISION_SEVERITY)]
temp.df$freq=temp.df$freq/surface_miles$miles[match(temp.df$THY_DESIGN_SPEED_AMT, surface_miles$THY_DESIGN_SPEED_AMT)]
temp.df=temp.df %>%
  group_by(THY_DESIGN_SPEED_AMT, COLLISION_SEVERITY) %>% summarise(count=freq) %>%
  mutate(pct=round(count/sum(count), 2))

temp.df$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.df$COLLISION_SEVERITY)

ggplot(data=temp.df, aes(x=THY_DESIGN_SPEED_AMT, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  #geom_text(aes(label=pct), size=6, vjust=0, angle=0, hjust=0.5)+
  geom_text(aes(label=pct), size=6, angle=0, position = position_stack(vjust = 0.5))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID per mile")+
  xlab("Speed")+
  labs(fill="Collision Severity")
####################################
#################################### by ADT
####################################
temp.df=df
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==0]="PDO"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

temp.df$THY_ADT_AMT=as.numeric(gsub(",", "", temp.df$THY_ADT_AMT))

ADT.func=function(ADT){
  if (is.na(ADT)){
    return(NA)
  }
  if (ADT < 10000){
    return(1)
  }
  if (ADT>=10000 && ADT<25000){
    return(2)
  }
  if (ADT>=25000 && ADT < 50000){
    return(3)
  }
  if(ADT >= 50000 && ADT<100000){
    return(4)
  }
  if (ADT>=100000 && ADT<150000){
    return(5)
  }
  if (ADT>=150000 && ADT<200000){
    return(6)
  }
  if (ADT>=200000 && ADT<250000){
    return(7)
  }
  if (ADT>=250000 && ADT<300000){
    return(8)
  }
  if (ADT>=300000 && ADT<350000){
    return(9)
  }
  if (ADT>=350000 && ADT<400000){
    return(10)
  }
  if (ADT>=400000 && ADT<=461358){
    return(11)
  }
}

temp.df$THY_ADT_AMT=unlist(lapply(temp.df$THY_ADT_AMT, function(x) ADT.func(x)))

temp_Marker=hwMarker
temp_Marker$THY_ADT_AMT=as.numeric(gsub(",","",temp_Marker$THY_ADT_AMT))
temp_Marker$THY_ADT_AMT=unlist(lapply(temp_Marker$THY_ADT_AMT, function(x) ADT.func(x)))

surface_miles=setDT(temp_Marker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_ADT_AMT)]

temp.df=setDT(temp.df)[, .(freq=length(unique(CASE_ID))), by=.(THY_ADT_AMT, COLLISION_SEVERITY)]
temp.df$freq=temp.df$freq/surface_miles$miles[match(temp.df$THY_ADT_AMT, surface_miles$THY_ADT_AMT)]
temp.df=temp.df %>%
  group_by(THY_ADT_AMT, COLLISION_SEVERITY) %>% summarise(count=freq) %>%
  mutate(pct=round(count/sum(count), 2))

temp.df$COLLISION_SEVERITY=gsub(" or ", " or \n", temp.df$COLLISION_SEVERITY)

ggplot(data=temp.df, aes(x=THY_ADT_AMT, y=count, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_text_repel(aes(label=pct), size=6, vjust=0.1, angle=0, hjust=0.5)+
  scale_x_continuous(limits=c(0.5, 11.5), breaks=seq(1, 11, 1), labels = seq(1, 11, 1))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ylab("Count case ID per mile")+
  xlab("Average daily traffic groups")+
  labs(fill="Collision Severity")
