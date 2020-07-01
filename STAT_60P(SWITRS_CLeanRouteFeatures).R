library(data.table)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggrepel)
library(stringr)
setwd("//ahmct-065/teams/PMRF/Amir")

hwMarker=fread(file="HighwayElementMarkers.csv", sep=",", header = TRUE)
df=fread(file="./CHP/CHP_CleanRouteFeatures.cleaned.csv", sep=",", header=TRUE)

####################################
#################################### by surface_type_desc
####################################
surface_miles_LT=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_LT_SURF_TYPE_DESC)]
colnames(surface_miles_LT)=c("THY_SURF_TYPE_DESC", "miles")
surface_miles_RT=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_RT_SURF_TYPE_DESC)]
colnames(surface_miles_RT)=c("THY_SURF_TYPE_DESC", "miles")
surface_miles=bind_rows(surface_miles_LT, surface_miles_RT) %>%
  group_by(THY_SURF_TYPE_DESC) %>%
  summarise_each(funs(sum))
surface_miles$THY_SURF_TYPE_DESC=gsub("  ", " - ", surface_miles$THY_SURF_TYPE_DESC)

temp.df=df
temp.df=setDT(temp.df)[, .(freq=length(unique(collision_id))), 
                       by=.(surface_type_descr, collision_severity)]
temp.df$surface_type_descr=gsub("  ", " - ", temp.df$surface_type_descr)
temp.df$freq=temp.df$freq / surface_miles$miles[match(temp.df$surface_type_descr, 
                                                      surface_miles$THY_SURF_TYPE_DESC)]
temp.df=temp.df %>%
  group_by(surface_type_descr, collision_severity) %>% summarise(count=freq) %>%
  mutate(pct=round(count/sum(count), 2))

temp.df1=temp.df
temp.df1$collision_severity[temp.df1$collision_severity==0]="PDO"
temp.df1$collision_severity[temp.df1$collision_severity==1]="Fatality or symptomatic injury"
temp.df1$collision_severity[temp.df1$collision_severity==2]="Fatality or symptomatic injury"
temp.df1$collision_severity[temp.df1$collision_severity==3]="Fatality or symptomatic injury"
temp.df1$collision_severity[temp.df1$collision_severity==4]="Fatality or symptomatic injury"
temp.df1$surface_type_descr=str_wrap(temp.df1$surface_type_descr, 15)
temp.df1$collision_severity=str_wrap(temp.df1$collision_severity, 20)

temp.df$collision_severity[temp.df$collision_severity==0]="PDO"
temp.df$collision_severity[temp.df$collision_severity==1]="Fatal"
temp.df$collision_severity[temp.df$collision_severity==2]="Severe injury"
temp.df$collision_severity[temp.df$collision_severity==3]="Visible injury"
temp.df$collision_severity[temp.df$collision_severity==4]="Complaint of pain"
temp.df$surface_type_descr=str_wrap(temp.df$surface_type_descr, 15)
temp.df$collision_severity=str_wrap(temp.df$collision_severity, 15)


temp.df1=na.omit(temp.df1)
p.main=ggplot(data=temp.df1, aes(x=surface_type_descr, y=count, fill=collision_severity))+
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

temp.df=na.omit(temp.df)
temp.df$collision_severity=factor(temp.df$collision_severity, levels=c("PDO", 
                                                                       "Complaint of pain", 
                                                                       "Visible injury", 
                                                                       "Severe injury", 
                                                                       "Fatal"))
p.severity=ggplot(data=temp.df, aes(x=surface_type_descr, y=count, fill=collision_severity))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size=18, family = "Century Gothic"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin=margin(0, 20, 0, 0), size=18, family = "Century Gothic"),
        legend.title = element_text(size=18, family = "Century Gothic"),
        legend.text = element_text(size=18, family = "Century Gothic"),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ylab("Number of collisions")+
  labs(fill="Collision severity")+
  facet_wrap(~collision_severity, ncol=1, scales = "free_y")+
  scale_x_discrete(breaks=temp.df1$surface_type_descr)+
  theme(strip.text.x = element_text(size = 18, family = "Century Gothic"),
        strip.text.y = element_text(size = 18, family = "Century Gothic"))

ggarrange(p.main, p.severity, heights = c(2, 4))
####################################
#################################### by median_type
####################################
surface_miles=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_MEDIAN_TYPE_DESC)]

temp.df=df
temp.df=setDT(temp.df)[, .(freq=length(unique(collision_id))), 
                       by=.(median_type_descr, collision_severity)]
temp.df$freq=temp.df$freq / surface_miles$miles[match(temp.df$median_type_descr, 
                                                      surface_miles$THY_MEDIAN_TYPE_DESC)]
temp.df=temp.df %>%
  group_by(median_type_descr, collision_severity) %>% summarise(count=freq) %>%
  mutate(pct=round(count/sum(count), 2))

temp.df1=temp.df
temp.df1$collision_severity[temp.df1$collision_severity==0]="PDO"
temp.df1$collision_severity[temp.df1$collision_severity==1]="Fatality or symptomatic injury"
temp.df1$collision_severity[temp.df1$collision_severity==2]="Fatality or symptomatic injury"
temp.df1$collision_severity[temp.df1$collision_severity==3]="Fatality or symptomatic injury"
temp.df1$collision_severity[temp.df1$collision_severity==4]="Fatality or symptomatic injury"
temp.df1$median_type_descr=str_wrap(temp.df1$median_type_descr, 10)
temp.df1$collision_severity=str_wrap(temp.df1$collision_severity, 20)

temp.df$collision_severity[temp.df$collision_severity==0]="PDO"
temp.df$collision_severity[temp.df$collision_severity==1]="Fatal"
temp.df$collision_severity[temp.df$collision_severity==2]="Severe injury"
temp.df$collision_severity[temp.df$collision_severity==3]="Visible injury"
temp.df$collision_severity[temp.df$collision_severity==4]="Complaint of pain"
temp.df$median_type_descr=str_wrap(temp.df$median_type_descr, 10)


temp.df1=na.omit(temp.df1)
p.main=ggplot(data=temp.df1, aes(x=median_type_descr, y=count, fill=collision_severity))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center') +
  theme(axis.text.x = element_text(angle =0, hjust = 0.5, size=18, family = "Century Gothic"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 20, 0, 0), size=18, family = "Century Gothic"),
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
p.severity=ggplot(data=temp.df, aes(x=median_type_descr, y=count, fill=collision_severity))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size=18, family = "Century Gothic"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin=margin(0, 20, 0, 0), size=18, family = "Century Gothic"),
        legend.title = element_text(size=18, family = "Century Gothic"),
        legend.text = element_text(size=18, family = "Century Gothic"),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ylab("Number of collisions")+
  labs(fill="Collision severity")+
  facet_wrap(~collision_severity, ncol=1, scales = "free_y")+
  theme(strip.text.x = element_text(size = 18, family = "Century Gothic"),
        strip.text.y = element_text(size = 18, family = "Century Gothic"))

ggarrange(p.main, p.severity, heights = c(2, 4))
####################################
#################################### by group_code_desc
####################################
surface_miles=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_HIGHWAY_GROUP_DESC)]

temp.df=df
temp.df=setDT(temp.df)[, .(freq=length(unique(collision_id))), 
                       by=.(hwy_group_descr, collision_severity)]
temp.df$freq=temp.df$freq / surface_miles$miles[match(temp.df$hwy_group_descr, 
                                                    surface_miles$THY_HIGHWAY_GROUP_DESC)]
temp.df=temp.df %>%
  group_by(hwy_group_descr, collision_severity) %>% summarise(count=freq) %>%
  mutate(pct=round(count/sum(count), 2))

temp.df1=temp.df
temp.df1$collision_severity[temp.df1$collision_severity==0]="PDO"
temp.df1$collision_severity[temp.df1$collision_severity==1]="Fatality or symptomatic injury"
temp.df1$collision_severity[temp.df1$collision_severity==2]="Fatality or symptomatic injury"
temp.df1$collision_severity[temp.df1$collision_severity==3]="Fatality or symptomatic injury"
temp.df1$collision_severity[temp.df1$collision_severity==4]="Fatality or symptomatic injury"
temp.df1$hwy_group_descr=str_wrap(temp.df1$hwy_group_descr, 15)
temp.df1$collision_severity=str_wrap(temp.df1$collision_severity, 20)

temp.df$collision_severity[temp.df$collision_severity==0]="PDO"
temp.df$collision_severity[temp.df$collision_severity==1]="Fatal"
temp.df$collision_severity[temp.df$collision_severity==2]="Severe injury"
temp.df$collision_severity[temp.df$collision_severity==3]="Visible injury"
temp.df$collision_severity[temp.df$collision_severity==4]="Complaint of pain"
temp.df$hwy_group_descr=str_wrap(temp.df$hwy_group_descr, 15)

temp.df1=na.omit(temp.df1)
p.main=ggplot(data=temp.df1, aes(x=hwy_group_descr, y=count, fill=collision_severity))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center') +
  theme(axis.text.x = element_text(angle =0, hjust = 0.5, size=18, family = "Century Gothic"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 20, 0, 0), size=18, family = "Century Gothic"),
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
p.severity=ggplot(data=temp.df, aes(x=hwy_group_descr, y=count, fill=collision_severity))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size=18, family = "Century Gothic"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin=margin(0, 20, 0, 0), size=18, family = "Century Gothic"),
        legend.title = element_text(size=18, family = "Century Gothic"),
        legend.text = element_text(size=18, family = "Century Gothic"),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ylab("Number of collisions")+
  labs(fill="Collision severity")+
  facet_wrap(~collision_severity, ncol=1, scales = "free_y")+
  theme(strip.text.x = element_text(size = 18, family = "Century Gothic"),
        strip.text.y = element_text(size = 18, family = "Century Gothic"))

ggarrange(p.main, p.severity, heights = c(2, 4))
####################################
#################################### by number of lanes
####################################
surface_miles_LT=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_LT_LANES_AMT)]
colnames(surface_miles_LT)=c("THY_LANES_AMT", "miles")
surface_miles_RT=setDT(hwMarker)[, .(miles=sum(THY_LENGTH_MILES_AMT)), by=.(THY_RT_LANES_AMT)]
colnames(surface_miles_RT)=c("THY_LANES_AMT", "miles")
surface_miles=bind_rows(surface_miles_LT, surface_miles_RT) %>%
  group_by(THY_LANES_AMT) %>%
  summarise_each(funs(sum))

temp.df=df
temp.df=setDT(temp.df)[, .(freq=length(unique(collision_id))), 
                       by=.(num_lanes, collision_severity)]
temp.df$freq=temp.df$freq / surface_miles$miles[match(temp.df$num_lanes, 
                                                      surface_miles$THY_LANES_AMT)]
temp.df=temp.df %>%
  group_by(num_lanes, collision_severity) %>% summarise(count=freq) %>%
  mutate(pct=round(count/sum(count), 2))

temp.df1=temp.df
temp.df1$collision_severity[temp.df1$collision_severity==0]="PDO"
temp.df1$collision_severity[temp.df1$collision_severity==1]="Fatality or symptomatic injury"
temp.df1$collision_severity[temp.df1$collision_severity==2]="Fatality or symptomatic injury"
temp.df1$collision_severity[temp.df1$collision_severity==3]="Fatality or symptomatic injury"
temp.df1$collision_severity[temp.df1$collision_severity==4]="Fatality or symptomatic injury"
temp.df1$num_lanes=str_wrap(temp.df1$num_lanes, 15)
temp.df1$collision_severity=str_wrap(temp.df1$collision_severity, 20)

temp.df$collision_severity[temp.df$collision_severity==0]="PDO"
temp.df$collision_severity[temp.df$collision_severity==1]="Fatal"
temp.df$collision_severity[temp.df$collision_severity==2]="Severe injury"
temp.df$collision_severity[temp.df$collision_severity==3]="Visible injury"
temp.df$collision_severity[temp.df$collision_severity==4]="Complaint of pain"
temp.df$num_lanes=str_wrap(temp.df$num_lanes, 15)

temp.df1=na.omit(temp.df1)
p.main=ggplot(data=temp.df1, aes(x=num_lanes, y=count, fill=collision_severity))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center') +
  theme(axis.text.x = element_text(angle =0, hjust = 0.5, size=18, family = "Century Gothic"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 20, 0, 0), size=18, family = "Century Gothic"),
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
p.severity=ggplot(data=temp.df, aes(x=num_lanes, y=count, fill=collision_severity))+
  geom_bar(stat="identity")+
  theme_ipsum(axis_title_just = 'center') +
  theme(axis.text.x = element_text(size=18, family = "Century Gothic"),
        axis.text.y = element_text(size=18, family = "Century Gothic"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin=margin(0, 20, 0, 0), size=18, family = "Century Gothic"),
        legend.title = element_text(size=18, family = "Century Gothic"),
        legend.text = element_text(size=18, family = "Century Gothic"),
        plot.margin = unit(c(0.2,0,0.2,0), "lines"),
        panel.spacing = unit(0.2, "lines"))+
  ylab("Number of collisions")+
  labs(fill="Collision severity")+
  facet_wrap(~collision_severity, ncol=2, scales = "free_y")+
  theme(strip.text.x = element_text(size = 18, family = "Century Gothic"),
        strip.text.y = element_text(size = 18, family = "Century Gothic"))

ggarrange(p.main, p.severity, heights = c(2, 4))
####################################
#################################### by design speed
####################################
temp.df=df[,c("wono", "collision_severity", "road_speed")]
temp.df$collision_severity[temp.df$collision_severity==0]="PDO"
temp.df$collision_severity[temp.df$collision_severity==1]="Fatal"
temp.df$collision_severity[temp.df$collision_severity==2]="Severe injury"
temp.df$collision_severity[temp.df$collision_severity==3]="Visible injury"
temp.df$collision_severity[temp.df$collision_severity==4]="Complaint of pain"

temp.df$road_speed=as.numeric(gsub(",", "", temp.df$road_speed))

temp.df$collision_severity=factor(temp.df$collision_severity, levels=c("PDO", 
                                                                       "Complaint of pain", 
                                                                       "Visible injury", 
                                                                       "Severe injury", 
                                                                       "Fatal"))
temp.df=na.omit(temp.df)
ggplot(temp.df, aes(x=collision_severity, y=road_speed, fill=collision_severity))+
  geom_violin(trim = TRUE, scale = "width", na.rm = TRUE)+
  #scale_y_continuous(limits = c(0, 15))+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic"),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 18, family = "Century Gothic"),
        axis.text.y = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic"))+
  xlab("Collision severity")+
  ylab("Design speed (mph)")
####################################
#################################### by ADT
####################################
temp.df=df[,c("wono", "collision_severity", "road_adt")]
temp.df$collision_severity[temp.df$collision_severity==0]="PDO"
temp.df$collision_severity[temp.df$collision_severity==1]="Fatal"
temp.df$collision_severity[temp.df$collision_severity==2]="Severe injury"
temp.df$collision_severity[temp.df$collision_severity==3]="Visible injury"
temp.df$collision_severity[temp.df$collision_severity==4]="Complaint of pain"

temp.df$road_adt=as.numeric(gsub(",", "", temp.df$road_adt))

temp.df$collision_severity=factor(temp.df$collision_severity, levels=c("PDO", 
                                                                       "Complaint of pain", 
                                                                       "Visible injury", 
                                                                       "Severe injury", 
                                                                       "Fatal"))
temp.df=na.omit(temp.df)
ggplot(temp.df, aes(x=collision_severity, y=road_adt, fill=collision_severity))+
  geom_violin(trim = TRUE, scale = "width", na.rm = TRUE)+
  #scale_y_continuous(limits = c(0, 15))+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic"),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 18, family = "Century Gothic"),
        axis.text.y = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic"))+
  xlab("Collision severity")+
  ylab("Average daily traffic (number of vehicles)")
####################################
#################################### by density
####################################
temp.df=df[,c("wono", "collision_severity", "collision_density11_18")]
temp.df$collision_severity[temp.df$collision_severity==0]="PDO"
temp.df$collision_severity[temp.df$collision_severity==1]="Fatal"
temp.df$collision_severity[temp.df$collision_severity==2]="Severe injury"
temp.df$collision_severity[temp.df$collision_severity==3]="Visible injury"
temp.df$collision_severity[temp.df$collision_severity==4]="Complaint of pain"

temp.df$collision_density11_18=as.numeric(gsub(",", "", temp.df$collision_density11_18))

temp.df$collision_severity=factor(temp.df$collision_severity, levels=c("PDO", 
                                                                       "Complaint of pain", 
                                                                       "Visible injury", 
                                                                       "Severe injury", 
                                                                       "Fatal"))
temp.df=na.omit(temp.df)
ggplot(temp.df, aes(x=collision_severity, y=collision_density11_18, fill=collision_severity))+
  geom_violin(trim = TRUE, scale = "width", na.rm = TRUE)+
  #scale_y_continuous(limits = c(0, 15))+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic"),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 18, family = "Century Gothic"),
        axis.text.y = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic"))+
  xlab("Collision severity")+
  ylab("Collision density")
