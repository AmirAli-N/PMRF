library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(viridis)
setwd("//ahmct-065/teams/PMRF/Amir")

df=fread(file = "./bin/Final Datasets/SWITRS.csv", sep=",", header = TRUE)
###########################################################################################
###########################################################################################
ggplot(data=df, aes(x=TYPE_OF_COLLISION, fill=TYPE_OF_COLLISION))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"), legend.position = "none")+
  ylab("count")+
  xlab("type of collision")+
  labs(fill="collision")

temp.df=setDT(df)[order(ACCIDENT_YEAR, TYPE_OF_COLLISION), .(count=length(unique(CASE_ID))), 
                  by=.(ACCIDENT_YEAR, TYPE_OF_COLLISION)]

temp.df=pivot_wider(temp.df, id_cols = TYPE_OF_COLLISION, names_from = ACCIDENT_YEAR, 
                    values_from = count)
fwrite(temp.df, file="pat.csv", sep=",", append=FALSE)
###########################################################################################
###########################################################################################
#temp.df=df[which(df$ROAD_COND_1=="D" | df$ROAD_COND_2=="D"), ]
#temp.df=setDT(temp.df)[, .(count=length(unique(CASE_ID))), by=.(TYPE_OF_COLLISION, COLLISION_SEVERITY)]
temp.df=setDT(df)[order(TYPE_OF_COLLISION, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), by=.(TYPE_OF_COLLISION, COLLISION_SEVERITY)]
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==0]="PDO"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==1]="Fatal"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==2]="Severe Injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==3]="Other Visible Injury"
temp.df$COLLISION_SEVERITY[temp.df$COLLISION_SEVERITY==4]="Complaint of Pain"

temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="A"]="Head-On"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="B"]="Sideswipe"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="C"]="Rear End"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="D"]="Broadside"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="E"]="Hit Object"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="F"]="Overturned"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="G"]="Vehicle/Pedestrian"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="H"]="Other"
temp.df$TYPE_OF_COLLISION[temp.df$TYPE_OF_COLLISION=="-"]="Not Stated"

temp.df=temp.df%>%group_by(TYPE_OF_COLLISION)%>%
  mutate(pct=round(count/sum(count),2))
temp.df=cbind.data.frame(temp.df, ROAD_CONDITION="All Collisions")

wz.df=df[which(df$ROAD_COND_1=="D" | df$ROAD_COND_2=="D"), ]
wz.df=setDT(wz.df)[order(TYPE_OF_COLLISION, COLLISION_SEVERITY), .(count=length(unique(CASE_ID))), by=.(TYPE_OF_COLLISION, COLLISION_SEVERITY)]
wz.df$COLLISION_SEVERITY[wz.df$COLLISION_SEVERITY==0]="PDO"
wz.df$COLLISION_SEVERITY[wz.df$COLLISION_SEVERITY==1]="Fatal"
wz.df$COLLISION_SEVERITY[wz.df$COLLISION_SEVERITY==2]="Severe Injury"
wz.df$COLLISION_SEVERITY[wz.df$COLLISION_SEVERITY==3]="Other Visible Injury"
wz.df$COLLISION_SEVERITY[wz.df$COLLISION_SEVERITY==4]="Complaint of Pain"

wz.df$TYPE_OF_COLLISION[wz.df$TYPE_OF_COLLISION=="A"]="Head-On"
wz.df$TYPE_OF_COLLISION[wz.df$TYPE_OF_COLLISION=="B"]="Sideswipe"
wz.df$TYPE_OF_COLLISION[wz.df$TYPE_OF_COLLISION=="C"]="Rear End"
wz.df$TYPE_OF_COLLISION[wz.df$TYPE_OF_COLLISION=="D"]="Broadside"
wz.df$TYPE_OF_COLLISION[wz.df$TYPE_OF_COLLISION=="E"]="Hit Object"
wz.df$TYPE_OF_COLLISION[wz.df$TYPE_OF_COLLISION=="F"]="Overturned"
wz.df$TYPE_OF_COLLISION[wz.df$TYPE_OF_COLLISION=="G"]="Vehicle/Pedestrian"
wz.df$TYPE_OF_COLLISION[wz.df$TYPE_OF_COLLISION=="H"]="Other"
wz.df$TYPE_OF_COLLISION[wz.df$TYPE_OF_COLLISION=="-"]="Not Stated"

wz.df=wz.df%>%group_by(TYPE_OF_COLLISION)%>%
  mutate(pct=round(count/sum(count),2))
wz.df=cbind.data.frame(wz.df, ROAD_CONDITION="Workzone Collisions")

label_temp=temp.df%>%group_by(COLLISION_SEVERITY)%>%top_n(2, pct)
temp.df=merge(temp.df, label_temp[, c("TYPE_OF_COLLISION", "COLLISION_SEVERITY", "pct")], 
              by=c("TYPE_OF_COLLISION", "COLLISION_SEVERITY"), all.x = TRUE)

label_wz=wz.df%>%group_by(COLLISION_SEVERITY)%>%top_n(2, pct)
wz.df=merge(wz.df, label_wz[, c("TYPE_OF_COLLISION", "COLLISION_SEVERITY", "pct")], 
              by=c("TYPE_OF_COLLISION", "COLLISION_SEVERITY"), all.x = TRUE)

ggplot(data=temp.df, aes(x=COLLISION_SEVERITY, y=TYPE_OF_COLLISION, size=pct.x, 
                           fill=pct.x)) +
  geom_point(shape=21, alpha=0.5)+
  labs(fill="Fraction of \ncollision severity \nin type of collision")+
  scale_size(range = c(1, 25))+
  scale_fill_viridis(discrete=FALSE, option="A") +
  theme_ipsum(axis_title_just = 'center') +
  geom_text(aes(label=pct.y), size=6, vjust=-.9, angle=0, hjust=-.9, show.legend = FALSE)+
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(angle = 0, vjust = 0.5, size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  guides(size=FALSE)+
  ggtitle("All state road collisions")
  ylab("Type of Collision")+
  xlab("Collision Severity")
  
ggplot(data=wz.df, aes(x=COLLISION_SEVERITY, y=TYPE_OF_COLLISION, size=pct.x, 
                           fill=pct.x)) +
  geom_point(shape=21, alpha=0.5)+
  labs(fill="Fraction of \ncollision severity \nin type of collision")+
  scale_size(range = c(1, 25))+
  scale_fill_viridis(discrete=FALSE, option="A") +
  theme_ipsum(axis_title_just = 'center') +
  geom_text(aes(label=pct.y), size=6, vjust=-.9, angle=0, hjust=-.9, show.legend = FALSE)+
  theme(plot.title = element_text(angle = 0, hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(angle = 0, vjust = 0.5, size=14),
        axis.title.y = element_text(size=20, face = "bold"),
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  guides(size=FALSE)+
  ggtitle("All state road work zone collisions")
  ylab("Type of Collision")+
  xlab("Collision Severity")

setwd("C:/Users/anasr/Desktop")
all_wz.df=rbind.data.frame(temp.df, wz.df)
all_wz.df=setDT(all_wz.df)[order(TYPE_OF_COLLISION, COLLISION_SEVERITY, ROAD_CONDITION), ,]
all_wz.df=pivot_wider(all_wz.df, id_cols = c(TYPE_OF_COLLISION, ROAD_CONDITION), names_from = COLLISION_SEVERITY, 
                      values_from = pct)
fwrite(all_wz.df, file="type_severity-all.csv", sep=",", append = FALSE)
