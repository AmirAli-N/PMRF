setwd("//ahmct-065/teams/PMRF/Amir")

library(data.table)
library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(forcats)



temp.chp=collision_match.df
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==0]="PDO"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

temp.chp=temp.chp[order(Activity, -COLLISION_SEVERITY), .(len.id=length(unique(CASE_ID))), by=.(Activity, COLLISION_SEVERITY)]

cutoff_pdo=quantile(temp.chp$len.id[which(temp.chp$COLLISION_SEVERITY=="PDO")])[4]
cutoff_injury=quantile(temp.chp$len.id[which(temp.chp$COLLISION_SEVERITY=="Fatality or symptomatic injury")])[4]

label.df=LEMO_LCS_AADT.df[order(Activity), .(len.tot=length(unique(`Work Order No`))) , by=.(Activity, `Activity Description`)]
label.df=label.df[-c(which(label.df$Activity=="D40050" & label.df$`Activity Description`=="ILLEGAL ENCAMPMENT DEBRIS RMVL")),]
temp.chp=merge(temp.chp, label.df, by="Activity", all.x=TRUE)

temp.chp$`Activity Description`[which(temp.chp$len.id<cutoff_injury & temp.chp$COLLISION_SEVERITY=="Fatality or symptomatic injury")]=""
temp.chp$`Activity Description`[which(temp.chp$COLLISION_SEVERITY=="PDO")]=""

ggplot(data=temp.chp, aes(x=Activity, y=len.id, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_hline(yintercept = cutoff_pdo, linetype="dashed", color="blue")+
  geom_hline(yintercept = cutoff_injury, linetype="dashed", color="red")+
  geom_text(aes(label=`Activity Description`), size=4, vjust=0.4, angle=90, hjust="bottom")+
  #scale_x_continuous(limits=c(-1, 26), breaks=seq(0, 24, 1), labels = time.label)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylab("COUNT CASE_ID")

################################################################################################
################################################################################################
################################################################################################

temp.chp=collision_match.df
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==0]="PDO"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

temp.chp=temp.chp[order(Activity, -COLLISION_SEVERITY), .(len.id=length(unique(CASE_ID))), by=.(Activity, COLLISION_SEVERITY)]

label.df=LEMO_LCS_AADT.df[order(Activity), .(len.tot=length(unique(`Work Order No`))) , by=.(Activity, `Activity Description`)]
label.df=label.df[-c(which(label.df$Activity=="D40050" & label.df$`Activity Description`=="ILLEGAL ENCAMPMENT DEBRIS RMVL")),]
temp.chp=merge(temp.chp, label.df, by="Activity", all.x=TRUE)

temp.chp$len.id=temp.chp$len.id / temp.chp$len.tot

cutoff_pdo=quantile(temp.chp$len.id[which(temp.chp$COLLISION_SEVERITY=="PDO")])[4]
cutoff_injury=quantile(temp.chp$len.id[which(temp.chp$COLLISION_SEVERITY=="Fatality or symptomatic injury")])[4]

temp.chp$`Activity Description`[which(temp.chp$len.id<cutoff_injury & temp.chp$COLLISION_SEVERITY=="Fatality or symptomatic injury")]=""
temp.chp$`Activity Description`[which(temp.chp$COLLISION_SEVERITY=="PDO")]=""

ggplot(data=temp.chp, aes(x=Activity, y=len.id, fill=forcats::fct_rev(COLLISION_SEVERITY)))+
  geom_bar(stat="identity")+
  geom_hline(yintercept = cutoff_pdo, linetype="dashed", color="blue")+
  geom_hline(yintercept = cutoff_injury, linetype="dashed", color="red")+
  geom_text(aes(label=`Activity Description`), size=4, vjust=0.4, angle=90, hjust="bottom")+
  #scale_x_continuous(limits=c(-1, 26), breaks=seq(0, 24, 1), labels = time.label)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylab("COUNT CASE_ID")

################################################################################################
################################################################################################
################################################################################################
collision_match.df=fread(file="./bin/2020-02-10_2013 bin/CHP.matches.csv", sep=",", header=TRUE)
LEMO_LCS_AADT.df=fread(file="./bin/2020-02-10_2013 bin/LEMO_LCS_AADT.summary.csv", sep=",", header=TRUE)

temp.chp=collision_match.df
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==0]="PDO"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==1]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==2]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==3]="Fatality or symptomatic injury"
temp.chp$COLLISION_SEVERITY[temp.chp$COLLISION_SEVERITY==4]="Fatality or symptomatic injury"

temp.LEMO=LEMO_LCS_AADT.df[,c("Work Order No", "Activity", "Workdate", "matchType", "Status", "Work Type")]
colnames(temp.chp)[1]="Work Order No"
temp.chp=temp.chp[,c("Work Order No", "Activity", "Workdate", "CASE_ID", "COLLISION_SEVERITY")]

temp.merge=temp.chp %>% left_join(temp.LEMO, by=c("Work Order No", "Activity", "Workdate"))
temp.merge=distinct(temp.merge)

temp.merge$matchType[temp.merge$matchType=="No postmile match"]="no match"
temp.merge$matchType[temp.merge$matchType=="No date match"]="no match"
temp.merge$matchType[temp.merge$matchType=="No route match"]="no match"

temp.merge=temp.merge[c(which((temp.merge$Status=="Approved" & temp.merge$matchType=="exact/partial match") |
                                (temp.merge$matchType=="no match"))),]
temp.merge=setDT(temp.merge)[order(Activity, -COLLISION_SEVERITY), .(len.id=length(unique(CASE_ID))), 
                             by=.(Activity, COLLISION_SEVERITY, matchType)]

label.df=LEMO_LCS_AADT.df[order(Activity), .(len.tot=length(unique(`Work Order No`))) , by=.(Activity, `Activity Description`)]
label.df=label.df[-c(which(label.df$Activity=="D40050" & label.df$`Activity Description`=="ILLEGAL ENCAMPMENT DEBRIS RMVL")),]

temp.merge=merge(temp.merge, label.df, by="Activity", all.x=TRUE)

df=setDT(temp.merge)[order(Activity, `Activity Description`, COLLISION_SEVERITY, matchType)]

# ggplot(data=temp.merge, aes(x=Activity, y=interaction(matchType, COLLISION_SEVERITY), size=len.id, fill=Activity))+
#   geom_point(shape=21, alpha=0.5)+
#   scale_size(range=c(0.5, 15))+
#   #geom_hline(yintercept = cutoff_pdo, linetype="dashed", color="blue")+
#   #geom_hline(yintercept = cutoff_injury, linetype="dashed", color="red")+
#   #geom_text(aes(label=`Activity Description`), size=4, vjust=0.4, angle=90, hjust="bottom")+
#   #scale_x_continuous(limits=c(-1, 26), breaks=seq(0, 24, 1), labels = time.label)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

temp.mergeAB=temp.merge[substr(Activity,1,1)=="A" | substr(Activity,1,1)=="B",]
temp.mergeC=temp.merge[substr(Activity,1,1)=="C",]
temp.mergeD=temp.merge[substr(Activity,1,1)=="D",]
temp.mergeE=temp.merge[substr(Activity,1,1)=="E",]
temp.mergeFHJ=temp.merge[substr(Activity,1,1)=="F" | substr(Activity,1,1)=="H" | substr(Activity,1,1)=="J",]
temp.mergeK=temp.merge[substr(Activity,1,1)=="K"]
temp.mergeM=temp.merge[substr(Activity,1,1)=="M"]
temp.mergeRSY=temp.merge[substr(Activity,1,1)=="R" | substr(Activity,1,1)=="S" | substr(Activity,1,1)=="Y",]

p1=ggplot(data=temp.mergeAB, aes(x=Activity, y=len.id, group=interaction(matchType, COLLISION_SEVERITY), fill=interaction(matchType, COLLISION_SEVERITY)))+
  geom_bar(position="dodge", stat="identity")+
  theme(plot.title = element_text(size=20, face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, size=14),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=20, face = "bold"), legend.position = "none")+
  ggtitle("A & B Family")+ylab("No. of work zone collision")

p1

p2=ggplot(data=temp.mergeC, aes(x=Activity, y=len.id, group=interaction(matchType, COLLISION_SEVERITY), fill=interaction(matchType, COLLISION_SEVERITY)))+
  geom_bar(position="dodge", stat="identity")+
  theme(plot.title = element_text(size=20, face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, size=14),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_blank(), legend.position = "none")+
  ggtitle("C Family")

p2

p3=ggplot(data=temp.mergeD, aes(x=Activity, y=len.id, group=interaction(matchType, COLLISION_SEVERITY), fill=interaction(matchType, COLLISION_SEVERITY)))+
  geom_bar(position="dodge", stat="identity")+
  theme(plot.title = element_text(size=20, face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, size=14),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_blank(), legend.position = "none")+
  ggtitle("D Family")

p4=ggplot(data=temp.mergeE, aes(x=Activity, y=len.id, group=interaction(matchType, COLLISION_SEVERITY), fill=interaction(matchType, COLLISION_SEVERITY)))+
  geom_bar(position="dodge", stat="identity")+
  theme(plot.title = element_text(size=20, face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, size=14),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_blank(), legend.position = "none")+
  ggtitle("E Family")

p5=ggplot(data=temp.mergeFHJ, aes(x=Activity, y=len.id, group=interaction(matchType, COLLISION_SEVERITY), fill=interaction(matchType, COLLISION_SEVERITY)))+
  geom_bar(position="dodge", stat="identity")+
  theme(plot.title = element_text(size=20, face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, size=14),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_blank(), legend.position = "none")+
  ggtitle("F, H & J Family")

p6=ggplot(data=temp.mergeK, aes(x=Activity, y=len.id, group=interaction(matchType, COLLISION_SEVERITY), fill=interaction(matchType, COLLISION_SEVERITY)))+
  geom_bar(position="dodge", stat="identity")+
  theme(plot.title = element_text(size=20, face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, size=14),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_blank(), legend.position = "none")+
  ggtitle("K Family")

p7=ggplot(data=temp.mergeM, aes(x=Activity, y=len.id, group=interaction(matchType, COLLISION_SEVERITY), fill=interaction(matchType, COLLISION_SEVERITY)))+
  geom_bar(position="dodge", stat="identity")+
  theme(plot.title = element_text(size=20, face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, size=14),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_blank(), legend.position = "none")+
  ggtitle("M Family")

p8=ggplot(data=temp.mergeRSY, aes(x=Activity, y=len.id, group=interaction(matchType, COLLISION_SEVERITY), fill=interaction(matchType, COLLISION_SEVERITY)))+
  geom_bar(position="dodge", stat="identity")+
  theme(plot.title = element_text(size=20, face="bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=14),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_blank(), 
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=14))+
  ggtitle("R, S & Y Family")+labs(fill="Match type.Collision severity")
  
p8

get_legend=function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend=get_legend(p8)

p8=ggplot(data=temp.mergeRSY, aes(x=Activity, y=len.id, group=interaction(matchType, COLLISION_SEVERITY), fill=interaction(matchType, COLLISION_SEVERITY)))+
  geom_bar(position="dodge", stat="identity")+
  theme(plot.title = element_text(size=20, face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, size=14),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14),
        axis.title.y = element_blank(), legend.position = "none")+
  ggtitle("R, S & Y Family")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, legend, nrow=3)
