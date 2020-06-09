library(data.table)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(plotly)
#######################################################
#############################number of unique workorder
#######################################################
tot_wono=length(unique(LEMO_WorkOrder.df$`Work Order No`))

#number of unique workorders by each district
LEMO_WorkOrder.dist.df=LEMO_WorkOrder.df[order(Dist), .(len.wono=length(unique(`Work Order No`))/tot_wono), by=.(Dist)]
LEMO_WorkOrder.dist.df$Dist=paste("Dist. ", LEMO_WorkOrder.dist.df$Dist, sep="")
fwrite(LEMO_WorkOrder.dist.df, file="./bin/WorkOrder.by.district.csv", sep=",", append = TRUE)
rm(LEMO_WorkOrder.dist.df)

#######################################################
##############most frequent activities in each district
#######################################################

LEMO_WorkOrder.dist.df=LEMO_WorkOrder.df[order(Dist, Activity), .(len.act=length(unique(`Work Order No`))), by=.(Dist, Activity, `Activity Description`)]
LEMO_WorkOrder.dist.label=LEMO_WorkOrder.dist.df %>% group_by(Dist) %>% top_n(n=5, wt=len.act)
LEMO_WorkOrder.dist.label=setDT(LEMO_WorkOrder.dist.label)[order(Dist, -len.act)]
LEMO_WorkOrder.dist.label=LEMO_WorkOrder.dist.label[which(LEMO_WorkOrder.dist.label$Dist==7 |
                                                            LEMO_WorkOrder.dist.label$Dist==4)]
fwrite(LEMO_WorkOrder.dist.label, file="./bin/5freq_act.by.dist.csv", sep=",", append = FALSE)

ggplot(data=LEMO_WorkOrder.dist.df[which(LEMO_WorkOrder.dist.df$Dist==7 |
                                         LEMO_WorkOrder.dist.df$Dist==4)], aes(x=Activity, y=Dist, size=len.act, fill=Activity)) +
  geom_point(alpha=0.5, shape=21) +
  geom_text(size=4, data=LEMO_WorkOrder.dist.label, aes(x=Activity, y=Dist, label=Activity), hjust=-0.6, angle=90, vjust=0.3)+
  scale_y_continuous(limits=c(4,8.5), breaks=c(4, 7), labels=c(4,7))+
  scale_size(range = c(0.01, 20), name="Number of activities")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

rm(LEMO_WorkOrder.dist.df, LEMO_WorkOrder.dist.label)

#######################################################
##############number of unique workorder by each county
#######################################################

LEMO_WorkOrder.county.df=LEMO_WorkOrder.df[order(beginCounty), .(len.wono=length(unique(`Work Order No`))/tot_wono), 
                                         by=.(beginCounty)]
LEMO_WorkOrder.county.df$beginCounty=county_abbr$COUNTY[match(LEMO_WorkOrder.county.df$beginCounty, county_abbr$`PEMS code`)]
fwrite(LEMO_WorkOrder.county.df, file="./bin/act.by.county.csv", sep=",", append = FALSE)

mean(LEMO_WorkOrder.county.df$len.wono)
median(LEMO_WorkOrder.county.df$len.wono)
quantile(LEMO_WorkOrder.county.df$len.wono)
cutoff=round(quantile(LEMO_WorkOrder.county.df$len.wono)[4],2)

  ggplot(data=LEMO_WorkOrder.county.df, aes(x=beginCounty, y=len.wono, fill=beginCounty))+
  geom_col()+
  geom_hline(yintercept = cutoff, linetype="dashed", color="red")+
  geom_text(aes(0, cutoff, label=cutoff, vjust=-1, hjust=-1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")+
  ylab("Number of work orders")+xlab("County")
rm(LEMO_WorkOrder.county.df)

#######################################################
##################################man hours by activity
#######################################################

LEMO_WorkOrder.hour.df=LEMO_WorkOrder.df[, .(hour.mean=mean(Hours.sum), hour.sum=sum(Hours.sum)), by=.(Activity, `Activity Description`)]
fwrite(LEMO_WorkOrder.hour.df, file="./bin/man_hour.by.activity.csv", sep=",", append = FALSE)

mean(LEMO_WorkOrder.hour.df$hour.sum)
median(LEMO_WorkOrder.hour.df$hour.sum)
quantile(LEMO_WorkOrder.hour.df$hour.sum)
cutoff=round(quantile(LEMO_WorkOrder.hour.df$hour.sum)[4])

LEMO_WorkOrder.hour.df$`Activity Description`[LEMO_WorkOrder.hour.df$hour.sum<cutoff]=""

ggplot(data=LEMO_WorkOrder.hour.df, aes(x=Activity, y=hour.sum, fill=Activity))+
  geom_col()+
  geom_text(size=3, aes(x=Activity, y=hour.sum, label=`Activity Description`), hjust=-0.05, angle=90, vjust=0.3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")+
  ylab("Total man hour")+xlab("Activity")+ylim(0, 650000)


LEMO_WorkOrder.hour.df=LEMO_WorkOrder.df[, .(hour.mean=mean(Hours.sum), hour.sum=sum(Hours.sum)), by=.(Activity, `Activity Description`)]

mean(LEMO_WorkOrder.hour.df$hour.mean)
median(LEMO_WorkOrder.hour.df$hour.mean)
quantile(LEMO_WorkOrder.hour.df$hour.mean)
cutoff=round(quantile(LEMO_WorkOrder.hour.df$hour.mean)[4])

LEMO_WorkOrder.hour.df$`Activity Description`[LEMO_WorkOrder.hour.df$hour.mean<cutoff]=""

ggplot(data=LEMO_WorkOrder.hour.df, aes(x=Activity, y=hour.mean, fill=Activity))+
  geom_col()+
  geom_text(size=3, aes(x=Activity, y=hour.mean, label=`Activity Description`), hjust=-0.05, angle=90, vjust=0.3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")+
  ylab("Average man hour")+xlab("Activity")+ylim(0, 80)

rm(LEMO_WorkOrder.hour.df)

#######################################################
#######################################cost by activity
#######################################################

LEMO_WorkOrder.cost.df=LEMO_WorkOrder.df[, .(cost.mean=mean(LEM.sum), cost.sum=sum(LEM.sum)), by=.(Activity, `Activity Description`)]
fwrite(LEMO_WorkOrder.cost.df, file="./bin/cost.by.activity.csv", sep=",", append=FALSE)

mean(LEMO_WorkOrder.cost.df$cost.sum)
median(LEMO_WorkOrder.cost.df$cost.sum)
quantile(LEMO_WorkOrder.cost.df$cost.sum)
cutoff=round(quantile(LEMO_WorkOrder.cost.df$cost.sum)[4])

LEMO_WorkOrder.cost.df$`Activity Description`[LEMO_WorkOrder.cost.df$cost.sum<cutoff]=""

ggplot(data=LEMO_WorkOrder.cost.df, aes(x=Activity, y=cost.sum, fill=Activity))+
  geom_col()+
  geom_text(size=3, aes(x=Activity, y=cost.sum, label=`Activity Description`), hjust=-0.05, angle=90, vjust=0.3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")+
  ylab("Total cost")+xlab("Activity")+ylim(0, 25000000)

LEMO_WorkOrder.cost.df=LEMO_WorkOrder.df[, .(cost.mean=mean(LEM.sum), cost.sum=sum(LEM.sum)), by=.(Activity, `Activity Description`)]

mean(LEMO_WorkOrder.cost.df$cost.mean)
median(LEMO_WorkOrder.cost.df$cost.mean)
quantile(LEMO_WorkOrder.cost.df$cost.mean)
cutoff=round(quantile(LEMO_WorkOrder.cost.df$cost.mean)[4])

LEMO_WorkOrder.cost.df$`Activity Description`[LEMO_WorkOrder.cost.df$cost.mean<cutoff]=""

ggplot(data=LEMO_WorkOrder.cost.df, aes(x=Activity, y=cost.mean, fill=Activity))+
  geom_col()+
  geom_text(size=3, aes(x=Activity, y=cost.mean, label=`Activity Description`), hjust=-0.05, angle=90, vjust=0.3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")+
  ylab("Average cost")+xlab("Activity")+ylim(0, 9000)

rm(LEMO_WorkOrder.cost.df)

#######################################################
###################actvity by avg. cost & avg. man hour
#######################################################
LEMO_WorkOrder.cost_hour.df=LEMO_WorkOrder.df[,.(hour.mean=mean(Hours.sum), cost.mean=mean(LEM.sum), freq=length(unique(`Work Order No`))),
                                              by=.(Activity, `Activity Description`)]

mean(LEMO_WorkOrder.cost_hour.df$hour.mean)
mean(LEMO_WorkOrder.cost_hour.df$cost.mean)
quantile(LEMO_WorkOrder.cost_hour.df$hour.mean)
x_cutoff=round(quantile(LEMO_WorkOrder.cost_hour.df$hour.mean)[4])
quantile(LEMO_WorkOrder.cost_hour.df$cost.mean)
y_cutoff=round(quantile(LEMO_WorkOrder.cost_hour.df$cost.mean)[4])
quantile(LEMO_WorkOrder.cost_hour.df$freq[LEMO_WorkOrder.cost_hour.df$hour.mean>x_cutoff & 
                                          LEMO_WorkOrder.cost_hour.df$cost.mean>y_cutoff])
z_cutoff=round(quantile(LEMO_WorkOrder.cost_hour.df$freq[LEMO_WorkOrder.cost_hour.df$hour.mean>x_cutoff & 
                                                           LEMO_WorkOrder.cost_hour.df$cost.mean>y_cutoff])[4])

LEMO_WorkOrder.cost_hour.df$Activity[LEMO_WorkOrder.cost_hour.df$hour.mean<x_cutoff | 
                                     LEMO_WorkOrder.cost_hour.df$cost.mean<y_cutoff |
                                     LEMO_WorkOrder.cost_hour.df$freq<z_cutoff]=""

ggplot(data=LEMO_WorkOrder.cost_hour.df, aes(x=hour.mean, y=cost.mean, size=freq, fill=Activity)) +
  geom_point(alpha=0.5, shape=21) +
  geom_hline(yintercept = y_cutoff, linetype="dashed", color="red")+
  geom_vline(xintercept = x_cutoff, linetype="dashed", color="red")+
  geom_text_repel(size=5, aes(x=hour.mean, y=cost.mean, label=Activity), hjust=-0.25, angle=90, vjust=0.3)+
  #scale_y_continuous(limits=c(1,12.5), breaks=1:12, labels=1:12)+
  scale_size(range = c(1, 20), name="Number of activities")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

rm(LEMO_WorkOrder.cost_hour.df)

#######################################################
####################################activity by closure
#######################################################

LEMO_closures.df=LEMO_WorkOrder_Closure.df[which(LEMO_WorkOrder_Closure.df$Status=="Approved" &
                                                 LEMO_WorkOrder_Closure.df$`Work Type`!="Construction" &
                                                 LEMO_WorkOrder_Closure.df$`Work Type`!="Bridge")]

LEMO_closures.num.df=LEMO_closures.df[, (closure.num=length(unique(`Work Order No`))), by=.(Activity, `Activity Description`)]
colnames(LEMO_closures.num.df)[3]="closure.num"

mean(LEMO_closures.num.df$closure.num)
median(LEMO_closures.num.df$closure.num)
quantile(LEMO_closures.num.df$closure.num)
cutoff=round(quantile(LEMO_closures.num.df$closure.num)[4])

LEMO_closures.num.df$`Activity Description`[LEMO_closures.num.df$closure.num<cutoff]=""

ggplot(data=LEMO_closures.num.df, aes(x=Activity, y=closure.num, fill=Activity))+
  geom_col()+
  geom_text(size=3, aes(x=Activity, y=closure.num, label=`Activity Description`), hjust=-0.05, angle=90, vjust=0.3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")+
  ylab("Number of closures")+xlab("Activity")+ylim(0, 1500)

LEMO_closures.num.df=LEMO_closures.df[, (closure.num=length(unique(`Work Order No`))), by=.(Activity, `Activity Description`)]
colnames(LEMO_closures.num.df)[3]="closure.num"
LEMO_WorkOrder.num.df=LEMO_WorkOrder.df[, .(act.num=length(unique(`Work Order No`))), by=.(Activity)]
LEMO_closures.num.df=merge(LEMO_closures.num.df, LEMO_WorkOrder.num.df, by="Activity", all.y=TRUE)
LEMO_closures.num.df$closure.num[is.na(LEMO_closures.num.df$closure.num)]=0
LEMO_closures.num.df=cbind.data.frame(LEMO_closures.num.df, "frac.closure"=LEMO_closures.num.df$closure.num/LEMO_closures.num.df$act.num)
fwrite(LEMO_closures.num.df, file="./bin/closure.by.act.csv", sep=",", append = FALSE)

mean(LEMO_closures.num.df$frac.closure)
median(LEMO_closures.num.df$frac.closure)
quantile(LEMO_closures.num.df$frac.closure)
cutoff=round(quantile(LEMO_closures.num.df$frac.closure)[4],2)

LEMO_closures.num.df$`Activity Description`[LEMO_closures.num.df$frac.closure<cutoff]=""

ggplot(data=LEMO_closures.num.df, aes(x=Activity, y=frac.closure, fill=Activity))+
  geom_col()+
  geom_text(size=3, aes(x=Activity, y=frac.closure, label=`Activity Description`), hjust=-0.05, angle=90, vjust=0.3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")+
  ylab("Fraction of work orders with closures")+xlab("Activity")+ylim(0, 0.5)

#######################################################
#####activity by cost, duration, closure, and frequency
#######################################################

LEMO_closures.df=LEMO_WorkOrder_Closure.df[which(LEMO_WorkOrder_Closure.df$Status=="Approved" &
                                                   LEMO_WorkOrder_Closure.df$`Work Type`!="Construction" &
                                                   LEMO_WorkOrder_Closure.df$`Work Type`!="Bridge")]
LEMO_closures.num.df=LEMO_closures.df[, (closure.num=length(unique(`Work Order No`))), by=.(Activity, `Activity Description`)]
colnames(LEMO_closures.num.df)[3]="closure.num"
LEMO_WorkOrder.num.df=LEMO_WorkOrder.df[, .(act.num=length(unique(`Work Order No`))), by=.(Activity)]
LEMO_closures.num.df=merge(LEMO_closures.num.df, LEMO_WorkOrder.num.df, by=c("Activity"), all.y=TRUE)
LEMO_closures.num.df$closure.num[is.na(LEMO_closures.num.df$closure.num)]=0
LEMO_closures.num.df=cbind.data.frame(LEMO_closures.num.df, 
                                      "frac.closure"=LEMO_closures.num.df$closure.num/LEMO_closures.num.df$act.num)

LEMO_WorkOrder.cost_hour.df=LEMO_WorkOrder.df[,.(hour.mean=mean(Hours.sum), cost.mean=mean(LEM.sum), 
                                                 freq=length(unique(`Work Order No`))),
                                              by=.(Activity, `Activity Description`)]

df=merge(LEMO_WorkOrder.cost_hour.df, LEMO_closures.num.df[,c("Activity", "frac.closure")], by="Activity", all.x = TRUE)
df=df[-c(which(df$Activity=="D40050" & df$`Activity Description`=="ILLEGAL ENCAMPMENT DEBRIS RMVL")),]

x_cutoff=round(quantile(df$hour.mean)[4])
y_cutoff=round(quantile(df$cost.mean)[4])
z_cutoff=round(quantile(df$freq)[4])
c_cutoff=round(quantile(df$frac.closure)[4], 2)

df$Activity[df$hour.mean<x_cutoff | df$cost.mean<y_cutoff | df$freq<z_cutoff | df$frac.closure<c_cutoff]=""

ggplot(data=df, aes(x=hour.mean, y=cost.mean, size=freq, fill=Activity)) +
  geom_point(alpha=0.5, shape=21) +
  geom_hline(yintercept = y_cutoff, linetype="dashed", color="red")+
  geom_vline(xintercept = x_cutoff, linetype="dashed", color="red")+
  geom_text(size=5, aes(x=hour.mean, y=cost.mean, label=Activity), hjust=-0.25, angle=90, vjust=0.3)+
  #scale_y_continuous(limits=c(1,12.5), breaks=1:12, labels=1:12)+
  scale_size(range = c(0.01, 20), name="Number of activities")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

plot_ly(df, x=df$hour.mean, y=df$cost.mean, z=df$frac.closure, size=df$freq, 
        color=paste(df$Activity, df$`Activity Description`, sep=": "),
        marker=list(symbol='circle', sizemode='diameter'), sizes=c(5,50),
        text=~paste('Activity:', Activity, '<br>Avg. duration:', hour.mean, '<br>Avg. cost:',
                    cost.mean, '<br>Fraction of work order with matching closures:', 
                    frac.closure, '<br>Frequency:', freq)) %>%
  layout(title='Activities by cost, duration, and fraction of matching closure',
         scene=list(xaxis=list(title='Avg. duration (in hours)'),
                    yaxis=list(title='Avg. cost'),
                    zaxis=list(title='Fraction of matching closures')))