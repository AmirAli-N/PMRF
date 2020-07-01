library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(ggrepel)
library(ggforce)

setwd("Z:/PMRF/Amir/bin/Final Datasets")
df=fread(file="LEMO_WorkOrder.csv", sep=",", header = TRUE)
df=na.omit(df, cols = c("Workdate", "Activity", "rID"))

###########################
## top 3 family by dist
###########################
df.temp=setDT(df)[order(Dist, Activity), .(freq=length(unique(`Work Order No`))), by=.(Dist, Activity)]
df.temp=cbind.data.frame(df.temp, family=substring(df.temp$Activity, 1, 1))
df.group=setDT(df.temp)[order(Dist, family), .(freq=sum(freq)), by=.(Dist, family)]
df.group=df.group %>% group_by(Dist) %>% mutate(pct=freq/sum(freq))
df.group=na.omit(df.group, cols="Dist")
df.group=df.group[df.group$Dist!=56,]
df.group$Dist=as.character(df.group$Dist)
df.group$Dist=factor(df.group$Dist, levels = c("1", "2", "3", "4", "5", "6", 
                                               "7", "8", "9", "10", "11", "12╨"))
df.label=df.group %>% group_by(Dist) %>% top_n(n=3, wt=freq)
df.label=setDT(df.label)[order(Dist, freq),,]
df.label=df.label %>% group_by(Dist) %>% mutate(order=order(freq, decreasing = TRUE))


ggplot(df.group, aes(x=family, y=Dist, size=freq, fill=Dist))+
  geom_point(alpha=0.5, shape=21)+
  scale_size(range = c(1, 20))+
  scale_y_discrete(breaks=seq(1, 12, 1), labels=seq(1, 12, 1), expand = c(0, 0.5))+
  geom_text(size=5, data=df.label, aes(x=family, y=Dist, label=order), family="Century Gothic")+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, family = "Century Gothic"),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 18, family = "Century Gothic"),
        axis.text.y = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic"))+
  xlab("IMMS family grouping")+
  ylab("District")
rm(df.temp, df.group, df.label)
############################
## activity by top 3 family in dist 4
############################
df.temp=df[Dist==4,]  
df.temp=setDT(df.temp)[order(Activity), .(freq=length(unique(`Work Order No`))), by=.(Activity)]  
df.temp=cbind.data.frame(df.temp, family=substring(df.temp$Activity, 1, 1))
df.temp=df.temp[which(df.temp$family %in% c("D", "C", "M")),]

ggplot(df.temp, aes(x=Activity, y=freq, fill=Activity))+
  geom_bar(stat='identity')+
  #scale_y_discrete(breaks=seq(1, 12, 1), labels=seq(1, 12, 1), expand = c(0, 0.5))+
  #geom_text(size=5, data=df.label, aes(x=family, y=Dist, label=order), family="Century Gothic")+
  theme_ipsum(axis_title_just = 'center')+
  theme(legend.position = 'none',
        axis.text.x = element_text(angle= 90, size = 18, family = "Century Gothic", 
                                   vjust = 0.5, hjust = 1),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 18, family = "Century Gothic"),
        axis.text.y = element_text(size = 18, family = "Century Gothic"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, family = "Century Gothic"))+
  xlab("IMMS activity codes")+
  ylab("Number of work orders")
rm(df.temp)
############################
## most freq, costly, lengthy activities
############################
df.temp=df[,.(hour.mean=mean(Hours.sum, na.rm = TRUE), 
              cost.mean=mean(LEM.sum, na.rm = TRUE), 
              freq=length(unique(`Work Order No`))),
           by=.(Activity, `Activity Description`)]

mean(df.temp$hour.mean, na.rm = TRUE)
mean(df.temp$cost.mean, na.rm = TRUE)
quantile(df.temp$hour.mean, na.rm = TRUE)
x_cutoff=round(quantile(df.temp$hour.mean, na.rm = TRUE)[4])
quantile(df.temp$cost.mean, na.rm = TRUE)
y_cutoff=round(quantile(df.temp$cost.mean, na.rm = TRUE)[4])
quantile(df.temp$freq[df.temp$hour.mean > x_cutoff & 
                      df.temp$cost.mean > y_cutoff], 
         na.rm = TRUE)
z_cutoff=round(quantile(df.temp$freq[df.temp$hour.mean > x_cutoff & 
                                     df.temp$cost.mean > y_cutoff],
                        na.rm = TRUE)[4])

#error correction
df.temp=df.temp[-which(df.temp$Activity=="E11040" & 
                       df.temp$`Activity Description`=="ALL OTHER WEED CONTROL RDSD"),]
df.temp=df.temp[-which(df.temp$Activity=="C24040" & 
                       df.temp$`Activity Description`=="ALL OTHER CONTROL LANDSCAPE"),]

df.temp$Activity[df.temp$hour.mean < x_cutoff | 
                 df.temp$cost.mean < y_cutoff |
                 df.temp$freq < z_cutoff]=""

p=ggplot(data=df.temp, 
       aes(x=hour.mean, y=cost.mean, size=freq, fill=Activity)) +
  geom_point(alpha=0.5, shape=21) +
  geom_hline(yintercept = y_cutoff, linetype="dashed", color="red", size=1.5)+
  geom_vline(xintercept = x_cutoff, linetype="dashed", color="red", size=1.5)+
  scale_size(range = c(1, 20), name="Number of activities")+
  theme_ipsum(axis_title_just = 'center')+
  xlab("Average person-hour")+
  ylab("Average cost ($)")+
  facet_zoom(ylim = c(1000, 2250), xlim = c(25, 37.5),  shrink = TRUE, 
             horizontal = FALSE, zoom.size = 0.75)+
  geom_text_repel(size=5, aes(x=hour.mean, y=cost.mean, label=Activity), 
                  hjust=0, vjust=0, family="Century Gothic")+
  theme(axis.text.x = element_text(size=18, family = "Century Gothic", color = "black"),
        axis.title.x = element_text(size=18, family = "Century Gothic", 
                                    color = "black", margin = margin(15, 0, 0, 0)),
        axis.text.y = element_text(size=18, family = "Century Gothic", color = "black"),
        axis.title.y = element_text(size = 18, family = "Century Gothic", 
                                    color = "black", margin = margin(0, 15, 0, 0)),
        legend.position = "none",
        strip.background = element_rect(fill = alpha("grey", 0.5), linetype = 2,
                                        colour="black"))

pb=ggplot_build(p)
pb$data[[4]]$alpha[which((pb$data[[4]]$label=="M10010" | 
                          pb$data[[4]]$label=="A30010"))]=0
pb$data[[4]]$alpha[c(9, 29)]=NA
pb$data[[4]]$alpha[c(1:8, 10:28, 30:100)]=0
pg=ggplot_gtable(pb)
plot(pg)

rm(df.temp, p, pb, pg)
