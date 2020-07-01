library(data.table)
library(ggplot2)
library(viridis)
library(hrbrthemes)

setwd("Z:/PMRF/Amir/bin/Final Datasets")
df=fread(file = "CHP_density.csv", sep = ",", header = TRUE)

df.example=df[which(df$route==1 & df$county=="LA"),]
ggplot(df.example, aes(x=bin_start, y=freq))+
  scale_x_binned(breaks = c(df.example$bin_start, df.example$bin_end[31]), 
                 labels = c(df.example$bin_start, df.example$bin_end[31]), 
                 trans = 'identity', limits = c(26.41, 88.41), right = FALSE)+
  geom_bar(stat = 'identity', position=position_identity(), 
           fill="blue", color="blue")+
  theme_ipsum(axis_title_just = 'center')+
  theme(axis.text.x = element_text(size = 18, family = "Century Gothic", 
                                   angle  = 45, color = "black", hjust =1, vjust=1),
        axis.title.x = element_text(margin=margin(15, 0, 0, 0), size = 18, 
                                    family = "Century Gothic", color = "black"),
        axis.text.y = element_text(size = 18, family = "Century Gothic", color = "black"),
        axis.title.y = element_text(margin=margin(0, 15, 0, 0), size = 18, 
                                    family = "Century Gothic", color = "black"))+
  xlab("SR 1 segments")+
  ylab("Number of collisions")
  
