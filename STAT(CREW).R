library(data.table)
library(dplyr)
library(tidyr)
library(basicTrendline)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(forcats)
library(stringr)
library(ggpubr)

setwd("Z:/PMRF/Amir")
df=fread(file = "activity+crew.by.district.csv", sep = ",", header = TRUE)
df=df[!df$FY %in% c(2011, 2012),]

df.family=setDT(df)[order(FY, FAMILY), .(sum.employee=sum(EMPCOUNT)), 
                    by=.(FY, FAMILY)]
df.family=pivot_wider(df.family, id_cols = FAMILY, names_from = FY, values_from = sum.employee)
df.activity=setDT(df)[order(FY, ACTCODE), .(sum.employee=sum(EMPCOUNT)), 
                      by=.(FY, ACTCODE)]
df.activity_pivot=pivot_wider(df.activity, id_cols = ACTCODE, names_from = FY, values_from = sum.employee)
df.activity_pivot=cbind.data.frame(df.activity_pivot, 
                                   change=df.activity_pivot$`2019`-df.activity_pivot$`2013`)
x=seq(2013, 2019, 1)
slope=c()
for (i in 1:dim(df.activity_pivot)[1]){
  y=unlist(df.activity_pivot[i, !names(df.activity_pivot) %in% ("ACTCODE")], use.names = FALSE)
  if (length(which(is.na(y)))>3){
    slope=c(slope, NA)
    next
  }
  trend.model=trendline_summary(x, y, model = "line2P", summary = FALSE, eDigit = 2)
  slope=c(slope, trend.model$parameter$a)
}
df.activity_pivot=cbind.data.frame(df.activity_pivot, slope=slope)
fwrite(df.activity_pivot, file = "activity_crew+netChange_slope.csv", sep = ",", append = FALSE)

df.activity_pivot=setDT(df.activity_pivot)[order(change),,]
df.activity_pivot=setDT(df.activity_pivot)[order(slope),,]

example.code=c("C20040", "C21040", "C22040", "C23040", "C24040",
               "C30020", "C30040", "C31040", "C32040")
df.example=df.activity[df.activity$ACTCODE %in% example.code, ]
df.example$ACTCODE[df.example$ACTCODE=="C20040"]="C20040: Mechanical Control"
df.example$ACTCODE[df.example$ACTCODE=="C21040"]="C21040: Chemical Control"
df.example$ACTCODE[df.example$ACTCODE=="C22040"]="C22040: Manual Control"
df.example$ACTCODE[df.example$ACTCODE=="C23040"]="C23040: Rodent Control"
df.example$ACTCODE[df.example$ACTCODE=="C24040"]="C24040: All Other Weed Control"
df.example$ACTCODE[df.example$ACTCODE=="C30020"]="C30020: Tree Insepction"
df.example$ACTCODE[df.example$ACTCODE=="C30040"]="C30040: Tree Trimming"
df.example$ACTCODE[df.example$ACTCODE=="C31040"]="C31040: Remove Tree"
df.example$ACTCODE[df.example$ACTCODE=="C32040"]="C32040: Brush Control"

ggplot(df.example, 
       aes(x=FY, y=sum.employee, color=ACTCODE))+
  geom_point()+
  scale_fill_viridis(discrete=FALSE, option="C") +
  theme_ipsum(axis_title_just = 'center')+
  geom_smooth(method=lm)+
  scale_x_continuous(limits = c(2013, 2019), 
                     breaks = seq(2013, 2019, 1), 
                     labels = as.character(seq(2013, 2019, 1)))+
  scale_y_continuous(limits = c(0, 2750),
                     breaks = seq(0, 2750, 500),
                     labels = as.character(seq(0, 2750, 500)))+
  stat_regline_equation(label.x = 2013, label.y = 2500, 
                        geom = "text", size=7, family="Century Gothic")+
  stat_cor(label.x = 2013, label.y = 2100, 
           geom = "text", size=7, family="Century Gothic")+
  theme(legend.position = "none",
        axis.text.x = element_text(family = "Century Gothic", color = "black", size = 18),
        axis.text.y = element_text(family = "Century Gothic", color = "black", size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r=20, b=0, l=0),
                                    family = "Century Gothic", color= "black", size = 24))+
  ylab("Number of maintenance employees")+
  facet_wrap(~ACTCODE, ncol=3, scales = "free")+
  theme(strip.text = element_text(size=20, family = "Century Gothic", color="black"),
        axis.line.x.bottom = element_line(color = "black", size = 1),
        axis.line.y.left = element_line(color = "black", size = 1))