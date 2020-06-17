library(networkD3)
library(data.table)
library(ggplot2)
library(htmlwidgets)
library(stringr)
library(ggalluvial)
library(forcats)
library(hrbrthemes)
library(viridis)

library(extrafont)
font_import()
loadfonts(device = "win")

setwd("G:/My Drive/WorkingDesk/CalTrans-60% report")

df=fread(file="group_table_act_descr.example.csv", sep=",", header = FALSE)
colnames(df)=c("ProposalGroups", "TableName", "ActivityCode", "FamilyName", "Description")
df$ProposalGroups=factor(df$ProposalGroups, levels = c("Litter, debris, and graffiti removal", "Road sweeping",
                                                       "Sign installation and repair", "Hazardous spill cleaning"))
df$TableName=factor(df$TableName, levels = c("Litter and Debris Activities", "Graffiti Activities", 
                                             "Sweeping Roadway Activities", 
                                             "Carcass Pickup, Inspection & Investigation Activities", 
                                             "Illegal Sign Removal Activities", "Spills Activities", 
                                             "Hazmat Storage and Disposal"))

df=setDT(df)[order(ActivityCode), .(Freq=length(ActivityCode)), by=.(ProposalGroups, FamilyName, TableName, ActivityCode)]

ggplot(df, aes(y = Freq, axis1 = forcats::fct_relabel(ProposalGroups, str_wrap, width=15) , 
               axis2 = FamilyName, axis3=forcats::fct_relabel(TableName, str_wrap, width=15), 
               axis4=ActivityCode)) +
  geom_alluvium(aes(fill=ProposalGroups), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", infer.label = TRUE, family="Century Gothic") +
  scale_x_discrete(limits = c("ProposalGroups", "FamilyName", "TableName", "ActivityCode"), 
                   labels=c("AHMCT maintenance function", "IMMS Family", "IMMS Table Name", "Activity Code"), 
                   expand = c(.05, .05)) +
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(grid = FALSE)+
  #scale_fill_brewer(type = "qual", palette = "Set1")+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(family = "Century Gothic"))
