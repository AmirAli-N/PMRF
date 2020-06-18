library(networkD3)
library(data.table)
library(ggplot2)
library(htmlwidgets)
library(stringr)
library(ggalluvial)
library(forcats)
library(hrbrthemes)
library(viridis)
library(ggrepel)

library(extrafont)
font_import()
loadfonts(device = "win")

setwd("G:/My Drive/WorkingDesk/CalTrans-60% report")

df=fread(file="proposal_family_table_act_descr.example_many.to.one.csv", sep=",", header = FALSE)
#df=fread(file="proposal_family_table_act_descr.example_one.to.many.csv", sep=",", header = FALSE)
colnames(df)=c("ProposalGroups", "FamilyName", "TableName", "ActivityCode", "Description")

#df$ProposalGroups=factor(df$ProposalGroups, levels = c("Litter, debris, and graffiti removal", 
#                                                       "Road sweeping", "Sign installation and repair", 
#                                                      "Hazardous spill cleaning"))
#df$TableName=factor(df$TableName, levels = c("Litter and Debris Activities", "Graffiti Activities", 
#                                             "Sweeping Roadway Activities", 
#                                            "Carcass Pickup, Inspection & Investigation Activities", 
#                                             "Illegal Sign Removal Activities", "Spills Activities", 
#                                             "Hazmat Storage and Disposal"))

df=setDT(df)[order(ActivityCode), .(Freq=length(ActivityCode)), 
             by=.(ProposalGroups, FamilyName, TableName, ActivityCode)]

ggplot(df, aes(y = Freq, axis1 = forcats::fct_relabel(ProposalGroups, str_wrap, width=25) , 
               axis2 = FamilyName, axis3 = forcats::fct_relabel(TableName, str_wrap, width=30), 
               axis4=ActivityCode)) +
#  geom_alluvium(aes(fill=ProposalGroups), width = 1/12) +
  geom_alluvium(aes(fill=ProposalGroups), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", infer.label = TRUE, family="Century Gothic", size=8) +
  scale_x_discrete(limits = c("ProposalGroups", "FamilyName", "TableName", "ActivityCode"), 
                   labels=c("AHMCT classification", "IMMS family grouping",
                            "IMMS table name", "IMMS activity code"), 
                   expand = c(0.15, 0.05)) +
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(grid = FALSE)+
  #scale_fill_brewer(type = "qual", palette = "Set1")+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(family = "Century Gothic", size=24, color="black"))