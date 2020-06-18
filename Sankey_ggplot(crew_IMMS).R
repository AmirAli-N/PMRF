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

df=fread(file="crew_proposal_family_table_act_descr.example.csv", sep=",", header = FALSE)
colnames(df)=c("Crew", "ProposalGroups", "FamilyName", "TableName", "ActivityCode", "Description")

df=setDT(df)[order(ActivityCode), .(Freq=length(ActivityCode)), 
             by=.(Crew, ProposalGroups, FamilyName, TableName, ActivityCode)]

ggplot(df, aes(y = Freq, 
               axis1 = forcats::fct_relabel(Crew, str_wrap, width=15),
               axis2 = forcats::fct_relabel(ProposalGroups, str_wrap, width=25) , 
               axis3 = FamilyName, 
               axis4 = forcats::fct_relabel(TableName, str_wrap, width=25), 
               axis5 = ActivityCode)) +
  #  geom_alluvium(aes(fill=ProposalGroups), width = 1/12) +
  geom_alluvium(aes(fill=ActivityCode), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", infer.label = TRUE, family="Century Gothic", size=8) +
  scale_x_discrete(limits = c("Crew", "ProposalGroups", "FamilyName", "TableName", "ActivityCode"), 
                   labels=c("Responsbile\n crew team", "AHMCT\n classification", "IMMS\n family grouping",
                            "IMMS\n table name", "IMMS\n activity code"), 
                   expand = c(0.15, 0.05)) +
  scale_fill_viridis(discrete=TRUE, option="C") +
  theme_ipsum(grid = FALSE)+
  #scale_fill_brewer(type = "qual", palette = "Set1")+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(family = "Century Gothic", size=24, color="black"))
