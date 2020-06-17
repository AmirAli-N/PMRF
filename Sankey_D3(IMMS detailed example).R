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

nodes=cbind.data.frame(1:(length(unique(df$ProposalGroups))+length(unique(df$FamilyName))+
                            length(unique(df$TableName))+length(unique(df$ActivityCode))), 
                       c(unique(df$ProposalGroups), unique(df$FamilyName), unique(df$TableName),
                         unique(df$ActivityCode)))

colnames(nodes)=c("num", "name")
nodes$name=str_wrap(nodes$name, width = 15)

links=as.data.frame(matrix(NA, nrow = 0, ncol=3))
colnames(links)=c("source","target","value")

prop.group=unique(df$ProposalGroups)
family.name=unique(df$FamilyName)
table.name=unique(df$TableName)
activity=unique(df$ActivityCode)
descr=unique(df$Description)

for (i in prop.group){
  num_link1=0
  for (j in family.name){
    num_link1=length(which(df$FamilyName==j & df$ProposalGroups==i))*10
    if (num_link1>0){
      links=rbind.data.frame(links, cbind.data.frame("source"=which(prop.group==i)-1, 
                                                     "target"=which(family.name==j)+length(prop.group)-1, 
                                                     "value"=num_link1))
    }
  }
}
for (j in family.name){
  num_link2=0
  for (k in table.name){
    num_link2=length(which(df$TableName==k & df$FamilyName==j))*10
    if (num_link2>0){
      links=rbind.data.frame(links, cbind.data.frame("source"=which(family.name==j)+length(prop.group)-1, 
                                                     "target"=which(table.name==k)+length(prop.group)+
                                                       length(family.name)-1, 
                                                     "value"=num_link2))
    }
  }
}
for (k in table.name){
  num_link3=0
  for (t in activity){
    num_link3=length(which(df$ActivityCode==t & df$TableName==k))*10
    if (num_link3>0){
      links=rbind.data.frame(links, cbind.data.frame("source"=which(table.name==k)+length(prop.group)+
                                                       length(family.name)-1, 
                                                     "target"=which(activity==t)+length(prop.group)+
                                                       length(family.name)+length(table.name)-1, 
                                                     "value"=num_link3))
    }
  }
}

p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                   Target = "target", Value = "value", NodeID = "name",
                   units = "TWh", fontSize = 16, fontFamily = "century gothic", nodeWidth = 10)
p