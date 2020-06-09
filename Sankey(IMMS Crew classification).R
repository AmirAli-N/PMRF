library(networkD3)
library(data.table)
setwd("C:/Users/anasr/Desktop")

df=fread(file="Book1.csv", sep=",", header = FALSE)
colnames(df)=c("ProposalGroups", "TableName", "ActivityCode","Description")

nodes=cbind.data.frame(1:(length(unique(df$ProposalGroups))+length(unique(df$TableName))), 
                       c(unique(df$ProposalGroups), unique(df$TableName)))
colnames(nodes)=c("num", "name")

links=as.data.frame(matrix(NA, nrow = 0, ncol=3))
colnames(links)=c("source","target","value")

prop.group=unique(df$ProposalGroups)
table.name=unique(df$TableName)
activity=unique(df$ActivityCode)

for (i in prop.group){
  num_link1=0
  for (j in table.name){
    num_link1=length(which(df$TableName==j & df$ProposalGroups==i))*10
    if (num_link1>0){
      links=rbind.data.frame(links, cbind.data.frame("source"=which(prop.group==i)-1, 
                                                     "target"=which(table.name==j)+length(prop.group)-1, 
                                                     "value"=num_link1))
    }
  }
}
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                   Target = "target", Value = "value", NodeID = "name",
                   units = "TWh", fontSize = 14, nodeWidth = 10)

p

df=fread(file="Book2.csv", sep=",", header = FALSE)
colnames(df)=c("TableName", "Crew")

nodes=cbind.data.frame(1:(length(unique(df$TableName))+length(unique(df$Crew))), 
                       c(unique(df$TableName), unique(df$Crew)))
colnames(nodes)=c("num", "name")
links=as.data.frame(matrix(NA, nrow = 0, ncol=3))
colnames(links)=c("source","target","value")

table.name=unique(df$TableName)
crew.name=unique(df$Crew)

for (i in table.name){
  num_link1=0
  for(j in crew.name){
    num_link1=length(which(df$TableName==i & df$Crew==j))*10
    if(num_link1>0){
      links=rbind.data.frame(links, cbind.data.frame("source"=which(table.name==i)-1,
                                                     "target"=which(crew.name==j)+length(table.name)-1,
                                                     "value"=num_link1))
    }
  }
}

p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                   Target = "target", Value = "value", NodeID = "name",
                   units = "TWh", fontSize = 14, nodeWidth = 10)

p
