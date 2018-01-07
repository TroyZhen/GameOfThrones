library(data.table)
setwd('C:/Users/qw004/Desktop/ISOM 673/final project/OneDrive_2017-12-04/asoiaf-master/data')
nodes <- fread("asoiaf-all-nodes.csv")
nodes[,BookofDeath:=NULL]
setkey(nodes,Id)

AllegiancesNumber <- as.data.table(table(nodes$Allegiances))
AllegiancesNumber[order(-N)]

as.data.table(table(nodes$Gender))
as.data.table(table(nodes$Nobility))
edges <- fread("asoiaf-all-edges.csv")
edges

library(igraph)
el <- as.data.frame(edges)
graph <- graph.data.frame(el, directed = F)
vcol  = rep("white", vcount(graph))
vcol[V(graph)$name=="Jon-Snow"] = "red"
vcol[V(graph)$name=="Tyrion-Lannister"] = "red"
plot(graph,vertex.color=vcol,vertex.label.color='red',vertex.label = ifelse(V(graph)$name=="Jon-Snow", V(graph)$name, NA),vertex.size=3)


degrees <- as.data.table(degree(graph),keep.rownames = T)
degrees[order(-V2)][1:20]

btws <- as.data.table(betweenness(graph),keep.rownames = T)
btws[order(-V2)][1:20]

cls <- as.data.table(closeness(graph),keep.rownames = T)
cls[order(-V2)][1:20]

evc <- as.data.table(evcent(graph, weights = E(graph)$weight)$vector,keep.rownames = T)
evc[order(-V2)][1:20]

nodes <- nodes[degrees]
setnames(nodes,"V2","degree")

setkey(nodes,Id)
nodes <- nodes[cls]
setnames(nodes,"V2","closeness")

setkey(nodes,Id)
nodes <- nodes[btws]
setnames(nodes,"V2","betweenness")

setkey(nodes,Id)
nodes <- nodes[evc]
setnames(nodes,"V2","evc")

nodes[,.(N = .N,
         M_degree = mean(degree),
         M_closeness = mean(closeness),
         M_btwns = mean(betweenness),
         M_evc = mean(evc)), by = Allegiances]

nodes[,.(N = .N,
         M_degree = mean(degree),
         M_closeness = mean(closeness),
         M_btwns = mean(betweenness),
         M_evc = mean(evc)), by = Gender]

nodes[,.(N = .N,
         M_degree = mean(degree),
         M_closeness = mean(closeness),
         M_btwns = mean(betweenness),
         M_evc = mean(evc)), by = Nobility]

nodes[,.(M_degree = mean(degree),
         M_closeness = mean(closeness),
         M_btwns = mean(betweenness),
         M_evc = mean(evc)), by = Death]

mydata <- as.data.frame(nodes[,3:10])
names(mydata)
mydata$degree <- scale(mydata$degree)
mydata$closeness <- scale(mydata$closeness)
mydata$betweenness <- scale(mydata$betweenness)
mydata$evc <- scale(mydata$evc)

lr_death <- glm(Death~.,family = "binomial", data = mydata)
summary(lr_death)

newdata <- nodes[Death==0,]
newdata[,Death:=NULL]
pdt_data <- as.data.frame(newdata)
pdt_data$degree <- scale(pdt_data$degree)
pdt_data$closeness <- scale(pdt_data$closeness)
pdt_data$betweenness <- scale(pdt_data$betweenness)
pdt_data$evc <- scale(pdt_data$evc)

p_death <- predict(lr_death, newdata = pdt_data,type = "response")
outcome <- cbind(pdt_data,p_death)
outcome <- as.data.table(outcome)
outcome[1:20,][order(-p_death)]
nodes

# Book1
nodes1 <- fread("asoiaf-book1-nodes.csv")
nodes1[,Label:=NULL]
nodes1[,book1:=1]
setkey(nodes1,Id)
nodes1
setkey(nodes,Id)
nodesAll <- nodes1[nodes, nomatch =NA]
nodesAll[is.na(book1),book1:=0]
nodes1 <- nodes[nodes1]

edges1 <- fread("asoiaf-book1-edges.csv")
edges1

el1 <- as.data.frame(edges1)
graph1 <- graph.data.frame(el1, directed = F)
plot(graph1)

degrees1 <- as.data.table(degree(graph1),keep.rownames = T)
degrees1[order(-V2)][1:20]

btws1 <- as.data.table(betweenness(graph1),keep.rownames = T)
btws1[order(-V2)][1:20]

cls1 <- as.data.table(closeness(graph1),keep.rownames = T)
cls1[order(-V2)][1:20]

evc1 <- as.data.table(evcent(graph1, weights = E(graph1)$weight)$vector,keep.rownames = T)
evc1[order(-V2)][1:20]

nodes1[,book1:=NULL]
nodes1[,degree:=NULL]
nodes1[,closeness:=NULL]
nodes1[,betweenness:=NULL]
nodes1[,evc:=NULL]
nodes1 <- nodes1[degrees1]
setnames(nodes1,"V2","degree")

setkey(nodes1,Id)
nodes1 <- nodes1[cls1]
setnames(nodes1,"V2","closeness")

setkey(nodes1,Id)
nodes1 <- nodes1[btws1]
setnames(nodes1,"V2","betweenness")

setkey(nodes1,Id)
nodes1 <- nodes1[evc1]
setnames(nodes1,"V2","evc")

dim(nodes1)

mydata1 <- as.data.frame(nodes1[,3:ncol(nodes1)])
names(mydata1)
mydata1$degree <- scale(mydata1$degree)
mydata1$closeness <- scale(mydata1$closeness)
mydata1$betweenness <- scale(mydata1$betweenness)
mydata1$evc <- scale(mydata1$evc)

lr_death1 <- glm(Death~.,family = "binomial", data = mydata1)
summary(lr_death1)

# Book2
nodes2 <- fread("asoiaf-book2-nodes.csv")
nodes2[,Label:=NULL]
nodes2[,book2:=1]
setkey(nodes2,Id)
nodes2
setkey(nodesAll,Id)
nodesAll <- nodes2[nodesAll, nomatch =NA]
nodesAll[is.na(book2),book2:=0]

nodes2 <- nodesAll[nodes2]

edges2 <- fread("asoiaf-book2-edges.csv")
edges2

el2 <- as.data.frame(edges2)
graph2 <- graph.data.frame(el2, directed = F)
plot(graph2)

degrees2 <- as.data.table(degree(graph2),keep.rownames = T)
degrees2[order(-V2)][2:20]

btws2 <- as.data.table(betweenness(graph2),keep.rownames = T)
btws2[order(-V2)][2:20]

cls2 <- as.data.table(closeness(graph2),keep.rownames = T)
cls2[order(-V2)][2:20]

evc2 <- as.data.table(evcent(graph2, weights = E(graph2)$weight)$vector,keep.rownames = T)
evc2[order(-V2)][2:20]

nodes2[,book2:=NULL]
nodes2[,i.book2:=NULL]
nodes2[,degree:=NULL]
nodes2[,closeness:=NULL]
nodes2[,betweenness:=NULL]
nodes2[,evc:=NULL]
nodes2 <- nodes2[degrees2]
setnames(nodes2,"V2","degree")

setkey(nodes2,Id)
nodes2 <- nodes2[cls2]
setnames(nodes2,"V2","closeness")

setkey(nodes2,Id)
nodes2 <- nodes2[btws2]
setnames(nodes2,"V2","betweenness")

setkey(nodes2,Id)
nodes2 <- nodes2[evc2]
setnames(nodes2,"V2","evc")

nodes2

dim(nodes2)

mydata2 <- as.data.frame(nodes2[,2:ncol(nodes2)])
names(mydata2)
mydata2$Label <- NULL
mydata2$degree <- scale(mydata2$degree)
mydata2$closeness <- scale(mydata2$closeness)
mydata2$betweenness <- scale(mydata2$betweenness)
mydata2$evc <- scale(mydata2$evc)

lr_death2 <- glm(Death~.,family = "binomial", data = mydata2)
summary(lr_death2)

# Book3
nodes3 <- fread("asoiaf-book3-nodes.csv")
nodes3[,Label:=NULL]
nodes3[,book3:=1]
setkey(nodes3,Id)
nodes3
setkey(nodesAll,Id)
nodesAll <- nodes3[nodesAll, nomatch =NA]
nodesAll[is.na(book3),book3:=0]

nodes3 <- nodesAll[nodes3]

edges3 <- fread("asoiaf-book3-edges.csv")
edges3

el3 <- as.data.frame(edges3)
graph3 <- graph.data.frame(el3, directed = F)
plot(graph3)

degrees3 <- as.data.table(degree(graph3),keep.rownames = T)

btws3 <- as.data.table(betweenness(graph3),keep.rownames = T)

cls3 <- as.data.table(closeness(graph3),keep.rownames = T)

evc3 <- as.data.table(evcent(graph3, weights = E(graph3)$weight)$vector,keep.rownames = T)

nodes3[,book3:=NULL]
nodes3[,i.book3:=NULL]
nodes3[,degree:=NULL]
nodes3[,closeness:=NULL]
nodes3[,betweenness:=NULL]
nodes3[,evc:=NULL]
nodes3 <- nodes3[degrees3]
setnames(nodes3,"V2","degree")

setkey(nodes3,Id)
nodes3 <- nodes3[cls3]
setnames(nodes3,"V2","closeness")

setkey(nodes3,Id)
nodes3 <- nodes3[btws3]
setnames(nodes3,"V2","betweenness")

setkey(nodes3,Id)
nodes3 <- nodes3[evc3]
setnames(nodes3,"V2","evc")

nodes3
dim(nodes3)

mydata3 <- as.data.frame(nodes3)
names(mydata3)
mydata3$Label <- NULL
mydata3$Id <- NULL
mydata3$degree <- scale(mydata3$degree)
mydata3$closeness <- scale(mydata3$closeness)
mydata3$betweenness <- scale(mydata3$betweenness)
mydata3$evc <- scale(mydata3$evc)

lr_death3 <- glm(Death~.,family = "binomial", data = mydata3)
summary(lr_death3)

# Book45
nodes45 <- fread("asoiaf-book45-nodes.csv")
nodes45[,Label:=NULL]
nodes45[,book45:=1]
setkey(nodes45,Id)
nodes45
setkey(nodesAll,Id)
nodesAll <- nodes45[nodesAll, nomatch =NA]
nodesAll[is.na(book45),book45:=0]

nodes45 <- nodesAll[nodes45]

edges45 <- fread("asoiaf-book45-edges.csv")
edges45

el45 <- as.data.frame(edges45)
graph45 <- graph.data.frame(el45, directed = F)
plot(graph45)

degrees45 <- as.data.table(degree(graph45),keep.rownames = T)

btws45 <- as.data.table(betweenness(graph45),keep.rownames = T)

cls45 <- as.data.table(closeness(graph45),keep.rownames = T)

evc45 <- as.data.table(evcent(graph45, weights = E(graph45)$weight)$vector,keep.rownames = T)

nodes45[,book45:=NULL]
nodes45[,i.book45:=NULL]
nodes45[,degree:=NULL]
nodes45[,closeness:=NULL]
nodes45[,betweenness:=NULL]
nodes45[,evc:=NULL]
nodes45 <- nodes45[degrees45]
setnames(nodes45,"V2","degree")

setkey(nodes45,Id)
nodes45 <- nodes45[cls45]
setnames(nodes45,"V2","closeness")

setkey(nodes45,Id)
nodes45 <- nodes45[btws45]
setnames(nodes45,"V2","betweenness")

setkey(nodes45,Id)
nodes45 <- nodes45[evc45]
setnames(nodes45,"V2","evc")

nodes45
dim(nodes45)

mydata45 <- as.data.frame(nodes45)
names(mydata45)
mydata45$Label <- NULL
mydata45$Id <- NULL
mydata45$i.book3 <- NULL
mydata45$degree <- scale(mydata45$degree)
mydata45$closeness <- scale(mydata45$closeness)
mydata45$betweenness <- scale(mydata45$betweenness)
mydata45$evc <- scale(mydata45$evc)

lr_death45 <- glm(Death~.,family = "binomial", data = mydata45)
summary(lr_death45)

