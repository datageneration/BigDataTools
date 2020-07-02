## Sample program for Big Data Tool workshop
## NCHU Sustainability and Green Growth, July 2, 2020

install.packages(c("readr","network","ggplot2", "utf8","dplyr","BBmisc","igraph"))
library(readr)
library(network)
library(ggplot2)
library(utf8)
library(ggplot2)
library(dplyr)
library(BBmisc)
library(igraph)
links <- read_csv("https://raw.githubusercontent.com/kho7/tw_sna/master/data/180825_leg_edges.csv")
View(links)
nodes <- read_csv("https://raw.githubusercontent.com/kho7/tw_sna/master/data/180825_leg_nodes.csv")
View(nodes)

## Examine Data ##
head(nodes)
head(links)


## Data management
nodes$ltsb=log(nodes$totsbill+1)





nodes$ltsb

nrow(nodes); length(unique(nodes$id))

nrow(links); nrow(unique(links[,c("from", "to")]))

nodes$KMT
sum(nodes$KMT)
# 63 KMT


nodes$DPP
sum(nodes$DPP)
# 40 DPP


nodes$PFP
sum(nodes$PFP)
# 3 PFP

nodes$TSU
sum(nodes$TSU)
# 3 TSU
# What are the other 3?

nodes$male
sum(nodes$male)
# 70 males

nodes$mainlander
sum(nodes$mainlander)
# Missing data

nodes$SMD
sum(nodes$SMD)
# 79 SMD

nodes$incumbent
sum(nodes$incumbent)
# 47 incumbents

## Turning Network into igraph object ##

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
class(net)
net

## Intial Network Attributes ##
E(net)       # The edges of the "net" object
E(net)$date  # Edge attribute "date"
E(net)$term  # Edge attribute "term"
E(net)$sessionPeriod  # Edge attribute "sessionPeriod"
E(net)$sessionTimes  # Edge attribute "sessionTimes"
E(net)$billName  # Edge attribute "billName"

V(net)       # The vertices of the "net" object
V(net)$KMT   # Vertex/Node attribute "KMT"
V(net)$DPP   # Vertex/Node attribute "DPP"
V(net)$PFP # Vertex/Node attribute "PFP"
V(net)$TSU # Vertex/Node attribute "TSU"
V(net)$male # Vertex/Node attribute "male"
V(net)$mainlander # Vertex/Node attribute "mainlander"
V(net)$SMD # Vertex/Node attribute "SMD"
V(net)$birthyear # Vertex/Node attribute "birthyear"
V(net)$incumbent # Vertex/Node attribute "incumbent"
V(net)$Party # Vertex/Node attribute "Party"




net <- simplify(net, remove.multiple = T, remove.loops = T) 
# Remove multiple ties between two legislators
# svg(filename="plot01.svg")
plot(net, edge.arrow.size=.001,vertex.label=NA,vertex.size=nodes$ltsb)


# Labeling the nodes by party
# KMT members = blue
# DPP = forestgreen
# PFP = orange
# TSU = darkolivegreen
# Other = purple
V(net)$color=V(net)$Party
V(net)$color
V(net)$color=gsub("1", "blue", V(net)$color)
V(net)$color=gsub("2", "forestgreen", V(net)$color)
V(net)$color=gsub("3", "orange", V(net)$color)
V(net)$color=gsub("4", "darkolivegreen", V(net)$color)
V(net)$color=gsub("5", "purple", V(net)$color)


plot(net, vertex.color=V(net)$color, edge.arrow.size=.1,vertex.label=NA,edge.width=0.1, vertex.size=nodes$ltsb)
dev.off()

# Fruchterman Reingold Layout #
layFR <- layout.fruchterman.reingold(net)
layout=1


plot(net, vertex.color=V(net)$color, vertex.label=nodes$ide, 
     edge.arrow.size=.05, edge.color="gray70", 
     vertex.label.dist=0.75,vertex.label.font=1,
     vertex.label.cex=.5,
     vertex.label.family="STHeiti",layout=layFR,edge.width=0.05, 
     vertex.size=nodes$ltsb, main="")
