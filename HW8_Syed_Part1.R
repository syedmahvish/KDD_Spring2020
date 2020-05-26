### Assignment no : 8 Q1 #######
######################################################################
#  Course          : Knowledge Discovery and Data Mining (CS 513-A)
#  First Name      : Mahvish
#  Last Name       : Syed
#  Id              : 10456845
#  purpose         : Using hclust to develop a Cluster model.  
######################################################################
rm(list=ls())

filename <- file.choose()
breastCancer <- read.csv(filename, na.strings = "?" ) 
View(breastCancer)
dev.off()

#To factor the data set
breastCancer<-data.frame(lapply(na.omit(breastCancer),as.numeric))

#obtain distance between two points of all feature except ID and Diagnosis
breastCancer_dist <- dist(breastCancer[,-1,-2])
#obtain cluster for all feature except ID and Diagnosis
breastCancer_clust <- hclust(breastCancer_dist)
#plot cluster
plot(breastCancer_clust)

#obtain two cluster
breastCancer_clust_two <- cutree(breastCancer_clust , 2)

#tabulate clustered rows against the “diagnosis” column 
table(breastCancer_clust_two , breastCancer$diagnosis)

