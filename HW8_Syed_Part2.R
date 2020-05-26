### Assignment no : 8 Q2 #######
######################################################################
#  Course          : Knowledge Discovery and Data Mining (CS 513-A)
#  First Name      : Mahvish
#  Last Name       : Syed
#  Id              : 10456845
#  purpose         : Using kmean to develop a Cluster model.  
######################################################################
rm(list=ls())

filename <- file.choose()
breastCancer <- read.csv(filename, na.strings = "?" ) 
View(breastCancer)
dev.off()

#To factor the data set
breastCancer<-data.frame(lapply(na.omit(breastCancer),as.numeric))

#obtain two cluster for all feature except ID and Diagnosis
breastCancer_clust_kmean <- kmeans(breastCancer[,-1,-2], 2 , nstart = 10)
#plot cluster
breastCancer_clust_kmean$cluster
breastCancer_clust_kmean$centers

#tabulate clustered rows against the “diagnosis” column 
table(breastCancer_clust_kmean$cluster , breastCancer$diagnosis)

