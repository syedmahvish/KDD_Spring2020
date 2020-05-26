###  Q1 Part1 #######
######################################################################
#  Course          : Knowledge Discovery and Data Mining (CS 513-A)
#  First Name      : Mahvish
#  Last Name       : Syed
#  Id              : 10456845
#  purpose         : Using kmeans clustering method to develop a Cluster model.  
######################################################################

rm(list=ls())

filename <- file.choose()
admission <- read.csv(filename, na.strings = "?" ) 
View(admission)
dev.off()

#To factor the data set
admission<-data.frame(lapply(na.omit(admission),as.numeric))

#obtain two cluster for Gre and Gpa feature
admission_clust_kmean <- kmeans(admission[,c(3,4)], 2 , nstart = 10)
#plot cluster
admission_clust_kmean$cluster
admission_clust_kmean$centers

#tabulate clustered rows against the “ADMIT” column 
admission$ADMIT<-factor(admission$ADMIT, levels = c(0,1), labels = c("Not Admit","Admit"))
table(admission_clust_kmean$cluster , admission$ADMIT)