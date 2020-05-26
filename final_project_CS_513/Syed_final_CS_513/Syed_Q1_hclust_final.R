### Q1 Part2#######
######################################################################
#  Course          : Knowledge Discovery and Data Mining (CS 513-A)
#  First Name      : Mahvish
#  Last Name       : Syed
#  Id              : 10456845
#  purpose         : Using hclust to develop a Cluster model.  
######################################################################
rm(list=ls())

filename <- file.choose()
admission <- read.csv(filename, na.strings = "?" ) 
View(admission)
dev.off()

#To factor the data set
admission<-data.frame(lapply(na.omit(admission),as.numeric))

#obtain two cluster for Gre and Gpa feature
admission_dist <- dist(admission[,3,4])

admission_clust <- hclust(admission_dist)

#plot cluster
plot(admission_clust)

admission_cuttree_two <- cutree(admission_clust , 2)

#tabulate clustered rows against the “Admit” column 
admission$ADMIT<-factor(admission$ADMIT, levels = c(0,1), labels = c("Not Admit","Admit"))
table(admission_cuttree_two , admission$ADMIT)

