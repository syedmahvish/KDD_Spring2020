remove(list = ls())


#load data
fileName <- file.choose()
termination <- read.csv(fileName)
termination$STATUS <- factor(termination$STATUS, levels = c('A','T'), labels = c("not terminated","terminated"))
View(termination)
nrow(termination)


#cluster on basis of rehired.
termination$REHIRE = as.numeric(termination$REHIRE)

termination_clust_kmean <- kmeans(termination[,13,-21], 2 , nstart = 10)

termination_clust_kmean$cluster
termination_clust_kmean$centers

termination_clust_kmean$cluster <- factor(termination_clust_kmean$cluster, levels = c(1,2), labels = c("not Rehire","Rehire"))

#tabulate clustered rows against the “Status” column 
table(termination_clust_kmean$cluster , termination$STATUS)
plot(termination_clust_kmean$cluster , termination$STATUS)

#cluster on basis education level.
termination$EDUCATION_LEVEL = as.numeric(termination$EDUCATION_LEVEL)
termination_clust_kmean <- kmeans(termination[,20,-21], max(termination$EDUCATION_LEVEL) , nstart = 10)

termination_clust_kmean$cluster
termination_clust_kmean$centers

termination_clust_kmean$cluster <- factor(termination_clust_kmean$cluster, levels = c(1,2,3,4,5), labels = c("level 1","level 2", "level 3","level 4","level 5"))

#tabulate clustered rows against the “Status” column 
table(termination_clust_kmean$cluster , termination$STATUS)
plot(termination_clust_kmean$cluster , termination$STATUS)


#cluster on basis disable employee or vet terminated.
termination$DISABLED_EMP = as.numeric(termination$DISABLED_EMP)
termination$DISABLED_VET = as.numeric(termination$DISABLED_VET)
termination_clust_kmean <- kmeans(termination[,c(18,19),-21], 2 , nstart = 10)
termination_clust_kmean$cluster
termination_clust_kmean$centers
termination_clust_kmean$cluster <- factor(termination_clust_kmean$cluster, levels = c(1,2), labels = c("Disable Emp/Vet","Not Disable Emp/Vet"))
table(termination_clust_kmean$cluster , termination$STATUS)
plot(termination_clust_kmean$cluster , termination$STATUS)


#cluster on basis of all data
termination <- as.data.frame(sapply(termination, as.numeric))

termination_clust_kmean <- kmeans(termination[,c(-1,-14)], 10 , nstart = 10)

termination_clust_kmean$cluster
termination_clust_kmean$centers

termination$STATUS <- factor(termination$STATUS, levels = c(1,2), labels = c("not terminated","terminated"))
table(termination_clust_kmean$cluster , termination$STATUS)


#cluster for year of termination
termination <- na.omit(termination)
termination_clust_kmean <- kmeans(termination[,14,-21], length(unique(termination$TERMINATION_YEAR)) , nstart = 10)
termination_clust_kmean$cluster
termination_clust_kmean$centers
termination_clust_kmean$cluster <- factor(termination_clust_kmean$cluster, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), labels = c(2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017))
table(termination_clust_kmean$cluster , termination$STATUS)
plot(termination_clust_kmean$cluster , termination$STATUS)






