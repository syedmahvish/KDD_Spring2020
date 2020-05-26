###Assignment no : 6 Q2#######
######################################################################
#  Course          : Knowledge Discovery and Data Mining (CS 513-A)
#  First Name      : Mahvish
#  Last Name       : Syed
#  Id              : 10456845
#  purpose         : Use the Random Forest methodology to develop a classification model for the Diagnosis 
#                     and identify important features..  
######################################################################


rm(list=ls())

filename <- file.choose()
breastCancer <-  read.csv(filename )
View(breastCancer)
dev.off()

## categories are represented by the “factor” data 
breastCancer$Class<-factor(breastCancer$Class, levels = c(2,4), labels = c("benign","malignant"))
breastCancer <- breastCancer

# get tarining and test data
index <- sort(sample(nrow(breastCancer),round(.30*nrow(breastCancer))))
training <- breastCancer[-index,]
test <- breastCancer[index,]

#perform classification using randomForest.
result <- randomForest(Class~. , data = training , importance = TRUE, ntree = 1000)

#identify important features
importance(result)
varImpPlot(result)

#perform prediction
randomForest_predict <- predict(result , test)
table(randomForest_predict, test$Class)
#error rate for randomForest.
wrong <- (test[,11] != randomForest_predict)
error_rate= sum(wrong)/length(wrong)
error_rate







