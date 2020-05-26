###Assignment no : 7#######
######################################################################
#  Course          : Knowledge Discovery and Data Mining (CS 513-A)
#  First Name      : Mahvish
#  Last Name       : Syed
#  Id              : 10456845
#  purpose         : Use the ANN methodology to develop a classification model for the Diagnosis.  
######################################################################
rm(list=ls())

filename <- file.choose()
breastCancer <- read.csv(filename, na.strings = "?" ) 
View(breastCancer)
dev.off()

#To factor the data set
breastCancer<-data.frame(lapply(na.omit(breastCancer),as.numeric))

# get tarining and test data
index <- sort(sample(nrow(breastCancer),round(.30*nrow(breastCancer))))
training <- breastCancer[-index,]
test <- breastCancer[index,]

#install.packages("neuralnet")
library(neuralnet)

#perform classification using ANN
result <- neuralnet(diagnosis~. , data = training[-1] , hidden = 5, threshold = 0.01)
plot(result)

#Compute for ANN
predict <- compute(result, test)
predict$net.result 

#Display output in actual and ANN output format.
clean_output <- cbind(test$diagnosis , as.data.frame(predict$net.result))
colnames(clean_output) <- c("Actual_diagnosis" , "Neural_Net_output")
print(clean_output)

#display confusion matrix and error rate for ANN
predict_range <- ifelse(predict$net.result  <1.5,1,2)
table(predict_range , test$diagnosis)
wrong <- (test$diagnosis != predict_range)
error_rate= sum(wrong)/length(wrong)
error_rate



