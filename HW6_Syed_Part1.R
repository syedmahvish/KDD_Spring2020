###Assignment no : 6 Q1#######
######################################################################
#  Course          : Knowledge Discovery and Data Mining (CS 513-A)
#  First Name      : Mahvish
#  Last Name       : Syed
#  Id              : 10456845
#  purpose         : Use the C5.0 methodology to develop a classification model for the Diagnosis.  
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

#load library
library(C50)

#classification and prediction using C5.0 method
C5.0_result <- C5.0(x = training[,2:10], y = training$Class)
summary(C5.0_result)
C5.0_predict <- predict(C5.0_result , test[,2:10])
table(C5.0_predict, test$Class)
#error rate for C5.0_predict
wrong <- (test[,11] != C5.0_predict)
error_rate= sum(wrong)/length(wrong)
error_rate

#classification and prediction using C5.0 method with 10 trials.
C5.0_trail <- C5.0(x = training[,2:10], y = training$Class, trials = 10)
C5.0_trail_predict <- predict(C5.0_trail , test[,2:10])
table(C5.0_trail_predict, test$Class)
wrong <- (test[,11] != C5.0_trail_predict)
error_rate= sum(wrong)/length(wrong)
error_rate

#classification and prediction using C5.0 method with rules option as true.
C5.0_rule <- C5.0(x = training[,2:10], y = training$Class, rules = TRUE)
C5.0_rule_predict <- predict(C5.0_rule , test[,2:10])
table(C5.0_rule_predict, test$Class)
wrong <- (test[,11] != C5.0_rule_predict)
error_rate= sum(wrong)/length(wrong)
error_rate

#classification and prediction using C5.0 method with 10 trials and rules option as true..
C5.0_rule_trial <- C5.0(x = training[,2:10], y = training$Class,trials = 10, rules = TRUE)
C5.0_rule_trial_predict <- predict(C5.0_rule_trial , test[,2:10])
table(C5.0_rule_trial_predict, test$Class)
wrong <- (test[,11] != C5.0_rule_trial_predict)
error_rate= sum(wrong)/length(wrong)
error_rate
