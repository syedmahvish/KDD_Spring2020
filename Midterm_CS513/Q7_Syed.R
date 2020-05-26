
###    HW_Midterm_exam Q7  #######
######################################################################
#  Course          : Knowledge Discovery and Data Mining (CS 513-A)
#  First Name      : Mahvish
#  Last Name       : Syed
#  Id              : 10456845
#purpose           : Constructunweighted knn to  predict infection rate for Covid19 data
######################################################################

rm(list=ls())
#read data from file and replace ? with NA
file <- file.choose()
covidData <-  read.csv(file,
                na.strings = "?")

#remove NA's entry
covidData <- na.omit(covidData)
View(covidData)

#convert all colums as factor
covidData$Age <- as.factor(covidData$Age)
covidData$Exposure <- as.factor(covidData$Exposure)
covidData$MaritalStatus <- as.factor(covidData$MaritalStatus)
covidData$Cases <- as.factor(covidData$Cases)
covidData$MonthAtHospital <- as.factor(covidData$MonthAtHospital)
covidData$Infected <- as.factor(covidData$Infected)

# get training(70%) and testing(30%) data
index <- sort(sample(nrow( covidData),round(.30*nrow(covidData))))
training <- covidData[-index,]
test <- covidData[index,]

#load library
library(kknn) 

#Perform knn with k = 5 to predict infection rate.
predict_k1 <- kknn(formula= Infected~., training[,c(-1)] , test[,c(-1)], k=5,kernel ="rectangular"  )

# fit prediction
fit <- fitted(predict_k1)

#create confusion matrix
table(test$Infected, fit)

#calculate wrong classification and error rate
wrong <- (test$Infected!=fit)
error_rate <- sum(wrong)/length(wrong)
error_rate






                           