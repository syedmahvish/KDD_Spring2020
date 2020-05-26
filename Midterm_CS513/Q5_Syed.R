###    HW_Midterm_exam Q5  #######
######################################################################
#  Course          : Knowledge Discovery and Data Mining (CS 513-A)
#  First Name      : Mahvish
#  Last Name       : Syed
#  Id              : 10456845
#purpose           : Construct a Naïve Bayes model to classify infection on COVID19 data
######################################################################

#Read data from covid19_v3.csv
rm(list = ls())
fileName <- file.choose()

#Read file and replace  ? with NA
covidData <- read.csv(fileName, na.strings = "?" ) 

#remove NA's entry
covidData<-na.omit(covidData)
View(covidData)

#discretize the age and monthAtHospital

#find value greater than 50 and replace with "51 or over" and store back in column age
covidData$Age <- replace(covidData$Age , covidData$Age > 50, "51 or over")
#find value between “35 to 50” and replace with "35 to 50"
covidData$Age <- replace(covidData$Age , covidData$Age >= 35 & covidData$Age <= 50, "35 to 50")
#find value less than 35 and replace with "less than 35"
covidData$Age <- replace(covidData$Age , covidData$Age < 35, "less than 35")



#find value of “MonthAtHospital” greater than 6 and replace it to  and “6 or more months”
covidData$MonthAtHospital <- replace(covidData$MonthAtHospital ,covidData$MonthAtHospital >= 6, "6 or more months")
#find value of “MonthAtHospital” less than 6 and replace it to into “less than 6 months” 
covidData$MonthAtHospital <- replace(covidData$MonthAtHospital ,covidData$MonthAtHospital < 6, "less than 6 months")



#convert all colums as factor
covidData$Age <- as.factor(covidData$Age)
covidData$Exposure <- as.factor(covidData$Exposure)
covidData$MaritalStatus <- as.factor(covidData$MaritalStatus)
covidData$Cases <- as.factor(covidData$Cases)
covidData$MonthAtHospital <- as.factor(covidData$MonthAtHospital)
covidData$Infected <- as.factor(covidData$Infected)

#load library
library(e1071)

# get training(70%) and testing(30%) data
index <- sort(sample(nrow(covidData),round(.30*nrow(covidData))))
training <- covidData[-index,]
test <- covidData[index,]

## Naive Bayes classification using all variables 
nBayes <- naiveBayes(Infected~., data =training[,-1])

#perform prediction using test data
category_all<-predict(nBayes,test[,-1])

# confusion matrix
table(NBayes=category_all, Infected=test$Infected)

# calculate error rate
NB_wrong<-sum(category_all!=test$Infected )
NB_error_rate<-NB_wrong/length(category_all)
NB_error_rate







