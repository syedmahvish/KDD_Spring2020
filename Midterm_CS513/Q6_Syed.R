###    HW_Midterm_exam Q6    #######
######################################################################
#  Course          : Knowledge Discovery and Data Mining (CS 513-A)
#  First Name      : Mahvish
#  Last Name       : Syed
#  Id              : 10456845
#purpose           : Construct a CART model to classify infection on COVID19 data.
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

#load libraries
library(rpart)
library(rpart.plot)  			
library(rattle)         
library(RColorBrewer) 

## get training(70%) and testing(30%) data
index <- sort(sample(nrow(covidData),round(.30*nrow(covidData))))
training <- covidData[-index,]
test <- covidData[index,]

## construct cart  model for training data.
CART_Infected <- rpart( Infected~., data = training[,-1])
## plot cart model using fancyRpartPlot
fancyRpartPlot(CART_Infected)
#perform prediction on test data
CART_predict2 <- predict(CART_Infected ,test[,-1], type="class")
CART_predict2
df<-as.data.frame(cbind(test,CART_predict2))
df
#confusion matrix
table(Actual=test$Infected,CART=CART_predict2)
#calculate wrong classified
CART_wrong<-sum(test[,"Infected"]!=CART_predict2)
CART_wrong
#calculate error rate
error_rate= CART_wrong/length(test$Infected)
error_rate









