###    HW_Midterm_exam Q2   #######
######################################################################
#  Course          : Knowledge Discovery and Data Mining (CS 513-A)
#  First Name      : Mahvish
#  Last Name       : Syed
#  Id              : 10456845
# purpose          : Perform the EDA on Covid19 data.
######################################################################


rm(list = ls())

fileName <- file.choose()

covidData <- read.csv(fileName, na.strings = "?" ) 

View(covidData)


#I.	Summarizing each column (e.g. min, max, mean )
summary(covidData)

#II.	Identifying missing values
na_values_covidData <- is.na(covidData)
View(na_values_covidData)

#III.	Displaying the frequency table of “Infected” vs. “MaritalStatus” 
table(Infected = covidData$Infected, Martial_status = covidData$MaritalStatus)


#IV.	Displaying the scatter plot of “Age”, “MaritalStatus” and “MonthAtHospital”, one pair at a time
plot(covidData$Age , covidData$MaritalStatus , main = 'Age vs MaritalStatus', xlab = "Age", ylab = "MaritalStatus", pch = 21,  bg = c("red", "green3"))
plot(covidData$Age , covidData$MonthAtHospital, main = 'Age vs MonthAtHospital', xlab = "Age", ylab = "MonthAtHospital", pch = 21, bg = c("red", "green3"))
plot(covidData$MaritalStatus, covidData$MonthAtHospital, main = 'MonthAtHospital vs MaritalStatus', xlab = "MaritalStatus", ylab = "MonthAtHospital", pch = 21, bg = c("red", "green3"))


#V.	Show box plots for columns:  “Age”, “MaritalStatus” and “MonthAtHospital”
boxplot(covidData$Age, covidData$MaritalStatus , covidData$MonthAtHospital)

#VI.	Replacing the missing values of “Cases” with the “mean” of “Cases”.
covidData[is.na(covidData$Cases) , 5] <- mean(covidData$Cases, na.rm = TRUE)
View(covidData)


