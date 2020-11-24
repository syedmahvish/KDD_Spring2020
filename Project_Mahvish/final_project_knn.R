remove(list = ls())

#install kknn 
#install.packages("kknn")
library(kknn)

#load data
fileName <- file.choose()
termination <- read.csv(fileName)
termination$STATUS <- factor(termination$STATUS, levels = c('A','T'), labels = c("not terminated","terminated"))
View(termination)
nrow(termination)

dev.off()

# knn classification for salary
index <- sample(nrow (termination), as.integer(.70 * nrow(termination)))
training<-termination[index,c(2,3,21)]
nrow(training)
test<- termination[-index,c(2,3,21)]
nrow(test)


#termination based on salary
predict_k <- kknn(formula=STATUS~., training, test[,-21], k=5, kernel ="rectangular")
fit <- fitted(predict_k)
output_k <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS != fit)
rate_k<-sum(wrong)/length(wrong)

predict_k1 <- kknn(formula=STATUS~., training, test[,-21], k=10, kernel ="rectangular")
fit <- fitted(predict_k1)
output_k1 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS != fit)
rate_k1<-sum(wrong)/length(wrong)

predict_k2 <- kknn(formula=STATUS~., training, test[,-21], k=20, kernel ="rectangular")
fit <- fitted(predict_k2)
output_k2 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k2<-sum(wrong)/length(wrong)

predict_k5 <- kknn(formula=STATUS~., training, test[,-21], k=50, kernel ="rectangular")
fit <- fitted(predict_k5)
output_k5 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k5<-sum(wrong)/length(wrong)

predict_k10 <- kknn(formula=STATUS~., training, test[,-21], k=100, kernel ="rectangular")
fit <- fitted(predict_k10)
output_k10 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k10 <-sum(wrong)/length(wrong)

predict_k15 <- kknn(formula=STATUS~., training, test[,-21], k=150, kernel ="rectangular")
fit <- fitted(predict_k15)
output_k15 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k15<-sum(wrong)/length(wrong)

predict_k20 <- kknn(formula=STATUS~., training, test[,-21], k=200, kernel ="rectangular")
fit <- fitted(predict_k20)
output_k20 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k20<-sum(wrong)/length(wrong)

predict_k30 <- kknn(formula=STATUS~., training, test[,-21], k=300, kernel ="rectangular")
fit <- fitted(predict_k30)
output_k30 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k30<-sum(wrong)/length(wrong)

predict_k50 <- kknn(formula=STATUS~., training, test[,-21], k=500, kernel ="rectangular")
fit <- fitted(predict_k50)
output_k50 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k50<-sum(wrong)/length(wrong)

predict_k100 <- kknn(formula=STATUS~., training, test[,-21], k=1000, kernel ="rectangular")
fit <- fitted(predict_k100)
output_k100 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k100<-sum(wrong)/length(wrong)


print('*******Annual salary and hourly salary used for classification of termination status for k = 5********')
output_k
rate_k
print('*******Annual salary and hourly salary used for classification of termination status for k = 10********')
output_k1
rate_k1
print('*******Annual salary and hourly salary used for classification of termination status for k = 20********')
output_k2
rate_k2
print('*******Annual salary and hourly salary used for classification of termination status for k = 50********')
output_k5
rate_k5
print('*******Annual salary and hourly salary used for classification of termination status for k = 1000********')
output_k10
rate_k10
print('*******Annual salary and hourly salary used for classification of termination status for k = 150********')
output_k15
rate_k15
print('*******Annual salary and hourly salary used for classification of termination status for k = 200********')
output_k20
rate_k20
print('*******Annual salary and hourly salary used for classification of termination status for k = 300********')
output_k30
rate_k30
print('*******Annual salary and hourly salary used for classification of termination status for k = 500********')
output_k50
rate_k50
print('*******Annual salary and hourly salary used for classification of termination status for k = 1000********')
output_k100
rate_k100


dev.off()

# knn classification for termination based on performance and job satisfication
index <- sample(nrow (termination), as.integer(.70 * nrow(termination)))
training<-termination[index,c(8,17,21,23,24,25,26,27)]
nrow(training)
test<- termination[-index,c(8,17,21,23,24,25,26,27)]
nrow(test)


predict_k <- kknn(formula=STATUS~., training, test[,-21], k=5, kernel ="rectangular")
fit <- fitted(predict_k)
output_k <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS != fit)
rate_k<-sum(wrong)/length(wrong)

predict_k1 <- kknn(formula=STATUS~., training, test[,-21], k=10, kernel ="rectangular")
fit <- fitted(predict_k1)
output_k1 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS != fit)
rate_k1<-sum(wrong)/length(wrong)

predict_k2 <- kknn(formula=STATUS~., training, test[,-21], k=20, kernel ="rectangular")
fit <- fitted(predict_k2)
output_k2 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k2<-sum(wrong)/length(wrong)

predict_k5 <- kknn(formula=STATUS~., training, test[,-21], k=50, kernel ="rectangular")
fit <- fitted(predict_k5)
output_k5 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k5<-sum(wrong)/length(wrong)

predict_k10 <- kknn(formula=STATUS~., training, test[,-21], k=100, kernel ="rectangular")
fit <- fitted(predict_k10)
output_k10 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k10 <-sum(wrong)/length(wrong)

predict_k15 <- kknn(formula=STATUS~., training, test[,-21], k=150, kernel ="rectangular")
fit <- fitted(predict_k15)
output_k15 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k15<-sum(wrong)/length(wrong)

predict_k20 <- kknn(formula=STATUS~., training, test[,-21], k=200, kernel ="rectangular")
fit <- fitted(predict_k20)
output_k20 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k20<-sum(wrong)/length(wrong)

predict_k30 <- kknn(formula=STATUS~., training, test[,-21], k=300, kernel ="rectangular")
fit <- fitted(predict_k30)
output_k30 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k30<-sum(wrong)/length(wrong)

predict_k50 <- kknn(formula=STATUS~., training, test[,-21], k=500, kernel ="rectangular")
fit <- fitted(predict_k50)
output_k50 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k50<-sum(wrong)/length(wrong)

predict_k100 <- kknn(formula=STATUS~., training, test[,-21], k=1000, kernel ="rectangular")
fit <- fitted(predict_k100)
output_k100 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k100<-sum(wrong)/length(wrong)


print('*******Performance and job satisfication used for classification of termination status for k = 5********')
output_k
rate_k
print('*******Performance and job satisfication used for classification of termination status for k = 10********')
output_k1
rate_k1
print('*******Performance and job satisfication used for classification of termination status for k = 20********')
output_k2
rate_k2
print('*******Performance and job satisfication used for classification of termination status for k = 50********')
output_k5
rate_k5
print('*******Performance and job satisfication used for classification of termination status for k = 1000********')
output_k10
rate_k10
print('*******Performance and job satisfication used for classification of termination status for k = 150********')
output_k15
rate_k15
print('*******Performance and job satisfication used for classification of termination status for k = 200********')
output_k20
rate_k20
print('*******Performance and job satisfication used for classification of termination status for k = 300********')
output_k30
rate_k30
print('*******Performance and job satisfication used for classification of termination status for k = 500********')
output_k50
rate_k50
print('*******Performance and job satisfication used for classification of termination status for k = 1000********')
output_k100
rate_k100

# knn classification for termination based on complete data
termination <- as.data.frame(sapply(termination, as.numeric))
termination$STATUS <- factor(termination$STATUS, levels = c(1,2), labels = c("not terminated","terminated"))

index <- sample(nrow (termination), as.integer(.70 * nrow(termination)))
training<-termination[index,c(-1,-14)]
nrow(training)
test<- termination[-index,c(-1,-14)]
nrow(test)

#termination based on complete data

predict_k <- kknn(formula=STATUS~., training, test[,-19], k=5, kernel ="rectangular")
fit <- fitted(predict_k)
output_k <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS != fit)
rate_k<-sum(wrong)/length(wrong)

predict_k1 <- kknn(formula=STATUS~., training, test[,-19], k=10, kernel ="rectangular")
fit <- fitted(predict_k1)
output_k1 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS != fit)
rate_k1<-sum(wrong)/length(wrong)

predict_k2 <- kknn(formula=STATUS~., training, test[,-19], k=20, kernel ="rectangular")
fit <- fitted(predict_k2)
output_k2 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k2<-sum(wrong)/length(wrong)

predict_k5 <- kknn(formula=STATUS~., training, test[,-19], k=50, kernel ="rectangular")
fit <- fitted(predict_k5)
output_k5 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k5<-sum(wrong)/length(wrong)

predict_k10 <- kknn(formula=STATUS~., training, test[,-19], k=100, kernel ="rectangular")
fit <- fitted(predict_k10)
output_k10 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k10 <-sum(wrong)/length(wrong)

predict_k15 <- kknn(formula=STATUS~., training, test[,-19], k=150, kernel ="rectangular")
fit <- fitted(predict_k15)
output_k15 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k15<-sum(wrong)/length(wrong)

predict_k20 <- kknn(formula=STATUS~., training, test[,-19], k=200, kernel ="rectangular")
fit <- fitted(predict_k20)
output_k20 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k20<-sum(wrong)/length(wrong)

predict_k30 <- kknn(formula=STATUS~., training, test[,-19], k=300, kernel ="rectangular")
fit <- fitted(predict_k30)
output_k30 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k30<-sum(wrong)/length(wrong)

predict_k50 <- kknn(formula=STATUS~., training, test[,-19], k=500, kernel ="rectangular")
fit <- fitted(predict_k50)
output_k50 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k50<-sum(wrong)/length(wrong)

predict_k100 <- kknn(formula=STATUS~., training, test[,-19], k=1000, kernel ="rectangular")
fit <- fitted(predict_k100)
output_k100 <- table(Actual=test$STATUS, Fitted=fit)
wrong<- ( test$STATUS!=fit)
rate_k100<-sum(wrong)/length(wrong)


print('*******Complete data used classification of termination status for k = 5********')
output_k
rate_k
print('*******Complete data used for classification of termination status for k = 10********')
output_k1
rate_k1
print('*******Complete data used for classification of termination status for k = 20********')
output_k2
rate_k2
print('*******Complete data for classification of termination status for k = 50********')
output_k5
rate_k5
print('*******Complete data used for classification of termination status for k = 1000********')
output_k10
rate_k10
print('*******Complete data used for classification of termination status for k = 150********')
output_k15
rate_k15
print('*******Complete data used for classification of termination status for k = 200********')
output_k20
rate_k20
print('*******Complete data used for classification of termination status for k = 300********')
output_k30
rate_k30
print('*******Complete data used for classification of termination status for k = 500********')
output_k50
rate_k50
print('*******Complete data used for classification of termination status for k = 1000********')
output_k100
rate_k100










