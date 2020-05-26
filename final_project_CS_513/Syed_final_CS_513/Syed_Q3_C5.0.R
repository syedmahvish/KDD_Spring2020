###  Q3 #######
######################################################################
#  Course          : Knowledge Discovery and Data Mining (CS 513-A)
#  First Name      : Mahvish
#  Last Name       : Syed
#  Id              : 10456845
#  purpose         : Use the C5.0 methodology to develop a classification model for the Admission  
######################################################################


rm(list=ls())
file<-file.choose()
admission <-  read.csv(file,
                na.strings = "?",
                colClasses=c("Applicant"="factor",
                             "ADMIT"="factor","RANK"="factor","GPA"="factor",
                             "GRE"="factor"))

admission$ADMIT<-factor(admission$ADMIT, levels = c(0,1), labels = c("Not Admit","Admit"))

#install.packages("C50")
library('C50')

# get tarining and test data
index <- sort(sample(nrow(admission),round(.30*nrow(admission))))
training <- admission[-index,]
test <- admission[index,]

#perform c5.0 classification
C50_admission <- C5.0( ADMIT~.,data=training[,-1,-2] )

summary(C50_admission)
dev.off()
#plot them
plot(C50_admission)

C50_predict<-predict( C50_admission ,test , type="class" )

table(actual=test$ADMIT,C50=C50_predict)
wrong<- (test$ADMIT !=C50_predict)
error_rate<-sum(wrong)/length(test$ADMIT)
error_rate

#accuracy 
accuracy <- (1 - error_rate) * 100
accuracy


