#SVM
setwd("C:/Users/Hume Dickie/Desktop/KDD Cup")
data <- read.table("bio_train.dat")

require(sampling)
stratifiedSamples <- strata(data,'V3',c(1296,1296),"srswor")

subset1 <- data[stratifiedSamples$ID_unit,]
train <- subset1[c(1:972,1620:2592),]
test<- subset1[c(973:1619),]

require(e1071)
tune.out <- tune(svm,V3~.,data=train,kernel="linear",ranges = list(cost = c(1.0,1.25,1.5,1.75,2.0)))
tune.out

linearAll.fit <- svm(V3~.,data = train,kernel = "linear",cost = 0.073)
linear5.fit <- svm(V3~V8+V48+V58+V61+V66,data = train,kernal = "linear",cost = 0.073)

tune.out <- tune(svm,V3~.,data=train,kernel="radial",ranges = list(cost = c(1.0,1.25,1.5,1.75,2.0)))
tune.out

radialAll.fit <- svm(V3~.,data = train,kernel = "radial",cost = 0.043)
radial5.fit <- svm(V3~V8+V48+V58+V61+V66,data = train,kernal = "radial",cost = 0.043)


(table(data$V3)[2])/(dim(data)[1])
#ERROR TO BEAT 0.008891877

#All
trainingPred <- predict(linearAll.fit,train)
trainConfMatrix <- table(trainingPred,train$V3)
(trainConfMatrix[2,1] + trainConfMatrix[1,2])/dim(train)[1]
#training error = 0.0005141388

testPred <- predict(linearAll.fit,test)
testConfMatrix <- table(testPred,test$V3)
(testConfMatrix[2,1] + testConfMatrix[1,2])/dim(test)[1]
#test error = 0.001545595

#only 5 
trainingPred <- predict(linear5.fit,train)
trainConfMatrix <- table(trainingPred,train$V3)
(trainConfMatrix[2,1] + trainConfMatrix[1,2])/dim(train)[1]
#training error = 0.0005141388

testPred <- predict(linear5.fit,test)
testConfMatrix <- table(testPred,test$V3)
(testConfMatrix[2,1] + testConfMatrix[1,2])/dim(test)[1]
# test error = 0.001545595

#RADIAL KERNEL
#All
trainingPred <- predict(radialAll.fit,train)
trainConfMatrix <- table(trainingPred,train$V3)
(trainConfMatrix[2,1] + trainConfMatrix[1,2])/dim(train)[1]
#training error = 0.0005141388

testPred <- predict(radialAll.fit,test)
testConfMatrix <- table(testPred,test$V3)
(testConfMatrix[2,1] + testConfMatrix[1,2])/dim(test)[1]
#test error = 0.001545595

#only 5 
trainingPred <- predict(radial5.fit,train)
trainConfMatrix <- table(trainingPred,train$V3)
(trainConfMatrix[2,1] + trainConfMatrix[1,2])/dim(train)[1]
#training error = 0.0005141388

testPred <- predict(radial5.fit,test)
testConfMatrix <- table(testPred,test$V3)
(testConfMatrix[2,1] + testConfMatrix[1,2])/dim(test)[1]
# test error = 0.001545595


