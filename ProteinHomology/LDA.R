

#LDA or QDA
library(MASS)

setwd("C:/Users/Hume Dickie/Desktop/KDD Cup")
bioData <- read.table("bio_train.dat")
attach(bioData)

#split data train/test 80/20
testIndex <- 116600
numRows <- 145751
train <- bioData[1:testIndex,]
test <- bioData[testIndex+1:numRows,]

#p-values with 3 stars in the summary
#v5, v8, v11, v13, v16, v17, v21, v23, v31, v33, v48, v49, v50 ,v51, v53, v56, v57, v61, v64, 65, 66 ,67
vars <- c('V5','V8','V11','V13','V16','V17','V21','V23','V31','V33','V48','V49','V50','V51','V53','V56','V57','V61','V64','V65','V66','V67')
formula <- paste(vars,sep = "", collapse = " + ")
#this doesn't work for some reason. Error: variable lengths differ

#model
lda.fit <- lda(V3~V5 + V8 + V11 + V13 + V16 + V17 + V21 + V23 + V31 + V33 + V48 + V49 + V50 + V51 + V53 + V56 + V57 + V61 + V64 + V65 + V66 + V67,data = train)
lda.pred <- predict(lda.fit,test)
lda.class <- lda.pred$class
confMat <- table(lda.class,test$V3)
(confMat[1,1]+confMat[2,2])/(numRows-testIndex)
