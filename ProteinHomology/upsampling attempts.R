#Data Mining PROJECT 
#John LaGue

#Our initial code using logisitc regression and upsampling
#This code is my attempt at reducing the number of predictor variables and then applying bootstrap (random sampling with replacement) upsampling to the data set
# We found that upsampling only a small amount (rather than 50/50 for each class, we did 85/15) provided the best results. 
#The data without alteration has a 99/1 ratio of 0s to 1s in the target variable column
#*****See 'try 4' for our best logistic model using upsampling


library(randomForest)
library(MASS)
library(class)
library(ggplot2)
library(ISLR)
library("RWeka")
getwd()
f="5 Semester/Data Mining/Project/bio_trainT.arff"
setwd("c:/Users/John/Documents")
data=read.arff(f)

base_logit<- glm(attribute_2~., data=data, family="binomial")
summary(base_logit)

logit<-suppressWarnings(glm(attribute_2~attribute_4+ attribute_7+ attribute_10+ attribute_12+ attribute_15+ attribute_16+ attribute_20+ attribute_22+ attribute_30+ attribute_32+ attribute_47+ attribute_48+ attribute_49+ attribute_50+ attribute_52+attribute_55+ attribute_56+ attribute_60+attribute_63+attribute_64+attribute_65+attribute_66, data=data, family='binomial'))
summary(logit)

logit1<-suppressWarnings(glm(attribute_2~attribute_4+ attribute_10+ attribute_12+ attribute_15+ attribute_16+ attribute_20+ attribute_22+ attribute_30+ attribute_32+ attribute_47+  attribute_50+ attribute_52+attribute_55+ attribute_56+ attribute_60+attribute_63+attribute_64+attribute_65+attribute_66, data=data, family='binomial'))
summary(logit1)

#------------------train/test
trainsize<-floor(length(data[,1])*.7)
set.seed(1)
data.train<- sample(length(data[,1]), trainsize)
train<-data[data.train,]
test<-data[-data.train,]

#-----------Upsample---------------
toAdd=d<-data[!(data$attribute_2==0),]
toAdd= toAdd[sample(1:nrow(toAdd), (5*1296), replace = TRUE),]
train<-rbind(train, toAdd)

#------------Find with optimal attributes-----------------------------------------------------
#train<-data[1:trainsize,]
#test<-data[trainsize+1:length(data[,1]),]

logitT<-suppressWarnings(glm(attribute_2~attribute_4+ attribute_7+ attribute_10+ attribute_12+ attribute_15+ attribute_16+ attribute_20+ attribute_22+ attribute_30+ attribute_32+ attribute_47+ attribute_48+ attribute_49+ attribute_50+ attribute_52+attribute_55+ attribute_56+ attribute_60+attribute_63+attribute_64+attribute_65+attribute_66, data=train, family='binomial'))
summary(logitT)

logitT1<-suppressWarnings(glm(attribute_2~attribute_4+ attribute_10+ attribute_12+ attribute_15+ attribute_16+ attribute_20+ attribute_22+ attribute_30+ attribute_32+ attribute_47+ attribute_52+attribute_55+ attribute_56+ attribute_60++ attribute_63+ attribute_64+attribute_65+attribute_66, data=train, family='binomial'))
summary(logitT1)

#Performance on Training Set---------
probs<- predict(logitT1, type='response')
preds=ifelse(probs>0.5,1,0)

table(preds, train[,3])
mean(preds==train[,3])

#Performance on Test Set---------
probsTest<- predict(logitT1, newdata= test, type='response')
predsTest=ifelse(probsTest>0.5,1,0)
table(predsTest, test[,3])
(43310+297)/(43310+297+96+22)
mean(predsTest==test[,3])


# Trying to correct for imbalanced classes-----------

#------------------ADDING observations that have 1-------------------------

balancing_logit<- suppressWarnings(glm(attribute_2~., data=train, family='binomial'))
summary(balancing_logit)

balancing_logitT<-suppressWarnings(glm(attribute_2~ attribute_0 + attribute_1+attribute_3+attribute_4+ attribute_6+attribute_7+ attribute_8+attribute_9+attribute_10+attribute_11+ attribute_12+attribute_13+attribute_14+ attribute_15+ attribute_16+ attribute_17+attribute_20+ attribute_22+ attribute_23+attribute_25+ attribute_31+attribute_32+ attribute_33+attribute_34+attribute_35+attribute_36+attribute_37+attribute_38+attribute_39+attribute_40+attribute_42+attribute_47+ attribute_48+ attribute_49+ attribute_50+ attribute_52+attribute_54+attribute_55+ attribute_56+attribute_59+attribute_60+attribute_63+attribute_64+attribute_65+attribute_66+attribute_67+attribute_68+attribute_69+attribute_70+attribute_71+attribute_72+attribute_73+attribute_74+attribute_75, data=train, family='binomial'))
summary(balancing_logitT)

balancing_logitT2<-suppressWarnings(glm(attribute_2~ attribute_0 + attribute_1+attribute_3+attribute_4+ attribute_6+attribute_7+ attribute_8+attribute_9+attribute_10+attribute_11+ attribute_12+attribute_13+attribute_14+ attribute_15+ attribute_16+ attribute_17+attribute_20+ attribute_22+ attribute_23+attribute_25+attribute_32+ attribute_33+attribute_34+attribute_35+attribute_36+attribute_37+attribute_38+attribute_39+attribute_40+attribute_42+attribute_47+ attribute_48+ attribute_49+ attribute_50+ attribute_52+attribute_54+attribute_55+ attribute_56+attribute_59+attribute_60+attribute_63+attribute_64+attribute_65+attribute_66+attribute_67+attribute_68+attribute_69+attribute_70+attribute_71+attribute_73+attribute_74+attribute_75, data=train, family='binomial'))
summary(balancing_logitT2)

#---Testing Performance---------------

probsTest1<- predict(balancing_logit, newdata= test, type='response')
predsTest1=ifelse(probsTest1>0.5,1,0)
table(predsTest1, test[,3])
mean(predsTest1==test[,3])
#-----
probsTest2<- predict(balancing_logitT, newdata= test, type='response')
predsTest2=ifelse(probsTest2>0.5,1,0)
table(predsTest2, test[,3])
mean(predsTest2==test[,3])
#------
probsTest3<- predict(balancing_logitT2, newdata= test, type='response')
predsTest3=ifelse(probsTest3>0.5,1,0)
table(predsTest3, test[,3])
mean(predsTest3==test[,3])

#-----Reducing false positives now----------

#balancing_logit1<- suppressWarnings(glm(attribute_2~., data=train, family='binomial'))
#summary(balancing_logit1)

#balancing_logit1T<-suppressWarnings(glm(attribute_2~ attribute_0 + attribute_1+attribute_3+attribute_4+ attribute_6+attribute_7+ attribute_8+attribute_9+attribute_10+attribute_11+ attribute_12+attribute_13+attribute_14+ attribute_15+ attribute_16+ attribute_17+attribute_20+ attribute_22+ attribute_23+attribute_25+ attribute_31+attribute_32+ attribute_33+attribute_34+attribute_35+attribute_36+attribute_37+attribute_38+attribute_39+attribute_40+attribute_42+attribute_47+ attribute_48+ attribute_49+ attribute_50+ attribute_52+attribute_55+ attribute_56+attribute_59+attribute_60+attribute_63+attribute_64+attribute_65+attribute_66+attribute_67+attribute_68+attribute_69+attribute_70+attribute_71+attribute_72+attribute_73+attribute_74+attribute_75, data=train, family='binomial'))
#summary(balancing_logit1T)

#balancing_logitT12<-suppressWarnings(glm(attribute_2~ attribute_0 + attribute_1+attribute_3+attribute_4+ attribute_6+attribute_7+ attribute_8+attribute_9+attribute_10+attribute_11+ attribute_12+attribute_13+attribute_14+ attribute_15+ attribute_16+ attribute_17+attribute_20+ attribute_22+ attribute_23+attribute_25+attribute_32+ attribute_33+attribute_34+attribute_35+attribute_36+attribute_37+attribute_38+attribute_39+attribute_40+attribute_42+attribute_47+ attribute_48+ attribute_49+ attribute_50+ attribute_52+attribute_54+attribute_55+ attribute_56+attribute_59+attribute_60+attribute_63+attribute_64+attribute_65+attribute_66+attribute_67+attribute_68+attribute_69+attribute_70+attribute_71+attribute_73+attribute_74+attribute_75, data=train, family='binomial'))
#summary(balancing_logitT12)

#---Testing Performance---------------

probsTest11<- predict(balancing_logit1, newdata= test, type='response')
predsTest11=ifelse(probsTest11>0.5,1,0)
table(predsTest11, test[,3])
mean(predsTest11==test[,3])
#-----
probsTest12<- predict(balancing_logit1T, newdata= test, type='response')
predsTest12=ifelse(probsTest12>0.5,1,0)
table(predsTest12, test[,3])
mean(predsTest12==test[,3])
#------
probsTest13<- predict(balancing_logitT12, newdata= test, type='response')
predsTest13=ifelse(probsTest13>0.9,1,0)
table(predsTest13, test[,3])
mean(predsTest13==test[,3])


#----------------------------Try 3-------------------------

#balancing_logit2<- suppressWarnings(glm(attribute_2~., data=train, family='binomial'))
#summary(balancing_logit2)

balancing_logit2T<-suppressWarnings(glm(attribute_2~ attribute_0 + attribute_1+attribute_3+attribute_4+ attribute_6+attribute_7+ attribute_8+attribute_9+attribute_10+attribute_11+ attribute_12+attribute_13+attribute_14+ attribute_15+ attribute_16+ attribute_17+attribute_20+ attribute_22+ attribute_23+attribute_25+ attribute_31+ attribute_33+attribute_34+attribute_35+attribute_36+attribute_37+attribute_39+attribute_40+attribute_42+attribute_47+ attribute_48+ attribute_49+ attribute_50+ attribute_52+attribute_55+ attribute_56+attribute_60+attribute_63+attribute_64+attribute_65+attribute_66+attribute_67+attribute_68+attribute_69+attribute_70+attribute_71+attribute_72+attribute_73+attribute_74+attribute_75, data=train, family='binomial'))
summary(balancing_logit2T)


#---Testing Performance---------------

#probsTest21<- predict(balancing_logit2, newdata= test, type='response')
#predsTest21=ifelse(probsTest21>0.5,1,0)
#table(predsTest21, test[,3])
#mean(predsTest21==test[,3])

#-----
probsTest12<- predict(balancing_logit2T, newdata= test, type='response')
predsTest12=ifelse(probsTest12>0.915,1,0)
table(predsTest12, test[,3])
mean(predsTest12==test[,3])


#-------------------------Try 4------------------------- 

#****************THIS IS BEST LOGISTIC MODEL AND FINAL MODEL USED FOR JOHNS RESULTS ***************************************

#balancing_logit3<- suppressWarnings(glm(attribute_2~., data=train, family='binomial'))
#summary(balancing_logit3)

#best performing model. Make sure to reset the training and testing sets before adding upsampling.
balancing_logit3T<-suppressWarnings(glm(attribute_2~ attribute_0 + attribute_1+attribute_3+attribute_4+ attribute_6+attribute_7+ attribute_8+attribute_9+attribute_10+attribute_11+ attribute_12+attribute_13+attribute_14+ attribute_15+ attribute_16+attribute_20+ attribute_21+attribute_22+ attribute_23+attribute_25+ attribute_32+ attribute_33+attribute_34+attribute_35+attribute_36+attribute_37+attribute_39+attribute_40+attribute_42+attribute_47+ attribute_48+ attribute_49+ attribute_50+ attribute_52+attribute_55+ attribute_56+attribute_59+attribute_60+attribute_63+attribute_64+attribute_65+attribute_66+attribute_67+attribute_68+attribute_69+attribute_70+attribute_71+attribute_72+attribute_73+attribute_74+attribute_75, data=train, family='binomial'))
summary(balancing_logit3T)


#---Testing Performance---------------

#probsTest31<- predict(balancing_logit3, type='response')
#predsTest31=ifelse(probsTest31>0.5,1,0)
#table(predsTest31, train[,3])
#mean(predsTest31==train[,3])

#-----
probsTest13<- predict(balancing_logit3T, newdata= test, type='response')
predsTest13=ifelse(probsTest13>0.15,1,0)
table(predsTest13, test[,3])
mean(predsTest13==test[,3])


#-------------LDA------------------------------------------------------------------------------
trainsize<-round(length(data[,1])*.7)
train<-data[1:trainsize,]
test<-data[trainsize+1:length(data[,1]),]
?qda
base_lda<- suppressWarnings(qda(attribute_2~attribute_0 + attribute_1+attribute_3+attribute_4+ attribute_6+attribute_7+ attribute_8+attribute_9+attribute_10+attribute_11+ attribute_12+attribute_13+attribute_14+ attribute_15+ attribute_16+attribute_20+ attribute_21+attribute_22+ attribute_23+attribute_25+ attribute_32+ attribute_33+attribute_34+attribute_35+attribute_36+attribute_37+attribute_39+attribute_40+attribute_42+attribute_47+ attribute_48+ attribute_49+ attribute_50+ attribute_52+attribute_55+ attribute_56+attribute_59+attribute_60+attribute_63+attribute_64+attribute_65+attribute_66+attribute_67+attribute_68+attribute_69+attribute_70+attribute_71+attribute_72+attribute_73+attribute_74+attribute_75, data=train))
summary(base_lda)
base_lda
#NOT Working
lda.pred=suppressWarnings(predict(base_lda, newdata=test))
table(base_lda, test[,3])
mean(base_lda==test[,3])


#LDA resulted in numerous errors


