#Predictor selection for protein homology problem

setwd("C:/Users/Hume Dickie/Desktop/KDD Cup")
data <- read.table("bio_train.dat")
glm.fit <- glm(V3~.,data = data,family = "binomial")

summary(glm.fit)
#the 0-star p values tell us we can ignore V9, V10, V12, V14, V18-20, V24-30, V32, V37-40, V44-46, V54, V55, V72, V73. We also ignore V1 and V2 bc we know theyre irrelevant from docs

glm.fit <- glm(V3~.-V1-V2-V9-V10-V12-V14-V18-V19-V20-V24-V25-V26-V27-V28-V29-V30-V32-V37-V38-V39-V40-V44-V45-V46-V54-V55-V72-V73,data = data, family = "binomial")
# 0-star p-values: V35, V41, V60, V68

glm.fit <- glm(V3~.-V1-V2-V9-V10-V12-V14-V18-V19-V20-V24-V25-V26-V27-V28-V29-V30-V32-V35-V37-V38-V39-V40-V41-V44-V45-V46-V54-V55-V60-V68-V72-V73,data = data, family = "binomial")
#this model has p-values that all have at least a .

dotOrBetter <- c('V4','V5','V6','V7','V8','V11','V13','V15','V16','V17','V21','V22','V23','V31','V33','V34','V36','V42','V43','V47','V48','V49','V50','V51','V52','V53','V56','V57','V58','V59','V61','V62','V63','V64','V65','V66','V67','V69','V70','V71','V74','V75','V76','V77')
dotOrBetterFormula <- paste(dotOrBetter,sep = "",collapse = "+")
glm.dotOrBetter <- glm(paste(c('V3~',dotOrBetterFormula),sep="",collapse=""),data = data, family = "binomial")

oneStarOrBetter <- c('V4','V5','V6','V7','V8','V11','V13','V15','V16','V17','V21','V22','V23','V31','V33','V34','V43','V47','V48','V49','V50','V51','V52','V53','V56','V57','V58','V59','V61','V62','V63','V64','V65','V66','V67','V69','V70','V71','V74','V75','V76','V77')
oneStarOrBetterFormula <- paste(oneStarOrBetter,sep = "",collapse = "+")
glm.oneStarOrBetter <- glm(paste(c('V3~',oneStarOrBetterFormula),sep="",collapse=""),data = data, family = "binomial")

twoStarsOrBetter <- c('V5','V6','V8','V11','V13','V16','V17','V21','V22','V23','V31','V33','V43','V48','V49','V50','V51','V53','V56','V57','V58','V59','V61','V63','V64','V65','V66','V67','V69','V70','V71','V74','V75','V76','V77')
twoStarsOrBetterFormula <- paste(twoStarsOrBetter,sep = "",collapse = "+")
glm.twoStarsOrBetter <- glm(paste(c('V3~',twoStarsOrBetterFormula),sep="",collapse=""),data = data, family = "binomial")
#with this subset, V4, V47 and V52 fell below one star so they were excluded

threeStars <- c('V5','V6','V8','V11','V13','V16','V17','V21','V23','V31','V33','V43','V48','V51','V53','V56','V57','V58','V59','V61','V64','V65','V66','V67','V69','V70','V76')
threeStarFormula <- paste(threeStars,sep = "",collapse = "+")
glm.threeStars <- glm(paste(c('V3~',threeStarFormula),sep="",collapse=""),data = data, family = "binomial")
#V74 fell out of relevance

#looking for interaction effects
formula = c()
for ( i in 1:26) { #doesnt include last one because I dont want to look at the square
  for ( j in (i+1):27){
    formula <- paste(c(formula,paste(c(threeStars[i],'*',threeStars[j],' '))),sep = " ",collapse = "")
  }
}
formula <- gsub(" ","+",formula)
formula <- paste(formula,threeStarFormula,sep = "", collapse = "+")

glm.threeStarPlus2Interactions <- glm(paste('V3~',formula,sep="",collapse = ""),data = data, family = "binomial")
# all of these interactions have p values < 2e-16.... maybe that makes sense... maybe we should look at interactions of predictors we have excluded instead
#THIS TOOK A VERY LONG TIME
# COME BACK TO THIS. No interaction effects included atm

#-----------------------------------------
table(data$V3)[1]/dim(data)[1] #accuracy of null hypthesis = 99.11081% ; proportion of 1s in column V3 = 0.008891877%

#pearsons coefficient of skewness := (mean - mode)/SD
# mean = 0.9911081, mode = 0
# SD of binary random variable := sqrt(np(1-p)). So SD = sqrt(145751*0.9911081*0.008891877) = 35.8395879237

#pearsons coefficient of skewness = 0.9911081/35.8395879237 = 0.02765400378

#install.packages("sampling")

select <- rep(0,dim(data)[1])
select[data$V3 == 1] <- 1

require(sampling)
stratifiedSamples <- strata(data,'V3',c(1296,1296),"srswor") #srswor is sampling without replacement, srswr is sampling WITH replacement

subset1 <- data[stratifiedSamples$ID_unit,]
train <- subset1[c(1:972,1620:2592),]
test<- subset1[c(973:1619),] #equal parts 0s and 1s

glm.strat3star.fit <- glm(paste(c('V3~',threeStarFormula),sep="",collapse=""),data = train, family = "binomial")
summary(glm.strat3star.fit)
glm.stratAll.fit <- glm(V3~.,data = train, family = "binomial")
summary(glm.stratAll.fit)

glm.underSampledImportant.fit <- glm(V3~V8+V48+V58+V61+V66,data = train, family = "binomial")
summary(glm.underSampledImportant.fit)

predictionProbs <- predict(glm.strat3star.fit,test,type="response")
predictions <- rep(0,length(test))
predictions[predictionProbs>0.5] <- 1
predictions[predictionProbs<=0.5] <- 0

confMatrix <- table(predictions,test$V3)
(confMatrix[1,1]+confMatrix[2,2])/(dim(test)[1])
#All predictors 87.91345% on average (5 times), threeStar
#0.8887172, 0.877898, 0.8717156, 0.8763524, 0.8933539 = 88.16074% on average

#(0.8902628 + 0.8918083 + 0.8902628 + 0.8902628 + 0.8825348)/5 = 88.90263%
confMatrix








  #Predictor selection for protein homology problem
  
  setwd("C:/Users/Hume Dickie/Desktop/KDD Cup")
data <- read.table("bio_train.dat")
glm.fit <- glm(V3~.,data = data,family = "binomial")

summary(glm.fit)
#the 0-star p values tell us we can ignore V9, V10, V12, V14, V18-20, V24-30, V32, V37-40, V44-46, V54, V55, V72, V73. We also ignore V1 and V2 bc we know theyre irrelevant from docs

glm.fit <- glm(V3~.-V1-V2-V9-V10-V12-V14-V18-V19-V20-V24-V25-V26-V27-V28-V29-V30-V32-V37-V38-V39-V40-V44-V45-V46-V54-V55-V72-V73,data = data, family = "binomial")
# 0-star p-values: V35, V41, V60, V68

glm.fit <- glm(V3~.-V1-V2-V9-V10-V12-V14-V18-V19-V20-V24-V25-V26-V27-V28-V29-V30-V32-V35-V37-V38-V39-V40-V41-V44-V45-V46-V54-V55-V60-V68-V72-V73,data = data, family = "binomial")
#this model has p-values that all have at least a .

dotOrBetter <- c('V4','V5','V6','V7','V8','V11','V13','V15','V16','V17','V21','V22','V23','V31','V33','V34','V36','V42','V43','V47','V48','V49','V50','V51','V52','V53','V56','V57','V58','V59','V61','V62','V63','V64','V65','V66','V67','V69','V70','V71','V74','V75','V76','V77')
dotOrBetterFormula <- paste(dotOrBetter,sep = "",collapse = "+")
glm.dotOrBetter <- glm(paste(c('V3~',dotOrBetterFormula),sep="",collapse=""),data = data, family = "binomial")

oneStarOrBetter <- c('V4','V5','V6','V7','V8','V11','V13','V15','V16','V17','V21','V22','V23','V31','V33','V34','V43','V47','V48','V49','V50','V51','V52','V53','V56','V57','V58','V59','V61','V62','V63','V64','V65','V66','V67','V69','V70','V71','V74','V75','V76','V77')
oneStarOrBetterFormula <- paste(oneStarOrBetter,sep = "",collapse = "+")
glm.oneStarOrBetter <- glm(paste(c('V3~',oneStarOrBetterFormula),sep="",collapse=""),data = data, family = "binomial")

twoStarsOrBetter <- c('V5','V6','V8','V11','V13','V16','V17','V21','V22','V23','V31','V33','V43','V48','V49','V50','V51','V53','V56','V57','V58','V59','V61','V63','V64','V65','V66','V67','V69','V70','V71','V74','V75','V76','V77')
twoStarsOrBetterFormula <- paste(twoStarsOrBetter,sep = "",collapse = "+")
glm.twoStarsOrBetter <- glm(paste(c('V3~',twoStarsOrBetterFormula),sep="",collapse=""),data = data, family = "binomial")
#with this subset, V4, V47 and V52 fell below one star so they were excluded

threeStars <- c('V5','V6','V8','V11','V13','V16','V17','V21','V23','V31','V33','V43','V48','V51','V53','V56','V57','V58','V59','V61','V64','V65','V66','V67','V69','V70','V76')
threeStarFormula <- paste(threeStars,sep = "",collapse = "+")
glm.threeStars <- glm(paste(c('V3~',threeStarFormula),sep="",collapse=""),data = data, family = "binomial")
#V74 fell out of relevance

#looking for interaction effects
formula = c()
for ( i in 1:26) { #doesnt include last one because I dont want to look at the square
  for ( j in (i+1):27){
    formula <- paste(c(formula,paste(c(threeStars[i],'*',threeStars[j],' '))),sep = " ",collapse = "")
  }
}
formula <- gsub(" ","+",formula)
formula <- paste(formula,threeStarFormula,sep = "", collapse = "+")

glm.threeStarPlus2Interactions <- glm(paste('V3~',formula,sep="",collapse = ""),data = data, family = "binomial")
# all of these interactions have p values < 2e-16.... maybe that makes sense... maybe we should look at interactions of predictors we have excluded instead
#THIS TOOK A VERY LONG TIME
# COME BACK TO THIS. No interaction effects included atm

#-----------------------------------------
table(data$V3)[1]/dim(data)[1] #accuracy of null hypthesis = 99.11081% ; proportion of 1s in column V3 = 0.008891877%

#pearsons coefficient of skewness := (mean - mode)/SD
# mean = 0.9911081, mode = 0
# SD of binary random variable := sqrt(np(1-p)). So SD = sqrt(145751*0.9911081*0.008891877) = 35.8395879237

#pearsons coefficient of skewness = 0.9911081/35.8395879237 = 0.02765400378

#install.packages("sampling")

select <- rep(0,dim(data)[1])
select[data$V3 == 1] <- 1
positiveExamples <- data[select == 1,]
negativeExamples <- data[select == 0,]

require(sampling)
stratifiedSamples <- strata(data,'V3',c(1296,1296),"srswor") #srswor is sampling without replacement, srswr is sampling WITH replacement
#I cant get strata to work the way I want it to. The way it is now selects rows the way I want, but drops all the predictors except V3! And i include the others
#it tries to use them to create additional strata which we dont want


#glm.strat3star.fit <- glm(paste(c('V3~',threeStarFormula),sep="",collapse=""),data = stratifiedSamples, family = "binomial")






#Predictor selection for protein homology problem

setwd("C:/Users/Hume Dickie/Desktop/KDD Cup")
data <- read.table("bio_train.dat")
glm.fit <- glm(V3~.,data = data,family = "binomial")

summary(glm.fit)
#the 0-star p values tell us we can ignore V9, V10, V12, V14, V18-20, V24-30, V32, V37-40, V44-46, V54, V55, V72, V73. We also ignore V1 and V2 bc we know theyre irrelevant from docs

glm.fit <- glm(V3~.-V1-V2-V9-V10-V12-V14-V18-V19-V20-V24-V25-V26-V27-V28-V29-V30-V32-V37-V38-V39-V40-V44-V45-V46-V54-V55-V72-V73,data = data, family = "binomial")
# 0-star p-values: V35, V41, V60, V68

glm.fit <- glm(V3~.-V1-V2-V9-V10-V12-V14-V18-V19-V20-V24-V25-V26-V27-V28-V29-V30-V32-V35-V37-V38-V39-V40-V41-V44-V45-V46-V54-V55-V60-V68-V72-V73,data = data, family = "binomial")
#this model has p-values that all have at least a .

dotOrBetter <- c('V4','V5','V6','V7','V8','V11','V13','V15','V16','V17','V21','V22','V23','V31','V33','V34','V36','V42','V43','V47','V48','V49','V50','V51','V52','V53','V56','V57','V58','V59','V61','V62','V63','V64','V65','V66','V67','V69','V70','V71','V74','V75','V76','V77')
dotOrBetterFormula <- paste(dotOrBetter,sep = "",collapse = "+")
glm.dotOrBetter <- glm(paste(c('V3~',dotOrBetterFormula),sep="",collapse=""),data = data, family = "binomial")

oneStarOrBetter <- c('V4','V5','V6','V7','V8','V11','V13','V15','V16','V17','V21','V22','V23','V31','V33','V34','V43','V47','V48','V49','V50','V51','V52','V53','V56','V57','V58','V59','V61','V62','V63','V64','V65','V66','V67','V69','V70','V71','V74','V75','V76','V77')
oneStarOrBetterFormula <- paste(oneStarOrBetter,sep = "",collapse = "+")
glm.oneStarOrBetter <- glm(paste(c('V3~',oneStarOrBetterFormula),sep="",collapse=""),data = data, family = "binomial")

twoStarsOrBetter <- c('V5','V6','V8','V11','V13','V16','V17','V21','V22','V23','V31','V33','V43','V48','V49','V50','V51','V53','V56','V57','V58','V59','V61','V63','V64','V65','V66','V67','V69','V70','V71','V74','V75','V76','V77')
twoStarsOrBetterFormula <- paste(twoStarsOrBetter,sep = "",collapse = "+")
glm.twoStarsOrBetter <- glm(paste(c('V3~',twoStarsOrBetterFormula),sep="",collapse=""),data = data, family = "binomial")
#with this subset, V4, V47 and V52 fell below one star so they were excluded

threeStars <- c('V5','V6','V8','V11','V13','V16','V17','V21','V23','V31','V33','V43','V48','V51','V53','V56','V57','V58','V59','V61','V64','V65','V66','V67','V69','V70','V76')
threeStarFormula <- paste(threeStars,sep = "",collapse = "+")
glm.threeStars <- glm(paste(c('V3~',threeStarFormula),sep="",collapse=""),data = data, family = "binomial")
#V74 fell out of relevance

#looking for interaction effects
formula = c()
for ( i in 1:26) { #doesnt include last one because I dont want to look at the square
  for ( j in (i+1):27){
    formula <- paste(c(formula,paste(c(threeStars[i],'*',threeStars[j],' '))),sep = " ",collapse = "")
  }
}
formula <- gsub(" ","+",formula)
formula <- paste(formula,threeStarFormula,sep = "", collapse = "+")

glm.threeStarPlus2Interactions <- glm(paste('V3~',formula,sep="",collapse = ""),data = data, family = "binomial")
# all of these interactions have p values < 2e-16.... maybe that makes sense... maybe we should look at interactions of predictors we have excluded instead
#THIS TOOK A VERY LONG TIME
# COME BACK TO THIS. No interaction effects included atm

#-----------------------------------------
table(data$V3)[1]/dim(data)[1] #accuracy of null hypthesis = 99.11081% ; proportion of 1s in column V3 = 0.008891877%

#pearsons coefficient of skewness := (mean - mode)/SD
# mean = 0.9911081, mode = 0
# SD of binary random variable := sqrt(np(1-p)). So SD = sqrt(145751*0.9911081*0.008891877) = 35.8395879237

#pearsons coefficient of skewness = 0.9911081/35.8395879237 = 0.02765400378

#install.packages("sampling")

select <- rep(0,dim(data)[1])
select[data$V3 == 1] <- 1

require(sampling)
stratifiedSamples <- strata(data,'V3',c(1296,1296),"srswor") #srswor is sampling without replacement, srswr is sampling WITH replacement

subset1 <- data[stratifiedSamples$ID_unit,]
train <- subset1[c(1:972,1620:2592),]
test<- subset1[c(973:1619),] #equal parts 0s and 1s

glm.strat3star.fit <- glm(paste(c('V3~',threeStarFormula),sep="",collapse=""),data = train, family = "binomial")
summary(glm.strat3star.fit)
glm.stratAll.fit <- glm(V3~.,data = train, family = "binomial")
summary(glm.stratAll.fit)

glm.underSampledImportant.fit <- glm(V3~V8+V48+V58+V61+V66,data = train, family = "binomial")
summary(glm.underSampledImportant.fit)

predictionProbs <- predict(glm.underSampledImportant.fit,test,type="response")
predictions <- rep(0,length(test))
threshhold = 0.99
predictions[predictionProbs>threshhold] <- 1
predictions[predictionProbs<=threshhold] <- 0

confMatrix <- table(predictions,test$V3)
(confMatrix[1,1]+confMatrix[2,2])/(dim(test)[1])
#All predictors 87.91345% on average (5 times), threeStar = 88.16074% on average, simple5 = 88.90263%


#results <- rep (0,50)
#for ( i in 1:999) {
  stratifiedSamples <- strata(data,'V3',c(1296,1296),"srswor")
  subset1 <- data[stratifiedSamples$ID_unit,]
  train <- subset1[c(1:972,1620:2592),]
  test<- subset1[c(973:1619),] #equal parts 0s and 1s
  #glm.stratAll.fit <- glm(V3~.,data = train, family = "binomial")
  glm.underSampledImportant.fit <- glm(V3~V8+V48+V58+V61+V66,data = train, family = "binomial")
  predictionProbs <- predict(glm.glm.underSampledImportant.fit.fit,test,type="response")
  predictions <- rep(0,length(test))
  
  threshhold = 0.453
  #threshhold = i/1000
  
  predictions[predictionProbs>threshhold] <- 1
  predictions[predictionProbs<=threshhold] <- 0
  
  avgError = 0
  for (j in 1:10) {
    confMatrix <- table(predictions,test$V3)
    error<-(confMatrix[1,1]+confMatrix[2,2])/(dim(test)[1])
    avgError<-avgError + error
  }
  avgError = avgError/10
  
  results[i] <- avgError
#}

plot(1:999,results)
identify(1:999,results)
#best threshold for 5 variavle model is 0.453 -> 85.63%
# best threshold for all variables is 0.373 -> 85.94% accuracy