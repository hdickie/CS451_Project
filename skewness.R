# setwd("D:\\USF Files\\CS 451\\Project")

train.data <- read.delim("bio_train.dat", header = FALSE)

# data with V3 = 0
train.data0 <- train.data[train.data$V3 == 0,]
# data with V3 = 1
train.data1 <- train.data[train.data$V3 == 1,]

#Over-sampling
set.seed(1)
144455-1296
# sampling with replacement of data with V3 = 1
data1.oversample <- train.data1[sample(1:nrow(train.data1), 143159, replace = TRUE),]
data1.oversample <- rbind(data1.oversample, train.data1)
# over-sampled data frame
train.oversample <- rbind(data1.oversample, train.data0)

oversample.glm <- glm (V3 ~., data = train.oversample, family = "binomial")
summary(oversample.glm)

oversample.glm2 <- glm(V3 ~ V4+V5+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V21+V22+V23+V24+V25+V26+V27+V30+V31+V32+
                         V33+V34+V35+V36+V37+V38+V39+V40+V41+V44+V48+V49+V50+V51+V53+V54+V55+V56+V57+V59+V60+V61+
                         V64+V65+V66+V67+V68+V69+V70+V71+V72+V74+V75+V76, data=train.oversample, family="binomial")
summary(oversample.glm2)

oversample.glm3 <- glm(V3 ~ V4+V5+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V21+V22+V23+V24+V25+V26+V27+V30+V31+V32+
                         V33+V34+V35+V36+V37+V38+V39+V40+V41+V44+V48+V49+V50+V51+V53+V54+V56+V57+V59+V60+V61+
                         V64+V65+V66+V67+V68+V69+V70+V71+V72+V74+V75+V76, data=train.oversample, family="binomial")
summary(oversample.glm3)

oversample.glm4 <- glm(V3 ~ V4+V5+V7+V8+V9+V10+V11+V13+V14+V16+V17+V21+V22+V23+V24+V26+V30+V31+V32+
                         V33+V34+V35+V36+V37+V38+V39+V40+V41+V48+V49+V50+V51+V53+V54+V56+V61+
                         V64+V65+V66+V67+V68+V69+V70+V71+V72+V74+V75+V76, data=train.oversample, family="binomial")
summary(oversample.glm4)

predict.glm4 <- predict(oversample.glm4,train.oversample,type="response")
predictions <- rep(0,length(train.oversample))
predictions <- 
  
  
  
  
  #Under-sampling
  # sampling without replacement of data with V3 = 0
  data0.undersample <- train.data0[sample(1:nrow(train.data0), 1296, replace = FALSE),]
# under-sampled data frame
train.undersample <- rbind(data0.undersample, train.data1)

data0.strat <- strata(train.data0, stratanames = as.vector(names(train.data0)), 1296)
data0.strat <- strata
class(names(train.data0))

require(boot)
oversample.glm <- glm(V3 ~ ., data = train.oversample, family = "binomial")
summary(oversample.glm)
# cv.glm(train.oversample,oversample.glm)

undersample.glm <- glm(V3~ ., data = train.undersample, family = "binomial")
summary(undersample.glm)
cv.glm(train.undersample,undsample.glm)

