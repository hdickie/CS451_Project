###unsupervised
#PCA - removed first 3 columns
setwd("C:/Users/Hume Dickie/Desktop/KDD Cup")
data <- read.table("bio_train.dat")

predictors <- data[,4:77]

pr.out <- prcomp(predictors, scale = FALSE) # I think scaling should be false bc hclust did not
#work well with the scaled data

pr.var <- pr.out$sdev^2
pve <- pr.var/sum(pr.var)
plot(pve , xlab="Principal Component ", ylab="Proportion of Variance Explained ", ylim=c(0,1),type="b")

plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type="b")

summary(pr.out)
#only the first 29 account for more than 1% of variance each. First 11 more than 2% each
#first 11 cum = 62.6%, first 29 -> 88%, first 32 = 90%
#only the first 3 look super valuable. Maaaaaybe the 4th one.

#WE SHOULD TRY TO PROJECT ALL DATA ONtO THESE 3/4 VECTORS AND THEN TRY ALL OUR MODELS AGAIN ON THAT



#K-Means
set.seed(1001)
#km.out <- kmeans(predictors,3,nstart = 50) This data set is so huge i feel like this is not a reasonable option
#hierarchical tho

#GOAL: Do clustering on normalized and original data, w/ average and complete linkage,
#p.415 has relevant stuff. Clusters should be done multiple times and are not robust to removal
#of subsets of observations

#LMAO data set is way too big. hclust on undersampled then
attach(data)
require(sampling)
stratifiedSamples <- strata(data,'V3',c(1296,1296),"srswor") #srswor is sampling without replacement, srswr is sampling WITH replacement

subset1 <- data[stratifiedSamples$ID_unit,] #all 1s and an equal number of 0s

par(mfrow=c(1,2))

hc.complete <- hclust(dist(subset1), method = "complete")
hc.average <- hclust(dist(subset1),method = "average")
#unscaled
plot(hc.complete ,main="Complete Linkage ", xlab="", sub="", cex=.9)
plot(hc.average , main="Average Linkage", xlab="", sub="", cex=.9)

#scaled
hc.s_comp <-hclust(dist(scale(subset1)),method = "complete")
hc.s_avg <-hclust(dist(scale(subset1)),method = "average")
plot(hc.s_comp, main="Scaled Complete Linkage")
plot(hc.s_avg, main="Scaled Average Linkage")

unscaled_complete <- cutree(hc.complete,9)
v3againstCluster <- table(unscaled_complete,subset1$V3)
v3againstCluster #not very useful...

proportionOfOnes <- v3againstCluster[,2]/apply(v3againstCluster,1,sum)
proportionOfOnes





#dimensional reduction via PCA

predictors <- data[,4:77]
pr.out <- prcomp(predictors, scale = FALSE)
transformedData <- as.matrix(predictors)%*%as.matrix(pr.out$rotation[,1:3])
dimReduced <- data.frame(transformedData,data$V3)

stratifiedSamples <- strata(dimReduced,'data.V3',c(1296,1296),"srswor") #srswor is sampling without replacement, srswr is sampling WITH replacement

subset1 <- dimReduced[stratifiedSamples$ID_unit,]
train <- subset1[c(1:972,1620:2592),]
test<- subset1[c(973:1619),] #equal parts 0s and 1s



#logistic
reducedLogit.fit <- glm(data.V3~PC1 + PC2 + PC3,data = train,family = "binomial")


predictionProbs <- predict(reducedLogit.fit,test,type="response")
predictions <- rep(0,dim(test)[1])
predictions[predictionProbs>0.5] <- 1
predictions[predictionProbs<=0.5] <- 0

length(predictionProbs)
dim(test)

confMatrix <- table(predictions,test$data.V3)
(confMatrix[1,1]+confMatrix[2,2])/(dim(test)[1])
confMatrix
#accuracy of 73.7% Still terrible

#cluster
require(sampling)
predictors <- data[,4:77]
pr.out <- prcomp(predictors, scale = FALSE)
transformedData <- as.matrix(predictors)%*%as.matrix(pr.out$rotation[,1:3])
dimReduced <- data.frame(transformedData,data$V3)

set.seed(5)
stratifiedSamples <- strata(dimReduced,'data.V3',c(1296,1296),"srswor")

subset1 <- dimReduced[stratifiedSamples$ID_unit,] #all 1s and an equal number of 0s

par(mfrow=c(1,2))

hc.complete <- hclust(dist(subset1), method = "complete")
hc.average <- hclust(dist(subset1),method = "average")
#unscaled
plot(hc.complete ,main="Complete Linkage ", xlab="", sub="", cex=.9)
plot(hc.average , main="Average Linkage", xlab="", sub="", cex=.9) #this crashes RStudio every time

#lets do only complete linkage then
unscaled_complete <- cutree(hc.complete,7)
v3againstCluster <- table(unscaled_complete,subset1$data.V3)
v3againstCluster #extremely unbalanced

proportionOfOnes <- v3againstCluster[,2]/apply(v3againstCluster,1,sum)
proportionOfOnes
