rm(list=ls())
data(Glass, package="mlbench")
training = Glass[c(1:50, 91:146), -10]
trainingType = factor(Glass[c(1:50, 91:146), 10])
testing=Glass[51:90 , -10]
testingType = factor(Glass[51:90, 10])

library(class)
library(stats)

training <- training[complete.cases(training),]
testing <- testing[complete.cases(testing),]
train.pca <- prcomp(training, center = TRUE, scale. = TRUE)
testing.pca <- predict(train.pca, newdata = testing)

prediction = knn(training, testing, trainingType, k=3)
prediction_pca = knn(train.pca$x[,1:2], testing.pca[,1:2], trainingType, k = 3)

cat("Confusion matrix:\n\n")
xtab1 = table(prediction, testingType)
print(xtab1)
accuracy = sum(prediction == testingType)/length(testingType)
cat(paste("\nAccuracy:\t", format(accuracy, digits=3), "\n",sep=" "))

cat("\nConfusion matrix PCA:\n\n")
## Confusion matrix PCA:
xtab = table(prediction_pca, testingType)
print(xtab)

cat("\nEvaluation PCA:\n\n")
accuracy = sum(prediction_pca == testingType)/length(testingType)
cat(paste("Accuracy PCA:\t", format(accuracy, digits=3), "\n",sep=" "))


######################################## ???????
recall = xtab1[1,1]/sum(xtab[1,])
# Ως θετική κλάση η δευτερη
recall = xtab1[2,2]/sum(xtab[1,])

### HOW MANY PCA
data_norm <- scale(training, scale=F)
S <- cov(data_norm)
udv <- svd(S)
print(cumsum(udv$d)/sum(udv$d))
plot(cumsum(udv$d)/sum(udv$d))
