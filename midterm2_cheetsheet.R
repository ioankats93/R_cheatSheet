rm(list=ls())
data(Glass, package="mlbench")
training = Glass[c(1:50, 91:146), -10]
trainingType = factor(Glass[c(1:50, 91:146), 10])
testing=Glass[51:90, -10]
testingType = factor(Glass[51:90, 10])

library(class)
library(stats)

training <- training[complete.cases(training),]
testing <- testing[complete.cases(testing),]
train.pca <- prcomp(training, center = TRUE, scale. = TRUE)
testing.pca <- predict(train.pca, newdata = testing)

# Ποσοστό  πληροφορίας του αρχικού dataset που ενσωματώνει το PC1
train.pca <- prcomp(training, center = TRUE, scale. = TRUE)
summary(train.pca)

# Κρατήστε  4 πρώτα Principal Compontens .Ποιό το ποσοστό της απώλεια πληροφορίας
# 1 - PC4(Cumulative Proportion)

# Εκπαιδεύστε έναο μοντέλο με τη χρήση KKN ( k =3),με στόχο να κατατάξετε
#τα δεδομένα ελέγχου: Ποιά είναι η τιμή της μετρικής Accuracy για τα δεδομένα ελέγχου.
prediction = knn(training, testing, trainingType, k=3)
prediction_pca = knn(train.pca$x[,1:2], testing.pca[,1:2], trainingType, k = 3)

cat("Confusion matrix:\n\n")
xtab = table(prediction, testingType)
print(xtab)
accuracy = sum(prediction == testingType)/length(testingType)
precision = xtab[1,1]/sum(xtab[,1])
recall = xtab[1,1]/sum(xtab[1,])
f = 2 * (precision * recall) / (precision + recall)
cat(paste("Accuracy:\t", format(accuracy, digits=3), "\n",sep=" "))
cat(paste("Precision:\t", format(precision, digits=3), "\n",sep=" "))
cat(paste("Recall:\t\t", format(recall, digits=3), "\n",sep=" "))
cat(paste("F-measure:\t", format(f, digits=3), "\n",sep=" "))

cat("\nConfusion matrix PCA:\n\n")
xtab = table(prediction_pca, testingType)
print(xtab)
## Confusion matrix PCA:
cat("\nEvaluation PCA:\n\n")
accuracy = sum(prediction_pca == testingType)/length(testingType)
precision = xtab[1,1]/sum(xtab[,1])
recall = xtab[1,1]/sum(xtab[2,])
f = 2 * (precision * recall) / (precision + recall)
cat(paste("Accuracy:\t", format(accuracy, digits=3), "\n",sep=" "))
cat(paste("Precision:\t", format(precision, digits=3), "\n",sep=" "))
cat(paste("Recall:\t\t", format(recall, digits=3), "\n",sep=" "))
cat(paste("F-measure:\t", format(f, digits=3), "\n",sep=" "))

# Εφαρμόστε PCA στα δεδομένα εκπαίδευσης.Εκπαιδεύστε ένα μοντέλο με τη χρήση
# kNN κ=3 για να κατατάξετε τα δεδομένα ελέγχου. Ποιός ο βέλτιστος αριθμός των
# Principal Components που πρέπει να κρατήσουμε βάση μετρικής
data_norm <- scale(training, scale=F)
S <- cov(data_norm)
udv <- svd(S)
print(cumsum(udv$d)/sum(udv$d))
plot(cumsum(udv$d)/sum(udv$d))

########################################################################
# Ποιά η ευκλείδια απόσταση μεταξύ δύο αντικειμένων
# setosa      4.9 , 3.0, 1.4, 0.2
# versicolor  5.6 , 2.5, 3.9 1.1

setosa <- c(4.9, 3, 1.4, 0.2)
versicolor <- c(5.6, 2.5, 3.9, 1.1)
x <-dist (rbind(setosa, versicolor))
cat("\nEucleidian Distance",x)
