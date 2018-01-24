rm(list=ls())
setwd("~/Desktop")
getwd()
# Διαχωρείστε τα δεδομένα με βάση του παρακάτω τρόπους
# Hierarchical clustering me single linkage (Αριθμός Ομάδων = 2)
# Hierarchical clustering me complete linkage (Αριθμός ομάδων 2)
# Εφαρμόστε DBSCAN minPts = 5 για τιμες του eps 0.75, 1.00, 1.25, 1.50
# Εφαρμόστε k-means (Αριθμός ομάδων 2)
# Ποιοί αλγόριθμοι μπορούν να διαχωρίσουν επιτυχώς τα δεδομένα
dataset <- read.table("midterm_14.data", sep=",", header = T)

# Αυτα είναι απο το τεστ
target <- dataset[,3]
dataset <- dataset[, 1:2]

# Hierarchical single
hc <- hclust(dist(dataset), method = "single")
#Hierarchical clustering me complete linkage
hc1 <- hclust(dist(dataset), method = "complete")
# Dbscan minPts = 5 different eps
clustering = dbscan(dataset, eps = 0.75, minPts = 5)
clustering1 = dbscan(dataset, eps = 1, minPts = 5)
clustering2 = dbscan(dataset, eps = 1.25, minPts = 5)
clustering3 = dbscan(dataset, eps = 1.5, minPts = 5)

demo <- data.frame(dataset)
kmeansClust <- kmeans(demo, 2)
plot(demo, col = kmeansClust$cluster + 1 , main = "kmeans")
plot(demo, col = clustering$cluster + 1 , main = "DBSCAN eps = 0.75")
plot(demo, col = clustering1$cluster + 1 , main = "DBSCAN eps = 1")
plot(demo, col = clustering2$cluster + 1 , main = "DBSCAN eps = 1.25")
plot(demo, col = clustering3$cluster + 1 , main = "DBSCAN eps = 1.5")

#Σετ μονοδιαστατων σημείων  {6, 12, 18, 24, 30, 42, 48}. Κάνε DBSCAN
# με Eps = 10 και MinPts = 1.Πόσα clusters δημιουργήθηκαν
db <- c(6, 12, 18, 24, 30, 42, 48)
cl <- dbscan(matrix(db), eps=10, minPts=1)
print(cl)
