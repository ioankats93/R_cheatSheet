#Ομαδοποιήστε τα δεδομένα με τονK-means με
#αρχικά κέντρα τα(-4, 10), (0,0) και (4,10)
rm(list=ls())
setwd("~/Desktop")
getwd()
kdata <- read.table("midterm.data", sep=",", header = T)
plot(kdata, pch=15)
# create the clustering
start.x <- c(-4, 0, 4)
start.y <- c(10, 0, 10)
clust <- kmeans(kdata, centers=cbind(start.x, start.y))
#clust = kmeans(kdata, centers =3) # για 3 random
# ποιά η τιμή της μετρικής cohesion
cohesion = clust$tot.withinss
# ποιά η τιμή της μετρικής seperation
separation = clust$betweenss
plot(kdata, col = clust$cluster, pch = 15)
text(kdata, labels = row.names(kdata), pos = 2)
points(clust$centers, col = 1:length(clust$centers), pch = "+", cex = 2)

#pick number of clusters
#SSE <- (nrow(kdata) - 1) * sum(apply(kdata, 2, var))
#for(i in 2:10) {
#SSE[i] <- kmeans(kdata, centers = i)$tot.withinss
#}
#plot(1:10, SSE, type="b", xlab="Number of Clusters", ylab="SSE")

# Ομαδοποιήστε τα δεδομένα χρησιμοποιόντας τον K-means.
# Ποιά η τιμή της μετρικής silhouette?
library(cluster)
silhouette = silhouette(clust$cluster, dist(kdata))
plot(silhouette) # Βγάζουμε Μ.Ο

#Ομαδοποιήστε τα δεδομένα με τον K-means με
#αρχικά κέντρα τα(-2, 0), (2,0) και (0,10)
#Τι ισχύει για τα μοντέλα που κατασκευάσατε ?
#συγκρίνω το clust με το clust1 διαλέγω το μεγαλυτερο
start.x <- c(-2, 2, 0)
start.y <- c(0, 0, 10)
clust1 <- kmeans(kdata, centers=cbind(start.x, start.y))
