# COMP473 Assignment 2
# Question 3 - Clustering

# Hierarchical Clustering using Euclidean distance measure
library(ISLR)
nci.data=NCI60$data
X=scale(t(nci.data))
P=X%*%prcomp(X)$rotation
hc=hclust(dist(X), method = "complete")

table(cutree(hc,3))
table(cutree(hc,4))
table(cutree(hc,5))
table(cutree(hc,6))

par(mfrow=c(1,4))
plot(P[,c(1,2)],col=cutree(hc,3),pch=19,main="K=3")
plot(P[,c(1,2)],col=cutree(hc,4),pch=19,main="K=4")
plot(P[,c(1,2)],col=cutree(hc,5),pch=19,main="K=5")
plot(P[,c(1,2)],col=cutree(hc,6),pch=19,main="K=6")

#Hierarchical Clustering using correlation-based distance measure
library(ISLR)
nci.data=NCI60$data
X=scale(t(nci.data))
P=X%*%prcomp(X)$rotation
dd=as.dist(1-cor(t(X)))
hcc=hclust(dd, method = "complete")

table(cutree(hcc,3))
table(cutree(hcc,4))
table(cutree(hcc,5))
table(cutree(hcc,6))

par(mfrow=c(1,4))
plot(P[,c(1,2)],col=cutree(hcc,3),pch=19, main="K=3")
plot(P[,c(1,2)],col=cutree(hcc,4),pch=19, main="K=4")
plot(P[,c(1,2)],col=cutree(hcc,5),pch=19, main="K=5")
plot(P[,c(1,2)],col=cutree(hcc,6),pch=19, main="K=6")

#K-mean Clustering

library(ISLR)
nci.data=NCI60$data
X=scale(t(nci.data))
P=X%*%prcomp(X)$rotation

km3= kmeans(X,3,nstart =20 )
table(km3$cluster)

km4= kmeans(X,4,nstart =20 )
table(km4$cluster)

km5= kmeans(X,5,nstart =20 )
table(km5$cluster)

km6= kmeans(X,6,nstart =20 )
table(km6$cluster)

par(mfrow=c(1,4))
plot(P[,c(1,2)],col=km3$cluster,pch=19, main="K=3")
plot(P[,c(1,2)],col=km4$cluster,pch=19, main="K=4")
plot(P[,c(1,2)],col=km5$cluster,pch=19, main="K=5")
plot(P[,c(1,2)],col=km6$cluster,pch=19, main="K=6")
