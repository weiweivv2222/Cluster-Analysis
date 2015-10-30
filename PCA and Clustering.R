#http://planspace.org/2013/02/03/pca-3d-visualization-and-clustering-in-r/
#load data
data(iris)
str(iris)
round(cor(iris[,-5]), 2)#Sepal.length is highly correlated with sepal.length and width

#Principal components analysis
pc=princomp(iris[,-5], cor=T, scores=T)
summary(pc)#According to Cumulative proportion we see that the first three 
#components properly descriptes the information of the whole data set
plot(pc, type="lines")
biplot(pc)
pc$loadings

#plot the scores along the first three principal components
library(rgl)
iris$Species = factor(iris$Species,levels = c("versicolor" ,"virginica","setosa"))
plot3d(pc$scores[,1:3], col=as.integer(iris$Species))

text3d(pc$scores[,1:3], texts=rownames(iris))
text3d(pc$loadings[,1:3], texts=rownames(pc$loadings), col="red")
coords = NULL
for (i in (1:nrow(pc$loadings))){
        coords = rbind(coords, rbind(c(0,0,0), pc$loadings[i, 1:3]))
}
lines3d(coords,col="red" , lwd=4)

#K-Mean clustering
set.seed(42)
cl = kmeans(iris[,-5],3)
iris$cluster = as.factor(cl$cluster)
plot3d(pc$scores[,1:3], col=iris$cluster, main="k-means clusters")
plot3d(pc$scores[,1:3], col=as.integer(iris$Species), main="actual species")
with(iris, table(cluster, Species))

#hierarchical clustering
dis=dist(iris[,-5], method="euclidean")
tree = hclust(dis, method="ward.D")
iris$hcluster = as.factor((cutree(tree, k=3) -2) %% 3 +1)
plot(tree)
rect.hclust(tree, k=3, border="red")
myplclust(tree, lab=rep(1:3,each=50), lab.col=rep(1:3,each=50))
with(iris, table(hcluster, Species))
