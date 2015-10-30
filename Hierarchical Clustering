#Hierarchical clustering
set.seed(1234)
#par(mar=c(0,0,0,0))
x=rnorm(12, mean=rep(1:3,each=4),sd=0.2)
y=rnorm(12, mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))

#cluster
#data frame object
dataFrame = data.frame(x=x,y=y)
# matrix object
#dataFrame=as.matrix(x,y)
distxy=dist(dataFrame)
hClustering=hclust(distxy)
plot(hClustering,hang=3)

#pretty dendrograms
dataFrame = data.frame(x=x,y=y)
distxy=dist(dataFrame)
hClustering=hclust(distxy)
myplclust(hClustering, lab=rep(1:3,each=4), lab.col=rep(1:3,each=4))
