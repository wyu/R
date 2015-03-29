library(tsne)
library(lattice)
library(rgl)
library(FactoMineR)
library(vegan)

mydata = read.table("/tmp/adult6.csv", header=TRUE, sep=",")

# initialize counter to 0
x <- 0
epc <- function(x) {
  x <<- x + 1
  filename <- paste("/tmp/plot", x, "jpg", sep=".")
  cat("> Plotting TSNE to ", filename, " ")
  
  # plot to d:\\plot.x.jpg file of 2400x1800 dimension
  jpeg(filename, width=2400, height=1800)
  
  plot(x, t='n', main="T-SNE")
  text(x, labels=rownames(mydata))
  dev.off()
}

test=mydata; test$cohort=NULL;
# run tsne (maximum iterations:500, callback every 100 epochs, target dimension k=2)
tsne_data <- tsne(test, k=3, epoch_callback=epc, max_iter=1000, epoch=200)

colnames(tsne_data)=c('V1','V2', 'V3');
data=cbind(mydata, tsne_data);
data3=cbind(mydata, tsne_data);

# plot the output
xyplot(V1~V2, group=cohort, data)
xyplot(V1~V2|cohort, data)

d3a = subset(data3, cohort=='cohort_a')
d3b = subset(data3, cohort=='cohort_b')
d3c = subset(data3, cohort=='cohort_c')
d3d = subset(data3, cohort=='cohort_d')
scatterplot3d(d3a$V1,d3a$V2,d3a$V3, main="3-D Scatterplot of Cohort A")
scatterplot3d(d3c$V1,d3c$V2,d3c$V3, main="3-D Scatterplot of Cohort C")
scatterplot3d(d3d$V1,d3d$V2,d3d$V3, main="3-D Scatterplot of Cohort C")

#http://stats.stackexchange.com/questions/123040/whats-wrong-with-t-sne-vs-pca-for-dimensional-reduction-using-r
km <- kmeans(test,5,10000)
# run principle component analysis
pc<-prcomp(test)
plot(pc$x[,1], pc$x[,2],col=km$cluster,pch=16)
# plot spiderweb and connect outliners with dotted line
pc<-cbind(pc$x[,1], pc$x[,2])
ordispider(pc, factor(km$cluster), label = TRUE)
ordihull(pc, factor(km$cluster), lty = "dotted")

plot3d(d3a[,c('V1','V2','V3')], main="T-SNE", col = km$cluster,type="s",size=1,scale=0.2)
