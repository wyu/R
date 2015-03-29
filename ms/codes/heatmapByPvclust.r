computeOverallClusterL1NormCluster = function(d){
  anal            = list();
  anal$dist       = dist2(d,diagonal=0);
  anal$dendrogram = as.dendrogram(hclust(as.dist(anal$dist)))
  anal$leafOrder  = order.dendrogram(anal$dendrogram)
  anal$legend     = list(top=list(fun=dendrogramGrob,args=list(x=anal$dendrogram, side="top")))
  anal$levelPlot  = levelplot(anal$dist[anal$leafOrder, anal$leafOrder],scales=list(x=list(rot=90)), xlab="",ylab="", legend=anal$legend)
  return(anal);
}

computerOverallClusterCorrelationCluster = function(d){
  anal            = list();
  anal$dist       = (1-cor(d))/2;
  anal$distMatrix = as.dist(anal$dist);
  anal$dendrogram = as.dendrogram(hclust(as.dist(anal$distMatrix)))
  anal$leafOrder  = order.dendrogram(anal$dendrogram)
  anal$legend     = list(top=list(fun=dendrogramGrob,args=list(x=anal$dendrogram, side="top")))
  anal$levelPlot  = levelplot(anal$dist [anal$leafOrder, anal$leafOrder],scales=list(x=list(rot=90)), xlab="",ylab="", legend=anal$legend)
  return(anal);
}

computerOverallClusterPVCorrelationCluster = function(d){
  anal            = list();
  anal$dist       = (1-cor(d))/2;  #I DON'T THINK THIS IS RIGHT - HOW TO GET THE DISTANCE USED OUT OF HCLUST????
  anal$pvclust    = pvclust(d,method.hclust="average",method.dist="correlation",nboot=100);
  anal$dendrogram = as.dendrogram(anal$pvclust$hclust)
  anal$leafOrder  = order.dendrogram(anal$dendrogram)
  anal$legend     = list(top=list(fun=dendrogramGrob,args=list(x=anal$dendrogram, side="top")))
  anal$levelPlot  = levelplot(anal$dist[anal$leafOrder, anal$leafOrder],scales=list(x=list(rot=90)), xlab="",ylab="", legend=legend)
  return(anal);
}

source("http://bioconductor.org/biocLite.R")
biocLite("genefilter")

library(genefilter)

analysis = list()
analysis$overallCluster        = list();

d = scale(sqrt(specs))
analysis$overallCluster$L1norm      = computeOverallClusterL1NormCluster(scale(sqrt(specs)));
analysis$overallCluster$Corrclust   = computerOverallClusterCorrelationCluster(d);
analysis$overallCluster$PVCorrclust = computerOverallClusterPVCorrelationCluster(d);

d = dat$datE[cut$table$passed,]; #d = data$dat if want to do all data
d = d[1:500,]


analysis$overallCluster$L1norm      = computeOverallClusterL1NormCluster(d);
analysis$overallCluster$Corrclust   = computerOverallClusterCorrelationCluster(d);
analysis$overallCluster$PVCorrclust = computerOverallClusterPVCorrelationCluster(d);

#May want to consider using one of the following for additional plots:
#  heatmap.plus
#  heatmap
#  heatmap.2
#Unfortunately, the arguments required for heatmap, heatmap.2 and heatmap.plus are
#  slightly different.

print(analysis$overallCluster$L1norm$levelPlot);
print(analysis$overallCluster$Corrclust$levelPlot);
plot( analysis$overallCluster$PVCorrclust$pvclust);
print(analysis$overallCluster$PVCorrclust$levelPlot);  