clusterSpectraResiduals=function(datfile)
{
  library(pheatmap) # I like esoteric packages!
  library(RColorBrewer)
  library(latticeExtra)
  library(lattice)
  library(pvclust)
  library(kernlab)
  library(snow)
  
  datfile = "ai_devi_example"
  src.dir = "/home/wyu/Projects/R/data/"
  # let's working thro some examples first  
  residuals = read.csv(paste(src.dir, datfile, "_clust.csv",   sep="") ,stringsAsFactors=FALSE) 
  members   = read.csv(paste(src.dir, datfile, "_members.csv", sep="") ,stringsAsFactors=FALSE) 
  #rownames(residuals) = paste(residuals$type, ":", residuals$m.z)
  rownames(residuals) = paste("m/z", residuals$m.z)
  r = as.matrix(residuals[4:13])
  p = as.matrix(subset(residuals, type=="precursors"&m.z>=584.28&m.z<=586.28)[4:13])
  m = as.matrix(subset(residuals, type=="msms")[4:13])
  heatmap(r)
  
  km = kmeans(r, 2)
  m2 <- cbind(r,km$cluster)
  o <- order(m2[, 6])
  m2 <- m2[o, ]
  pheatmap(m2[,1:5], cluster_rows=F,cluster_cols=F, col=brewer.pal(10,"Set3"),border_color=NA)
  
  #data(mtcars)
  #x  <- t(as.matrix(scale(mtcars)))
  x = scale(p)
  #dd.row <- as.dendrogram(hclust(dist(x)))
  pc = pvclust(x, nboot=1000)  
  plot(pc)
  mc = pvclust(t(x), nboot=1000)  
  plot(mc)
  
  plot(pvclust(  scale(m),  nboot=1000) )
  plot(pvclust(t(scale(m)), nboot=1000)  )
  
  row.ord <- order.dendrogram(dd.row)
  
  #dd.col <- as.dendrogram(hclust(dist(t(x))))
  dd.col <- as.dendrogram(pvclust(t(x)))
  col.ord <- order.dendrogram(dd.col)
  
  levelplot(x[row.ord, col.ord],
            aspect = "fill",
            scales = list(x = list(rot = 90)),
            at=seq(-3,3,0.5),
            colorkey = list(space = "left"),
            legend =
              list(right = list(fun = dendrogramGrob, args = list(x = dd.col, ord = col.ord, side = "right", size = 10)),
                   top   = list(fun = dendrogramGrob, args = list(x = dd.row, side = "top", size = 10)))
            )
  
  # play around with the member spectra
  baseline = min(subset(members, ai!=0)$ai)
  m0706 = subset(members, type=='msms'&msid=="105620706")
  m9700 = subset(members, type=='msms'&msid=="105599700")
  m2773 = subset(members, type=='msms'&msid=="105622773")
  m0706.min = min(subset(m0706, ai!=0)$ai); 
  m9700.min = min(subset(m9700, ai!=0)$ai); 
  m2773.min = min(subset(m2773, ai!=0)$ai); 
  
  m3 = cbind()
    
  plot(x=log(m0706$ai+m0706.min), y=log(m9700$ai+m9700.min))
  plot(x=log(m0706$ai+m0706.min), y=log(m2773$ai+m2773.min))
  
  # plot the mass spectra
  members$msid = as.factor(members$msid)
  xyplot(ri ~ m.z|msid, data=subset(members, type=="msms"), type="h", layout=c(1,10,1))
  xyplot(zscore ~ m.z|msid, data=subset(members, type=="msms"&m.z>350), type="h", layout=c(2,5,1), scales="free")
  
}