clusterSpectraResiduals=function(datfile)
{
  #library(pheatmap) # I like esoteric packages!
  #library(RColorBrewer)
  #library(latticeExtra)
  library(lattice)
  #library(pvclust)
  #library(kernlab)
  #library(snow)
  library(reshape)
  library(nnls)
  library(stringr)
  
  source('~/Projects/R/ms/codes/prepareMSMS.r')
  source('~/Projects/R/ms/codes/deconvMSMS.r')
  
  datfile = "ai_devi_example"
  src.dir = "/home/wyu/Projects/R/ms/data/"
  
  data  = prepareMSMS(paste(src.dir, datfile, sep=""), "ai_devi_example")
  # for illustration purpose, remove spectra that're the same
  data.6 = subset(data$msms, !(id %in% c("ms105599700", "ms105620706", "ms105626195", "ms105630746")))
  decon = deconvMSMS(data$msms)
  decon.6 = deconvMSMS(data.6)
  
  write.csv(components, file=paste(src.dir, datfile, "_components.csv",   sep=""))  
  
  panel.ms = function(x, y,...) {
    # normally called by xyplot() to do routine plotting
    for (i in seq(1:length(x)))
    {
      panel.segments(x0=x[i], y0=0, x1=x[i],y1=y[i])
    }    
  }  
  #cmps = c("mz583.8", "mz584.609", "mz585.28")
  fig.deconv = list()
  fig.deconv$high = xyplot(ai~mz|id, data=decon.6$spectra, subset=mz>=300&mz<=1200,
         panel=panel.ms, layout=c(1,9), strip=FALSE, 
         scales=list(x=list(draw=T), y=list(axs="i", relation="free", tick.number=2)), 
         ylab=unique(decon.6$spectra$id), xlab="m/z")
  fig.deconv$low  = xyplot(ai~mz|id, data=decon.6$spectra, subset=mz<300,
         panel=panel.ms, layout=c(1,9), strip=FALSE, 
         scales=list(x=list(draw=T), y=list(axs="i", relation="free", tick.number=2)), 
         ylab=unique(decon.6$spectra$id), xlab="m/z")
  # plot the precursor isolation 
  panel.prec = function(x, y,...) {
    # normally called by xyplot() to do routine plotting
    for (i in seq(1:length(x)))
    {
      panel.segments(x0=x[i], y0=0, x1=x[i],y1=y[i])
    }    
    panel.rect(xleft=584.28, ybottom=0, xright=586.28,ytop=1E8, lwd=0, col="gray", alpha=0.25)
  }  
  plot.prec = subset(data$precs, m.z>=583&m.z<=587&!(id %in% c("ms105599700", "ms105620706", "ms105626195", "ms105630746")))
  xyplot(ai~m.z|id, data=plot.prec,
         panel=panel.prec, layout=c(1,7), strip=FALSE, 
         scales=list(x=list(draw=T), y=list(axs="i", relation="free", tick.number=2)), 
         ylab=unique(plot.prec$id), xlab="m/z")
  print(fig.deconv)
  
  str_sub(unique(decon$spectra$id), -6, -1)
  par(mfrow=c(1,1))
  heatmap(scale(sqrt(specs)))
  heatmap(scale(asinh(specs)))

  #data(mtcars)
  #x  <- t(as.matrix(scale(mtcars)))
  x = scale(specs)
  #dd.row <- as.dendrogram(hclust(dist(x)))
  pc = pvclust(t(x), nboot=100)  
  plot(pc)
  mc = pvclust(x, nboot=1000)  
  plot(mc)

  # group the precursors
  precursor.groups = cutree(pc$hclust, 2)
  precursor.result = cbind(specs, precursor.groups)

  frag.groups = cutree(mc$hclust, 2)
  frag.result = cbind(t(specs), frag.groups)
  frag.result = rbind(as.vector(precursor.groups), frag.result)
  rownames(frag.result)[1] = "precursor"
  
  write.csv(frag.result, file=paste(src.dir, datfile, "_frag_groups.csv",   sep=""))

  # construct the data frame for the glm model
  msms = subset(members, type=="msms"&spec=="ai_devi_example")
  msms$id = paste("ms", msms$msid, sep="")
  msms$type = NULL; msms$spec = NULL; msms$zscore = NULL; msms$ri = NULL; msms$msid = NULL
  mzs = unique(msms$m.z)
  for (mz in mzs)
  {
    msms[msms$m.z==mz,]$ai = as.integer(round(1000 * msms[msms$m.z==mz,]$ai / sum(msms[msms$m.z==mz,]$ai)))
  }
  # normzlie the peptides to the sum of intensities  
  colnames(peptides) = c("id","p1","p2","p3")
  s = peptides$p1 + peptides$p2 + peptides$p3
  peptides$p1 = peptides$p1 / s; peptides$p2 = peptides$p2 / s; peptides$p3 = peptides$p3 / s
  
  # merge the data frames
  msms = merge(msms, peptides, by.x="id")  
  
  # build a simple model
  lm1 = glm(ai ~ p1 + p2 + p3, data=subset(msms, m.z=="769.391"), family="poisson", intercept=FALSE)
  summary(lm1)
  lm2 = glm(ai ~ p1 + p2 + p3, data=subset(msms, m.z=="354.168"))
  summary(lm2)
  
  # try nnls for positive coeffs, http://www.jameskeirstead.ca/blog/positive-coefficient-regression-in-r/
  components = list()
  for (i in seq(1:length(mzs)))
  {
    nnls.ai <- nnls(as.matrix(msms[msms$m.z==mzs[i],c("p1","p2","p3")]), msms[msms$m.z==mzs[i],]$ai)
    components[[i]] = c(mzs[i], coefficients(nnls.ai))
  }
  components = do.call("rbind", components)
  write.csv(components, file=paste(src.dir, datfile, "_components.csv",   sep=""))  
  
  dend.pc = as.dendrogram(pc$hclust)
  leaf.pc = order.dendrogram(dend.pc)
  legd.pc = list(top=list(fun=dendrogramGrob,args=list(x=dend.pc, side="top")))
  dend.mc = as.dendrogram(mc$hclust)
  leaf.mc = order.dendrogram(dend.mc)
  legd.mc = list(top=list(fun=dendrogramGrob,args=list(x=dend.mc, side="top")))
  
  levelplot(anal$dist[anal$leafOrder, anal$leafOrder],scales=list(x=list(rot=90)), xlab="",ylab="", legend=legend)
  levelplot(x[row.ord, col.ord],
            aspect = "fill",
            scales = list(x = list(rot = 90)),
            at=seq(-3,3,0.5),
            colorkey = list(space = "left"),
            legend =
              list(right = list(fun = dendrogramGrob, args = list(x = dd.col, ord = col.ord, side = "right", size = 10)),
                   top   = list(fun = dendrogramGrob, args = list(x = dd.row, side = "top", size = 10)))
  )
  
  anal$dendrogram = as.dendrogram(anal$pvclust$hclust)
  anal$leafOrder  = order.dendrogram(anal$dendrogram)
  anal$legend     = list(top=list(fun=dendrogramGrob,args=list(x=anal$dendrogram, side="top")))
  anal$levelPlot  = levelplot(anal$dist[anal$leafOrder, anal$leafOrder],scales=list(x=list(rot=90)), xlab="",ylab="", legend=legend)
  
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
 
  # try the PCA
  pc = princomp(t(specs), cor=TRUE, scores=TRUE)
  
  prec = subset(members, type=="precursors"&spec=="ai_devi_example")
  prec$id = paste("id", prec$msid, sep="")
  prec$type = NULL; prec$spec = NULL; prec$zscore = NULL; prec$ai = NULL; prec$msid = NULL
  prec.md = melt(prec, id=c("id", "m.z"))
  isolation = cast(prec.md, m.z ~ id)
  
  # set the m/z as the rownames
  rownames(isolation) = isolation$m.z; isolation$m.z = NULL
  iso = t(as.matrix(isolation))
  heatmap(scale(iso))
  heatmap(scale(asinh(iso)))
  
  xyplot(sqrt(id105599700) ~ sqrt(id105615807), data=spectra)
  r = as.matrix(residuals[4:13])
  p = as.matrix(subset(residuals, type=="precursors"&m.z>=584.28&m.z<=586.28)[4:13])
  m = as.matrix(subset(residuals, type=="msms")[4:13])
  heatmap(r)
  
  km = kmeans(r, 2)
  m2 <- cbind(r,km$cluster)
  o <- order(m2[, 6])
  m2 <- m2[o, ]
  pheatmap(m2[,1:5], cluster_rows=F,cluster_cols=F, col=brewer.pal(10,"Set3"),border_color=NA)
  
  
}