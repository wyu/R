ms.ai.pdf=function()
{
  library(lattice); library(effects);

  # we want to model a simple question. Given a pair of MS spectra, how do we known if the ion intensities are the same?
  # Ordinarily, we'd need to derive the probability distribution of the ion intensities. i.e. The C.I. if the ion 
  # were allow to show up randomly.
  
  # the following was derived from a set of 6 MS/MS that're deemed the same by the clustering algorithm.
  # we first calculate the composite spectrum and the relative abundance of each ions in such composite.
  # we then go through each member spectrum can calculate the intensity deviation of each ions from that
  # predicted from the base peak of the spectrum and relative abundance of the ion. The data frame is arranged
  # by the intensity ~ devi.
  src.dir = "/users/wyu/Documents/papers/spectra_cluster/spectral_clustering/data/"
    
  ms.ai   = read.csv(paste(src.dir, "ai_devi_example_pdf.csv", sep="") ,stringsAsFactors=FALSE)
  #ms.ai   = read.csv(paste(src.dir, "pdf_Sloppy_ISB_20130328.csv", sep="") ,stringsAsFactors=FALSE)
  
  # this seems to be the simplest way to achive uniform variance. It's sort of consistent with the literature
  # report starting the noise is proportional to intensity^x, where x = 0.8. Our x is more like 0.5 here.
  xyplot(devi/sqrt(ai) ~ log(ai), data=ms.ai)
  
  # map the column name
  # some additional columns
  ms.ai$ai.devi.pct     = 100 * ms.ai$devi / ms.ai$ai
  ms.ai$devi.abs.log = log(abs(ms.ai$devi))
  ms.ai$ai.log        = log(ms.ai$ai)
  
  lm0 = lm(devi.abs.log ~ ai.log, data=ms.ai)
  xyplot(ai.devi.pct ~ log(f.ai), 
         data=ms.ai,
         scale=list(y=list(limits=c(-2000,2000))))
  
  # plot in log-log space for uniform varian accross the intensity range
  xyplot(log(abs(devi)) ~ log(ai), data=subset(ms.ai, abs(devi)> 1))
  # try the sqrt transform
  xyplot(sqrt(abs(ai.devi)) ~ sqrt(f.ai), data=subset(ms.ai, abs(ai.devi)> 1))
  
  lm1 = lm(ai.devi.abs.log ~ f.ai.log, data=subset(ms.ai, abs(ai.devi)> 1))
  plot(lm1)

  # now we've proved the linear model (lm1). We can simple look at the relative devi. 
  # wola, an uniform varian across the intensity and no slope. What does it mean?
  xyplot(log(abs(devi/ai)) ~ log(ai), data=subset(ms.ai, abs(devi)> 1))
  
  # to apply the model to a new ion
  # calculate the c.i. first
  ms.ai.1 = subset(ms.ai, abs(ai.devi)> 1)
  ai.ci = quantile(log(abs(ms.ai.1$ai.devi/ms.ai.1$f.ai)), prob=c(0.05,0.95))
  
  
  # plot the model just for fun
  plot(allEffects(lm1))
  toPlot = as.data.frame(effect("f.ai.log",lm1,se=TRUE))

  err.width = 0.05; err.color = 'gray'
  panel.ci <- function(x, y, lx, ux, subscripts, pch = 16, ...)
  {
    #print(paste("panel", subscripts, sep=":"))
    x <- as.numeric(x)
    y <- as.numeric(y)
    lx <- as.numeric(lx[subscripts])
    ux <- as.numeric(ux[subscripts])
    #print(paste("x,y,lx,ux", x, y, lx, ux, sep=", "))
    panel.segments(x0=x-err.width, y0=lx, x1=x+err.width, y1=lx, col=err.color, subscripts=subscripts)
    panel.segments(x0=x-err.width, y0=ux, x1=x+err.width, y1=ux, col=err.color, subscripts=subscripts)
    panel.segments(x0=x, y0=lx, x1=x, y1 =ux, col=err.color, subscripts=subscripts)    
    panel.xyplot(x,y, subscripts=subscripts, ...)
  }
  xyplot(fit~f.ai.log,
         data=toPlot,
         panel = panel.ci,
         lx = toPlot$lower, 
         ux = toPlot$upper,
         par.settings=simpleTheme(pch=c(10),cex=1.5,lwd=2,col=c("black")),
         type=c("p"), as.table=TRUE,
         #scale=list(alternating=1, y=list(limits=c(-70,120))),
         ylab="% Inhibition versus pre-dose",
         xlab="dose (mg)",
         main="% Inhibition at 4 and 24hr versus Dosage", sep="")  
}