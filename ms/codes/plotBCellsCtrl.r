plotBCellsCtrl = function(dFrame) {
  # subset only the relevant data
  D = subset(dFrame, controls!="samples")
  D$antiIgD = factor(D$antiIgD, level=c("NPC", "PBS", "0.5", "10"), ordered=T)    
  
  plotParameters = list(
    themeForStimPlots = simpleTheme(pch = c(15:20),
                                    lty = c(2,1,3,3,2, 2,1,2,2,1, 2,1,2,1),
                                    lwd = c(2,2,1,1,2,2,2,2,2,2,2,2,2,2,2,1),
                                    cex = c(1.5),
                                    col=c(rep("purple",4),rep("blue",3),rep("green",3),
                                          rep("yellow",3),rep("orange",3),rep("red",4))),
    thin3  = simpleTheme(pch=c(19, 12, 22),cex=1.25,lwd=1,col=c("red","darkgreen","blue")),
    thin6  = simpleTheme(pch=c(19, 12, 22),cex=0.8,lwd=0.8,col=c("red","darkgreen","blue")),
    thin6.vert.x = simpleTheme(pch=c(19, 12, 22),cex=0.8,lwd=0.8,col=c("red","darkgreen","blue")),
    chart.layout = c(3,2))
  
  # make up a new column reflecting the order or the row
  D$rowid = factor(paste(D$patientId, D$timepoint, sep=","), ordered=T)  
  
  xyplot(BCells~rowid|Period,group=antiIgD,
         data=D,
         type=c("a", "g", "p"),
         scale=list(alternating=1, x=list(rot=90), y=list(limits=c(0,max(D$BCells)))),
         strip = FALSE,
         auto.key=list(space="top",columns=3),                                    
         par.settings=plotParameters$thin6,
         layout=c(3,2), as.table=TRUE,
         xlab="log[antiIgD], ug/ml", 
         ylab="MFI of pAKT in B Cells from the pre/post run Controls")

  # establish the linear model for each subject, period
  #lm.10p1 = lm(BCells~rowid, subset(D, (antiIgD==10)&(Period==1)&(controls=="cntrl-A1004")))
  #lm.10p2 = lm(BCells~rowid, subset(D, (antiIgD==10)&(Period==2)))
  #y4ec90 = predict(lm.fit, newdata=data.frame(logAntiIgD=ec90))     

  #trellis.focus("panel", 1, 1)
  #panel.segments(ec90-0.5, y4ec90, x1=ec90+0.5, y1=y4ec90, col=colors[tp][[1]])      
  #trellis.unfocus()    
  
  # do the box plot instead
  bwplot(BCells~Period|controls*antiIgD, data=D, 
         auto.key=T,type=c("p","a","g"),
         layout=c(4,1), as.table=TRUE,
         main="CV of pAKT Measurements from Pre/Post Run Ctrls @ antiIgD=10") 
  bwplot(BCells~Period|controls, data=subset(D, antiIgD==0.5), 
         auto.key=T,type=c("p","a","g"),
         layout=c(1,2), as.table=TRUE,
         scale=list(y=list(limits=c(0,max(D$BCells)))),
         main="CV of pAKT Measurements from Pre/Post Run Ctrls @ antiIgD=0.5") 
  
  # calculate the CV
  co.var   <- function(x) ( 100*sd(x)/mean(x))
  by_sample = list(patientId=D$controls, antiIgD=D$antiIgD, period=D$Period)
  sample.cv   = aggregate(x=D, by=by_sample, FUN=co.var)
  sample.mean = aggregate(x=D, by=by_sample, FUN=mean)
  
  D = subset(dFrame, antiIgD=="NPC")
  
  D$rowid = factor(paste(D$patientId, D$antiIgD, D$timepoint, sep=","), ordered=T)  
  
  xyplot(BCells~rowid|Period,
         data=D,
         type=c("a", "g", "p"),
         scale=list(alternating=1, y=list(limits=c(0,max(D$BCells)))),
         strip = TRUE,
         auto.key=list(space="top",columns=3),                                    
         par.settings=plotParameters$thin6.vert.x,
         layout=c(3,1), as.table=TRUE,
         xlab="log[antiIgD], ug/ml", 
         ylab="MFI of pAKT in B Cells from the pre/post run Controls")
}