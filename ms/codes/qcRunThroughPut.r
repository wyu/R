qcRunThroughput=function()
{
  library(lattice)
  library(reshape)

  #thro = read.csv(stringsAsFactors=F, row.names=NULL, "/home/wyu/Projects/R/data/sputum_thro_59881.csv")
  
  dir = "/home/wyu/Projects/R/ms/data/ms.qc"
  fileNames = list.files(dir)
  
  rawData = lapply(fileNames, function(fileName){read.csv(paste(dir,"/",fileName,sep=""),stringsAsFactors=FALSE)})
  names(rawData) = sub(".csv","",fileNames)
  
  thro = do.call("rbind", rawData); rm(rawData, fileNames)

  # fixup the desc
  thro[thro$name=="Or2013_0513_CF_03", ]$desc = "post repair"
  thro[thro$name=="Or2013_0513_CF_04", ]$desc = "post repair"
  
  # remove the row col
  thro$row     = NULL
  # set the default loading to 1ug for the sputum samples
  thro[thro$id.prj==2862&is.na(thro$loading),]$loading = 1

  # convert the columns
  thro$id.run  = as.factor(thro$id.run)
  thro$id.prj  = as.factor(thro$id.prj)
  thro$id.expt = as.factor(thro$id.expt)
  thro$rt.pct  = as.numeric(thro$rt.pct)
  thro$rt      = as.numeric(thro$rt)
  thro$width   = as.numeric(thro$width)  
  thro$annot   = as.logical(thro$annot)
  thro$loading = as.factor(thro$loading)
  
  # prepare the list of runs
  md = melt(thro, id=c("id.run", "id.prj", "id.expt", "rt", "rt.pct", "slice", "loading", "annot", "name", "desc", "width"))
  runs = cast(md, id.run+id.expt+id.prj+name+desc+loading ~ .)
    
  plotParam = list(
    stim = simpleTheme(pch = c(15:20),
                             lty = c(2,1,3,3,2, 2,1,2,2,1, 2,1,2,1),
                             lwd = c(2,2,1,1,2,2,2,2,2,2,2,2,2,2,2,1),
                             cex = c(1.5),
                             col=c(rep("purple",4),rep("blue",3),rep("green",3),rep("yellow",3),rep("orange",3),rep("red",4))),
    thin3  = simpleTheme(pch=c(19, 12, 22),cex=1.25,lwd=1,col=c("red","darkgreen","blue")),
    thin6  = simpleTheme(pch=c(19, 12, 22),cex=0.8,lwd=0.8,col=c("red","darkgreen","blue")),
    thin   = simpleTheme(pch=c(10:22),cex=0.8,lwd=0.8),
    thin6.vert.x = simpleTheme(pch=c(19, 12, 22),cex=0.8,lwd=0.8,col=c("red","darkgreen","blue")),
    chart.layout = c(3,2))
  
  repairs = subset(thro, id.expt==35187|id.expt==34987)
  repairs$id.expt = as.factor(repairs$id.expt)
  xyplot(log10(apex) ~ rt.pct|desc, group = id.expt, data=repairs, auto.key=list(space="top",columns=4), par.settings=plotParam$thin)
  xyplot(pkw ~ rt.pct|desc, group = id.expt, data=repairs, auto.key=list(space="top",columns=4), par.settings=plotParam$thin)
  xyplot(ppm ~ rt.pct|desc, group = id.expt, data=repairs, auto.key=list(space="top",columns=4), par.settings=plotParam$thin)
  xyplot(mz ~ rt.pct|desc, group = id.expt, data=repairs, auto.key=list(space="top",columns=4), par.settings=plotParam$thin)
  xyplot(mass ~ rt.pct|id.run, group = id.expt, data=repairs, par.settings=plotParam$thin)
  
  sputum = subset(thro, id.prj==2862&desc!="Velos Orbitrap")
  # LC peak width and apex
  xyplot(pkw ~ rt.pct|loading, group = desc, data=sputum, auto.key=list(space="top",columns=4), par.settings=plotParam$thin)
  xyplot(log10(apex) ~ rt.pct|loading, group = desc, data=sputum, auto.key=list(space="top",columns=4), par.settings=plotParam$thin)
  # apex of LCMS
  xyplot(apex ~ rt|name, data=thro, subset=id.prj==2862)
  # apex of LCMS by loading
  xyplot(apex ~ rt|desc, data=thro, subset=id.prj==2862)
  # does higher loading produce more peptides?
  xyplot(err.n ~ rt.pct|desc, group=name, data=thro, subset=annot==TRUE&id.expt %in% c(34278,34908,34909,34910))
  xyplot(err.n ~ rt.pct|desc, group=name, data=thro, subset=annot==TRUE&id.run %in% c(73254,73256,73258))
  xyplot(err.n ~ log(apex)|desc, group=name, data=thro, subset=annot==TRUE&id.run %in% c(73254,73256,73258))
  # does higher loading produce more peptides?
  xyplot(err.n ~ rt|desc, data=thro, subset=id.prj==2862)
  
  newdata <- cast(md, id.run + rt + slice + val + annot ~ variable)
  j1 = subset(thro, slice=="mw"&annot==TRUE)
  j2 = cbind(subset(j1, stat=="apex")$val, subset(j1, stat=="tic")$val)
  xyplot(pkw ~ rt|name, data=thro)
  xyplot(val~rt.pct|name, data=junk, type=c("p","l"), scales="free")
  xyplot(val~rt.pct|stat, data=subset(thro, slice=="mw"), group=name, type=c("p","l"), scales="free", auto.key=TRUE)
  xyplot(val/width~rt.pct|stat, data=subset(thro, slice=="slot"), group=name, type=c("p","l"), scales="free", auto.key=TRUE)
}