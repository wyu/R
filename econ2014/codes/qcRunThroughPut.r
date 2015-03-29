qcRunThroughput=function()
{
  library(lattice)
  library(reshape)

  #thro = read.csv(stringsAsFactors=F, row.names=NULL, "/home/wyu/Projects/R/data/sputum_thro_59881.csv")
  
  dir = "/home/wyu/Projects/R/data/ms.qc"
  fileNames = list.files(dir)
  
  rawData = lapply(fileNames, function(fileName){read.csv(paste(dir,"/",fileName,sep=""),stringsAsFactors=FALSE)})
  names(rawData) = sub(".csv","",fileNames)
  
  thro = do.call("rbind", rawData); rm(rawData, fileNames)

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
  thro$loading = as.logical(thro$loading)
  
  # prepare the list of runs
  md = melt(thro, id=c("id.run", "id.prj", "id.expt", "rt", "rt.pct", "slice", "loading", "annot", "name", "desc", "width"))
  runs = cast(md, id.run+id.expt+id.prj+name+desc+loading ~ .)
    
  # LC peak width
  xyplot(pkw ~ rt|desc, group = loading, data=thro, subset=id.prj==2862)
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