modelPeptideXIC = function()
{
  library(lattice)
  
  dat.cal = read.csv("/home/wyu/Projects/R/data/matrix_cal.csv",stringsAsFactors=FALSE)
  dat.cmp = read.csv("/home/wyu/Projects/R/data/matrix_cmp.csv",stringsAsFactors=FALSE, row.names=1:n)
  dat.pts = read.csv("/home/wyu/Projects/R/data/matrix_pt.csv", stringsAsFactors=FALSE)
  
  slice  = subset(dat.pts, rt.0>30&rt.0<30.5)
  slice1 = subset(slice, isotope==1)
  slice1$ai0 = (slice[slice$isotope==1,]$ai * slice[slice$isotope==2,]$ai * slice[slice$isotope==3,]$ai)^(1/3)
    
  xyplot(ai~rt|peptide,group=run,
         data=slice1,
         type=c("l"), auto.key=TRUE,
         scales=list(relation="free"),
         layout=c(4,3), as.table=TRUE)
  
}
