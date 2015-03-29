modelMixedPrecursors=function()
{
  ms.indiv = read.csv("/home/wyu/Projects/R/data/mock_precursors.csv",stringsAsFactors=FALSE)  
  
  ms.indiv$A = 100 * ms.indiv$A / max(ms.indiv$A)
  ms.indiv$B = 100 * ms.indiv$B / max(ms.indiv$B)
  ms.indiv$C = 100 * ms.indiv$C / max(ms.indiv$C)
  
  barplot(ms.indiv$A)
  
  # the combine 
  frac.A = 0.9; frac.B = c(0.1, 0.2, 0.05)
  
  mixed = data.frame(mz=ms.indiv$mz)
  mixed$AB1 = ms.indiv$A * frac.A + ms.indiv$B * frac.B[1]
  mixed$AB2 = ms.indiv$A * frac.A + ms.indiv$B * frac.B[2]
  mixed$AB3 = ms.indiv$A * frac.A + ms.indiv$B * frac.B[3]
  
  barplot(mixed$AB1)
  
  # remove the rows without any data
  spectra = mixed[rowMeans(mixed[,2:4])!=0,]
  spectra[,1] = paste("m/z", spectra[,1], "")
  
  # let's do some clutering
  
}