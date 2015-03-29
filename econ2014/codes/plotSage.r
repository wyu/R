plotSage=function()
{
  library(lattice)
  
  src.dir = "/users/wyu/Documents/papers/spectra_cluster/spectral_clustering/data/"
  
  sage01 = read.csv(paste(src.dir, "sage_Simple_cond_index3_HCD2_20130331.csv", sep="") ,stringsAsFactors=FALSE)
  sage01$file = "cond_index3"
  sage02 = read.csv(paste(src.dir, "sage_Simple_cond_index2_HCD2_20130331.csv", sep="") ,stringsAsFactors=FALSE)
  sage02$file = "cond_index2"
  sage03 = read.csv(paste(src.dir, "sage_Simple_cond_index1_HCD2_20130331.csv", sep="") ,stringsAsFactors=FALSE)
  sage03$file = "cond_index1"
  sage04 = read.csv(paste(src.dir, "sage_Simple_condi_mixed_HCD2_20130331.csv", sep="") ,stringsAsFactors=FALSE)
  sage04$file = "cond_mixed"
  sage05 = read.csv(paste(src.dir, "sage_Simple_cond_pure_index_HCD2_20130331.csv", sep="") ,stringsAsFactors=FALSE)
  sage05$file = "cond_pure_index"
  sage06 = read.csv(paste(src.dir, "sage_Simple_condi_pure_HCD2_20130331.csv", sep="") ,stringsAsFactors=FALSE)
  sage06$file = "cond_pure"
  sage07 = read.csv(paste(src.dir, "sage_Simple_condi_10pct_HCD2_20130331.csv", sep="") ,stringsAsFactors=FALSE)
  sage07$file = "cond_10pct"
  sage08 = read.csv(paste(src.dir, "sage_Simple_condi_HCD2_20130331.csv", sep="") ,stringsAsFactors=FALSE)
  sage08$file = "cond"
  sage09 = read.csv(paste(src.dir, "sage_Simple_HCD2_20130331.csv", sep="") ,stringsAsFactors=FALSE)
  sage09$file = "simple"
  sage10 = read.csv(paste(src.dir, "sage_Simple_cond_index4_HCD2_20130331.csv", sep="") ,stringsAsFactors=FALSE)
  sage10$file = "cond_index4"
  
  sages = rbind(sage01,sage02,sage03,sage04,sage05,sage06,sage07,sage08,sage09,sage10)
  
  xyplot(prob ~ dp, group=file, data=subset(sages, chart=='transition'&type=='gmean'), type='p')
}