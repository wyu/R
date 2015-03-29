prepareMSMS=function(datafile, name)
{
  #residuals = read.csv(paste(src.dir, datfile, "_clust.csv",   sep="") ,stringsAsFactors=FALSE) 
  members   = read.csv(paste(datafile, "_members.csv",    sep="") ,stringsAsFactors=TRUE) 
  peptides  = read.csv(paste(datafile, "_precursors.csv", sep="") ,stringsAsFactors=TRUE) 
  
  msms = subset(members, type=="msms"&spec==name)
  msms$id = paste("id", msms$msid, sep="")
  msms$type = NULL; msms$spec = NULL; msms$zscore = NULL; msms$ai = NULL; msms$msid = NULL
  md = melt(msms, id=c("id", "m.z"))
  spectra = cast(md, m.z ~ id)
  # set the m/z as the rownames
  rownames(spectra) = spectra$m.z; spectra$m.z = NULL
    
  # construct the data frame for the glm model
  members$id = paste("ms", members$msid, sep="")
  members$zscore = NULL; members$ri = NULL; members$msid = NULL
  msms = subset(members, type=="msms"&spec==name)
  precursors = subset(members, type=="precursors"&spec==name)
  
  msms$type = NULL; precursors$type = NULL; msms$spec = NULL; precursors$spec = NULL;
  p = precursors
  p$id = NULL; p$row.names = NULL
  p = aggregate(p, by=list(mz = p$m.z), sum)
  p$id = "combined"
  p = p[,c(1,3,4)]
  colnames(p)[1] = "m.z"
  precursors = rbind(precursors, p)
  
  #mzs = unique(msms$m.z)
  #for (mz in mzs)
  #{
  #  msms[msms$m.z==mz,]$ai = as.integer(round(1000 * msms[msms$m.z==mz,]$ai / sum(msms[msms$m.z==mz,]$ai)))
  #}
  # normzlie the peptides to the sum of intensities  
  peptides[,2:length(peptides)] = peptides[,2:length(peptides)] / rowSums(peptides[,2:length(peptides)])
  colnames(peptides)[1] = "id"
 
  # prepare the precursor isolation
  
  
  #m=merge(msms, peptides, by.x="id")
  #s=t(as.matrix(spectra))
  #d = list(specs=s, msms=m)
  return (list(specs=spectra, precs = precursors, msms=merge(msms, peptides, by.x="id")))
}