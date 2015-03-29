writeMGF = function(spectra, prec.mzs, zs, filename)
{
  #spectra  = decon.6$spectra
  #prec.mzs = c(583.8, 584.609, 585.28)
  #filename = paste(src.dir, "deconvd", sep="")
  #zs = c(2,2,2)
    
  # loop thro the spectra
  for (i in seq(1:length(prec.mzs)))
  {
    msid = paste("mz", prec.mzs[i], sep="")
    # initialize the file
    mgf = file(paste(filename, "_", msid, ".mgf", sep=""))
    headers = c(paste("CHARGE=", zs[i], "+", sep=""), "BEGIN IONS", 
                 paste("TITLE=", msid, sep=""), 
                 paste("PEPMASS=", prec.mzs[i], sep=""))    
    spec = subset(spectra, type=="member"&id==msid&ai>0)
    ions = paste(spec$mz, round(spec$ai, 1), sep=" ")
    writeLines(c(headers, ions, "END IONS"), mgf)    
    close(mgf)
  }
  
}