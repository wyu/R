deconvMSMS = function(ms2)
{
  # ms2 = data$msms
  # try nnls for positive coeffs, http://www.jameskeirstead.ca/blog/positive-coefficient-regression-in-r/
  mzs        = unique(ms2$m.z)
  components = list()
  resids     = list()
  cmps       = colnames(ms2)[4:dim(ms2)[2]]
  for (i in seq(1:length(mzs)))
  {
    nnls.ai <- nnls(as.matrix(ms2[ms2$m.z==mzs[i],cmps]), ms2[ms2$m.z==mzs[i],]$ai)
    components[[i]] = c(mzs[i], coefficients(nnls.ai))
    #resids[[i]] = 
  }
  components = data.frame(do.call("rbind", components))
  colnames(components) = c("mz", cmps)
  
  # reformat the components
  m = melt(components, id.vars="mz", measure.vars=cmps)
  colnames(m) = c("mz","id","ai")
  m$type = "member"
              
  # mesh the original and deconvoluted spectra together
  m2 = ms2[,1:3]; colnames(m2) = c("id","mz","ai")
  m2$type = "component"
              
  return (list(spectra=rbind(m2, m), cmps=components))
}