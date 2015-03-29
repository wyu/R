modelProteinDigestion=function()
{
  # some starting parameters
  cells = 1E6; 
  # create a proteome with 20k proteins according to the copy numbers given in 
  proteins = data.frame(copy=10^rnorm(seq(1:20000), log10(5000), 1.1))
  proteins$fmol = proteins$copy * cells * 1E15 / 6.023E23
  
  # check the ditribution of the copy numbers
  start = density(proteins$copy, breaks=10^seq(from=0, to=15,by=0.1))
  
  for (i in c(1:100))
  {
    proteins$rate = 
  }
}