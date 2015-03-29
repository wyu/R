PerCapitaGDP=function()
{
  library(lattice)
  library(reshape)
  
  src.dir = "/home/wyu/Projects/R/econ2014/data/"  
  countries = read.csv(paste(src.dir, "mpd_2013-01, GDP per capita (1990 Int GK$)  by country.csv", sep="") ,stringsAsFactors=FALSE)
  countries = countries[-c(164:1013)]; countries[countries==""]=NA
  countries = melt(countries, id.vars=c("Year","Total.World"), na.rm=TRUE)
  countries$value=as.numeric(sub(",", "", as.character(countries$value))); 
  countries$Total.World=as.numeric(sub(",", "", as.character(countries$Total.World))); 
  colnames(countries)=c("Year","World","Country","GDPpC");
  
  #compute the rank in the same year using the available data
  
  xyplot(GDPpC/World~Year, group=Country,data=droplevels(subset(countries, Year>1950&Country %in% c("China","USA","Taiwan", "Hong.Kong"))), 
         type=c("p","l"), auto.key=TRUE, scales=list(y=c(log=TRUE, equispaced.log=F)))
  
  
  
}