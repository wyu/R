vsCountry=function()
{
  gdp.india = Quandl("IMF/MAP_WEO_NGDP_IND")
  gdp.china = Quandl("IMF/MAP_WEO_NGDP_CHN")
  
  gdp = cbind(gdp.china, gdp.india, by=c("Year")); 
  colnames(gdp)=c("Year","China","Year2","India","By");
  
  xyplot(China/India~Year, data=gdp, type="a")
}
# Q. Does GDP per capita vs median household income indicative of the distortion in income distribution?
GDPvsHouseholdIncome=function()
{
  gdp.capita = Quandl("USER_1GO/1GU")
  income = 
}