testStockSeries = function(){
  library(quantmod)
  # Specify lookup parameters, and save for future sessions.    
  getSymbols("AAPL",src="yahoo")  
  barChart(AAPL) 
  barChart(AAPL) 

  getSymbols("DJI")  
  
  # grab all stock symbols from the exchanges
  stocks = stockSymbols()
  
  library(quantmod)
  
  getSymbols("^DJI", from="1900-01-01")
  
  dji = Cl(DJI["/2011"])
  
  djiVol = aggregate(
    dji,
    as.numeric(format(index(dji), "%Y")),
    function(ss) coredata(tail(TTR:::volatility(
      ss,
      n=NROW(ss),
      calc="close"), 1)))
  ecdf(as.vector(djiVol))(as.numeric(tail(djiVol,1)))
  # The result is 0.8214286, the 82nd quantile
}