#install.packages("Quandl")
#install.packages("devtools")
library(devtools)
#install_github('R-package','quandl')
library(Quandl)
# the auth token
Quandl.auth("yU7dAjDby3DhzodRmqUL")

library(lattice)

mydata = Quandl("NSE/OIL")