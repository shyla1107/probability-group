#install.packages("quantmod")

library(quantmod)			# import quantmod

getSymbols("AAPL",from = "2016-11-02",to = "2018-11-02",src = "yahoo")				# import data from YAHOO
head(AAPL)				#show the first a few lines of data
ap = as.data.frame(AAPL)

# hist for close price
close <- as.matrix(ap[,4])
hist(close, breaks = 20)

# hist for log-return
open <- as.matrix(ap[,1])
logrt = close/open
hist(logrt, breaks = 50)