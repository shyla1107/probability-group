rm(list = ls())   # clear database
install.packages("quantmod")

library(quantmod)

getSymbols("^GSPC",from = "2010-01-01",to = "2017-12-31",src = "yahoo")
# import luxury
getSymbols("AAPL",from = "2010-01-01",to = "2017-12-31",src = "yahoo")
getSymbols("RL",from = "2010-01-01",to = "2017-12-31",src = "yahoo")
getSymbols("LULU",from = "2010-01-01",to = "2017-12-31",src = "yahoo")
getSymbols("PVH",from = "2010-01-01",to = "2017-12-31",src = "yahoo")
getSymbols("TIF",from = "2010-01-01",to = "2017-12-31",src = "yahoo")
# import normal
getSymbols("GPS",from = "2010-01-01",to = "2017-12-31",src = "yahoo")
getSymbols("HNNMY",from = "2010-01-01",to = "2017-12-31",src = "yahoo")
getSymbols("KO",from = "2010-01-01",to = "2017-12-31",src = "yahoo")
getSymbols("NKE",from = "2010-01-01",to = "2017-12-31",src = "yahoo")
getSymbols("REV",from = "2010-01-01",to = "2017-12-31",src = "yahoo")

# close price function
adj_cp <- function(symbol){
  ap <- as.data.frame(symbol)
  adj_close <- scale(as.matrix(ap[,6]))
  rownames(adj_close) = rownames(ap)
  return(adj_close)
}

# log return function
log_return <- function(symbol){
  ap <- as.data.frame(symbol)
  adj_close <- as.matrix(ap[,6])
  rownames(adj_close) = rownames(ap)
  open <- as.matrix(ap[,1])
  rownames(open) = rownames(ap)
  log_rt <- adj_close/open
  return(log_rt)
}

# get log return
sp500 <- log_return(GSPC)
aapl <- log_return(AAPL)
rl <- log_return(RL)
lulu <- log_return(LULU)
pvh <- log_return(PVH)
tif <- log_return(TIF)
gps <- log_return(GPS)
hnnmy <- log_return(HNNMY)
ko <- log_return(KO)
nke <- log_return(NKE)
rev <- log_return(REV)

# get close price
sp500cp <- adj_cp(GSPC)
aaplcp <- adj_cp(AAPL)
rlcp <- adj_cp(RL)
lulucp <- adj_cp(LULU)
pvhcp <- adj_cp(PVH)
tifcp <- adj_cp(TIF)
gpscp <- adj_cp(GPS)
hnnmycp <- adj_cp(HNNMY)
kocp <- adj_cp(KO)
nkecp <- adj_cp(NKE)
revcp <- adj_cp(REV)

# Regression Function
regression <- function(ind,stocks){
  result <- matrix(data=NA, nrow = length(stocks), ncol = 5, byrow = FALSE, dimnames = NULL)
  y <- ind
  i = 1
  for(x in stocks){
    fb<-lm(y~x)
    regress <- summary(fb)
    result[i,1]<-regress$coef[2,1]  # get x's coefficient(slope)
    result[i,2]<-regress$coef[2,3]  # get x's t-value
    result[i,3]<-regress$coef[2,4]  # get x's p-value
    result[i,4]<-regress$r.squared  # get R-squared
    result[i,5]<-regress$adj.r.squared  # get R-squared
    i = i+1
  }
  colnames(result) <- c('Coefficient(Slope)','T-Value','P-Value','R-Squared','Adjusted R-Squared')
  return(result)
}

stk_log_return <- list(aapl,rl,lulu,pvh,tif,gps,hnnmy,ko,nke,rev)
stk_close_pr <- list(aaplcp,rlcp,lulucp,pvhcp,tifcp,gpscp,hnnmycp,kocp,nkecp,revcp)

# Regression 1: Log-Returns Regression between S&P 500 and Consumer-Goods Stocks
regress1 <- round(regression(sp500, stk_log_return),6)
rownames(regress1) <- c('Apple','Ralph Lauren','Lululemon','PVH','Tiffany','GAP','H&M','Coca-Cola','NIKE','Revlon')

# Regression 2: Adjusted Prices Regression between S&P 500 and Consumer-Goods Stocks
regress2 <- regression(sp500cp, stk_close_pr)
rownames(regress2) <- c('Apple','Ralph Lauren','Lululemon','PVH','Tiffany','GAP','H&M','Coca-Cola','NIKE','Revlon')

# Regression 3: Log-Returns Regression between Normal-Goods and Luxury-Goods Stocks
regress3_p <- matrix(data=NA, nrow = 5, ncol = 5, byrow = FALSE, dimnames = NULL)
regress3_r2 <- matrix(data=NA, nrow = 5, ncol = 5, byrow = FALSE, dimnames = NULL)
regress3_coe <- matrix(data=NA, nrow = 5, ncol = 5, byrow = FALSE, dimnames = NULL)
corr <- matrix(data=NA, nrow = 5, ncol = 5, byrow = FALSE, dimnames = NULL)


lux_log_return <- list(aapl,rl,lulu,pvh,tif)
nor_log_return <- list(gps,hnnmy,ko,nke,rev)

col = 1
for(y in lux_log_return){
  row=1
  for(x in nor_log_return){
    regress3_p[row, col] <- summary(lm(y~x))$coef[2,4]    # p-value
    regress3_r2[row, col] <- summary(lm(y~x))$r.squared    # r-squared
    regress3_coe[row, col] <- summary(lm(y~x))$coef[2,1]    # coefficient
    corr[row,col] <- cor(y,x)
    row<-row+1
  }
  col<-col+1
}

regress3_r2 <- round(regress3_r2,6)
regress3_coe <- round(regress3_coe,6)


colnames(regress3_p) <- c('Apple','Ralph Lauren','Lululemon','PVH','Tiffany')
rownames(regress3_p) <- c('GAP','H&M','Coca-Cola','NIKE','Revlon')
colnames(regress3_r2) <- c('Apple','Ralph Lauren','Lululemon','PVH','Tiffany')
rownames(regress3_r2) <- c('GAP','H&M','Coca-Cola','NIKE','Revlon')
colnames(regress3_coe) <- c('Apple','Ralph Lauren','Lululemon','PVH','Tiffany')
rownames(regress3_coe) <- c('GAP','H&M','Coca-Cola','NIKE','Revlon')
colnames(corr) <- c('Apple','Ralph Lauren','Lululemon','PVH','Tiffany')
rownames(corr) <- c('GAP','H&M','Coca-Cola','NIKE','Revlon')

plot(aapl,sp500,xlab = "Apple",ylab = "S&P 500",pch=19)
smoothScatter(aaplcp,sp500cp,xlab = "Apple",ylab = "S&P 500")