library(quantmod)			# import quantmod

getSymbols("AAPL",from = "2016-10-31",to = "2018-11-03",src = "yahoo")				# import data from YAHOO
head(AAPL)				#show the first a few lines of data
ap = as.data.frame(AAPL)

# hist for close price
close <- as.matrix(ap[,4])
rownames(close) = rownames(ap)
hist(close, breaks = 20)

# hist for log-return
open <- as.matrix(ap[,1])
rownames(open) = rownames(ap)
logrt = log(close/open)
rownames(logrt) = rownames(ap)
hist(logrt, breaks = 50)

######################### part a
# Active-Trading Weekly Log-return
weekrt = data.frame(matrix(0,length(wk),1))
rownames(weekrt) = wk

date <- rownames(logrt)

weekrt[1,1] = sum(logrt[1:5])

for(w in seq(2,length(wk))){
  weeklog=0
  for (d in seq(1,length(date))){  
    if ((wk[w-1] < date[d]) && (date[d] <= wk[w])) weeklog = weeklog+logrt[d]
  }
  weekrt[w,1] = weeklog
}

weeklogrt = as.matrix(weekrt)
### histogram figure 1
hist(weeklogrt, breaks = 30, main = "Histogram for AAPL Active-Trading Weekly Log-return", xlab = "weekly log-return")


######################## part b
# histograms for x and y

first<-logrt[1:90]
second<- weeklogrt[20:105]

avg1 = mean(first)
x = sqrt(5)*(first-avg1)
### histogram figure 2
hist(x, breaks = 30, main = "Histogram of ¡Ì5(xt-x¡¥) for the first half data" , xlab = "¡Ì5(xt-x¡¥)")

avg2 = mean(second)
y = second-avg2
### histogram figure 3
hist(y, breaks = 30, main = "Histogram of yw-y¡¥ for the second half data" , xlab = "yw-y¡¥")


####################### part c
####################### interval estimation for unknown mean and variance

x = logrt
n <- length(x)-1
avg <- mean(x)
s2 <- sum((x-avg)^2)/(n)

##### mean confidence interval
t.test(x, conf.level = 0.95)

##### variance confidence interval
chisq.var.test<-function(x,n,a,alt=2,sigma0=1)
{
  result<-list()
  v<-var(x)
  result$interval<-c((n-1)*v/qchisq(1-a/2,n-1,lower.tail=T),(n-1)*v/qchisq(a/2,n-1,lower.tail=T))
  chi2<-(n-1)*v/sigma0
  result$chi2<-chi2
  p<-pchisq(chi2,n-1)
  if(alt==2)
    result$p.value<-2*min(pchisq(chi2,n-1),pchisq(chi2,n-1,lower.tail=F))
  else
    result$p.value<-pchisq(chi2,n-1,lower.tail=F)
  result
}

chisq.var.test(x,n,a=0.05,alt=2,sigma0=1)