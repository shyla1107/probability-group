temp<-read.csv(paste0('~/Desktop/Columbia/Pro&Sta/Stock Data/HistoricalQuotes_AAPL.csv'),
               header = TRUE)
AAPL<-data.frame(temp)

##Choose AAPL 
#---------Part (a)----------
x_AAPL<-log(AAPL$close/AAPL$open)
i=1
wr_AAPL<-c()
while (i <= length(x_AAPL)-4){
  wr_AAPL<-c(wr_AAPL,sum(x_AAPL[i:i+4]))
  i<-i+5
}
hist(wr_AAPL,main=paste0("Histogram for AAPL active-trading weekly log-returns"),
     xlab="log-returns",xlim=c(min(wr_AAPL),max(wr_AAPL)),breaks=50)

#---------Part (b)----------
m<-floor(length(x_AAPL)/2)
x1<-x_AAPL[1:m]
x2<-x_AAPL[m+1:length(x_AAPL)]
x2<-x2[!is.na(x2)]
#first half:x1
result1<-sqrt(5)*(x1-mean(x1))
hist(result1,main=paste0("Histogram for first half of AAPL "),
     xlim=c(min(result1),max(result1)),breaks=50)
#second half:x2
w2_AAPL<-c()
i=1
while (i <= length(x2)-4){
  w2_AAPL<-c(w2_AAPL,sum(x_AAPL[i:i+4]))
  i<-i+5
}
result2<-w2_AAPL-mean(w2_AAPL)
hist(result2,main=paste0("Histogram for second half of AAPL "),
     xlim=c(min(result2),max(result2)),breaks=50)

par(mfrow=c(1,2))
hist(result1,main=paste0("Histogram for first half of AAPL "),
     xlim=c(min(result1),max(result1)),breaks=20)
hist(result2,main=paste0("Histogram for second half of AAPL "),
     xlim=c(min(result2),max(result2)),breaks=30)

#---------Part (c)----------
#CI for unknown mean
S<-sqrt(sum((x_AAPL-mean(x_AAPL))^2)/(length(x_AAPL)-1))
l1<-mean(x_AAPL)-qt(0.975,df=length(x_AAPL)-1)*S/sqrt(length(x_AAPL))
r1<-mean(x_AAPL)+qt(0.975,df=length(x_AAPL)-1)*S/sqrt(length(x_AAPL))
CI1<-c(l1,r1)

#CI for unknown variance
l2<-(length(x_AAPL)-1)*S^2/qchisq(0.975, df=length(x_AAPL)-1)
r2<-(length(x_AAPL)-1)*S^2/qchisq(0.025, df=length(x_AAPL)-1)
CI2<-c(l2,r2)

