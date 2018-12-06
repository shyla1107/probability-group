codelist<-c('AAPL','HNNMY','GPS','EL','AMZN','AOS','NKE','KO','INTC','SNE')
for (i in 1:length(codelist)){
  temp<-read.csv(paste0('~/Desktop/Columbia/Pro&Sta/Stock Data/HistoricalQuotes_',codelist[i],'.csv'),
                       header = TRUE)
  temp<-data.frame(temp)
  assign(codelist[i],temp)
}

xrange=c(min(AAPL$close),max(AAPL$close))
jpeg(paste0('~/Desktop/Columbia/Pro&Sta/Stock Data/AAPL_ClosePrice.jpg'))
hist(AAPL$close,main="Histogram for AAPL closeprice",
     xlab="ClosePrice",xlim=xrange,breaks=20)
dev.off()

for (i in codelist){
  temp<-get(i)
  logR<-log(temp$close/temp$open)
  jpeg(paste0('~/Desktop/Columbia/Pro&Sta/Stock Data/',i,'_LogReturn.jpg'))
  hist(logR,main=paste0("Histogram for ",i," log-returns"),
       xlab="log-return",xlim=c(min(logR),max(logR)),breaks=50)
  dev.off()
}