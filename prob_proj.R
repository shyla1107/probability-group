codelist<-c('AAPL','HNNMY','GPS','LULU','RL','PVH','NKE','KO','TIF','REV')
#select time from 2010-01-01 to 2017-12-31
for (i in 1:length(codelist)){
  temp<-read.csv(paste0('~/Downloads/stock data/',codelist[i],'.csv'),
                 header = TRUE)
  temp<-data.frame(temp)
  temp$ret<-(temp$close/temp$open)-1
  temp$logret<-log(temp$close/temp$open)
  
  #calculate mean and var
  temp$mu<-mean(temp$logret)
  temp$sigmas<-var(temp$logret)
  
  
  temp$date<-as.Date(temp$date)
  temp<-temp[(temp$date>=as.Date('2010-01-01'))&(temp$date<=as.Date('2017-12-31')),]
  assign(codelist[i],temp)
}

#visualize the distribution of close price 
for (i in codelist){
  temp<-get(i)
  xrange=c(min(temp$close),max(temp$close))
  jpeg(paste0('~/Downloads/stock data/',i,'_ClosePrice.jpg'))
  hist(temp$close,main=paste0("Histogram for ",i," closeprice"),
       xlab="ClosePrice",xlim=xrange,breaks=20)
  dev.off()
}

#calculate return and visualize the distribution of return
for (i in codelist){
  temp<-get(i)
  jpeg(paste0('~/Downloads/stock data/',i,'_Return.jpg'))
  hist(temp$ret,main=paste0("Histogram for ",i," returns"),
       xlab="return",xlim=c(min(temp$ret),max(temp$ret)),breaks=50)
  dev.off()
}

#calculate log-return and visualize the distribution of log-return
for (i in codelist){
  temp<-get(i)
  jpeg(paste0('~/Downloads/stock data/',i,'_LogReturn.jpg'))
  hist(temp$logret,main=paste0("Histogram for ",i," log-returns"),
       xlab="log-return",xlim=c(min(temp$logret),max(temp$logret)),breaks=50)
  dev.off()
}



# mean confidence interval
t.test(AAPL$logret, conf.level = 0.95)
t.test(GPS$logret, conf.level = 0.95)
t.test(HNNMY$logret, conf.level = 0.95)
t.test(KO$logret, conf.level = 0.95)
t.test(LULU$logret, conf.level = 0.95)
t.test(NKE$logret, conf.level = 0.95)
t.test(PVH$logret, conf.level = 0.95)
t.test(REV$logret, conf.level = 0.95)
t.test(RL$logret, conf.level = 0.95)
t.test(TIF$logret, conf.level = 0.95)

#variance confidence interval
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

chisq.var.test(AAPL$logret,length(AAPL$logret)-1,a=0.05,alt=2,sigma0=1)
chisq.var.test(GPS$logret,length(GPS$logret)-1,a=0.05,alt=2,sigma0=1)
chisq.var.test(HNNMY$logret,length(HNNMY$logret)-1,a=0.05,alt=2,sigma0=1)
chisq.var.test(KO$logret,length(KO$logret)-1,a=0.05,alt=2,sigma0=1)
chisq.var.test(LULU$logret,length(LULU$logret)-1,a=0.05,alt=2,sigma0=1)
chisq.var.test(NKE$logret,length(NKE$logret)-1,a=0.05,alt=2,sigma0=1)
chisq.var.test(PVH$logret,length(PVH$logret)-1,a=0.05,alt=2,sigma0=1)
chisq.var.test(REV$logret,length(REV$logret)-1,a=0.05,alt=2,sigma0=1)
chisq.var.test(RL$logret,length(RL$logret)-1,a=0.05,alt=2,sigma0=1)
chisq.var.test(TIF$logret,length(TIF$logret)-1,a=0.05,alt=2,sigma0=1)
