library(forecast)

library(tidyverse)

library(gridExtra)

#series,date_vector

#date_vector=date_vector_1
#series=sr_1

ARIMA_ANOMAL<-function(series,date_vector,level=0.90){
  multiplier=qnorm(1-(1-level)/2)
  aa=auto.arima(series,trace = F)
  upper=fitted(aa) + multiplier*sqrt(aa$sigma2)
  lower=fitted(aa) - multiplier*sqrt(aa$sigma2)
  dd=data.frame(DATE=date_vector,Fitted=aa$fitted,Upper=upper,
                Lower=lower,Series=series)
  
  Ano_nor=rep("NORMAL",length(as.vector(date_vector)))
  
  Ano_nor[which(dd$Series>dd$Upper | dd$Series<dd$Lower)]="ABNORMAL"
  
  dd$Ano_nor=Ano_nor
  
  ABNORMAL_dd=dd[which(Ano_nor=="ABNORMAL"),]
  
  Plot=ggplot(dd,aes(x=DATE,y=Series)) + 
    geom_line(col='grey30',size=.7)+
    geom_ribbon(aes(x=DATE,ymin=Lower, ymax=Upper), alpha=0.20)+
    geom_point(aes(color=Ano_nor))+
    geom_text(data=ABNORMAL_dd,aes(x=DATE,y=Series,label=DATE),
              size=2)
  
  ABNORMAL_DATES=ABNORMAL_dd$DATE
  
  l=list(Model=aa,Plot=Plot,ABNORMAL_DATES=ABNORMAL_DATES)
  
  return(l)
}

####################################################################
anomal<-function(series,date_vector,logic="IQR"){
  
  sr=series
  
  sr_decompose=decompose(sr)
  
  random=sr_decompose$random
  
  random=na.omit(random)
  
  if(logic=="IQR"){
    random_upper=quantile(random,.75)+1.5*IQR(random)
    random_lower=quantile(random,.25)-1.5*IQR(random)
  }
  
  else if(logic=="NORMALITY"){
    random_upper=mean(random)+sd(random)
    random_lower=mean(random)-sd(random)
  }
  
  else{
    MAD=mad(random)
    random_upper=mad(random)
    random_lower=(-mad(random))
  }
  
  
  Ano_nor=rep("NORMAL",length(date_vector))
  Ano_nor[which(random>random_upper | random<random_lower)+3]="ABNORMAL"
  
  
  Date=date_vector
  
  dd=data.frame(sr,DATE=Date,TREND=sr_decompose$trend,
                SEASONAL=sr_decompose$seasonal,
                RANDOM=sr_decompose$random,Ano_nor)
  
  SERIES=ggplot(data=dd,aes(x=DATE,y=sr))+
    geom_line(col='grey30',size=.7)+
    geom_point(size=1.3,aes(color=Ano_nor))+
    ylab("SERIES")
  
  TREND=ggplot(data=dd,aes(x=DATE,y=TREND))+
    geom_line(col='grey30',size=.9)+
    ylab("TREND")#+geom_point(size=1.3,aes(color=Ano_nor))
  
  SEASONAL=ggplot(data=dd,aes(x=DATE,y=SEASONAL))+
    geom_line(col='grey30',size=.7)+
    ylab("SEASONAL")#+geom_point(size=1.3,aes(color=Ano_nor))
  
  
  ABNORMAL_dd=dd[which(Ano_nor=="ABNORMAL"),]
  
  RANDOM=ggplot(data=dd,aes(x=DATE,y=RANDOM))+
    geom_line(col='grey30',size=.7)+
    ylab("RANDOM")+geom_point(size=1.3,aes(color=Ano_nor))+
    geom_hline(yintercept = random_upper,col="grey60")+
    geom_hline(yintercept = random_lower,col="grey60")+
    geom_text(data=ABNORMAL_dd,aes(label=DATE),size=2,
              vjust=-.5*as.numeric(ABNORMAL_dd$RANDOM>0,
                                   check_overlap = T,
                                   angle = 45))
  
  DECOMPOSED_PLOT=grid.arrange(SERIES,TREND,SEASONAL,RANDOM,ncol=1)
  
  anomal_all=list(ABNORMAL_DATES=ABNORMAL_dd$DATE,
                  SERIES=SERIES,
                  SEASONAL=SEASONAL,
                  RANDOM=RANDOM,
                  DECOMPOSED_PLOT=DECOMPOSED_PLOT
  )
  return(anomal_all)
  
  
}


#final=function(series,date_vector,logic="IQR"){
#  if(logic=="ARIMA"){res=ARIMA_ANOMAL(series,date_vector)}
#  else{res=anomal(series,date_vector,logic="IQR")}
#  rerun(res)
#}


aaa=ARIMA_ANOMAL(AirPassengers,1:length(AirPassengers))
aaa