ap_plot <-function(gauge1,gauge2,gauge3,gauge4){
    par(mfrow=c(2,2))
    plot(seq(as.Date("2017-10-01"), by='days', length.out=length(gauge1)),gauge1, main=deparse(substitute(gauge1)),ylab='Streamflow (cfs)',xlab='Month of Water-Year',type='l')
    plot(seq(as.Date("2017-10-01"), by='days', length.out=length(gauge2)),gauge2, main=deparse(substitute(gauge2)),ylab='Streamflow (cfs)',xlab='Month of Water-Year',type='l')
    plot(seq(as.Date("2017-10-01"), by='days', length.out=length(gauge3)),gauge3, main=deparse(substitute(gauge3)),ylab='Streamflow (cfs)',xlab='Month of Water-Year',type='l')
    plot(seq(as.Date("2017-10-01"), by='days', length.out=length(gauge4)),gauge4, main=deparse(substitute(gauge4)),ylab='Streamflow (cfs)',xlab='Month of Water-Year',type='l')
}


ap_readNWISdata <- function(sites,service,startDate,endDate){
    gauge = readNWISdata(sites=sites,service=service,startDate=startDate,endDate=endDate,parameterCd="00060")
    return(gauge$X_00060_00003)
}


ap_multiyear <-function(sites,service,startDate,endDate){
    gauge <- NULL
    fake = seq(1:365)
    for (i in 1:5){
        startDate = startDate
        endDate = endDate
        s <- as.POSIXlt(as.Date(startDate))
        e <- as.POSIXlt(as.Date(endDate))
        s$year <- s$year-i
        e$year <- e$year-i
        startDate = as.Date(s)
        endDate = as.Date(e)
        tmp = readNWISdata(sites=sites,service='dv',startDate=startDate,endDate=endDate)
        length(tmp) = length(fake)
        tmp = tmp$X_00060_00003
        gauge <-cbind(gauge,tmp)
        }
gauge = data.frame(gauge)
date = seq(as.Date("2017-10-01"), by='days', length.out=365)
gauge <-cbind(gauge,date)
names(gauge)<-c('WY2018','WY2017','WY2016','WY2015','WY2014','Date')
#names(gauge)<-c('WY2018','WY2017','WY2016','WY2015','WY2014','WY2013','WY2012','WY2011','WY2010','WY2009','Date')
gauge <- melt(gauge,id.vars="Date")
}


ap_multiyear_plot<-function(gauge){
    ggplot(gauge, aes(Date,value, col=variable)) + 
  geom_line() +labs(x = "Month of Water Year", colour = "WY", y = "Discharge (cfs)")
}

