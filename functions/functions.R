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




multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

ap_multiyear_plot <- function(gauge1,gauge2,gauge3,gauge4) {
    p1 <- ggplot(gauge1, aes(x=gauge1$Date, y=gauge1$value, colour=gauge1$variable))+
    geom_line() +
    ggtitle(deparse(substitute(gauge1)))+xlab("Date")+ylab("Discharge(CFS)")+labs(colour="WY")
    
    p1h <- ggplot(gauge1, aes(gauge1$value))+
    geom_histogram() +
    ggtitle(deparse(substitute(gauge1)))+ xlab("Discharge (CFS)")
    
    p2 <- ggplot(gauge2, aes(x=gauge2$Date, y=gauge2$value, colour=gauge2$variable))+
    geom_line() +
    ggtitle(deparse(substitute(gauge2)))+xlab("Date")+ylab("Discharge(CFS)")+labs(colour="WY")
    
    p2h <- ggplot(gauge2, aes(gauge2$value))+
    geom_histogram() +
    ggtitle(deparse(substitute(gauge2)))+ xlab("Discharge (CFS)")
    
    p3 <- ggplot(gauge3, aes(x=gauge3$Date, y=gauge3$value, colour=gauge3$variable))+
    geom_line() +
    ggtitle(deparse(substitute(gauge3)))+xlab("Date")+ylab("Discharge(CFS)")+labs(colour="WY")

    p3h <- ggplot(gauge3, aes(gauge3$value))+
    geom_histogram() +
    ggtitle(deparse(substitute(gauge3)))+ xlab("Discharge (CFS)")
    
    p3 + labs(aesthetic="WY")
    
    p4 <- ggplot(gauge4, aes(x=gauge4$Date, y=gauge4$value, colour=gauge4$variable))+
    geom_line() +
    ggtitle(deparse(substitute(gauge4)))+xlab("Date")+ylab("Discharge(CFS)")+labs(colour="WY")

    p4h <- ggplot(gauge4, aes(gauge4$value))+
    geom_histogram() +
    ggtitle(deparse(substitute(gauge4))) + xlab("Discharge (CFS)")
    
    multiplot(p1,p2,p3,p4, cols=1)
    
    }

ap_readNWISpeak <- function(siteNumbers){
    gauge = readNWISpeak(siteNumbers)
    time = as.data.frame.Date(gauge$peak_dt)
    vel = gauge$peak_va
    cbind(time,vel)
      
}