ap_plot <-function(gage1,gage2,gage3,gage4){
  df = gage1 %>%
    left_join(.,gage2,by=c("Date","WY"))%>%
    left_join(.,gage3,by=c("Date","WY"))%>%
    left_join(.,gage4,by=c("Date","WY")) %>%
    setNames(.,c("Date","Gage_1","WY","Gage_2","Gage_3","Gage_4"))%>%
    pivot_longer(
      cols = starts_with("Gage"),
      names_to = "Gage",
      values_to = "Discharge",
      values_drop_na = TRUE
    )
  df$WY = factor(df$WY)
  ggplot(df) +
    geom_line(aes(x=Date,y=Discharge,color=WY))+
    facet_wrap(~Gage,nrow = 4,scales="free")
}


ap_readNWISdata <- function(sites,service,startDate,endDate){
  gauge = readNWISdata(sites=sites,service=service,startDate=startDate,endDate=endDate,parameterCd="00060")%>%
    addWaterYear()%>%
    select(dateTime,X_00060_00003,waterYear) %>%
    setNames(.,c("Date","Discharge","WY"))
  return(gauge)
}


ap_multiyear <-function(sites,service,startDate,endDate,numYear){
  gauge = readNWISdata(sites=sites,service=service,startDate=ymd(startDate)-years(numYear),endDate=endDate,parameterCd="00060")%>%
    addWaterYear()%>%
    select(dateTime,X_00060_00003,waterYear) %>%
    setNames(.,c("Date","Discharge","WY"))
}



ap_multiyear_plot <-function(gage1,gage2,gage3,gage4){
  df = gage1 %>%
    left_join(.,gage2,by=c("Date","WY"))%>%
    left_join(.,gage3,by=c("Date","WY"))%>%
    left_join(.,gage4,by=c("Date","WY")) %>%
    setNames(.,c("Date","Gage_1","WY","Gage_2","Gage_3","Gage_4"))%>%
    pivot_longer(
      cols = starts_with("Gage"),
      names_to = "Gage",
      values_to = "Discharge",
      values_drop_na = TRUE
    )
  df$WY = factor(df$WY)
  df$Date = lubridate::yday(df$Date)
  ggplot(df) +
    geom_line(aes(x=Date,y=Discharge,color=WY))+
    scale_x_continuous(breaks = c(1, 121, 244, 335), 
                       labels = c("Jan", "May", "Sep", "Dec"))+
    facet_wrap(~Gage,nrow = 4,scales="free")
}


ap_readNWISpeak <- function(siteNumbers){
  gage = readNWISpeak(siteNumbers)%>%
    select(peak_dt,peak_va)%>%
    setNames(c("Date","Discharge"))
  gage$Date = lubridate::ymd(gage$Date)
  return(gage)
}

ap_peakPlot <-function(peakData){
  year = ggplot(peakData)+
    geom_col(aes(x=Date,y=Discharge))+
    scale_fill_viridis_c()+
    xlab("Year")+
    ylab("Discharge (cfs)")+
    theme(legend.position = "none")+
    theme_minimal()
  hist =ggplot(peakData)+
    geom_histogram(aes(x=lubridate::month(Date,label=T)),stat="count")+ 
    xlab("Month")+
    ylab("Number of Peaks")+
    theme_minimal()
  library(patchwork)
  hist / year
}
