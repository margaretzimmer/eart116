
#####Load packages here####
library(ggplot2)
library(tidyverse)
library(lubridate)
library(reshape2)
library(dataRetrieval) #This is the open source package from the USGS "dataRetreival"
source("functions/functions.R") # These are pre-canned functions that I have made. Look in the functions folder if you are interested in these.

piedra = ap_readNWISdata(sites='09349800',service='dv',startDate="2017-10-01",endDate="2018-09-30")

gauge = readNWISdata(sites='09349800',service='dv',startDate="2017-10-01",endDate="2018-09-30",parameterCd="00060") %>%
  addWaterYear()%>%
  select(dateTime,X_00060_00003,waterYear) %>%
  setNames(.,c("Date","Discharge","WY"))

ap_readNWISdata <- function(sites,service,startDate,endDate){
  gauge = readNWISdata(sites=sites,service=service,startDate=startDate,endDate=endDate,parameterCd="00060")%>%
    addWaterYear()%>%
    select(dateTime,X_00060_00003,waterYear) %>%
    setNames(.,c("Date","Discharge","WY"))
  return(gauge)
}

### site2
site2name = ap_readNWISdata(sites='sitenumber',service='dv',startDate="YYYY-MM-DD",endDate="YYYY-MM-DD")

### site3
site3name = ap_readNWISdata(sites='sitenumber',service='dv',startDate="YYYY-MM-DD",endDate="YYYY-MM-DD")

### site4
site4name = ap_readNWISdata(sites='sitenumber',service='dv',startDate="YYYY-MM-DD",endDate="YYYY-MM-DD")

ap_plot(piedra,site2name,site3name,site4name)


ap_multiyear <-function(sites,service,startDate,endDate,numYear){
  gauge = readNWISdata(sites=sites,service=service,startDate=ymd(startDate)-years(numYear),endDate=endDate,parameterCd="00060")%>%
    addWaterYear()%>%
    select(dateTime,X_00060_00003,waterYear) %>%
    setNames(.,c("Date","Discharge","WY"))
}

piedra_multiyear = ap_multiyear(sites='09380000',service='dv',startDate='2017-10-01',endDate='2018-09-30',numYear = 7)


### site2
site2name_multiyear = ap_multiyear()

### site3
site3name_multiyear = ap_multiyear()

### site4
site4name_multiyear = ap_multiyear()

summary(piedra_multiyear)

summary()

summary()

summary()

ap_multiyear_plot()

ggplot(piedra_multiyear)+
  geom_line(aes(x=lubridate::yday(Date),y=Discharge,color=factor(WY)))+
  scale_color_discrete(type = "viridis",)+
  xlab(label="Date")+
  theme_minimal()



piedra_peak = ap_readNWISpeak('09349800')
print(piedra_peak)

gauge2_peak = ap_readNWISpeak()
print()

gauge4_peak = ap_readNWISpeak()
print()

gauge4_peak = ap_readNWISpeak()
print()

piedra =readNWISsite ('09349800')
unknown1 =readNWISsite ('09402000')
unknown2 =readNWISsite ('07010000')
unknown3 =readNWISsite ('09380000')


print(piedra$station_nm)
print(unknown1$station_nm)
print(unknown2$station_nm)
print(unknown3$station_nm)
