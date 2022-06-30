#Environmental Data

library(ggplot2)
library(Hmisc)
library(plotrix)
library(plyr)
library(dplyr)
library(lubridate)
library(hrbrthemes)
options(hrbrthemes.loadfonts = TRUE)

outsidePoundHOBO<-read.csv("HOBOdata.csv")
outsidePoundHOBO$Date.time<-mdy_hms(outsidePoundHOBO$Date.time)
outsidePoundHOBO$Location=as.factor(outsidePoundHOBO$Location)
#outsidePoundHOBO<-outsidePoundHOBO[1:138,]
hrbrthemes::import_roboto_condensed()
ggplot(outsidePoundHOBO, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line()+
  ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))


ggplot(outsidePoundHOBO, aes(x=Date.time, y=High.sal, group=Location, color=Location))+ geom_line()+
  ylab("Salinity")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))

