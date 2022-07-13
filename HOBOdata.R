#HOBO Data
#RK 7/13/22

library(ggplot2)
library(Hmisc)
library(plotrix)
library(plyr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(hrbrthemes)
options(hrbrthemes.loadfonts = TRUE)
hrbrthemes::import_roboto_condensed()
library(tidyquant)

#Import data
HOBOdata<-read.csv("HOBOdata.csv")

#Convert date and location
HOBOdata$Date.time<-mdy_hms(HOBOdata$Date.time)
HOBOdata$Location=as.factor(HOBOdata$Location)

names(HOBOdata)[3]<-"Salinity"

#Temperature ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Temperature Plot all data
allTemps<-ggplot(HOBOdata, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line()+
  ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))
allTemps

#Temperature Plot no salinity <5
noLowSal<- HOBOdata[HOBOdata$Salinity>5,]
tempNoLowSal<-ggplot(noLowSal, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line()+
  ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))
tempNoLowSal

#Rolling daily Average all temps
DailyAvgTemp<-ggplot(HOBOdata, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line(alpha=0.4)+geom_ma(n=24, linetype="solid")+
  ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))
DailyAvgTemp

#Rolling Daily Average no low sal
noLowSal<- HOBOdata[HOBOdata$Salinity>5,]
tempNoLowSalRolling<-ggplot(noLowSal, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line(alpha=0.4)+geom_ma(n=24, linetype="solid")+
  ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))
tempNoLowSalRolling

#Two different temperature plots
grid.arrange(allTemps, tempNoLowSal, ncol=1)

#Salinity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Salinity Plot all data
allSalinity<-ggplot(HOBOdata, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line()+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="All salinity data")

allSalinity

#Salinity Plot no salinity <5
salinityNoLowSal<-ggplot(noLowSal, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line()+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="All salinity data >5")

salinityNoLowSal

#Salinity Plot no salinity <5 ROLLING AVERAGE
salinityNoLowSalRolling<-ggplot(noLowSal, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line(alpha=0.4)+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+geom_ma(n=24, linetype="solid")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="All salinity data >5")
salinityNoLowSalRolling

#Salinity Plot no salinity <25
noLowerSal<- HOBOdata[HOBOdata$Salinity>25,]
salinityNoLowerSal<-ggplot(noLowerSal, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line()+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="All salinity data >25")
salinityNoLowerSal

#Salinity Plot no salinity <25 ROLLING AVERAGE
salinityNoLowerSalRolling<-ggplot(noLowerSal, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line(alpha=0.4)+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+geom_ma(n=24, linetype="solid")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="All salinity data >25")
salinityNoLowerSalRolling

#Salinity plot starting 6/17
starting6.17<-HOBOdata[HOBOdata$Date.time>"2022-06-17 04:00:00",]
salinity.delayedStart<-ggplot(starting6.17, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line()+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="All salinity data starting 6/17")
salinity.delayedStart

#Salinity plot starting 6/17 no salinity <5
starting6.17NoLowSal<-HOBOdata[HOBOdata$Date.time>"2022-06-17 04:00:00" & HOBOdata$Salinity >5,]

salinity.delayedStart.noLowSal<-ggplot(starting6.17NoLowSal, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line()+
 theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="Salinity data >5 starting 6/17")
salinity.delayedStart.noLowSal

#Salinity plot starting 6/17 no salinity <5 ROLLING AVERAGE
salinity.delayedStart.noLowSalRolling<-ggplot(starting6.17NoLowSal, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line(alpha=0.4)+geom_ma(n=24, linetype="solid")+
  theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="Salinity data >5 starting 6/17")
salinity.delayedStart.noLowSalRolling

#Salinity plot starting 6/17 no salinity <25
starting6.17NoLowerSal<-HOBOdata[HOBOdata$Date.time>"2022-06-17 04:00:00" & HOBOdata$Salinity>25,]

salinity.delayedStart.noLowerSal<-ggplot(starting6.17NoLowerSal, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line()+
theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="Salinity data >25 starting 6/17")
salinity.delayedStart.noLowerSal

#Salinity plot starting 6/17 no salinity <25 ROLLING AVERAGE
salinity.delayedStart.noLowerSalRolling<-ggplot(starting6.17NoLowerSal, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line(alpha=0.4)+geom_ma(n=24, linetype="solid")+
  theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="Salinity data >25 starting 6/17")
salinity.delayedStart.noLowerSalRolling

#Six different salinity plots
grid.arrange(allSalinity, salinityNoLowSal, salinityNoLowerSal, salinity.delayedStart, salinity.delayedStart.noLowerSal, salinity.delayedStart.noLowSal, ncol=2)

