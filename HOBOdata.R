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
HOBOdata_unordered<-read.csv("HOBOdata.csv")

#Convert date and location
HOBOdata_unordered$Date.time<-mdy_hms(HOBOdata_unordered$Date.time)
HOBOdata<-HOBOdata_unordered[order(HOBOdata_unordered$Location),]
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
tempNoLowSalRolling<-ggplot(noLowSal, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line(alpha=0.4)+geom_ma(n=24, linetype="solid")+
  ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))
tempNoLowSalRolling

#grid.arrange(tempNoLowSalRolling,DailyAvgTemp2, ncol=1)

# InsideCommonDay<-InsideCommon[hour(InsideCommon$Date.time) %in% (5:15),]
# OutsideCommonDay<-OutsideCommon[hour(OutsideCommon$Date.time) %in% (5:15),]
# diffDay<-InsideCommonDay$Temp-OutsideCommonDay$Temp
# mean(diffDay)
# summary(diffDay)

rolling_diffs<-commonInside2$daily_avg-commonOutside2$out_daily_avg
length(rolling_diffs)
length(rolling_diffs[rolling_diffs>0])
length(rolling_diffs[rolling_diffs>0.5])
max(rolling_diffs, na.rm = TRUE)
min(rolling_diffs, na.rm = TRUE)

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

sal_differences<-InsideCommon$Salinity-OutsideCommon$Salinity
meanSalDiff<-mean(sal_differences)
meanSalDiff
se_sal_Diff<-std.error(sal_differences)
se_sal_Diff
(length(sal_differences[sal_differences>0]))/(length(sal_differences)) #percentage of hours where inside was warmer

commonSalInside2 <- InsideCommon %>%
  select(Location, Date.time, Salinity) %>%
  mutate(daily_sal_avg= rollmean(Salinity, k = 24, fill = NA))

commonSalOutside2 <- OutsideCommon %>%
  select(Location, Date.time, Salinity) %>%
  mutate(out_sal_daily_avg= rollmean(Salinity, k = 24, fill = NA))

both_sal_rolling<-c(commonSalInside2$daily_sal_avg, commonSalOutside2$out_sal_daily_avg)
common$sal_rolling<-both_sal_rolling



#USE THIS SECTION IN POSTER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Average difference
Inside<-noLowSal[noLowSal$Location=="Inside",]
Outside<-noLowSal[noLowSal$Location=="Outside",]
InsideDates<-Inside$Date.time
OutsideDates<-Outside$Date.time
commonDates<-base::intersect(InsideDates, OutsideDates)
common<-noLowSal[(noLowSal$Date.time %in% commonDates),]

InsideCommon<-common[common$Location=='Inside',]
OutsideCommon<-common[common$Location=='Outside',]
differences<-InsideCommon$Temp-OutsideCommon$Temp
meanDiff<-mean(differences)
meanDiff
seDiff<-std.error(differences)
seDiff
(length(differences[differences>0]))/(length(differences)) #percentage of hours where inside was warmer

commonInside2 <- InsideCommon %>%
  select(Location, Date.time, Temp) %>%
  mutate(daily_avg= rollmean(Temp, k = 24, fill = NA),
         daily_03 = rollmean(Temp, k = 72, fill = NA))

commonOutside2 <- OutsideCommon %>%
  select(Location, Date.time, Temp) %>%
  mutate(out_daily_avg= rollmean(Temp, k = 24, fill = NA),
         out_daily_03 = rollmean(Temp, k = 72, fill = NA))

both_rolling<-c(commonInside2$daily_avg, commonOutside2$out_daily_avg)
common$rolling<-both_rolling


DailyAvgTemp2<-ggplot(common, aes(x=Date.time, y=rolling, group=Location, color=Location))+geom_line()+
  ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))#+ylim(12.5, 22.5)
DailyAvgTemp2

DailyAvgSal2<-ggplot(common, aes(x=Date.time, y=sal_rolling, group=Location, color=Location))+geom_line()+
  ylab("Salinity")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+ylim(25,35)
DailyAvgSal2

rolling_sal_diffs<-commonSalInside2$daily_sal_avg-commonSalOutside2$out_sal_daily_avg
length(rolling_sal_diffs)
length(rolling_sal_diffs[rolling_sal_diffs>0])
length(rolling_sal_diffs[rolling_sal_diffs>0])/length(rolling_sal_diffs)

library(patchwork)
combined <- DailyAvgTemp2/DailyAvgSal2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
combined
