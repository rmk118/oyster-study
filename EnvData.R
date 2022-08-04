#Environmental Data
#RK 8/1/22

library(ggplot2)
library(ggsci)
library(ggpubr)
library(gridExtra)
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
library(viridis)
library(mgcv)
library(visreg)
library(sm)
library(patchwork)

#Import data
HOBOdata_unordered<-read.csv("HOBOdata.csv")

#Convert date and location
HOBOdata_unordered$Date.time<-mdy_hms(HOBOdata_unordered$Date.time)
HOBOdata<-HOBOdata_unordered[order(HOBOdata_unordered$Location),]
HOBOdata$Location=as.factor(HOBOdata$Location)

#Rename salinity column
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

#Rolling daily average all temps
DailyAvgTemp<-ggplot(HOBOdata, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line(alpha=0.4)+geom_ma(n=24, linetype="solid")+
  ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))
DailyAvgTemp

#Rolling daily average no  salinity <5
tempNoLowSalRolling<-ggplot(noLowSal, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line(alpha=0.4)+geom_ma(n=24, linetype="solid")+
  ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))
tempNoLowSalRolling

#Data from only 05:00 to compare to Leeman et al.
# InsideCommonDay<-InsideCommon[hour(InsideCommon$Date.time) %in% (5:15),]
# OutsideCommonDay<-OutsideCommon[hour(OutsideCommon$Date.time) %in% (5:15),]
# diffDay<-InsideCommonDay$Temp-OutsideCommonDay$Temp
# mean(diffDay)
# summary(diffDay)

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

#Temperature differences
Inside<-noLowSal[noLowSal$Location=="Inside",]
Outside<-noLowSal[noLowSal$Location=="Outside",]
InsideDates<-Inside$Date.time
OutsideDates<-Outside$Date.time
commonDates<-base::intersect(InsideDates, OutsideDates)
common<-noLowSal[(noLowSal$Date.time %in% commonDates),]

InsideCommon<-common[common$Location=='Inside',]
OutsideCommon<-common[common$Location=='Outside',]
differences<-InsideCommon$Temp-OutsideCommon$Temp
meanTempDiff<-mean(differences)
meanTempDiff #inside averaged 0.650 deg. C warmer
seTempDiff<-std.error(differences)
seTempDiff #0.0187
(length(differences[differences>0]))/(length(differences)) #percentage of hours where inside was warmer = 94.3%

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


mean(noLowSal[noLowSal$Location=="Inside", "Temp"]) #mean inside temp 18.09568
max(noLowSal[noLowSal$Location=="Inside", "Temp"]) #24.07
sd(noLowSal[noLowSal$Location=="Inside", "Temp"]) #sd inside temp 2.10

mean(noLowSal[noLowSal$Location=="Outside", "Temp"]) #mean outside temp 17.39034
max(noLowSal[noLowSal$Location=="Outside", "Temp"]) #22.53
sd(noLowSal[noLowSal$Location=="Outside", "Temp"]) #sd outside temp 1.97

min(both_rolling, na.rm=TRUE) #min mean daily temp = 13.67
max(both_rolling, na.rm=TRUE) #max mean daily temp = 21.53

a<-mean(commonInside2$daily_avg, na.rm=TRUE) #18.10601
b<-mean(commonOutside2$out_daily_avg, na.rm=TRUE) #17.45055
a-b # 0.655

rolling_diffs<-commonInside2$daily_avg-commonOutside2$out_daily_avg #difference in rolling daily mean between locations
mean(rolling_diffs, na.rm=TRUE) #mean rolling daily avg. is 0.655 deg. C higher inside
length(rolling_diffs)
length(rolling_diffs[rolling_diffs>0]) #inside daily mean always higher
length(rolling_diffs[rolling_diffs>0.5])/1060 #78.9% diff was >0.5 deg. C
max(rolling_diffs, na.rm = TRUE) #max 1.19 deg. C higher inside
min(rolling_diffs, na.rm = TRUE) #min 0.1 deg. C higher inside

DailyAvgTemp2<-ggplot(common, aes(x=Date.time, y=rolling, group=Location, color=Location))+geom_line()+
  ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))#+ylim(12.5, 22.5)
DailyAvgTemp2

grid.arrange(tempNoLowSalRolling,DailyAvgTemp2, ncol=1) #shows that using the geom_ma function gives the same graph as manually calculated rolling average

#Salinity analysis
sal_differences<-InsideCommon$Salinity-OutsideCommon$Salinity
meanSalDiff<-mean(sal_differences)
meanSalDiff #inside averages 0.28 mS/L higher
sd_sal_Diff<-sd(sal_differences)
sd_sal_Diff #2.136
(length(sal_differences[sal_differences>0]))/(length(sal_differences)) #percentage of hours where inside had higher salinity = 73.9% 

commonSalInside2 <- InsideCommon %>%
  select(Location, Date.time, Salinity) %>%
  mutate(daily_sal_avg= rollmean(Salinity, k = 24, fill = NA))

commonSalOutside2 <- OutsideCommon %>%
  select(Location, Date.time, Salinity) %>%
  mutate(out_sal_daily_avg= rollmean(Salinity, k = 24, fill = NA))

both_sal_rolling<-c(commonSalInside2$daily_sal_avg, commonSalOutside2$out_sal_daily_avg)
common$sal_rolling<-both_sal_rolling

DailyAvgSal2<-ggplot(common, aes(x=Date.time, y=sal_rolling, group=Location, color=Location))+geom_line()+ylab("Conductivity (mS/cm)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))+ylim(25,35)
DailyAvgSal2

rolling_sal_diffs<-commonSalInside2$daily_sal_avg-commonSalOutside2$out_sal_daily_avg
mean(rolling_sal_diffs, na.rm = TRUE) #0.290
length(rolling_sal_diffs) #1060
length(rolling_sal_diffs[rolling_sal_diffs>0]) #797
length(rolling_sal_diffs[rolling_sal_diffs>0])/length(rolling_sal_diffs) #75.2%

mean(commonSalInside2$daily_sal_avg, na.rm=TRUE) #31.68435
sd(commonSalInside2$daily_sal_avg, na.rm=TRUE) #1.44
mean(commonSalOutside2$out_sal_daily_avg, na.rm=TRUE) #31.39409
sd(commonSalOutside2$out_sal_daily_avg, na.rm=TRUE) #0.43

min(both_sal_rolling, na.rm=TRUE) #min mean daily sal = 26.48
max(both_sal_rolling, na.rm=TRUE) #max mean daily sal = 33.96

###############################################################################
######################### ChlA/Turbidity  ########################################

dateFix = function(df) {
  df$Trial_Date = as.Date(df$Trial, "%m/%d/%y")
  return(df)
}

# Reading Data for Algae Chla extracted values data sheet
chlaDatasheet = read.csv("chlA.csv")
chlaDatasheet2 = read.csv("chlA_updated.csv")

ChlaDatasheet = dateFix(chlaDatasheet)
ChlaDatasheet2 = dateFix(chlaDatasheet2)
ChlaDatasheet2 = ChlaDatasheet2[ChlaDatasheet2$Major_issue == FALSE,]

#ChlaDatasheet = select(ChlaDatasheet,-c(21,22,23,24,25))
ChlFs = 0.000482
FoFa_max = 1.7718

#Calculating Chla ug/L and Phaeo ug/L from Raw Data
ChlaDatasheet = ChlaDatasheet %>%
  mutate(Ave_Chl1 = (ChlFs*(FoFa_max/(FoFa_max-1))* 
                       (ChlaDatasheet$Fo-ChlaDatasheet$Fa)*
                       (((ChlaDatasheet$Acetone_vol)/ChlaDatasheet$Vol_Filtered))))

ChlaDatasheet = ChlaDatasheet %>%
  mutate(ChlaDatasheet, Ave_Phaeo1 = ((ChlFs*(FoFa_max/(FoFa_max-1)))*
                                        ((FoFa_max-1)*(ChlaDatasheet$Fo-ChlaDatasheet$Fa))*
                                        (((ChlaDatasheet$Acetone_vol)/ChlaDatasheet$Vol_Filtered))))

#Calculating Chla ug/L and Phaeo ug/L from Raw Data
ChlaDatasheet2 = ChlaDatasheet2 %>%
  mutate(Ave_Chl1 = (ChlFs*(FoFa_max/(FoFa_max-1))* 
                       (ChlaDatasheet2$Fo-ChlaDatasheet2$Fa)*
                       (((ChlaDatasheet2$Acetone_vol)/ChlaDatasheet2$Vol_Filtered))))

ChlaDatasheet2 = ChlaDatasheet2 %>%
  mutate(ChlaDatasheet2, Ave_Phaeo1 = ((ChlFs*(FoFa_max/(FoFa_max-1)))*
                                        ((FoFa_max-1)*(ChlaDatasheet2$Fo-ChlaDatasheet2$Fa))*
                                        (((ChlaDatasheet2$Acetone_vol)/ChlaDatasheet2$Vol_Filtered))))

#Function to calculate mean and standard error
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      SE = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)
}

#Create summary data frame
df2<-data_summary(ChlaDatasheet, "Ave_Chl1", 
                         groupnames=c("Trial_Date", "Location"))

chlA_graph<-ggplot(df2, aes(x=Trial_Date, y=mean, group=Location, color=Location)) + 
  geom_line()+ylab("Chlorophyll A (μg/L)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 15, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))
chlA_graph

#INCLUDING 8/2
df_updated<-data_summary(ChlaDatasheet2, "Ave_Chl1", 
                         groupnames=c("Trial_Date", "Location"))
#with error bars
chlA_graph3<-ggplot(df_updated, aes(x=Trial_Date, y=mean, group=Location, color=Location)) + 
  geom_line()+ylab("Chlorophyll A (μg/L)")+
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                position=position_dodge(0.05))+theme_classic()+scale_y_continuous(limits=c(0,17))
chlA_graph3

chlA_graph4<-ggplot(df_updated, aes(x=Trial_Date, y=mean, group=Location, color=Location)) + 
  geom_line()+ylab("Chlorophyll A (μg/L)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+scale_y_continuous(limits=c(0,16))
#+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))

chlA_graph4

df_updated<-df_updated[c(1:14),]
#Average difference
OutsideChlA<-df_updated[df_updated$Location=="Outside",'mean']
InsideChlA<-df_updated[df_updated$Location=="Inside",'mean']

differencesChlA<-data.frame(OutsideChlA,InsideChlA)
differencesChlA$Diff<-differencesChlA$InsideChlA-differencesChlA$OutsideChlA
meanDiffChlA<-mean(differencesChlA$Diff)
meanDiffChlA #0.73 μg/L
sdDiffChlA<-sd(differencesChlA$Diff)
sdDiffChlA #2.08

#USE THIS SECTION IN POSTER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

chlA_graph2<-ggplot(df_updated, aes(x=Trial_Date, y=mean, group=Location, color=Location)) + 
  geom_line()+ylab("Chlorophyll A (μg/L)")+xlab("")+theme_classic()+theme(axis.title.y = element_text(margin = margin(r = 10)))#+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)
chlA_graph2

chlA_graph2_themed<-ggplot(df_updated, aes(x=Trial_Date, y=mean, group=Location, color=Location)) + 
  geom_line()+ylab("Chlorophyll A (μg/L)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)#scale_y_continuous(limits=c(0,10.5))
chlA_graph2_themed

DailyAvgSal2<-ggplot(common, aes(x=Date.time, y=sal_rolling, group=Location, color=Location))+geom_line()+
  ylab("Conductivity (mS/cm)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+ylim(25,35)+
  theme(axis.title.y = element_text(margin = margin(r = 10)))
DailyAvgSal2

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
grid.arrange(chlA_graph, chlA_graph2, ncol=2) #updating vol filtered and removing sample data where major spills occurred has minimal impact on graph

###############################################################################
######################### Turbidity  ########################################
###############################################################################

turbidity<-read.csv("turbidity.csv")

#Create summary data frame
df_turbdity<-data_summary(turbidity, "Turbidity", 
                  groupnames=c("Date", "Location"))

df_turbdity$Date <- mdy(df_turbdity$Date)

#Plot
turbidity_graph<-ggplot(df_turbdity, aes(x=Date, y=mean, group=Location, color=Location)) + 
  geom_line()+theme_classic()+scale_y_continuous(limits=c(0,4.5))+ylab("Turbidity (NTU)")+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))
  #+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+
turbidity_graph
turbidity_graph_themed<-turbidity_graph+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))

#Average difference
OutsideTurbidity<-df_turbdity[df_turbdity$Location=="Outside",'mean']
InsideTurbidity<-df_turbdity[df_turbdity$Location=="Inside",'mean']
differences_turbidity<-data.frame(OutsideTurbidity,InsideTurbidity)
differences_turbidity$Diff<-differences_turbidity$OutsideTurbidity-differences_turbidity$InsideTurbidity
meanTurbidityDiff<-mean(differences_turbidity$Diff)
meanTurbidityDiff #1.07 NTU
sdTurbidityDiff<-sd(differences_turbidity$Diff)
sdTurbidityDiff #0.84 NTU

mean(OutsideTurbidity) #2.91
sd(OutsideTurbidity) #1.06
mean(InsideTurbidity) #1.84
sd(InsideTurbidity) #0.79
###############################################################################
######################### Combined graphs  ########################################
###############################################################################

#Temp, salinity, ChlA
combined <- DailyAvgTemp2/(DailyAvgSal2 + chlA_graph) + plot_layout(nrow=2, byrow=FALSE, guides = "collect") & theme(legend.position = "bottom")
combined

#Temp, salinity, ChlA, turbidity
allFour<-DailyAvgTemp2/(DailyAvgSal2 + chlA_graph2 + turbidity_graph_themed) + plot_layout(nrow=2, byrow=FALSE, guides = "collect") & theme(legend.position = "bottom")
allFour

#ChlA and turbidity, theme_classic
chlA_graph2 + turbidity_graph + plot_layout(nrow=1, guides = "collect") & theme(legend.position = "bottom")

#ChlA and turbidity, theme_ipsum
combined2<-chlA_graph2_themed + turbidity_graph_themed + plot_layout(nrow=1, guides = "collect") & theme(legend.position = "bottom")
combined2+ plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14))
