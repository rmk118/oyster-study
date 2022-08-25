#Environmental Data
#RK 8/25/22

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

######################### Part 1: HOBO Data  ########################################
#Import HOBO data
HOBOdata_unordered<-read.csv("HOBOdata.csv")

#Convert date and location
HOBOdata_unordered$Date.time<-mdy_hms(HOBOdata_unordered$Date.time)
HOBOdata<-HOBOdata_unordered[order(HOBOdata_unordered$Location),]
HOBOdata$Location<-as.factor(HOBOdata$Location)

#Rename salinity column
names(HOBOdata)[3]<-"Salinity"

noAir<- HOBOdata[HOBOdata$Salinity>5,]

#Part 1a: Temperature Plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Temperature Plot all data
allTemps<-ggplot(HOBOdata, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line()+ylab("Temperature (°C)")+xlab("")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+theme(axis.title.y = element_text(margin = margin(r = 10)))
allTemps

#Temperature Plot no salinity <5
tempNoAir<-ggplot(noAir, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line()+ylab("Temperature (°C)")+xlab("")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+theme(axis.title.y = element_text(margin = margin(r = 10)))
tempNoAir

#Rolling daily average all temps
DailyAvgTemp<-ggplot(HOBOdata, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line(alpha=0.4)+geom_ma(n=24, linetype="solid")+ylab("Temperature (°C)")+xlab("")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+theme(axis.title.y = element_text(margin = margin(r = 10)))
DailyAvgTemp

#Rolling daily average temp no  salinity <5
tempNoAirRolling<-ggplot(noAir, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line(alpha=0.4)+geom_ma(n=24, linetype="solid")+ylab("Temperature (°C)")+xlab("")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+theme(axis.title.y = element_text(margin = margin(r = 10)))
tempNoAirRolling

#Part 1b: Salinity Plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Salinity Plot all data
allSalinity<-ggplot(HOBOdata, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line()+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Conductivity (mS/cm)", subtitle="All salinity data")
allSalinity

#Salinity Plot no salinity <5
salinityNoAir<-ggplot(noAir, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line()+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Conductivity (mS/cm)", subtitle="All salinity data >5")
salinityNoAir

#Salinity Plot no salinity <5 ROLLING AVERAGE
salinityNoAirRolling<-ggplot(noAir, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line(alpha=0.4)+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+geom_ma(n=24, linetype="solid")+theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Conductivity (mS/cm)") #, subtitle="All salinity data >5")
salinityNoAirRolling

#Salinity plot starting 6/17
starting6.17<-HOBOdata[HOBOdata$Date.time>"2022-06-17 04:00:00",]
salinity.delayedStart<-ggplot(starting6.17, aes(x=Date.time, y=Salinity, group=Location, color=Location))+geom_line()+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="All salinity data starting 6/17")
salinity.delayedStart

#Salinity plot starting 6/17 no salinity <5
starting6.17NoAir<-HOBOdata[HOBOdata$Date.time>"2022-06-17 04:00:00" & HOBOdata$Salinity >5,]
salinity.delayedStart.noAir<-ggplot(starting6.17NoAir, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line()+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="Salinity data >5 starting 6/17")
salinity.delayedStart.noAir

#Salinity plot starting 6/17 no salinity <5 ROLLING AVERAGE
salinity.delayedStart.noAirRolling<-ggplot(starting6.17NoAir, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line(alpha=0.4)+geom_ma(n=24, linetype="solid")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="Salinity data >5 starting 6/17")
salinity.delayedStart.noAirRolling

#Part 1c: Temperature Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Find hours when both sensors logged with no issues
Inside<-noAir[noAir$Location=="Inside",]
Outside<-noAir[noAir$Location=="Outside",]
InsideDates<-Inside$Date.time
OutsideDates<-Outside$Date.time
commonDates<-base::intersect(InsideDates, OutsideDates)
common<-noAir[(noAir$Date.time %in% commonDates),]

InsideCommon<-common[common$Location=='Inside',]
OutsideCommon<-common[common$Location=='Outside',]
differences<-InsideCommon$Temp-OutsideCommon$Temp #difference inside minus outside: each hour
meanTempDiff<-mean(differences)
meanTempDiff #inside averaged 0.650°C warmer
seTempDiff<-std.error(differences)
seTempDiff #0.0187
sd(differences) #0.61
(length(differences[differences>0]))/(length(differences)) #percentage of hours where inside was warmer = 94.3%
max(differences) #4.49
min(differences) #-3.61

inMean<-mean(noAir[noAir$Location=="Inside", "Temp"])#mean inside = 18.10°C
inMax<-max(noAir[noAir$Location=="Inside", "Temp"]) #max inside = 24.07°C
inMin<-min(noAir[noAir$Location=="Inside", "Temp"]) #min inside = 13.29°C
inSD<-sd(noAir[noAir$Location=="Inside", "Temp"]) #standard deviation inside = 2.10°C

outMean<-mean(noAir[noAir$Location=="Outside", "Temp"]) #mean outside = 17.39034°C
outMax<-max(noAir[noAir$Location=="Outside", "Temp"]) #max outside = 22.53°C
outMin<-min(noAir[noAir$Location=="Outside", "Temp"]) #min outside = 12.80°C
outSD<-sd(noAir[noAir$Location=="Outside", "Temp"]) #standard deviation outside = 1.97°C

inMean-outMean
inMax-outMax
inMin-outMin
sqrt((inSD)^2+(outSD)^2)

#Calculate rolling 24-hour means
commonInside2 <- InsideCommon %>%
  select(Location, Date.time, Temp) %>%
  mutate(daily_avg= rollmean(Temp, k = 24, fill = NA))

commonOutside2 <- OutsideCommon %>%
  select(Location, Date.time, Temp) %>%
  mutate(out_daily_avg= rollmean(Temp, k = 24, fill = NA))

both_rolling<-c(commonInside2$daily_avg, commonOutside2$out_daily_avg)
common$rolling<-both_rolling

inMeanMin<-min(commonInside2$daily_avg, na.rm=TRUE) #min inside mean daily temp = 13.92°C
outMeanMin<-min(commonOutside2$out_daily_avg, na.rm=TRUE) #min outside mean daily temp = 13.67°C
inMeanMin-outMeanMin
inMeanMax<-max(commonInside2$daily_avg, na.rm=TRUE) #max inside mean daily temp = 21.54°C
outMeanMax<-max(commonOutside2$out_daily_avg, na.rm=TRUE) #max outside mean daily temp = 20.85°C
inMeanMax-outMeanMax
min(both_rolling, na.rm=TRUE) #min mean daily temp = 13.67°C (outside, June 19)
max(both_rolling, na.rm=TRUE) #max mean daily temp = 21.53°C (inside, July 24)

a<-mean(commonInside2$daily_avg, na.rm=TRUE) #18.10601
b<-mean(commonOutside2$out_daily_avg, na.rm=TRUE) #17.45055
a-b # 0.655
c<-(sd(commonInside2$daily_avg, na.rm=TRUE))^2 #3.42
d<-(sd(commonOutside2$out_daily_avg, na.rm=TRUE))^2 #3.11
sqrt(d+c) #2.55

rolling_diffs<-commonInside2$daily_avg-commonOutside2$out_daily_avg #difference in rolling daily mean between locations
mean(rolling_diffs, na.rm=TRUE) #mean rolling daily avg. is 0.655 deg. C higher inside
sd(rolling_diffs, na.rm = TRUE) #0.20
length(rolling_diffs)
length(rolling_diffs[rolling_diffs>0]) #inside daily mean always higher
length(rolling_diffs[rolling_diffs>0.5])/1060 #78.9% diff was >0.5 deg. C
max(rolling_diffs, na.rm = TRUE) #max 1.19 deg. C higher inside
min(rolling_diffs, na.rm = TRUE) #min 0.1 deg. C higher inside

DailyAvgTemp2<-ggplot(common, aes(x=Date.time, y=rolling, group=Location, color=Location))+geom_line()+ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))#+ylim(12.5, 22.5)
DailyAvgTemp2

grid.arrange(tempNoAirRolling,DailyAvgTemp2, ncol=1) #shows that using the geom_ma function gives the same graph as manually calculated rolling average

#Part 1d: Salinity Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
inSalMean<-mean(noAir[noAir$Location=="Inside", "Salinity"])#mean inside = 31.66 mS/cm
inSalMax<-max(noAir[noAir$Location=="Inside", "Salinity"]) #max inside = 35.42 mS/cm
inSalMin<-min(noAir[noAir$Location=="Inside", "Salinity"]) #min inside = 5.07 mS/cm
inSalSD<-sd(noAir[noAir$Location=="Inside", "Salinity"]) #standard dev inside = 2.21 mS/cm

outSalMean<-mean(noAir[noAir$Location=="Outside", "Salinity"]) #mean outside = 31.38 mS/cm
outSalMax<-max(noAir[noAir$Location=="Outside", "Salinity"]) #max outside = 32.85 mS/cm
outSalMin<-min(noAir[noAir$Location=="Outside", "Salinity"]) #min outside = 22.11 mS/cm
outSalSD<-sd(noAir[noAir$Location=="Outside", "Salinity"]) #standard dev outside = 0.69 mS/cm

inSalMean-outSalMean
inSalMax-outSalMax
inSalMin-outSalMin
sqrt((inSalSD)^2+(outSalSD)^2)

sal_differences<-InsideCommon$Salinity-OutsideCommon$Salinity
meanSalDiff<-mean(sal_differences)
meanSalDiff #inside averages 0.28 mS/L higher
sd_sal_Diff<-sd(sal_differences)
sd_sal_Diff #2.136
max(sal_differences) #9.58
min(sal_differences) #-26.12
(length(sal_differences[sal_differences>0]))/(length(sal_differences)) #percentage of hours where inside had higher salinity = 73.9% 

#Calculate rolling mean
commonSalInside2 <- InsideCommon %>%
  select(Location, Date.time, Salinity) %>%
  mutate(daily_sal_avg= rollmean(Salinity, k = 24, fill = NA))

commonSalOutside2 <- OutsideCommon %>%
  select(Location, Date.time, Salinity) %>%
  mutate(out_sal_daily_avg= rollmean(Salinity, k = 24, fill = NA))

both_sal_rolling<-c(commonSalInside2$daily_sal_avg, commonSalOutside2$out_sal_daily_avg)
common$sal_rolling<-both_sal_rolling



grid.arrange(salinityNoAirRolling,DailyAvgSal2, ncol=1)

inSalMeanAvg<-mean(commonSalInside2$daily_sal_avg, na.rm=TRUE) #31.68435
inSalMeanSD<-sd(commonSalInside2$daily_sal_avg, na.rm=TRUE) #1.44
inSalMeanMax<-max(commonSalInside2$daily_sal_avg, na.rm=TRUE) #33.96
inSalMeanMin<-min(commonSalInside2$daily_sal_avg, na.rm=TRUE) #26.48
outSalMeanAvg<-mean(commonSalOutside2$out_sal_daily_avg, na.rm=TRUE) #31.39
outSalMeanMax<-max(commonSalOutside2$out_sal_daily_avg, na.rm=TRUE) #32.02
outSalMeanMin<-min(commonSalOutside2$out_sal_daily_avg, na.rm=TRUE) #30.36
outSalMeanSD<-sd(commonSalOutside2$out_sal_daily_avg, na.rm=TRUE) #0.43

inSalMeanAvg-outSalMeanAvg
inSalMeanMax-outSalMeanMax
inSalMeanMin-outSalMeanMin
sqrt((inSalMeanSD)^2+(outSalMeanSD)^2) #1.50

min(both_sal_rolling, na.rm=TRUE) #min mean daily sal = 26.48
max(both_sal_rolling, na.rm=TRUE) #max mean daily sal = 33.96

rolling_sal_diffs<-commonSalInside2$daily_sal_avg-commonSalOutside2$out_sal_daily_avg
mean(rolling_sal_diffs, na.rm = TRUE) #0.290
sd(rolling_sal_diffs, na.rm = TRUE) #1.24
max(rolling_sal_diffs, na.rm = TRUE) #2.06
min(rolling_sal_diffs, na.rm = TRUE) #-4.66
length(rolling_sal_diffs[rolling_sal_diffs>0])/length(rolling_sal_diffs) #75.2%

DailyAvgSal2<-ggplot(common, aes(x=Date.time, y=sal_rolling, group=Location, color=Location))+geom_line()+ylab("Conductivity (mS/cm)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))+ylim(25,35)
DailyAvgSal2

######################### Part 2: ChlA  ########################################

dateFix = function(df) {
  df$Trial_Date = as.Date(df$Trial, "%m/%d/%y")
  return(df)
}

# Reading Data for Algae Chla extracted values data sheet
chlaDatasheet = read.csv("chlA.csv")

ChlaDatasheet = dateFix(chlaDatasheet)
ChlaDatasheet = ChlaDatasheet[ChlaDatasheet$Major_issue == FALSE,]

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

inMeanChl<-mean(ChlaDatasheet[ChlaDatasheet$Location=="Inside", "Ave_Chl1"]) #7.49
inMaxChl<-max(ChlaDatasheet[ChlaDatasheet$Location=="Inside", "Ave_Chl1"]) #14.60
inMinChl<-min(ChlaDatasheet[ChlaDatasheet$Location=="Inside", "Ave_Chl1"]) #2.22
inSDChl<-sd(ChlaDatasheet[ChlaDatasheet$Location=="Inside", "Ave_Chl1"]) #3.52
outMeanChl<-mean(ChlaDatasheet[ChlaDatasheet$Location=="Outside", "Ave_Chl1"]) #6.07
outMaxChl<-max(ChlaDatasheet[ChlaDatasheet$Location=="Outside", "Ave_Chl1"]) #13.18
outMinChl<-min(ChlaDatasheet[ChlaDatasheet$Location=="Outside", "Ave_Chl1"]) #2.80
outSDChl<-sd(ChlaDatasheet[ChlaDatasheet$Location=="Outside", "Ave_Chl1"]) #2.38

inMeanChl-outMeanChl
inMaxChl-outMaxChl
inMinChl-outMinChl
sqrt((inSDChl)^2+(outSDChl)^2) #4.25

#Create summary data frame
df_updated<-data_summary(ChlaDatasheet, "Ave_Chl1", 
                         groupnames=c("Trial_Date", "Location"))
#with error bars
chlA_updated_errorbars<-ggplot(df_updated, aes(x=Trial_Date, y=mean, group=Location, color=Location)) +geom_line()+ylab("Chlorophyll A (μg/L)")+xlab("")+ geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2, position=position_dodge(0.05))+theme_classic()+scale_y_continuous(limits=c(0,16.5))+ theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+ theme(axis.title.y = element_text(margin = margin(r = 15)))
chlA_updated_errorbars

#no error bars
chlA_graph<-ggplot(df_updated, aes(x=Trial_Date, y=mean, group=Location, color=Location)) + 
  geom_line()+ylab("Chlorophyll A (μg/L)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+ theme(axis.title.y = element_text(margin = margin(r = 10)))+scale_y_continuous(limits=c(0,16))
chlA_graph

OutsideChlA<-df_updated[df_updated$Location=="Outside",'mean']
InsideChlA<-df_updated[df_updated$Location=="Inside",'mean']
mean(InsideChlA) #7.63
mean(OutsideChlA) #6.03
mean(InsideChlA)-mean(OutsideChlA) #1.60
sd(InsideChlA) #3.47
sd(OutsideChlA) #2.28
sqrt((sd(InsideChlA))^2+(sd(OutsideChlA))^2) #4.15
max(InsideChlA) #14.45
max(OutsideChlA) #10.16
max(InsideChlA)-max(OutsideChlA) #4.29
min(InsideChlA) #2.35
min(OutsideChlA) #2.97
min(InsideChlA)-min(OutsideChlA)

differencesChlA<-data.frame(OutsideChlA,InsideChlA)
differencesChlA$Diff<-differencesChlA$InsideChlA-differencesChlA$OutsideChlA
meanDiffChlA<-mean(differencesChlA$Diff)
meanDiffChlA #1.60 μg/L
sdDiffChlA<-sd(differencesChlA$Diff)
sdDiffChlA #3.09 μg/L
max(differencesChlA$Diff) #6.62
min(differencesChlA$Diff) #-1.62
length(differencesChlA$Diff[differencesChlA$Diff>0])/length(differencesChlA$Diff) #50.0%

######################### Part 3: Turbidity  ########################################

#Import data
turbidity<-read.csv("turbidity.csv")

#Create summary data frame
df_turbdity<-data_summary(turbidity, "Turbidity", 
                  groupnames=c("Date", "Location"))
#Convert date format
df_turbdity$Date <- mdy(df_turbdity$Date)

#Standard plot
turbidity_graph<-ggplot(df_turbdity, aes(x=Date, y=mean, group=Location, color=Location)) + 
  geom_line()+theme_classic()+scale_y_continuous(limits=c(0,15))+ylab("Turbidity (NTU)")+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))
turbidity_graph

#Plot with error bars
turbidity_graph_errorbars<-ggplot(df_turbdity, aes(x=Date, y=mean, group=Location, color=Location)) + geom_line()+theme_classic()+scale_y_continuous(limits=c(0,15))+ylab("Turbidity (NTU)")+xlab("")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+ theme(axis.title.y = element_text(margin = margin(r = 15)))+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))
turbidity_graph_errorbars

turbidity_graph_themed<-turbidity_graph+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))
turbidity_graph_themed

#Average difference
OutsideTurbidity<-df_turbdity[df_turbdity$Location=="Outside",'mean']
InsideTurbidity<-df_turbdity[df_turbdity$Location=="Inside",'mean']
differences_turbidity<-data.frame(OutsideTurbidity,InsideTurbidity)
differences_turbidity$Diff<-differences_turbidity$OutsideTurbidity-differences_turbidity$InsideTurbidity
meanTurbidityDiff<-mean(differences_turbidity$Diff)
meanTurbidityDiff #2.04 NTU
sdTurbidityDiff<-sd(differences_turbidity$Diff)
sdTurbidityDiff #3.07 NTU
max(differences_turbidity$Diff) #10.52
min(differences_turbidity$Diff) #0.05

mean(OutsideTurbidity) #4.00
sd(OutsideTurbidity) #3.41
mean(InsideTurbidity) #1.95
sd(InsideTurbidity) #0.76

mean(OutsideTurbidity)-mean(InsideTurbidity) #2.04
sqrt((sd(OutsideTurbidity))^2+(sd(InsideTurbidity))^2) #3.49

max(OutsideTurbidity) #13.32
min(OutsideTurbidity) #1.37
max(InsideTurbidity) #3.03
min(InsideTurbidity) #0.87
max(OutsideTurbidity)-max(InsideTurbidity) #10.29
min(OutsideTurbidity)-min(InsideTurbidity) #0.504

###############################################################################
######################### Combined graphs  ########################################
###############################################################################

#Temp, salinity, ChlA
combined <- DailyAvgTemp2/(DailyAvgSal2 + chlA_graph) + plot_layout(nrow=2, byrow=FALSE, guides = "collect") & theme(legend.position = "bottom")
combined

#Temp, salinity, ChlA, turbidity
allFour<-DailyAvgTemp2/(DailyAvgSal2 + chlA_graph + turbidity_graph_themed) + plot_layout(nrow=2, byrow=FALSE, guides = "collect") & theme(legend.position = "bottom")
allFour

#ChlA and turbidity, theme_classic
chlA_graph2 + turbidity_graph + plot_layout(nrow=1, guides = "collect") & theme(legend.position = "bottom")

#ChlA and turbidity, theme_ipsum
combined2<-chlA_graph + turbidity_graph_themed + plot_layout(nrow=1, guides = "collect") & theme(legend.position = "bottom")
combined2#+ plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14))

#ChlA and turbidity, error bars
combined_errorbars<- chlA_updated_errorbars + turbidity_graph_errorbars + plot_layout(nrow=1, guides = "collect") & theme(legend.position = "bottom")
combined_errorbars+ plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14))
