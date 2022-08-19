#Environmental Data
#RK 8/19/22

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

#Part 1a: Temperature Plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Temperature Plot all data
allTemps<-ggplot(HOBOdata, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line()+ylab("Temperature (°C)")+xlab("")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+theme(axis.title.y = element_text(margin = margin(r = 10)))
allTemps

#Temperature Plot no salinity <5
noAir<- HOBOdata[HOBOdata$Salinity>5,]
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
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="All salinity data")
allSalinity

#Salinity Plot no salinity <5
salinityNoAir<-ggplot(noAir, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line()+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="All salinity data >5")
salinityNoAir

#Salinity Plot no salinity <5 ROLLING AVERAGE
salinityNoAirRolling<-ggplot(noAir, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line(alpha=0.4)+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+geom_ma(n=24, linetype="solid")+theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="All salinity data >5")
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
(length(differences[differences>0]))/(length(differences)) #percentage of hours where inside was warmer = 94.3%

mean(noAir[noAir$Location=="Inside", "Temp"]) #mean inside = 18.10°C
max(noAir[noAir$Location=="Inside", "Temp"]) #max inside = 24.07°C
sd(noAir[noAir$Location=="Inside", "Temp"]) #standard deviation inside = 2.10°C

mean(noAir[noAir$Location=="Outside", "Temp"]) #mean outside = 17.39034°C
max(noAir[noAir$Location=="Outside", "Temp"]) #max outside = 22.53°C
sd(noAir[noAir$Location=="Outside", "Temp"]) #standard deviation outside = 1.97°C

#Calculate rolling 24-hour means
commonInside2 <- InsideCommon %>%
  select(Location, Date.time, Temp) %>%
  mutate(daily_avg= rollmean(Temp, k = 24, fill = NA))

commonOutside2 <- OutsideCommon %>%
  select(Location, Date.time, Temp) %>%
  mutate(out_daily_avg= rollmean(Temp, k = 24, fill = NA))

both_rolling<-c(commonInside2$daily_avg, commonOutside2$out_daily_avg)
common$rolling<-both_rolling

min(both_rolling, na.rm=TRUE) #min mean daily temp = 13.67°C (outside, June 19)
max(both_rolling, na.rm=TRUE) #max mean daily temp = 21.53°C (inside, July 24)

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

DailyAvgTemp2<-ggplot(common, aes(x=Date.time, y=rolling, group=Location, color=Location))+geom_line()+ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))#+ylim(12.5, 22.5)
DailyAvgTemp2

grid.arrange(tempNoAirRolling,DailyAvgTemp2, ncol=1) #shows that using the geom_ma function gives the same graph as manually calculated rolling average

#Part 1d: Salinity Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sal_differences<-InsideCommon$Salinity-OutsideCommon$Salinity
meanSalDiff<-mean(sal_differences)
meanSalDiff #inside averages 0.28 mS/L higher
sd_sal_Diff<-sd(sal_differences)
sd_sal_Diff #2.136
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

DailyAvgSal2<-ggplot(common, aes(x=Date.time, y=sal_rolling, group=Location, color=Location))+geom_line()+ylab("Conductivity (mS/cm)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))+ylim(25,35)
DailyAvgSal2

rolling_sal_diffs<-commonSalInside2$daily_sal_avg-commonSalOutside2$out_sal_daily_avg
mean(rolling_sal_diffs, na.rm = TRUE) #0.290
length(rolling_sal_diffs[rolling_sal_diffs>0])/length(rolling_sal_diffs) #75.2%

mean(commonSalInside2$daily_sal_avg, na.rm=TRUE) #31.68435
sd(commonSalInside2$daily_sal_avg, na.rm=TRUE) #1.44
mean(commonSalOutside2$out_sal_daily_avg, na.rm=TRUE) #31.39409
sd(commonSalOutside2$out_sal_daily_avg, na.rm=TRUE) #0.43

min(both_sal_rolling, na.rm=TRUE) #min mean daily sal = 26.48
max(both_sal_rolling, na.rm=TRUE) #max mean daily sal = 33.96

DailyAvgSal2<-ggplot(common, aes(x=Date.time, y=sal_rolling, group=Location, color=Location))+geom_line()+ylab("Conductivity (mS/cm)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+ylim(25,35)+theme(axis.title.y = element_text(margin = margin(r = 10)))
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

#Create summary data frame (INCLUDING 8/2)
df_updated<-data_summary(ChlaDatasheet, "Ave_Chl1", 
                         groupnames=c("Trial_Date", "Location"))
#with error bars
chlA_updated_errorbars<-ggplot(df_updated, aes(x=Trial_Date, y=mean, group=Location, color=Location)) +geom_line()+ylab("Chlorophyll A (μg/L)")+xlab("")+ geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2, position=position_dodge(0.05))+theme_classic()+scale_y_continuous(limits=c(0,16.5))+ theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+ theme(axis.title.y = element_text(margin = margin(r = 10)))
chlA_updated_errorbars

#no error bars
chlA_graph<-ggplot(df_updated, aes(x=Trial_Date, y=mean, group=Location, color=Location)) + 
  geom_line()+ylab("Chlorophyll A (μg/L)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+ theme(axis.title.y = element_text(margin = margin(r = 10)))+scale_y_continuous(limits=c(0,16))
chlA_graph

OutsideChlA<-df_updated[df_updated$Location=="Outside",'mean']
InsideChlA<-df_updated[df_updated$Location=="Inside",'mean']

differencesChlA<-data.frame(OutsideChlA,InsideChlA)
differencesChlA$Diff<-differencesChlA$InsideChlA-differencesChlA$OutsideChlA
meanDiffChlA<-mean(differencesChlA$Diff)
meanDiffChlA #1.60 μg/L
sdDiffChlA<-sd(differencesChlA$Diff)
sdDiffChlA #3.09 μg/L

#### NOT INCLUDING 8/2 ##################################
df_old<-df_updated[c(1:14),]
#Average difference
OutsideChlA_old<-df_old[df_old$Location=="Outside",'mean']
InsideChlA_old<-df_old[df_old$Location=="Inside",'mean']

differencesChlA_old<-data.frame(OutsideChlA_old,InsideChlA_old)
differencesChlA_old$Diff<-differencesChlA_old$InsideChlA_old-differencesChlA_old$OutsideChlA_old
meanDiffChlA_old<-mean(differencesChlA_old$Diff)
meanDiffChlA_old #0.73 μg/L
sdDiffChlA_old<-sd(differencesChlA_old$Diff)
sdDiffChlA_old #2.08

chlA_graph2<-ggplot(df_old, aes(x=Trial_Date, y=mean, group=Location, color=Location)) + 
  geom_line()+ylab("Chlorophyll A (μg/L)")+xlab("")+theme_classic()+theme(axis.title.y = element_text(margin = margin(r = 10)))
chlA_graph2

chlA_graph2_themed<-ggplot(df_old, aes(x=Trial_Date, y=mean, group=Location, color=Location))+ geom_line()+ylab("Chlorophyll A (μg/L)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)
chlA_graph2_themed

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
turbidity_graph_errorbars<-ggplot(df_turbdity, aes(x=Date, y=mean, group=Location, color=Location)) + geom_line()+theme_classic()+scale_y_continuous(limits=c(0,15))+ylab("Turbidity (NTU)")+xlab("")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+ theme(axis.title.y = element_text(margin = margin(r = 10)))+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))
turbidity_graph_errorbars

turbidity_graph_themed<-turbidity_graph+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))

#Average difference
OutsideTurbidity<-df_turbdity[df_turbdity$Location=="Outside",'mean']
InsideTurbidity<-df_turbdity[df_turbdity$Location=="Inside",'mean']
differences_turbidity<-data.frame(OutsideTurbidity,InsideTurbidity)
differences_turbidity$Diff<-differences_turbidity$OutsideTurbidity-differences_turbidity$InsideTurbidity
meanTurbidityDiff<-mean(differences_turbidity$Diff)
meanTurbidityDiff #2.04 NTU
sdTurbidityDiff<-sd(differences_turbidity$Diff)
sdTurbidityDiff #3.07 NTU

mean(OutsideTurbidity) #4.00
sd(OutsideTurbidity) #3.41
mean(InsideTurbidity) #1.95
sd(InsideTurbidity) #0.76

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

#ChlA and turbidity, error bars
combined_errorbars<- chlA_updated_errorbars + turbidity_graph_errorbars + plot_layout(nrow=1, guides = "collect") & theme(legend.position = "bottom")
combined_errorbars+ plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14))
