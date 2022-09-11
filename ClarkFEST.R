#ClarkFEST Poster figures - Ruby Krasnow
#Updated 9/11/22

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

data_summary <- function(data, varname, groupnames){
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      SE = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)}

#Import data
allData<-read.csv("oysterDataAll.csv", na.strings=c(""," ","NA"))

#Fix date format
allData$Date<-mdy(allData$Date)

#Convert variables to factors
allData<-within(allData, {
  Cage<-as.factor(Cage)
  Bag<-as.factor(Bag)
  Location<-as.factor(Location)
  Gear<-as.factor(Gear)
  Treatment<-as.factor(Treatment)
  Replicate<-as.factor(Replicate)
})

allData$Replicate2<-paste0(allData$Treatment,".",allData$Replicate)
allData$Replicate2<-as.factor(allData$Replicate2)
str(allData)

SamplingOne<-allData[allData$Date=="2022-06-14",]
SamplingTwo<-allData[allData$Date=="2022-07-05",]
SamplingThree<-allData[allData$Date=="2022-07-26",]
SamplingFour<-allData[allData$Date=="2022-08-15",]

#import and subset data
foulingCI<-read.csv("BiofoulingCI.csv")
foulingCI<-subset(foulingCI, select=c(Location,Gear,Treatment,Oyster,Fouling_weight, Whole_wet_weight, Fouling_ratio, Dry_tissue, Dry_shell, Condition_index))
foulingCI<-rename(foulingCI, Weight=Whole_wet_weight)

#Convert variables to factors
foulingCI<-within(foulingCI, {
  Location<-as.factor(Location)
  Gear<-as.factor(Gear)
  Treatment<-as.factor(Treatment)
})
str(foulingCI)
table(foulingCI$Location, foulingCI$Gear)

############################## Figure 1 - growth/condition index trade-off ###########
#Figure 1a: Shell height over time
timeGraphHeightDf<-data_summary(allData, "Height", 
                                groupnames=c("Date", "Location", "Gear"))

timeGraphHeight<-ggplot(timeGraphHeightDf, aes(x=Date, y=mean, color=Location, linetype=Gear)) +geom_line()+geom_point()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+theme_classic()+ylab("Shell height (mm)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 15)))
timeGraphHeight

#Figure 1b: Change in shell height from initial population, as RGR

SamplingFour$Change<-(SamplingFour$Height-47.64)/47.64/73

heightChange1<-ggplot(data = SamplingFour, aes(x = Gear, y = Change, fill=Location))+geom_boxplot()+ylab("RGR (% per day) ")+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)))
heightChange1

#Figure 1c: Change in condition index from initial population (by treatment), as % change

initialCImean<-mean(foulingCI[foulingCI$Treatment=="Initial","Condition_index"])
fouling<-foulingCI[foulingCI$Location=="Inside" | foulingCI$Location=="Outside",]

fouling$Change<-(fouling$Condition_index-initialCImean)/initialCImean/60
CIChange1<-ggplot(data = fouling, aes(x = Gear, y = Change, fill=Location))+geom_boxplot()+ylab("Change in condition index (% per day) ")+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 15)))+scale_y_continuous(limits=c(-0.02,0.03))#+scale_fill_manual(breaks = fouling$Location, values = c("white", "gray"))
CIChange1

#Figure 1 - all
figure1.1<- heightChange1 + CIChange1 + plot_layout(nrow=1, guides = "collect")
figure1.2<-timeGraphHeight/figure1.1
figure1.2+ plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14)) #Fig. 1

############################# Fig. 3 - Appearance (cup ratio, shell shape deviation) ###########
timeGraphCupRatioDf<-data_summary(allData, "Cup.ratio", groupnames=c("Date", "Location", "Gear"))

timeGraphCupRatio<-ggplot(timeGraphCupRatioDf, aes(x=Date, y=mean, color=Location, linetype=Gear))+geom_line()+geom_point()+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)))+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+ylab("Cup ratio (SW/SH)")+xlab("")
timeGraphCupRatio

timeGraphShellShapeDf<-data_summary(allData, "Shell.shape", groupnames=c("Date", "Location", "Gear"))

timeGraphShellShape<-ggplot(timeGraphShellShapeDf, aes(x=Date, y=mean, color=Location, linetype=Gear))+geom_line() +geom_point()+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)))+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+ylab("Shell shape deviation")+xlab("")
timeGraphShellShape

RatioTwo<-ggplot(data = SamplingFour, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+ylab("Cup ratio (SW/SH)")+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)))#+scale_y_continuous(limits=c(0,0.55))
RatioTwo

ShapeTwo<-ggplot(data = SamplingFour, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+ylab("Shell shape deviation")+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)))
ShapeTwo

figure2<- timeGraphCupRatio + timeGraphShellShape + plot_layout(nrow=1, guides = "collect")
figure2

figure2.2<- RatioTwo + ShapeTwo + plot_layout(nrow=1, guides = "collect")
figure2.2

############################## Figure 4 - Biofouling ###########

fouling_Graph1<-ggplot(data = fouling, aes(x = Gear, y = Fouling_ratio, fill=Location))+geom_boxplot()+ylab("Fouling ratio")+theme_classic()
fouling_Graph1

############################## Figure 2 - Environment ###########

#Fig. 2a - Temp
#Import HOBO data
HOBOdata_unordered<-read.csv("HOBOdata.csv")
#Convert date and location
HOBOdata_unordered$Date.time<-mdy_hms(HOBOdata_unordered$Date.time)
HOBOdata<-HOBOdata_unordered[order(HOBOdata_unordered$Location),]
HOBOdata$Location<-as.factor(HOBOdata$Location)
#Rename salinity column
names(HOBOdata)[3]<-"Salinity"
noAir<- HOBOdata[HOBOdata$Salinity>5,]

#Find hours when both sensors logged with no issues
Inside<-noAir[noAir$Location=="Inside",]
Outside<-noAir[noAir$Location=="Outside",]
InsideDates<-Inside$Date.time
OutsideDates<-Outside$Date.time
commonDates<-base::intersect(InsideDates, OutsideDates)
common<-noAir[(noAir$Date.time %in% commonDates),]
InsideCommon<-common[common$Location=='Inside',]
OutsideCommon<-common[common$Location=='Outside',]

#Calculate rolling 24-hour means
commonInside2 <- InsideCommon %>%
  mutate(daily_avg_temp= rollmean(Temp, k = 24, fill = NA))

commonOutside2 <- OutsideCommon %>%
  mutate(out_daily_avg_temp= rollmean(Temp, k = 24, fill = NA))

both_rolling<-c(commonInside2$daily_avg_temp, commonOutside2$out_daily_avg_temp)
common$rolling<-both_rolling

DailyAvgTemp<-ggplot(common, aes(x=Date.time, y=rolling, group=Location, color=Location))+geom_line()+ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))#+ylim(12.5, 22.5)
DailyAvgTemp

#Fig. 2b - Salinity
#Calculate rolling mean
commonSalInside2 <- InsideCommon %>%
  mutate(daily_sal_avg= rollmean(Salinity, k = 24, fill = NA))

commonSalOutside2 <- OutsideCommon %>%
  mutate(out_sal_daily_avg= rollmean(Salinity, k = 24, fill = NA))

both_sal_rolling<-c(commonSalInside2$daily_sal_avg, commonSalOutside2$out_sal_daily_avg)
common$sal_rolling<-both_sal_rolling

DailyAvgSal<-ggplot(common, aes(x=Date.time, y=sal_rolling, group=Location, color=Location))+geom_line()+ylab("Conductivity (mS/cm)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))+ylim(25,35)
DailyAvgSal

#Fig. 2c - ChlA
dateFix = function(df) {
  df$Trial_Date = as.Date(df$Trial, "%m/%d/%y")
  return(df)
}
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

#Create summary data frame
df_updated<-data_summary(ChlaDatasheet, "Ave_Chl1", 
                         groupnames=c("Trial_Date", "Location"))
#with error bars
chlA_graph_error<-ggplot(df_updated, aes(x=Trial_Date, y=mean, group=Location, color=Location)) +geom_line()+ylab("Chlorophyll A (μg/L)")+xlab("")+ geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2, position=position_dodge(0.05))+theme_classic()+scale_y_continuous(limits=c(0,16.5))+ theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+ theme(axis.title.y = element_text(margin = margin(r = 15)))
chlA_graph_error

#no error bars
chlA_graph<-ggplot(df_updated, aes(x=Trial_Date, y=mean, group=Location, color=Location)) + 
  geom_line()+ylab("Chlorophyll A (μg/L)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+ theme(axis.title.y = element_text(margin = margin(r = 10)))+scale_y_continuous(limits=c(0,16))
chlA_graph

#Temp, salinity, ChlA
allThree<-DailyAvgTemp/(DailyAvgSal + chlA_graph) + plot_layout(nrow=2, byrow=FALSE, guides = "collect") & theme(legend.position = "bottom")
allThree