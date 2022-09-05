#Manuscript figures
#Updated 9/5/22

library(nlme)
library(lme4)
library(car)
library(MASS)
library(plotrix)
library(plyr)
library(dplyr)
library(ARTool)
library(ggplot2)
library(agricolae)
library(lubridate)
library(patchwork)
library(multcompView)
library(hrbrthemes)
options(hrbrthemes.loadfonts = TRUE)
hrbrthemes::import_roboto_condensed()

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

#Figure 1a: Shell height over time
timeGraphHeightDf<-data_summary(allData, "Height", 
                                groupnames=c("Date", "Location", "Gear"))

timeGraphHeight<-ggplot(timeGraphHeightDf, aes(x=Date, y=mean, color=Location, linetype=Gear)) +geom_line()+geom_point()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+theme_classic()+ylab("Shell height (mm)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 15)))
timeGraphHeight

#Figure 1b: Change in shell height from initial population, as RGR

#Change in shell height
SamplingFour$Change<-(SamplingFour$Height-47.64)/47.64/73

heightChange1<-ggplot(data = SamplingFour, aes(x = Gear, y = Change, fill=Location))+geom_boxplot()+ylab("RGR (% per day) ")
heightChange1

#Figure 1c: Change in condition index from initial population (by treatment), as % change

initialCImean<-mean(foulingCI[foulingCI$Treatment=="Initial","Condition_index"])
fouling<-foulingCI[foulingCI$Location=="Inside" | foulingCI$Location=="Outside",]

fouling$Change<-(fouling$Condition_index-initialCImean)/initialCImean/60
CIChange1<-ggplot(data = fouling, aes(x = Gear, y = Change, fill=Location))+geom_boxplot()+ylab("Condition index change (% per day) ")+scale_y_continuous(limits=c(-0.02,0.04))
CIChange1

figure1<- timeGraphHeight + heightChange1 + initialCImean + plot_layout(nrow=1, guides = "collect") & theme(legend.position = "bottom")
figure1+ plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14))
