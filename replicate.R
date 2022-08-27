#Ruby Krasnow
#8/26/22

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
allData<-read.csv("replicatetest.csv", na.strings=c(""," ","NA"))

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

SamplingFour<-allData[allData$Date=="2022-08-15",]
table(SamplingFour$Location,SamplingFour$Gear)


#art ANOVA final heights - model not appropriate?
artFinal<-art(Height ~ Gear * Location, data=SamplingFour)
artFinal #appropriate!
anova(artFinal)
