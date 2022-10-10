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

# heightlmer<-lmer(Height ~ Date + Gear + Location + Gear:Location + (1|Replicate2), data=allData)
# summary(heightlmer, cor=T)
# hist(resid(heightlmer))
# confint(heightlmer)
# hist(allData$Height)
# qqp(allData$Height)
# 
# Anova(heightlmer)

heightANOVA3 <- aov(Height ~ Gear * Location, data = SamplingFour)
summary(heightANOVA3) #significant location
leveneTest(Height ~ Gear * Location, data = SamplingFour) #p=0.020
plot(heightANOVA3,1)
plot(heightANOVA3,2)
heightResiduals3<-heightANOVA3$residuals
hist(heightResiduals3)
shapiro.test(heightResiduals3) #p=0.023

#art ANOVA final heights replicate
artFinal<-art(Height ~ Gear * Location + (1|Replicate2), data=SamplingFour)
artFinal #appropriate!
anova(artFinal)

#art ANOVA final heights no replicate
artFinal2<-art(Height ~ Gear * Location, data=SamplingFour)
artFinal2 #appropriate!
anova(artFinal2)


cages<-SamplingFour[SamplingFour$Gear=="FC",]
#art ANOVA final cage heights
artCagesFinal<-art(Height ~ Replicate * Location, data=cages)
artCagesFinal #appropriate
anova(artCagesFinal)
