library(nlme)
library(lme4)
library(car)
library(MASS)
library(dplyr)
library(ARTool)
library(ggplot2)
library(agricolae)
library(lubridate)

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
})

BP.replicate<- allData[allData$Gear == "BP","Treatment"]
FB.replicate<- allData[allData$Gear == "FB","Bag"]
FC.replicate<- allData[allData$Gear == "FC","Cage"]
replicateColumn1<-c(BP.replicate, FB.replicate, FC.replicate)
allData$Replicate<-replicateColumn1
allData$Replicate<-droplevels(allData$Replicate)

str(allData)


#DAY 1 Height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Select data
SamplingOne<-allData[allData$Date=="2022-06-14",]
str(SamplingOne)
table(SamplingOne$Location, SamplingOne$Gear)

heightOneGraph1<-ggplot(data = SamplingOne, aes(x = Gear, y = Height, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,75))+ylab("Shell height (mm)")
heightOneGraph1

heightOneGraph2 <- ggplot(SamplingOne, aes(x=Gear, y=Height, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
heightOneGraph2 + facet_grid(. ~ Location)

#ANOVA to demonstrate normality assumption not met
heightANOVA <- aov(Height ~ Gear * Location, data = SamplingOne)
summary(heightANOVA) #nothing significant
leveneTest(Height ~ Gear * Location, data = SamplingOne) #p=0.2035
plot(heightANOVA,1)
plot(heightANOVA,2)
heightResiduals<-heightANOVA$residuals
shapiro.test(heightResiduals) #p=0.00023

artHeightOne<-art(Height ~ Gear * Location, data=SamplingOne)
anova(artHeightOne) #no significant differences

#DAY 1 Linear Growth Rate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

heightChange<-(SamplingOne$Height-47.64)
SamplingOne$LGR<-heightChange/11

summary(SamplingOne$LGR)

LGROneGraph1<-ggplot(data = SamplingOne, aes(x = Gear, y = LGR, fill=Location))+geom_boxplot()+ylab("LGR (mm/day)")
LGROneGraph1

#ANOVA - everything the same as height, since just adding and multiplying by a constant
lgrANOVA <- aov(LGR ~ Gear * Location, data = SamplingOne)
summary(lgrANOVA) #nothing significant
leveneTest(LGR ~ Gear * Location, data = SamplingOne) #p=0.2035
plot(lgrANOVA,1)
plot(lgrANOVA,2)
lgrResiduals<-lgrANOVA$residuals
shapiro.test(lgrResiduals) #p=0.00023

artLGROne<-art(LGR ~ Gear * Location, data=SamplingOne)
anova(artLGROne) #no significant differences, everything the same as height

#DAY 2 Height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SamplingTwo<-allData[allData$Date=="2022-07-05",]

heightTwoGraph1<-ggplot(data = SamplingTwo, aes(x = Gear, y = Height, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,85))+ylab("Shell height (mm)")
heightTwoGraph1

heightTwoGraph2<- ggplot(SamplingTwo, aes(x=Gear, y=Height, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
heightTwoGraph2 + facet_grid(. ~ Location)

#ANOVA - assumptions met!
heightANOVA2 <- aov(Height ~ Gear * Location, data = SamplingTwo)
summary(heightANOVA2) #significant location and interaction
leveneTest(Height ~ Gear * Location, data = SamplingTwo) #p=0.3146
plot(heightANOVA2,1)
plot(heightANOVA2,2)
heightResiduals2<-heightANOVA2$residuals
shapiro.test(heightResiduals2) #p=0.07

TukeyHSD(heightANOVA2, which='Gear:Location')

HSD.test(heightANOVA2, trt = c("Location", "Gear"), console = TRUE)


data_summary <- function(data, varname, groupnames){
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      SE = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)
}

#DAY 3 Height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SamplingThree<-allData[allData$Date=="2022-07-26",]

heightThreeGraph1<-ggplot(data = SamplingThree, aes(x = Gear, y = Height, fill=Location))+geom_boxplot()+ylab("Shell height (mm)")#+scale_y_continuous(limits=c(0,85))
heightThreeGraph1

#just location
heightThreeGraph2<-ggplot(data = SamplingThree, aes(x = Location, y = Height))+geom_boxplot()+ylab("Shell height (mm)")#+scale_y_continuous(limits=c(0,85))
heightThreeGraph2

heightThreeGraph3<-ggplot(data = SamplingThree, aes(x = Gear, y = Height))+geom_boxplot()+ylab("Shell height (mm)")#+scale_y_continuous(limits=c(0,85))
heightThreeGraph3

#ANOVA - assumptions not met
heightANOVA3 <- aov(Height ~ Gear * Location, data = SamplingThree)
summary(heightANOVA3) #significant location
leveneTest(Height ~ Gear * Location, data = SamplingThree) #p=0.020
plot(heightANOVA3,1)
plot(heightANOVA3,2)
heightResiduals3<-heightANOVA3$residuals
shapiro.test(heightResiduals3) #p=0.023




#Height over time/Growth Rates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#USE THIS SECTION IN POSTER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#time graph both location and gear 
timeGraphHeightDf<-data_summary(allData, "Height", 
                                groupnames=c("Date", "Location", "Gear"))

timeGraphHeight<-ggplot(timeGraphHeightDf, aes(x=Date, y=mean, color=Location, linetype=Gear)) +geom_line()+geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+ylab("Shell height (mm)")+xlab("")
timeGraphHeight

mean(SamplingThree[SamplingThree$Location=="Outside", "Height"])
mean(SamplingThree[SamplingThree$Location=="Inside", "Height"])

mean(SamplingThree[SamplingThree$Gear=="BP", "Height"])-47.64
mean(SamplingThree[SamplingThree$Gear=="FB", "Height"])-47.64
mean(SamplingThree[SamplingThree$Gear=="FC", "Height"])-47.64

#art ANOVA - location significant
artDay3<-art(Height ~ Gear * Location, data=SamplingThree)
anova(artDay3)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#time graph just location
timeGraphLocationDf<-data_summary(allData, "Height", 
                                groupnames=c("Date", "Location"))

timeGraphLocation<-ggplot(timeGraphLocationDf, aes(x=Date, y=mean, color=Location)) +geom_line()+geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))
timeGraphLocation

#time graph just gear
timeGraphGearDf<-data_summary(allData, "Height", groupnames=c("Date", "Gear"))
timeGraphGear<-ggplot(timeGraphGearDf, aes(x=Date, y=mean, color=Gear)) +geom_line()+geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))
timeGraphGear

heightRepMeansOne<-data_summary(SamplingOne, "Height", 
                                groupnames=c("Replicate", "Location", "Gear"))

heightRepMeansTwo<-data_summary(SamplingTwo, "Height", 
                                groupnames=c("Replicate", "Location", "Gear"))

heightRepMeansThree<-data_summary(SamplingThree, "Height", 
                                groupnames=c("Replicate", "Location", "Gear"))


heightRepMeansTwo$Height_diff1<-(heightRepMeansTwo$mean-heightRepMeansOne$mean)/21
heightRepMeansThree$Height_diff0<-(heightRepMeansOne$mean-47.64)/11
heightRepMeansThree$Height_diff1<-heightRepMeansTwo$Height_diff1
heightRepMeansThree$Height_diff2<-(heightRepMeansThree$mean-heightRepMeansTwo$mean)/21

heightDiffs0<-data_summary(heightRepMeansThree, "Height_diff0", 
                           groupnames=c("Location", "Gear"))

heightDiffs1<-data_summary(heightRepMeansThree, "Height_diff1", 
                                groupnames=c("Location", "Gear"))

heightDiffs2<-data_summary(heightRepMeansThree, "Height_diff2", 
                           groupnames=c("Location", "Gear"))

growthRateDay2<-ggplot(heightDiffs1, aes(x = Gear, y = mean, colour = Location, group = Location)) +geom_point(size = 4) + geom_line()
growthRateDay2

growthRateDay3<-ggplot(heightDiffs2, aes(x = Gear, y = mean, colour = Location, group = Location)) +geom_point(size = 4) + geom_line()
growthRateDay3

heightDiffs0$Date<-mdy("6/14/2022")
heightDiffs1$Date<-mdy("7/05/2022")
heightDiffs2$Date<-mdy("7/26/2022")
bothDays<-rbind(heightDiffs1, heightDiffs2)
bothDays2<-rbind(heightDiffs0,heightDiffs1, heightDiffs2)

timeGraphLGR<-ggplot(bothDays, aes(x=Date, y=mean, color=Location, linetype=Gear)) +geom_line()+geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+ylab("Linear growth rate (mm/day)")
timeGraphLGR

bothDays_loc<-data_summary(bothDays, "mean", 
                           groupnames=c("Location", "Date"))
timeGraphLGR_loc<-ggplot(bothDays_loc, aes(x=Date, y=mean, group=Location, color=Location)) +geom_line()+geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+ylab("Linear growth rate (mm/day)")
timeGraphLGR_loc

bothDays_gear<-data_summary(bothDays, "mean", 
                           groupnames=c("Gear", "Date"))
timeGraphLGR_gear<-ggplot(bothDays_gear, aes(x=Date, y=mean, group=Gear, color=Gear)) +geom_line()+geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+ylab("Linear growth rate (mm/day)")
timeGraphLGR_gear

mean(bothDays_loc[bothDays_loc$Location=="Outside", "mean"])
mean(bothDays_loc[bothDays_loc$Location=="Inside", "mean"])

mean(bothDays[bothDays$Location=="Outside", "mean"])
mean(bothDays[bothDays$Location=="Inside", "mean"])

artGrowth<-art(mean ~ Gear * Location, data=bothDays)
anova(artGrowth)

artGrowth2<-art(mean ~ Gear * Location, data=bothDays2)
anova(artGrowth2)

artGrowth3<-art(mean ~ Gear * Location, data=heightDiffs2)
anova(artGrowth3)