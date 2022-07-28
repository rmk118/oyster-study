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

data_summary <- function(data, varname, groupnames){
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      SE = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)
}

#Height time graph
timeGraphHeightDf<-data_summary(allData, "Height", 
                                groupnames=c("Date", "Location", "Gear"))

timeGraphHeight<-ggplot(timeGraphHeightDf, aes(x=Date, y=mean, color=Location, linetype=Gear)) + 
  geom_line() +
  geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                                             position=position_dodge(0.05))
timeGraphHeight

#SamplingDay2
HSD.test(heightANOVA2, trt = c("Location", "Gear"), console = TRUE)

#DAY 2 Linear Growth Rate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

BP.replicate1<- SamplingOne[SamplingOne$Gear == "BP","Treatment"]
FB.replicate1<- SamplingOne[SamplingOne$Gear == "FB","Bag"]
FC.replicate1<- SamplingOne[SamplingOne$Gear == "FC","Cage"]
replicateColumn1<-c(BP.replicate1, FB.replicate1, FC.replicate1)
SamplingOne$Replicate<-replicateColumn1

BP.replicate2<- SamplingTwo[SamplingTwo$Gear == "BP","Treatment"]
FB.replicate2<- SamplingTwo[SamplingTwo$Gear == "FB","Bag"]
FC.replicate2<- SamplingTwo[SamplingTwo$Gear == "FC","Cage"]
replicateColumn2<-c(BP.replicate2, FB.replicate2, FC.replicate2)
SamplingTwo$Replicate<-replicateColumn2


heightRepMeansTwo$Height_diff<-heightRepMeansTwo$Means-heightRepMeansOne$Means

heightDiffs <- 
  heightRepMeansTwo %>% 
  group_by(Location, Gear) %>%
  summarise(Mean.diff = mean(Height_diff))
heightDiffs

interaction_plot4<-ggplot(heightDiffs, aes(x = Gear, y = Mean.diff, colour = Location, group = Location)) +
  geom_point(size = 4) + geom_line()
interaction_plot4