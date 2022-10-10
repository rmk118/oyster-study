#Ruby Krasnow
#10/10/22

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

#DAY 1 Height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
artHeightOne #appropriate
anova(artHeightOne) #no significant differences

#DAY 2 Height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SamplingTwo<-allData[allData$Date=="2022-07-05",]

# heightTwoGraph1<-ggplot(data = SamplingTwo, aes(x = Gear, y = Height, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,85))+ylab("Shell height (mm)")
# heightTwoGraph1
# 
# heightTwoGraph2<- ggplot(SamplingTwo, aes(x=Gear, y=Height, group=Gear)) + 
#   geom_boxplot(aes(fill=Gear))
# heightTwoGraph2 + facet_grid(. ~ Location)
# 
# #Just location
# heightTwoGraph3<-ggplot(data = SamplingTwo, aes(x = Location, y = Height))+geom_boxplot()+scale_y_continuous(limits=c(0,85))+ylab("Shell height (mm)")
# heightTwoGraph3

#ANOVA - assumptions met!
# heightANOVA2 <- aov(Height ~ Gear * Location, data = SamplingTwo)
# summary(heightANOVA2) #significant location and interaction
# leveneTest(Height ~ Gear * Location, data = SamplingTwo) #p=0.3146
# plot(heightANOVA2,1)
# plot(heightANOVA2,2)
# heightResiduals2<-heightANOVA2$residuals
# shapiro.test(heightResiduals2) #p=0.07
# 
# HSD1<-TukeyHSD(heightANOVA2, which='Gear:Location')
# 
# HSDresults<-(HSD.test(heightANOVA2, trt = c("Location", "Gear"), console = TRUE))
# HSDresults

#DAY 3 Height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SamplingThree<-allData[allData$Date=="2022-07-26",]

#location and gear
# heightThreeGraph1<-ggplot(data = SamplingThree, aes(x = Gear, y = Height, fill=Location))+geom_boxplot()+ylab("Shell height (mm)")#+scale_y_continuous(limits=c(0,85))
# heightThreeGraph1
# 
# #Just location
# heightThreeGraph2<-ggplot(data = SamplingThree, aes(x = Location, y = Height))+geom_boxplot()+ylab("Shell height (mm)")#+scale_y_continuous(limits=c(0,85))
# heightThreeGraph2
# 
# #ANOVA - assumptions not met
# heightANOVA3 <- aov(Height ~ Gear * Location, data = SamplingThree)
# summary(heightANOVA3) #significant location
# leveneTest(Height ~ Gear * Location, data = SamplingThree) #p=0.020
# plot(heightANOVA3,1)
# plot(heightANOVA3,2)
# heightResiduals3<-heightANOVA3$residuals
# shapiro.test(heightResiduals3) #p=0.023
# 
# #art ANOVA July 26 heights - location significant
# artDay3<-art(Height ~ Gear * Location, data=SamplingThree)
# artDay3 #appropriate
# anova(artDay3)

#Height over time/LGR ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Height over time graph both location and gear 
timeGraphHeightDf<-data_summary(allData, "Height",groupnames=c("Date", "Location", "Gear"))

timeGraphHeight<-ggplot(timeGraphHeightDf, aes(x=Date, y=mean, color=Location, linetype=Gear)) +geom_line()+geom_point()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+theme_classic()+ylab("Shell height (mm)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 15)))
timeGraphHeight

SamplingFour<-allData[allData$Date=="2022-08-15",]

#Mean final heights by location
mean(SamplingFour[SamplingFour$Location=="Outside", "Height"]) #57.21
mean(SamplingFour[SamplingFour$Location=="Inside", "Height"]) #60.49

#Mean final heights by gear
day3BP<-mean(SamplingFour[SamplingFour$Gear=="BP", "Height"]) #56.34
day3FC<-mean(SamplingFour[SamplingFour$Gear=="FC", "Height"]) #58.99
day3FB<-mean(SamplingFour[SamplingFour$Gear=="FB", "Height"]) #61.24
sd(SamplingFour[SamplingFour$Gear=="BP", "Height"]) #8.32
sd(SamplingFour[SamplingFour$Gear=="FC", "Height"]) #9.99
sd(SamplingFour[SamplingFour$Gear=="FB", "Height"]) #10.04

#Mean final heights minus initial pop. mean by gear
day3BP-47.64 #8.699
day3FC-47.64 #11.355
day3FB-47.64 #13.596

#Mean final heights minus initial treatment means by gear
BP_heightdiff<-mean(SamplingFour[SamplingFour$Gear=="BP", "Height"])-
  mean(SamplingOne[SamplingOne$Gear=="BP", "Height"]) #10.84
BP_heightdiff/62 #0.17

FC_heightdiff<-mean(SamplingFour[SamplingFour$Gear=="FC", "Height"])-mean(SamplingOne[SamplingOne$Gear=="FC", "Height"]) #12.94
FC_heightdiff/62 #0.21

FB_heightdiff<-mean(SamplingFour[SamplingFour$Gear=="FB", "Height"])-mean(SamplingOne[SamplingOne$Gear=="FB", "Height"]) #16.77
FB_heightdiff/62 #0.27

#art ANOVA final heights - model not appropriate?
artFinal<-art(Height ~ Gear * Location, data=SamplingFour)
artFinal #not appropriate
anova(artFinal)

bags<-SamplingFour[SamplingFour$Gear=="FB",]
cages<-SamplingFour[SamplingFour$Gear=="FC",]
replicateTest<-ggplot(data = bags, aes(x = Gear, y = Height, fill=Replicate2))+geom_boxplot()+ylab("Shell height (mm)")
replicateTestCages<-ggplot(data = cages, aes(x = Gear, y = Height, fill=Replicate2))+geom_boxplot()+ylab("Shell height (mm)")


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Height over time graph just location
timeGraphLocationDf<-data_summary(allData, "Height", 
                                groupnames=c("Date", "Location"))

timeGraphLocation<-ggplot(timeGraphLocationDf, aes(x=Date, y=mean, color=Location)) +geom_line()+geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))
timeGraphLocation

#Height over time graph just gear
timeGraphGearDf<-data_summary(allData, "Height", groupnames=c("Date", "Gear"))
timeGraphGear<-ggplot(timeGraphGearDf, aes(x=Date, y=mean, color=Gear)) +geom_line()+geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))
timeGraphGear

#Calculating LGR
heightRepMeansOneB<-data_summary(SamplingOne, "Height", 
                                groupnames=c("Replicate2", "Location", "Gear"))

heightRepMeansTwoB<-data_summary(SamplingTwo, "Height", 
                                groupnames=c("Replicate2", "Location", "Gear"))

heightRepMeansThreeB<-data_summary(SamplingThree, "Height", 
                                  groupnames=c("Replicate2", "Location", "Gear"))

heightRepMeansFourB<-data_summary(SamplingFour, "Height", 
                                  groupnames=c("Replicate2", "Location", "Gear"))

#Replicates BPi.BP, FB.1, FB.2, etc.
heightRepMeansThreeB$LGR0<-(heightRepMeansOneB$mean-47.64)/11
heightRepMeansThreeB$LGR1<-(heightRepMeansTwoB$mean-heightRepMeansOneB$mean)/21
heightRepMeansThreeB$LGR2<-(heightRepMeansThreeB$mean-heightRepMeansTwoB$mean)/21
heightRepMeansFourB$LGR3<-(heightRepMeansFourB$mean-heightRepMeansThreeB$mean)/23
heightRepMeansFourB$LGR_overall<-(heightRepMeansFourB$mean-heightRepMeansOneB$mean)/62

heightDiffs0B<-data_summary(heightRepMeansThreeB, "LGR0", 
                           groupnames=c("Location", "Gear"))

heightDiffs1B<-data_summary(heightRepMeansThreeB, "LGR1", 
                           groupnames=c("Location", "Gear"))

heightDiffs2B<-data_summary(heightRepMeansThreeB, "LGR2", 
                           groupnames=c("Location", "Gear"))

heightDiffs3B<-data_summary(heightRepMeansFourB, "LGR3", 
                            groupnames=c("Location", "Gear"))

heightDiffsOverall<-data_summary(heightRepMeansFourB, "LGR_overall", 
                            groupnames=c("Location", "Gear"))

growthRateDay2B<-ggplot(heightDiffs1B, aes(x = Gear, y = mean, colour = Location, group = Location)) +geom_point(size = 4) + geom_line()+ylab("Linear growth rate (mm/day)")+theme_ipsum(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+ theme(axis.title.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)))
growthRateDay2B

growthRateDay3B<-ggplot(heightDiffs2B, aes(x = Gear, y = mean, colour = Location, group = Location)) +geom_point(size = 4) + geom_line()+ylab("Linear growth rate (mm/day)")+theme_ipsum(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+ theme(axis.title.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)))+ylim(0,0.35)
growthRateDay3B

growthRateDay4B<-ggplot(heightDiffs3B, aes(x = Gear, y = mean, colour = Location, group = Location)) +geom_point(size = 4) + geom_line()+ylab("Linear growth rate (mm/day)")+theme_ipsum(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+ theme(axis.title.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)))
growthRateDay4B

overallGrowth<-ggplot(heightDiffsOverall, aes(x = Gear, y = mean, colour = Location, group = Location)) +geom_point(size = 4) + geom_line()+ylab("Linear growth rate (mm/day)")+theme_ipsum(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+ theme(axis.title.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)))+ylim(0,0.3)
overallGrowth

#Day 2 and 3 growth rates
combinedB<-growthRateDay2B + growthRateDay3B + plot_layout(nrow=1, guides = "collect") & theme(legend.position = "bottom")
combinedB+ plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14))

heightDiffs0B$Date<-mdy("6/14/2022")
heightDiffs1B$Date<-mdy("7/05/2022")
heightDiffs2B$Date<-mdy("7/26/2022")
bothDaysB<-rbind(heightDiffs1B, heightDiffs2B)
bothDays2B<-rbind(heightDiffs0B,heightDiffs1B, heightDiffs2B)

#LGR sampling days 1 & 2
timeGraphLGR<-ggplot(bothDaysB, aes(x=Date, y=mean, color=Location, linetype=Gear)) +geom_line()+geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+ylab("Linear growth rate (mm/day)")
timeGraphLGR

#LGR over time including Week 1
ggplot(bothDays2B, aes(x=Date, y=mean, color=Location, linetype=Gear)) +geom_line()+geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+ylab("Linear growth rate (mm/day)")

bothDays_loc<-data_summary(bothDaysB, "mean",groupnames=c("Location", "Date"))
timeGraphLGR_loc<-ggplot(bothDays_loc, aes(x=Date, y=mean, group=Location, color=Location)) +geom_line()+geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+ylab("Linear growth rate (mm/day)")
timeGraphLGR_loc

bothDays_gear<-data_summary(bothDaysB, "mean", groupnames=c("Gear", "Date"))
timeGraphLGR_gear<-ggplot(bothDays_gear, aes(x=Date, y=mean, group=Gear, color=Gear)) +geom_line()+geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+ylab("Linear growth rate (mm/day)")
timeGraphLGR_gear

mean(heightDiffs1B[heightDiffs1B$Location=="Outside" & heightDiffs1B$Gear == "FC", "mean"]) #mean LGR FCo interval 1 = 0.001488
mean(heightRepMeansThreeB[heightRepMeansThreeB$Location=="Outside" & heightRepMeansThreeB$Gear=="FC","LGR1"]) #same num. as above
sd(heightRepMeansThreeB[heightRepMeansThreeB$Location=="Outside" & heightRepMeansThreeB$Gear=="FC","LGR1"]) #0.050
mean(heightRepMeansThreeB[heightRepMeansThreeB$Location=="Inside" & heightRepMeansThreeB$Gear=="FC","LGR1"]) #0.359

mean(heightDiffs1B[heightDiffs1B$Location=="Outside", "mean"])#mean LGR outside days 1-2= 0.096
sd(heightDiffs1B[heightDiffs1B$Location=="Outside", "mean"]) #SD outside = 0.112
mean(heightDiffs1B[heightDiffs1B$Location=="Inside", "mean"]) #mean LGR inside days 1-2 = 0.224
sd(heightDiffs1B[heightDiffs1B$Location=="Inside", "mean"]) #SD inside = 0.12

mean(heightDiffs2B[heightDiffs2B$Location=="Outside", "mean"])#mean LGR outside days 2-3= 0.23
sd(heightDiffs2B[heightDiffs2B$Location=="Outside", "mean"]) #SD outside = 0.13
mean(heightDiffs2B[heightDiffs2B$Location=="Inside", "mean"]) #mean LGR inside days 2-3 = 0.17
sd(heightDiffs2B[heightDiffs2B$Location=="Inside", "mean"]) #SD inside = 0.07

mean(heightDiffs1B[,"mean"]) #mean LGR interval 1 = 0.16
mean(heightDiffs2B[,"mean"]) #mean LGR interval 2 = 0.20

mean(bothDays_loc[bothDays_loc$Location=="Outside", "mean"])#mean LGR outside days 1-3= 0.163
mean(bothDays_loc[bothDays_loc$Location=="Inside", "mean"])#mean LGR inside days 1-3= 0.201

mean(bothDaysB[bothDaysB$Location=="Outside", "mean"])#the same number as above
mean(bothDaysB[bothDaysB$Location=="Inside", "mean"])#the same number as above

mean(bothDays_gear[bothDays_gear$Gear=="BP", "mean"]) #0.152
mean(bothDays_gear[bothDays_gear$Gear=="FB", "mean"]) #0.213
mean(bothDays_gear[bothDays_gear$Gear=="FC", "mean"]) #0.182

#Mean growth rate by gear type boxplot
avg_growth_by_gear<-ggplot(data = bothDaysB, aes(x = Gear, y = mean, fill=Location))+geom_boxplot()+ylab("Avg growth rate (mm/day)")+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)))
avg_growth_by_gear

#ART ANOVA growth rates b/w sampling days 1 and 2; same results as height
heightRepMeansThreeB$Replicate2<-as.factor(heightRepMeansThreeB$Replicate2)
artGrowthRep<-art(LGR1 ~ Gear * Location + Error(Replicate2), data=heightRepMeansThreeB) #appropriate
artGrowthRep
anova(artGrowthRep)

art.con(artGrowthRep, "Gear:Location", adjust="holm") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

mean(SamplingTwo[SamplingTwo$Treatment=="FCi", "Height"]) #52.97917
mean(SamplingTwo[SamplingTwo$Treatment=="FCo", "Height"]) #46.70833
mean(SamplingTwo[SamplingTwo$Treatment=="FBo", "Height"]) #45.375

#ART ANOVA growth rate b/w sampling days 1 and 4: both gear and loc significant
artGrowthRep_overall2<-art(LGR_overall ~ Gear * Location + Error(Replicate2), data=heightRepMeansFourB) #appropriate
artGrowthRep_overall2
anova(artGrowthRep_overall2)

heightlmer<-lmer(Height ~ Date + Gear + Location + Gear:Location + (1|Replicate2), data=allData)
summary(heightlmer, cor=T)
hist(resid(heightlmer))
confint(heightlmer)
hist(allData$Height)
qqp(allData$Height)

Anova(heightlmer)
#display(heightlmer)

#Mean height cages replicates
cages<-SamplingFour[SamplingFour$Gear=="FC",]
cagesGrowth<-ggplot(data = cages, aes(x = Cage, y = Height, fill=Location))+geom_boxplot()+ylab("Shell height (mm)")+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)))
cagesGrowth

finalCagesGraph2 <- ggplot(cages, aes(x=Cage, y=Height, group=Cage)) + 
  geom_boxplot(aes(fill=Gear))
finalCagesGraph2 + facet_grid(. ~ Location)

#art ANOVA final cage heights
artCagesFinal<-art(Height ~ Replicate * Location, data=cages)
artCagesFinal #not appropriate
anova(artCagesFinal)

outsideCages<-cages[cages$Location=="Outside",]
kruskal.test(Height~Cage, data=outsideCages)


FC2<-SamplingOne[SamplingOne$Gear=="FC",]
ggplot(data = FC2, aes(x = Location, y = Height, fill=Cage))+geom_boxplot()+scale_y_continuous(limits=c(0,80))+ylab("Height")+theme_classic()

FC4<-SamplingFour[SamplingFour$Gear=="FC",]
ggplot(data = FC4, aes(x = Location, y = Height, fill=Cage))+geom_boxplot()+scale_y_continuous(limits=c(0,80))+ylab("Height")+theme_classic()


FB4<-SamplingFour[SamplingFour$Gear=="FB",]
ggplot(data = FB4, aes(x = Location, y = Cup.ratio, fill=Bag))+geom_boxplot()+scale_y_continuous(limits=c(0,0.5))+ylab("Cup ratio")+theme_classic()

#September data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Import data
sept<-read.csv("septData.csv", na.strings=c(""," ","NA"))

#Convert variables to factors
sept<-within(sept, {
  Cage<-as.factor(Cage)
  Bag<-as.factor(Bag)
  Location<-as.factor(Location)
  Gear<-as.factor(Gear)
  Treatment<-as.factor(Treatment)
  Replicate<-as.factor(Replicate)
})

sept$Replicate2<-paste0(sept$Treatment,".",sept$Replicate)
sept$Replicate2<-as.factor(sept$Replicate2)
sept$Date<-"09-13-2022"
sept$Date<-mdy(sept$Date)

sept2<- sept %>% dplyr::select("Date","Location","Gear","Treatment","Cage","Bag","Oyster","Height","Length","Width","Cup.ratio","Shell.shape","Replicate","Replicate2")

str(sept2)

ggplot(data = sept2, aes(x = Gear, y = Height, fill=Location))+geom_boxplot()+ylab("Shell height (mm)")+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)))

heightSept <- ggplot(sept2, aes(x=Gear, y=Height, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
heightSept + facet_grid(. ~ Location)

ggplot(data = sept2, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+ylab("Cup ratio (SW/SH)")+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)))

ggplot(data = sept2, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+ylab("Shell shape")+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)))

plusSept<-rbind(allData,sept2)

#Height over time graph both location and gear 
septHeightTime<-data_summary(plusSept, "Height", 
                                groupnames=c("Date", "Location", "Gear"))

septHeightTimeGraph<-ggplot(septHeightTime, aes(x=Date, y=mean, color=Location, linetype=Gear)) +geom_line()+geom_point()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+theme_classic()+ylab("Shell height (mm)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 15)))
septHeightTimeGraph

heightRepMeansSept<-data_summary(sept, "Height",groupnames=c("Replicate2", "Location", "Gear"))
heightRepMeansSept$LGR<-(heightRepMeansSept$mean-heightRepMeansFourB$mean)/29

septGrowth<-ggplot(heightRepMeansSept, aes(x=Gear, y=LGR, color=Location))+geom_bar(stat = "identity")+theme_classic()+ylab("Growth rate (mm/day)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 15)))
septGrowth

septGrowth2<-data_summary(heightRepMeansSept, "LGR",groupnames=c("Location", "Gear"))
septGrowth2graph<-ggplot(septGrowth2, aes(x=Gear, y=mean, fill=Location))+geom_bar(stat = "identity", position=position_dodge())+theme_classic()+ylab("Growth rate (mm/day)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 15)))
septGrowth2graph
