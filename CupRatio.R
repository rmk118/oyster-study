#Cup ratio & shell shape analysis and graphs
#RK 10/10/22

library(nlme)
library(lme4)
library(car)
library(MASS)
library(dplyr)
library(tidyverse)
library(ARTool)
library(plotrix)
library(lubridate)
library(hrbrthemes)
options(hrbrthemes.loadfonts = TRUE)
hrbrthemes::import_roboto_condensed()
library(patchwork)

#Import data
#allData<-read.csv("oysterDataAll.csv", na.strings=c(""," ","NA"))
allData<-read.csv("replicatetest.csv", na.strings=c(""," ","NA"))

#Fix date format
allData$Date<-mdy(allData$Date)
allData$Fan.ratio<-round(allData$Length/allData$Height, digits=2)

#Convert variables to factors
allData<-within(allData, {
  Cage<-as.factor(Cage)
  Bag<-as.factor(Bag)
  Location<-as.factor(Location)
  Gear<-as.factor(Gear)
  Treatment<-as.factor(Treatment)
  Replicate<-as.factor(Replicate)
})
str(allData)

allData$Replicate2<-paste0(allData$Treatment,".",allData$Replicate)
allData$Replicate2<-as.factor(allData$Replicate2)
str(allData)

SamplingFour<-allData[allData$Date=="2022-08-15",]
table(SamplingFour$Location,SamplingFour$Gear)

#DAY 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Select data
SamplingOne<-allData[allData$Date=="2022-06-14",]
str(SamplingOne)
table(SamplingOne$Location, SamplingOne$Gear)

#Combined graph - cup ratio
cupRatioGraph1<-ggplot(data = SamplingOne, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+ylab("Cup ratio (shell width/height)")+theme_classic()#+scale_y_continuous(limits=c(0,0.5))

#Combined graph - shell shape
shellShapeGraph1<-ggplot(data = SamplingOne, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+ylab("Shell shape")+theme_classic()+scale_y_continuous(limits=c(0,9))

#Graph inside vs. graph outside - cup ratio
cupRatioGraph2 <- ggplot(SamplingOne, aes(x=Gear, y=Cup.ratio, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
cupRatioGraph2 + facet_grid(. ~ Location)

#Graph inside vs. graph outside - shell shape
shellShapeGraph2 <- ggplot(SamplingOne, aes(x=Gear, y=Shell.shape, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
shellShapeGraph2 + facet_grid(. ~ Location)

#Cup ratio ART - significant effect of both location and gear
artCupRatioOne<-art(Cup.ratio ~ Gear * Location, data=SamplingOne)
artCupRatioOne #appropriate
anova(artCupRatioOne)

#Shell shape ART - significant effect of location
artShellShapeOne<-art(Shell.shape ~ Gear * Location, data=SamplingOne)
artShellShapeOne #appropriate
anova(artShellShapeOne)

#Location cup ratio post-hoc - inside higher cup ratio than outside
LocationPostHoc<-art.con(artCupRatioOne, "Location", adjust="bonferroni")# %>%  run ART-C for X1 × X2
  summary(LocationPostHoc) %>%   #add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                   cutpoints = c(0, .001, .01, .05, .10, 1),
                  symbols = c("***", "**", "*", ".", " ")))
  
#Gear post-hoc - FC cup ratio higher than BP
GearPostHoc<-art.con(artCupRatioOne, "Gear", adjust="bonferroni")# %>%  run ART-C for X1 × X2
  summary(GearPostHoc) %>%   #add significance stars to the output
    mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                         cutpoints = c(0, .001, .01, .05, .10, 1),
                         symbols = c("***", "**", "*", ".", " ")))
  
  
#Location shell shape post-hoc - inside
LocationPostHocShell<-art.con(artShellShapeOne, "Location", adjust="bonferroni")# %>%  run ART-C for X1 × X2
  summary(LocationPostHocShell) %>%   #add significance stars to the output
    mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                         cutpoints = c(0, .001, .01, .05, .10, 1),
                         symbols = c("***", "**", "*", ".", " ")))
  
#DAY 2 Cup Ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Import and select data
  
SamplingTwo<-allData[allData$Date=="2022-07-05",]
SamplingThree<-allData[allData$Date=="2022-07-26",]

#Summary function
data_summary <- function(data, varname, groupnames){
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      SE = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-plyr::ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)
}

#Cup ratio time graph
timeGraphCupRatioDf<-data_summary(allData, "Cup.ratio", 
                          groupnames=c("Date", "Location", "Gear"))

timeGraphCupRatio<-ggplot(timeGraphCupRatioDf, aes(x=Date, y=mean, color=Location, linetype=Gear)) + geom_line() +geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2, position=position_dodge(0.05))
timeGraphCupRatio


#Shell shape time graph
timeGraphShellShapeDf<-data_summary(allData, "Shell.shape",groupnames=c("Date", "Location", "Gear"))

timeGraphShellShape<-ggplot(timeGraphShellShapeDf, aes(x=Date, y=mean, color=Location, linetype=Gear)) +geom_line() +geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+ylab("Shell shape index")+xlab("")
timeGraphShellShape

#Fan ratio time graph
timeGraphFanRatioDf<-data_summary(allData, "Fan.ratio",groupnames=c("Date", "Location", "Gear"))

timeGraphFanRatio<-ggplot(timeGraphFanRatioDf, aes(x=Date, y=mean, color=Location, linetype=Gear)) + geom_line() +geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))
timeGraphFanRatio

#Day one fan ratio - both location and gear significant
artFanRatio<-art(Cup.ratio ~ Gear * Location + (1|Replicate2), data=SamplingOne)
artFanRatio #not appropriate
anova(artFanRatio)

fanRatioGraph1<-ggplot(data = SamplingOne, aes(x = Gear, y = Fan.ratio, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,1.5))+ylab("Fan ratio (shell length/height)")+theme_classic()

#DAY 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SamplingFour<-allData[allData$Date=="2022-08-15",]

#Combined graph cup ratio
ggplot(data = SamplingFour, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+ylab("Cup ratio (shell width/height)")+theme_classic()#+scale_y_continuous(limits=c(0,0.5))

ggplot(data = SamplingFour, aes(x = Replicate2, y = Cup.ratio, fill=Treatment))+geom_boxplot()+ylab("Cup ratio (shell width/height)")+theme_classic()#+scale_y_continuous(limits=c(0,0.5))

#Cup ratio ART 4- significant effect of gear, location, and interaction
artCupRatioFour<-art(Cup.ratio ~ Gear * Location + (1|Replicate2), data=SamplingFour)
artCupRatioFour #appropriate
anova(artCupRatioFour)

test2<-data_summary(SamplingFour, "Cup.ratio", groupnames=c("Treatment"))
test2

test1<-data_summary(allData, "Cup.ratio", groupnames=c("Treatment"))
test1


#Combined graph cup ratio
ggplot(data = allData, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+ylab("Cup ratio (shell width/height)")+theme_classic()#+scale_y_continuous(limits=c(0,0.5))                                             
artCupRatioAll<-art(Cup.ratio ~ Gear * Location + (1|Date), data=allData)
artCupRatioAll #appropriate
anova(artCupRatioAll)


#Gear post-hoc - BP > FB
CupAllGearPostHoc<-art.con(artCupRatioAll, "Gear", adjust="bonferroni")
summary(CupAllGearPostHoc) %>%   #add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#interaction post-hoc: BPi>FBi, BPi>FCi, FCo>FBi
CupAllGearPostHoc<-art.con(artCupRatioAll, "Gear:Location", adjust="bonferroni")
summary(CupAllGearPostHoc) %>%   #add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#Shell shape ART four- significant effect of gear
artShellFour<-art(Shell.shape ~ Gear * Location, data=SamplingFour)
anova(artShellFour)

#floating gear higher than BP
Shell3PostHoc<-art.con(artShellThree, "Gear", adjust="bonferroni")
summary(Shell3PostHoc) %>%   #add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))


initialFBi.1<-mean(allData[allData$Date=="2022-06-14" &allData$Replicate2=="FBi.1","Cup.ratio"])
initialFBi.2<-mean(allData[allData$Date=="2022-06-14" &allData$Replicate2=="FBi.2","Cup.ratio"])
initialFBi.3<-mean(allData[allData$Date=="2022-06-14" &allData$Replicate2=="FBi.3","Cup.ratio"])
initialFBo.1<-mean(allData[allData$Date=="2022-06-14" &allData$Replicate2=="FBo.1","Cup.ratio"])
initialFBo.2<-mean(allData[allData$Date=="2022-06-14" &allData$Replicate2=="FBo.2","Cup.ratio"])
initialFBo.3<-mean(allData[allData$Date=="2022-06-14" &allData$Replicate2=="FBo.3","Cup.ratio"])

initialFCi.1<-mean(allData[allData$Date=="2022-06-14" &allData$Replicate2=="FCi.1","Cup.ratio"])
initialFCi.2<-mean(allData[allData$Date=="2022-06-14" &allData$Replicate2=="FCi.2","Cup.ratio"])
initialFCi.3<-mean(allData[allData$Date=="2022-06-14" &allData$Replicate2=="FCi.3","Cup.ratio"])
initialFCo.1<-mean(allData[allData$Date=="2022-06-14" &allData$Replicate2=="FCo.1","Cup.ratio"])
initialFCo.2<-mean(allData[allData$Date=="2022-06-14" &allData$Replicate2=="FCo.2","Cup.ratio"])
initialFCo.3<-mean(allData[allData$Date=="2022-06-14" &allData$Replicate2=="FCo.3","Cup.ratio"])

SamplingFour$CupChange<-SamplingFour