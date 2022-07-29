#Cup ratio & shell shape analysis and graphs
#RK 7/28/22

library(nlme)
library(lme4)
library(car)
library(MASS)
library(plyr); library(dplyr)
library(ARTool)
library(plotrix)
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

#DAY 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Select data
SamplingOne<-allData[allData$Date=="2022-06-14",]
str(SamplingOne)
table(SamplingOne$Location, SamplingOne$Gear)

#Combined graph - cup ratio
cupRatioGraph1<-ggplot(data = SamplingOne, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,0.5))+ylab("Cup ratio (shell width/height)")+theme_classic()

#Combined graph - shell shape
shellShapeGraph1<-ggplot(data = SamplingOne, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,9))+ylab("Shell shape")+theme_classic()

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
anova(artCupRatioOne)

#Shell shape ART - significant effect of location
artShellShapeOne<-art(Shell.shape ~ Gear * Location, data=SamplingOne)
anova(artShellShapeOne)

#Location post-hoc - inside higher cup ratio than outside
LocationPostHoc<-art.con(artCupRatioOne, "Location", adjust="holm")# %>%  run ART-C for X1 × X2
  summary(LocationPostHoc) %>%   #add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                   cutpoints = c(0, .001, .01, .05, .10, 1),
                  symbols = c("***", "**", "*", ".", " ")))
  
#Gear post-hoc - FC cup ratio higher than BP
GearPostHoc<-art.con(artCupRatioOne, "Gear", adjust="holm")# %>%  run ART-C for X1 × X2
  summary(GearPostHoc) %>%   #add significance stars to the output
    mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                         cutpoints = c(0, .001, .01, .05, .10, 1),
                         symbols = c("***", "**", "*", ".", " ")))
  
#DAY 2 Cup Ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Import and select data
  
SamplingTwo<-allData[allData$Date=="2022-07-05",]

#Combined graph cup ratio
ggplot(data = SamplingTwo, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,0.5))+ylab("Cup ratio (shell width/height)")+theme_classic()

#Combined graph shell shape
ggplot(data = SamplingTwo, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,9))+ylab("Shell shape")+theme_classic()

#Cup ratio ART - significant effect of both location and significant interaction
artCupRatioTwo<-art(Cup.ratio ~ Gear * Location, data=SamplingTwo)
anova(artCupRatioTwo)

#Location post-hoc
LocationGearPostHoc<-art.con(artCupRatioTwo, "Gear:Location", adjust="holm")
summary(LocationGearPostHoc) %>%   #add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

data_summary <- function(data, varname, groupnames){
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      SE = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)
}

#Cup ratio time graph
timeGraphCupRatioDf<-data_summary(allData, "Cup.ratio", 
                          groupnames=c("Date", "Location", "Gear"))

timeGraphCupRatio<-ggplot(timeGraphCupRatioDf, aes(x=Date, y=mean, color=Location, linetype=Gear)) + 
  geom_line() +
  geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                                             position=position_dodge(0.05))
timeGraphCupRatio


#Shell shape time graph
timeGraphShellShapeDf<-data_summary(allData, "Shell.shape", 
                                  groupnames=c("Date", "Location", "Gear"))

timeGraphShellShape<-ggplot(timeGraphShellShapeDf, aes(x=Date, y=mean, color=Location, linetype=Gear)) + 
  geom_line() +
  geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                                             position=position_dodge(0.05))
timeGraphShellShape


artCupRatioAll<-art(Cup.ratio ~ Gear * Location + (1|Date), data=allData)
summary(artCupRatioAll)
anova(artCupRatioAll)
