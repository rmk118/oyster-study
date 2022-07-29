#Cup ratio & shell shape analysis and graphs
#RK 7/28/22

library(nlme)
library(lme4)
library(car)
library(MASS)
library(plyr); library(dplyr)
library(tidyverse)
library(ARTool)
library(plotrix)
library(lubridate)
library(hrbrthemes)
options(hrbrthemes.loadfonts = TRUE)
hrbrthemes::import_roboto_condensed()

#Import data
allData<-read.csv("oysterDataAll.csv", na.strings=c(""," ","NA"))

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
SamplingThree<-allData[allData$Date=="2022-07-26",]

#Combined graph cup ratio
ggplot(data = SamplingTwo, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+ylab("Cup ratio (shell width/height)")+theme_classic()#+scale_y_continuous(limits=c(0,0.5))

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


# artCupRatioAll<-art(Cup.ratio ~ Gear * Location + (1|Date), data=allData)
# summary(artCupRatioAll)
# anova(artCupRatioAll)

#Fan ratio time graph
timeGraphFanRatioDf<-data_summary(allData, "Fan.ratio", 
                                  groupnames=c("Date", "Location", "Gear"))

timeGraphFanRatio<-ggplot(timeGraphFanRatioDf, aes(x=Date, y=mean, color=Location, linetype=Gear)) + 
  geom_line() +
  geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                                             position=position_dodge(0.05))
timeGraphFanRatio

#Day one fan ratio - both location and gear significant
artFanRatio<-art(Cup.ratio ~ Gear * Location, data=SamplingOne)
anova(artFanRatio)


fanRatioGraph1<-ggplot(data = SamplingOne, aes(x = Gear, y = Fan.ratio, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,1.5))+ylab("Fan ratio (shell length/height)")+theme_classic()
fanRatioGraph3<-ggplot(data = SamplingThree, aes(x = Gear, y = Fan.ratio, fill=Location))+geom_boxplot()+ylab("Fan ratio (shell length/height)")+theme_classic()#+scale_y_continuous(limits=c(0,1.2))

allData

myData<-as.tibble(allData)
myData %>%
  group_by(Date, Location, Gear) %>%
  summarise(
    n = n(),
    meanCup = mean(Cup.ratio),
    #sdCup = sd(Cup.ratio),
    meanFan = mean(Fan.ratio),
   # sdFan = sd(Fan.ratio),
    meanShell = mean(Shell.shape),
    #sdShell = sd(Shell.shape)
  )


#DAY 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Combined graph cup ratio
ggplot(data = SamplingThree, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+ylab("Cup ratio (Shell width/Shell height")+xlab("Gear")+scale_y_continuous(limits=c(0.15,0.45))+theme_ipsum(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+theme(axis.title.y = element_text(margin = margin(r = 12)))

#Cup ratio only by gear
ggplot(data = SamplingThree, aes(x = Gear, y = Cup.ratio))+geom_boxplot()+ylab("Cup ratio (shell width/height)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+ theme(axis.title.y = element_text(margin = margin(r = 10)))#+scale_y_continuous(limits=c(0,0.5))



#Shell shape only by gear
ggplot(data = SamplingThree, aes(x = Gear, y = Shell.shape))+geom_boxplot()+ylab("Shell shape")+theme_ipsum(axis_title_just="cc", axis_title_size = 15, axis_text_size = 10)+xlab("Gear")+ theme(axis.title.y = element_text(margin = margin(r = 5)))+scale_y_continuous(limits=c(0,7.5))

#USE THIS SECTION IN POSTER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Combined graph cup ratio
CupRatioPoster<-ggplot(data = SamplingThree, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+ylab("Cup ratio (SW/SH)")+xlab("Gear")+scale_y_continuous(limits=c(0.15,0.45))+theme_classic()#+theme_ipsum(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+theme(axis.title.y = element_text(margin = margin(r = 12)))

#Combined graph shell shape
ShellShapePoster<-ggplot(data = SamplingThree, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,7.5))+ylab("Shell shape index")+theme_classic()

library(patchwork)
combined <- CupRatioPoster + ShellShapePoster + plot_layout(guides = "collect") & theme(legend.position = "bottom")

#Cup ratio ART - significant effect of gear
artCupRatioThree<-art(Cup.ratio ~ Gear * Location, data=SamplingThree)
anova(artCupRatioThree)

#Gear post-hoc
Cup3GearPostHoc<-art.con(artCupRatioThree, "Gear", adjust="holm")
summary(Cup3GearPostHoc) %>%   #add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

#Shell shape ART - significant effect of gear
artShellThree<-art(Shell.shape ~ Gear * Location, data=SamplingThree)
anova(artShellThree)

Shell3PostHoc<-art.con(artShellThree, "Gear", adjust="holm")
summary(Shell3PostHoc) %>%   #add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Combined graph fan ratio
ggplot(data = SamplingThree, aes(x = Gear, y = Fan.ratio, fill=Location))+geom_boxplot()+ylab("Fan ratio (Shell length/Shell height")+xlab("Gear")+theme_ipsum(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+theme(axis.title.y = element_text(margin = margin(r = 12)))

#Fan ratio ART - significant effect of gear
artFanThree<-art(Fan.ratio ~ Gear * Location, data=SamplingThree)
anova(artFanThree)
