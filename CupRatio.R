#Cup ratio analysis and graphs
#RK 7/13/22

library(nlme)
library(lme4)
library(car)
library(MASS)
library(dplyr)
library(ARTool)

#DAY 1 Cup Ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Import and select data
SamplingOne<-read.csv("6_14_22.csv", na.strings=c(""," ","NA"))
SamplingOne<-subset(SamplingOne, select = c(Location,Gear,Treatment,Cage,Bag,Oyster,Height,Length,Width,Cup.ratio,Shell.shape))

#Convert variables to factors
SamplingOne<-within(SamplingOne, {
  Cage<-as.factor(Cage)
  Bag<-as.factor(Bag)
  Location<-as.factor(Location)
  Gear<-as.factor(Gear)
  Treatment<-as.factor(Treatment)
})
str(SamplingOne)
table(SamplingOne$Location, SamplingOne$Gear)

#Combined graph
ggplot(data = SamplingOne, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,0.5))+ylab("Cup ratio (shell width/height)")+theme_classic()

#Graph inside vs. graph outside
bw2 <- ggplot(SamplingOne, aes(x=Gear, y=Cup.ratio, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
bw2 + facet_grid(. ~ Location)

#ART - significant effect of both location and gear
artCupRatioOne<-art(Cup.ratio ~ Gear * Location, data=SamplingOne)
anova(artCupRatioOne)

#Location post-hoc - inside higher cup ratio than outside
LocationPostHoc<-art.con(artCupRatioOne, "Location", adjust="holm")# %>%  run ART-C for X1 × X2
  summary(LocationPostHoc) %>%   #add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                   cutpoints = c(0, .001, .01, .05, .10, 1),
                  symbols = c("***", "**", "*", ".", " ")))
  
#Gear post-hoc - FC higher than BP
GearPostHoc<-art.con(artCupRatioOne, "Gear", adjust="holm")# %>%  run ART-C for X1 × X2
  summary(GearPostHoc) %>%   #add significance stars to the output
    mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                         cutpoints = c(0, .001, .01, .05, .10, 1),
                         symbols = c("***", "**", "*", ".", " ")))
  
#DAY 2 Cup Ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Import and select data
  
SamplingTwo<-read.csv("7_5_22.csv", na.strings=c(""," ","NA"))
SamplingTwo<-subset(SamplingTwo, select = c(Location,Gear,Treatment,Cage,Bag,Oyster,Height,Length,Width,Cup.ratio,Shell.shape))
  
#Convert variables to factors
SamplingTwo<-within(SamplingTwo, {
    Cage<-as.factor(Cage)
    Bag<-as.factor(Bag)
    Location<-as.factor(Location)
    Gear<-as.factor(Gear)
    Treatment<-as.factor(Treatment)
  })
str(SamplingTwo)
table(SamplingTwo$Location, SamplingTwo$Gear)

#Combined graph
ggplot(data = SamplingTwo, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,0.5))+ylab("Cup ratio (shell width/height)")+theme_classic()

#ART - significant effect of both location and significant interaction
artCupRatioTwo<-art(Cup.ratio ~ Gear * Location, data=SamplingTwo)
anova(artCupRatioTwo)

#Location post-hoc
LocationGearPostHoc<-art.con(artCupRatioTwo, "Gear:Location", adjust="holm")
summary(LocationGearPostHoc) %>%   #add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

SamplingOne$Day<-"One"
SamplingTwo$Day<-"Two"
SamplingAll<-rbind(SamplingOne,SamplingTwo)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      SE = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)
}

timeGraphDf<-data_summary(SamplingAll, "Cup.ratio", 
                          groupnames=c("Day", "Location"))

timeGraph<-ggplot(timeGraphDf, aes(x=Day, y=mean, group=Location, color=Location)) + 
  geom_line() +
  geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                                             position=position_dodge(0.05))
