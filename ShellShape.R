#Shell shape analysis and graphs
#RK 7/13/22

library(nlme)
library(lme4)
library(car)
library(MASS)
library(dplyr)
library(ARTool)

#DAY 1 Shell Shape ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Input and select data
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

#Combined graph
ShapeOne<-ggplot(data = SamplingOne, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,8))+ylab("Shell shape index")+theme_classic()
ShapeOne

#Graph inside vs. graph outside
bw2 <- ggplot(SamplingOne, aes(x=Gear, y=Shell.shape, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
bw2 + facet_grid(. ~ Location)

#ART - location significant
artShellShapeOne<-art(Shell.shape ~ Gear * Location, data=SamplingOne)
anova(artShellShapeOne)

#Location post-hoc - outside higher than inside (inside more ideal shell shape)
LocationPostHoc<-art.con(artShellShapeOne, "Location", adjust="holm")# %>%  run ART-C for X1 × X2
summary(LocationPostHoc) %>%   #add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

summaryOne<-summarise(group_by(SamplingOne, Treatment),Mean=mean(Shell.shape),SD=sd(Shell.shape))
summaryOne

#DAY 2 Shell Shape ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Input and select data
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

#Combined graph
ShapeTwo<-ggplot(data = SamplingTwo, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,8.5))+ylab("Shell shape index")+theme_classic()
ShapeTwo

#Graph inside vs. graph outside
ShapeThree <- ggplot(SamplingTwo, aes(x=Gear, y=Shell.shape, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
ShapeThree + facet_grid(. ~ Location)

#ART - both location and gear:location significant
artShellShapeTwo<-art(Shell.shape ~ Gear * Location, data=SamplingTwo)
anova(artShellShapeTwo)

summaryTwo<-summarise(group_by(SamplingTwo, Treatment),Mean=mean(Shell.shape),SD=sd(Shell.shape))
summaryTwo

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

timeGraphDf<-data_summary(SamplingAll, "Shell.shape", 
                  groupnames=c("Day", "Location"))

timeGraph<-ggplot(timeGraphDf, aes(x=Day, y=mean, group=Location, color=Location)) + 
  geom_line() +
  geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                                             position=position_dodge(0.05))
