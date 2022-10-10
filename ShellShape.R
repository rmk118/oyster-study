#Shell shape analysis and graphs
#RK 10/4/22

library(ggplot2)
library(ggsci)
library(ggpubr)
library(gridExtra)
library(Hmisc)
library(plotrix)
library(dplyr)
library(lubridate)
library(gridExtra)
library(hrbrthemes)
options(hrbrthemes.loadfonts = TRUE)
hrbrthemes::import_roboto_condensed()
library(tidyquant)
library(viridis)
library(mgcv)
library(visreg)
library(sm)
library(patchwork)

data_summary <- function(data, varname, groupnames){
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      SE = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-plyr::ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)}

#Import data
#allData<-read.csv("oysterDataAll.csv", na.strings=c(""," ","NA"))
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

#Import data
sept1<-read.csv("septData.csv", na.strings=c(""," ","NA"))

#Convert variables to factors
sept1<-within(sept1, {
  Cage<-as.factor(Cage)
  Bag<-as.factor(Bag)
  Location<-as.factor(Location)
  Gear<-as.factor(Gear)
  Treatment<-as.factor(Treatment)
  Replicate<-as.factor(Replicate)
})

sept1$Replicate2<-paste0(sept1$Treatment,".",sept1$Replicate)
sept1$Replicate2<-as.factor(sept1$Replicate2)
sept1$Date<-"09-13-2022"
sept1$Date<-mdy(sept1$Date)

sept<- sept1 %>% dplyr::select("Date","Location","Gear","Treatment","Cage","Bag","Oyster","Height","Length","Width","Cup.ratio","Shell.shape","Replicate","Replicate2")


plusSept<-rbind(allData,sept)
str(plusSept)
rm(sept1)
rm(allData)

SamplingOne<-plusSept[plusSept$Date=="2022-06-14",]
SamplingTwo<-plusSept[plusSept$Date=="2022-07-05",]
SamplingThree<-plusSept[plusSept$Date=="2022-07-26",]
SamplingFour<-plusSept[plusSept$Date=="2022-08-15",]
SamplingFive<-plusSept[plusSept$Date=="2022-09-13",]




timeGraphShellShapeDf<-data_summary(plusSept, "Shell.shape", groupnames=c("Date", "Location", "Gear"))

timeGraphShellShape<-ggplot(timeGraphShellShapeDf, aes(x=Date, y=mean, color=Location, linetype=Gear))+geom_line() +geom_point()+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)))+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+ylab("Shell shape deviation")+xlab("")
timeGraphShellShape

initialShellDf<-data_summary(SamplingOne, "Shell.shape", groupnames=c("Date", "Location", "Gear"))
names(initialShellDf)[4:5]<-c("initialShellShape", "initialSE")

augShellDf<-data_summary(SamplingFour, "Shell.shape", groupnames=c("Date", "Location", "Gear"))
names(augShellDf)[4:5]<-c("augShellShape", "augSE")

septShellDf<-data_summary(SamplingFive, "Shell.shape", groupnames=c("Date", "Location", "Gear"))
names(septShellDf)[4:5]<-c("septShellShape", "septSE")

augShellDf = augShellDf %>% 
  left_join(initialShellDf, by = c("Location", "Gear"))
augShellDf$shellChange<-augShellDf$augShellShape-augShellDf$initialShellShape

SamplingFour$heightChange<-SamplingFour$Height-47.64
augHeightChangeDf<-data_summary(SamplingFour, "heightChange", 
                        groupnames=c("Location", "Gear"))
names(augHeightChangeDf)[3:4]<-c("heightChange","heightSE")

augShellDf = augShellDf %>% 
  left_join(augHeightChangeDf, by = c("Location", "Gear"))

ggplot(data = augShellDf, aes(x = heightChange, y = shellChange, color=Location))+geom_point()+ylab("Change in Shell shape")+xlab("Change in SH")+theme_classic()

ggplot(data = augShellDf) +
  geom_point(aes(x = heightChange, y = shellChange, color = Location, shape=Gear), size=3) +
  theme_bw() +geom_smooth(aes(x = heightChange, y = shellChange),method = "lm")+xlab("Δ shell height (mm)")+ylab("Δ shell shape")+ theme(axis.title.y = element_text(margin = margin(r = 15)))

lm.shell1 = lm(shellChange ~ heightChange, data = augShellDf)
summary(lm.shell1)


septShellDf = septShellDf %>% 
  left_join(initialShellDf, by = c("Location", "Gear"))
septShellDf$shellChange<-septShellDf$septShellShape-septShellDf$initialShellShape

SamplingFive$heightChange<-SamplingFive$Height-47.64
septHeightChangeDf<-data_summary(SamplingFive, "heightChange", 
                                groupnames=c("Location", "Gear"))
names(septHeightChangeDf)[3:4]<-c("heightChange","heightSE")

septShellDf = septShellDf %>% 
  left_join(septHeightChangeDf, by = c("Location", "Gear"))

ggplot(data = septShellDf, aes(x = heightChange, y = shellChange, color=Location))+geom_point()+ylab("Change in Shell shape")+xlab("Change in SH")+theme_classic()

ggplot(data = septShellDf) +
  geom_point(aes(x = heightChange, y = shellChange, color = Location, shape=Gear), size=3) +
  theme_bw() +geom_smooth(aes(x = heightChange, y = shellChange),method = "lm")+xlab("Δ shell height (mm)")+ylab("Δ shell shape")+ theme(axis.title.y = element_text(margin = margin(r = 15)))

lm.shell2 = lm(shellChange ~ heightChange, data = septShellDf)
summary(lm.shell2)



#With replicates - Sept
initialShellDfRep<-data_summary(SamplingOne, "Shell.shape", groupnames=c("Date", "Location", "Gear", "Replicate2"))
names(initialShellDfRep)[5:6]<-c("initialShellShape", "initialShellSE")

septShellDfRep<-data_summary(SamplingFive, "Shell.shape", groupnames=c("Date", "Location", "Gear", "Replicate2"))
names(septShellDfRep)[5:6]<-c("septShellShape", "septShellSE")

septShellDfRep = septShellDfRep %>% 
  left_join(initialShellDfRep, by = c("Location", "Gear", "Replicate2"))
septShellDfRep$shellChange<-septShellDfRep$septShellShape-septShellDfRep$initialShellShape

changeSeptDfRep<-data_summary(SamplingFive, "heightChange", 
                           groupnames=c("Location", "Gear", "Replicate2"))

names(changeSeptDfRep)[4:5]<-c("heightChange","heightSE")

septShellDfRep = septShellDfRep %>% 
  left_join(changeSeptDfRep, by = c("Location", "Gear","Replicate2"))

ggplot(data = septShellDfRep, aes(x = heightChange, y = shellChange, color=Location))+geom_point()+ylab("Change in Shell shape")+xlab("Change in SH")+theme_classic()

ggplot(data = septShellDfRep) +
  geom_point(aes(x = heightChange, y = shellChange, color = Location, shape=Gear), size=3) +
  theme_bw() +geom_smooth(aes(x = heightChange, y = shellChange, color=Location),method = "lm")+xlab("Δ shell height (mm)")+ylab("Δ shell shape")+ theme(axis.title.y = element_text(margin = margin(r = 15)))

lm.shellSept = lm(shellChange ~ heightChange, data = septShellDfRep)
summary(lm.shellSept)

lm.shellSept2 = lm(shellChange ~ heightChange * Location * Gear, data = septShellDfRep)
selectedShellSept<-step(lm.shellSept2) #CupChange ~ heightChange + Location + Gear + heightChange:Gear
summary(selectedShellSept)

summary(lm.cupSept2)

# #DAY 1 Shell Shape ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #Input and select data
# SamplingOne<-read.csv("6_14_22.csv", na.strings=c(""," ","NA"))
# SamplingOne<-subset(SamplingOne, select = c(Location,Gear,Treatment,Cage,Bag,Oyster,Height,Length,Width,Cup.ratio,Shell.shape))
# 
# #Convert variables to factors
# SamplingOne<-within(SamplingOne, {
#   Cage<-as.factor(Cage)
#   Bag<-as.factor(Bag)
#   Location<-as.factor(Location)
#   Gear<-as.factor(Gear)
#   Treatment<-as.factor(Treatment)
# })
# 
# #Combined graph
# ShapeOne<-ggplot(data = SamplingOne, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,8))+ylab("Shell shape index")+theme_classic()
# ShapeOne
# 
# #Graph inside vs. graph outside
# bw2 <- ggplot(SamplingOne, aes(x=Gear, y=Shell.shape, group=Gear)) + 
#   geom_boxplot(aes(fill=Gear))
# bw2 + facet_grid(. ~ Location)
# 
# #ART - location significant
# artShellShapeOne<-art(Shell.shape ~ Gear * Location, data=SamplingOne)
# anova(artShellShapeOne)
# 
# #Location post-hoc - outside higher than inside (inside more ideal shell shape)
# LocationPostHoc<-art.con(artShellShapeOne, "Location", adjust="holm")# %>%  run ART-C for X1 × X2
# summary(LocationPostHoc) %>%   #add significance stars to the output
#   mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
#                        cutpoints = c(0, .001, .01, .05, .10, 1),
#                        symbols = c("***", "**", "*", ".", " ")))
# 
# summaryOne<-summarise(group_by(SamplingOne, Treatment),Mean=mean(Shell.shape),SD=sd(Shell.shape))
# summaryOne
# 
# #DAY 2 Shell Shape ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #Input and select data
# SamplingTwo<-read.csv("7_5_22.csv", na.strings=c(""," ","NA"))
# SamplingTwo<-subset(SamplingTwo, select = c(Location,Gear,Treatment,Cage,Bag,Oyster,Height,Length,Width,Cup.ratio,Shell.shape))
# 
# #Convert variables to factors
# SamplingTwo<-within(SamplingTwo, {
#   Cage<-as.factor(Cage)
#   Bag<-as.factor(Bag)
#   Location<-as.factor(Location)
#   Gear<-as.factor(Gear)
#   Treatment<-as.factor(Treatment)
# })
# 
# #Combined graph
# ShapeTwo<-ggplot(data = SamplingTwo, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,8.5))+ylab("Shell shape index")+theme_classic()
# ShapeTwo
# 
# #Graph inside vs. graph outside
# ShapeThree <- ggplot(SamplingTwo, aes(x=Gear, y=Shell.shape, group=Gear)) + 
#   geom_boxplot(aes(fill=Gear))
# ShapeThree + facet_grid(. ~ Location)
# 
# 
# summaryTwo<-summarise(group_by(SamplingTwo, Treatment),Mean=mean(Shell.shape),SD=sd(Shell.shape))
# summaryTwo
# 
# SamplingOne$Day<-"One"
# SamplingTwo$Day<-"Two"
# SamplingAll<-rbind(SamplingOne,SamplingTwo)
# 
# data_summary <- function(data, varname, groupnames){
#   require(plyr)
#   summary_func <- function(x, col){
#     c(mean = mean(x[[col]], na.rm=TRUE),
#       SE = std.error(x[[col]], na.rm=TRUE))
#   }
#   data_sum<-ddply(data, groupnames, .fun=summary_func,
#                   varname)
#   return(data_sum)
# }
# 
# timeGraphDf<-data_summary(SamplingAll, "Shell.shape", 
#                   groupnames=c("Day", "Location"))
# 
# timeGraph<-ggplot(timeGraphDf, aes(x=Day, y=mean, group=Location, color=Location)) + 
#   geom_line() +
#   geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
#                                              position=position_dodge(0.05))


ClarkFEST2<-ggplot(data = SamplingFour, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+ylab("Shell shape index")+theme_classic()#+scale_y_continuous(limits=c(0,0.5))
ClarkFEST2

# ART - both location and location significant, no interaction
artShellShape<-art(Shell.shape ~ Gear * Location, data=SamplingFour)
artShellShape #appropriate
anova(artShellShape)

#gear post-hoc
ShellShapeAugPostHoc<-art.con(artShellShape, "Gear")
summary(ShellShapeAugPostHoc) %>%   #add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))