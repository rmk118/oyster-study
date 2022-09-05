#Storage/working document
#Updated 9/5/23

library(nlme)
library(lme4)
library(car)
library(MASS)
library(plotrix)
library(ARTool)
library(dplyr)
library(ggplot2)
library(agricolae)
library(lubridate)
library(patchwork)
library(multcompView)
library(hrbrthemes)
options(hrbrthemes.loadfonts = TRUE)
hrbrthemes::import_roboto_condensed()

#Salinity data <25 (too harsh of a cutoff)

#Salinity Plot no salinity <25
noLowerSal<- HOBOdata[HOBOdata$Salinity>25,]
salinityNoLowerSal<-ggplot(noLowerSal, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line()+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="All salinity data >25")
salinityNoLowerSal

#Salinity Plot no salinity <25 ROLLING AVERAGE
salinityNoLowerSalRolling<-ggplot(noLowerSal, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line(alpha=0.4)+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+geom_ma(n=24, linetype="solid")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="All salinity data >25")
salinityNoLowerSalRolling

#Salinity plot starting 6/17 no salinity <25
starting6.17NoLowerSal<-HOBOdata[HOBOdata$Date.time>"2022-06-17 04:00:00" & HOBOdata$Salinity>25,]
salinity.delayedStart.noLowerSal<-ggplot(starting6.17NoLowerSal, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line()+
  theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="Salinity data >25 starting 6/17")
salinity.delayedStart.noLowerSal

#Salinity plot starting 6/17 no salinity <25 ROLLING AVERAGE
salinity.delayedStart.noLowerSalRolling<-ggplot(starting6.17NoLowerSal, aes(x=Date.time, y=Salinity, group=Location, color=Location))+ geom_line(alpha=0.4)+geom_ma(n=24, linetype="solid")+
  theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+labs(x="", y="Salinity", subtitle="Salinity data >25 starting 6/17")
salinity.delayedStart.noLowerSalRolling

#Six different salinity plots
grid.arrange(allSalinity, salinityNoLowSal, salinityNoLowerSal, salinity.delayedStart, salinity.delayedStart.noLowerSal, salinity.delayedStart.noLowSal, ncol=2)

#### R markdown tables

# |                  Test                 | P-value | Violation? |
#   |:-------------------------------------:|:-------:|:----------:|
#   | Levene's test (equality of variances) |    `r round(height.levene[1,3], digits=4)`     |     No     |
# |          Bag 1: Shapiro-Wilk          |   `r round(bag1height.normality$p.value, digits=3)`     |     No     |
# |          Bag 2: Shapiro-Wilk          |   `r round(bag2height.normality$p.value, digits=3)`      |     No     |
# |          Bag 3: Shapiro-Wilk          |    `r round(bag3height.normality$p.value, digits=3)`     |     No     |
# |          Bag 4: Shapiro-Wilk          |     `r round(bag4height.normality$p.value, digits=3)`    |     No     |
# 
# #### Table #2: Length 
# 
# |                  Test                 | P-value | Violation? |
# |:-------------------------------------:|:-------:|:----------:|
# | Levene's test (equality of variances) |    `r round(length.levene[1,3], digits=4)`     |     No     |
#   |          Bag 1: Shapiro-Wilk          |   `r round(bag1length.normality$p.value, digits=3)`     |     No     |
#   |          Bag 2: Shapiro-Wilk          |   `r round(bag2length.normality$p.value, digits=3)`      |     No     |
#   |          Bag 3: Shapiro-Wilk          |    `r round(bag3length.normality$p.value, digits=3)`     |     No     |
#   |          Bag 4: Shapiro-Wilk          |     `r round(bag4length.normality$p.value, digits=3)`    |     No     |
#   
#   #### Table #3: Width 
#   
#   |                  Test                 | P-value | Violation? |
#   |:-------------------------------------:|:-------:|:----------:|
#   | Levene's test (equality of variances) |    `r round(width.levene[1,3], digits=4)`     |     No     |
# |          Bag 1: Shapiro-Wilk          |   `r round(bag1width.normality$p.value, digits=3)`     |     No     |
# |          Bag 2: Shapiro-Wilk          |   `r round(bag2width.normality$p.value, digits=3)`      |     Yes     |
# |          Bag 3: Shapiro-Wilk          |    `r round(bag3width.normality$p.value, digits=3)`     |     No     |
# |          Bag 4: Shapiro-Wilk          |     `r round(bag4width.normality$p.value, digits=3)`    |     Yes     |


#Old linear models
dtID<-subset(SamplingNew,select = c(Gear,Location,Oyster))
names(dtID)
any(duplicated(dtID))

names(gsummary(SamplingNew, form=~Oyster, inv=TRUE))
names(gsummary(SamplingNew, form=~Treatment, inv=TRUE))
names(gsummary(SamplingNew, form=~Gear, inv=TRUE))

lmm <- lmer(Cup.ratio ~ Gear + Location + Gear:Location + (1 | Replicate), data = SamplingNew,
            REML = FALSE)
summary(lmm)
Anova(lmm)

lmm3 <- lmer(Cup.ratio ~ Gear + Location + Gear:Location + (1 | Replicate), data = IndividualOysters, REML = FALSE)
summary(lmm3) #variance explained by replicate indistinguishable from 0
Anova(lmm3) #same p-value as when treating bags as replicates for FC

lmm4 <- lm(Cup.ratio ~ Gear + Location + Gear:Location, data = IndividualOysters)
summary(lmm4)
Anova(lmm4) #very similar to above

plot(fitted(lmm4), resid(lmm4))
qqnorm(resid(lmm4)); qqline(resid(lmm4))

lmtest::bptest(lmm4)

#ANOVA of negative sqrt cup ratio (better than reciprocal, prob worse than log)
IndividualOysters$sqrt.Cup.ratio<-(IndividualOysters$Cup.ratio)^(-1/2)
leveneTest(sqrt.Cup.ratio ~ Gear * Location, data = IndividualOysters) #also good, p=0.1836
sqrtCupRatioANOVA <- aov(sqrt.Cup.ratio ~ Gear * Location, data = IndividualOysters)
summary(sqrtCupRatioANOVA)
plot(sqrtCupRatioANOVA,1)
plot(sqrtCupRatioANOVA,2)
qqp(sqrtCupRatioANOVA$residuals)
shapiro.test(sqrtCupRatioANOVA$residuals) #p=1.823e-5

#ANOVA of log.Shell.Shape
IndividualOysters$log.Shell.shape<-log(IndividualOysters$Shell.shape+1)
leveneTest(log.Shell.shape ~ Gear * Location, data = IndividualOysters) #better, p=0.1789
logShellShapeANOVA <- aov(log.Shell.shape ~ Gear * Location, data = IndividualOysters)
summary(logShellShapeANOVA)
plot(logShellShapeANOVA,1)
plot(logShellShapeANOVA,2)
qqp(logShellShapeANOVA$residuals)
shapiro.test(logShellShapeANOVA$residuals) #better, p=1.8e-6

library(WRS2)
t2way(Cup.ratio ~ Gear * Location, data=IndividualOysters)
postCupRatio<-mcp2atm(Cup.ratio ~ Gear * Location, data=IndividualOysters)
postCupRatio

#With each oyster as individuals, each bag as replicate
IndividualOysters<-read.csv("6_14_22_2.csv", na.strings=c(""," ","NA"))
IndividualOysters<-subset(IndividualOysters, select = c(Location,Gear,Treatment,Cage,Bag,Oyster,Height,Length,Width,Cup.ratio,Shell.shape))
str(IndividualOysters)

#Convert variables to factors
IndividualOysters<-within(IndividualOysters, {
  Cage<-as.factor(Cage)
  Bag<-as.factor(Bag)
  Location<-as.factor(Location)
  Gear<-as.factor(Gear)
  Treatment<-as.factor(Treatment)
})

#ANOVA of log cup ratio - improves normality
IndividualOysters$log.Cup.ratio<-log(IndividualOysters$Cup.ratio)
leveneTest(log.Cup.ratio ~ Gear * Location, data = IndividualOysters) #good, p=0.2556
logCupRatioANOVA <- aov(log.Cup.ratio ~ Gear * Location, data = IndividualOysters)
summary(logCupRatioANOVA)
plot(logCupRatioANOVA,1)
plot(logCupRatioANOVA,2)
hist(logCupRatioANOVA$residuals)
shapiro.test(logCupRatioANOVA$residuals) #p=1.837e-5

#Shell Shape ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

leveneTest(Shell.shape~Gear*Location, data=IndividualOysters) #marginally non-significant, p=0.05086
ShellShapeANOVA <- aov(Shell.shape ~ Gear * Location, data = IndividualOysters)
summary(ShellShapeANOVA)
plot(ShellShapeANOVA,1)
plot(ShellShapeANOVA,2)
shapiro.test(ShellShapeANOVA$residuals) #bad, p=2.2e-16

#ANOVA of cube root shell shape (best)
IndividualOysters$cube.Shell.shape<-abs(IndividualOysters$Shell.shape)^(1/3)
leveneTest(cube.Shell.shape ~ Gear * Location, data = IndividualOysters) #good, p=0.4136
cubeShellShapeANOVA <- aov(cube.Shell.shape ~ Gear * Location, data = IndividualOysters)
summary(cubeShellShapeANOVA)
plot(cubeShellShapeANOVA,1)
plot(cubeShellShapeANOVA,2)
qqp(cubeShellShapeANOVA$residuals)
shapiro.test(cubeShellShapeANOVA$residuals) #significant at p=0.05 but not p=0.01
hist(cubeShellShapeANOVA$residuals)

cupRatioANOVA <- aov(Cup.ratio ~ Gear * Location, data = SamplingNew)
summary(cupRatioANOVA)
leveneTest(Cup.ratio ~ Gear * Location, data = SamplingNew) #p=0.4658
plot(cupRatioANOVA,1)
plot(cupRatioANOVA,2)
cupRatioResiduals<-cupRatioANOVA$residuals
shapiro.test(cupRatioResiduals) #highly non-normal, p=2e-12

art.con(alignedOysters, "Gear", adjust="holm") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

###############################################################################
######################### Graphs  ########################################

ggplot(data = IndividualOysters, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+ylab("Shell shape index")

#Temperature plot starting 6/17
temp.delayedStart<-ggplot(starting6.17, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line()+
  ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 10, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))
temp.delayedStart

# ChlaDatasheet = ChlaDatasheet %>%
#                    mutate(Ave_Chl1 = (ChlFs*(FoFa_max/(FoFa_max-1))* 
#                                      (ChlaDatasheet$Fo-ChlaDatasheet$Fa)*
#                                      (((ChlaDatasheet$Acetone_vol/1000)/ChlaDatasheet$Dilution_Factor)/ChlaDatasheet$Vol_Filtered)))
# 
# ChlaDatasheet = ChlaDatasheet %>%
#   mutate(ChlaDatasheet, Ave_Phaeo1 = ((ChlFs*(FoFa_max/(FoFa_max-1)))*
#                                      ((FoFa_max-1)*(ChlaDatasheet$Fo-ChlaDatasheet$Fa))*
#                                      (((ChlaDatasheet$Acetone_vol/1000)/ChlaDatasheet$Dilution_Factor)/ChlaDatasheet$Vol_Filtered)))

short<-SamplingTwo[SamplingTwo$Height < 24,]
shortOne<-SamplingOne[SamplingOne$Height < 24,]

#Data from only 05:00 to compare to Leeman et al.
# InsideCommonDay<-InsideCommon[hour(InsideCommon$Date.time) %in% (5:15),]
# OutsideCommonDay<-OutsideCommon[hour(OutsideCommon$Date.time) %in% (5:15),]
# diffDay<-InsideCommonDay$Temp-OutsideCommonDay$Temp
# mean(diffDay)
# summary(diffDay)

######################################################################
######################### ANOVAs.R  ########################################

# library(nlme)
# library(lme4)
# library(car)
# library(MASS)
# library(dplyr)
# library(ARTool)
# library(WRS2)

#Read and subset data
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

######################### No random effects  ########################################

#Shell height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Regular ANOVA
RegHeightOne<-aov(Height~ Gear * Location, data = SamplingOne)
summary(RegHeightOne)
#ART
artHeightOne<-art(Height ~ Gear * Location, data=SamplingOne)
anova(artHeightOne)
#WRS
wrsHeightOne<-t2way(Height ~ Gear * Location, data=SamplingOne)
wrsHeightOne

#Cup ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Regular ANOVA
RegRatioOne<-aov(Cup.ratio ~ Gear * Location, data = SamplingOne)
summary(RegRatioOne)
#ART
artRatioOne<-art(Cup.ratio ~ Gear * Location, data=SamplingOne)
anova(artRatioOne)
#WRS
wrsRatioOne<-t2way(Cup.ratio ~ Gear * Location, data=SamplingOne)
wrsRatioOne

#Shape Index ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Regular ANOVA
RegShapeOne<-aov(Shell.shape ~ Gear * Location, data = SamplingOne)
summary(RegShapeOne)
#ART
artShapeOne<-art(Shell.shape ~ Gear * Location, data=SamplingOne)
anova(artShapeOne)
#WRS
wrsShapeOne<-t2way(Shell.shape ~ Gear * Location, data=SamplingOne)
wrsShapeOne

#Shell length ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Regular ANOVA
RegLenOne<-aov(Length ~ Gear * Location, data = SamplingOne)
summary(RegLenOne)
#ART
artLenOne<-art(Length ~ Gear * Location, data=SamplingOne)
anova(artLenOne)
#WRS
wrsLenOne<-t2way(Length ~ Gear * Location, data=SamplingOne)
wrsLenOne

#Shell width ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Regular ANOVA
RegWidOne<-aov(Width ~ Gear * Location, data = SamplingOne)
summary(RegWidOne)
#ART
artWidOne<-art(Width ~ Gear * Location, data=SamplingOne)
anova(artWidOne)
#WRS
wrsWidOne<-t2way(Width ~ Gear * Location, data=SamplingOne)
wrsWidOne

######################### Replicate 1-3, BP = NULL  ########################################

SamplingTwo <- SamplingOne[order(SamplingOne$Gear),]
BP.replicate<- SamplingTwo[SamplingTwo$Gear == "BP","Bag"]
FB.replicate<- SamplingTwo[SamplingTwo$Gear == "FB","Bag"]
FC.replicate<- SamplingTwo[SamplingTwo$Gear == "FC","Cage"]
replicateColumn<-c(BP.replicate, FB.replicate, FC.replicate)
SamplingTwo$Replicate<-replicateColumn
SamplingTwo<-droplevels(SamplingTwo)
str(SamplingTwo)

#Shell Height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Regular LM
RegHeightTwo <- lmer(Height ~ Gear + Location + Gear:Location + (1 | Replicate), data = SamplingTwo, REML = FALSE)
summary(RegHeightTwo)
Anova(RegHeightTwo)
#ART
artHeightTwo<-art(Height ~ Gear * Location+ (1|Replicate), data=SamplingTwo)
anova(artHeightTwo)

######################### Replicate 1-3, BP = BPi/BPo  ########################

SamplingThree<-SamplingTwo
BP.replicate2<- SamplingThree[SamplingThree$Gear == "BP","Treatment"]
replicateColumn2<-c(BP.replicate2, FB.replicate, FC.replicate)
SamplingThree$Replicate<-replicateColumn2
str(SamplingThree)
SamplingThree<-droplevels(SamplingThree)

#Shell Height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Regular LM
RegHeightThree <- lmer(Height ~ Gear + Location + Gear:Location + (1 | Replicate), data = SamplingThree, REML = FALSE)
summary(RegHeightThree)
Anova(RegHeightThree)
#ART
artHeightThree<-art(Height ~ Gear * Location+ (1|Replicate), data=SamplingThree)
anova(artHeightThree)

#### Replicate 1-12, BP = BPi/BPo  #######################

# IndividualOysters$Replicate<-as.factor(IndividualOysters$Replicate)
# str(IndividualOysters)
# Inside<-IndividualOysters[which(IndividualOysters$Location=="Inside"),]
# InsideOne<-Inside[which(Inside$Cage==1),]
# InsideOne$Replicate<-as.factor(7)
#   
#   
# IndividualOysters$Replicate[IndividualOysters$Cage==1]<-as.factor(7)
# IndividualOysters$Replicate[IndividualOysters$Cage==2]<-as.factor(8)
# IndividualOysters$Replicate[IndividualOysters$Cage==3]<-as.factor(9)
# IndividualOysters$Replicate[IndividualOysters$Cage==4]<-as.factor(10)
# IndividualOysters$Replicate[IndividualOysters$Cage==5]<-as.factor(11)
# IndividualOysters$Replicate[IndividualOysters$Cage==6]<-as.factor(12)

#lm with no random effect; no significance
alignedOystersHeight<-art(Height ~ Gear * Location, data=IndividualOysters)
anova(alignedOystersHeight)

#mixed effects linear model with replicate as random effect; no significance
alignedOystersHeight2<-art(Height ~ Gear * Location + (1|Replicate), data=IndividualOysters)
anova(alignedOystersHeight2)

#mixed effects linear model with bag as random effect; location significant
alignedOystersHeight3<-art(Height ~ Gear * Location + (1|Bag), data=IndividualOysters)
anova(alignedOystersHeight3)

####################################################################
######################### ModelOptions.R  ########################################

#Read and subset data
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

#Regular ANOVA no random effects
RegCupRatioOne<-aov(Cup.ratio~ Gear * Location, data = SamplingOne)
summary(RegCupRatioOne)

#ART one: no replicates included as random effect
artCupRatioOne<-art(Cup.ratio ~ Gear * Location, data=SamplingOne)
anova(artCupRatioOne)

artShellShapeOne<-art(Shell.shape ~ Gear * Location, data=SamplingOne)
anova(artShellShapeOne)

FB<-SamplingOne[SamplingOne$Gear=="FB",]
ggplot(data = FB, aes(x = Location, y = Cup.ratio, fill=Bag))+geom_boxplot()+scale_y_continuous(limits=c(0,0.5))+ylab("Cup ratio (shell width/height)")+theme_classic()

FC<-SamplingOne[SamplingOne$Gear=="FC",]
ggplot(data = FC, aes(x = Location, y = Cup.ratio, fill=Cage))+geom_boxplot()+scale_y_continuous(limits=c(0,0.5))+ylab("Cup ratio (shell width/height)")+theme_classic()

FB2<-SamplingOne[SamplingOne$Gear=="FB",]
ggplot(data = FB2, aes(x = Location, y = Shell.shape, fill=Bag))+geom_boxplot()+scale_y_continuous(limits=c(0,8))+ylab("Shell shape")+theme_classic()

df2<-summarise(group_by(FB2, Bag),Mean=mean(Shell.shape),SD=sd(Shell.shape))

#CAGE ONE IS MUCH HIGHER ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FC2<-SamplingOne[SamplingOne$Gear=="FC",]
ggplot(data = FC2, aes(x = Location, y = Shell.shape, fill=Cage))+geom_boxplot()+scale_y_continuous(limits=c(0,8))+ylab("Shell shape")+theme_classic()

FC2outside<-FC2[FC2$Location=="Outside",]
df3<-summarise(group_by(FC2, Cage),Mean=mean(Shell.shape),SD=sd(Shell.shape))
df4<-summarise(group_by(FC2outside, Cage),Mean=mean(Shell.shape),SD=sd(Shell.shape))

#WRS
wrsCupRatioOne<-t2way(Cup.ratio ~ Gear * Location, data=SamplingOne)
wrsCupRatioOne

SamplingOne <- SamplingOne[order(SamplingOne$Gear),]
BP.replicate<- SamplingOne[SamplingOne$Gear == "BP","Bag"]
FB.replicate<- SamplingOne[SamplingOne$Gear == "FB","Bag"]
FC.replicate<- SamplingOne[SamplingOne$Gear == "FC","Cage"]
replicateColumn<-c(BP.replicate, FB.replicate, FC.replicate)
SamplingOne$Replicate<-replicateColumn
SamplingOne<-droplevels(SamplingOne)
str(SamplingOne)

# BP.replicate<- allData[allData$Gear == "BP","Treatment"]
# FB.replicate<- allData[allData$Gear == "FB","Bag"]
# FC.replicate<- allData[allData$Gear == "FC","Cage"]
# replicateColumn1<-c(BP.replicate, FB.replicate, FC.replicate)
# allData$Replicate<-replicateColumn1
# allData$Replicate<-droplevels(allData$Replicate)

#Replicates 1,2,3
# heightRepMeansThree$LGR0<-(heightRepMeansOne$mean-47.64)/11
# heightRepMeansThree$LGR1<-(heightRepMeansTwo$mean-heightRepMeansOne$mean)/21
# heightRepMeansThree$LGR2<-(heightRepMeansThree$mean-heightRepMeansTwo$mean)/21

# #Replicates 1,2,3
# heightDiffs0<-data_summary(heightRepMeansThree, "LGR0", 
#                            groupnames=c("Location", "Gear"))
# 
# heightDiffs1<-data_summary(heightRepMeansThree, "LGR1", 
#                                 groupnames=c("Location", "Gear"))
# 
# heightDiffs2<-data_summary(heightRepMeansThree, "LGR2", 
#                            groupnames=c("Location", "Gear"))

#DAY 1 Linear Growth Rate same trends/sig. as height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# heightChange<-(SamplingOne$Height-47.64)
# SamplingOne$LGR<-heightChange/11
# 
# summary(SamplingOne$LGR)
# 
# LGROneGraph1<-ggplot(data = SamplingOne, aes(x = Gear, y = LGR, fill=Location))+geom_boxplot()+ylab("LGR (mm/day)")
# LGROneGraph1
# 
# #ANOVA - everything the same as height, since just adding and multiplying by a constant
# lgrANOVA <- aov(LGR ~ Gear * Location, data = SamplingOne)
# summary(lgrANOVA) #nothing significant
# leveneTest(LGR ~ Gear * Location, data = SamplingOne) #p=0.2035
# plot(lgrANOVA,1)
# plot(lgrANOVA,2)
# lgrResiduals<-lgrANOVA$residuals
# shapiro.test(lgrResiduals) #p=0.00023

# artLGROne<-art(LGR ~ Gear * Location, data=SamplingOne)
# artLGROne #appropriate
# anova(artLGROne) #no significant differences, everything the same as height



#Regular LM (REML true or false?) -- modify this formula to try parametric LM with other random effect configurations
RegCupRatioTwo <- lmer(Cup.ratio ~ Gear + Location + Gear:Location + (1 | Replicate), data = SamplingOne, REML = FALSE)
summary(RegCupRatioTwo)
Anova(RegCupRatioTwo)

#ART two: replicates 1,2,3 for all floating treatments, BP replicate = null
artCupRatioTwo<-art(Cup.ratio ~ Gear * Location+ (1|Replicate), data=SamplingOne)
anova(artCupRatioTwo)

BP.replicate2<- SamplingOne[SamplingOne$Gear == "BP","Treatment"]
replicateColumn2<-c(BP.replicate2, FB.replicate, FC.replicate)
SamplingOne$Replicate2<-replicateColumn2
SamplingOne<-droplevels(SamplingOne)
str(SamplingOne)

#ART three: replicates 1,2,3 for all floating treatments, BP replicate = BPi or BPo
artCupRatioThree<-art(Cup.ratio ~ Gear * Location+ (1|Replicate2), data=SamplingOne)
anova(artCupRatioThree)

SamplingTwo<-read.csv("6_14_22_2.csv", na.strings=c(""," ","NA"))
SamplingTwo<-subset(SamplingTwo, select = c(Location,Gear,Treatment,Cage,Bag,Oyster,Height,Length,Width,Cup.ratio,Shell.shape))

#Convert variables to factors
SamplingTwo<-within(SamplingTwo, {
  Cage<-as.factor(Cage)
  Bag<-as.factor(Bag)
  Location<-as.factor(Location)
  Gear<-as.factor(Gear)
  Treatment<-as.factor(Treatment)
})

SamplingTwo <- SamplingTwo[order(SamplingTwo$Gear),]
BP.replicate3<- SamplingTwo[SamplingTwo$Gear == "BP","Bag"]
FB.replicate2<- SamplingTwo[SamplingTwo$Gear == "FB","Bag"]
FC.replicate2<- rep(c(7,8,9,10,11,12),each=32)
replicateColumn3<-c(BP.replicate3, FB.replicate2, FC.replicate2)
SamplingTwo$Replicate<-as.factor(replicateColumn3)
SamplingTwo<-droplevels(SamplingTwo)
str(SamplingTwo)

#ART four: replicates 1-12 for floating gear, BP replicate = null
artCupRatioFour<-art(Cup.ratio ~ Gear * Location+ (1|Replicate), data=SamplingTwo)
anova(artCupRatioFour)

BP.replicate4<- SamplingTwo[SamplingTwo$Gear == "BP","Treatment"]
replicateColumn4<-c(rep(c("BPi","BPo"),each=96), FB.replicate2, FC.replicate2)
SamplingTwo$Replicate2<-as.factor(replicateColumn4)
SamplingTwo<-droplevels(SamplingTwo)
str(SamplingTwo)

#ART five: replicates 1-12 for floating gear (bag for FB, cage for FC), BP replicate = BPi/BPo
artCupRatioFive<-art(Cup.ratio ~ Gear * Location+ (1|Replicate2), data=SamplingTwo)
anova(artCupRatioFive)

BP.replicate5<- SamplingTwo[SamplingTwo$Gear == "BP","Bag"]
FB.replicate3<- SamplingTwo[SamplingTwo$Gear == "FB","Bag"]
FC.replicate3<- SamplingTwo[SamplingTwo$Gear == "FC","Bag"]
replicateColumn5<-c(BP.replicate5, FB.replicate3, FC.replicate3)
SamplingTwo$Replicate3<-as.factor(replicateColumn5)
SamplingTwo<-droplevels(SamplingTwo)
str(SamplingTwo)

#ART six: replicates 1-30 for floating gear (bags for both), BP replicate = null
artCupRatioSix<-art(Cup.ratio ~ Gear * Location+ (1|Replicate3), data=SamplingTwo)
anova(artCupRatioSix)

BP.replicate6<- SamplingTwo[SamplingTwo$Gear == "BP","Treatment"]
replicateColumn6<-c(BP.replicate6, FB.replicate3, FC.replicate3)
SamplingTwo$Replicate4<-as.factor(replicateColumn6)
SamplingTwo<-droplevels(SamplingTwo)
str(SamplingTwo)

#ART seven: replicates 1-30 for floating gear (bags for both), BP replicate = BPi/BPo
artCupRatioSeven<-art(Cup.ratio ~ Gear * Location+ (1|Replicate4), data=SamplingTwo)
anova(artCupRatioSeven)

###############################################################################
######################### RGR: essentially the same as LGR ########################################

# heightRepMeansThree$RGR0<-((heightRepMeansOne$mean-47.64)/47.64)*100/11
# heightRepMeansThree$RGR1<-((heightRepMeansTwo$mean-heightRepMeansOne$mean)/heightRepMeansOne$mean)*100/21
# heightRepMeansThree$RGR2<-((heightRepMeansThree$mean-heightRepMeansTwo$mean)/heightRepMeansTwo$mean)*100/21
# 
# RGR0<-data_summary(heightRepMeansThree, "RGR0",groupnames=c("Location", "Gear"))
# RGR1<-data_summary(heightRepMeansThree, "RGR1",groupnames=c("Location", "Gear"))
# RGR2<-data_summary(heightRepMeansThree, "RGR2",groupnames=c("Location", "Gear"))
# 
# RGRday1<-ggplot(RGR1, aes(x = Gear, y = mean, colour = Location, group = Location)) +geom_point(size = 4) + geom_line()+ylab("Relative growth rate (% change/day)")+theme_ipsum(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+ theme(axis.title.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)))
# RGRday1
# 
# RGRday2<-ggplot(RGR2, aes(x = Gear, y = mean, colour = Location, group = Location)) +geom_point(size = 4) + geom_line()+ylab("Relative growth rate (% change/day)")+theme_ipsum(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+ theme(axis.title.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)))
# RGRday2

#import and subset data
foulingCI<-read.csv("BiofoulingCI.csv")
foulingCI<-subset(foulingCI, select=c(Location,Gear,Treatment,Oyster,Fouling_weight, Whole_wet_weight, Fouling_ratio, Dry_tissue, Dry_shell, Condition_index))
foulingCI<-dplyr::rename(foulingCI, Weight=Whole_wet_weight)

#Convert variables to factors
foulingCI<-within(foulingCI, {
  Location<-as.factor(Location)
  Gear<-as.factor(Gear)
  Treatment<-as.factor(Treatment)
})

head(foulingCI)
#only comparing final treatments
fouling<-foulingCI[foulingCI$Location=="Inside" | foulingCI$Location=="Outside",]

leveneTest(Weight~Gear*Location, data=fouling) #non-significant, p=0.144
weightANOVA <- aov(Weight ~ Gear * Location, data = fouling)
summary(weightANOVA)
plot(weightANOVA,1)
plot(weightANOVA,2)
hist(weightANOVA$residuals)
shapiro.test(weightANOVA$residuals) #bad, p=3.4e-05

leveneTest(Fouling_ratio~Gear*Location, data=fouling) #bad, p=4.3e-09
foulingANOVA <- aov(Fouling_ratio ~ Gear * Location, data = fouling)
summary(foulingANOVA)
plot(foulingANOVA,1)
plot(foulingANOVA,2)
hist(foulingANOVA$residuals)
shapiro.test(foulingANOVA$residuals) #bad, p=1.4e-07

leveneTest(Condition_index~Gear*Location, data=fouling) #good, p=0.81
conditionANOVA <- aov(Condition_index ~ Gear * Location, data = fouling)
summary(conditionANOVA)
plot(conditionANOVA,1)
plot(conditionANOVA,2)
hist(conditionANOVA$residuals)
shapiro.test(conditionANOVA$residuals) #bad, p=2.3e-11

initialMean<-mean(foulingCI[foulingCI$Location=="Initial", "Weight"]) #9.04
mean(foulingCI[foulingCI$Gear=="BP", "Weight"])-initialMean #9.98
sd(foulingCI[foulingCI$Gear=="BP", "Weight"]) #7.94
mean(foulingCI[foulingCI$Gear=="FB", "Weight"])-initialMean #12.32
sd(foulingCI[foulingCI$Gear=="FB", "Weight"]) #8.94
mean(foulingCI[foulingCI$Gear=="FC", "Weight"])-initialMean #14.04
sd(foulingCI[foulingCI$Gear=="FC", "Weight"]) #9.79

initialShell<-mean(foulingCI[foulingCI$Location=="Initial", "Dry_shell"]) #5.70
mean(foulingCI[foulingCI$Gear=="BP", "Dry_shell"])-initialShell #7.31
sd(foulingCI[foulingCI$Gear=="BP", "Dry_shell"]) #5.74
mean(foulingCI[foulingCI$Gear=="FB", "Dry_shell"])-initialShell #8.14
sd(foulingCI[foulingCI$Gear=="FB", "Dry_shell"]) #5.98
mean(foulingCI[foulingCI$Gear=="FC", "Dry_shell"])-initialShell #9.35
sd(foulingCI[foulingCI$Gear=="FC", "Dry_shell"]) #6.67

fouling$shellChange<-fouling$Dry_shell-initialShell
shellChangeGraph1<-ggplot(data = fouling, aes(x = Gear, y = shellChange, fill=Location))+geom_boxplot()+ylab("Change in dry shell weight (g)")
shellChangeGraph1

artShellChange<-art(shellChange ~ Gear * Location, data=fouling)
anova(artShellChange)

#### NOT INCLUDING 8/2 ##################################
df_old<-df_updated[c(1:14),]
#Average difference
OutsideChlA_old<-df_old[df_old$Location=="Outside",'mean']
InsideChlA_old<-df_old[df_old$Location=="Inside",'mean']

differencesChlA_old<-data.frame(OutsideChlA_old,InsideChlA_old)
differencesChlA_old$Diff<-differencesChlA_old$InsideChlA_old-differencesChlA_old$OutsideChlA_old
meanDiffChlA_old<-mean(differencesChlA_old$Diff)
meanDiffChlA_old #0.73 μg/L
sdDiffChlA_old<-sd(differencesChlA_old$Diff)
sdDiffChlA_old #2.08

chlA_graph2<-ggplot(df_old, aes(x=Trial_Date, y=mean, group=Location, color=Location)) + 
  geom_line()+ylab("Chlorophyll A (μg/L)")+xlab("")+theme_classic()+theme(axis.title.y = element_text(margin = margin(r = 10)))
chlA_graph2

chlA_graph2_themed<-ggplot(df_old, aes(x=Trial_Date, y=mean, group=Location, color=Location))+ geom_line()+ylab("Chlorophyll A (μg/L)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)
chlA_graph2_themed
