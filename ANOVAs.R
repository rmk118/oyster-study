library(nlme)
library(lme4)
library(car)
library(MASS)
library(dplyr)
library(ARTool)
library(WRS2)

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

###############################################################################
######################### No random effects  ########################################
###############################################################################

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

###############################################################################
######################### Replicate 1-3, BP = NULL  ########################################
###############################################################################

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


###############################################################################
######################### Replicate 1-3, BP = BPi/BPo  ########################################
###############################################################################

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

###############################################################################
######################### Replicate 1-12, BP = BPi/BPo  ########################################
###############################################################################

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








