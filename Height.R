library(nlme)
library(lme4)
library(car)
library(MASS)
library(dplyr)
library(ARTool)

#DAY 1 Height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IndividualOysters<-read.csv("6_14_22_2.csv", na.strings=c(""," ","NA"))
IndividualOysters<-subset(IndividualOysters, select = c(Location,Gear,Treatment,Cage,Bag,Oyster,Height,Length,Width,Cup.ratio,Shell.shape))

#Convert variables to factors
IndividualOysters<-within(IndividualOysters, {
  Cage<-as.factor(Cage)
  Bag<-as.factor(Bag)
  Location<-as.factor(Location)
  Gear<-as.factor(Gear)
  Treatment<-as.factor(Treatment)
})

BP.replicate2<- IndividualOysters[IndividualOysters$Gear == "BP","Treatment"]
FB.replicate2<- IndividualOysters[IndividualOysters$Gear == "FB","Bag"]
FC.replicate2<- IndividualOysters[IndividualOysters$Gear == "FC","Bag"]
replicateColumn2<-c(BP.replicate2, FB.replicate2, FC.replicate2)
IndividualOysters$Replicate<-replicateColumn2

#Treat bags as replicates for FB & cages as replicates for FC
IndividualOysters$Replicate[IndividualOysters$Cage==1]<-as.factor(7)
IndividualOysters$Replicate[IndividualOysters$Cage==2]<-as.factor(8)
IndividualOysters$Replicate[IndividualOysters$Cage==3]<-as.factor(9)
IndividualOysters$Replicate[IndividualOysters$Cage==4]<-as.factor(10)
IndividualOysters$Replicate[IndividualOysters$Cage==5]<-as.factor(11)
IndividualOysters$Replicate[IndividualOysters$Cage==6]<-as.factor(12)

str(IndividualOysters)

ggplot(data = IndividualOysters, aes(x = Gear, y = Height, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,75))+ylab("Shell height (mm)")

bw2 <- ggplot(IndividualOysters, aes(x=Gear, y=Height, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
bw2 + facet_grid(. ~ Location)

#ANOVA to demonstrate normality assumption not met
heightANOVA <- aov(Height ~ Gear * Location, data = IndividualOysters)
summary(heightANOVA) #nothing significant
leveneTest(Height ~ Gear * Location, data = IndividualOysters) #p=0.2035
plot(heightANOVA,1)
plot(heightANOVA,2)
heightResiduals<-heightANOVA$residuals
shapiro.test(heightResiduals) #p=0.00023

#lm with no random effect; no significance
alignedOystersHeight<-art(Height ~ Gear * Location, data=IndividualOysters)
anova(alignedOystersHeight)

#mixed effects linear model with replicate as random effect; no significance
alignedOystersHeight2<-art(Height ~ Gear * Location + (1|Replicate), data=IndividualOysters)
anova(alignedOystersHeight2)

#mixed effects linear model with bag as random effect; location significant
alignedOystersHeight3<-art(Height ~ Gear * Location + (1|Bag), data=IndividualOysters)
anova(alignedOystersHeight3)