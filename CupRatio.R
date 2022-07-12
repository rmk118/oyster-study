library(nlme)
library(lme4)
library(car)
library(MASS)
library(dplyr)
library(ARTool)

#DAY 1 Cup Ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

BP.replicate2<- IndividualOysters[IndividualOysters$Gear == "BP","Bag"]
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
IndividualOysters<-droplevels(IndividualOysters)
str(IndividualOysters)

ggplot(data = IndividualOysters, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,0.5))+ylab("Cup ratio (shell width/height)")

bw2 <- ggplot(IndividualOysters, aes(x=Gear, y=Cup.ratio, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
bw2 + facet_grid(. ~ Location)

#ANOVA to demonstrate normality assumption not met
cupRatioANOVA <- aov(Cup.ratio ~ Gear * Location, data = SamplingNew)
summary(cupRatioANOVA) #location significant
leveneTest(Cup.ratio ~ Gear * Location, data = SamplingNew) #p=0.4658
plot(cupRatioANOVA,1)
plot(cupRatioANOVA,2)
cupRatioResiduals<-cupRatioANOVA$residuals
shapiro.test(cupRatioResiduals) #highly non-normal, p=2e-12

#lm with no random effect; both gear and location significant
alignedOystersCupRatio<-art(Cup.ratio ~ Gear * Location, data=IndividualOysters)
anova(alignedOystersCupRatio)

#mixed effects linear model with replicate as random effect; location significant
alignedOystersCupRatio2<-art(Cup.ratio ~ Gear * Location + (1|Replicate), data=IndividualOysters)
anova(alignedOystersCupRatio2)

#mixed effects linear model with bag as random effect; location significant
alignedOystersCupRatio3<-art(Cup.ratio ~ Gear * Location + (1|Bag), data=IndividualOysters)
anova(alignedOystersCupRatio3)

art.con(alignedOystersCupRatio, "Gear", adjust="holm") %>%
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))