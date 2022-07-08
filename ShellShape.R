library(nlme)
library(lme4)
library(car)
library(MASS)
library(dplyr)
library(ARTool)

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

ggplot(data = IndividualOysters, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+ylab("Shell shape index")

bw2 <- ggplot(IndividualOysters, aes(x=Gear, y=Shell.shape, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
bw2 + facet_grid(. ~ Location)

#ANOVA to demonstrate normality assumption not met
leveneTest(Shell.shape~Gear*Location, data=IndividualOysters) #marginally non-significant, p=0.05086
ShellShapeANOVA <- aov(Shell.shape ~ Gear * Location, data = IndividualOysters)
summary(ShellShapeANOVA) #location and interaction significant
plot(ShellShapeANOVA,1)
plot(ShellShapeANOVA,2)
shapiro.test(ShellShapeANOVA$residuals) #bad, p=2.2e-16

#lm with no random effect; location significant
alignedOystersShellShape<-art(Shell.shape ~ Gear * Location, data=IndividualOysters)
anova(alignedOystersShellShape)

#mixed effects linear model with replicate as random effect; location significant
alignedOystersShellShape2<-art(Cup.ratio ~ Gear * Location + (1|Replicate), data=IndividualOysters)
anova(alignedOystersShellShape2)

#mixed effects linear model with bag as random effect; location significant
alignedOystersShellShape3<-art(Cup.ratio ~ Gear * Location + (1|Bag), data=IndividualOysters)
anova(alignedOystersShellShape3)

art.con(alignedOystersShellShape, "Location", adjust="holm") %>%
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))