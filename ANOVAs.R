library(nlme)
library(lme4)
library(car)
library(MASS)

#Read and format data
SamplingOne<-read.csv("6_14_22.csv", na.strings=c(""," ","NA"))
SamplingNewUnordered<-subset(SamplingOne, select = c(Location,Gear,Treatment,Cage,Bag,Oyster,Height,Length,Width,Cup.ratio,Shell.shape))
str(SamplingNewUnordered)

#Convert variables to factors
SamplingNewUnordered<-within(SamplingNewUnordered, {
  Cage<-as.factor(Cage)
  Bag<-as.factor(Bag)
  Location<-as.factor(Location)
  Gear<-as.factor(Gear)
  Treatment<-as.factor(Treatment)
})

str(SamplingNewUnordered)
table(SamplingNewUnordered$Location, SamplingNewUnordered$Gear)
SamplingNew <- SamplingNewUnordered[order(SamplingNewUnordered$Gear),]
BP.replicate<- SamplingNew[SamplingNew$Gear == "BP","Bag"]
FB.replicate<- SamplingNew[SamplingNew$Gear == "FB","Bag"]
FC.replicate<- SamplingNew[SamplingNew$Gear == "FC","Cage"]
replicateColumn<-c(BP.replicate, FB.replicate, FC.replicate)
SamplingNew$Replicate<-replicateColumn
str(SamplingNew)

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

str(IndividualOysters)
table(IndividualOysters$Location, IndividualOysters$Gear)
BP.replicate2<- IndividualOysters[IndividualOysters$Gear == "BP","Treatment"]
FB.replicate2<- IndividualOysters[IndividualOysters$Gear == "FB","Bag"]
FC.replicate2<- IndividualOysters[IndividualOysters$Gear == "FC","Bag"]
replicateColumn2<-c(BP.replicate2, FB.replicate2, FC.replicate2)
IndividualOysters$Replicate<-replicateColumn2
str(IndividualOysters)

#Treat bags as replicates for FB & cages as replicates for FC
IndividualOysters$Replicate[IndividualOysters$Cage==1]<-as.factor(7)
IndividualOysters$Replicate[IndividualOysters$Cage==2]<-as.factor(8)
IndividualOysters$Replicate[IndividualOysters$Cage==3]<-as.factor(9)
IndividualOysters$Replicate[IndividualOysters$Cage==4]<-as.factor(10)
IndividualOysters$Replicate[IndividualOysters$Cage==5]<-as.factor(11)
IndividualOysters$Replicate[IndividualOysters$Cage==6]<-as.factor(12)


#Cup ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(data = SamplingNew, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,0.5))+ylab("Cup ratio (shell width/height)")

bw2 <- ggplot(IndividualOysters, aes(x=Gear, y=Cup.ratio, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
bw2 + facet_grid(. ~ Location)

cupRatioANOVA <- aov(Cup.ratio ~ Gear * Location, data = SamplingNew)
summary(cupRatioANOVA)
leveneTest(Cup.ratio ~ Gear * Location, data = SamplingNew) #p=0.4658
plot(cupRatioANOVA,1)
plot(cupRatioANOVA,2)
cupRatioResiduals<-cupRatioANOVA$residuals
shapiro.test(cupRatioResiduals) #highly non-normal, p=2e-12

cupRatioANOVA2 <- aov(Cup.ratio ~ Gear * Location, data = IndividualOysters)
summary(cupRatioANOVA2)
leveneTest(Cup.ratio ~ Gear * Location, data = IndividualOysters) #p=0.4658
plot(cupRatioANOVA2,1)
plot(cupRatioANOVA,2)
cupRatioResiduals<-cupRatioANOVA$residuals
shapiro.test(cupRatioResiduals)

#ANOVA of log cup ratio - improves normality
IndividualOysters$log.Cup.ratio<-log(IndividualOysters$Cup.ratio)
leveneTest(log.Cup.ratio ~ Gear * Location, data = IndividualOysters) #good, p=0.2556
logCupRatioANOVA <- aov(log.Cup.ratio ~ Gear * Location, data = IndividualOysters)
summary(logCupRatioANOVA)
plot(logCupRatioANOVA,1)
plot(logCupRatioANOVA,2)
hist(logCupRatioANOVA$residuals)
shapiro.test(logCupRatioANOVA$residuals) #p=1.837e-5

library(WRS2)
t2way(Cup.ratio ~ Gear * Location, data=IndividualOysters)
postCupRatio<-mcp2atm(Cup.ratio ~ Gear * Location, data=IndividualOysters)
postCupRatio

library(ARTool)
alignedOysters<-art(Cup.ratio ~ Gear * Location, data=IndividualOysters)
anova(alignedOysters)

#Height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
leveneTest(Height~Gear*Location, data=IndividualOysters) #good



#Shell Shape ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(data = IndividualOysters, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+ylab("Shell shape index")
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

ggplot(data = IndividualOysters, aes(x = Gear, y = cube.Shell.shape, fill=Location))+geom_boxplot()+ylab("Shell shape index")

