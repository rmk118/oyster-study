#Model Options - Cup ratio example
#RK 7/11/22

library(nlme)
library(lme4)
library(car)
library(MASS)
library(dplyr)
library(ARTool)
#library(WRS2)

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

 postHoc<-art.con(artCupRatioOne, "Location", adjust="holm") %>%  # run ART-C for X1 × X2
   summary(postHoc) #%>%   add significance stars to the output
      # mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
      #                  cutpoints = c(0, .001, .01, .05, .10, 1),
      #                  symbols = c("***", "**", "*", ".", " ")))

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