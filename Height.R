library(nlme)
library(lme4)
library(car)
library(MASS)
library(dplyr)
library(ARTool)
library(ggplot2)
library(agricolae)

#DAY 1 Height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SamplingOne<-read.csv("6_14_22_2.csv", na.strings=c(""," ","NA"))
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

heightOneGraph1<-ggplot(data = SamplingOne, aes(x = Gear, y = Height, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,75))+ylab("Shell height (mm)")
heightOneGraph1

heightOneGraph2 <- ggplot(SamplingOne, aes(x=Gear, y=Height, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
heightOneGraph2 + facet_grid(. ~ Location)

#ANOVA to demonstrate normality assumption not met
heightANOVA <- aov(Height ~ Gear * Location, data = SamplingOne)
summary(heightANOVA) #nothing significant
leveneTest(Height ~ Gear * Location, data = SamplingOne) #p=0.2035
plot(heightANOVA,1)
plot(heightANOVA,2)
heightResiduals<-heightANOVA$residuals
shapiro.test(heightResiduals) #p=0.00023

artHeightOne<-art(Height ~ Gear * Location, data=SamplingOne)
anova(artHeightOne) #no significant differences

#DAY 1 Linear Growth Rate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

heightChange<-(SamplingOne$Height-47.64)
SamplingOne$LGR<-heightChange/11

summary(SamplingOne$LGR)

LGROneGraph1<-ggplot(data = SamplingOne, aes(x = Gear, y = LGR, fill=Location))+geom_boxplot()+ylab("LGR (mm/day)")
LGROneGraph1

#ANOVA - everything the same as height, since just adding and multiplying by a constant
lgrANOVA <- aov(LGR ~ Gear * Location, data = SamplingOne)
summary(lgrANOVA) #nothing significant
leveneTest(LGR ~ Gear * Location, data = SamplingOne) #p=0.2035
plot(lgrANOVA,1)
plot(lgrANOVA,2)
lgrResiduals<-lgrANOVA$residuals
shapiro.test(lgrResiduals) #p=0.00023

artLGROne<-art(LGR ~ Gear * Location, data=SamplingOne)
anova(artLGROne) #no significant differences, everything the same as height

#DAY 2 Height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
summary(SamplingTwo)

heightTwoGraph1<-ggplot(data = SamplingTwo, aes(x = Gear, y = Height, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,85))+ylab("Shell height (mm)")
heightTwoGraph1

heightTwoGraph2<- ggplot(SamplingTwo, aes(x=Gear, y=Height, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
heightTwoGraph2 + facet_grid(. ~ Location)

#ANOVA - assumptions met!
heightANOVA2 <- aov(Height ~ Gear * Location, data = SamplingTwo)
summary(heightANOVA2) #significant location and interaction
leveneTest(Height ~ Gear * Location, data = SamplingTwo) #p=0.3146
plot(heightANOVA2,1)
plot(heightANOVA2,2)
heightResiduals2<-heightANOVA2$residuals
shapiro.test(heightResiduals2) #p=0.07

TukeyHSD(heightANOVA2, which='Gear:Location')

height_means <- 
  SamplingTwo %>% 
  group_by(Location, Gear) %>% # <- remember to group by *both* factors
  summarise(Means = mean(Height))
## `summarise()` regrouping output by 'Calluna' (override with `.groups` argument)
height_means

interaction_plot<-ggplot(height_means, aes(x = Gear, y = Means, colour = Location, group = Location)) +
  geom_point(size = 4) + geom_line()
interaction_plot

HSD.test(heightANOVA2, trt = c("Location", "Gear"), console = TRUE)

height_stats <- 
  SamplingOne %>% 
  group_by(Location,Gear) %>% # <- remember to group by the two factors
  summarise(Means = mean(Height), SEs = sd(Height)/sqrt(n()))
height_stats

columns<-ggplot(height_stats, 
       aes(x = Gear, y = Means, fill = Location,
           ymin = Means - SEs, ymax = Means + SEs)) +
  geom_col(position = position_dodge()) +
  # this adds the error bars
  geom_errorbar(position = position_dodge(0.9), width=.2)+theme_classic()

interaction_plot2<-ggplot(height_stats, aes(x = Gear, y = Means, color = Location, ymin = Means - SEs, ymax = Means + SEs)) + geom_point(size = 3, position =position_dodge(0.2)) + geom_errorbar(width = 0.1, position =position_dodge(0.2)) + ylab("Mean shell height (mm)")
interaction_plot2

interaction_plot3<-interaction.plot(x.factor = SamplingTwo$Gear, trace.factor = SamplingTwo$Location,  response = SamplingTwo$Height, fun = median, ylab = "Shell height", xlab = "Gear",col = c("pink", "blue"),lty = 1, lwd = 2,trace.label = "Location")

#DAY 2 Linear Growth Rate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

BP.replicate1<- SamplingOne[SamplingOne$Gear == "BP","Treatment"]
FB.replicate1<- SamplingOne[SamplingOne$Gear == "FB","Bag"]
FC.replicate1<- SamplingOne[SamplingOne$Gear == "FC","Cage"]
replicateColumn1<-c(BP.replicate1, FB.replicate1, FC.replicate1)
SamplingOne$Replicate<-replicateColumn1

heightRepMeansOne <- 
  SamplingOne %>% 
  group_by(Location, Gear, Replicate) %>%
  summarise(Means = mean(Height))
heightRepMeansOne

BP.replicate2<- SamplingTwo[SamplingTwo$Gear == "BP","Treatment"]
FB.replicate2<- SamplingTwo[SamplingTwo$Gear == "FB","Bag"]
FC.replicate2<- SamplingTwo[SamplingTwo$Gear == "FC","Cage"]
replicateColumn2<-c(BP.replicate2, FB.replicate2, FC.replicate2)
SamplingTwo$Replicate<-replicateColumn2

heightRepMeansTwo <- 
  SamplingTwo %>% 
  group_by(Location, Gear, Replicate) %>%
  summarise(Means = mean(Height))
heightRepMeansTwo

heightRepMeansTwo$Height_diff<-heightRepMeansTwo$Means-heightRepMeansOne$Means
