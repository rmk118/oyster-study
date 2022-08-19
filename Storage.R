#Storage document
#Updated 8/19/22


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