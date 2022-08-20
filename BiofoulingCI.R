#Biofouling and Condition Index
#RK 8/19/22

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

#import and subset data
foulingCI<-read.csv("BiofoulingCI.csv")
foulingCI<-subset(foulingCI, select=c(Location,Gear,Treatment,Oyster,Fouling_weight, Whole_wet_weight, Fouling_ratio, Dry_tissue, Dry_shell, Condition_index))
foulingCI<-rename(foulingCI, Weight=Whole_wet_weight)

#Convert variables to factors
foulingCI<-within(foulingCI, {
  Location<-as.factor(Location)
  Gear<-as.factor(Gear)
  Treatment<-as.factor(Treatment)
})
str(foulingCI)
table(foulingCI$Location, foulingCI$Gear)

#only comparing final treatments
fouling<-foulingCI[foulingCI$Location=="Inside" | foulingCI$Location=="Outside",]

#Graphs
weightGraph1<-ggplot(data = foulingCI, aes(x = Gear, y = Weight, fill=Location))+geom_boxplot()+ylab("Whole wet weight (g)")
weightGraph1

CI_Graph1<-ggplot(data = foulingCI, aes(x = Gear, y = Condition_index, fill=Location))+geom_boxplot()+ylab("Condition index")
CI_Graph1

fouling_Graph1<-ggplot(data = fouling, aes(x = Gear, y = Fouling_ratio, fill=Location))+geom_boxplot()+ylab("Fouling ratio")
fouling_Graph1

#both Levene's test and Shapiro-Wilk were significant for weight & fouling ratio, so using nonparametric ANOVA
artWeight<-art(Weight ~ Gear * Location, data=fouling)
artWeight
anova(artWeight) #location significant, p=0.01

artFouling<-art(Fouling_ratio ~ Gear * Location, data=fouling)
artFouling
foulingANOVA<-anova(artFouling) #gear significant, p<2e-16, location significant p=0.045, interaction significant p=0.018

art.con(artFouling, "Gear:Location", adjust="bonferroni") %>%  #post-hoc fouling
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))


artCI<-art(Condition_index ~ Gear * Location, data=fouling)
artCI
anova(artCI)

art.con(artCI, "Gear:Location", adjust="bonferroni") %>%  #post-hoc fouling
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))