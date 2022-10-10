#Biofouling and Condition Index
#RK 10/10/22

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

weightGraph2<-ggplot(data = fouling, aes(x = Gear, y = Weight, fill=Location))+geom_boxplot()+ylab("Whole wet weight (g)")
weightGraph2

CI_Graph1<-ggplot(data = foulingCI, aes(x = Gear, y = Condition_index, fill=Location))+geom_boxplot()+ylab("Condition index")
CI_Graph1

CI_Graph2<-ggplot(data = fouling, aes(x = Gear, y = Condition_index, fill=Location))+geom_boxplot()+ylab("Condition index")
CI_Graph2

fouling_Graph1<-ggplot(data = fouling, aes(x = Gear, y = Fouling_ratio, fill=Location))+geom_boxplot()+ylab("Fouling ratio")
fouling_Graph1

#all residuals highly non-normal plus significant Levene's tests for fouling ratio, so using nonparametric ANOVA

#location significant for weight, p=0.01
artWeight<-art(Weight ~ Gear * Location, data=fouling)
artWeight
anova(artWeight)

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

art.con(artCI, "Gear:Location") %>%  #post-hoc condition index
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))


art.con(artCI, "Gear") %>%  #post-hoc condition index
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

art.con(artCI, "Location") %>%  #post-hoc condition index
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

data_summary <- function(data, varname, groupnames){
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      SE = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-plyr::ddply(data, groupnames, .fun=summary_func, varname)
  return(data_sum)}

mean(fouling[fouling$Gear=="BP", "Condition_index"]) #11.00
sd(fouling[fouling$Gear=="BP", "Condition_index"]) #2.32
mean(fouling[fouling$Gear=="FC", "Condition_index"]) #10.28
sd(fouling[fouling$Gear=="FC", "Condition_index"]) #2.35
mean(fouling[fouling$Gear=="FB", "Condition_index"]) #9.81
sd(fouling[fouling$Gear=="FB", "Condition_index"]) #3.66

CIgraphDf<-data_summary(fouling, "Condition_index", groupnames=c("Location", "Gear"))

CIline_graph<-ggplot(CIgraphDf, aes(x=Gear, y=mean, color=Location, group=Location))+geom_line()+geom_point()+theme_classic()+ylab("Condition index")+xlab("Gear")+theme(axis.title.y = element_text(margin = margin(r = 15)))+ylim(8,13)#+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))
CIline_graph
