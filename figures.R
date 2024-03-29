#Manuscript figures
#Updated 10/4/22

library(ggplot2)
library(ggsci)
library(ggpubr)
library(gridExtra)
library(Hmisc)
library(plotrix)
library(plyr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(hrbrthemes)
options(hrbrthemes.loadfonts = TRUE)
hrbrthemes::import_roboto_condensed()
library(tidyquant)
library(viridis)
library(mgcv)
library(visreg)
library(sm)
library(patchwork)

data_summary <- function(data, varname, groupnames){
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      SE = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)}

#Import data
allData<-read.csv("oysterDataAll.csv", na.strings=c(""," ","NA"))

#Fix date format
allData$Date<-mdy(allData$Date)

#Convert variables to factors
allData<-within(allData, {
  Cage<-as.factor(Cage)
  Bag<-as.factor(Bag)
  Location<-as.factor(Location)
  Gear<-as.factor(Gear)
  Treatment<-as.factor(Treatment)
  Replicate<-as.factor(Replicate)
})

allData$Replicate2<-paste0(allData$Treatment,".",allData$Replicate)
allData$Replicate2<-as.factor(allData$Replicate2)
str(allData)

#Import data
sept1<-read.csv("septData.csv", na.strings=c(""," ","NA"))

#Convert variables to factors
sept1<-within(sept1, {
  Cage<-as.factor(Cage)
  Bag<-as.factor(Bag)
  Location<-as.factor(Location)
  Gear<-as.factor(Gear)
  Treatment<-as.factor(Treatment)
  Replicate<-as.factor(Replicate)
})

sept1$Replicate2<-paste0(sept1$Treatment,".",sept1$Replicate)
sept1$Replicate2<-as.factor(sept1$Replicate2)
sept1$Date<-"09-13-2022"
sept1$Date<-mdy(sept1$Date)

sept<- sept1 %>% dplyr::select("Date","Location","Gear","Treatment","Cage","Bag","Oyster","Height","Length","Width","Cup.ratio","Shell.shape","Replicate","Replicate2")


plusSept<-rbind(allData,sept)
str(plusSept)

SamplingOne<-plusSept[plusSept$Date=="2022-06-14",]
SamplingTwo<-plusSept[plusSept$Date=="2022-07-05",]
SamplingThree<-plusSept[plusSept$Date=="2022-07-26",]
SamplingFour<-plusSept[plusSept$Date=="2022-08-15",]
SamplingFive<-plusSept[plusSept$Date=="2022-09-13",]

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

############################## Figure 1 - growth/condition index trade-off ###########
#Figure 1a: Shell height over time
timeGraphHeightDf<-data_summary(plusSept, "Height", 
                                groupnames=c("Date", "Location", "Gear"))

timeGraphHeight<-ggplot(timeGraphHeightDf, aes(x=Date, y=mean, color=Location, linetype=Gear)) +geom_line()+geom_point()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+theme_classic()+ylab("Shell height (mm)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 15)))
timeGraphHeight

#Figure 1b: Change in shell height from initial population, as RGR

SamplingFour$Change<-(SamplingFour$Height-47.64)/47.64/73

heightChange1<-ggplot(data = SamplingFour, aes(x = Gear, y = Change, fill=Location))+geom_boxplot()+ylab("RGR (% per day) ")+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)))
heightChange1

SamplingFour$Change2<-SamplingFour$Height-47.64
change2df<-data_summary(SamplingFour, "Change2", 
                                groupnames=c("Location", "Gear"))

SamplingFive$Change<-(SamplingFive$Height-47.64)/47.64

heightChangeSept<-ggplot(data = SamplingFive, aes(x = Gear, y = Change, fill=Location))+geom_boxplot()+ylab("GR (mm per day) ")+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)))
heightChangeSept

SamplingFive$Change2<-SamplingFive$Height-47.64
change2dfSept<-data_summary(SamplingFive, "Change2", 
                        groupnames=c("Location", "Gear"))


#Figure 1c: Change in condition index from initial population (by treatment), as % change

initialCImean<-mean(foulingCI[foulingCI$Treatment=="Initial","Condition_index"])
fouling<-foulingCI[foulingCI$Location=="Inside" | foulingCI$Location=="Outside",]

fouling$Change<-(fouling$Condition_index-initialCImean)/initialCImean/60
CIChange1<-ggplot(data = fouling, aes(x = Gear, y = Change, fill=Location))+geom_boxplot()+ylab("Change in condition index (% per day) ")+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 15)))+scale_y_continuous(limits=c(-0.02,0.03))#+scale_fill_manual(breaks = fouling$Location, values = c("white", "gray"))
CIChange1

fouling$Change2<-fouling$Condition_index-initialCImean
fouling2df<-data_summary(fouling, "Change2", 
                        groupnames=c("Location", "Gear"))
names(fouling2df)[3]<-"CI"
names(fouling2df)[4]<-"CISE"

change2df = change2df %>% 
  left_join(fouling2df, by = c("Location", "Gear"))
names(change2df)[3:4]<-c("heightChange","heightSE")

ggplot(data = change2df, aes(x = heightChange, y = CI, color=Location))+geom_point()+ylab("Change in CI")+ylab("Change in SH")+theme_classic()

ggplot(data = change2df) +
  geom_point(aes(x = heightChange, y = CI, color = Location, shape=Gear), size=3) +
  theme_bw() +geom_smooth(aes(x = heightChange, y = CI),method = "lm")+xlab("Δ shell height (mm)")+ylab("Δ condition index")+ theme(axis.title.y = element_text(margin = margin(r = 15)))

lm.temp = lm(CI ~ heightChange, data = change2df)
summary(lm.temp)

#Figure 1 - all
figure1.1<- heightChange1 + CIChange1 + plot_layout(nrow=1, guides = "collect")
figure1.2<-timeGraphHeight/figure1.1
figure1.2+ plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14)) #Fig. 1

############################# Fig. 3 - Appearance (cup ratio, shell shape deviation) ###########
timeGraphCupRatioDf<-data_summary(plusSept, "Cup.ratio", groupnames=c("Date", "Location", "Gear"))

timeGraphCupRatio<-ggplot(timeGraphCupRatioDf, aes(x=Date, y=mean, color=Location, linetype=Gear))+geom_line()+geom_point()+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)))+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+ylab("Cup ratio (SW/SH)")+xlab("")
timeGraphCupRatio

timeGraphShellShapeDf<-data_summary(plusSept, "Shell.shape", groupnames=c("Date", "Location", "Gear"))

timeGraphShellShape<-ggplot(timeGraphShellShapeDf, aes(x=Date, y=mean, color=Location, linetype=Gear))+geom_line() +geom_point()+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)))+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(0.05))+ylab("Shell shape deviation")+xlab("")
timeGraphShellShape

RatioTwo<-ggplot(data = SamplingFour, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+ylab("Cup ratio (SW/SH)")+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)))#+scale_y_continuous(limits=c(0,0.55))
RatioTwo

ShapeTwo<-ggplot(data = SamplingFour, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+ylab("Shell shape deviation")+theme_classic()+ theme(axis.title.y = element_text(margin = margin(r = 10)))
ShapeTwo

figure2<- timeGraphCupRatio + timeGraphShellShape + plot_layout(nrow=1, guides = "collect")
figure2

figure2.2<- RatioTwo + ShapeTwo + plot_layout(nrow=1, guides = "collect")
figure2.2

initialCupDf<-data_summary(SamplingOne, "Cup.ratio", groupnames=c("Date", "Location", "Gear"))
names(initialCupDf)[4:5]<-c("initialCupRatio", "initialSE")
finalCupDf<-data_summary(SamplingFour, "Cup.ratio", groupnames=c("Date", "Location", "Gear"))
names(finalCupDf)[4:5]<-c("finalCupRatio", "finalSE")

septCupDf<-data_summary(SamplingFive, "Cup.ratio", groupnames=c("Date", "Location", "Gear"))
names(septCupDf)[4:5]<-c("septCupRatio", "septSE")

finalCupDf = finalCupDf %>% 
  left_join(initialCupDf, by = c("Location", "Gear"))
finalCupDf$CupChange<-finalCupDf$finalCupRatio-finalCupDf$initialCupRatio

finalCupDf = finalCupDf %>% 
  left_join(change2df, by = c("Location", "Gear"))

ggplot(data = finalCupDf, aes(x = heightChange, y = CupChange, color=Location))+geom_point()+ylab("Change in Cup Ratio")+xlab("Change in SH")+theme_classic()

ggplot(data = finalCupDf) +
  geom_point(aes(x = heightChange, y = CupChange, color = Location, shape=Gear), size=3) +
  theme_bw() +geom_smooth(aes(x = heightChange, y = CupChange),method = "lm")+xlab("Δ shell height (mm)")+ylab("Δ cup ratio")+ theme(axis.title.y = element_text(margin = margin(r = 15)))

lm.cup = lm(CupChange ~ heightChange, data = finalCupDf)
summary(lm.cup)

septCupDf = septCupDf %>% 
  left_join(initialCupDf, by = c("Location", "Gear"))
septCupDf$CupChange<-septCupDf$septCupRatio-septCupDf$initialCupRatio

septCupDf = septCupDf %>% 
  left_join(change2dfSept, by = c("Location", "Gear"))

ggplot(data = septCupDf, aes(x = heightChange, y = CupChange, color=Location))+geom_point()+ylab("Change in Cup Ratio")+xlab("Change in SH")+theme_classic()

ggplot(data = septCupDf) +
  geom_point(aes(x = heightChange, y = CupChange, color = Location, shape=Gear), size=3) +
  theme_bw() +geom_smooth(aes(x = heightChange, y = CupChange),method = "lm")+xlab("Δ shell height (mm)")+ylab("Δ cup ratio")+ theme(axis.title.y = element_text(margin = margin(r = 15)))

lm.cupSept = lm(CupChange ~ heightChange, data = septCupDf)
summary(lm.cupSept)

#With replicates
initialCupDfRep<-data_summary(SamplingOne, "Cup.ratio", groupnames=c("Date", "Location", "Gear", "Replicate2"))
names(initialCupDfRep)[5:6]<-c("initialCupRatio", "initialSE")
finalCupDfRep<-data_summary(SamplingFour, "Cup.ratio", groupnames=c("Date", "Location", "Gear", "Replicate2"))
names(finalCupDfRep)[5:6]<-c("finalCupRatio", "finalSE")

finalCupDfRep = finalCupDfRep %>% 
  left_join(initialCupDfRep, by = c("Location", "Gear", "Replicate2"))
finalCupDfRep$CupChange<-finalCupDfRep$finalCupRatio-finalCupDfRep$initialCupRatio

change3df<-data_summary(SamplingFour, "Change2", 
                        groupnames=c("Location", "Gear", "Replicate2"))
names(change3df)[4:5]<-c("heightChange","heightSE")

finalCupDfRep = finalCupDfRep %>% 
  left_join(change3df, by = c("Location", "Gear","Replicate2"))

ggplot(data = finalCupDfRep, aes(x = heightChange, y = CupChange, color=Location))+geom_point()+ylab("Change in Cup Ratio")+xlab("Change in SH")+theme_classic()

ggplot(data = finalCupDfRep) +
  geom_point(aes(x = heightChange, y = CupChange, color = Location, shape=Gear), size=3) +
  theme_bw() +geom_smooth(aes(x = heightChange, y = CupChange, color=Location),method = "lm")+xlab("Δ shell height (mm)")+ylab("Δ cup ratio")+ theme(axis.title.y = element_text(margin = margin(r = 15)))

lm.cup = lm(CupChange ~ heightChange * Location * Gear, data = finalCupDfRep)
selectedCup<-step(lm.cup) #CupChange ~ heightChange + Location + Gear + heightChange:Gear
summary(selectedCup)

summary(lm.cup)
gvlma(selectedCup)

#With replicates - Sept

septCupDfRep<-data_summary(SamplingFive, "Cup.ratio", groupnames=c("Date", "Location", "Gear", "Replicate2"))
names(septCupDfRep)[5:6]<-c("finalCupRatio", "finalSE")

septCupDfRep = septCupDfRep %>% 
  left_join(initialCupDfRep, by = c("Location", "Gear", "Replicate2"))
septCupDfRep$CupChange<-septCupDfRep$finalCupRatio-septCupDfRep$initialCupRatio

changeSeptdf<-data_summary(SamplingFive, "Change2", 
                        groupnames=c("Location", "Gear", "Replicate2"))
names(changeSeptdf)[4:5]<-c("heightChange","heightSE")

septCupDfRep = septCupDfRep %>% 
  left_join(changeSeptdf, by = c("Location", "Gear","Replicate2"))

ggplot(data = septCupDfRep, aes(x = heightChange, y = CupChange, color=Location))+geom_point()+ylab("Change in Cup Ratio")+xlab("Change in SH")+theme_classic()

ggplot(data = septCupDfRep) +
  geom_point(aes(x = heightChange, y = CupChange, color = Location, shape=Gear), size=3) +
  theme_bw() +geom_smooth(aes(x = heightChange, y = CupChange, color=Location),method = "lm")+xlab("Δ shell height (mm)")+ylab("Δ cup ratio")+ theme(axis.title.y = element_text(margin = margin(r = 15)))

lm.cupSept2 = lm(CupChange ~ heightChange * Location * Gear, data = septCupDfRep)
selectedCupSept<-step(lm.cupSept2) #CupChange ~ heightChange + Location + Gear + heightChange:Gear
summary(selectedCupSept)

summary(lm.cupSept2)
############################## Figure 4 - Biofouling ###########

fouling_Graph1<-ggplot(data = fouling, aes(x = Gear, y = Fouling_ratio, fill=Location))+geom_boxplot()+ylab("Fouling ratio")+theme_classic()
fouling_Graph1

############################## Figure 2 - Environment ###########

#Fig. 2a - Temp
#Import HOBO data
HOBOdata_unordered<-read.csv("HOBOdata.csv")
#Convert date and location
HOBOdata_unordered$Date.time<-mdy_hms(HOBOdata_unordered$Date.time)
HOBOdata<-HOBOdata_unordered[order(HOBOdata_unordered$Location),]
HOBOdata$Location<-as.factor(HOBOdata$Location)
#Rename salinity column
names(HOBOdata)[3]<-"Salinity"
noAir<- HOBOdata[HOBOdata$Salinity>5,]

#Find hours when both sensors logged with no issues
Inside<-noAir[noAir$Location=="Inside",]
Outside<-noAir[noAir$Location=="Outside",]
InsideDates<-Inside$Date.time
OutsideDates<-Outside$Date.time
commonDates<-base::intersect(InsideDates, OutsideDates)
common<-noAir[(noAir$Date.time %in% commonDates),]
InsideCommon<-common[common$Location=='Inside',]
OutsideCommon<-common[common$Location=='Outside',]

#Calculate rolling 24-hour means
commonInside2 <- InsideCommon %>%
  mutate(daily_avg_temp= rollmean(Temp, k = 24, fill = NA))

commonOutside2 <- OutsideCommon %>%
  mutate(out_daily_avg_temp= rollmean(Temp, k = 24, fill = NA))

both_rolling<-c(commonInside2$daily_avg_temp, commonOutside2$out_daily_avg_temp)
common$rolling<-both_rolling

DailyAvgTemp<-ggplot(common, aes(x=Date.time, y=rolling, group=Location, color=Location))+geom_line()+ylab("Temperature (°C)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+
  theme(axis.title.y = element_text(margin = margin(r = 10)))#+ylim(12.5, 22.5)
DailyAvgTemp

#Fig. 2b - Salinity
#Calculate rolling mean
commonSalInside2 <- InsideCommon %>%
  mutate(daily_sal_avg= rollmean(Salinity, k = 24, fill = NA))

commonSalOutside2 <- OutsideCommon %>%
  mutate(out_sal_daily_avg= rollmean(Salinity, k = 24, fill = NA))

both_sal_rolling<-c(commonSalInside2$daily_sal_avg, commonSalOutside2$out_sal_daily_avg)
common$sal_rolling<-both_sal_rolling

DailyAvgSal<-ggplot(common, aes(x=Date.time, y=sal_rolling, group=Location, color=Location))+geom_line()+ylab("Conductivity (mS/cm)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))+ylim(25,35)
DailyAvgSal

#Fig. 2c - TCM

#Fig. 2d - ChlA
dateFix = function(df) {
  df$Trial_Date = as.Date(df$Trial, "%m/%d/%y")
  return(df)
}
chlaDatasheet = read.csv("chlA.csv")
ChlaDatasheet = dateFix(chlaDatasheet)
ChlaDatasheet = ChlaDatasheet[ChlaDatasheet$Major_issue == FALSE,]
ChlFs = 0.000482
FoFa_max = 1.7718

#Calculating Chla ug/L and Phaeo ug/L from Raw Data
ChlaDatasheet = ChlaDatasheet %>%
  mutate(Ave_Chl1 = (ChlFs*(FoFa_max/(FoFa_max-1))* 
                       (ChlaDatasheet$Fo-ChlaDatasheet$Fa)*
                       (((ChlaDatasheet$Acetone_vol)/ChlaDatasheet$Vol_Filtered))))

ChlaDatasheet = ChlaDatasheet %>%
  mutate(ChlaDatasheet, Ave_Phaeo1 = ((ChlFs*(FoFa_max/(FoFa_max-1)))*
                                        ((FoFa_max-1)*(ChlaDatasheet$Fo-ChlaDatasheet$Fa))*
                                        (((ChlaDatasheet$Acetone_vol)/ChlaDatasheet$Vol_Filtered))))

#Create summary data frame
df_updated<-data_summary(ChlaDatasheet, "Ave_Chl1", 
                         groupnames=c("Trial_Date", "Location"))
#with error bars
chlA_graph_error<-ggplot(df_updated, aes(x=Trial_Date, y=mean, group=Location, color=Location)) +geom_line()+ylab("Chlorophyll A (μg/L)")+xlab("")+ geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2, position=position_dodge(0.05))+theme_classic()+scale_y_continuous(limits=c(0,16.5))+ theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+ theme(axis.title.y = element_text(margin = margin(r = 15)))
chlA_graph_error

#no error bars
chlA_graph<-ggplot(df_updated, aes(x=Trial_Date, y=mean, group=Location, color=Location)) + 
  geom_line()+ylab("Chlorophyll A (μg/L)")+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+ theme(axis.title.y = element_text(margin = margin(r = 10)))+scale_y_continuous(limits=c(0,16))
chlA_graph

#Fig. 2e - Turbidity
#Import data
turbidity<-read.csv("turbidity.csv")

#Create summary data frame
df_turbdity<-data_summary(turbidity, "Turbidity", 
                          groupnames=c("Date", "Location"))
#Convert date format
df_turbdity$Date <- mdy(df_turbdity$Date)

#Standard plot
turbidity_graph<-ggplot(df_turbdity, aes(x=Date, y=mean, group=Location, color=Location)) + 
  geom_line()+theme_classic()+scale_y_continuous(limits=c(0,15))+ylab("Turbidity (NTU)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))
turbidity_graph

turbidity_graph_themed<-turbidity_graph+theme_ipsum_rc(axis_title_just="cc", axis_title_size = 13, axis_text_size = 10)+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))
turbidity_graph_themed

#Temp, salinity, ChlA, turbidity
allFour<-DailyAvgTemp/(DailyAvgSal + chlA_graph + turbidity_graph_themed) + plot_layout(nrow=2, byrow=FALSE, guides = "collect") & theme(legend.position = "bottom")
allFour