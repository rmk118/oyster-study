#packages: ggplot2, car

# initialHeights1<-c(63,32,48,61,71,41,58,69,33,34,70,61,61,51,39,45,61,60,61,67,60,55,40,69,55,29,60,46,46,53,75,42,21,35,72,61,48,46,47,73,55,31,43,37,52,56,52,40,63,48,50,19,59,59,47,63,41,41,26,35,48,64,44,60,40,49,47,35,55,45,60,42,42,46,49,56,48,45,40,37,60,59,37,28,51,61,45,35,26,54,41,44,58,39,41,47,34,47,50,52,55,43,44,45,41,26,50,38,41,58,42,37,36,48,28,24,32,29,45,70,64,67,24,35,63,56,41,37,47,55,38,75,47,43,40,39,65,50,42,42,40,60,28,36,60,64,48,48,50,49,33,34,40,43,59,46,30,38,58,43,45,35,34,37,48,58,58,42,46,57,44,24,40,40,41,37,59,57,68,40,45,49,50,35,35,30,36,30,32,70,34,41,54,29,43,52,32,37,38,70,42,32,56,37,35,58,50,38,42,43,55,60,43,68,46,60,53,55,59,55,32,50,41,64,37,59,57,52,55,50,38,60,61,37,60,62,55,45,63,47,35,62,61,65,36,62,59,42,39,49,35,50,49,46,43,45,34,44,44,30,41,49,45,41,42,48,32,80,69,69,50,46,57,45,59,63,56,51,45,51)
# 
# oysterAvgInitial<-mean(initialHeights1)
# initialHeightSD<-sd(initialHeights1)
# shapiro.test(initialHeights1)
# upperLimit<-oysterAvg+2*initialHeightSD
# lowerLimit<-oysterAvg-2*initialHeightSD
# initialHeights<-data.frame(initialHeights1)
# ggplot(initialHeights,aes(x=initialHeights1))+geom_histogram(binwidth = 5, fill="#69b3a2", color="#e9ecef")+theme_classic()+xlab("Shell height")+ylab("Frequency")
# 
# FBout<-read.csv("FBout.csv")
# 
# #Testing height for normality (all good)
# bag1height<- FBout[FBout$Bag == 1,"Height"]
# shapiro.test(bag1height)
# bag2height<- FBout[FBout$Bag == 2,"Height"]
# shapiro.test(bag2height)
# bag3height<- FBout[FBout$Bag == 3,"Height"]
# shapiro.test(bag3height)
# bag4height<- FBout[FBout$Bag == 4,"Height"]
# shapiro.test(bag4height)
# 
# #Testing length for normality (all good)
# bag1length<- FBout[FBout$Bag == 1,"Length"]
# shapiro.test(bag1length)
# bag2length<- FBout[FBout$Bag == 2,"Length"]
# shapiro.test(bag2length)
# bag3length<- FBout[FBout$Bag == 3,"Length"]
# shapiro.test(bag3length)
# bag4length<- FBout[FBout$Bag == 4,"Length"]
# shapiro.test(bag4length)
# 
# #Testing width for normality
# bag1width<- FBout[FBout$Bag == 1,"Width"]
# shapiro.test(bag1width) #Bag 1 good
# bag2width<- FBout[FBout$Bag == 2,"Width"]
# shapiro.test(bag2width) #Bag 2 not normal, p=0.0231
# bag3width<- FBout[FBout$Bag == 3,"Width"]
# shapiro.test(bag3width) #Bag 3 good
# bag4width<- FBout[FBout$Bag == 4,"Width"]
# shapiro.test(bag4width) #Bag 4 not normal, p=0.04589
# 
# summary(FBout)
# NewBag<-as.character(FBout$Bag)
# FBout$Bag<-NewBag
# leveneTest(Height~Bag, data=FBout)
# HeightResults<-aov(Height~Bag, data=FBout)
# shapiro.test(HeightResults$residuals)
# summary(HeightResults)
# 
# leveneTest(Length~Bag, data=FBout)
# LengthResults<-aov(Length~Bag, data=FBout)
# shapiro.test(LengthResults$residuals)
# summary(LengthResults)
# 
# FBout$log.Width<-log(FBout$Width)
# 
# bag1logwidth<- FBout[FBout$Bag == "1","log.Width"]
# shapiro.test(bag1logwidth)
# bag2logwidth<- FBout[FBout$Bag == "2","log.Width"]
# shapiro.test(bag2logwidth)
# bag3logwidth<- FBout[FBout$Bag == "3","log.Width"]
# shapiro.test(bag3logwidth)
# bag4logwidth<- FBout[FBout$Bag == "4","log.Width"]
# shapiro.test(bag4logwidth)
# 
# kruskal.test(Width~Bag, data=FBout)
# 
# WidthResults<-aov(Width~Bag, data=FBout)
# summary(WidthResults)
# 
# ggplot(data = FBout, aes(x = Bag, y = Height))+geom_boxplot()+theme_classic()+xlab("Bag")+ylab("Height")+theme(text = element_text(size=12))
# ggplot(data = FBout, aes(x = Bag, y = Length))+geom_boxplot()+theme_classic()+xlab("Bag")+ylab("Length")+theme(text = element_text(size=12))

SamplingOne<-read.csv("6_14_22.csv")
SamplingNew<-SamplingOne[c(1:320,353:608),]
ggplot(data = SamplingNew, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,0.5))

ggplot(data = SamplingNew, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,8))



