#Graphs
#RK 7/13/22

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

#Cup ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Overall plot day 1 - combined
CupRatioOne<-ggplot(data = SamplingOne, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,0.5))+ylab("Cup ratio (shell width/height)")

#Overall plot day 1 - separated by location
CupRatioTwo <- ggplot(SamplingOne, aes(x=Gear, y=Cup.ratio, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
CupRatioTwo + facet_grid(. ~ Location)

#Shell height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Overall plot day 1 - combined
HeightOne<-ggplot(data = SamplingOne, aes(x = Gear, y = Height, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,80))+ylab("Shell height (mm)")
HeightOne

#Overall plot day 1 - combined
HeightTwo <- ggplot(SamplingOne, aes(x=Gear, y=Height, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
HeightTwo + facet_grid(. ~ Location)

#Shell shape ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Overall plot day 1 - combined
ShapeOne<-ggplot(data = SamplingOne, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,8))+ylab("Shell shape index")
ShapeOne

#Overall plot day 1 - combined
ShapeTwo <- ggplot(SamplingOne, aes(x=Gear, y=Shell.shape, group=Gear)) + 
  geom_boxplot(aes(fill=Gear))
ShapeTwo + facet_wrap(. ~ Location)

#All day 1 combined plots~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grid.arrange(HeightOne, CupRatioOne, ShapeOne, ncol=3)
