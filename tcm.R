#Seahorse data - RK
#Updated 9/11/22

#### read in libraries needed 
library(ggplot2)
library(dplyr)
library(lubridate)

### read in your csv files
FBdata <- read.csv("InnerFB_Accel.csv",header=TRUE)
FBdata<-rename(FBdata, "Datetime"="ISO.8601.Time", "accelX"="Ax..g.", "accelY"="Ay..g.", "accelZ"="Az..g.")

FCdata <- read.csv("InnerFC_Accel.csv",header=TRUE)
FCdata<-rename(FCdata, "Datetime"="ISO.8601.Time", "accelX"="Ax..g.", "accelY"="Ay..g.", "accelZ"="Az..g.")

#format your time as a POSICXct 
FBdata$Datetime<-ymd_hms(FBdata$Datetime)
FCdata$Datetime<-ymd_hms(FCdata$Datetime)

#### if you want to take the x,y, and z axis and turn it into a "motion index" you do this:
## acceleration
str(FBdata)
FBdata = FBdata %>% 
  mutate(
    diffX = accelX - lag(accelX),
    diffY = accelY - lag(accelY),
    diffZ = accelZ - lag(accelZ),
    motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
str(FBdata)

str(FCdata)
FCdata = FCdata %>% 
  mutate(
    diffX = accelX - lag(accelX),
    diffY = accelY - lag(accelY),
    diffZ = accelZ - lag(accelZ),
    motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
str(FCdata)

noFlips<-FBdata[FBdata$motionIndex<0.025,]
noOutlier<-FCdata[FCdata$motionIndex<6,]
noFlipsCages<-FCdata[FCdata$motionIndex<0.025,]

#acceleration plot FB all
FBplotAll<-ggplot(data=FBdata, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")
FBplotAll

#acceleration plot FB no flips
FBplot<-ggplot(data=noFlips, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")
FBplot

#acceleration plot FC all
FCplotAll<-ggplot(data=FCdata, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")
FCplotAll

#acceleration plot FC

FCplotAll<-ggplot(data=noOutlier, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")
FCplotAll





