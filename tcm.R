#Seahorse data - RK
#Updated 9/11/22

#### read in libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyquant)
library(patchwork)

### read in csv files
# FBdata <- read.csv("InnerFB_Accel.csv",header=TRUE)
# FBdata<-rename(FBdata, "Datetime"="ISO.8601.Time", "accelX"="Ax..g.", "accelY"="Ay..g.", "accelZ"="Az..g.")

FCdata <- read.csv("InnerFC_Accel.csv",header=TRUE)
FCdata<-rename(FCdata, "Datetime"="ISO.8601.Time", "accelX"="Ax..g.", "accelY"="Ay..g.", "accelZ"="Az..g.")

# #format time as a POSICXct 
# FBdata$Datetime<-ymd_hms(FBdata$Datetime)
FCdata$Datetime<-ymd_hms(FCdata$Datetime)

# #### take the x,y, and z axis and turn it into a "motion index":
# str(FBdata)
# FBdata = FBdata %>% 
#   mutate(
#     diffX = accelX - lag(accelX),
#     diffY = accelY - lag(accelY),
#     diffZ = accelZ - lag(accelZ),
#     motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
# str(FBdata)

str(FCdata)
FCdata = FCdata %>% 
  mutate(
    diffX = accelX - lag(accelX),
    diffY = accelY - lag(accelY),
    diffZ = accelZ - lag(accelZ),
    motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
str(FCdata)

# noFlips<-FBdata[FBdata$motionIndex<0.025,]
noOutlier<-FCdata[FCdata$motionIndex<6,]
noFlipsCages<-FCdata[FCdata$motionIndex<0.025,]
# 
# #acceleration plot FB all
# FBplotAll<-ggplot(data=FBdata, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")
# FBplotAll

# #acceleration plot FB no flips
# FBplot<-ggplot(data=noFlips, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")
# FBplot
# 
# #acceleration plot FC all
# FCplotAll<-ggplot(data=FCdata, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")
# FCplotAll

# #acceleration plot FC
# FCplotAll<-ggplot(data=noOutlier, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")
# FCplotAll

#Create new dataframe for FB for only the first day
FBday1 <- read.csv("InnerFB_Accel.csv",header=TRUE)
FBday1<- FBday1 %>% select(ISO.8601.Time, Ax..g.,Ay..g., Az..g.)
FBday1<-rename(FBday1, "Datetime"="ISO.8601.Time", "accelX"="Ax..g.", "accelY"="Ay..g.", "accelZ"="Az..g.")
FBday1$Datetime<-ymd_hms(FBday1$Datetime)

startTime<-ymd_hms("2022-06-09 14:48:00")
endTime<-ymd_hms("2022-06-09 19:55:43")
day1<-interval(startTime, endTime)

FBday1<-FBday1[FBday1$Datetime %within% day1,]
FBday1 = FBday1 %>% 
  mutate(
    diffX = accelX - lag(accelX),
    diffY = accelY - lag(accelY),
    diffZ = accelZ - lag(accelZ),
    motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
str(FBday1)

FBplotDay1<-ggplot(data=FBday1, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")+ ggtitle("Inside floating bags")+ylim(0,0.005)
FBplotDay1

#Average FC data over 10-minute intervals

FCday1 <- read.csv("InnerFC_Accel.csv",header=TRUE)
FCday1<-rename(FCday1, "Datetime"="ISO.8601.Time", "accelX"="Ax..g.", "accelY"="Ay..g.", "accelZ"="Az..g.")
FCday1$Datetime<-ymd_hms(FCday1$Datetime)

FCday1<-FCday1[FCday1$Datetime %within% day1,]
FCday1a = FCday1 %>% 
  mutate(
    diffX = accelX - lag(accelX),
    diffY = accelY - lag(accelY),
    diffZ = accelZ - lag(accelZ),
    motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
str(FCday1a)

originalLength<-length(FCday1a$Datetime) #18464
FCday1a<-FCday1a[FCday1a$motionIndex<0.005,]
newLength<-length(FCday1a$Datetime) #15853

FCplotDay1a<-ggplot(data=FCday1a, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")+ggtitle("All cage data (1 measurement/sec)")
FCplotDay1a

FCday1a <- FCday1a %>%
  mutate(rolling= rollmean(motionIndex, k = 600, fill = NA))
FCday1a$Gear<-"FC"
# 
# FBday1a <- FBday1 %>%
#   mutate(rolling= rollmean(motionIndex, k = 6, fill = NA))
FBday1a$rolling<-FBday1a$motionIndex
FBday1a$Gear<-"FB"

# 
# FBrollingA<-ggplot(FBday1a, aes(x=Datetime, y=rolling))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")+ ggtitle("Inside floating cages")
# FBrollingA
# 
# FCrollingA<-ggplot(FCday1a, aes(x=Datetime, y=rolling))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")+ ggtitle("Inside floating cages")
# FCrollingA

both<-rbind(FBday1a, FCday1a)
ggplot(both, aes(x=Datetime, y=rolling, group=Gear, color=Gear))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")+theme(axis.title.y = element_text(margin = margin(r = 10)))

FBplotDay1 + FCrollingA + plot_layout(nrow=1)


FBrollingA + FCrollingA + plot_layout(nrow=1)



