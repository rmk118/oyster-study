#Seahorse data - RK
#Updated 9/11/22

#### read in libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyquant)
library(patchwork)

### read in csv files
FBdata<- read.csv("Inner_FB_Accel_10.6.22.csv",header=TRUE)
FBdata<-rename(FBdata, "Datetime"="ISO.8601.Time", "accelX"="Ax..g.", "accelY"="Ay..g.", "accelZ"="Az..g.")
FBdata<-subset(FBdata, select=c(Datetime, accelX, accelY, accelZ))

OutFCdata<- read.csv("Outer_Cage_Accel_10.6.22.csv",header=TRUE)
OutFCdata<-rename(OutFCdata, "Datetime"="ISO.8601.Time", "accelX"="Ax..g.", "accelY"="Ay..g.", "accelZ"="Az..g.")
OutFCdata<-subset(OutFCdata, select=c(Datetime, accelX, accelY, accelZ))

InFCdata<- read.csv("Inner_Cage_Accel_10.6.22.csv",header=TRUE)
InFCdata<-rename(InFCdata, "Datetime"="ISO.8601.Time", "accelX"="Ax..g.", "accelY"="Ay..g.", "accelZ"="Az..g.")
InFCdata<-subset(InFCdata, select=c(Datetime, accelX, accelY, accelZ))


#format time as a POSICXct 
FBdata$Datetime<-ymd_hms(FBdata$Datetime)
OutFCdata$Datetime<-ymd_hms(OutFCdata$Datetime)
InFCdata$Datetime<-ymd_hms(InFCdata$Datetime)

# #### take the x,y, and z axis and turn it into a "motion index":
str(FBdata)
FBdata = FBdata %>%
  mutate(
    diffX = accelX - lag(accelX),
    diffY = accelY - lag(accelY),
    diffZ = accelZ - lag(accelZ),
    motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
str(FBdata)

str(OutFCdata)
OutFCdata = OutFCdata %>% 
  mutate(
    diffX = accelX - lag(accelX),
    diffY = accelY - lag(accelY),
    diffZ = accelZ - lag(accelZ),
    motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
str(OutFCdata)

str(InFCdata)
InFCdata = InFCdata %>% 
  mutate(
    diffX = accelX - lag(accelX),
    diffY = accelY - lag(accelY),
    diffZ = accelZ - lag(accelZ),
    motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
str(InFCdata)

# noFlips<-FBdata[FBdata$motionIndex<0.025,]
# noFlipsCages<-FCdata[FCdata$motionIndex<0.025,]

# acceleration plot FB all
FBplotAll<-ggplot(data=FBdata, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")
FBplotAll

# acceleration plot outside FC all
OutFCplotAll<-ggplot(data=OutFCdata, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")
OutFCplotAll

# acceleration plot inside FC
InFCplotAll<-ggplot(data=InFCdata, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")
InFCplotAll


# #Average FC data over 10-minute intervals
# FCday1 <- read.csv("InnerFC_Accel.csv",header=TRUE)
# FCday1<-rename(FCday1, "Datetime"="ISO.8601.Time", "accelX"="Ax..g.", "accelY"="Ay..g.", "accelZ"="Az..g.")
# FCday1$Datetime<-ymd_hms(FCday1$Datetime)
# 
# FCday1<-FCday1[FCday1$Datetime %within% day1,]
# FCday1a = FCday1 %>% 
#   mutate(
#     diffX = accelX - lag(accelX),
#     diffY = accelY - lag(accelY),
#     diffZ = accelZ - lag(accelZ),
#     motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
# str(FCday1a)
# 
# FCplotDay1a<-ggplot(data=FCday1a, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "Time", y = "Motion")+ggtitle("All cage data (1 measurement/sec)")
# FCplotDay1a
# 
# FCday1a <- FCday1a %>%
#   mutate(rolling= rollmean(motionIndex, k = 600, fill = NA))
# FCday1a$Gear<-"FC"
# # 
# # FBday1a <- FBday1 %>%
# #   mutate(rolling= rollmean(motionIndex, k = 6, fill = NA))
# FBday1a$rolling<-FBday1a$motionIndex
# FBday1a$Gear<-"FB"


combined<- FBplotAll + InFCplotAll + (OutFCplotAll+ggtitle("Outside cages")) + plot_layout(ncol=1, guides = "collect")

# Remove title from first subplot
combined[[1]] = combined[[1]] + theme(axis.text.x = element_blank(),
                                        axis.ticks.x = element_blank(),
                                        axis.title.x = element_blank() )+ggtitle("Inside bags")

# Remove title from second subplot
combined[[2]] = combined[[2]] + theme(axis.text.x = element_blank(),
                                       axis.ticks.x = element_blank(),
                                       axis.title.x = element_blank() )+ggtitle("Inside cages")

combined



