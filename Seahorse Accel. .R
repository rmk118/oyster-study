#### read in libraries needed 
library(ggplot2)
library(dplyr)

### read in your csv file that is from Domino
data <- read.csv( file.choose(),header=TRUE)  

#format your time as a POSICXct 
data$Datetime <- as.POSIXct(ints$Datetime, format = '%m/%d/%y %H:%M:%S')

#plot the accelercation 
Plot = ggplot(data, aes(x = Datetime, y = Speed..cm.s.))+
  geom_line()+
  ggtitle("TITLE")+
  scale_x_datetime(date_labels = "%m/%d/%y")+
  theme_bw()+
  labs( x = "Date", y = "Current Speed (cm/s)")+
  theme(legend.key.size = unit(0.2, "line"),
        legend.position = c(0.4, 0.9),
        axis.title.y = element_text(color = "black"),
        axis.text.y.left = element_text(color = "black"),
        plot.title = element_text(color="Black", size = 16, face="bold.italic"))

Plot

#### if you want to take the x,y, and z axis and turn it into a "motion index" you do this:
## acceleration
str(data)
data$accelX = as.numeric(data$accelX)
data$accelY = as.numeric(data$accelY)
data$accelZ = as.numeric(data$accelZ)
data = data %>% 
  mutate(
    diffX = accelX - lag(accelX),
    diffY = accelY - lag(accelY),
    diffZ = accelZ - lag(accelZ),
    motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
str(data)

#acceleration plot 
a= ggplot()+
  geom_line(aes(x = datetime, y = motionIndex, color = "Motion"), data = data)+
  theme_bw()+
  labs( x = "Time", y = "Motion")+
  scale_x_datetime(labels = date_format("%m/%d %H:%M"))+
  ggtitle("TITLE") +
  theme(legend.key.size = unit(1.5, "line"),
        legend.position = "right", 
        legend.title = element_blank(),
        legend.justification = "center",
        axis.title = element_text(color = "black"))
a     





