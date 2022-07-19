# RK 7.13.2022

# Packages
library(ggplot2)
library(ggsci)
library(ggpubr)
library(gridExtra)
library(dplyr)
library(lubridate)
library(plotrix)
library(viridis)
library(mgcv)
library(visreg)
library(sm)
library(Hmisc)
library(plyr)

# Clears the all the saved data to start a blank workplace
rm(list = ls())

# Theme for creating extra classy gg plots
# mytheme <- theme(panel.grid.major = element_blank(),
#                  panel.grid.minor = element_blank(),
#                  panel.background = element_blank(), 
#                  legend.text = element_text(size = 9),
#                  #legend.title = element_blank(),
#                  #legend.position = "none",
#                  legend.key = element_rect(fill = "white"),
#                  legend.spacing = unit(0.0, "line"),
#                  #text = element_text(size = 10, color = "black"),
#                  axis.text.y = element_text(color = "black", size = 8),
#                  axis.text.x = element_text(color = "black", size = 8),
#                  panel.border = element_rect(size = 0.5, fill = NA),
#                  plot.margin=unit(c(3,3,3,3),"mm"),
#                  strip.background = element_rect(fill="white"))

###############################################################################
######################### Read in Data  ########################################

dateFix = function(df) {
  df$Trial_Date = as.Date(df$Trial, "%m/%d/%y")
  return(df)
}

# Reading Data for Algae Chla extracted values data sheet

chlaDatasheet = read.csv("chlA.csv")

ChlaDatasheet = dateFix(chlaDatasheet)

#ChlaDatasheet = select(ChlaDatasheet,-c(21,22,23,24,25))
ChlFs = 0.000493
FoFa_max = 1.7039

#Calculating Chla ug/L and Phaeo ug/L from Raw Data
ChlaDatasheet = ChlaDatasheet %>%
  mutate(Ave_Chl1 = (ChlFs*(FoFa_max/(FoFa_max-1))* 
                       (ChlaDatasheet$Fo-ChlaDatasheet$Fa)*
                       (((ChlaDatasheet$Acetone_vol)/ChlaDatasheet$Vol_Filtered))))

ChlaDatasheet = ChlaDatasheet %>%
  mutate(ChlaDatasheet, Ave_Phaeo1 = ((ChlFs*(FoFa_max/(FoFa_max-1)))*
                                        ((FoFa_max-1)*(ChlaDatasheet$Fo-ChlaDatasheet$Fa))*
                                        (((ChlaDatasheet$Acetone_vol)/ChlaDatasheet$Vol_Filtered))))

#Function to calculate mean and standard error
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      SE = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)
}

#Create summary data frame
df2<-data_summary(ChlaDatasheet, "Ave_Chl1", 
                  groupnames=c("Trial_Date", "Location"))

chlA_graph<-ggplot(df2, aes(x=Trial_Date, y=mean, group=Location, color=Location)) + 
  geom_line() +
  geom_point()+theme_classic()+geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                position=position_dodge(0.05))+scale_y_continuous(limits=c(0,10))+ylab("Chlorophyll A (ug/L)")+xlab("")
chlA_graph







###############################################################################
######################### Turbidity  ########################################
###############################################################################

turbidity<-read.csv("turbidity.csv")

#Create summary data frame
df3<-data_summary(turbidity, "Turbidity", 
                  groupnames=c("Date", "Location"))

df3$Date <- mdy(df3$Date)

#Plot
turbidity_graph<-ggplot(df3, aes(x=Date, y=mean, group=Location, color=Location)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                position=position_dodge(0.05))+theme_classic()+scale_y_continuous(limits=c(0,4))+ylab("Turbidity (NTU)")+xlab("")

###############################################################################
######################### Combined graphs  ########################################
###############################################################################

grid.arrange(turbidity_graph,chlA_graph, ncol=2)

