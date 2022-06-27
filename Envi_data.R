#Environmental Data
#packages: ggplot2, Hmisc, plotrix, plyr

library(ggplot2)
library(Hmisc)
library(plotrix)
library(plyr)

#Turbidity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
turbidity<-read.csv("turbidity.csv")

#Function to calculate mean and standard error
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      SE = std.error(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
#Create summary data frame
df2<-data_summary(turbidity, varname="Turbidity", 
                    groupnames=c("Date", "Location"))

#Convert date to factor variable
df2$Date=as.factor(df2$Date)
head(df2)

#Plot
ggplot(df2, aes(x=Date, y=Turbidity, group=Location, color=Location)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Turbidity-SE, ymax=Turbidity+SE), width=.2,
                position=position_dodge(0.05))+theme_classic()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~