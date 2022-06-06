---
title: "OysterWorkspace"
author: "Ruby K"
date: '2022-06-06'
output:
  html_document: default
  pdf_document: default
---
## Initial Analysis

```{r, initialHeights, include=FALSE}
initialHeights1<-c(63,32,48,61,71,41,58,69,33,34,70,61,61,51,39,45,61,60,61,67,60,55,40,69,55,29,60,46,46,53,75,42,21,35,72,61,48,46,47,73,55,31,43,37,52,56,52,40,63,48,50,19,59,59,47,63,41,41,26,35,48,64,44,60,40,49,47,35,55,45,60,42,42,46,49,56,48,45,40,37,60,59,37,28,51,61,45,35,26,54,41,44,58,39,41,47,34,47,50,52,55,43,44,45,41,26,50,38,41,58,42,37,36,48,28,24,32,29,45,70,64,67,24,35,63,56,41,37,47,55,38,75,47,43,40,39,65,50,42,42,40,60,28,36,60,64,48,48,50,49,33,34,40,43,59,46,30,38,58,43,45,35,34,37,48,58,58,42,46,57,44,24,40,40,41,37,59,57,68,40,45,49,50,35,35,30,36,30,32,70,34,41,54,29,43,52,32,37,38,70,42,32,56,37,35,58,50,38,42,43,55,60,43,68,46,60,53,55,59,55,32,50,41,64,37,59,57,52,55,50,38,60,61,37,60,62,55,45,63,47,35,62,61,65,36,62,59,42,39,49,35,50,49,46,43,45,34,44,44,30,41,49,45,41,42,48,32,80,69,69,50,46,57,45,59,63,56,51,45,51)
```

```{r, initialHeights-analysis}
oysterAvgInitial<-mean(initialHeights1)
oysterAvgInitial
initialHeightSD<-sd(initialHeights1)
initialHeightSD
shapiro.test(initialHeights1)
```

The initial shell heights are normally distributed (Shapiro-Wilk test, W=0.988, P=0.024).

```{r, initialHeights-analysis2}
upperLimit<-oysterAvgInitial+2*initialHeightSD
upperLimit
lowerLimit<-oysterAvgInitial-2*initialHeightSD
lowerLimit
```

```{r include=FALSE}
initialHeights<-data.frame(initialHeights1)
```

The mean is `r oysterAvgInitial`mm and the standard deviation is `r initialHeightSD` mm. The acceptable range of oyster shell heights for the experimental bags will be from `r upperLimit` mm to `r lowerLimit` mm.

```{r warning=FALSE, initialShellHeightDist, fig.width=4, fig.height=3, fig.align='center'}
library(ggplot2)
ggplot(initialHeights,aes(x=initialHeights1))+geom_histogram(binwidth = 5, fill="#69b3a2", color="#e9ecef")+theme_classic()+xlab("Shell height")+ylab("Frequency")
```


## Random Number Generation for Sampling

```{r random-table}
library(knitr)
randTable<-data.frame()
for (i in 1:6) {
  newRow<-sample(1:3, 30, replace=TRUE)
  randTable<-rbind(randTable,newRow)
}
colnames(randTable)<-c(1:30)
rownames(randTable)<-c("control_in", "control_out", "FB_in", "FB_out", "OG_in", "OG_out")

Ones<-c()
Twos<-c()
Threes<-c()

for (i in 1:6) {
  sumOne<-0
  sumTwo<-0
  sumThree<-0
  for (j in 1:30) {
    ifelse(randTable[i,j]==1,sumOne<-sumOne+1, ifelse(randTable[i,j]==2, sumTwo<-sumTwo+1, sumThree<-sumThree+1) )
  }
  Ones[i]=sumOne
  Twos[i]=sumTwo
  Threes[i]=sumThree
}
randTable<-cbind(randTable, Ones)
randTable<-cbind(randTable, Twos)
randTable<-cbind(randTable, Threes)

kable(randTable)
```
This code segment can be run each day that the shell dimensions will be sampled, generating a new table of random numbers each time. When sampling for biofouling or condition index, the "j" loop can be altered to run 15 times instead of 30.