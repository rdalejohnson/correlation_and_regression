
library(plyr)
library(tidyverse)

############READING, CLEANING, AND REVIEW OF DATA SET#####################
############READING, CLEANING, AND REVIEW OF DATA SET#####################
############READING, CLEANING, AND REVIEW OF DATA SET#####################

cancerData =read.csv("Cancer.csv",sep=",") 

summary(cancerData$PROTEIN)

plyr::count(cancerData, 'SUBJNO')
#plyr::count(cancerData, 'PROTEIN')


############################################# THE IDEAL DATA VALUES #######################################
############################################# THE IDEAL DATA VALUES #######################################
############################################# THE IDEAL DATA VALUES #######################################

##DESCRIPTIVE STATS AND GRAPHS/CHARTS

plyr::count(cancerData, 'IDEAL')
summary(cancerData$IDEAL)

thirdQuantile <- quantile(cancerData$IDEAL)[4]
firstQuantile <- quantile(cancerData$IDEAL)[2]
interqurtRange <- IQR(cancerData$IDEAL)*1.5
outliersAbove <- thirdQuantile + interqurtRange
outliersBelow <- firstQuantile - interqurtRange

#Get a histogram, mean, standard deviation
histogramIDEAL=hist(cancerData$IDEAL, main="A histogram of the IDEAL values")
meanIDEAL <- mean(cancerData$IDEAL,na.rm=TRUE)
sdIDEAL <- sd(cancerData$IDEAL,na.rm=TRUE)
print (paste("IDEAL Mean: ", meanIDEAL))
print (paste("IDEAL St. Dev: ", sdIDEAL))

### BOX PLOT; note the outliers;
cancerIdealBoxPlot <-
  boxplot(cancerData$IDEAL,
          main = "Cancer Data IDEAL Boxplot",
          xlab = "x",
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE
          )

cancerIdealBoxPlot


qqnorm(cancerData$IDEAL)
qqline(cancerData$IDEAL)
ks.test(x=cancerData$IDEAL,"pnorm",mean=meanIDEAL,sd=sdIDEAL)
shapiro.test(cancerData$IDEAL)