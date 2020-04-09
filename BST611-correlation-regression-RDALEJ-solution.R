
library(plyr)
library(tidyverse)

############CLEANING AND REVIEW OF DATA SET#####################
############CLEANING AND REVIEW OF DATA SET#####################
############CLEANING AND REVIEW OF DATA SET#####################

cancerData =read.csv("Cancer.csv",sep=",") 

summary(cancerData$PROTEIN)

plyr::count(cancerData, 'SUBJNO')
plyr::count(cancerData, 'PROTEIN')


############################################# THE IDEAL VALUES #######################################
############################################# THE IDEAL VALUES #######################################
############################################# THE IDEAL VALUES #######################################

##DESCRIPTIVE STATS AND GRAPHS/CHARTS

summary(cancerData$IDEAL)
plyr::count(cancerData, 'IDEAL')

#Get a histogram, mean, standard deviation
histogramIDEAL=hist(cancerData$IDEAL, main="A histogram of the IDEAL values")
meanIDEAL <- mean(cancerData$IDEAL,na.rm=TRUE)
sdIDEAL <- sd(cancerData$IDEAL,na.rm=TRUE)
print (paste("IDEAL Mean: ", meanIDEAL))
print (paste("IDEAL St. Dev: ", sdIDEAL))




qqnorm(cancerData$IDEAL)
qqline(cancerData$IDEAL)
ks.test(x=cancerData$IDEAL,"pnorm",mean=meanIDEAL,sd=sdIDEAL)
shapiro.test(cancerData$IDEAL)