#EXCELLENT correlation article: 
#   https://www.westga.edu/academics/research/vrc/assets/docs/scatterplots_and_correlation_notes.pdf
#Confidence intervals on lm
#   https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals

library(plyr)
library(tidyverse)
library(MASS)
library(Hmisc)
library(PerformanceAnalytics)




############READING, CLEANING, AND REVIEW OF DATA SET#####################
############READING, CLEANING, AND REVIEW OF DATA SET#####################
############READING, CLEANING, AND REVIEW OF DATA SET#####################

cancerData =read.csv("Cancer.csv",sep=",") 
nrow(cancerData)
nrow(na.omit(cancerData))

#summary(cancerData$PROTEIN)

#plyr::count(cancerData, 'SUBJNO')
#plyr::count(cancerData, 'PROTEIN')


############################################# THE IDEAL DATA VALUES #######################################
############################################# THE IDEAL DATA VALUES #######################################
############################################# THE IDEAL DATA VALUES #######################################

##DESCRIPTIVE STATS AND GRAPHS/CHARTS

#plyr::count(cancerData, 'IDEAL')
summary(cancerData$IDEAL)

thirdQuantile <- quantile(cancerData$IDEAL)[4]
firstQuantile <- quantile(cancerData$IDEAL)[2]
interqurtRange <- IQR(cancerData$IDEAL)*1.5
outliersAbove <- unname(thirdQuantile + interqurtRange)
outliersBelow <- unname(firstQuantile - interqurtRange)

outliersListIDEAL <-  cancerData$IDEAL[cancerData$IDEAL < outliersBelow | cancerData$IDEAL > outliersAbove] 

outliersListIDEAL

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



############################################# THE PROTEIN DATA VALUES #######################################
############################################# THE PROTEIN DATA VALUES #######################################
############################################# THE PROTEIN DATA VALUES #######################################

##DESCRIPTIVE STATS AND GRAPHS/CHARTS

#cancerData$PROTEIN[cancerData$PROTEIN > 6] <- NA   ############ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

cancerData <- cancerData[!(cancerData$PROTEIN > 6),]

#plyr::count(cancerData, 'PROTEIN')
summary(cancerData$PROTEIN)

thirdQuantile <- quantile(cancerData$PROTEIN)[4]
firstQuantile <- quantile(cancerData$PROTEIN)[2]
interqurtRange <- IQR(cancerData$PROTEIN)*1.5
outliersAbove <- unname(thirdQuantile + interqurtRange)
outliersBelow <- unname(firstQuantile - interqurtRange)

print (IQR(cancerData$PROTEIN))

outliersListPROTEIN <-  cancerData$PROTEIN[cancerData$PROTEIN < outliersBelow | cancerData$PROTEIN > outliersAbove] 

outliersListPROTEIN

#Get a histogram, mean, standard deviation
histogramPROTEIN=hist(cancerData$PROTEIN, main="A histogram of the PROTEIN values")
meanPROTEIN <- mean(cancerData$PROTEIN,na.rm=TRUE)
sdPROTEIN <- sd(cancerData$PROTEIN,na.rm=TRUE)
print (paste("PROTEIN Mean: ", meanPROTEIN))
print (paste("PROTEIN St. Dev: ", sdPROTEIN))

### BOX PLOT; note the outliers;
cancerPROTEINBoxPlot <-
  boxplot(cancerData$PROTEIN,
          main = "Cancer Data PROTEIN Boxplot",
          xlab = "x",
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE
  )

cancerPROTEINBoxPlot

qqnorm(cancerData$PROTEIN)
qqline(cancerData$PROTEIN)
ks.test(x=cancerData$PROTEIN,"pnorm",mean=meanPROTEIN,sd=sdPROTEIN)
shapiro.test(cancerData$PROTEIN)


#################### CORRELATION - RELATIONSHIPS BETWEEN PROTEIN and IDEAL #########################

#plot(cancerData$PROTEIN, cancerData$IDEAL, main = "Scatter plot of Protein with Ideal")

plot(cancerData$IDEAL, cancerData$PROTEIN, main = "Scatter plot of Protein with Ideal")

cor.test(cancerData$IDEAL,cancerData$PROTEIN)

table=rcorr(as.matrix(cancerData[,c(2,3)]))
print(table)

library(PerformanceAnalytics)
chart.Correlation(cancerData[,c(2,3)], histogram=TRUE, pch=19)


############################## REGRESSION ######################################
regression = lm(cancerData$PROTEIN ~ cancerData$IDEAL)

plot(regression$fitted.values,regression$residuals )





plot(cancerData$IDEAL,regression$residuals, main = "Plot of studentized residuals \n  and predictor (ideal body weight) to assess homogenity  \n of variance and linearity")



histogramRESIDUALS=hist(regression$residuals, main="A histogram of the regression residuals")



qqnorm(regression$residuals)
qqline(regression$residuals)





#Normality on Residuals: The residuals are normally distributed. No issues. The normality assumption is met.


#### Or you can just try statement "plot (regression)". It will give you all diagnostic figures defaulted by R.

plot (regression)









##### >>>>>>>>>>>>>> CONFIDENCE INTERVALS ON THE REGRESSION <<<<<<<<<<<<<<<<<<
confint(regression)



#### Setp 5:  Examine Model Fit Diagnostics

summary(regression)



