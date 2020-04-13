#################>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> REGRESSION <<<<<<<<<<<<<<<<<<< #################################
#################>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> REGRESSION <<<<<<<<<<<<<<<<<<< #################################
#################>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> REGRESSION <<<<<<<<<<<<<<<<<<< #################################
#################>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> REGRESSION <<<<<<<<<<<<<<<<<<< #################################
#################>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> REGRESSION <<<<<<<<<<<<<<<<<<< #################################
#################>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> REGRESSION <<<<<<<<<<<<<<<<<<< #################################


#####Research Question: Does a person's height predict his/her weight loss?

#####Procedure:

#Step 1: Conduct Univariate Analyses: Descriptive Statistics and Cleaning the Data

#Step 2: Conduct Bivariate Analyses

#Step 3: Conduct Simple OLS Regression Analyses

#Step 4: Assess the Model Assumptions

#Step 5: Examine Model Fit Diagnostics

#Step 6: Examine Data for Influential Observations

#Step 7: State a Conclusion

lab = read.csv("wt_loss_data.csv",sep=",") 


####Step 1:  Univariate Analyses: Descriptive Statistics and Cleaning the Data: 
##### See Correlstion Example

#### Step 2:  Conduct Bivariate Analyses
##### See Correlation Example

#### Step 3: Conduct Simple OLS Regression Analyses

regression = lm(lab$Loss ~ lab$Height)


#### Setp 4: Assess the Model Assumptions
##### 1) Plot of the standardized residuals against the standardized predicted outcome variable 

plot(regression$fitted.values,regression$residuals )








### Residual by Predicted: Equal amount above and below line. No outliers that would impact the slope of line. 
### Potential outliers on right side may impact the slope. Outlier at bottom may impact the coefficient. 
### Linearity, Equal Variance, and Error Normality are met.

##### 2) Plot the standardized residuals against X

plot(lab$Height,regression$residuals, main = "Plot of studentized residuals \n  and predictor (height)to assess homogenity  \n of variance and linearity")








#Height by Loss: The dots are nicely scattered around. No Issues.

##### 3) Normal Quantile Plot of the standardized residuals

qqnorm(regression$residuals)
qqline(regression$residuals)





#Normality on Residuals: The residuals are normally distributed. No issues. The normality assumption is met.


#### Or you can just try statement "plot (regression)". It will give you all diagnostic figures defaulted by R.

plot (regression)



histogramRESIDUALS=hist(regression$residuals, main="A histogram of the regression residuals")






##### >>>>>>>>>>>>>> CONFIDENCE INTERVALS ON THE REGRESSION <<<<<<<<<<<<<<<<<<
confint(regression)



#### Setp 5:  Examine Model Fit Diagnostics

summary(regression)







##### 1st Step: Analysis of Variance Table: The Global F-Test of 7.18 is statistically significant at p=0.0082. If this was not significant, we would stop. Since it is significant, let's look at the other model fit diagnostics.

Reduced=lm(lab$Loss ~ lab$Height) #fit reduced model
Full=lm(lab$Loss~0+as.factor(lab$Height)) #fit full model
anova(Reduced, Full) #get lack-of-fit test


##### Lack of Fit: The null hypothesis is the that model has good fit, so we do not want to reject this one. The p-value of 0.3348 indicates that we do not reject the null hypothesis and conclude that we have good fit.

##### Summary of Fit Table: R-Squared is 0.0462. The independent variable, height, explains 4.62% of the variance in the dependent variable, weight loss. Ignore the Parameter Estimates for now.


### Step 6: Examine Data for Influential Observations

#### We will, again, use the results we output from statement "lm()".

#### We created a new dataset called "resid" which saves the necessary variables for us to examine to determine if any outliers are present in the data.

#### Look for potential influential observations: ##### Studentized Residuals: ??? 2 and ??? -2
##### Hat Diagonals: >2p/n or > (2)(2)/150 =0.0267: n=150; p=# of parameters including intercept.
##### Cook's D: > 4/n or > 4/150 = 0.0267; n= 150

library(MASS)
Studentized_Residuals=studres(regression)

Hat_Diagonals=lm.influence(regression)$hat
Cooks_d=cooks.distance(regression)

resid=as.data.frame(cbind(Studentized_Residuals,Hat_Diagonals,Cooks_d))
colnames(resid)=c("Studentized_Residuals","Hat_Diagonals","Cooks_D")

whole=cbind(lab,resid)


#### Studentized Residuals:

filter.studentized.residuals=whole[whole$Studentized_Residuals>=2|whole$Studentized_Residuals<=-2,]

filter.studentized.residuals


#We have 5 subjects who meet the criterion that the residuals are greater than and equal to 2, and 2 subjects who meet the criterion that the residuals are less than and equal to -2. The -3.70124 value is associated with a subject who lost 8.57 lbs.

#### Hat values > 0.0267

filter.hat=whole[whole$Hat_Diagonals>0.0267,]
filter.hat


#Our suspicious outlier (ID=97) whose Loss=8.57, is not greater than the cutpoint. Its value is 0.009.


#### Cook's D values > 0.0267

filter.cooks.d=whole[whole$Cooks_D>0.0267,]
filter.cooks.d


#Cook's D values > 0.0267 should be considered. Our spicious outlier 8.57 is greater than the cutpoint and may potentially impact the regression coefficient. Our suspicious outlier 8.6 is the 0.057965 value. We have one other subject that is greater and has about three times the effect (0.1739389681). Looking in the data set, this individual has weight loss of 14.2 and a height of 51 inches. This individual is also the Hat Diagonal of 0.1000403438.

#We should run the regression without each of these two data points to see if there is a difference in the model fit and the results below. For our purposes, let's assume no difference.

### Step 7: State a Conclusion

summary(regression)


#### The significant overall model (F(1,148)=7.18; p=.008) suggests that at least one slope in the model is significant. Because a height of zero does not make practical sense, the intercept should not be interpreted. If the intercept were to be interpreted, it would be "When height is zero, the mean weight loss was 23.31 pounds". For the variable, height, we interpret "as height increases by one inch, the associated mean decrease in weight loss is 0.11 pounds".


