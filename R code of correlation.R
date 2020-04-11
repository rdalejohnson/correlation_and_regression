### Research Question: Is a person's age, height, and start weight associated with weight loss?

#####Procedure:

#Step 1:  Conduct Univariate Analyses

#Step 2:  Check the Assumptions

#Step 3:  Run the Statistical Test

#Step 4:  State a Conclusion



###Step 1:  Univariate Analyses: Descriptive Statistics and Cleaning the Data

##### For continuous variables, we will use "summary" to obtain summary statistics and use "hist" to get a 
##### Plot the PP plot to check normality. Use the "qqnorm" function.

##### Age

lab = read.csv("wt_loss_data.csv",sep=",") 

summary(lab$Age)
sd(lab$Age)
hitsgram=hist(lab$Age, main="a histogram of the Age")


##### Height

summary(lab$Height)
sd(lab$Height)
hitsgram=hist(lab$Height, main="a histogram of the Height")


#### Based on the outputs, Age ranges from 21 to 79 with a mean of 50.3. Height ranges from 51 to 78 with a mean of 65.64. We have some outliers on both sides of this distribution. Minimum & Maximum values located under Simple Statistics in the Correlation Analyses.

##### Start weight

summary(lab$Start_Weight)
sd(lab$Start_Weight)
hitsgram=hist(lab$Start_Weight, main="a histogram of the start weight")



##### Loss

summary(lab$Loss)
sd(lab$Loss)
hitsgram=hist(lab$Loss, main="a histogram of the Loss")


#### Start Weight ranges from 90 to 366 with a mean of 182.73. 
#### This distribution is positively skewed (median is 173.5). Loss ranges from 8.57 to 20.41 with a mean of 15.94 pounds. Possibly one problem outlier (8.57). No missing data.



### Step 2:  Check Assumptions
#### 1) Normal Distribution of X and Y -- qqnorm
#### 2)  Linear Relationship Between X and Y -- scatterplot or you may use R package "Hmisc" and "PerformanceAnalytics"

#### Check normality
#### Age

qqnorm(lab$Age)
qqline(lab$Age)


#### Height

qqnorm(lab$Height)
qqline(lab$Height)


#### Start weight

qqnorm(lab$Start_Weight)
qqline(lab$Start_Weight)



#### Loss

qqnorm(lab$Loss)
qqline(lab$Loss)


### Check Linear Relationship Between X and Y

#### Scatterplot:  plot(X,Y) for scatterplot of variables X and Y with X on the x-axis and Y on the y-axis.  

#### Plot 
#### 1) Age with Loss; 
#### 2) Height with Loss; 
#### 3) Start Weight with Loss.  


##### Scatter plot of Age with Loss


plot(lab$Age,lab$Loss, main = "Scatter plot of Age with Loss")

##### Data is pretty scattered with no obvious direction so 
#### probably not a significant relationship. No weird data points.

##### Scatter plot of Height with Loss


plot(lab$Height,lab$Loss, main = "Scatter plot of Height with Loss")

##### Has a negative slope. Two data points (left side of graph) might attenuate the slope of the line.
##### May need to remove??


##### Scatter plot of Start Weight with Loss

plot(lab$Start_Weight,lab$Loss, main = "Scatter plot of Start Weight with Loss")


#### Slight negative slope. A couple of weird data points, 
#### but they balance each other out so should not influence the correlation.

### Step 3:	Run Statistical Test: Correlation
#### The three primary correlations:  
#### 1) Age with Loss; 
#### 2) Height with Loss;
#### 3) Start Weight with Loss.

#### 1) Age with Loss

cor.test(lab$Age,lab$Loss)

#Age with Loss:  r=0.064; p=0.4358
#Not Statistically Significant.

#### 2) Height with Loss

cor.test(lab$Height,lab$Loss)

#Height with Loss:  r= -0.215; p=0.008
#Statistically Significant.

#### 3) Start Weight with Loss.

cor.test(lab$Start_Weight,lab$Loss)

#Start Weight with Loss:  r= -0.113; p=0.1693
#Not Statistically Significant.


library(Hmisc)
table=rcorr(as.matrix(lab[,c(2,4,5,7)]))
print(table)

library(PerformanceAnalytics)
chart.Correlation(lab[,c(2,4,5,7)], histogram=TRUE, pch=19)


#### The correlation matrix will always have a 1.0000 running along the diagonal because age (first row) correlated with age (first column) will be 1.000. Height (second row) correlated with height (second column) will be 1.000, etc. Because the lower part of the correlation matrix is identically to the top part, you only need to interpret one. I prefer the lower part. The correlation of Age (first column) with Height (second row) is -0.10699. That crazy box X should be a six. The p-value associated with this relationship is 0.1925. Thus, Age and Height are not statistically correlated.

#### Step 4: State a conclusion
#### The research question was whether a person's age, height, and start weight 
#### were associated with weight loss. 
#### Based on the correlation, only the subject's height is statistically correlated 
#### with weight loss (r= -0.2151; p=0.0082); 
#### however, this relationship is weak. In particular, as subjects get taller, 
#### weight loss is smaller OR Taller subjects are associated with less weight loss. 
#### There were two data points that attenuated this relationship. 
#### The next step would be to remove these two data points to 
#### see their impact on the strength of the relationship.
