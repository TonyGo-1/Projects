#Load in data. This is the same masterdata Set used in the Logistic Regression Analysis project. This can also be found in my github page.
MasterData<-read.csv("MasterData.csv")

summary(MasterData)

install.packages("car")
library(car)
library(tidyverse)

###Chapter 1: T-tests & ANOVA###

##Independent Samples t-test: Tests the difference between two independent means. Dependet variable has to be interval
#or ratio. If the calculated t-value is greater than the critical t-value, we reject the null hypothesis. Null states
#the population means are equal.

#Example: Do people with High Cholesterol have a significant difference in means for Total Cholesterol? We'll use
#a Welch's t-test, which is similar to an independent samples t-test. Unlike a standard independent t-test, the former 
#does not assume equal variance. The levene test shows that the variances are significantly different. A welch t-test
#would be appropriate.

MasterData$High_Chol<-as.factor(MasterData$High_Chol)
leveneTest(TotalCholesterol_mgdl~High_Chol,MasterData)

#We see based on the p-value there is a highly significant difference in Total Cholesterol between High Cholesterol
#individuals and Low Cholesterol individuals, which should be expected. 

t.test(TotalCholesterol_mgdl ~ High_Chol, MasterData)
leveneTest(TotalCholesterol_mgdl~factor(High_Chol),MasterData)

##Work in Progress

###Chapter 2: Visualizing Data ###

##Boxplots
#This boxplot is comparing High Cholectrol Groups and Total Cholesterol groups
MasterData %>%
  ggplot(aes(x=High_Chol,y=TotalCholesterol_mgdl,fill=High_Chol))+
  geom_boxplot()+
  scale_fill_manual(values=c("purple","green"),
                    labels=c("Low","High"))+
  labs(x="High Cholesterol Group",
       y="Total Cholesterol (mg/dL)",
       title="Cholesterol Levels by High Cholesterol Status",
       fill="Group")+theme_minimal()




###Chapter 3: Regression Analysis Assumptions ###
#There are 4 assumptions that need to be met before conducting Linear Regression. 
#LINE (Linearity, Independence, Normality and Equal Variance). Equal Variance can also be called homoscedasticity.

##Normality
#To test normality (in linear regression), we need to 

#A Linear Regression is fit predicting total cholesterol from two Variables: Age & BMI.
model<-lm(TotalCholesterol_mgdl ~ Age + BMI, data = MasterData)

#plotting residuals against 
residual_data<-data.frame(resid=residuals(model))
qq<-ggplot(residual_data, aes(sample = resid)) +
  stat_qq()+
  stat_qq_line(color = "blue") +
  theme_minimal() +
  labs(title="Q-Q Plot: Regression Residuals")

qq

#It seems our qqplot shows heavy skewness.

###Chapter 4: Handling Missing Data ###

