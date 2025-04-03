library(dplyr)
library(ggplot2)
library(car)
library(MASS)
library(mice)
library(ResourceSelection)
library(table1)
library(knitr)
library(gt)


###READING IN DATA###
MasterData<-read.csv("MasterData.csv")

length(MasterData$SEQN) #there are 11933 subjects
table(MasterData$Gender) #there are 6358 females and 5575 males
table1(~Age+Country_of_Birth|Gender,data=MasterData)->sTABLE
sTABLE
###CHI-SQUARE TEST###

#note high-sample sizes can cause significiant p-values

##Identify Categorical Variables (and any variables)
#sapply(MasterData,class)

#Gender,Race,Active Duty,Country of Birth
#Gender is significant
#x<-table(MasterData$High_Chol,MasterData$Gender)
#chisq.test(x)

#Race is significant
#x<-table(MasterData$High_Chol,MasterData$Race)
#chisq.test(x)

#Active Duty is not significant
#x<-table(MasterData$High_Chol[MasterData$Active_Duty!="Refused"],MasterData$Active_Duty[MasterData$Active_Duty!="Refused"])
#chisq.test(x)

#Country of Birth is significant
#x<-table(MasterData$High_Chol,MasterData$Country_of_Birth)
#chisq.test(x)

#All categorical variables selected seem to be significant. We'll include them in the univariate analysis.

###Univariate Analysis of Logistic Regression###

predictors<-c("Gender","Race","Active_Duty","Country_of_Birth","Weightkg","StandingHeightcm","BMI","Age","People_in_Household",
              "DirectHDLCholesterol_mgdL",
              "HD2HD3","HD2","HD3","epiHD3","WhiteBloodCellCount","LymphocytePercent","MonocytePercent",
              "Segmented_neutrophilsPercent","EosinophilsPercent","BasophilsPercent","RedBloodCellCount",
              "PlateletCount","Hemoglobin")

yvar<-"High_Chol"

results<-list()

#This bit of code loops over the predictors for univariate Logistic Regression analysis. The results are stored in
#an empty list one by one.
for (predictor in predictors){
  x<-as.formula(paste(yvar,"~",predictor))
  model<-glm(x,data=MasterData,family=binomial)
  results[[predictor]]<-summary(model)
}

kable(p_values_df, col.names = c("Predictor", "P-value"), digits = 4)

# Display the table in the Viewer
browsable(table_output)
table_output
library(htmltools)

p_values <- sapply(predictors, function(predictor) {
  x <- as.formula(paste(yvar, "~", predictor))
  model <- glm(x, data = MasterData, family = binomial)
  summary(model)$coefficients[2, 4]  # Extract p-value for predictor
})

# Convert to a data frame for better readability
p_values_df <- data.frame(Predictor = predictors, P_Value = p_values)

# Print results
print(p_values_df)

results[1]$Gender$coefficients
#Weightkg YES
results[1]$Weightkg$coefficients
#StandingHeightcm YES
results[2]
#BMI YES
results[3]
#Age YES
results[4]
#People in Household YES
results[5]
#DirectHDLCholesterol_mgDL YES
results[6]
#HD2HD3 YES
results[7]
#HD2 YES
results[8]
#HD3 YES
results[9]
#epiHD3 YES
results[10]
#White Blood Cell Count #Borderline
results[11]
#LymphocytePercent NO
results[12]
#Monocyte Percent YES
results[13]
#Segmented_neutrophilsPercent YES
results[14]
#EosinophilsPercent YES
results[15]
#BasophilsPercent YES
results[16]
#RedBloodCellCount YES
results[17]
#Platelet Count #Borderline
results[18]
#Hemogoblin YES
results[19]

###Checking for Multicollinearity###
full_model<-glm(High_Chol ~ Weightkg+StandingHeightcm+
                    BMI+Gender+Age+Race+ 
                    Country_of_Birth+People_in_Household+
                    DirectHDLCholesterol_mgdL+
                    HD2HD3+HD2+HD3+epiHD3+WhiteBloodCellCount+
                    MonocytePercent+
                    Segmented_neutrophilsPercent+EosinophilsPercent+
                    BasophilsPercent+RedBloodCellCount+
                    PlateletCount+Hemoglobin,data=MasterData,family=binomial)
vif(full_model)
full_model

#Remove HD3. It has the highest VIF.
model <- glm(High_Chol ~ Weightkg + StandingHeightcm +
                    BMI + Gender + Age + Race+ 
                    Country_of_Birth + People_in_Household +
                    DirectHDLCholesterol_mgdL+HD2HD3+HD2+epiHD3+WhiteBloodCellCount+
                    MonocytePercent+
                    Segmented_neutrophilsPercent+EosinophilsPercent+
                    BasophilsPercent+RedBloodCellCount+
                    PlateletCount+Hemoglobin, data=MasterData, family = binomial)
vif(model)
model

#Remove Weightkg. It has the highest VIF.
model <- glm(High_Chol ~ StandingHeightcm +
               BMI + Gender + Age + Race+ 
               Country_of_Birth + People_in_Household +
               DirectHDLCholesterol_mgdL+HD2HD3+HD2+epiHD3+WhiteBloodCellCount+
               MonocytePercent+
               Segmented_neutrophilsPercent+EosinophilsPercent+
               BasophilsPercent+RedBloodCellCount+
               PlateletCount+Hemoglobin, data=MasterData, family = binomial)
vif(model)
model

###Independence will be assumed###

#table(MasterData$SEQN) %>% sort(descending=T) %>% head()
#filter(MasterData,SEQN==130378)

###Outliers and Influential Points

#Calculate Cook's Distance and Leverage
cooks_dist<-cooks.distance(model)
leverage<-hatvalues(model)

#Combine results into a data frame
influence_table<-data.frame(
  Observation=1:length(cooks_dist), 
  Cook_Distance=cooks_dist,         
  Leverage=leverage                 
)

#Labeled it decreasing order.
influence_table<-influence_table[order(-influence_table$Cook_Distance), ]

#plot(x=influence_table$Cook_Distance, y=influence_table$Leverage)
#plot(influence_table$Cook_Distance)

influence_table
head(influence_table,10)

###Testing Linearity###
##Box-Tidwell Test

#this loops creates and interaction term for every continuous variable in the list above, as
#well as a logit. A model is fit with every model with the interaction term and a p-value is 
#extracted for each interaction term. The below method is a bit crude, but the p-value for
#each interaction term is pasted into a table. bt is a table of the p-values for the interaction terms.

bt_results <- data.frame(Variable=character(),P_Value=numeric(),stringsAsFactors=FALSE)

continuous_vars <- c("StandingHeightcm", "BMI", "Age", 
                     "People_in_Household", "DirectHDLCholesterol_mgdL", "HD2HD3",
                     "HD2", "epiHD3", "WhiteBloodCellCount", 
                     "MonocytePercent", "Segmented_neutrophilsPercent",
                     "EosinophilsPercent", "BasophilsPercent", 
                     "RedBloodCellCount", "PlateletCount", "Hemoglobin")

for (var in continuous_vars) {
  MasterData[[paste0("log_", var)]] <- log(MasterData[[var]] + 1)
  MasterData[[paste0("interaction_", var)]] <- MasterData[[var]] * MasterData[[paste0("log_", var)]]
  formula<-as.formula(paste("High_Chol ~", var, "+ log_", var, "+ interaction_", var, 
                              "+ Gender + Age + Race + Country_of_Birth + People_in_Household + 
                                DirectHDLCholesterol_mgdL + HD2 + HD3 + epiHD3 + 
                                WhiteBloodCellCount + LymphocytePercent + 
                                MonocytePercent + EosinophilsPercent + 
                                BasophilsPercent + RedBloodCellCount + 
                                PlateletCount + Hemoglobin", sep = ""))
  bt_model<-glm(formula,data=MasterData,family=binomial)
  p_value<-summary(bt_model)$coefficients[paste0("interaction_", var), "Pr(>|z|)"]
  bt_results<-rbind(bt_results, data.frame(Variable=var,P_Value=p_value))
}

print(bt_results)

#Age,DirectHDLCholesterol_mgdL,HD2,epiHD3,EosinophilsPercent,BasophilsPercent,PlateletCount
#are significant at an alpha of 0.05. StandingHeightcm,HD2HD3,Segmented_neutrophilsPercent are borderline.
#BMI,People_in_Household,WhiteBloodCellCount,MonocytePercent,RedBloodCellCount and Hemoglobin are not significant.

#MasterData$log_BMI<-log(MasterData$BMI + 1)
MasterData$log_Age<-log(MasterData$Age + 1)
MasterData$log_DirectHDLCholesterol_mgdL<-log(MasterData$DirectHDLCholesterol_mgdL + 1)
MasterData$log_HD2<-log(MasterData$HD2+1)
MasterData$log_epiHD3<-log(MasterData$epiHD3+1)
#MasterData$log_MonocytePercent<-log(MasterData$MonocytePercent + 1)
MasterData$log_EosinophilsPercent<-log(MasterData$EosinophilsPercent + 1)
MasterData$log_BasophilsPercent<-log(MasterData$BasophilsPercent + 1)
MasterData$log_PlateletCount<-log(MasterData$PlateletCount + 1)
MasterData$log_StandingHeightcm<-log(MasterData$StandingHeightcm + 1)
MasterData$log_HD2HD3<-log(MasterData$HD2HD3 + 1)
MasterData$log_Segmented_neutrophilsPercent<-log(MasterData$Segmented_neutrophilsPercent+1)

model_transformed <- glm(High_Chol ~ log_StandingHeightcm+BMI+Gender+log_Age
                         +Race+Country_of_Birth+People_in_Household+log_DirectHDLCholesterol_mgdL
                         +log_HD2HD3+log_HD2+log_epiHD3+WhiteBloodCellCount+MonocytePercent+
                           log_Segmented_neutrophilsPercent+
                           log_EosinophilsPercent + log_BasophilsPercent + RedBloodCellCount+
                           log_PlateletCount + Hemoglobin
                           , data = MasterData, family = binomial)

summary(model_transformed)

#rechecking vif
vif(model_transformed)

#We will continue with this model
model_transformed <- glm(High_Chol ~ log_StandingHeightcm+BMI+Gender+log_Age
                         +Race+Country_of_Birth+People_in_Household+log_DirectHDLCholesterol_mgdL
                         +log_HD2HD3+log_HD2+log_epiHD3+WhiteBloodCellCount+MonocytePercent+
                           log_Segmented_neutrophilsPercent+
                           log_EosinophilsPercent + log_BasophilsPercent + RedBloodCellCount+
                           log_PlateletCount + Hemoglobin
                         , data = MasterData, family = binomial)


###Stepwise Function to get an idea of what the final model could look like###
stepwise_model<-step(model_transformed, direction = "both", trace = TRUE)
summary(stepwise_model)


###Hosmer Lemeshow Test###
model_data<-model.frame(stepwise_model)
predicted_probs <- predict(stepwise_model, type = "response")
y_obs <- model_data$High_Chol
hl_test <- hoslem.test(y_obs, predicted_probs, g = 10)
hl_test

stepwise_model
