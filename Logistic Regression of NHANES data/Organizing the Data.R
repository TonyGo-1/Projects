install.packages("dplyr")
require(SASxport)
library(foreign)
library(dplyr)

###ORGANIZING DATASET PORTION

#SEQN is basically the ID number

###DEMOGRAPHICS
DEMO<-read.xport("DEMO_L.XPT")

DEMO<-DEMO %>% rename(Data_Release_Cycle=SDDSRVYR)
DEMO<-DEMO %>% mutate(Data_Release_Cycle=case_when(
  Data_Release_Cycle == 12 ~ "NHANES August 2021-August 2023 public Release"
))

DEMO<-DEMO %>% rename(Interview_and_Exam=RIDSTATR)
DEMO<-DEMO %>%
  mutate(Interview_and_Exam=case_when(
    Interview_and_Exam == 1 ~ "Interview Only",
    Interview_and_Exam == 2 ~ "Both Interview and MEC examined",
    is.na(Interview_and_Exam) ~ "NA"
  ))

DEMO<-DEMO%>%rename(Gender=RIAGENDR)
DEMO<-DEMO%>%
  mutate(Gender=case_when(
    Gender == 1 ~ "M",
    Gender == 2 ~ "F"))

#Something to keep in mind is that the 80 stands for 80 and above, not just 80 years old.
DEMO<-DEMO%>%rename(Age=RIDAGEYR)

DEMO<-DEMO%>%rename(Age_in_Months=RIDAGEMN)

#We'll delete the RIDRETH1 column because RIDRETH3 has more categories for RACE.
DEMO<-DEMO%>%select(-RIDRETH1)
  
DEMO<-DEMO %>% rename(Race=RIDRETH3)
DEMO<-DEMO %>%
  mutate(Race=case_when(
    Race == 1 ~ "Mexican American",
    Race == 2 ~ "Other Hispanic",
    Race == 3 ~ "Non-Hispanic White",
    Race == 4 ~ "Non-Hispanic Black",
    Race == 6 ~ "Non-Hispanic Asian",
    Race == 7 ~ "Other Race including Multi-racial"
))

#Six-Month Period when the Exam was performed. 
DEMO<-DEMO %>% rename(Exam_Period=RIDEXMON)
DEMO<-DEMO %>% mutate(Exam_Period=case_when(
  Exam_Period == 1 ~ "Nov 1 - Apr 30",
  Exam_Period == 2 ~ "May 1 - Oct 31",
  is.na(Exam_Period) ~ NA_character_
))

#Age in months of the participant at the time of the exam
DEMO<-DEMO %>% rename(Age_in_Months_EXAM=RIDEXAGM)

DEMO<-DEMO %>% rename(Active_Duty=DMQMILIZ)
DEMO<-DEMO %>% mutate(Active_Duty=case_when(
  Active_Duty == 1 ~ "Yes",
  Active_Duty == 2 ~ "No",
  Active_Duty == 7 ~ "Refused",
  Active_Duty == 9 ~ "Don't know",
  is.na(Active_Duty) ~ NA_character_
))

DEMO<-DEMO %>% rename(Country_of_Birth=DMDBORN4)
DEMO<-DEMO %>% mutate(Country_of_Birth=case_when(
  Country_of_Birth == 1 ~ "United States",
  Country_of_Birth == 2 ~ "Others",
  is.na(Country_of_Birth) ~ NA_character_
))

DEMO<-DEMO %>% rename(Length_of_time_in_US=DMDYRUSR)
DEMO<-DEMO %>% mutate(Length_of_time_in_US=case_when(
  Length_of_time_in_US == 1 ~ "<1 year",
  Length_of_time_in_US == 2 ~ "1-4 years",
  Length_of_time_in_US == 3 ~ "5-9 years",
  Length_of_time_in_US == 4 ~ "10-14 years",
  Length_of_time_in_US == 5 ~ "15-19 years",
  Length_of_time_in_US == 6 ~ "20 years or more",
  Length_of_time_in_US == 77 ~ "Refused",
  Length_of_time_in_US == 99 ~ "Don't Know",
  is.na(Length_of_time_in_US) ~ NA_character_
))

DEMO<-DEMO %>% rename(Education=DMDEDUC2)
DEMO<-DEMO %>% mutate(Education=case_when(
  Education == 1 ~ "<9th Grade",
  #includes 12th grade with no diploma
  Education == 2 ~ "9-11th Grade",
  Education == 3 ~ "High school Graduate/GED equivalent",
  Education == 4 ~ "Some College/AA degree",
  Education == 5 ~ "College Graduate or above",
  Education == 7 ~ "Refused",
  Education == 9 ~ "Don't know",
  is.na(Education) ~ NA_character_
))

DEMO<-DEMO %>% rename(Marital_Status=DMDMARTZ)
DEMO<-DEMO %>% mutate(Marital_Status=case_when(
  Marital_Status == 1 ~ "Married/Living with a Partner",
  Marital_Status == 2 ~ "Widowed/Divorced/Separated",
  Marital_Status == 3 ~ "Never Married",
  Marital_Status == 77 ~ "Refused",
  Marital_Status == 99 ~ "Don't Know",
  is.na(Marital_Status) ~ NA_character_
))

#Pregnancy status for females between 20-44 years of age at the time of MEC exam
DEMO<-DEMO %>% rename(Pregnancy_Status_at_Exam=RIDEXPRG)
DEMO<-DEMO %>% mutate(Pregnancy_Status_at_Exam=case_when(
  Pregnancy_Status_at_Exam == 1 ~ "Yes",
  Pregnancy_Status_at_Exam == 2 ~ "No",
  Pregnancy_Status_at_Exam == 3 ~ "Unsure",
  is.na(Pregnancy_Status_at_Exam) ~ NA_character_
))

#It should be kept in mind that 7 is the maximum. A value of 7 is counted as "7 or more".
DEMO<-DEMO %>% rename(People_in_Household=DMDHHSIZ)

#Household Reference person is the first person who owns or rents the residence where members of the
#household live.
DEMO<-DEMO %>% rename(HH_refperson_gender=DMDHRGND)
DEMO<-DEMO %>% mutate(HH_refperson_gender=case_when(
  HH_refperson_gender == 1 ~ "M",
  HH_refperson_gender == 2 ~ "F",
  is.na(HH_refperson_gender) ~ NA_character_
))

DEMO<-DEMO %>% rename(HH_refperson_age=DMDHRAGZ)
DEMO<-DEMO %>% mutate(HH_refperson_age=case_when(
  HH_refperson_age == 1 ~ "<20 years",
  HH_refperson_age == 2 ~ "20-39 years",
  HH_refperson_age == 3 ~ "40-59 years",
  HH_refperson_age == 4 ~ "60+ years",
  is.na(HH_refperson_age) ~ NA_character_
))

DEMO<-DEMO %>% rename(HH_refperson_EducationLvl=DMDHREDZ)
DEMO<-DEMO %>% mutate(HH_refperson_EducationLvl=case_when(
  HH_refperson_EducationLvl == 1 ~ "Less than High School Degree",
  HH_refperson_EducationLvl == 2 ~ "High School Grad/GED/Some College/AA degree",
  HH_refperson_EducationLvl == 3 ~ "College graduate or above",
  is.na(HH_refperson_EducationLvl) ~ NA_character_
))

DEMO<-DEMO %>% rename(HH_refperson_Marital_Status=DMDHRMAZ)
DEMO<-DEMO %>% mutate(HH_refperson_Marital_Status=case_when(
  HH_refperson_Marital_Status == 1 ~ "Married/Living with a Partner",
  HH_refperson_Marital_Status == 2 ~ "Widowed/Divorced/Separated",
  HH_refperson_Marital_Status == 3 ~ "Never Married",
  is.na(HH_refperson_Marital_Status) ~ NA_character_
))

#This is the HH ref person's spouse's education level
DEMO<-DEMO %>% rename(HH_spouse_educationLvl=DMDHSEDZ)
DEMO<-DEMO %>% mutate(HH_spouse_educationLvl=case_when(
  HH_spouse_educationLvl == 1 ~ "Less than high school degree",
  HH_spouse_educationLvl == 2 ~ "High School Grad/GED/Some College/AA Degree",
  HH_spouse_educationLvl == 3 ~ "College Graduate or Above",
  is.na(HH_spouse_educationLvl) ~ NA_character_
))

DEMO<-DEMO %>% rename(Full_Sample_2yearInt_Wght=WTINT2YR)

#A value of 0 stands for "Not MEC Examined"
DEMO<-DEMO %>% rename(Full_Sample_2yearMEC_Wght=WTMEC2YR)

DEMO<-DEMO %>% rename(Masked_Var_pseudostratum=SDMVSTRA)

DEMO<-DEMO %>% rename(Masked_Var_pesudoPSU=SDMVPSU)

#A value of 5 means 5 or above
DEMO<-DEMO %>% rename(Ratio_of_Family_Income_to_Poverty=INDFMPIR)

###BODY MEASURES
BM<-read.xport("BMX_L.XPT")

#BMC = Body Measures Component
BM<-BM %>% rename(BMC_Status_Code=BMDSTATS)
BM<-BM %>% mutate(BMC_Status_Code=case_when(
  BMC_Status_Code == 1 ~ "Complete data for age group",
  BMC_Status_Code == 2 ~ "Partial:Only Height and Weight",
  BMC_Status_Code == 3 ~ "Other Partial Exam",
  BMC_Status_Code == 4 ~ "No body measures"
))

BM<-BM %>% rename(Weightkg=BMXWT)

BM<-BM %>% rename(WeightComment=BMIWT)
BM<-BM %>% mutate(WeightComment=case_when(
  WeightComment == 1 ~ "Could not obtain",
  WeightComment == 2 ~ "Clothing",
  WeightComment == 3 ~ "Medical appliance",
))

#0 to 47 Months
BM<-BM %>% rename(RecumbentLengthcm=BMXRECUM)

#0 to 47 Months
BM<-BM %>% rename(RecumbentLengthComment=BMIRECUM)
BM<-BM %>% mutate(RecumbentLengthComment=case_when(
  RecumbentLengthComment == 1 ~ "Could not obtain",
  RecumbentLengthComment == 3 ~ "Not straight",
))

#0 to 6 months
BM<-BM %>% rename(HeadCircumference=BMXHEAD)

BM<-BM %>% rename(HeadCircumferenceComment=BMIHEAD)

BM<-BM %>% rename(StandingHeightcm=BMXHT)

BM<-BM %>% rename(StandingHeightComment=BMIHT)
BM<-BM %>% mutate(StandingHeightComment=case_when(
  StandingHeightComment == 1 ~ "Could not obtain",
  StandingHeightComment == 3 ~ "Not Straight"
))

#Body Mass Index(kg/m^2)
BM<-BM %>% rename(BMI=BMXBMI)

#2 years to 19 years
BM<-BM %>% rename(BMICategory_Children_Youth=BMDBMIC)
BM<-BM %>% mutate(BMICategory_Children_Youth=case_when(
  BMICategory_Children_Youth == 1 ~ "Underweight",
  BMICategory_Children_Youth == 2 ~ "Normal Weight",
  BMICategory_Children_Youth == 3 ~ "Overweight",
  BMICategory_Children_Youth == 4 ~ "Obese"
))

BM<-BM %>% rename(UpperLegLengthcm=BMXLEG)

BM<-BM %>% rename(UpperLegLengthComment=BMILEG)
BM<-BM %>% mutate(UpperLegLengthComment=case_when(
  UpperLegLengthComment == 1 ~ "Could not obtain"
))

BM<-BM %>% rename(UpperArmLengthcm=BMXARML)

BM<-BM %>% rename(UpperArmLengthComment=BMIARML)
BM<-BM %>% mutate(UpperArmLengthComment=case_when(
  UpperArmLengthComment == 1 ~ "Could not obtain"
))

BM<-BM %>% rename(ArmCircumferencecm=BMXARMC)

BM<-BM %>% rename(ArmCircumferenceComment=BMIARMC)
BM<-BM %>% mutate(ArmCircumferenceComment=case_when(
  ArmCircumferenceComment == 1 ~ "Could not obtain"))

BM<-BM %>% rename(WaistCicumferencecm=BMXWAIST)

BM<-BM %>% rename(WaistCicumferenceComment=BMIWAIST)
BM<-BM %>% mutate(WaistCicumferenceComment=case_when(
  WaistCicumferenceComment == 1 ~ "Could not obtain"))

#12 years to 150 years
BM<-BM %>% rename(HipCircumference=BMXHIP)

BM<-BM %>% rename(HipCircumferenceComment=BMIHIP)
BM<-BM %>% mutate(HipCircumferenceComment=case_when(
  HipCircumferenceComment == 1 ~ "Could not obtain"
))

###CHOLESTEROL - High-Density Lipoprotein
HDL<-read.xport("HDL_L.XPT")

HDL<-HDL %>% rename(Phlebotomy_2year_Weight=WTPH2YR)

#mg/dL
HDL<-HDL %>% rename(DirectHDLCholesterol_mgdL=LBDHDD)
#mmol/L
HDL<-HDL %>% rename(DirectHDLCholesterol_mmolL=LBDHDDSI)

###CHOLESTEROL - Total Cholesterol
TC<-read.xport("TCHOL_L.XPT")

TC<-TC %>% rename(Phlebotomy_2year_Weight=WTPH2YR)

#mg/dL
TC<-TC %>% rename(TotalCholesterol_mgdl=LBXTC)
#mmol/L
TC<-TC %>% rename(TotalCholesterol_mmolL=LBDTCSI)

###COMPLETE BLOOD COUNT
CBC<-read.xport("CBC_L.XPT")

CBC<-CBC %>% rename(Phlebotomy_2year_Weight=WTPH2YR)

CBC<-CBC %>% rename(WhiteBloodCellCount=LBXWBCSI)

# %
CBC<-CBC %>% rename(LymphocytePercent=LBXLYPCT)
CBC<-CBC %>% rename(MonocytePercent=LBXMOPCT)
CBC<-CBC %>% rename(Segmented_neutrophilsPercent=LBXNEPCT)
CBC<-CBC %>% rename(EosinophilsPercent=LBXEOPCT)
CBC<-CBC %>% rename(BasophilsPercent=LBXBAPCT)

#1000 cells/uL
CBC<-CBC %>% rename(LymphocyteNumber=LBDLYMNO)
CBC<-CBC %>% rename(MonocyteNumber=LBDMONO)
CBC<-CBC %>% rename(Segmented_neutrophilsNumber=LBDNENO)
CBC<-CBC %>% rename(EosinophilsNumber=LBDEONO)
CBC<-CBC %>% rename(BasophilsNumber=LBDBANO)

#million cells/uL
CBC<-CBC %>% rename(RedBloodCellCount=LBXRBCSI)

#g/dL
CBC<-CBC %>% rename(Hemoglobin=LBXHGB)

CBC<-CBC %>% rename(HematocritPercent=LBXHCT)

#fL
CBC<-CBC %>% rename(MeanCellVolume=LBXMCVSI)

#Mean Cell Hgb concentration (g/dL)
CBC<-CBC %>% rename(MeanCellHgbConc=LBXMC)

#pg
CBC<-CBC %>% rename(MeanCellHemoglobin=LBXMCHSI)

# %
CBC<-CBC %>% rename(RedCellDistributionWidth=LBXRDW)

#1000 cells u/L
CBC<-CBC %>% rename(PlateletCount=LBXPLTSI)

#fL
CBC<-CBC %>% rename(MeanPlateletVolume=LBXMPSI)

CBC<-CBC %>% rename(NucleatedRedBloodCells=LBXNRBC)

###VITAMIN D
VITD<-read.xport("VID_L.XPT")

VITD<-VITD %>% rename(Phlebotomy_2year_Weight=WTPH2YR)

#25-hydroxyvitaminD2+D3 (nmol/L)
VITD<-VITD %>% rename(HD2HD3=LBXVIDMS)

#25-hydroxyvitamineD2+D3 comment code
VITD<-VITD %>% rename(HD2HD3CC=LBDVIDLC)
VITD<-VITD %>% mutate(HD2HD3CC=case_when(
  HD2HD3CC == 0 ~ "At or above the detection limit",
  HD2HD3CC == 1 ~ "Below lower detection limit"
))

#25-hydroxyvitamin D2(nmol/L)
VITD<-VITD %>% rename(HD2=LBXVD2MS)

#25-hydroxyvitamin D2 comment code
VITD<-VITD %>% rename(HD2CC=LBDVD2LC)
VITD<-VITD %>% mutate(HD2CC=case_when(
  HD2CC == 0 ~ "At or above detection limit",
  HD2CC == 1 ~ "Below lower detection limit"
))

#25-hydroxyvitamin D3 (nmol/L)
VITD<-VITD %>% rename(HD3=LBXVD3MS)

#25-hydroxyvitamin D3 comment code
VITD<-VITD %>% rename(HD3CC=LBDVD3LC)
VITD<-VITD %>% mutate(HD3CC=case_when(
  HD3CC == 0 ~ "At or above detection limit",
  HD3CC == 1 ~ "Below lower detection limit"
))

#epi-25-hydroxyvitamine D3 (nmol/L)
VITD<-VITD %>% rename(epiHD3=LBXVE3MS)

#epi-25-hydroxyvitamin D3 comment code
VITD<-VITD %>% rename(epiHD3CC=LBDVE3LC)
VITD<-VITD %>% mutate(epiHD3CC=case_when(
  VITD == 0 ~ "At or above the detection limit",
  VITD == 1 ~ "Below lower detection limit"
))

#vec1 <- colnames(DEMO)
#vec2 <- colnames(VITD)
#vec3 <- colnames(BM)
#vec4 <- colnames(CBC)
#vec5 <- colnames(HDL)
#vec6 <- colnames(TC)

#Find intersection of all vectors
#result <- Reduce(intersect, list(vec1, vec2, vec3, vec4, vec5, vec6))
#result

###COMBINING DATA
MasterData<-BM %>% 
  full_join(CBC, by = "SEQN") %>% 
  full_join(DEMO, by = "SEQN") %>% 
  full_join(HDL, by = "SEQN") %>% 
  full_join(TC, by = "SEQN") %>% 
  full_join(VITD, by = "SEQN")

table(MasterData$SEQN)
#I am defining High Cholesterol as total cholesterol being greater than or equal
#to 240 mg/dL as defined by John Hopkins Medicine. The TRUE statement is sort of
#like a catch-all condition that labels anything with whatever value (in this case
#0) if none of the previous conditions were met.

MasterData<-MasterData %>% 
  mutate(High_Chol=case_when(
    TotalCholesterol_mgdl >= 240 ~ 1,
    TRUE ~ 0
  ))

###CLEANING DATA###
colnames(MasterData)

##Finding Percentage of Missing Values. If the column has a majority of its values missing I won't use it.
na_percentage<-colSums(is.na(MasterData))/nrow(MasterData)*100
na_percentage

names(MasterData)
MasterData_clean<-MasterData[,c("SEQN", "Weightkg", "StandingHeightcm",
                                "BMI", "Gender", "Age", "Race", "Active_Duty", 
                                "Country_of_Birth", "People_in_Household",
                                "TotalCholesterol_mgdl", "DirectHDLCholesterol_mgdL",
                                "HD2HD3", "HD2", "HD3", "epiHD3", "WhiteBloodCellCount",
                                "LymphocytePercent", "MonocytePercent", 
                                "Segmented_neutrophilsPercent", "EosinophilsPercent",
                                "BasophilsPercent", "RedBloodCellCount", 
                                "PlateletCount", "Hemoglobin", "High_Chol")]

#MasterData_clean$Gender <- as.factor(MasterData_clean$Gender)
#MasterData_clean$Race <- as.factor(MasterData_clean$Race)
#MasterData_clean$Active_Duty <- as.factor(MasterData_clean$Active_Duty)
#MasterData_clean$Country_of_Birth <- as.factor(MasterData_clean$Country_of_Birth)
#MasterData_clean$High_Chol<-as.factor(MasterData_clean$High_Chol)

continuous_vars<-MasterData_clean%>%select(where(is.numeric))%>%colnames()
continuous_vars

MasterData_clean <- MasterData_clean %>%
  mutate(across(all_of(continuous_vars), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

cat_vars<-MasterData_clean%>%select(where(is.character))%>%colnames()
cat_vars

calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

MasterData_clean <- MasterData_clean %>%
  mutate(across(all_of(cat_vars), ~ ifelse(is.na(.), calculate_mode(.[!is.na(.)]), .)))



#install.packages("zoo")
#library(zoo)

#MasterData_clean$Active_Duty <- as.character(MasterData_clean$Active_Duty)

#MasterData_clean <- MasterData_clean %>%
#  mutate(Active_Duty = na.locf(Active_Duty, na.rm = FALSE),  # Forward fill
#         Active_Duty = na.locf(Active_Duty, fromLast = TRUE))  # Backward fill

#head(MasterData_clean)

na_percentage<-colSums(is.na(MasterData_clean))/nrow(MasterData_clean)*100
na_percentage

head(MasterData_clean)



###Multiple Imputation###
#There is a lot of missing data
#imputed_data<-mice(MasterData_clean, m = 5, method = 'pmm', maxit = 50, seed = 500)

#model_fit <- with(data= imputed_data, lm(High_Chol ~ HD2HD3 + WhiteBloodCellCount))
#pooled_results <- pool(model_fit)
#summary(pooled_results)

#MasterData_clean<-complete(imputed_data,action="long")

write.csv(MasterData_clean,"MasterData.csv", row.names = FALSE)

##CHECK##
# Filters out features with low variance
#DB_FILTER <- nearZeroVar(MASTER_DATA_TRAIN %>% select(-CRS_Tomorrow, -PT_ID, -hr_of_crs, -CRS_EVER),
#                         freqCut = 19,
#                         uniqueCut = 10,
#                         saveMetrics = F,
#                         names = TRUE,
#                         foreach = FALSE,
#                         allowParallel = TRUE)

#set.seed(2024)

#install.packages("caret")
#library("caret")
#nearZeroVar(MasterData,names=TRUE) #these don't change much. Ignore

###IMPUTE THE MEAN FOR CONTINUOUS. FOR CATEGORICAL IMPUTE WITH MOST FREQUENT###


##CHECK##
