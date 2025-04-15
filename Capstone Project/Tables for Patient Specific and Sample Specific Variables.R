AML<-read.csv("C:/Users/Tony/Documents/Capstone Project/WorkingDataSetVer3.csv")

##Patient Specific Table
require(table1)
require(tidyverse)
require(tableone)

length(unique(AML$patient_id))

table(AML$patient_id,
      AML$platform)

AML<-AML %>%
  mutate(
    Days_to_death_or_Last_Follow_Up=case_when(
      !is.na(days_to_death) ~ days_to_death,
      TRUE ~ days_to_last_followup
    )
  )

AML_clean <- AML[!is.na(AML$vital_status), ]

colnames(AML_clean)

#I think it's kind of pointless to leave patient_id in here. I also think cases.submitter_id
#isn't very useful either. I'll leave both out.

table1(~Age+gender+Days_to_death_or_Last_Follow_Up
       +acute_myeloid_leukemia_calgb_cytogenetics_risk_category
       +leukemia_french_american_british_morphology_code|vital_status,data=AML_clean)->PST
PST

#Clock Table 
colnames(AML)
table1(~DNAmAge+Age+DNAmGrimAgeBasedOnRealAge+DNAmAgeSkinBloodClock+DNAmPhenoAge|vital_status,data=AML_clean)

#Components of GrimAge
table1(~Age+gender+DNAmGrimAgeBasedOnRealAge+DNAmB2M+DNAmADM+DNAmCystatinC+DNAmTIMP_1
       |vital_status,data=AML_clean)