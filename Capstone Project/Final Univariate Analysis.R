### Three hashes are titles
## Two hashes are subtitles (ex.What I'm testing)
# One hash is a comment or code that I commented out

###LOAD PACKAGES AND LOAD DATA (FILTER FOR ILLUMINA HUMAN METHYLATION 450)###
require(tidyverse)
require(survival)
require(survminer)
require(gtsummary)
require(MASS)
require(tidyverse)
require(tableone)
require(table1)
require(dplyr)

###Modifying MasterData
MasterData <- read_csv("C:/Users/Tony/Documents/Capstone Project/WorkingDataSetVer3.csv") %>% 
  filter(platform=='Illumina Human Methylation 450')

MasterData<-MasterData%>%
  mutate(EpiAgeBinary=case_when(AgeAccelGrim > 0 ~ 'Positive',
                                AgeAccelGrim < 0 ~ 'Negative',
                                TRUE ~ 'Neither'),
         vital_status_binary=case_when(vital_status=='Dead' ~ 1,
                                       vital_status == 'Alive' ~ 0,
                                       TRUE ~ NA_real_))%>%
  filter(!duplicated(cases.submitter_id))

MasterData<-MasterData%>%
  mutate(leukemia_french_american_british_morphology_code=case_when(
    leukemia_french_american_british_morphology_code=="Not Classified"~NA_character_,
    TRUE~leukemia_french_american_british_morphology_code
  ))

##Combining M6 and M7.
MasterData<-MasterData%>%
  mutate(newmorphcode=case_when(
    leukemia_french_american_british_morphology_code=="M6" ~ "M6 and M7",
    leukemia_french_american_british_morphology_code=="M7" ~ "M6 and M7",
    TRUE ~ leukemia_french_american_british_morphology_code
  ))

##Scaling Continuous Variables
MasterData$ScaledCD8T<-scale(MasterData$CD8T)
MasterData$ScaledCD4T<-scale(MasterData$CD4T)
MasterData$ScaledNK<-scale(MasterData$NK)
MasterData$ScaledBcell<-scale(MasterData$Bcell)
MasterData$ScaledMono<-scale(MasterData$Mono)
MasterData$ScaledGran<-scale(MasterData$Gran)
MasterData$ScaledPlasmaBlast<-scale(MasterData$PlasmaBlast)
MasterData$ScaledCD8pCD28nCD45RAn<-scale(MasterData$CD8pCD28nCD45RAn)
MasterData$ScaledCD8.naive<-scale(MasterData$CD8.naive)
MasterData$ScaledCD4.naive<-scale(MasterData$CD4.naive)
MasterData$ScaledDNAmGrimAge2BasedOnRealAge<-scale(MasterData$DNAmGrimAge2BasedOnRealAge)
MasterData$ScaledDNAmADM<-scale(MasterData$DNAmADM)
MasterData$ScaledDNAmB2M<-scale(MasterData$DNAmB2M)
MasterData$ScaledDNAmCystatinC<-scale(MasterData$DNAmCystatinC)
MasterData$ScaledDNAmTIMP1<-scale(MasterData$DNAmTIMP1)
MasterData$ScaledDNAmadm<-scale(MasterData$DNAmadm)
MasterData$ScaledDNAmTL<-scale(MasterData$DNAmTL)
MasterData$Scaledlab_procedure_leukocyte_result_unspecified_value<-scale(MasterData$lab_procedure_leukocyte_result_unspecified_value)
###Combine the two columns 'days_to_death' and 'days_to_last_followup' 
##New columns created: combined_time and 'event indicator'

MasterData<-MasterData %>%
  mutate(
    combined_time=case_when(
      !is.na(days_to_death) ~ days_to_death,
      TRUE ~ days_to_last_followup
    )
  )


###Run the univariate Analysis

##List of variables
variables_list <- c("gender","Age","race","SenescenceSecrete","SenescenceMembrane",
                    "EpiAgeBinary","AgeAccelGrim","lab_procedure_blast_cell_outcome_percentage_value"
                    ,"lab_procedure_bone_marrow_cellularity_outcome_percent_value",
                    "lab_procedure_leukocyte_result_unspecified_value","lab_procedure_hemoglobin_result_specified_value",
                    "lab_procedure_bone_marrow_blast_cell_outcome_percent_value",
                    "lab_procedure_bone_marrow_promyelocyte_result_percent_value",
                    "lab_procedure_bone_marrow_promonocyte_count_result_percent_value",
                    "lab_procedure_abnormal_lymphocyte_result_percent_value","ScaledCD8T","ScaledCD4T","ScaledNK",
                    "ScaledBcell","ScaledMono","ScaledGran","ScaledPlasmaBlast","ScaledCD8pCD28nCD45RAn",
                    "ScaledCD8.naive","ScaledCD4.naive",
                    "DNAmAge","DNAmPhenoAge","ScaledDNAmGrimAge2BasedOnRealAge","DNAmGrimAge2BasedOnPredictedAge",
                    "DNAmGrimAgeBasedOnPredictedAge","ScaledDNAmTL","ScaledDNAmADM","ScaledDNAmB2M","ScaledDNAmCystatinC","DNAmGDF15",
                    "DNAmLeptin","DNAmPACKYRS","DNAmPAI1","ScaledDNAmTIMP1","DNAmGDF_15","DNAmCystatin_C","DNAmTIMP_1",
                    "ScaledDNAmadm","Scaledlab_procedure_leukocyte_result_unspecified_value","DNAmpai_1","DNAmleptin","DNAmGrimAgeBasedOnPredictedAgeAdjAge","DNAmTLAdjAge",
                    "DNAmAgeSkinBloodClock","newmorphcode")

###MAKE SURE THE SCALED VERSION HAS NO ORIGINAL COUNTERPART.


##Fit Cox models and create regression tables for each variable
cox_tables<-lapply(seq_along(variables_list), function(i) {
  formula<-as.formula(paste("Surv(combined_time, vital_status_binary) ~ ",variables_list[i]))
  cox_model<-coxph(formula,data = MasterData)
  tbl_regression(cox_model,exp=TRUE) %>%
    modify_caption(paste("Cox Proportional Hazards Model"))
})

##Table Output
combined_table<-tbl_stack(cox_tables)
combined_table%>%add_q()->combined_table

combined_table

#EpiAgeBinary and AgeAccelGrim are not significant, but I included them. 

##this one is for p-values. It should be noted that newmorph code is not significant under q-values
#Let's remove Age and SenescenceSecrete for now. Maybe we can add it back in later
significant_variables_list<-c(
  "Scaledlab_procedure_leukocyte_result_unspecified_value","ScaledCD4.naive",
  "lab_procedure_hemoglobin_result_specified_value","ScaledDNAmGrimAge2BasedOnRealAge","ScaledDNAmTL","ScaledDNAmADM",
  "ScaledDNAmB2M","ScaledDNAmCystatinC","ScaledDNAmTIMP1","newmorphcode"
  )

#table of significant variables
table1(~Scaledlab_procedure_leukocyte_result_unspecified_value+ScaledCD4.naive
       +lab_procedure_hemoglobin_result_specified_value+ScaledDNAmGrimAge2BasedOnRealAge
       +ScaledDNAmTL+ScaledDNAmADM
       +ScaledDNAmB2M+ScaledDNAmCystatinC+ScaledDNAmTIMP1+newmorphcode|vital_status,data=MasterData)->t
t

#"ScaledDNAmadm" was removed from list above

##Fit Cox model with significant variables
MasterData_clean<-na.omit(MasterData[,c("combined_time",
                                        "vital_status_binary",
                                        significant_variables_list)])

#there seems to be a lot of NA values again
sum(is.na(MasterData$SenescenceSecrete)) #46 We need to do something about this
sum(is.na(MasterData$lab_procedure_leukocyte_result_unspecified_value)) #1
sum(is.na(MasterData$lab_procedure_hemoglobin_result_specified_value)) #1
sum(is.na(MasterData$newmorphcode)) #2

cox_full_model<-coxph(Surv(combined_time,vital_status_binary)~.,
                      data=MasterData_clean)

cox_full_model%>%tbl_regression(exp=TRUE)%>%add_q()->cox_full_model

cox_full_model

##AIC TEST
#convert it back from a table
cox_full_model<-coxph(Surv(combined_time,vital_status_binary)~.,
                      data=MasterData_clean)


cox_backward<-stepAIC(cox_full_model,direction="both")
cox_backward
AIC(cox_backward)
cox_backward%>%tbl_regression(exp=TRUE)

summary(cox_backward)
basehaz(cox_backward)

###TEST PH ASSUMPTIONS
test.ph=cox.zph(cox_backward)
test.ph

ggcoxzph(test.ph)[6]

###TRY SURVIVAL PLOT WITH LOW MED HIGH (TERTILE FOR SCALED DNAmADM and SCALEDDNAmTIMP1). Kaplan

#Scatterplot
#DNAmAge, DNAmPhenoAge, DNAmGrimAge2BasedOnRealAge, DNAmGrimAgeBasedOnRealAge, DNAmGrimAgeBasedOnPredictedAgeAgeAdjAge
ggplot(MasterData,aes(x=Age,y=DNAmAge)) +
  geom_point() +
  lims(x=c(0,100),y=c(0,100)) +
  labs(title = "Scatterplot of Real Age vs Epigenetic Age",
       x = "Real Age",
       y = "Epigenetic Age") +
  stat_cor() +
  geom_abline() +
  #geom_smooth(method="lm") +
  theme_minimal()
#most patient's epigenetic age was higher than their real age

colnames(MasterData)
ggplot(MasterData,aes(x=Age,y=DNAmGrimAgeBasedOnRealAge)) +
  geom_point() +
  lims(x=c(0,100),y=c(0,100)) +
  labs(title = "Scatterplot of Real Age vs Epigenetic Age",
       x = "Real Age",
       y = "Epigenetic Age") +
  stat_cor() +
  geom_abline() +
  #geom_smooth(method="lm") +
  theme_minimal()

#PLAY AROUND A LITTLE BIT TO MAKE THE SCATTERPLOT LOOK PRETTIER
colnames(MasterData)

ggplot(MasterData,aes(x=Age,y=SenescenceSecrete)) +
  geom_point() +
  labs(title="Scatterplot of Age VS SenescenceScore",
       x="Age",
       y="Senescence Score")+
  theme_minimal()

ggplot(MasterData,aes(x=AgeAccelGrim,y=SenescenceSecrete)) +
  geom_point() +
  labs(title="Scatterplot of Age VS SenescenceScore",
       x="AgeAccelGrim",
       y="Senescence Score")+
  theme_minimal()

###tertiles
library(survival)
library(survminer)

quantile(MasterData$ScaledCD4.naive)
MasterData%>%mutate(GROUPS=case_when(ScaledCD4.naive <= -1~"Low",
                                     ScaledCD4.naive >= 1~"High",
                                     TRUE~ "Medium"))->MasterData
                
table(MasterData$GROUPS)

colnames(MasterData)

surv_obj<-Surv(time=MasterData[["combined_time"]],event=MasterData[["vital_status_binary"]])
KM<-survfit(surv_obj ~ GROUPS,data=MasterData)
ggsurvplot(KM,data=MasterData,risk.table=TRUE,pval=TRUE, 
           conf.int=TRUE, 
           xlab="Time", 
           ylab="Survival Probability",
           title="Kaplan-Meier Curve of CD4.naive",
           legend.title="Levels of CD4.naive",
           legend.labs=c("Low", "Medium", "High"))

MasterData$tertile<-cut(MasterData[["DNAmTIMP1"]],breaks= 
                          quantile(MasterData[["DNAmTIMP1"]],
                                   probs=seq(0,1,by=1/3),
                                   na.rm=TRUE),include.lowest=TRUE,
                        labels=c('Low','Medium','High'))

surv_obj<-Surv(time=MasterData[["combined_time"]],event=MasterData[["vital_status_binary"]])
KM<-survfit(surv_obj ~ tertile,data=MasterData)
ggsurvplot(KM,data=MasterData,risk.table=TRUE,pval=TRUE, 
           conf.int=TRUE, 
           xlab="Time", 
           ylab="Survival Probability",
           title="Kaplan-Meier Curve of DNAmTIMP1",
           legend.title="Levels of DNAmTIMP1",
           legend.labs=c("Low", "Medium", "High"))




quantile(MasterData$CD4.naive,seq(from=0,to=1,length.out=4))->catvec
catvec

###6/6/24 Cox PH Model
List_of_Variables_in_our_model<-c("Scaledlab_procedure_leukocyte_result_unspecified_value","ScaledCD4.naive",
         "lab_procedure_hemoglobin_result_specified_value","ScaledDNAmADM","ScaledDNAmTIMP1",
         "newmorphcode")

colnames(MasterData_clean)

MasterData_clean%>%mutate(GROUPS=case_when(ScaledCD4.naive <= -1~"Very Low",
                                           ScaledCD4.naive <= -0.5 & ScaledCD4.naive > -1~"Low",
                                           ScaledCD4.naive < 0.5 & ScaledCD4.naive > -0.5 ~ "Medium",
                                           ScaledCD4.naive >= 0.5 & ScaledCD4.naive < 1~"High",
                                           ScaledCD4.naive >= 1~"Very High",
                                           TRUE~ "Medium"))->MasterData_clean

surv_obj<-Surv(time=MasterData_clean[["combined_time"]],event=MasterData_clean[["vital_status_binary"]])

SurvivalCurve<-survfit(surv_obj ~ GROUPS,data=MasterData_clean)

ggsurvplot(SurvivalCurve,data=MasterData_clean,risk.table=TRUE,pval=TRUE, 
           xlab="Time", 
           ylab="Survival Probability",
           title="Survival Curve of CD4.naive",
           legend.title="Levels of CD4.naive",
           legend.labs=c("Very Low","Low","Medium","High","Very High"))


###ScaledDNAmTIMP1
MasterData_clean%>%mutate(GROUPS=case_when(ScaledDNAmTIMP1 <= -1~"Very Low",
                                           ScaledDNAmTIMP1 <= -0.5 & ScaledDNAmTIMP1 > -1~"Low",
                                           ScaledDNAmTIMP1 < 0.5 & ScaledDNAmTIMP1 > -0.5 ~ "Medium",
                                           ScaledDNAmTIMP1 >= 0.5 & ScaledDNAmTIMP1 < 1~"High",
                                           ScaledDNAmTIMP1 >= 1~"Very High",
                                           TRUE~ "Medium"))->MasterData_clean

surv_obj<-Surv(time=MasterData_clean[["combined_time"]],event=MasterData_clean[["vital_status_binary"]])

SurvivalCurve<-survfit(surv_obj ~ GROUPS,data=MasterData_clean)

ggsurvplot(SurvivalCurve,data=MasterData_clean,risk.table=TRUE,pval=TRUE, 
           xlab="Time", 
           ylab="Survival Probability",
           title="Survival Curve of DNAmTIMP1",
           legend.title="Levels of DNAmTIMP1",
           legend.labs=c("Very Low","Low","Medium","High","Very High"))


write.csv(MasterData_clean, "MasterData_clean.csv", row.names = FALSE)

#Cox Ph Attempt
cox_model<-coxph(formula=Surv(combined_time,vital_status_binary) ~ Age + Scaledlab_procedure_leukocyte_result_unspecified_value + ScaledCD4.naive
      + lab_procedure_hemoglobin_result_specified_value + ScaledDNAmADM + newmorphcode, data = MasterData)

cox_model<-coxph(formula=Surv(combined_time,vital_status_binary) ~ DNAmGrimAgeBasedOnRealAge + Scaledlab_procedure_leukocyte_result_unspecified_value + ScaledCD4.naive
                 + lab_procedure_hemoglobin_result_specified_value + ScaledDNAmADM + newmorphcode, data = MasterData)

surv_fit <- survfit(cox_model)
plot(surv_fit, xlab = "Time", ylab = "Survival Probability", main = "Survival Curve")

colnames(MasterData)
