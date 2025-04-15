### Three hashes are titles
## Two hashes are subtitles (ex.What I'm testing)
# One hash is a comment or code that I commented out

###LOAD PACKAGES AND LOAD DATA (FILTER FOR ILLUMINA HUMAN METHYLATION 450)###
require(tidyverse)
require(survival)
require(survminer)
require(gtsummary)
require(MASS)

MasterData <- read_csv("C:/Users/Tony/Documents/Capstone Project/WorkingDataSetVer3.csv") %>% 
  filter(platform=='Illumina Human Methylation 450')

###Add two columns "EpiAgeBinary" and "vital_status_binary"
MasterData<-MasterData%>%
  mutate(EpiAgeBinary=case_when(AgeAccelGrim > 0 ~ 'Positive',
                                AgeAccelGrim < 0 ~ 'Negative',
                                TRUE ~ 'Neither'),
  vital_status_binary=case_when(vital_status=='Dead' ~ 1,
                                vital_status == 'Alive' ~ 0,
                                TRUE ~ NA_real_))%>%
  filter(!duplicated(cases.submitter_id))


###Run the univariate Analysis



##List of variables (lab_procedure_hematocrit_outcome_percent_value)
variables_list <- c("gender","Age","race","SenescenceSecrete","SenescenceMembrane",
                    "EpiAgeBinary","AgeAccelGrim","lab_procedure_blast_cell_outcome_percentage_value"
                    ,"lab_procedure_bone_marrow_cellularity_outcome_percent_value",
                    "lab_procedure_leukocyte_result_unspecified_value","lab_procedure_hemoglobin_result_specified_value",
                    "lab_procedure_bone_marrow_blast_cell_outcome_percent_value",
                    "lab_procedure_bone_marrow_promyelocyte_result_percent_value",
                    "lab_procedure_bone_marrow_promonocyte_count_result_percent_value",
                    "lab_procedure_abnormal_lymphocyte_result_percent_value","CD8T","CD4T","NK",
                    "Bcell","Mono","Gran","PlasmaBlast","CD8pCD28nCD45RAn","CD8.naive","CD4.naive",
                    "DNAmAge","DNAmPhenoAge","DNAmGrimAge2BasedOnRealAge","DNAmGrimAge2BasedOnPredictedAge",
                    "DNAmGrimAgeBasedOnPredictedAge","DNAmTL","DNAmADM","DNAmB2M","DNAmCystatinC","DNAmGDF15",
                    "DNAmLeptin","DNAmPACKYRS","DNAmPAI1","DNAmTIMP1","DNAmGDF_15","DNAmCystatin_C","DNAmTIMP_1",
                    "DNAmadm","DNAmpai_1","DNAmleptin","DNAmGrimAgeBasedOnPredictedAgeAdjAge","DNAmTLAdjAge",
                    "DNAmAgeSkinBloodClock","leukemia_french_american_british_morphology_code")

colnames(MasterData)

##Fit Cox models and create regression tables for each variable
cox_tables<-lapply(seq_along(variables_list), function(i) {
  formula<-as.formula(paste("Surv(days_to_death, vital_status_binary) ~ ",variables_list[i]))
  cox_model<-coxph(formula,data = MasterData)
  tbl_regression(cox_model,exp=TRUE) %>%
    modify_caption(paste("Cox Proportional Hazards Model"))
})

##Table Output
combined_table<-tbl_stack(cox_tables)
combined_table

combined_table%>%add_q()



table(MasterData$leukemia_french_american_british_morphology_code)

###FINDING MODEL OF BEST FIT FOR COX REGRESSION

##Borderline Significance List
#EpiAgeBinary (0.07)
#AgeAccelGrim (0.10)
#lab_procedure_leukocyte_result_unspecified_value (0.10)
#lab_procedure_hemoglobin_result_specified_value (0.085)
#Gran (0.059)


##Did not include these values
#CD8pCD28nCD45RAn HR:1.02 [CI:1.00-1.04] (0.022)
#CD8.naive HR:1.00 [CI:1.00-1.00] (0.003)
#DNAmGrimAge2BasedOnRealAge HR:1.02 [CI:1.01-1.03] (0.004)
#DNAmADM HR:1.00 [CI:1.00-1.00] (0.021)
#DNAmB2M HR:1.00 [CI:1.00-1.00] (0.001)
#DNAmCystatinC HR:1.00 [CI:1.00-1.00] (<0.001)
#DNAmTIMP1 HR:1.00 [CI:1.00-1.00] (0.008)
#DNAmadm HR:1.00 [CI:1.00-1.00] (0.11)

#Did not include Age
significant_variables_list<-c(
                     "EpiAgeBinary","AgeAccelGrim",
                    "lab_procedure_leukocyte_result_unspecified_value",
                    "lab_procedure_hemoglobin_result_specified_value",
                    "NK","Bcell","Mono","Gran","DNAmTL",
                    "DNAmTLAdjAge",
                    "leukemia_french_american_british_morphology_code")

#Remove missing values from MasterData
MasterData_clean<-na.omit(MasterData[,c("days_to_death",
                                        "vital_status_binary",
                                        significant_variables_list)])


  tbl_regression(exp = TRUE)
# Fit Cox model with significant variables
cox_full_model<-coxph(Surv(days_to_death,vital_status_binary)~.,
                      data=MasterData_clean)

cox_full_model%>%tbl_regression(exp=TRUE)

# Perform backward selection using stepAIC
cox_backward<-stepAIC(cox_full_model,direction="both")
cox_backward
AIC(cox_backward)
cox_backward%>%tbl_regression(exp=TRUE)

###5/12/24
###CHECKING WHICH COLUMNS HAVE NA
significant_variables_list<-c(
  "EpiAgeBinary","AgeAccelGrim",
  "lab_procedure_leukocyte_result_unspecified_value",
  "lab_procedure_hemoglobin_result_specified_value",
  "NK","Bcell","Mono","Gran","DNAmTL",
  "DNAmTLAdjAge",
  "leukemia_french_american_british_morphology_code")

##checking which columns had a lot of NA values
sum(is.na(MasterData$EpiAgeBinary)) #0
sum(is.na(MasterData$AgeAccelGrim)) #0
sum(is.na(MasterData$lab_procedure_leukocyte_result_unspecified_value)) #1
sum(is.na(MasterData$lab_procedure_hemoglobin_result_specified_value)) #1
sum(is.na(MasterData$NK)) #0
sum(is.na(MasterData$Bcell)) #0
sum(is.na(MasterData$Mono)) #0
sum(is.na(MasterData$Gran)) #0
sum(is.na(MasterData$DNAmTL)) #0
sum(is.na(MasterData$DNAmTLAdjAge)) #0
sum(is.na(MasterData$leukemia_french_american_british_morphology_code)) #0
sum(is.na(MasterData$days_to_death)) #78 This one's the problem
sum(is.na(MasterData$vital_status_binary)) #0

max(MasterData$days_to_death,na.rm = TRUE)
MasterData%>%
  mutate(days_to_death_clean=case_when(is.na(days_to_death) & vital_status=="Alive" ~ 1706,
                                       TRUE ~ days_to_death))->MasterData

cox_full_model<-coxph(Surv(days_to_death_clean,vital_status_binary)~ AgeAccelGrim,
                      data=MasterData)

cox_full_model%>%tbl_regression(exp=TRUE)
summary(cox_full_model)
table(MasterData$vital_status)

MasterData$days_to_death
NA_distribution<-table(is.na(MasterData$days_to_death))
NA_distribution
#Maybe we can say MCAR? There doesn't seem to be a pattern to the distribution
#of NAs. If it's MAR, we may have to use permutation.

#Changing Undifferentiated into NA.
MasterData<-MasterData%>%
  mutate(leukemia_french_american_british_morphology_code=case_when(
    leukemia_french_american_british_morphology_code=="Not Classified"~NA_character_,
    TRUE~leukemia_french_american_british_morphology_code
  ))

#Combining M6 and M7.
MasterData<-MasterData%>%
  mutate(newmorphcode=case_when(
    leukemia_french_american_british_morphology_code=="M6" ~ "M6 and M7",
    leukemia_french_american_british_morphology_code=="M7" ~ "M6 and M7",
    TRUE ~ leukemia_french_american_british_morphology_code
  ))

significant_variables_list<-c(
  "EpiAgeBinary","AgeAccelGrim",
  "lab_procedure_leukocyte_result_unspecified_value",
  "lab_procedure_hemoglobin_result_specified_value",
  "NK","Bcell","Mono","Gran","DNAmTL",
  "DNAmTLAdjAge",
  "newmorphcode")

cox_full_model<-coxph(Surv(days_to_death,vital_status_binary)~.,
                      data=MasterData_clean)

cox_full_model%>%tbl_regression(exp=TRUE)

#Selecting new model
cox_backward<-stepAIC(cox_full_model,direction="both")
cox_backward
AIC(cox_backward)
cox_backward%>%tbl_regression(exp=TRUE)

###Scale and Center then Rerun

#CD8pCD28nCD45RAn HR:1.02 [CI:1.00-1.04] (0.022)
#CD8.naive HR:1.00 [CI:1.00-1.00] (0.003)
#DNAmGrimAge2BasedOnRealAge HR:1.02 [CI:1.01-1.03] (0.004)
#DNAmADM HR:1.00 [CI:1.00-1.00] (0.021)
#DNAmB2M HR:1.00 [CI:1.00-1.00] (0.001)
#DNAmCystatinC HR:1.00 [CI:1.00-1.00] (<0.001)
#DNAmTIMP1 HR:1.00 [CI:1.00-1.00] (0.008)
#DNAmadm HR:1.00 [CI:1.00-1.00] (0.11)

MasterData$ScaledCD8pCD28nCD45RAn<-scale(MasterData$CD8pCD28nCD45RAn)
MasterData$ScaledCD8.naive<-scale(MasterData$CD8.naive)
MasterData$ScaledDNAmGrimAge2BasedOnRealAge<-scale(MasterData$DNAmGrimAge2BasedOnRealAge)
MasterData$ScaledDNAmADM<-scale(MasterData$DNAmADM)
MasterData$ScaledDNAmB2M<-scale(MasterData$DNAmB2M)
MasterData$ScaledDNAmCystatinC<-scale(MasterData$DNAmCystatinC)
MasterData$ScaledDNAmTIMP1<-scale(MasterData$DNAmTIMP1)
MasterData$ScaledDNAmadm<-scale(MasterData$DNAmadm)

##Combine M6 and M7
MasterData<-MasterData%>%
  mutate(newmorphcode=case_when(
    leukemia_french_american_british_morphology_code=="M6" ~ "M6 and M7",
    leukemia_french_american_british_morphology_code=="M7" ~ "M6 and M7",
    TRUE ~ leukemia_french_american_british_morphology_code
  ))

#Changing Not Classified into NA.
MasterData<-MasterData%>%
  mutate(leukemia_french_american_british_morphology_code=case_when(
    leukemia_french_american_british_morphology_code=="Not Classified"~NA_character_,
    TRUE~leukemia_french_american_british_morphology_code
  ))


###RUNNING UNIVARIATE ANALYSIS

##Fit Cox models and create regression tables for each variable
cox_tables<-lapply(seq_along(variables_list), function(i) {
  formula<-as.formula(paste("Surv(days_to_death_clean, vital_status_binary) ~ ",variables_list[i]))
  cox_model<-coxph(formula,data = MasterData)
  tbl_regression(cox_model,exp=TRUE) %>%
    modify_caption(paste("Cox Proportional Hazards Model"))
})

##Table Output
combined_table<-tbl_stack(cox_tables)
combined_table


#Default
significant_variables_list<-c(
  "EpiAgeBinary","AgeAccelGrim","Age",
  "lab_procedure_hemoglobin_result_specified_value",
  "ScaledCD8.naive","ScaledDNAmADM",
  "ScaledDNAmB2M","ScaledDNAmCystatinC","ScaledDNAmGrimAge2BasedOnRealAge","ScaledDNAmTIMP1","ScaledDNAmadm","newmorphcode")

#Age
significant_variables_list<-c(
  "Age",
  "lab_procedure_hemoglobin_result_specified_value",
  "ScaledCD8.naive","ScaledDNAmADM",
  "ScaledDNAmB2M","ScaledDNAmCystatinC","ScaledDNAmTIMP1","ScaledDNAmadm","newmorphcode")

#AgeAccelGrim
significant_variables_list<-c(
  "AgeAccelGrim",
  "lab_procedure_hemoglobin_result_specified_value",
  "ScaledCD8.naive","ScaledDNAmADM",
  "ScaledDNAmB2M","ScaledDNAmCystatinC","ScaledDNAmTIMP1","ScaledDNAmadm","newmorphcode")

#EpiAgeBinary
significant_variables_list<-c(
  "EpiAgeBinary",
  "lab_procedure_hemoglobin_result_specified_value",
  "ScaledCD8.naive","ScaledDNAmADM",
  "ScaledDNAmB2M","ScaledDNAmCystatinC","ScaledDNAmTIMP1","ScaledDNAmadm","newmorphcode")

#ScaledDNAmGrimAge2BasedOnRealAge
significant_variables_list<-c(
  "lab_procedure_hemoglobin_result_specified_value",
  "ScaledCD8.naive","ScaledDNAmADM",
  "ScaledDNAmB2M","ScaledDNAmCystatinC","ScaledDNAmGrimAge2BasedOnRealAge","ScaledDNAmTIMP1","ScaledDNAmadm","newmorphcode")

#just components no age 
significant_variables_list<-c(
  "lab_procedure_hemoglobin_result_specified_value",
  "ScaledCD8.naive","ScaledDNAmADM",
  "ScaledDNAmB2M","ScaledDNAmCystatinC","ScaledDNAmTIMP1","ScaledDNAmadm","newmorphcode")

MasterData_clean<-na.omit(MasterData[,c("days_to_death_clean",
                                        "vital_status_binary",
                                        significant_variables_list)])


# Fit Cox model with significant variables
cox_full_model<-coxph(Surv(days_to_death_clean,vital_status_binary)~.,
                      data=MasterData_clean)

cox_full_model%>%tbl_regression(exp=TRUE)

# Perform backward selection using stepAIC
cox_backward<-stepAIC(cox_full_model,direction="both")
cox_backward
AIC(cox_backward)
cox_backward%>%tbl_regression(exp=TRUE)
quantile(MasterData$DNAmGrimAge2BasedOnRealAge)
MasterData%>%
  mutate(AgeDiff=DNAmGrimAge2BasedOnRealAge-Age)->MasterData

cox_full_model<-coxph(Surv(days_to_death_clean,vital_status_binary)~AgeDiff,
                      data=MasterData)
cox_full_model%>%tbl_regression(exp=TRUE)


###NOTE
#talk about univariate analysis (picking a few of the specific ones to talk about)
#maybe talk about a combined model