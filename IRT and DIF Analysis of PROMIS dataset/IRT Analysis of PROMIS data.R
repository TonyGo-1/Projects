#Dataset is from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5JAACI

install.packages("readxl")
install.packages("haven")
install.packages("mirt")
library(haven)
library(readxl)
library(mirt)


#Here we create a file for the dictionary. We also download the sas file into R for analysis below
Dictionary<-read_excel("DATADICTIONARY.xlsx")
data<-read_sas("p1w1painsupplement.sas7bdat")

#There are 966 subjects
length(data$caseid)

#There are 182 males and 780 females
table(data$gender)

#PAININ3: In the past few 7 days, How much did pain interfere with your enjoyment of life?
#PAINBE10: In the past 7 days, I had minor aches and pains
#PAINBE31: In the past 7 days, I limped because of pain

#All of these ranged from a Likert scale of 1 to 6. 1=Had no pain,2=Never,3=Rarely,4=Sometimes,5=Often,6=Always
df<-data[, c("PAININ3", "PAINBE10", "PAINBE31", "gender")]
df<-na.omit(df)

dim(df)

selectedpains<-df[,c("PAININ3","PAINBE10","PAINBE31")]
gen<-factor(df$gender)

table(gen)
summary(selectedpains)

for (item in colnames(selectedpains)) {
  cat("Item:", item, "\n")
  print(table(selectedpains[[item]], gen))
  cat("\n")
}

selectedpains$PAINBE10[selectedpains$PAINBE10 == 1] <- 2
table(selectedpains$PAINBE10, gen)


mod_mg <- multipleGroup(selectedpains, model = 1, group = gen)

dif_results <- DIF(mod_mg, which.par = c("a1", "d1", "d2", "d3", "d4", "d5"))
dif_results

