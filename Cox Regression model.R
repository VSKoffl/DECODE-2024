cox_data = read.csv(file.choose()) #INSULA_SF_36.csv
library(survival)
library(dplyr)
model_data = cox_data %>%
  filter (is.na(cox_data$week)!= TRUE) %>%
  select(HB_W0,week,flag,AGE,GENDER_Encoded,FP_W0,BP_WK_0,PF_WK_0,MH_WK_0)
model_data

cox = coxph(Surv(week,flag)~ HB_W0+AGE+FP_W0+BP_WK_0+PF_WK_0+MH_WK_0,data=model_data)
library(MASS)
stepAIC(cox,direction = "both")
cox_f = coxph(Surv(week,flag)~HB_W0,data = model_data) #Modified based on AIC results
summary(cox_f)

