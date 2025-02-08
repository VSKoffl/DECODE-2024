library(survival)
library(survminer)
data <- read.csv(file.choose(),header=T) #adhypo_imputed.csv
sex<- factor(data$Sex)
treatment <- factor(data$TRT)
country <- factor(data$Country)
trt_emer<- factor(data$TRTEMFL)
subject_id<-data$SUBJID
event_date<-data$ASTDT
start_date<-data$START
baseline_hba1c<-data$Hb
fpg_values<-data$Fp
data <- data.frame(
  subject_id,
  time<-(data$Time),
  event<-data$Event,
  treatment,
  sex,
  age=data$Age,
  sugar_levels_at_event<-data$GLUCSTD,
  baseline_hba1c,
  fpg_values,
  trt_emer,
  country,data=data
)
cox_model <- coxph(Surv(time, event) ~ age + sex + treatment + baseline_hba1c + fpg_values + sugar_levels_at_event + country+trt_emer, data = data)
cox_model
summary(cox_model)
