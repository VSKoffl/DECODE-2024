library(readxl)
ancova_data<-read_xlsx(file.choose(),col_names=T) #adsl_stat_COUNT.xlsx
ancova_data
attach(ancova_data)
ancova_data<-data.frame(ancova_data$Diff1,ancova_data$COUNTRY,ancova_data$AGE,ancova_data$SEX,ancova_data$TRT,ancova_data$HB_W0,ancova_data$COMPLFL,data=ancova_data)
ancova_data
Treatment<-as.factor(ancova_data$data.TRT)
Baseline_HbA1C<-ancova_data$data.HB_W0
Change_in_HbA1C<-ancova_data$data.Diff1
ancova<-aov(Change_in_HbA1C~Baseline_HbA1C+Treatment)
library(car)
ancovaR<-Anova(ancova,type="II")
ancovaR

model1<-lm(Change_in_HbA1C~Treatment+Baseline_HbA1C,data=ancova_data)
plot(fitted(model1),residuals(model1))

install.packages("emmeans")
library(emmeans)
marginal_means <- emmeans(model1,~Treatment)
summary(marginal_means)
pairwise_comparisons <- contrast(marginal_means, method = "pairwise")
summary(pairwise_comparisons)

