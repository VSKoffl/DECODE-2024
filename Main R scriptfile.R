# Co primary endpoint analysis -------------------------------------------------------

data = read.csv(file.choose()) #imputed_m_final.csv
library(dplyr)

View(data)
Insula = data %>%
  filter (Treatment_Encoded == 1) %>%
  select(SUBJID,HB_W0,HB_W26)
Insula['bashb.0.26'] = Insula['HB_W26'] - Insula['HB_W0'] # Subjects treated with Insula are filtered

Placebo = data %>%
  filter (Treatment_Encoded == 0) %>%
  select(SUBJID,HB_W0,HB_W26)
Placebo['bashb.0.26'] = Placebo['HB_W26'] - Placebo['HB_W0'] # Subjects treated with Placebo are filtered

summary(Insula)
summary(Placebo)

#Based on data summary, we could notice that mean change in Insula > mean change in Placebo

hist(x = Insula$bashb.0.26) 
hist(x = Placebo$bashb.0.26)

shapiro.test(x = Insula$bashb.0.26) #Normality accepted at 5% level of significance
shapiro.test(x = Placebo$bashb.0.26) #Normality accepted at 5% level of significance

t.test(Insula$bashb.0.26,Placebo$bashb.0.26) #Mean tested and significant difference noticed

prop.test(x = c(nrow(Insula[Insula$HB_W0>6.5 & Insula$HB_W26<6.5,]),nrow(Placebo[Placebo$HB_W0>6.5 & Placebo$HB_W26<6.5,])), n = c(nrow(Insula),nrow(Placebo)))
#proportions were tested and found that there is a significant difference in proportion

# Defining variance test --------------------------------------------------

var.test <- function(x, y) {
  # Check if y is valid (i.e., not zero or negative)
  if (y <= 0) {
    stop("The known standard deviation (y) must be greater than zero.")
  }
  
  # Calculate sample variance and sample size
  n <- length(x)
  sample_variance <- var(x)
  
  # Known variance based on standard deviation y
  known_variance <- y^2
  
  # Compute chi-square statistic
  chi_sq_statistic <- (n - 1) * sample_variance / known_variance
  
  # Print sample variance and standard deviation
  print(paste("Sample Variance:", sample_variance))
  print(paste("Sample Standard Deviation:", sqrt(sample_variance)))
  
  # Compute p-value
  p_value <- 2 * min(pchisq(chi_sq_statistic, df = n - 1), 1 - pchisq(chi_sq_statistic, df = n - 1))
  
  # Print results
  if (p_value <= 0.05) {
    cat("Reject H0, p-value:", p_value, "\n")
  } else {
    cat("Accept H0, p-value:", p_value, "\n")
  }
}

# SF-36 analysis ---------------------------------------------------

Insula_sf36 = read.csv(file.choose()) #INSULA_SF_36.csv
Placebo_sf36 = read.csv((file.choose())) #sf_36_placebo.csv

View(Insula_sf36)

summary(Insula_sf36)

shapiro.test(Insula_sf36$PF_WK_0) #Normality satisfied
t.test(Insula_sf36$PF_WK_0,mu = 47.40) #null accepted
var.test(Insula_sf36$PF_WK_0,9.15) #Accepted

shapiro.test(Insula_sf36$PF_WK_26) #Normality Satisfied 
t.test(Insula_sf36$PF_WK_26,mu = 53.61) #null accepted 
var.test(Insula_sf36$PF_WK_26,8.72) #Accepted

shapiro.test(Insula_sf36$BP_WK_0) #Normality Satisfied
t.test(Insula_sf36$BP_WK_0, mu = 52.05) #null accepted
var.test(Insula_sf36$BP_WK_0,11.34) #Accepted

shapiro.test(Insula_sf36$BP_WK_26) #Normality Satisfied
t.test(Insula_sf36$BP_WK_26, mu = 45.10) #null accepted
var.test(Insula_sf36$BP_WK_26,12.71) #Accepted

shapiro.test(Insula_sf36$MH_WK_0) #Normality Satisfied
t.test(Insula_sf36$MH_WK_0,mu = 48.93) #Null accepted
var.test(Insula_sf36$MH_WK_0,8.78) #Null accepted

shapiro.test(Insula_sf36$MH_WK_26) #Normality satisfied
t.test(Insula_sf36$MH_WK_26,mu = 47.3) #Null accepted
var.test(Insula_sf36$MH_WK_26,10.23) #Null accepted

summary(Placebo_sf36)

shapiro.test(Placebo_sf36$Pf_W0) #Normality Satisfied
t.test(Placebo_sf36$Pf_W0,mu = 46.96) #Null accepted
var.test(Placebo_sf36$Pf_W0,8.62) #Null accepted

shapiro.test(Placebo_sf36$Pf_W26) #Normality Satisfied
t.test(Placebo_sf36$Pf_W26,mu = 46.74) #Null accepted
var.test(Placebo_sf36$Pf_W26,10.32) #Null accepted

shapiro.test(Placebo_sf36$Bp_W0) #Normality Satisfied
t.test(Placebo_sf36$Bp_W0,mu = 47.13) #Null accepted
var.test(Placebo_sf36$Bp_W0,10.57) #Null accepted

shapiro.test(Placebo_sf36$Bp_W26) #Normality Satisfied
t.test(Placebo_sf36$Bp_W26,mu = 46.19) #Null accepted
var.test(Placebo_sf36$Bp_W26,12.58) #Accepted

shapiro.test(Placebo_sf36$Mh_W0) #Normality Satisfied
t.test(Placebo_sf36$Mh_W0,mu = 48.39) #Null accepted
var.test(Placebo_sf36$Mh_W0,10.53) #Accepted

shapiro.test(Placebo_sf36$Mh_W26) #Normality Satisfied
t.test(Placebo_sf36$Mh_W26,mu = 49.18) #Null accepted
var.test(Placebo_sf36$Mh_W26,11.65) #Accepted


# Insula sf-36 comparisons ------------------------------------------------

t.test(Insula_sf36$PF_WK_0,Insula_sf36$PF_WK_26) #Null rejected at 5%
t.test(Insula_sf36$BP_WK_0,Insula_sf36$BP_WK_26) #Null rejected at 5%
t.test(Insula_sf36$MH_WK_0,Insula_sf36$MH_WK_26) #Null accepted


# Insula vs Placebo comparison --------------------------------------------

t.test((Insula_sf36$PF_WK_26-Insula_sf36$PF_WK_0),(Placebo_sf36$Pf_W26-Placebo_sf36$Pf_W0))
t.test((Insula_sf36$BP_WK_26-Insula_sf36$BP_WK_0),(Placebo_sf36$Bp_W26-Placebo_sf36$Bp_W0))
t.test((Insula_sf36$MH_WK_26-Insula_sf36$MH_WK_0),(Placebo_sf36$Mh_W26-Placebo_sf36$Mh_W0))

