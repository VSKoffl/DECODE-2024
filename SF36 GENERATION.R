
# INSULA SF 36 GENERATION -------------------------------------------------
data<-read.csv(file.choose()) #imputed_m_final.csv
attach(data)
View(data)
library(dplyr)
# insula sf36 pf week0 ----------------------------------------------------

insula_data<-data %>%
  filter(data$Treatment_Encoded== 1) %>%
  select(SUBJID,HB_W0)

mu2<-mean(insula_data$HB_W0)
sd2<-sd(insula_data$HB_W0)
sd2
mu1<-47.4
sd1<-9.15
rho=-0.21
x2<-insula_data$HB_W0

mu_cond <- mu1 + rho * (sd1 /sd2) * (x2 - mu2)
sigma_cond <- sd1 * sqrt(1 - rho^2)

sample_size <- 41
conditional_samples1<- rnorm(sample_size, mean = mu_cond, sd = sigma_cond)
cor(insula_data$HB_W0,conditional_samples1)

# Insula sf36 pf week 26 --------------------------------------------------

insula_data<-data %>%
  filter(data$Treatment_Encoded== 1) %>%
  select(SUBJID,HB_W26)

mu2<-mean(insula_data$HB_W26)
sd2<-sd(insula_data$HB_W26)
sd2
mu1<-53.61
sd1<-8.72
rho=-0.21
x2<-insula_data$HB_W26


mu_cond <- mu1 + rho * (sd1 /sd2) * (x2 - mu2)
sigma_cond <- sd1 * sqrt(1 - rho^2)


sample_size <- 41
conditional_samples2<- rnorm(sample_size, mean = mu_cond, sd = sigma_cond)
cor(insula_data$HB_W26,conditional_samples2)
# insula sf 36 bp week0 ---------------------------------------------------

insula_data<-data %>%
  filter(data$Treatment_Encoded== 1) %>%
  select(SUBJID,HB_W0)

mu2<-mean(insula_data$HB_W0)
sd2<-sd(insula_data$HB_W0)
sd2
mu1<-52.05
sd1<-11.34
rho=0.169
x2<-insula_data$HB_W0

mu_cond <- mu1 + rho * (sd1 /sd2) * (x2 - mu2)
sigma_cond <- sd1 * sqrt(1 - rho^2)

sample_size <- 41
conditional_samples3<- rnorm(sample_size, mean = mu_cond, sd = sigma_cond)

cor(insula_data$HB_W0,conditional_samples3)


# insula sf36 bp week 26 ---------------------------------------------------

insula_data<-data %>%
  filter(data$Treatment_Encoded== 1) %>%
  select(SUBJID,HB_W26)

mu2<-mean(insula_data$HB_W26)
sd2<-sd(insula_data$HB_W26)
sd2
mu1<-45.10
sd1<-12.71
rho=0.169
x2<-insula_data$HB_W26

mu_cond <- mu1 + rho * (sd1 /sd2) * (x2 - mu2)
sigma_cond <- sd1 * sqrt(1 - rho^2)

sample_size <- 41
conditional_samples4<- rnorm(sample_size, mean = mu_cond, sd = sigma_cond)

cor(insula_data$HB_W26,conditional_samples4)


# insula sf36 mh week0 ----------------------------------------------------

insula_data<-data %>%
  filter(data$Treatment_Encoded== 1) %>%
  select(SUBJID,HB_W0)

mu2<-mean(insula_data$HB_W0)
sd2<-sd(insula_data$HB_W0)
sd2
mu1<-48.93
sd1<-8.78
rho=0.07
x2<-insula_data$HB_W0

mu_cond <- mu1 + rho * (sd1 /sd2) * (x2 - mu2)
sigma_cond <- sd1 * sqrt(1 - rho^2)

sample_size <- 41
conditional_samples5<- rnorm(sample_size, mean = mu_cond, sd = sigma_cond)

cor(insula_data$HB_W0,conditional_samples5)

# insula sf36 mh week 26 --------------------------------------------------

insula_data<-data %>%
  filter(data$Treatment_Encoded== 1) %>%
  select(SUBJID,HB_W26)

mu2<-mean(insula_data$HB_W26)
sd2<-sd(insula_data$HB_W26)
sd2
mu1<-47.3
sd1<-10.23
rho=0.07
x2<-insula_data$HB_W26


mu_cond <- mu1 + rho * (sd1 /sd2) * (x2 - mu2)
sigma_cond <- sd1 * sqrt(1 - rho^2)

sample_size <- 41
conditional_samples6<- rnorm(sample_size, mean = mu_cond, sd = sigma_cond)

cor(insula_data$HB_W26,conditional_samples6)

# PLACEBO SF36  GENERATION ------------------------------------------------


# placebo sf36 pf week 0 --------------------------------------------------


placebo_data<-data %>%
  filter(data$Treatment_Encoded== 0) %>%
  select(SUBJID,HB_W0)

mu2<-mean(placebo_data$HB_W0)
sd2<-sd(placebo_data$HB_W0)
sd2
mu1<-46.96
sd1<-8.62
rho=-0.05
x2<-placebo_data$HB_W0

mu_cond <- mu1 + rho * (sd1 /sd2) * (x2 - mu2)
sigma_cond <- sd1 * sqrt(1 - rho^2)

sample_size <- 39
conditional_samples7<- rnorm(sample_size, mean = mu_cond, sd = sigma_cond)

cor(placebo_data$HB_W0,conditional_samples7)


# placebo  sf36 pf week 26 ------------------------------------------------

placebo_data<-data %>%
  filter(data$Treatment_Encoded== 0) %>%
  select(SUBJID,HB_W26)

mu2<-mean(placebo_data$HB_W26)
sd2<-sd(placebo_data$HB_W26)
sd2
mu1<-46.74 
sd1<-10.32
rho=-0.05
x2<-placebo_data$HB_W26

mu_cond <- mu1 + rho * (sd1 /sd2) * (x2 - mu2)
sigma_cond <- sd1 * sqrt(1 - rho^2)

sample_size <- 39
conditional_samples8<- rnorm(sample_size, mean = mu_cond, sd = sigma_cond)

cor(placebo_data$HB_W26,conditional_samples8)


# placebo sf36 bp week 0 --------------------------------------------------

placebo_data<-data %>%
  filter(data$Treatment_Encoded== 0) %>%
  select(SUBJID,HB_W0)

mu2<-mean(placebo_data$HB_W0)
sd2<-sd(placebo_data$HB_W0)
sd2
mu1<-47.13 
sd1<-10.57
rho=0.06
x2<-placebo_data$HB_W0

mu_cond <- mu1 + rho * (sd1 /sd2) * (x2 - mu2)
sigma_cond <- sd1 * sqrt(1 - rho^2)

sample_size <- 39
conditional_samples9<- rnorm(sample_size, mean = mu_cond, sd = sigma_cond)

cor(placebo_data$HB_W0,conditional_samples9)


# placebo sf36 bp week26  -------------------------------------------------

placebo_data<-data %>%
  filter(data$Treatment_Encoded== 0) %>%
  select(SUBJID,HB_W26)

mu2<-mean(placebo_data$HB_W26)
sd2<-sd(placebo_data$HB_W26)
sd2
mu1<-46.19
sd1<-12.58
rho=0.06
x2<-placebo_data$HB_W26

mu_cond <- mu1 + rho * (sd1 /sd2) * (x2 - mu2)
sigma_cond <- sd1 * sqrt(1 - rho^2)

sample_size <- 39
conditional_samples10<- rnorm(sample_size, mean = mu_cond, sd = sigma_cond)

cor(placebo_data$HB_W26,conditional_samples10)


# placebo sf36 mh week 0 --------------------------------------------------

placebo_data<-data %>%
  filter(data$Treatment_Encoded== 0) %>%
  select(SUBJID,HB_W0)

mu2<-mean(placebo_data$HB_W0)
sd2<-sd(placebo_data$HB_W0)
sd2
mu1<-48.39  
sd1<-10.53
rho=-0.06
x2<-placebo_data$HB_W0

mu_cond <- mu1 + rho * (sd1 /sd2) * (x2 - mu2)
sigma_cond <- sd1 * sqrt(1 - rho^2)

sample_size <- 39
conditional_samples11<- rnorm(sample_size, mean = mu_cond, sd = sigma_cond)

cor(placebo_data$HB_W0,conditional_samples11)


# placebo sf36 mh week26 --------------------------------------------------

placebo_data<-data %>%
  filter(data$Treatment_Encoded== 0) %>%
  select(SUBJID,HB_W26)

mu2<-mean(placebo_data$HB_W26)
sd2<-sd(placebo_data$HB_W26)
sd2
mu1<-49.18
sd1<-11.65
rho=-0.06
x2<-placebo_data$HB_W26

mu_cond <- mu1 + rho * (sd1 /sd2) * (x2 - mu2)
sigma_cond <- sd1 * sqrt(1 - rho^2)

sample_size <- 39
conditional_samples12<- rnorm(sample_size, mean = mu_cond, sd = sigma_cond)

cor(placebo_data$HB_W26,conditional_samples12)






