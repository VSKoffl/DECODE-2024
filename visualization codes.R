####### boxplot for HbA1c change comparison between insula and placebo########

data<-read.csv(file.choose()) #imput_m_final data set
library(ggplot2)
attach(data)
View(data)
data$difference=HB_W26-HB_W0
ggplot(data, aes(x = as.factor(Treatment_Encoded), y =difference, fill = as.factor(Treatment_Encoded))) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16,
               outlier.size = 2, notch = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "blue", fill = "blue") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "blue", linetype = "dashed") +
  labs(title = "HbA1c Change Comparison: Insula  =1 vs Placebo = 0",
       x = "Group",
       y = "Change in HbA1c",
       fill = "Group") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )  

##############proportion chart of Subjects Becoming Diabetic from Pre-Diabetic After 26 Weeks##########

df<-read.csv(file.choose()) #imputed_m_final data set
attach(df)

library(dplyr)
library(ggplot2)

threshold <- 6.5 #puttiung  a threshold value

df <- df %>%
  mutate(
    week26_status = ifelse(HB_W26 >= threshold, 'Diabetic', 'Normal')
  )

status_counts <- df %>%
  group_by(Treatment_Encoded, week26_status) %>%
  summarise(count = n(),.groups = 'drop') %>%
  ungroup()

total_counts <- df %>%
  group_by(Treatment_Encoded) %>%
  summarise(total = n()) %>%
  ungroup()

# Merging status counts with total counts
status_summary <- status_counts %>%
  left_join(total_counts, by = 'Treatment_Encoded') %>%
  mutate(proportion = count / total)

status_summary$week26_status <- factor(status_summary$week26_status, levels = c('Diabetic', 'Normal'))

ggplot(status_summary, aes(x = as.factor(Treatment_Encoded), y = proportion, fill = week26_status)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c('Diabetic' = 'lightblue', 'Normal' = 'brown')) +  # Custom colors
  labs(
    x = 'Treatment',
    y = 'Proportion of Subjects',
    fill = 'Week 26 Status',
    title = 'Proportion of Subjects Becoming Diabetic from Pre-Diabetic After 26 Weeks'
  ) +
  theme_minimal()

############## boxplots for analysis category ############

data1<-read.csv(file.choose()) #adhypo_imputed data set
attach(data1)
library(ggplot2)
library(dplyr)
names(data1)
insula_avail=data1 %>%
  filter(data1$TRT== 'Insula') %>%
  select(ANACAT,GLUCSTD)

#plot for insula
ggplot(insula_avail, aes(x = ANACAT, y = GLUCSTD, fill = ANACAT)) +
  geom_boxplot() +
  labs(title = "Comparison of Glucose Levels by Analysis Category for insula",
       x = "Analysis Category",
       y = "Glucose Level") +
  theme_minimal()
placebo_avail=data1 %>%
  filter(data1$TRT== 'Placebo') %>%
  select(ANACAT,GLUCSTD)

#plot for placebo
ggplot(placebo_avail, aes(x = ANACAT, y = GLUCSTD, fill = ANACAT)) +
  geom_boxplot() +
  labs(title = "Comparison of Glucose Levels by Analysis Category for placebo",
       x = "Analysis Category",
       y = "Glucose Level") +
  theme_minimal()

############# heat map of treatment vs analysis categories##########

library(ggplot2)
adhypo=read.csv(file.choose()) #adhypo_imputed_final dataset
attach(adhypo)
ggplot(adhypo, aes(x = TRT, y = ANACAT, fill = GLUCSTD)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Heatmap of Treatments vs Analysis Categories",
       x = "Treatment",
       y = "Analysis Category",
       fill = "Value")

############# line chart for avreage change of HbA1c over time  by insula############
library(dplyr)
library(tidyr)
library(ggplot2)

data2 <- read.csv(file.choose()) # imputed_m_final data set
attach(data2)
names(data2)

data3 <- data2 %>%
  filter(Treatment_Encoded == 1) %>%
  select(AGE, FP_0, FP_12, FP_26, GENDER_Encoded, HB_12, HB_16, HB_20, HB_W0,
         HB_W26, HB_W4, HB_W8, SUBJID, Treatment_Encoded)

data2_long <- data3 %>%
  pivot_longer(
    cols = starts_with("HB_"),
    names_to = "week",
    values_to = "hba1c"
  ) %>%
  mutate(week = sub("HB_W|HB_", "", week)) %>%
  mutate(week = as.numeric(week))

ggplot(data2_long, aes(x = week, y = hba1c)) +
  geom_line(stat = "summary", fun = median) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 26)) +
  labs(
    x = 'Week',
    y = 'Median HbA1c',
    color = 'black',
    title = 'Median Change in HbA1c Over Time by Insula'
  ) +
  theme_minimal()

# proportion charts for diabetic to prediabetic ---------------------------

df=read.csv(file.choose()) #imputed_m_final.csv
library(dplyr)
attach(df)
df_insula=df %>%
  filter(df$Treatment_Encoded== 1) %>%
  select(SUBJID,HB_W0,HB_W4,HB_W8,HB_12,HB_16,HB_20,HB_W26)
df_insula <- df_insula %>%
  mutate(status_binary1 = ifelse(df_insula$HB_W0> 6.5, 0, ifelse(df_insula$HB_W0<=6.5, 1, NA)))

m1=mean(df_insula$status_binary1)
df_insula <- df_insula %>%
  mutate(status_binary2 = ifelse(df_insula$HB_W4> 6.5, 0, ifelse(df_insula$HB_W4<=6.5, 1, NA)))

m2=mean(df_insula$status_binary2)
df_insula <- df_insula %>%
  mutate(status_binary3 = ifelse(df_insula$HB_W8> 6.5, 0, ifelse(df_insula$HB_W8<=6.5, 1, NA)))

m3=mean(df_insula$status_binary3)
df_insula <- df_insula %>%
  mutate(status_binary4 = ifelse(df_insula$HB_12> 6.5, 0, ifelse(df_insula$HB_12<=6.5, 1, NA)))

m4=mean(df_insula$status_binary4)
df_insula <- df_insula %>%
  mutate(status_binary5 = ifelse(df_insula$HB_16> 6.5, 0, ifelse(df_insula$HB_16<=6.5, 1, NA)))

m5=mean(df_insula$status_binary5)
df_insula <- df_insula %>%
  mutate(status_binary6 = ifelse(df_insula$HB_20> 6.5, 0, ifelse(df_insula$HB_20<=6.5, 1, NA)))

m6=mean(df_insula$status_binary6)
df_insula <- df_insula %>%
  mutate(status_binary7 = ifelse(df_insula$HB_W26> 6.5, 0, ifelse(df_insula$HB_W26<=6.5, 1, NA)))

m7=mean(df_insula$status_binary7)
x1=as.factor(c(0,4,8,12,16,20,26))
y1=c(m1,m2,m3,m4,m5,m6,m7)
plot(x1,y1,... = "p",pch=16,xlab="week",ylab="proportion",main="Proportion of subjects became diabetic to prediabetic by insula")
lines(x1, y1, type = "o", col = "red")


# placebo -----------------------------------------------------------------

df_placebo=df %>%
  filter(df$Treatment_Encoded== 0) %>%
  select(SUBJID,HB_W0,HB_W4,HB_W8,HB_12,HB_16,HB_20,HB_W26)
df_placebo <- df_placebo %>%
  mutate(status_binary8 = ifelse(df_placebo$HB_W0> 6.5, 0, ifelse(df_placebo$HB_W0<=6.5, 1, NA)))

m8=mean(df_placebo$status_binary8)
df_placebo <- df_placebo %>%
  mutate(status_binary9 = ifelse(df_placebo$HB_W4> 6.5, 0, ifelse(df_placebo$HB_W4<=6.5, 1, NA)))

m9=mean(df_placebo$status_binary9)
df_placebo <- df_placebo %>%
  mutate(status_binary10 = ifelse(df_placebo$HB_W8> 6.5, 0, ifelse(df_placebo$HB_W8<=6.5, 1, NA)))

m10=mean(df_placebo$status_binary10)
df_placebo <- df_placebo %>%
  mutate(status_binary11 = ifelse(df_placebo$HB_12> 6.5, 0, ifelse(df_placebo$HB_12<=6.5, 1, NA)))

m11=mean(df_placebo$status_binary11)
df_placebo <- df_placebo %>%
  mutate(status_binary12 = ifelse(df_placebo$HB_16> 6.5, 0, ifelse(df_placebo$HB_16<=6.5, 1, NA)))

m12=mean(df_placebo$status_binary12)
df_placebo <- df_placebo %>%
  mutate(status_binary13 = ifelse(df_placebo$HB_20> 6.5, 0, ifelse(df_placebo$HB_20<=6.5, 1, NA)))

m13=mean(df_placebo$status_binary13)
df_placebo <- df_placebo %>%
  mutate(status_binary14 = ifelse(df_placebo$HB_W26> 6.5, 0, ifelse(df_placebo$HB_W26<=6.5, 1, NA)))

m14=mean(df_placebo$status_binary14)
x2=as.factor(c(0,4,8,12,16,20,26))
y2=c(m8,m9,m10,m11,m12,m13,m14)
plot(x2,y2,xlab="week",ylab="proportion",main="Proportion of subjects became diabetic to prediabetic by placebo")
lines(x2, y2, type = "o", col = "red")

