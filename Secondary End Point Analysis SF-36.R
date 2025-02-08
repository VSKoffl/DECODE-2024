# Define clinically meaningful change
clinically_meaningful_change <- 5
library(dplyr)
# Identify responders
insula_data <- read.csv(file.choose()) #INSULA_SF_36.csv
placebo_data <- read.csv(file.choose()) #sf_36_placebo.csv

insula_data <- insula_data %>%
  mutate(Responder = (insula_data$PF_WK_26-insula_data$PF_WK_0) >= clinically_meaningful_change)

placebo_data <- placebo_data %>%
  mutate(Responder = (placebo_data$Pf_W26-placebo_data$Pf_W0) >= clinically_meaningful_change)

# Calculate proportions
insula_proportion <- mean(insula_data$Responder)
placebo_proportion <- mean(placebo_data$Responder)

# Combine data for statistical comparison
combined_data <- data.frame(
  Treatment = c(rep("Insula", nrow(insula_data)), rep("Placebo", nrow(placebo_data))),
  Responder = c(insula_data$Responder, placebo_data$Responder)
)

# Perform a chi-square test
chi_square_result <- chisq.test(table(combined_data$Treatment, combined_data$Responder))

# Print results
cat("Proportion of Responders - Insula:", insula_proportion, "\n")
cat("Proportion of Responders - Placebo:", placebo_proportion, "\n")
cat("Chi-Square Test p-value:", chi_square_result$p.value, "\n")


# Install necessary package if not already installed
if(!require(epitools)) install.packages("epitools", dependencies=TRUE)

# Load the package
library(epitools)

# Assuming combined_data is your data frame with columns Treatment and Responder
# Create a 2x2 table of Treatment vs Responder
contingency_table <- table(combined_data$Treatment, combined_data$Responder)

# Calculate Odds Ratio (OR)
odds_ratio_result <- oddsratio(contingency_table, method = "wald")
print(odds_ratio_result)

# Calculate Risk Ratio (RR)
risk_ratio_result <- riskratio(contingency_table)
print(risk_ratio_result)

