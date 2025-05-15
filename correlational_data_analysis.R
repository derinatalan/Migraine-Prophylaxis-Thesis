### ───────────────────────
# Load libraries
### ───────────────────────
install.packages("ggpubr")
library(ggplot2)
library(dplyr)
library(readr)
library(ggpubr)
library(metafor)
library(forcats)

### ───────────────────────
# Load data and inspect
### ───────────────────────
protocol_data <- read_csv("~/Data_Files/protocol_data.csv")
mmd_data <- read_csv("~/Data_Files/mmd_data.csv")
mhd_data <- read_csv("~/Data_Files/mhd_data.csv")
amd_data <- read_csv("~/Data_Files/amd_data.csv")

# Clean protocol_data by removing rows with NA values in relevant columns
clean_protocol <- protocol_data %>%
  filter(!is.na(MMD_Baseline), !is.na(AMD_Baseline), !is.na(MHD_Baseline))

### ───────────────────────────────────────────────
# Correlation Plot 1: Baseline MMD vs AMD

ggplot(clean_protocol, aes(x = MMD_Baseline, y = AMD_Baseline)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  stat_cor(method = "pearson", 
           label.x = min(clean_protocol$MMD_Baseline, na.rm = TRUE), 
           label.y = max(clean_protocol$AMD_Baseline, na.rm = TRUE),
           size = 4,
           r.digits = 2,
           p.digits = 3,
           label.sep = ", ") +  # Makes the label look like "r = ..., p = ..."
  labs(title = "Baseline MMD vs AMD", x = "Baseline MMD", y = "Baseline AMD") +
  theme_minimal()

### ───────────────────────────────────────────────
# Correlation Plot 2: Baseline MHD vs AMD
ggplot(clean_protocol, aes(x = MHD_Baseline, y = AMD_Baseline)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  stat_cor(method = "pearson", 
           label.x = min(clean_protocol$MHD_Baseline, na.rm = TRUE), 
           label.y = max(clean_protocol$AMD_Baseline, na.rm = TRUE),
           size = 4,
           r.digits = 2,
           p.digits = 3,
           label.sep = ", ") +
  labs(title = "Baseline MHD vs AMD", x = "Baseline MHD", y = "Baseline AMD") +
  theme_minimal()

### ───────────────────────────────────────────────
# Correlation Plot 3: Baseline MHD vs MMD
ggplot(clean_protocol, aes(x = MHD_Baseline, y = MMD_Baseline)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  stat_cor(method = "pearson", 
           label.x = min(clean_protocol$MHD_Baseline, na.rm = TRUE), 
           label.y = max(clean_protocol$MMD_Baseline, na.rm = TRUE),
           size = 4,
           r.digits = 2,
           p.digits = 3,
           label.sep = ", ") + 
  labs(title = "Baseline MHD vs MMD", x = "Baseline MHD", y = "Baseline MMD") +
  theme_minimal()

### ───────────────────────────────────────────────
#Correlation Plot 4: Baseline MMD vs Change in MMD

clean_mmd <- protocol_data %>%
  filter(!is.na(MMD_Baseline), !is.na(Change_MMD))

ggplot(clean_mmd, aes(x = Change_MMD, y = MMD_Baseline)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  stat_cor(method = "pearson", 
           label.x = min(clean_mmd$Change_MMD, na.rm = TRUE), 
           label.y = max(clean_mmd$MMD_Baseline, na.rm = TRUE),
           size = 4,
           r.digits = 2,
           p.digits = 3,
           label.sep = ", ") +
  labs(title = "Baseline MMD vs Change in MMD", x = "Change in MMD", y = "Baseline MMD") +
  theme_minimal()

### ───────────────────────────────────────────────
#Correlation Plot 5: Baseline MHD vs Change in MHD

clean_mhd <- protocol_data %>%
  filter(!is.na(MHD_Baseline), !is.na(Change_MHD))

ggplot(clean_mhd, aes(x = Change_MHD, y = MHD_Baseline)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  stat_cor(method = "pearson", 
           label.x = min(clean_mhd$Change_MHD, na.rm = TRUE), 
           label.y = max(clean_mhd$MHD_Baseline, na.rm = TRUE),
           size = 4,
           r.digits = 2,
           p.digits = 3,
           label.sep = ", ") +
  labs(title = "Baseline MHD vs Change in MHD", x = "Change in MHD", y = "Baseline MHD") +
  theme_minimal()

### ───────────────────────────────────────────────
#Correlation Plot 6: Baseline AMD vs Change in AMD

clean_amd <- protocol_data %>%
  filter(!is.na(AMD_Baseline), !is.na(Change_AMD))

ggplot(clean_amd, aes(x = Change_AMD, y = AMD_Baseline)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  stat_cor(method = "pearson", 
           label.x = min(clean_amd$Change_AMD, na.rm = TRUE), 
           label.y = max(clean_amd$AMD_Baseline, na.rm = TRUE),
           size = 4,
           r.digits = 2,
           p.digits = 3,
           label.sep = ", ") +
  labs(title = "Baseline AMD vs Change in AMD", x = "Change in AMD", y = "Baseline AMD") +
  theme_minimal()

### ───────────────────────────────────────────────
# Correlation Plot 7: Baseline MMD vs Percent Response

clean_response <- protocol_data %>%
  filter(!is.na(Percent_Response), !is.na(MMD_Baseline))

ggplot(clean_response, aes(x = MMD_Baseline, y = Percent_Response)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  stat_cor(method = "pearson", 
           label.x = min(clean_response$MMD_Baseline, na.rm = TRUE), 
           label.y = max(clean_response$Percent_Response, na.rm = TRUE),
           size = 4,
           r.digits = 2,
           p.digits = 3,
           label.sep = ", ") +  
  labs(title = "Baseline MMD vs Percent Response", x = "Baseline MMD", y = "Percent Response") +
  theme_minimal()

### ───────────────────────────────────────────────
# Statistical Test: D_MMD by Migraine Type

mmd_migraine_type <- protocol_data %>%
  filter(!is.na(Change_MMD), !is.na(MMD_Baseline_SD), !is.na(Migraine_Type)) %>%
  mutate(
    yi = Change_MMD,
    vi = (MMD_Baseline_SD^2) / Sample_Size
  )

unique(mmd_migraine_type$Migraine_Type)

res_migraine_type <- rma(yi = yi, vi = vi, mods = ~ Migraine_Type, data = mmd_migraine_type)
summary(res_migraine_type)

### ───────────────────────────────────────────────
# Statistical Test: D_MMD by Intensity of NIBS

# Remove rows with missing frequency and change in MMD
mmd_frequency <- protocol_data %>%
  filter(!is.na(Frequency), !is.na(Change_MMD), !is.na(MMD_Baseline_SD), !is.na(Sample_Size)) %>%
  mutate(
    Frequency_Group = ifelse(Frequency <= median(Frequency, na.rm = TRUE), "Low", "High"),
    yi = Change_MMD,
    vi = (MMD_Baseline_SD^2) / Sample_Size
  )

res_frequency <- rma(yi = yi, vi = vi, mods = ~ Frequency_Group, data = mmd_frequency)
summary(res_frequency)

# Extract fixed effects estimates
coef_data <- data.frame(
  Term = rownames(res_frequency$b),
  Estimate = as.numeric(res_frequency$b),
  CI_Lower = res_frequency$ci.lb,
  CI_Upper = res_frequency$ci.ub
)

# Extract coefficients
intercept <- as.numeric(res_frequency$b[1])
high_coef <- as.numeric(res_frequency$b[2])

# Combine into a data frame of group estimates
group_estimates <- data.frame(
  Frequency_Group = c("Low", "High"),
  Estimate = c(intercept, intercept + high_coef)
)
# Extract variance-covariance matrix
vcov_matrix <- vcov(res_frequency)

# Calculate standard errors
se_low <- sqrt(vcov_matrix[1, 1])
se_high <- sqrt(vcov_matrix[1, 1] + vcov_matrix[2, 2] + 2 * vcov_matrix[1, 2])

# 95% Confidence Intervals
z <- qnorm(0.975)

group_estimates$CI_Lower <- group_estimates$Estimate - z * c(se_low, se_high)
group_estimates$CI_Upper <- group_estimates$Estimate + z * c(se_low, se_high)

ggplot(group_estimates, aes(x = Frequency_Group, y = Estimate)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Pooled Change in MMD by Frequency Group",
    x = "Frequency Group",
    y = "Effect Size (Change in MMD)"
  ) +
  theme_minimal()

median_freq <- median(protocol_data$Frequency, na.rm = TRUE)
print(median(protocol_data$Frequency, na.rm = TRUE))

### ───────────────────────────────────────────────
# Statistical Test: D_MMD by Drug Class
mmd_drug_class <- protocol_data %>%
  filter(!is.na(Drug_Class), !is.na(Change_MMD), !is.na(MMD_Baseline_SD), !is.na(Sample_Size)) %>%
  mutate(
    yi = Change_MMD,
    vi = (MMD_Baseline_SD^2) / Sample_Size
  )

res_drug_class <- rma(yi = yi, vi = vi, mods = ~ Drug_Class, data = mmd_drug_class)
summary(res_drug_class)

# Get estimates
intercept <- as.numeric(res_drug_class$b[1])
coefs <- res_drug_class$b[-1]

# Names of drug classes
class_names <- gsub("Drug_Class", "", rownames(res_drug_class$b)[-1])

# Full list of all classes with their estimated pooled effect
drug_class_estimates <- data.frame(
  Drug_Class = c(rownames(res_drug_class$b)[1], class_names),
  Estimate = c(intercept, intercept + coefs)
)
vcov_matrix <- vcov(res_drug_class)
z <- qnorm(0.975)

# Standard errors
se_list <- c(
  sqrt(vcov_matrix[1, 1]),  # for intercept
  sapply(2:length(res_drug_class$b), function(i) {
    sqrt(vcov_matrix[1, 1] + vcov_matrix[i, i] + 2 * vcov_matrix[1, i])
  })
)

# CI calculation
drug_class_estimates$CI_Lower <- drug_class_estimates$Estimate - z * se_list
drug_class_estimates$CI_Upper <- drug_class_estimates$Estimate + z * se_list

coef_data_filtered <- coef_data %>%
  filter(Term != "(Intercept)") %>%
  mutate(
    Drug_Class = gsub("Drug_Class", "", Term),
    Drug_Class = factor(Drug_Class)  # optional: reorder for plotting
  )

# Reorder so that the first factor level comes last
drug_class_estimates$Drug_Class <- fct_relevel(drug_class_estimates$Drug_Class, 
                                               c(levels(drug_class_estimates$Drug_Class)[-1], levels(drug_class_estimates$Drug_Class)[1]))

 # Plot
ggplot(drug_class_estimates, aes(x = Drug_Class, y = Estimate)) +
  geom_point(size = 3, color = "tomato") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Pooled Change in MMD by Drug Class",
    x = "Drug Class",
    y = "Effect Size (Change in MMD)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### ───────────────────────────────────────────────
# Statistical Test: D_MMD by Montage

# Filter the dataset to remove rows where 'Montage' is NA
mmd_montage_data <- protocol_data %>%
  filter(!is.na(Montage), !is.na(Change_MMD), !is.na(MMD_Baseline_SD), !is.na(Sample_Size))

# Group data by 'Montage' and calculate pooled (mean) Change_MMD for each Montage
pooled_montage_data <- mmd_montage_data %>%
  filter(!is.na(Montage), !is.na(Change_MMD)) %>%
  group_by(Montage) %>%
  summarise(
    pooled_change_mmd = mean(Change_MMD),
    se_change_mmd = sd(Change_MMD) / sqrt(n())
  )

# Replace NA with the maximum standard error (from valid rows)
max_se <- max(pooled_montage_data$se_change_mmd, na.rm = TRUE)
pooled_montage_data_filled <- pooled_montage_data %>%
  mutate(se_change_mmd = ifelse(is.na(se_change_mmd), max_se, se_change_mmd))

# View the resulting data
head(pooled_montage_data)

# Replace NA standard errors with 0 and filter out NA values in Change_MMD and Montage
pooled_montage_data_clean <- pooled_montage_data %>%
  filter(!is.na(Montage), !is.na(pooled_change_mmd)) %>%  # Remove rows with NA in Montage or Change_MMD
  mutate(se_change_mmd = ifelse(is.na(se_change_mmd), 0, se_change_mmd))  # Replace NA in SE with 0

head(pooled_montage_data_clean)

pooled_montage_data_clean$Montage <- fct_relevel(pooled_montage_data_clean$Montage, 
                                                 levels(pooled_montage_data_clean$Montage)[-1], 
                                                 levels(pooled_montage_data_clean$Montage)[1])

ggplot(pooled_montage_data_clean, aes(x = Montage, y = pooled_change_mmd)) +
  geom_point(size = 3, color = "tomato") +  
  geom_errorbar(aes(ymin = pooled_change_mmd - se_change_mmd, ymax = pooled_change_mmd + se_change_mmd), width = 0.2) +  # Error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(
    title = "Pooled Change in MMD by Montage",
    x = "Montage",
    y = "Pooled Effect Size (Change in MMD)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Perform a t-test for each montage to test if the pooled effect size is significantly different from 0
pooled_montage_data_clean <- pooled_montage_data_clean %>%
  mutate(
    t_stat = ifelse(se_change_mmd == 0, NA, pooled_change_mmd / se_change_mmd),  # Avoid division by zero
    p_value = ifelse(se_change_mmd == 0, NA, 2 * (1 - pt(abs(t_stat), df = n() - 1)))  # Avoid p-value for 0 SE
  )

print(pooled_montage_data_clean)

### ───────────────────────────────────────────────
# Statistical Test: D_MMD by Number of Sessions

# Filter the dataset to remove rows where 'No_Sessions' is NA
mmd_No_Sessions_data <- protocol_data %>%
  filter(!is.na(No_Sessions), !is.na(Change_MMD), !is.na(MMD_Baseline_SD), !is.na(Sample_Size))

# Group by both No_Sessions and Group, then calculate the pooled effect size
pooled_No_Sessions_data <- mmd_No_Sessions_data %>%
  filter(!is.na(No_Sessions), !is.na(Change_MMD)) %>%
  group_by(No_Sessions, Group) %>%
  summarise(
    pooled_change_mmd = mean(Change_MMD),
    se_change_mmd = sd(Change_MMD) / sqrt(n())
  ) %>%
  ungroup()

# Replace NA with the maximum standard error (from valid rows)
max_se <- max(pooled_No_Sessions_data$se_change_mmd, na.rm = TRUE)
pooled_No_Sessions_data_filled <- pooled_No_Sessions_data %>%
  mutate(se_change_mmd = ifelse(is.na(se_change_mmd), max_se, se_change_mmd))

# Replace NA standard errors with 0 and filter out NA values in Change_MMD and No_Sessions
pooled_No_Sessions_data_clean <- pooled_No_Sessions_data %>%
  filter(!is.na(No_Sessions), !is.na(pooled_change_mmd)) %>%  # Remove rows with NA in No_Sessions or Change_MMD
  mutate(se_change_mmd = ifelse(is.na(se_change_mmd), 0, se_change_mmd))  # Replace NA in SE with 0

# Relevel 'No_Sessions' to move the first data point to the last (right-most) position on the plot
pooled_No_Sessions_data_clean$No_Sessions <- fct_relevel(pooled_No_Sessions_data_clean$No_Sessions, 
                                                       levels(pooled_No_Sessions_data_clean$No_Sessions)[-1], 
                                                       levels(pooled_No_Sessions_data_clean$No_Sessions)[1])

# Filter the data into two groups: "pharma" and "nibs"
pharma_data <- pooled_No_Sessions_data_clean %>%
  filter(Group == "Pharma")

nibs_data <- pooled_No_Sessions_data_clean %>%
  filter(Group == "NIBS")

# Plot for "pharma" group
ggplot(pharma_data, aes(x = No_Sessions, y = pooled_change_mmd)) +
  geom_point(size = 3, color = "tomato") +
  geom_errorbar(aes(ymin = pooled_change_mmd - se_change_mmd, ymax = pooled_change_mmd + se_change_mmd), width = 0.2) +  # Error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(
    title = "Pooled Change in MMD by Number of Days Drug Taken",
    x = "Days Drug is Taken",
    y = "Pooled Effect Size (Change in MMD)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Plot for "nibs" group
ggplot(nibs_data, aes(x = No_Sessions, y = pooled_change_mmd)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = pooled_change_mmd - se_change_mmd, ymax = pooled_change_mmd + se_change_mmd), width = 0.2) +  # Error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(
    title = "Pooled Change in MMD by Number of NIBS Sessions",
    x = "Number of Sessions",
    y = "Pooled Effect Size (Change in MMD)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

### ───────────────────────────────────────────────
# Statistical Test: D_MMD by tDCS Polarity
tdcs_data <- protocol_data %>%
  filter(Intervention %in% c("a-tDCS", "c-tDCS"), !is.na(Change_MMD))

tdcs_data %>%
  group_by(Intervention) %>%
  summarise(
    mean_change_mmd = mean(Change_MMD, na.rm = TRUE),
    sd_change_mmd = sd(Change_MMD, na.rm = TRUE),
    n = n()
  )

t_test_result <- t.test(Change_MMD ~ Intervention, data = tdcs_data)
print(t_test_result)

ggplot(tdcs_data, aes(x = Intervention, y = Change_MMD, fill = Intervention)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(
    title = "Change in MMD by tDCS Polarity",
    x = "Intervention Type",
    y = "Change in Monthly Migraine Days (MMD)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("a-tDCS" = "blue", "c-tDCS" = "red"))

### ───────────────────────────────────────────────
# Statistical test: D_MMD by tDCS Montage
pooled_data <- tdcs_data %>%
  select(Intervention, Montage, Sample_Size, Change_MMD) %>%  # Select relevant columns
  filter(!is.na(Intervention), !is.na(Montage), !is.na(Sample_Size), !is.na(Change_MMD)) %>%  # Remove missing data
  mutate(
    SE_Change_MMD = ifelse(n() > 1, sd(Change_MMD, na.rm = TRUE) / sqrt(Sample_Size), # Normal SE calculation for n > 1
                           sd(Change_MMD, na.rm = TRUE) / sqrt(Sample_Size)),  # For single data point, still calculate from sample size
    Intervention_Montage = paste(Intervention, Montage, sep = "-")  # Combine Intervention and Montage
  )

# Pool the data based on the combined Intervention-Montage variable
pooled_data_grouped <- pooled_data %>%
  group_by(Intervention_Montage) %>%  # Group by the new combined Intervention-Montage
  summarise(
    pooled_change_mmd = mean(Change_MMD),  # Mean of Change_MMD
    se_change_mmd = sd(Change_MMD) / sqrt(n())  # Standard error of the mean
  )

# Fill missing SE values from studies
pooled_data_grouped <- pooled_data_grouped %>%
  mutate(
    se_change_mmd = case_when(
      Intervention_Montage == "c-tDCS-C3" ~ 1.08,
      Intervention_Montage == "c-tDCS-S1" ~ 1.17,
      Intervention_Montage == "c-tDCS-F8+C4" ~ 2.00,
      Intervention_Montage == "c-tDCS-O2+C3" ~ 1.8,
      Intervention_Montage == "a-tDCS-O2+C3" ~ 0.75,
      Intervention_Montage == "a-tDCS-O1" ~ 0.5,
      Intervention_Montage == "a-tDCS-F8+C4" ~ 0.5,
      TRUE ~ 1.2  # Default value if no specific match, can change it to any other constant
    )
  )

print(pooled_data_grouped)

# Plot the pooled data with error bars
ggplot(pooled_data_grouped, aes(x = Intervention_Montage, y = pooled_change_mmd)) +
  geom_point(size = 3, color = "blue") +  # Points for pooled effect size with steelblue color
  geom_errorbar(aes(ymin = pooled_change_mmd - se_change_mmd, ymax = pooled_change_mmd + se_change_mmd), 
                width = 0.2, color = "black") +  # Error bars in black
  labs(
    title = "Pooled Change in MMD by tDCS Montage",
    x = "tDCS Montage",
    y = "Pooled Effect Size (Change in MMD)"
  ) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
