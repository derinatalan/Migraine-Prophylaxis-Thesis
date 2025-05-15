### ─────────────────
# Load libraries
### ─────────────────

install.packages("tidyverse")
install.packages("metafor")
install.packages("scico") # color palette
library(tidyverse)
library(metafor)
library(scico)
library(dplyr)
library(tibble)
library(tidyr)

### ──────────────────────────
# Load data and inspect
### ──────────────────────────
mmd_data <- read_csv("~/Data_Files/mmd_data.csv")
glimpse(mmd_data)
summary(mmd_data)

### ──────────────────────────────────────────────────────────────
### Impute missing SDs using median SD for that timepoint
### ──────────────────────────────────────────────────────────────
mmd_data <- mmd_data %>%
  group_by(Timepoint) %>%
  mutate(
    SD_imputed = ifelse(is.na(MMD_SD), median(MMD_SD, na.rm = TRUE), MMD_SD),
    SD_was_imputed = is.na(MMD_SD)  # Flag for sensitivity analysis
  ) %>%
  ungroup()

### ─────────────────────────────────────────────────
### Calculate effect sizes (Mean Difference)
### ─────────────────────────────────────────────────
mmd_es <- escalc(
  measure = "MN", 
  yi = Change_MMD,
  vi = SD_imputed^2 / Sample_Size,
  data = mmd_data
)

### ─────────────────────────────────────
### Run main random-effects model
### ─────────────────────────────────────

# Overall effect size (MMD change across all studies)
res_overall <- rma(yi, vi, data = mmd_es, method = "REML")
summary(res_overall)
# Print I², τ², Q-test (already in summary)
print(res_overall$I2)  # I-squared
print(res_overall$tau2)  # Tau-squared

### ─────────────────────────────────────
### Subgroup analysis
### ─────────────────────────────────────

# NIBS vs Pharma
res_group <- rma(yi, vi, mods = ~ Group, data = mmd_es, method = "REML")
summary(res_group)

# Specific interventions
res_intervention <- rma(yi, vi, mods = ~ 0 + Intervention, data = mmd_es, method = "REML")
summary(res_intervention)

### ──────────────────
### Visualization
### ──────────────────

######### Grouped by Intervention ###############

# Group by Intervention to calculate pooled effect sizes
intervention_summary <- mmd_es %>%
  group_by(Intervention) %>%
  summarise(
    estimate = mean(yi),  # Mean effect size (pooled)
    ci.lb = mean(yi) - 1.96 * sqrt(mean(vi)),  # Lower bound of 95% CI
    ci.ub = mean(yi) + 1.96 * sqrt(mean(vi)),  # Upper bound of 95% CI
    .groups = 'drop'
  )

print(n=30, intervention_summary)

# Plot the forest plot using ggplot2
ggplot(intervention_summary, aes(x = estimate, y = Intervention)) +
  geom_point(size = 3, color = "blue") +  # Effect sizes, colored blue
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.2) +  # CI intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at 0
  theme_minimal() +
  labs(
    title = "Pooled Effect Sizes by Intervention",
    x = "Mean Difference in Monthly Migraine Days",
    y = "Intervention",
    subtitle = "Pooled estimates with 95% Confidence Intervals"
  ) +
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    plot.title = element_text(hjust = 0.5, size = 14),  # Center title
    plot.subtitle = element_text(hjust = 0.5, size = 10)  # Center subtitle
  )

# Violin plot of effect sizes by Group
ggplot(mmd_es, aes(x = Group, y = yi, fill = Group)) +
  geom_violin(trim = FALSE) +  # Violin plot to show distribution
  geom_point(aes(color = Group), position = position_jitter(width = 0.1), size = 2) +  # Points for effect sizes
  labs(
    title = "Distribution of Effect Sizes: Pharma vs. NIBS",
    x = "Treatment Group",
    y = "Effect Size (Mean Difference in Monthly Migraine Days)"
  ) +
  theme_minimal()

#Plot for all at T3
mmd_t3 <- mmd_data_clean %>%
  filter(Timepoint == "T3" & !is.na(Change_MMD) & !is.na(MMD_SD) & !is.na(Sample_Size))

# Calculate effect size (yi) and variance (vi)
mmd_t3 <- mmd_t3 %>%
  mutate(
    yi = Change_MMD,  # Mean difference in MMD
    vi = (MMD_SD^2) / Sample_Size  # Variance
  )

# Perform meta-analysis with Intervention as moderator
res_t3 <- rma(yi, vi, mods = ~ Intervention - 1, data = mmd_t3)  # -1 to get pooled estimate per group

intervention_summary_t3 <- mmd_t3 %>%
  group_by(Intervention) %>%
  summarise(
    estimate = mean(yi),  # Mean effect size (pooled)
    ci.lb = mean(yi) - 1.96 * sqrt(mean(vi)),  # Lower bound of 95% CI
    ci.ub = mean(yi) + 1.96 * sqrt(mean(vi)),  # Upper bound of 95% CI
    .groups = 'drop'
  )

# Plot the forest plot
ggplot(intervention_summary_t3, aes(x = estimate, y = Intervention)) +
  geom_point(size = 3, color = "blue") +  # Effect sizes, colored blue
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.2) +  # CI intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at 0
  theme_minimal() +
  labs(
    title = "Pooled Effect Sizes by Intervention at 12 weeks",
    x = "Mean Difference in Monthly Migraine Days",
    y = "Intervention",
    subtitle = "Pooled estimates with 95% Confidence Intervals"
  ) +
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    plot.title = element_text(hjust = 0.5, size = 14),  # Center title
    plot.subtitle = element_text(hjust = 0.5, size = 10)  # Center subtitle
  )

######### Within NIBS ###############

# Filter data for NIBS interventions
nibs_data <- mmd_es %>% filter(Group == "NIBS")
head(nibs_data)
nibs_res <- rma(yi, vi, mods = ~ Intervention, data = nibs_data)
summary(nibs_res)

# Forest plot comparing NIBS protocols
ggplot(nibs_data, aes(x = yi, y = Intervention)) +
  geom_point(color = "blue", size = 4) +  # Points for effect sizes
  geom_errorbarh(aes(xmin = yi - 1.96 * sqrt(vi), xmax = yi + 1.96 * sqrt(vi)), height = 0.2) +  # CI intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at 0
  labs(
    title = "NIBS Protocols Comparison",
    x = "Mean Difference in Monthly Migraine Days",
    y = "NIBS Intervention",
    subtitle = "Effect Sizes and 95% Confidence Intervals"
  ) +
  theme_minimal()

######### Within Pharma ###############

# Filter data for Pharma group
pharma_data <- mmd_es %>% filter(Group == "Pharma")
pharma_res <- rma(yi, vi, mods = ~ Intervention, data = pharma_data)
summary(pharma_res)

# Forest plot comparing Pharma treatments
ggplot(pharma_data, aes(x = yi, y = Intervention)) +
  geom_point(color = "blue", size = 4) +  # Points for effect sizes
  geom_errorbarh(aes(xmin = yi - 1.96 * sqrt(vi), xmax = yi + 1.96 * sqrt(vi)), height = 0.2) +  # CI intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at 0
  labs(
    title = "Pharma Treatments Comparison",
    x = "Mean Difference in Monthly Migraine Days",
    y = "Pharma Treatment",
    subtitle = "Effect Sizes and 95% Confidence Intervals"
  ) +
  theme_minimal()

### ─────────────────────────────────────
### Statistical Testing
### ─────────────────────────────────────

# Filter for T3 timepoint
t3_data <- mmd_data_clean %>%
  filter(Timepoint == "T3")

# Meta-regression model
res_meta <- rma(yi, vi, mods = ~ Intervention - 1, data = t3_data)  # -1 removes intercept, gives direct est for each intervention

# Extract estimates and variance-covariance matrix
estimates <- coef(res_meta)
vcov_matrix <- vcov(res_meta)

# Get all unique pairwise combinations
interventions <- names(estimates)
pairwise_comparisons <- combn(interventions, 2, simplify = FALSE)

# Calculate pairwise differences, SEs, and p-values
results <- lapply(pairwise_comparisons, function(pair) {
  diff <- estimates[pair[1]] - estimates[pair[2]]
  se_diff <- sqrt(vcov_matrix[pair[1], pair[1]] + vcov_matrix[pair[2], pair[2]] - 
                    2 * vcov_matrix[pair[1], pair[2]])
  z_val <- diff / se_diff
  p_val <- 2 * pnorm(-abs(z_val))
  tibble(
    Comparison = paste(pair[1], "vs", pair[2]),
    Difference = diff,
    SE = se_diff,
    Z = z_val,
    P_value = p_val
  )
}) %>% bind_rows()

# Adjust for multiple comparisons
results <- results %>%
  mutate(P_adj = p.adjust(P_value, method = "BH"))  # or use "bonferroni" if you prefer

# Statistically significant comparisons (p < 0.05)
significant_results <- results %>%
  filter(P_adj < 0.05)
print(significant_results)

# Non-significant comparisons (p ≥ 0.05)
nonsignificant_results <- results %>%
  filter(P_adj >= 0.05)
print(nonsignificant_results)

write.csv(significant_results, "mmd_significant_results.csv", row.names = FALSE)
write.csv(nonsignificant_results, "mmd_nonsignificant_results.csv", row.names = FALSE)

