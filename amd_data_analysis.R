### ─────────────────────────
### Load packages
### ─────────────────────────
install.packages("metafor")
install.packages("dplyr")
library(dplyr)
library(metafor)
library(ggplot2)
library(tibble)
library(tidyr)

### ────────────────────────────────
### Load data and inspect
### ────────────────────────────────
# Load the data (replace with the path to your actual file)
amd_data<- read.csv("~/Data_Files/amd_data.csv")
glimpse(amd_data)
summary(amd_data)

### ──────────────────────────────────────────────────────────────
### Impute missing SDs using median SD for that timepoint
### ──────────────────────────────────────────────────────────────
amd_data <- amd_data %>%
  group_by(Timepoint) %>%
  mutate(
    SD_imputed = ifelse(is.na(AMD_SD), median(AMD_SD, na.rm = TRUE), AMD_SD),
    SD_was_imputed = is.na(AMD_SD)  # Flag for sensitivity analysis
  ) %>%
  ungroup()


### ─────────────────────────────────────────────────
### Impute missing standard error
### ─────────────────────────────────────────────────
amd_es <- escalc(
  measure = "MN", 
  yi = Change_AMD,
  vi = SD_imputed^2 / Sample_Size,
  data = amd_data
)

### ─────────────────────────────────────
### Run main random-effects model
### ─────────────────────────────────────
res_overall <- rma(yi, vi, data = amd_es, method = "REML")
summary(res_overall)
# Print I², τ², Q-test
print(res_overall$I2)  # I-squared
print(res_overall$tau2)  # Tau-squared

### ───────────────────────────
### Subgroup analysis
### ───────────────────────────

# NIBS vs Pharma
res_group <- rma(yi, vi, mods = ~ Group, data = amd_es, method = "REML")
summary(res_group)

# Specific interventions
res_intervention <- rma(yi, vi, mods = ~ Intervention, data = amd_es, method = "REML")
summary(res_intervention)

### ──────────────────────
### Visualization
### ──────────────────────

######### Grouped by Intervention ###############

# Group by Intervention to calculate pooled effect sizes
intervention_summary <- amd_es %>%
  group_by(Intervention) %>%
  summarise(
    estimate = mean(yi),  # Mean effect size (pooled)
    ci.lb = mean(yi) - 1.96 * sqrt(mean(vi)),  # Lower bound of 95% CI
    ci.ub = mean(yi) + 1.96 * sqrt(mean(vi)),  # Upper bound of 95% CI
    .groups = 'drop'
  )

# Plot the forest plot
ggplot(intervention_summary, aes(x = estimate, y = Intervention)) +
  geom_point(size = 3, color = "blue") +  # Effect sizes
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.2) +  # CI intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at 0
  theme_minimal() +
  labs(
    title = "Pooled Effect Sizes by Intervention",
    x = "Mean Difference in Monthly Acute Medication Use",
    y = "Intervention",
    subtitle = "Pooled estimates with 95% Confidence Intervals"
  ) +
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

print(intervention_summary)
# Violin plot of effect sizes by Group
ggplot(amd_es, aes(x = Group, y = yi, fill = Group)) +
  geom_violin(trim = FALSE) + 
  geom_point(aes(color = Group), position = position_jitter(width = 0.1), size = 2) +  # Points for effect sizes
  labs(
    title = "Distribution of Effect Sizes: Pharma vs. NIBS",
    x = "Treatment Group",
    y = "Effect Size (Mean Difference in Monthly Acute Medication Use)"
  ) +
  theme_minimal()

######### Within NIBS ###############

# Filter data for NIBS interventions
nibs_data <- amd_es %>% filter(Group == "NIBS")
head(nibs_data)

# Meta-analysis for each NIBS protocol
nibs_res <- rma(yi, vi, mods = ~ Intervention, data = nibs_data)
summary(nibs_res)

# Forest plot comparing NIBS protocols
ggplot(nibs_data, aes(x = yi, y = Intervention)) +
  geom_point(aes(color = Intervention), size = 4) +  
  geom_errorbarh(aes(xmin = yi - 1.96 * sqrt(vi), xmax = yi + 1.96 * sqrt(vi)), height = 0.2) +  # CI intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at 0
  labs(
    title = "NIBS Protocols Comparison",
    x = "Mean Difference in Monthly Acute Medication Use",
    y = "NIBS Intervention",
    subtitle = "Effect Sizes and 95% Confidence Intervals"
  ) +
  theme_minimal()

######### Within Pharma ###############

# Filter data for Pharma group
pharma_data <- amd_es %>% filter(Group == "Pharma")

# Meta-analysis for Pharma treatments
pharma_res <- rma(yi, vi, mods = ~ Intervention, data = pharma_data)
summary(pharma_res)

# Forest plot comparing Pharma treatments
ggplot(pharma_data, aes(x = yi, y = Intervention)) +
  geom_point(aes(color = Intervention), size = 4) +  
  geom_errorbarh(aes(xmin = yi - 1.96 * sqrt(vi), xmax = yi + 1.96 * sqrt(vi)), height = 0.2) +  # CI intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at 0
  labs(
    title = "Pharma Treatments Comparison",
    x = "Mean Difference in Monthly Acute Medication Use",
    y = "Pharma Treatment",
    subtitle = "Effect Sizes and 95% Confidence Intervals"
  ) +
  theme_minimal()

### ─────────────────────────────────────
### Statistical Testing
### Pooled effect size comparison between different treatments
### ─────────────────────────────────────

# Filter T3
t3_data <- amd_data_clean %>%
  filter(Timepoint == "T3")

# Run meta-analysis per intervention
t3_effects <- t3_data %>%
  group_by(Intervention) %>%
  summarise(
    pooled = rma(yi, vi, method = "REML")$b,
    var = rma(yi, vi, method = "REML")$se^2
  ) %>%
  ungroup()

# Pairwise comparisons
comparison_results <- expand.grid(Intervention1 = t3_effects$Intervention,
                                  Intervention2 = t3_effects$Intervention) %>%
  filter(Intervention1 != Intervention2) %>%
  left_join(t3_effects, by = c("Intervention1" = "Intervention")) %>%
  rename(pooled1 = pooled, var1 = var) %>%
  left_join(t3_effects, by = c("Intervention2" = "Intervention")) %>%
  rename(pooled2 = pooled, var2 = var) %>%
  mutate(
    diff = pooled1 - pooled2,
    se_diff = sqrt(var1 + var2),
    z = diff / se_diff,
    p = 2 * (1 - pnorm(abs(z)))
  ) %>%
  select(Intervention1, Intervention2, diff, z, p)

# Remove duplicate comparisons (A vs B == B vs A)
comparison_results_unique <- comparison_results %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(Intervention1, Intervention2)), collapse = " vs ")) %>%
  ungroup() %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)

# View results
print(n = 315, comparison_results_unique %>% arrange(p))

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

# Statistically significant comparisons (p < 0.05)
significant_results <- results %>%
  filter(P_value < 0.05)
print(significant_results)

# Non-significant comparisons (p ≥ 0.05)
nonsignificant_results <- results %>%
  filter(P_value >= 0.05)
print(nonsignificant_results)

write.csv(significant_results, "amd_significant_results.csv", row.names = FALSE)
write.csv(nonsignificant_results, "amd_nonsignificant_results.csv", row.names = FALSE)
