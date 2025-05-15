### ───────────────────────
# Load libraries
### ───────────────────────
install.packages("tidyverse")
install.packages("metafor")
install.packages("scico") # color palette
library(tidyverse)
library(metafor)
library(scico)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

### ───────────────────────
# Load data and inspect
### ───────────────────────

mhd_data <- read_csv("~/Data_Files/mhd_data.csv")
glimpse(mhd_data)
summary(mhd_data)

### ─────────────────────────────────────────────────
# Calculate effect sizes (Mean Difference)
### ─────────────────────────────────────────────────
mhd_data <- escalc(
  measure = "MN", 
  yi = Change_MHD,
  vi = MHD_SD^2 / Sample_Size,
  data = mhd_data
)

### ─────────────────────────────────────
# Run main random-effects model
### ─────────────────────────────────────

# Overall effect size (MMD change across all studies)
res_overall <- rma(yi, vi, data = mhd_data, method = "REML")
summary(res_overall)

# Print I², τ², Q-test (already in summary)
print(res_overall$I2)  # I-squared
print(res_overall$tau2)  # Tau-squared

### ─────────────────────────────────────
### Subgroup analysis
### ─────────────────────────────────────

# (A) NIBS vs Pharma
res_group <- rma(yi, vi, mods = ~ Group, data = mhd_data, method = "REML")
summary(res_group)

# (B) By interventions
res_intervention <- rma(yi, vi, mods = ~ Intervention, data = mhd_data, method = "REML")
summary(res_intervention)

### ─────────────────────────────────────
### Visualization
### ─────────────────────────────────────

######### Grouped by Intervention ###############

# Group by Intervention to calculate pooled effect sizes
intervention_summary <- mhd_data %>%
  group_by(Intervention) %>%
  summarise(
    estimate = mean(yi),  # Mean effect size (pooled)
    ci.lb = mean(yi) - 1.96 * sqrt(mean(vi)),  # Lower bound of 95% CI
    ci.ub = mean(yi) + 1.96 * sqrt(mean(vi)),  # Upper bound of 95% CI
    .groups = 'drop'
  )

# Plot the forest plot
ggplot(intervention_summary, aes(x = estimate, y = Intervention)) +
  geom_point(size = 3, color = "blue") +  # Effect sizes, colored blue
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.2) +  # CI intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at 0
  theme_minimal() +
  labs(
    title = "Pooled Effect Sizes by Intervention",
    x = "Mean Difference in Monthly Headache Days",
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
nibs_data <- mhd_data %>% filter(Group == "NIBS")
head(nibs_data)

# Meta-analysis for each NIBS protocol
nibs_res <- rma(yi, vi, mods = ~ Intervention, data = nibs_data)
summary(nibs_res)

# Forest plot comparing NIBS protocols
ggplot(nibs_data, aes(x = yi, y = Intervention)) +
  geom_point(aes(color = Intervention), size = 4) +  # Points for effect sizes
  geom_errorbarh(aes(xmin = yi - 1.96 * sqrt(vi), xmax = yi + 1.96 * sqrt(vi)), height = 0.2) +  # CI intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at 0
  labs(
    title = "NIBS Protocols Comparison",
    x = "Mean Difference in Monthly Headache Days",
    y = "NIBS Intervention",
    subtitle = "Effect Sizes and 95% Confidence Intervals"
  ) +
  theme_minimal()

######### Within Pharma ###############

# Filter data for Pharma group
pharma_data <- mhd_data %>% filter(Group == "Pharma")
head(pharma_data)

# Meta-analysis for Pharma treatments
pharma_res <- rma(yi, vi, mods = ~ Intervention, data = pharma_data)
summary(pharma_res)

# Forest plot comparing Pharma treatments
ggplot(pharma_data, aes(x = yi, y = Intervention)) +
  geom_point(aes(color = Intervention), size = 4) +  # Points for effect sizes
  geom_errorbarh(aes(xmin = yi - 1.96 * sqrt(vi), xmax = yi + 1.96 * sqrt(vi)), height = 0.2) +  # CI intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at 0
  labs(
    title = "Pharma Treatments Comparison",
    x = "Mean Difference in Monthly Headache Days",
    y = "Pharma Treatment",
    subtitle = "Effect Sizes and 95% Confidence Intervals"
  ) +
  theme_minimal()

### ─────────────────────────────────────
### Statistical Testing
### ─────────────────────────────────────

# Run meta-analysis per intervention
t3_effects <- mhd_data %>%
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
res_meta <- rma(yi, vi, mods = ~ Intervention - 1, data = mhd_data)  # -1 removes intercept, gives direct est for each intervention

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

# Adjust for multiple comparisons if needed
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

write.csv(significant_results, "mhd_significant_results.csv", row.names = FALSE)
write.csv(nonsignificant_results, "mhd_nonsignificant_results.csv", row.names = FALSE)
