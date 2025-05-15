### ───────────────────────
# Load libraries
### ───────────────────────
library(tidyverse)
install.packages("meta")
library(meta)
library(dplyr)
library(ggplot2)
library(purrr)

### ───────────────────────
# Load data and inspect
### ───────────────────────
response_data <- read.csv("~/Data_Files/response_data.csv")
head(response_data)

# Add responder and non-responder columns
response_data <- response_data %>%
  mutate(
    responders = round(Percent_Response * Sample_Size / 100),
    non_responders = Sample_Size - responders
  )

### ───────────────────────────────────────
### Pooled by group (NIBS vs Pharma)
### ───────────────────────────────────────

# Subset and run meta-analysis for Pharma
meta_pharma <- metaprop(
  event = responders,
  n = Sample_Size,
  data = filter(response_data, Group == "Pharma"),
  sm = "PLOGIT",
  method = "Inverse",
  random = TRUE,
  common = FALSE
)

# Subset and run meta-analysis for NIBS
meta_nibs <- metaprop(
  event = responders,
  n = Sample_Size,
  data = filter(response_data, Group == "NIBS"),
  sm = "PLOGIT",
  method = "Inverse",
  random = TRUE,
  common = FALSE
)

# Get summary for each group
pooled_data <- data.frame(
  Group = c("Pharma", "NIBS"),
  Pooled_Proportion = c(meta_pharma$TE.random, meta_nibs$TE.random),
  Lower_CI = c(meta_pharma$lower.random, meta_nibs$lower.random),
  Upper_CI = c(meta_pharma$upper.random, meta_nibs$upper.random)
)

# Transform from logit back to proportions
pooled_data <- pooled_data %>%
  mutate(
    Pooled_Proportion = plogis(Pooled_Proportion),
    Lower_CI = plogis(Lower_CI),
    Upper_CI = plogis(Upper_CI)
  )
print(pooled_data)

# Plot forest plot
ggplot(pooled_data, aes(x = Group, y = Pooled_Proportion, fill = Group)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Pooled Response Rates (≥50% Reduction in Migraine Days)",
    y = "Pooled Proportion of Responders",
    x = ""
  ) +
  theme_minimal(base_size = 14)

### ───────────────────────────────────────
### Pooled by intervention
### ───────────────────────────────────────

# Unique intervention names
interventions <- unique(response_data$Intervention)

# Run metaprop for each intervention
meta_list <- map(interventions, function(intv) {
  dat <- filter(response_data, Intervention == intv)
  res <- metaprop(
    event = dat$responders,
    n = dat$Sample_Size,
    sm = "PLOGIT",
    method = "Inverse",
    random = TRUE,
    common = FALSE
  )
  tibble(
    Intervention = intv,
    TE = plogis(res$TE.random),
    lower = plogis(res$lower.random),
    upper = plogis(res$upper.random)
  )
})

# Combine into a single dataframe
pooled_by_intervention <- bind_rows(meta_list)

#Plot forest plot
ggplot(pooled_by_intervention, aes(x = reorder(Intervention, TE), y = TE)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Pooled Response Rates by Intervention",
    x = "Intervention",
    y = "Proportion of Responders (≥50% reduction)"
  ) +
  theme_minimal(base_size = 14)

### ────────────────────────────
### Statistical testing
### ────────────────────────────

# Calculate pooled estimates and SE for each intervention
meta_list <- map(interventions, function(intv) {
  dat <- filter(response_data, Intervention == intv)
  res <- metaprop(
    event = dat$responders,
    n = dat$Sample_Size,
    sm = "PLOGIT",        # Logit transformation for proportions
    method = "Inverse",   # Inverse variance method
    random = TRUE,        # Random-effects model
    common = FALSE        # No fixed-effect model
  )
  tibble(
    Intervention = intv,
    logit_TE = res$TE.random,  # Pooled logit estimate
    SE = res$seTE.random       # Standard error of pooled logit estimate
  )
})

# Combine into a single dataframe
logit_data <- bind_rows(meta_list)

# Create all unique pairs of interventions
pairwise_results <- expand.grid(A = logit_data$Intervention, B = logit_data$Intervention) %>%
  mutate(A = as.character(A), B = as.character(B)) %>%  # Convert factors to strings
  filter(A < B) %>%  # Remove duplicates and self-comparisons
  rowwise() %>%
  mutate(
    # Pooled logit estimate and SE for Intervention A
    TE_A = logit_data$logit_TE[logit_data$Intervention == A],
    SE_A = logit_data$SE[logit_data$Intervention == A],
    
    # Pooled logit estimate and SE for Intervention B
    TE_B = logit_data$logit_TE[logit_data$Intervention == B],
    SE_B = logit_data$SE[logit_data$Intervention == B],
    
    # Difference in logit estimates
    logit_diff = TE_A - TE_B,
    
    # Standard error of the difference
    SE_diff = sqrt(SE_A^2 + SE_B^2),
    
    # Z-score and p-value for the difference
    z = logit_diff / SE_diff,
    p_value = 2 * pnorm(-abs(z)),
    
    # Difference in response rates on the proportion scale
    prop_diff = plogis(TE_A) - plogis(TE_B)
  ) %>%
  ungroup() %>%
  select(A, B, prop_diff, z, p_value)

# Show results sorted by smallest p-value
pairwise_results %>%
  arrange(p_value) %>%
  mutate(
    p_value = signif(p_value, 3),
    prop_diff = round(prop_diff * 100, 1)
  ) %>%
  rename(
    `Intervention A` = A,
    `Intervention B` = B,
    `Difference (%)` = prop_diff,
    `Z-Score` = z,
    `P-Value` = p_value
  ) %>%
  print(n = Inf)

# Significant comparisons
sig_results <- pairwise_results %>%
  filter(p_value < 0.05)

# Non-significant comparisons
nonsig_results <- pairwise_results %>%
  filter(p_value >= 0.05)

write.csv(sig_results, "sig_response_pairwise_comparisons.csv", row.names = FALSE)
write.csv(nonsig_results, "nonsig_response_pairwise_comparisons.csv", row.names = FALSE)

# Plot the pairwise comparisons of significant results
ggplot(sig_results, aes(x = prop_diff, y = interaction(A, B), color = p_value)) +
  geom_point(size = 3) +   # Plot points
  scale_color_gradient(low = "blue", high = "red", guide = guide_colorbar(title = "p-value")) +
  theme_minimal() +
  labs(
    title = "Pairwise Comparisons of Pooled Response Rates (Significant Results)",
    x = "Difference in Response Rates (%)",
    y = "Intervention Pairs",
    caption = "Significant results (p < 0.05) are shown."
  ) +
  theme(axis.text.y = element_text(size = 8))  # Adjust y-axis text size if needed
