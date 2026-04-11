# ================================================
# 03_models.R
# Purpose: OLS regression — Full Sample & Subsamples
# Input:   data/processed/data_model.RData
#          data/processed/sub1_female.RData
#          data/processed/sub2_male.RData
# Output:  data/processed/model_results.RData
#          tables/table3_full_sample.csv
#          tables/table4_three_models.csv
# ================================================

library(tidyverse)
library(lmtest)
library(sandwich)


# ── LOAD DATA ─────────────────────────────────────

load("data/processed/data_model.RData")
load("data/processed/sub1_female.RData")
load("data/processed/sub2_male.RData")

cat("Full sample:         ", nrow(data_model),  "observations\n")
cat("Subsample 1 (Female):", nrow(sub1_female), "observations\n")
cat("Subsample 2 (Male):  ", nrow(sub2_male),   "observations\n")


# ════════════════════════════════════════════════
# STEP 1: RUN OLS — ALL 3 MODELS
# ════════════════════════════════════════════════

# Full sample — includes 'male' variable
model_full <- lm(
  ln_wage ~ educ + age + age2 + hours +
    male + cert + contract + state + fdi +
    match + educxmatch + bhxh,
  data = data_model
)

# Subsample Female — 'male' dropped (no variation within group)
model_female <- lm(
  ln_wage ~ educ + age + age2 + hours +
    cert + contract + state + fdi +
    match + educxmatch + bhxh,
  data = sub1_female
)

# Subsample Male — 'male' dropped (no variation within group)
model_male <- lm(
  ln_wage ~ educ + age + age2 + hours +
    cert + contract + state + fdi +
    match + educxmatch + bhxh,
  data = sub2_male
)

cat("\n✓ All 3 OLS models estimated\n")


# ════════════════════════════════════════════════
# STEP 2: APPLY ROBUST STANDARD ERRORS (HC1)
# ════════════════════════════════════════════════

# HC3 robust SE corrects for heteroskedasticity
# Required because Breusch-Pagan test (in 04_diagnostics.R)
# will confirm presence of heteroskedasticity

robust_full   <- coeftest(model_full,   vcov = vcovHC(model_full,   type = "HC3"))
robust_female <- coeftest(model_female, vcov = vcovHC(model_female, type = "HC3"))
robust_male   <- coeftest(model_male,   vcov = vcovHC(model_male,   type = "HC3"))

cat("✓ Robust SE (HC3) applied to all models\n")


# ════════════════════════════════════════════════
# STEP 3: EXTRACT MODEL FIT STATISTICS
# ════════════════════════════════════════════════

get_fit <- function(model, label) {
  s <- summary(model)
  cat(sprintf("\n[%s]\n", label))
  cat(sprintf("  R-squared:      %.4f\n", s$r.squared))
  cat(sprintf("  Adj R-squared:  %.4f\n", s$adj.r.squared))
  cat(sprintf("  N:              %d\n",   nrow(model$model)))
  return(s)
}

cat("\n--- Model Fit Statistics ---\n")
sum_full   <- get_fit(model_full,   "Full Sample")
sum_female <- get_fit(model_female, "Female Subsample")
sum_male   <- get_fit(model_male,   "Male Subsample")


# ════════════════════════════════════════════════
# STEP 4: BUILD TABLE 3 — FULL SAMPLE RESULTS
# ════════════════════════════════════════════════

# Extract coefficients with robust SE
coef_table <- as.data.frame(robust_full[, ])
colnames(coef_table) <- c("Coefficient", "Robust_SE", "t_stat", "p_value")

# Add significance stars
coef_table$Significance <- ifelse(
  coef_table$p_value < 0.01, "***",
  ifelse(coef_table$p_value < 0.05, "**",
         ifelse(coef_table$p_value < 0.10, "*", ""))
)

# Round numbers
coef_table <- coef_table %>%
  mutate(across(c(Coefficient, Robust_SE, t_stat), ~round(., 4)),
         p_value = round(p_value, 4))

# Add variable names
coef_table$Variable <- rownames(coef_table)
coef_table <- coef_table %>%
  select(Variable, Coefficient, Robust_SE, t_stat, p_value, Significance)

# Extract F-statistic p-value
f_pvalue <- round(pf(sum_full$fstatistic[1],
                     sum_full$fstatistic[2],
                     sum_full$fstatistic[3],
                     lower.tail = FALSE), 4)

# Add model fit rows at the bottom
fit_rows_3 <- data.frame(
  Variable     = c("R-squared", "Adj. R-squared", "F-statistic", "N"),
  Coefficient  = c(round(sum_full$r.squared, 4),
                   round(sum_full$adj.r.squared, 4),
                   round(sum_full$fstatistic[1], 3),
                   nrow(data_model)),
  Robust_SE    = NA,
  t_stat       = NA,
  p_value      = c(NA, NA, f_pvalue, NA),
  Significance = NA
)

table3 <- bind_rows(coef_table, fit_rows_3)

cat("\n--- TABLE 3: OLS Results — Full Sample ---\n")
print(table3, row.names = FALSE)


# ════════════════════════════════════════════════
# STEP 5: BUILD TABLE 4 — THREE MODELS SIDE BY SIDE
# ════════════════════════════════════════════════

# Helper function: format coefficient with stars and SE below
extract_coef <- function(robust_result, label) {
  df <- as.data.frame(robust_result[, ])
  colnames(df) <- c("Coef", "SE", "t", "p")
  
  df <- df %>%
    mutate(
      Stars   = ifelse(p < 0.01, "***",
                       ifelse(p < 0.05, "**",
                              ifelse(p < 0.10, "*", ""))),
      Display = paste0(round(Coef, 4), Stars,
                       "\n(", round(SE, 4), ")")
    ) %>%
    select(Display)
  
  colnames(df) <- label
  df$Variable  <- rownames(robust_result)
  df <- df %>% select(Variable, everything())
  return(df)
}

# Extract for each model
full_col   <- extract_coef(robust_full,   "Full_Sample")
female_col <- extract_coef(robust_female, "Female")
male_col   <- extract_coef(robust_male,   "Male")

# Merge side by side
# Note: full sample has 'male' row, subsamples do not → use full_join
table4 <- full_join(full_col, female_col, by = "Variable") %>%
  full_join(male_col, by = "Variable")

# Add model fit rows
fit_rows_4 <- data.frame(
  Variable    = c("R-squared", "Adj. R-squared", "N"),
  Full_Sample = as.character(c(round(sum_full$r.squared,   4),
                               round(sum_full$adj.r.squared, 4),
                               nrow(data_model))),
  Female      = as.character(c(round(sum_female$r.squared,   4),
                               round(sum_female$adj.r.squared, 4),
                               nrow(sub1_female))),
  Male        = as.character(c(round(sum_male$r.squared,   4),
                               round(sum_male$adj.r.squared, 4),
                               nrow(sub2_male)))
)

table4 <- bind_rows(table4, fit_rows_4)

cat("\n--- TABLE 4: Three Models Side by Side ---\n")
print(table4, row.names = FALSE)


# ════════════════════════════════════════════════
# STEP 6: MARGINAL EFFECT OF educ
# ════════════════════════════════════════════════

# With interaction term educxmatch:
# Marginal effect of educ = β_educ + β_educxmatch × match
# When match = 0: effect = β_educ only
# When match = 1: effect = β_educ + β_educxmatch

compute_marginal <- function(model_obj, label) {
  b_educ        <- coef(model_obj)["educ"]
  b_interaction <- coef(model_obj)["educxmatch"]
  
  cat(sprintf("\n[%s]\n", label))
  cat(sprintf("  Return to educ (match=0): %.4f → %.2f%% per level\n",
              b_educ, b_educ * 100))
  cat(sprintf("  Return to educ (match=1): %.4f → %.2f%% per level\n",
              b_educ + b_interaction, (b_educ + b_interaction) * 100))
  cat(sprintf("  Additional return from match: %.2f%% per level\n",
              b_interaction * 100))
}

cat("\n--- MARGINAL EFFECT OF EDUCATION ---\n")
compute_marginal(model_full,   "Full Sample")
compute_marginal(model_female, "Female Subsample")
compute_marginal(model_male,   "Male Subsample")


# ════════════════════════════════════════════════
# STEP 7: SAVE ALL OUTPUTS
# ════════════════════════════════════════════════

# Save all 6 model objects in one file
save(model_full,   model_female,   model_male,
     robust_full,  robust_female,  robust_male,
     file = "data/processed/model_results.RData")
cat("\n✓ Saved: data/processed/model_results.RData\n")
cat("  Contains: model_full, model_female, model_male\n")
cat("            robust_full, robust_female, robust_male\n")

write.csv(table3,
          "tables/table3_full_sample.csv",
          row.names = FALSE,
          na = "")
cat("✓ Saved: tables/table3_full_sample.csv\n")

write.csv(table4,
          "tables/table4_three_models.csv",
          row.names = FALSE,
          na = "")
cat("✓ Saved: tables/table4_three_models.csv\n")

cat("\n✓ 03_models.R complete\n")