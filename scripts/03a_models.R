#========================================
# 03a_models.R
# Input: data/processed/data_model.RData
# Output: 
#   - tables/table3_full_sample.csv
#   - data/processed/model_results.RData
#========================================

# ── 1. Packages ───────────────────────────────
library(tidyverse)
library(lmtest)
library(sandwich)
library(broom)

# ── 2. Load data ──────────────────────────────
load("data/processed/data_model.RData")

# ── 3. Model specification ────────────────────
model_final <- lm(
  ln_wage ~ educ 
  + age 
  + I(age^2) 
  + hours 
  + male 
  + cert 
  + contract 
  + state 
  + fdi 
  + match 
  + educ:match 
  + bhxh,
  data = data_model
)
summary(model_final)

# ── 4. Robust standard errors ─────────────────
robust_result <- coeftest(
  model_final, 
  vcov = vcovHC(model_final, type = "HC1")
)
robust_result

# ── 5. Convert regression output → table ──────
table3 <- tidy(robust_result)
colnames(table3)
table3 <- table3 %>%
  rename(
    Variable = term,
    Coefficient = estimate,
    `Robust SE` = std.error,
    `t-stat` = statistic,
    `p-value` = p.value
  )

colnames(table3)
table3 <- table3 %>%
  select(Variable, Coefficient, `Robust SE`, `t-stat`, `p-value`)

# ── 6. Model statistics ───────────────────────
model_summary <- summary(model_final)
model_stats <- tibble(
  Variable = c("N", "R-squared", "Adjusted R-squared", "F-statistic"),
  Coefficient = c(
    nobs(model_final),
    model_summary$r.squared,
    model_summary$adj.r.squared,
    model_summary$fstatistic[1]
  ),
  `Robust SE` = NA_real_,
  `t-stat` = NA_real_,
  `p-value` = NA_real_
)
colnames(model_stats)
# ── 7. Combine + clean ────────────────────────
table3_final <- bind_rows(table3, model_stats) %>%
  mutate(across(where(is.numeric), as.numeric))

# ── 8. Export CSV  ───────
dir.create("tables", showWarnings = FALSE)

write_csv(table3_final, "tables/table3_full_sample.csv")

# ── 9. Save model results ─────────────────────
save(model_final, robust_result, file = "data/processed/model_results.RData")