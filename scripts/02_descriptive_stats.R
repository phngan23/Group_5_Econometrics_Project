
# =============================
# 02_descriptive_stats.R
# =============================

library(tidyverse)

# Load data
load("data/processed/data_model.RData")

# =============================
# TABLE 1: FULL SAMPLE
# =============================

# Continuous variables
continuous_vars <- data_model %>%
  summarise(
    ln_wage_mean = mean(ln_wage),
    ln_wage_sd   = sd(ln_wage),
    ln_wage_min  = min(ln_wage),
    ln_wage_max  = max(ln_wage),

    educ_mean = mean(educ),
    educ_sd   = sd(educ),
    educ_min  = min(educ),
    educ_max  = max(educ),

    age_mean = mean(age),
    age_sd   = sd(age),
    age_min  = min(age),
    age_max  = max(age),

    hours_mean = mean(hours),
    hours_sd   = sd(hours),
    hours_min  = min(hours),
    hours_max  = max(hours)
  )

print(continuous_vars)


# Dummy variables (%)
dummy_vars <- data_model %>%
  summarise(
    male_pct     = mean(male) * 100,
    cert_pct     = mean(cert) * 100,
    contract_pct = mean(contract) * 100,
    state_pct    = mean(state) * 100,
    fdi_pct      = mean(fdi) * 100,
    match_pct    = mean(match) * 100,
    bhxh_pct     = mean(bhxh) * 100
  )

print(dummy_vars)


# =============================
# TABLE 2: BY GENDER
# =============================

table_gender <- data_model %>%
  group_by(male) %>%
  summarise(
    ln_wage = mean(ln_wage),
    educ    = mean(educ),
    age     = mean(age),
    hours   = mean(hours)
  )

print(table_gender)


# =============================
# T-TEST
# =============================

t_test <- t.test(ln_wage ~ male, data = data_model)
print(t_test)

