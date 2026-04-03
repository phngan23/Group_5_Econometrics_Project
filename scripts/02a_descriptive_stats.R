# ================================================
# 02a_descriptive_stats.R
# Purpose: Descriptive statistics — Table 1 & Table 2
# Input:   data/processed/data_model.RData
# Output:  tables/table1a_continuous.csv
#          tables/table1b_dummy.csv
#          tables/table2a_continuous_by_gender.csv
#          tables/table2b_dummy_by_gender.csv
# ================================================

library(tidyverse)

# ── LOAD DATA ─────────────────────────────────────

load("data/processed/data_model.RData")
cat("Observations loaded:", nrow(data_model), "\n")


# ════════════════════════════════════════════════
# TABLE 1A: CONTINUOUS VARIABLES — FULL SAMPLE
# ════════════════════════════════════════════════

# Build table manually — one row per variable
table1_continuous <- data.frame(
  Variable = c("ln_wage", "educ", "age", "hours"),
  Mean = c(
    round(mean(data_model$ln_wage), 3),
    round(mean(data_model$educ),    3),
    round(mean(data_model$age),     3),
    round(mean(data_model$hours),   3)
  ),
  SD = c(
    round(sd(data_model$ln_wage), 3),
    round(sd(data_model$educ),    3),
    round(sd(data_model$age),     3),
    round(sd(data_model$hours),   3)
  ),
  Min = c(
    round(min(data_model$ln_wage), 3),
    round(min(data_model$educ),    3),
    round(min(data_model$age),     3),
    round(min(data_model$hours),   3)
  ),
  Max = c(
    round(max(data_model$ln_wage), 3),
    round(max(data_model$educ),    3),
    round(max(data_model$age),     3),
    round(max(data_model$hours),   3)
  )
)

cat("\n--- Table 1A: Continuous Variables ---\n")
print(table1_continuous)


# ════════════════════════════════════════════════
# TABLE 1B: DUMMY VARIABLES — FULL SAMPLE
# ════════════════════════════════════════════════

# Build table manually — one row per variable
# Shows count and % of observations where dummy = 1
table1_dummy <- data.frame(
  Variable    = c("male", "cert", "contract", "state", "fdi", "match", "bhxh"),
  Description = c("Male", "Has vocational cert", "Permanent contract",
                  "State sector", "FDI sector",
                  "Job-skill matched", "Has social insurance"),
  N_1   = c(
    sum(data_model$male),     sum(data_model$cert),
    sum(data_model$contract), sum(data_model$state),
    sum(data_model$fdi),      sum(data_model$match),
    sum(data_model$bhxh)
  ),
  N_0   = c(
    sum(data_model$male == 0),     sum(data_model$cert == 0),
    sum(data_model$contract == 0), sum(data_model$state == 0),
    sum(data_model$fdi == 0),      sum(data_model$match == 0),
    sum(data_model$bhxh == 0)
  ),
  Pct_1 = c(
    round(mean(data_model$male)     * 100, 1),
    round(mean(data_model$cert)     * 100, 1),
    round(mean(data_model$contract) * 100, 1),
    round(mean(data_model$state)    * 100, 1),
    round(mean(data_model$fdi)      * 100, 1),
    round(mean(data_model$match)    * 100, 1),
    round(mean(data_model$bhxh)     * 100, 1)
  )
)

cat("\n--- Table 1B: Dummy Variables (N and % where = 1) ---\n")
print(table1_dummy)


# ════════════════════════════════════════════════
# TABLE 2A: CONTINUOUS VARIABLES BY GENDER
# ════════════════════════════════════════════════

female <- data_model %>% filter(male == 0)
male   <- data_model %>% filter(male == 1)

table2_continuous <- data.frame(
  Variable = c("ln_wage", "educ", "age", "hours"),
  
  Female_Mean = c(
    round(mean(female$ln_wage), 3),
    round(mean(female$educ),    3),
    round(mean(female$age),     3),
    round(mean(female$hours),   3)
  ),
  Female_SD = c(
    round(sd(female$ln_wage), 3),
    round(sd(female$educ),    3),
    round(sd(female$age),     3),
    round(sd(female$hours),   3)
  ),
  Male_Mean = c(
    round(mean(male$ln_wage), 3),
    round(mean(male$educ),    3),
    round(mean(male$age),     3),
    round(mean(male$hours),   3)
  ),
  Male_SD = c(
    round(sd(male$ln_wage), 3),
    round(sd(male$educ),    3),
    round(sd(male$age),     3),
    round(sd(male$hours),   3)
  )
)

cat("\n--- Table 2A: Continuous Variables by Gender ---\n")
print(table2_continuous)


# ════════════════════════════════════════════════
# TABLE 2B: DUMMY VARIABLES BY GENDER
# ════════════════════════════════════════════════

table2_dummy <- data.frame(
  Variable = c("cert", "contract", "state", "fdi", "match", "bhxh"),
  
  Female_Pct = c(
    round(mean(female$cert)     * 100, 1),
    round(mean(female$contract) * 100, 1),
    round(mean(female$state)    * 100, 1),
    round(mean(female$fdi)      * 100, 1),
    round(mean(female$match)    * 100, 1),
    round(mean(female$bhxh)     * 100, 1)
  ),
  Male_Pct = c(
    round(mean(male$cert)     * 100, 1),
    round(mean(male$contract) * 100, 1),
    round(mean(male$state)    * 100, 1),
    round(mean(male$fdi)      * 100, 1),
    round(mean(male$match)    * 100, 1),
    round(mean(male$bhxh)     * 100, 1)
  )
)

cat("\n--- Table 2B: Dummy Variables by Gender (% where = 1) ---\n")
print(table2_dummy)


# ════════════════════════════════════════════════
# STATISTICAL TESTS: MALE VS FEMALE
# ════════════════════════════════════════════════

# T-test for continuous variables
cat("\n--- T-tests: Male vs Female ---\n")
for (var in c("ln_wage", "educ", "age", "hours")) {
  result <- t.test(data_model[[var]] ~ data_model$male)
  cat(sprintf("%-10s : t = %6.3f, p = %.4f %s\n",
              var,
              result$statistic,
              result$p.value,
              ifelse(result$p.value < 0.05, "(*)", "")))
}

# Chi-square test for dummy variables
cat("\n--- Chi-square tests: Male vs Female ---\n")
for (var in c("cert", "contract", "state", "fdi", "match", "bhxh")) {
  tbl    <- table(data_model[[var]], data_model$male)
  result <- chisq.test(tbl)
  cat(sprintf("%-10s : chi2 = %6.3f, p = %.4f %s\n",
              var,
              result$statistic,
              result$p.value,
              ifelse(result$p.value < 0.05, "(*)", "")))
}
cat("Note: (*) = significant at 5% level\n")


# ════════════════════════════════════════════════
# SAVE OUTPUT FILES
# ════════════════════════════════════════════════

write.csv(table1_continuous, "tables/table1a_continuous.csv", row.names = FALSE)
write.csv(table1_dummy,      "tables/table1b_dummy.csv",      row.names = FALSE)
write.csv(table2_continuous, "tables/table2a_by_gender_continuous.csv", row.names = FALSE)
write.csv(table2_dummy,      "tables/table2b_by_gender_dummy.csv",      row.names = FALSE)

cat("\n✓ Saved: tables/table1a_continuous.csv\n")
cat("✓ Saved: tables/table1b_dummy.csv\n")
cat("✓ Saved: tables/table2a_by_gender_continuous.csv\n")
cat("✓ Saved: tables/table2b_by_gender_dummy.csv\n")
cat("\n✓ 02_descriptive_stats.R complete\n")