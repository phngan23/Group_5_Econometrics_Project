#========================================
# 03b_models.R
# Input: data/processed/data_model.RData
#        data/processed/sub1_female.RData
#        data/processed/sub1_male.RData
# Output: data/processed/model_results.RData
#         tables/table4_three_meodels.csv
#========================================

library(tidyverse)

# Load data
load("data/processed/data_model.RData")
load("data/processed/sub1_female.RData")
load("data/processed/sub2_male.RData")

# ================================
# DEFINE MODEL
# ================================

formula <- ln_wage ~ educ + age + age2 + hours +
           male + cert + contract + state + fdi +
           match + educxmatch + bhxh

# ================================
# RUN MODELS
# ================================

model_full   <- lm(formula, data = data_model)
model_female <- lm(formula, data = sub1_female)
model_male   <- lm(formula, data = sub2_male)

# ================================
# EXTRACT RESULTS
# ================================

get_results <- function(model, label) {
  broom::tidy(model) %>%
    select(term, estimate, std.error) %>%
    rename(
      !!paste0("coef_", label) := estimate,
      !!paste0("se_", label) := std.error
    )
}

res_full   <- get_results(model_full, "full")
res_female <- get_results(model_female, "female")
res_male   <- get_results(model_male, "male")

# ================================
# MERGE TABLE
# ================================

table4 <- res_full %>%
  left_join(res_female, by = "term") %>%
  left_join(res_male, by = "term")

# ================================
# SAVE OUTPUT
# ================================

dir.create("tables", showWarnings = FALSE)

write.csv(table4, "tables/table4_three_models.csv", row.names = FALSE)

# ================================
# FORMAT TABLE 
# ================================

format_col <- function(coef, se) {
  paste0(round(coef, 4), " (", round(se, 4), ")")
}

table4 <- table4 %>%
  mutate(
    full   = format_col(coef_full, se_full),
    female = format_col(coef_female, se_female),
    male   = format_col(coef_male, se_male)
  ) %>%
  select(term, full, female, male)

write.csv(table4, "tables/table4_three_models.csv", row.names = FALSE)

print(table4)

