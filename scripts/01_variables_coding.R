#========================================
# 01_variables_coding.R
# Purpose: Read clean data, create and encode variables
# Input: data/raw/data.csv
# Output: data/processed/data_model.RData
#         data/processed/sub1_female.RData
#         data/processed/sub1_male.RData
#========================================

# install packages and library
install.packages("tidyverse")
install.packages("haven") # read file .csv

library(tidyverse)
library(haven)


#========================================
# Step 1: Read Data
#========================================
data_raw <- read.csv("data/raw/data.csv")
names(data_raw)

# Quick check
glimpse(data_raw)
head(data_raw)

cat("Total observations in raw data:", nrow(data_raw), "\n")


#========================================
# Step 2: Remove invalid observations
#========================================
data_filtered <- data_raw %>%
  filter(
    C44A > 0,       # wage must be positive (no zeros)
    C46A > 0,       # working hours must be positive (removes 3 obs)
    C5   >= 15      # minimum legal working age in Vietnam
  )

cat("Observations after removing invalids:", nrow(data_filtered), "\n")

#========================================
# Step 3: Create and encode variables
#========================================

data_model <- data_filtered %>%
  mutate(
    
    # ------ DEPENDENT VARIABLE ------
    
    # Natural log of monthly wage from main job
    # Using log to normalize skewed wage distribution
    # and allow percentage interpretation of coefficients
    ln_wage = log(C44A),
    
    
    # ------ CONTINUOUS VARIABLES ------
    
    # Educational attainment (ordinal scale 1-9)
    # 1=no schooling, 2=incomplete primary, 3=primary,
    # 4=lower secondary, 5=upper secondary,
    # 6=vocational secondary, 7=college, 8=university, 9=postgraduate
    educ = as.numeric(C17),
    
    # Age in years (proxy for work experience)
    age  = as.numeric(C5),
    
    # Age squared — captures concave age-wage profile
    # (wages rise fast early in career, slow down later)
    age2 = as.numeric(C5)^2,
    
    # Usual weekly working hours
    hours = as.numeric(C46A),
    
    
    # ------ DUMMY VARIABLES ------
    
    # Gender: male = 1, female = 0
    # Base group: female workers
    male = ifelse(C3 == 1, 1, 0),
    
    # Vocational/professional certificate:
    # has certificate at elementary level or above = 1
    # C19: 1=none, 2=skilled worker no cert,
    #      3=skill training <3mo, 4=cert <3mo,
    #      5=elementary vocational, 6=intermediate, 7=college vocational
    cert = ifelse(C19 >= 5, 1, 0),
    
    # Labor contract type:
    # indefinite-term (permanent) contract = 1, all others = 0
    # C36: 1=indefinite, 2=1yr to <3yr, 3=3mo to <1yr,
    #      4=<3mo, 5=piece-rate, 6=verbal agreement, 7=no contract
    contract = ifelse(C36 == 1, 1, 0),
    
    # State-owned sector = 1, others = 0
    # Base group: domestic private sector
    # C31: 7=legislative/executive/judicial agencies,
    #      8=state organizations, 
    #      9=state public service units,
    #      10=state-owned enterprises
    state = ifelse(C31 %in% c(7, 8, 9, 10), 1, 0),
    
    # Foreign direct investment sector = 1, others = 0
    # Base group: domestic private sector
    # C31: 11 = foreign-invested enterprises
    fdi = ifelse(C31 == 11, 1, 0),
    
    # Job-skill match = 1 if working in trained field, 0 otherwise
    # C51: 1=matched, 2=not matched,
    #      3=no training received, 4=do not know
    # Groups 2, 3, 4 are all treated as non-matched (= 0)
    match = ifelse(C51 == 1, 1, 0),
    
    # Social insurance participation = 1, not participating = 0
    # Proxy for formal employment status
    bhxh = ifelse(C39 == 1, 1, 0),
    
    
    # ------ INTERACTION TERM ------
    
    # Education × Match
    # Tests whether returns to education are higher
    # when workers are employed in their trained field
    # Marginal effect of educ = β_educ + β_educxmatch × match
    educxmatch = as.numeric(C17) * ifelse(C51 == 1, 1, 0)
    
  )

cat("Observations after variable creation:", nrow(data_model), "\n")


#========================================
# Step 4: Check encoded variables
#========================================

cat("\n--- Summary of model variables ---\n")
summary(data_model %>% select(
  ln_wage, educ, age, age2, hours,
  male, cert, contract, state, fdi,
  match, educxmatch, bhxh
))

# Check dummy distributions (frequency and %)
cat("\n--- Dummy variable distributions ---\n")
dummy_vars <- c("male", "cert", "contract", "state", "fdi", "match", "bhxh")

for (var in dummy_vars) {
  freq  <- table(data_model[[var]])
  pct   <- round(prop.table(freq) * 100, 1)
  cat(sprintf("%-10s 0: %5d (%s%%)   1: %5d (%s%%)\n",
              paste0(var, ":"),
              freq["0"], pct["0"],
              freq["1"], pct["1"]))
}

# Confirm no remaining missing values
cat("\n--- Missing values in model variables ---\n")
missing <- colSums(is.na(data_model %>% select(
  ln_wage, educ, age, age2, hours,
  male, cert, contract, state, fdi,
  match, educxmatch, bhxh
)))
print(missing)
cat("Total missing:", sum(missing), "\n")


#========================================
# Step 5: Create subsamples
#========================================

# Subsample 1: Female workers (C3 = 2)
# Used to examine wage determinants among women
sub1_female <- data_model %>% filter(C3 == 2)

# Subsample 2: Male workers (C3 = 1)
# Used to examine wage determinants among men
sub2_male <- data_model %>% filter(C3 == 1)

cat("\n--- Subsample sizes ---\n")
cat("Full sample:         ", nrow(data_model),  "observations\n")
cat("Subsample 1 (Female):", nrow(sub1_female), "observations\n")
cat("Subsample 2 (Male):  ", nrow(sub2_male),   "observations\n")

#========================================
# Step 6: Save output files
#========================================

save(data_model,  file = "data/processed/data_model.RData")
save(sub1_female, file = "data/processed/sub1_female.RData")
save(sub2_male,   file = "data/processed/sub2_male.RData")

cat("\n✓ All files saved to data/processed/\n")
cat("  - data_model.RData  (full sample)\n")
cat("  - sub1_female.RData (female subsample)\n")
cat("  - sub2_male.RData   (male subsample)\n")