#========================================
# 04_correlation_vif.R
# Purpose: Correlation matrix heatmap + VIF check
# Input: data/processed/data_model.RData
#========================================

# Install:

install.packages("car")
install.packages("corrplot")

library(tidyverse)
library(car)        # for vif()
library(corrplot)   # for correlation heatmap


# Load data
load("data/processed/data_model.RData")

#========================================
# STEP 1: Select model variables
#========================================

model_vars <- data_model %>%
  select(ln_wage, educ, age, age2, hours,
         male, cert, contract, state, fdi,
         match, educxmatch, bhxh) %>%
  na.omit()  # remove any remaining NAs

cat("Observations used in correlation/VIF:", nrow(model_vars), "\n")


#========================================
# STEP 2: Correlation Matrix
#========================================

cor_matrix <- cor(model_vars, use = "complete.obs")

# Print rounded correlation matrix

cat("\n--- Correlation Matrix ---\n")
print(round(cor_matrix, 2))

# Flag high correlations (|r| > 0.7) — potential multicollinearity

cat("\n--- High correlations (|r| > 0.7, excluding diagonal) ---\n")
high_cor <- which(abs(cor_matrix) > 0.7 & cor_matrix != 1, arr.ind = TRUE)
if (nrow(high_cor) == 0) {
  cat("No high correlations found (excluding age/age2 pair).\n")
} else {
  for (i in 1:nrow(high_cor)) {
    r <- high_cor[i, ]
    cat(sprintf("  %s <-> %s : r = %.3f\n",
                rownames(cor_matrix)[r[1]],
                colnames(cor_matrix)[r[2]],
                cor_matrix[r[1], r[2]]))
  }
}


#========================================
# STEP 3: Correlation Heatmap
#========================================

# Variable labels for plot
var_labels <- c("ln(wage)", "educ", "age", "age²", "hours",
                "male", "cert", "contract", "state", "fdi",
                "match", "educ×match", "bhxh")

colnames(cor_matrix) <- var_labels
rownames(cor_matrix) <- var_labels

# Save correlation heatmap to figures/
png("figures/correlation_heatmap.png",
    width = 2400, height = 2000, res = 300)

# Plot heatmap
corrplot(
  cor_matrix,
  method    = "color",
  type      = "upper",
  tl.col    = "black",
  tl.srt    = 45,
  tl.cex    = 0.85,
  addCoef.col = "black",
  number.cex  = 0.65,
  col       = colorRampPalette(c("#2166AC", "white", "#D6604D"))(200),
  title     = "Correlation Matrix — Manufacturing Sector, Red River Delta",
  mar       = c(0, 0, 2, 0)
)

dev.off()   # close the PNG device — must have this line!
cat("✓ Saved: figures/correlation_heatmap.png\n")

#========================================
# STEP 4: VIF Check — Full Sample
#========================================

cat("\n========================================\n")
cat("VIF CHECK — FULL SAMPLE\n")
cat("========================================\n")

model_full <- lm(ln_wage ~ educ + age + age2 + hours +
                   male + cert + contract + state + fdi +
                   match + educxmatch + bhxh,
                 data = model_vars)

vif_full <- vif(model_full)

vif_df <- data.frame(
  Variable  = names(vif_full),
  VIF       = round(vif_full, 3),
  Tolerance = round(1 / vif_full, 3),
  Status    = ifelse(vif_full > 10, "severe",
              ifelse(vif_full > 5,  " Moderate", "oK"))
)

print(vif_df, row.names = FALSE)

cat("\nNote: High VIF for age and age² is EXPECTED and ACCEPTABLE\n")
cat("      because age² is derived from age (quadratic term).\n")
cat("      This does NOT indicate a real multicollinearity problem.\n")

#========================================
# STEP 5: VIF Check — Subsample Female
#========================================

load("data/processed/sub1_female.RData")

cat("\n========================================\n")
cat("VIF CHECK — SUBSAMPLE 1: FEMALE\n")
cat("========================================\n")

model_female <- lm(ln_wage ~ educ + age + age2 + hours +
                     cert + contract + state + fdi +
                     match + educxmatch + bhxh,
                   data = sub1_female %>% na.omit())

vif_female <- vif(model_female)

vif_df_f <- data.frame(
  Variable  = names(vif_female),
  VIF       = round(vif_female, 3),
  Tolerance = round(1 / vif_female, 3),
  Status    = ifelse(vif_female > 10, "severe",
              ifelse(vif_female > 5,  "Moderate", "oK"))
)

print(vif_df_f, row.names = FALSE)


#========================================
# STEP 6: VIF Check — Subsample Male
#========================================

load("data/processed/sub2_male.RData")

cat("\n========================================\n")
cat("VIF CHECK — SUBSAMPLE 2: MALE\n")
cat("========================================\n")

model_male <- lm(ln_wage ~ educ + age + age2 + hours +
                   cert + contract + state + fdi +
                   match + educxmatch + bhxh,
                 data = sub2_male %>% na.omit())

vif_male <- vif(model_male)

vif_df_m <- data.frame(
  Variable  = names(vif_male),
  VIF       = round(vif_male, 3),
  Tolerance = round(1 / vif_male, 3),
  Status    = ifelse(vif_male > 10, "severe",
              ifelse(vif_male > 5,  "Moderate", "oK"))
)

print(vif_df_m, row.names = FALSE)

# Save VIF results to tables/
write.csv(vif_df,
          "tables/vif_full_sample.csv",
          row.names = FALSE)

write.csv(vif_df_f,
          "tables/vif_female.csv",
          row.names = FALSE)

write.csv(vif_df_m,
          "tables/vif_male.csv",
          row.names = FALSE)

cat("✓ Saved: tables/vif_full_sample.csv\n")
cat("✓ Saved: tables/vif_female.csv\n")
cat("✓ Saved: tables/vif_male.csv\n")

#========================================
# STEP 7: Summary Note
#========================================

cat("\n========================================\n")
cat("SUMMARY: VIF INTERPRETATION GUIDE\n")
cat("========================================\n")
cat("VIF < 5    : no multicollinearity problem\n")
cat("VIF 5-10   : Moderate — monitor carefully\n")
cat("VIF > 10   : Severe — consider removing variable\n")
cat("\nSpecial cases in this model:\n")
cat("- age & age2     : VIF will be HIGH (~20-30) — NORMAL for quadratic terms\n")
cat("- educxmatch     : VIF may be elevated — NORMAL for interaction terms\n")
cat("- bhxh & contract: Check if VIF > 5, may need to drop one\n")
