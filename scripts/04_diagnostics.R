# ================================================
# 04_diagnostics.R
# Purpose: Test OLS assumption violations (Task 3.3)
#          1. Heteroskedasticity (Breusch-Pagan test)
#          2. Normality of residuals (Shapiro-Wilk)
#          3. Multicollinearity (VIF)
#          4. Model specification (RESET test)
# Input:   data/processed/model_results.RData
# Output:  tables/table5_diagnostics.csv
#          figures/residual_plots.png
# ================================================

library(tidyverse)
library(lmtest)   # for bptest(), resettest()
library(sandwich) # for vcovHC()
library(car)      # for vif()


# ── LOAD MODEL RESULTS FROM 03b ───────────────────

load("data/processed/model_results.RData")
cat("Models loaded: model_full, model_female, model_male ✓\n")


# ════════════════════════════════════════════════
# STEP 1: HETEROSKEDASTICITY TEST (Breusch-Pagan)
# ════════════════════════════════════════════════

# H0: Residuals have constant variance (homoskedasticity)
# H1: Residuals have non-constant variance (heteroskedasticity)
# If p < 0.05 → reject H0 → heteroskedasticity present
#              → must use robust standard errors (already done in 03a/03b)

bp_full   <- bptest(model_full)
bp_female <- bptest(model_female)
bp_male   <- bptest(model_male)

cat("\n--- Breusch-Pagan Test (Heteroskedasticity) ---\n")
cat(sprintf("Full Sample : BP = %.3f, p = %.4f  %s\n",
            bp_full$statistic,   bp_full$p.value,
            ifelse(bp_full$p.value   < 0.05, "→ Heteroskedasticity detected", "→ No issue")))
cat(sprintf("Female      : BP = %.3f, p = %.4f  %s\n",
            bp_female$statistic, bp_female$p.value,
            ifelse(bp_female$p.value < 0.05, "→ Heteroskedasticity detected", "→ No issue")))
cat(sprintf("Male        : BP = %.3f, p = %.4f  %s\n",
            bp_male$statistic,   bp_male$p.value,
            ifelse(bp_male$p.value   < 0.05, "→ Heteroskedasticity detected", "→ No issue")))
cat("Note: Robust SE (HC1) already applied in regression → this is handled\n")


# ════════════════════════════════════════════════
# STEP 2: NORMALITY OF RESIDUALS (Shapiro-Wilk)
# ════════════════════════════════════════════════

# H0: Residuals are normally distributed
# H1: Residuals are not normally distributed
# Note: With large samples (N > 5000), Shapiro-Wilk is very sensitive
#       Even tiny deviations from normality will show p < 0.05
#       → Look at the histogram and QQ-plot instead for large samples

# Shapiro-Wilk requires sample size <= 5000
# So we test on a random sample of 5000 residuals
set.seed(42)  # for reproducibility

resid_full_sample   <- sample(residuals(model_full),   5000)
resid_female_sample <- sample(residuals(model_female), 5000)
resid_male_sample   <- sample(residuals(model_male),   5000)

sw_full   <- shapiro.test(resid_full_sample)
sw_female <- shapiro.test(resid_female_sample)
sw_male   <- shapiro.test(resid_male_sample)

cat("\n--- Shapiro-Wilk Test (Normality of Residuals) ---\n")
cat(sprintf("Full Sample : W = %.4f, p = %.4f  %s\n",
            sw_full$statistic,   sw_full$p.value,
            ifelse(sw_full$p.value   < 0.05, "→ Non-normal", "→ Normal")))
cat(sprintf("Female      : W = %.4f, p = %.4f  %s\n",
            sw_female$statistic, sw_female$p.value,
            ifelse(sw_female$p.value < 0.05, "→ Non-normal", "→ Normal")))
cat(sprintf("Male        : W = %.4f, p = %.4f  %s\n",
            sw_male$statistic,   sw_male$p.value,
            ifelse(sw_male$p.value   < 0.05, "→ Non-normal", "→ Normal")))
cat("Note: With N > 5000, Shapiro-Wilk is highly sensitive.\n")
cat("      Non-normality of residuals does NOT invalidate OLS\n")
cat("      when sample size is large (Central Limit Theorem applies).\n")


# ════════════════════════════════════════════════
# STEP 3: MULTICOLLINEARITY (VIF)
# ════════════════════════════════════════════════

# VIF < 5   : No problem
# VIF 5-10  : Moderate — monitor
# VIF > 10  : Severe — consider action
# Note: age & age2 will have high VIF — this is NORMAL for quadratic terms

cat("\n--- VIF Check — Full Sample ---\n")
vif_full <- vif(model_full)
vif_df   <- data.frame(
  Variable  = names(vif_full),
  VIF       = round(vif_full, 3),
  Status    = ifelse(vif_full > 10, "High (expected for age²/interaction)",
                     ifelse(vif_full > 5,  "Moderate — monitor",
                            "OK"))
)
print(vif_df, row.names = FALSE)


# ════════════════════════════════════════════════
# STEP 4: MODEL SPECIFICATION (RESET Test)
# ════════════════════════════════════════════════

# RESET test checks if the functional form is correct
# H0: Model is correctly specified (no omitted nonlinear terms)
# H1: Model has functional form misspecification
# If p < 0.05 → may need to add squared terms or transform variables

reset_full   <- resettest(model_full,   power = 2:3, type = "fitted")
reset_female <- resettest(model_female, power = 2:3, type = "fitted")
reset_male   <- resettest(model_male,   power = 2:3, type = "fitted")

cat("\n--- RESET Test (Model Specification) ---\n")
cat(sprintf("Full Sample : F = %.3f, p = %.4f  %s\n",
            reset_full$statistic,   reset_full$p.value,
            ifelse(reset_full$p.value   < 0.05,
                   "→ Possible misspecification", "→ OK")))
cat(sprintf("Female      : F = %.3f, p = %.4f  %s\n",
            reset_female$statistic, reset_female$p.value,
            ifelse(reset_female$p.value < 0.05,
                   "→ Possible misspecification", "→ OK")))
cat(sprintf("Male        : F = %.3f, p = %.4f  %s\n",
            reset_male$statistic,   reset_male$p.value,
            ifelse(reset_male$p.value   < 0.05,
                   "→ Possible misspecification", "→ OK")))


# ════════════════════════════════════════════════
# STEP 5: BUILD TABLE 5 — DIAGNOSTICS SUMMARY
# ════════════════════════════════════════════════

table5 <- data.frame(
  Test = c(
    "Breusch-Pagan (Heteroskedasticity)",
    "Shapiro-Wilk (Normality — sample n=5000)",
    "RESET Test (Specification)",
    "VIF max — educ",
    "VIF max — age2",
    "VIF max — educxmatch"
  ),
  
  Full_Statistic = c(
    round(bp_full$statistic,    3),
    round(sw_full$statistic,    4),
    round(reset_full$statistic, 3),
    round(vif(model_full)["educ"],       3),
    round(vif(model_full)["age2"],       3),
    round(vif(model_full)["educxmatch"], 3)
  ),
  
  Full_pvalue = c(
    round(bp_full$p.value,    4),
    round(sw_full$p.value,    4),
    round(reset_full$p.value, 4),
    NA, NA, NA
  ),
  
  Full_Conclusion = c(
    ifelse(bp_full$p.value    < 0.05, "Detected → Robust SE applied", "Not detected"),
    ifelse(sw_full$p.value    < 0.05, "Non-normal → CLT applies (large N)", "Normal"),
    ifelse(reset_full$p.value < 0.05, "Possible misspecification", "OK"),
    ifelse(vif(model_full)["educ"]       > 10, "High", "OK"),
    "High — expected for quadratic term",
    ifelse(vif(model_full)["educxmatch"] > 10, "High — expected for interaction", "OK")
  )
)

cat("\n--- TABLE 5: Diagnostic Tests Summary ---\n")
print(table5, row.names = FALSE)


# ════════════════════════════════════════════════
# STEP 6: RESIDUAL PLOTS (Full Sample)
# ════════════════════════════════════════════════

# Save 4 standard diagnostic plots to one PNG file
png("figures/residual_plots.png",
    width = 3200, height = 2400, res = 300)

# Set layout to 2x2 grid
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Plot 1: Residuals vs Fitted
#         — checks linearity and homoskedasticity
#         — want: random scatter around horizontal line at 0
plot(model_full, which = 1,
     main = "Residuals vs Fitted",
     col  = "steelblue", pch = 16, cex = 0.3)

# Plot 2: Normal Q-Q plot of residuals
#         — checks normality of residuals
#         — want: points close to diagonal line
plot(model_full, which = 2,
     main = "Normal Q-Q Plot",
     col  = "steelblue", pch = 16, cex = 0.3)

# Plot 3: Scale-Location plot
#         — another check for heteroskedasticity
#         — want: horizontal red line with random scatter
plot(model_full, which = 3,
     main = "Scale-Location",
     col  = "steelblue", pch = 16, cex = 0.3)

# Plot 4: Residuals vs Leverage
#         — identifies influential observations (Cook's distance)
#         — want: no points beyond Cook's distance lines
plot(model_full, which = 5,
     main = "Residuals vs Leverage",
     col  = "steelblue", pch = 16, cex = 0.3)

dev.off()  # close PNG — MUST include this line
cat("\n✓ Saved: figures/residual_plots.png\n")


# ════════════════════════════════════════════════
# STEP 7: SAVE TABLE 5
# ════════════════════════════════════════════════

write.csv(table5,
          "tables/table5_diagnostics.csv",
          row.names = FALSE,
          na = "")
cat("✓ Saved: tables/table5_diagnostics.csv\n")

cat("\n✓ 04_diagnostics.R complete\n")
cat("  All diagnostic results saved.\n")