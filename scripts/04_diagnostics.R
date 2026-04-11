# ================================================
# 04_diagnostics.R
# Purpose: Test OLS assumption violations
#          1. Heteroskedasticity (Breusch-Pagan test + White Test)
#          2. Normality of residuals (Shapiro-Wilk + Jarque-Bera)
#          3. Multicollinearity (VIF for all samples
#          4. Model specification (RESET test)
# Input:   data/processed/model_results.RData
# Output:  tables/table5_diagnostics.csv
#          figures/residual_plots.png
# ================================================

library(tidyverse)
library(lmtest)   # for bptest(), resettest()
library(sandwich) # for vcovHC()
library(car)      # for vif()
library(tseries)  # for jarque.bera.test()


# LOAD MODEL RESULTS FROM 03
load("data/processed/model_results.RData")
cat("Models loaded successfully!\n")


# ════════════════════════════════════════════════
# STEP 1: HETEROSKEDASTICITY TEST (Breusch-Pagan + White)
# ════════════════════════════════════════════════
# ------ Breusch-Pagan Test -----
# H0: Residuals have constant variance (homoskedasticity)
# H1: Residuals have non-constant variance (heteroskedasticity)
# If p < 0.05 → reject H0 → heteroskedasticity present
#              → must use robust standard errors (already done in 03)

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
cat("Note: Robust SE (HC3) already applied in regression → this is handled\n")

# ------ White Test ------
# White test is a more general version of Breusch-Pagan.
# It tests for heteroskedasticity WITHOUT assuming a specific functional form
# by regressing squared residuals on fitted values and fitted values squared.
# This is the simplified White test (Harvey-Godfrey version):
#   aux regression: e^2 ~ yhat + yhat^2
#   test statistic: n * R^2 ~ Chi-squared(2)
# H0: Homoskedasticity
# H1: Heteroskedasticity of unknown form
# Advantage: catches nonlinear forms of heteroskedasticity that BP may miss

white_test <- function(model) {
  resid_sq  <- residuals(model)^2
  fitted_v  <- fitted(model)
  fitted_sq <- fitted(model)^2
  aux_model <- lm(resid_sq ~ fitted_v + fitted_sq)
  r2   <- summary(aux_model)$r.squared
  n    <- length(resid_sq)
  stat <- n * r2                                    # n*R^2 ~ Chi-sq(2)
  p_val <- pchisq(stat, df = 2, lower.tail = FALSE)
  return(list(statistic = stat, p.value = p_val))
}

wt_full   <- white_test(model_full)
wt_female <- white_test(model_female)
wt_male   <- white_test(model_male)

cat("\n--- White Test (Heteroskedasticity - General Form) ---\n")
cat(sprintf("Full Sample : Chi-sq = %.3f, p = %.4f  %s\n",
            wt_full$statistic,   wt_full$p.value,
            ifelse(wt_full$p.value   < 0.05, "-> Heteroskedasticity detected", "-> No issue")))
cat(sprintf("Female      : Chi-sq = %.3f, p = %.4f  %s\n",
            wt_female$statistic, wt_female$p.value,
            ifelse(wt_female$p.value < 0.05, "-> Heteroskedasticity detected", "-> No issue")))
cat(sprintf("Male        : Chi-sq = %.3f, p = %.4f  %s\n",
            wt_male$statistic,   wt_male$p.value,
            ifelse(wt_male$p.value   < 0.05, "-> Heteroskedasticity detected", "-> No issue")))
cat("Note: Both BP and White tests confirm heteroskedasticity -> Robust SE (HC3) appropriate\n")

# ════════════════════════════════════════════════
# STEP 2: NORMALITY OF RESIDUALS (Shapiro-Wilk)
# ════════════════════════════════════════════════

# ------ Shapiro-Wilk Test ------
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

# ------ Jarque-Bera Test ------
# Tests normality via SKEWNESS and KURTOSIS of the full residual distribution.
# JB = (n/6) * [S^2 + (K-3)^2/4]  ~  Chi-squared(2)
#   where S = skewness, K = kurtosis of residuals
# H0: S = 0 and K = 3  (i.e., residuals are normally distributed)
# H1: Residuals depart from normality in skewness or kurtosis (or both)
# Advantage over Shapiro-Wilk:
#   - Can be applied to the FULL sample (no n <= 5000 constraint)
#   - Directly identifies the source of non-normality (skew vs. fat tails)
# Caveat: still very sensitive with large N — use with QQ-plot for context

jb_full   <- jarque.bera.test(residuals(model_full))
jb_female <- jarque.bera.test(residuals(model_female))
jb_male   <- jarque.bera.test(residuals(model_male))

cat("\n--- Jarque-Bera Test (Normality - full sample) ---\n")
cat(sprintf("Full Sample : JB = %.3f, p = %.4f  %s\n",
            jb_full$statistic,   jb_full$p.value,
            ifelse(jb_full$p.value   < 0.05, "-> Non-normal (skewness/kurtosis)", "-> Normal")))
cat(sprintf("Female      : JB = %.3f, p = %.4f  %s\n",
            jb_female$statistic, jb_female$p.value,
            ifelse(jb_female$p.value < 0.05, "-> Non-normal (skewness/kurtosis)", "-> Normal")))
cat(sprintf("Male        : JB = %.3f, p = %.4f  %s\n",
            jb_male$statistic,   jb_male$p.value,
            ifelse(jb_male$p.value   < 0.05, "-> Non-normal (skewness/kurtosis)", "-> Normal")))
cat("Note: Rejection expected with large N; CLT ensures valid OLS inference regardless.\n")

# ════════════════════════════════════════════════
# STEP 3: MULTICOLLINEARITY (VIF)
# ════════════════════════════════════════════════

# VIF < 5   : No problem
# VIF 5-10  : Moderate - monitor
# VIF > 10  : Severe - consider action
# Note: age & age2 will have high VIF - this is NORMAL for quadratic terms

print_vif <- function(model, label) {
  cat(sprintf("\n--- VIF Check - %s ---\n", label))
  vif_vals <- vif(model)
  vif_df <- data.frame(
    Variable  = names(vif_vals),
    VIF       = round(vif_vals, 3),
    Tolerance = round(1 / vif_vals, 3),
    Status    = ifelse(vif_vals > 10,
                       "High (expected for quadratic/interaction)",
                       ifelse(vif_vals > 5, "Moderate - monitor", "OK"))
  )
  print(vif_df, row.names = FALSE)
  return(vif_vals)
}

vif_full_vals   <- print_vif(model_full,   "Full Sample")
vif_female_vals <- print_vif(model_female, "Female Subsample")
vif_male_vals   <- print_vif(model_male,   "Male Subsample")


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

# Helper functions for conclusions
conclude_hetero    <- function(p) ifelse(p < 0.05, "Detected -> Robust SE applied", "Not detected")
conclude_normality <- function(p) ifelse(p < 0.05, "Non-normal -> CLT applies (large N)", "Normal")
conclude_reset     <- function(p) ifelse(p < 0.05, "Possible misspecification", "OK")
conclude_vif       <- function(v, label) ifelse(v > 10, paste0("High - expected for ", label), "OK")

table5 <- data.frame(
  Test = c(
    "Breusch-Pagan Test (Heteroskedasticity)",
    "White Test (Heteroskedasticity)",
    "Shapiro-Wilk Test (Normality, n=5000)",
    "Jarque-Bera Test (Normality, full N)",
    "RESET Test (Model Specification)",
    "VIF - educ",
    "VIF - age",
    "VIF - age2",
    "VIF - match",
    "VIF - educxmatch"
  ),
  
  # Full Sample
  Full_Statistic = c(
    round(bp_full$statistic,    3),
    round(wt_full$statistic,    3),
    round(sw_full$statistic,    4),
    round(jb_full$statistic,    3),
    round(reset_full$statistic, 3),
    round(vif_full_vals["educ"],       3),
    round(vif_full_vals["age"],        3),
    round(vif_full_vals["age2"],       3),
    round(vif_full_vals["match"],      3),
    round(vif_full_vals["educxmatch"], 3)
  ),
  
  Full_pvalue = c(
    round(bp_full$p.value,    4),
    round(wt_full$p.value,    4),
    round(sw_full$p.value,    4),
    round(jb_full$p.value,    4),
    round(reset_full$p.value, 4),
    NA, NA, NA, NA, NA
  ),
  
  Full_Conclusion = c(
    conclude_hetero(bp_full$p.value),
    conclude_hetero(wt_full$p.value),
    conclude_normality(sw_full$p.value),
    conclude_normality(jb_full$p.value),
    conclude_reset(reset_full$p.value),
    conclude_vif(vif_full_vals["educ"],       "educ"),
    conclude_vif(vif_full_vals["age"],        "quadratic term"),
    conclude_vif(vif_full_vals["age2"],       "quadratic term"),
    conclude_vif(vif_full_vals["match"],      "interaction term"),
    conclude_vif(vif_full_vals["educxmatch"], "interaction term")
  ),
  
  # Female Subsample
  Female_Statistic = c(
    round(bp_female$statistic,    3),
    round(wt_female$statistic,    3),
    round(sw_female$statistic,    4),
    round(jb_female$statistic,    3),
    round(reset_female$statistic, 3),
    round(vif_female_vals["educ"],       3),
    round(vif_female_vals["age"],        3),
    round(vif_female_vals["age2"],       3),
    round(vif_female_vals["match"],      3),
    round(vif_female_vals["educxmatch"], 3)
  ),
  
  Female_pvalue = c(
    round(bp_female$p.value,    4),
    round(wt_female$p.value,    4),
    round(sw_female$p.value,    4),
    round(jb_female$p.value,    4),
    round(reset_female$p.value, 4),
    NA, NA, NA, NA, NA
  ),
  
  Female_Conclusion = c(
    conclude_hetero(bp_female$p.value),
    conclude_hetero(wt_female$p.value),
    conclude_normality(sw_female$p.value),
    conclude_normality(jb_female$p.value),
    conclude_reset(reset_female$p.value),
    conclude_vif(vif_female_vals["educ"],       "educ"),
    conclude_vif(vif_female_vals["age"],        "quadratic term"),
    conclude_vif(vif_female_vals["age2"],       "quadratic term"),
    conclude_vif(vif_female_vals["match"],      "interaction term"),
    conclude_vif(vif_female_vals["educxmatch"], "interaction term")
  ),
  
  # Male Subsample
  Male_Statistic = c(
    round(bp_male$statistic,    3),
    round(wt_male$statistic,    3),
    round(sw_male$statistic,    4),
    round(jb_male$statistic,    3),
    round(reset_male$statistic, 3),
    round(vif_male_vals["educ"],       3),
    round(vif_male_vals["age"],        3),
    round(vif_male_vals["age2"],       3),
    round(vif_male_vals["match"],      3),
    round(vif_male_vals["educxmatch"], 3)
  ),
  
  Male_pvalue = c(
    round(bp_male$p.value,    4),
    round(wt_male$p.value,    4),
    round(sw_male$p.value,    4),
    round(jb_male$p.value,    4),
    round(reset_male$p.value, 4),
    NA, NA, NA, NA, NA
  ),
  
  Male_Conclusion = c(
    conclude_hetero(bp_male$p.value),
    conclude_hetero(wt_male$p.value),
    conclude_normality(sw_male$p.value),
    conclude_normality(jb_male$p.value),
    conclude_reset(reset_male$p.value),
    conclude_vif(vif_male_vals["educ"],       "educ"),
    conclude_vif(vif_male_vals["age"],        "quadratic term"),
    conclude_vif(vif_male_vals["age2"],       "quadratic term"),
    conclude_vif(vif_male_vals["match"],      "interaction term"),
    conclude_vif(vif_male_vals["educxmatch"], "interaction term")
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
# plot(model_full, which = 1, main = "Residuals vs Fitted", col  = "steelblue", pch = 16, cex = 0.3)
plot(model_full, which = 1,
     id.n = 0,                      
     caption = "", sub.caption = "",
     col  = "steelblue", pch = 16, cex = 0.3)
title("Residuals vs Fitted", line = 1)

# Plot 2: Normal Q-Q plot of residuals
#         — checks normality of residuals
#         — want: points close to diagonal line
# plot(model_full, which = 2, main = "Normal Q-Q Plot", col  = "steelblue", pch = 16, cex = 0.3)
plot(model_full, which = 2,
     id.n = 0,
     caption = "", sub.caption = "",
     col  = "steelblue", pch = 16, cex = 0.3)
title("Normal Q-Q Plot", line = 1)

# Plot 3: Scale-Location plot
#         — another check for heteroskedasticity
#         — want: horizontal red line with random scatter
# plot(model_full, which = 3,main = "Scale-Location",col  = "steelblue", pch = 16, cex = 0.3)
plot(model_full, which = 3,
     id.n = 0,
     caption = "", sub.caption = "",
     col  = "steelblue", pch = 16, cex = 0.3)
title("Scale-Location", line = 1)
#title(ylab = expression(sqrt("|Standardized residuals|")))

# Plot 4: Residuals vs Leverage
#         — identifies influential observations (Cook's distance)
#         — want: no points beyond Cook's distance lines
# plot(model_full, which = 5,main = "Residuals vs Leverage", col  = "steelblue", pch = 16, cex = 0.3)
plot(model_full, which = 5,
     id.n = 3,
     caption = "", sub.caption = "",
     col  = "steelblue", pch = 16, cex = 0.3)
title("Residuals vs Leverage", line = 1)

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