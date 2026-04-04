#========================================
# 04_diagnostics.R
# Purpose: OLS Assumption Diagnostic Tests
#   1. Heteroskedasticity (Breusch-Pagan + White)
#   2. Normality of residuals (Shapiro-Wilk + Jarque-Bera)
#   3. Multicollinearity (VIF) — with mean-centering fix
#   4. Model misspecification (Ramsey RESET)
#   5. Robust Standard Errors (HC3) — fix for heteroskedasticity
# Output: Table 5 — Diagnostic Tests Summary (.csv)
#========================================

# Install if needed:
install.packages(c("car", "lmtest", "tseries", "sandwich"))
install.packages("sandwich")

library(tidyverse)
library(car)      
library(lmtest)    
library(tseries)   
library(sandwich)   


#----------------------------------------
# Load data
#----------------------------------------
base_path <- "C:/Users/HP/Downloads/Group_5_Econometrics_Project"
#Tùy máy nha cân nhắc xóa dòng này nha

load(file.path(base_path, "data/processed/data_model.RData"))
load(file.path(base_path, "data/processed/sub1_female.RData"))
load(file.path(base_path, "data/processed/sub2_male.RData"))


#========================================
# STEP 0: Mean-center educ and match
# to reduce multicollinearity in interaction term
#========================================

#Tạo các biến mới do cor cao nên các biến tương tác cần dùng Mean-center trước khi tạo biến tương tác
center_vars <- function(df) {
  df %>%
    mutate(
      educ_c     = educ  - mean(educ,  na.rm = TRUE),
      match_c    = match - mean(match, na.rm = TRUE),
      educxmatch = educ_c * match_c
    )
}

data_model  <- center_vars(data_model)
sub1_female <- center_vars(sub1_female)
sub2_male   <- center_vars(sub2_male)

cat("✓ Mean-centering applied to educ and match\n")
cat(sprintf("  educ  mean: %.3f\n", mean(data_model$educ,  na.rm = TRUE)))
cat(sprintf("  match mean: %.3f\n", mean(data_model$match, na.rm = TRUE)))


#----------------------------------------
# Model formula (using centered vars)
#----------------------------------------
formula_full <- ln_wage ~ educ_c + age + age2 + hours +
  male + cert + contract + state + fdi +
  match_c + educxmatch + bhxh

formula_sub <- ln_wage ~ educ_c + age + age2 + hours +
  cert + contract + state + fdi +
  match_c + educxmatch + bhxh


#----------------------------------------
# Fit models
#----------------------------------------
model_full   <- lm(formula_full, data = data_model  %>% na.omit())
model_female <- lm(formula_sub,  data = sub1_female %>% na.omit())
model_male   <- lm(formula_sub,  data = sub2_male   %>% na.omit())

cat("\n Models fitted successfully\n")
cat(sprintf("  Full sample n   = %d\n", nobs(model_full)))
cat(sprintf("  Female sample n = %d\n", nobs(model_female)))
cat(sprintf("  Male sample n   = %d\n", nobs(model_male)))


#========================================
# FUNCTION: Run all diagnostics
#========================================
run_diagnostics <- function(model, label) {
  
  cat("\n", strrep("=", 55), "\n")
  cat("DIAGNOSTIC TESTS —", label, "\n")
  cat(strrep("=", 55), "\n")
  
  results <- list()
  
  # ---- 1. HETEROSKEDASTICITY ----
  cat("\n--- 1. Heteroskedasticity ---\n")
  
  bp <- bptest(model)
  cat(sprintf("Breusch-Pagan: BP = %.4f, p = %.4f  %s\n",
              bp$statistic, bp$p.value,
              ifelse(bp$p.value < 0.05, "Reject H0", "Fail to reject H0")))
  
  results[["Breusch-Pagan"]] <- data.frame(
    Sample     = label,
    Test       = "Breusch-Pagan Test",
    Statistic  = round(as.numeric(bp$statistic), 4),
    P_value    = round(bp$p.value, 4),
    Conclusion = ifelse(bp$p.value < 0.05,
                        "Reject H0 - Heteroskedasticity present",
                        "Fail to reject H0 - Homoskedasticity"),
    stringsAsFactors = FALSE
  )
  
  white <- bptest(model, ~ fitted(model) + I(fitted(model)^2))
  cat(sprintf("White Test:    W  = %.4f, p = %.4f  %s\n",
              white$statistic, white$p.value,
              ifelse(white$p.value < 0.05, "Reject H0", "Fail to reject H0")))
  
  results[["White"]] <- data.frame(
    Sample     = label,
    Test       = "White Test",
    Statistic  = round(as.numeric(white$statistic), 4),
    P_value    = round(white$p.value, 4),
    Conclusion = ifelse(white$p.value < 0.05,
                        "Reject H0 - Heteroskedasticity present",
                        "Fail to reject H0 - Homoskedasticity"),
    stringsAsFactors = FALSE
  )
  
  # ---- 2. NORMALITY OF RESIDUALS ----
  # cân nhắc có thể bỏ kiểm định phần dư vì mẫu đang quá lớn nên nhìn sơ đồ nó gần chuẩn là ok rùi
  cat("\n--- 2. Normality of Residuals ---\n")
  
  resid_vals <- residuals(model)
  n          <- length(resid_vals)
  
  jb <- jarque.bera.test(resid_vals)
  cat(sprintf("Jarque-Bera:   JB = %.4f, p = %.4f  %s\n",
              jb$statistic, jb$p.value,
              ifelse(jb$p.value < 0.05, "Reject H0", "Fail to reject H0")))
  
  results[["Jarque-Bera"]] <- data.frame(
    Sample     = label,
    Test       = "Jarque-Bera Test",
    Statistic  = round(as.numeric(jb$statistic), 4),
    P_value    = round(jb$p.value, 4),
    Conclusion = ifelse(jb$p.value < 0.05,
                        "Reject H0 - Residuals not normal",
                        "Fail to reject H0 - Residuals normal"),
    stringsAsFactors = FALSE
  )
  
  if (n <= 5000) {
    sw <- shapiro.test(resid_vals)
  } else {
    set.seed(42)
    sw <- shapiro.test(sample(resid_vals, 5000))
  }
  sw_label <- ifelse(n > 5000,
                     "Shapiro-Wilk Test (n=5000 sample)",
                     "Shapiro-Wilk Test")
  cat(sprintf("Shapiro-Wilk:  W  = %.4f, p = %.4f  %s%s\n",
              sw$statistic, sw$p.value,
              ifelse(sw$p.value < 0.05, "Reject H0", "Fail to reject H0"),
              ifelse(n > 5000, " [sampled]", "")))
  
  results[["Shapiro-Wilk"]] <- data.frame(
    Sample     = label,
    Test       = sw_label,
    Statistic  = round(as.numeric(sw$statistic), 4),
    P_value    = round(sw$p.value, 4),
    Conclusion = ifelse(sw$p.value < 0.05,
                        "Reject H0 - Residuals not normal",
                        "Fail to reject H0 - Residuals normal"),
    stringsAsFactors = FALSE
  )
  
  # ---- 3. MULTICOLLINEARITY ----
  cat("\n--- 3. Multicollinearity (VIF) ---\n")
  
  vif_vals    <- vif(model)
  max_vif     <- max(vif_vals)
  mean_vif    <- mean(vif_vals)
  vif_no_quad <- vif_vals[!names(vif_vals) %in% c("age", "age2")]
  max_vif_adj <- max(vif_no_quad)
  
  cat(sprintf("Max VIF (all):            %.3f\n", max_vif))
  cat(sprintf("Max VIF (excl. age/age2): %.3f\n", max_vif_adj))
  cat(sprintf("Mean VIF:                 %.3f\n", mean_vif))
  print(round(vif_vals, 3))
  
  results[["VIF"]] <- data.frame(
    Sample     = label,
    Test       = "Multicollinearity - Max VIF (excl. age/age2)",
    Statistic  = round(max_vif_adj, 4),
    P_value    = NA,
    Conclusion = ifelse(max_vif_adj > 10,
                        "Severe multicollinearity detected",
                        ifelse(max_vif_adj > 5,
                               "Moderate multicollinearity - monitor",
                               "No multicollinearity problem (VIF < 5)")),
    stringsAsFactors = FALSE
  )
  
  # ---- 4. RAMSEY RESET TEST ----
  cat("\n--- 4. Ramsey RESET Test ---\n")
  
  reset <- resettest(model, power = 2:3, type = "fitted")
  cat(sprintf("RESET Test:    F = %.4f, p = %.4f  %s\n",
              reset$statistic, reset$p.value,
              ifelse(reset$p.value < 0.05, "Reject H0", "Fail to reject H0")))
  
  results[["RESET"]] <- data.frame(
    Sample     = label,
    Test       = "Ramsey RESET Test",
    Statistic  = round(as.numeric(reset$statistic), 4),
    P_value    = round(reset$p.value, 4),
    Conclusion = ifelse(reset$p.value < 0.05,
                        "Reject H0 - Model misspecification detected",
                        "Fail to reject H0 - No misspecification"),
    stringsAsFactors = FALSE
  )
  
  table_out <- bind_rows(results)
  return(table_out)
}


#========================================
# RUN DIAGNOSTICS FOR ALL SAMPLES
#========================================

table_full   <- run_diagnostics(model_full,   "Full Sample")
table_female <- run_diagnostics(model_female, "Subsample 1: Female")
table_male   <- run_diagnostics(model_male,   "Subsample 2: Male")


#========================================
# TABLE 5 — COMBINED SUMMARY
#========================================

table5_all <- bind_rows(table_full, table_female, table_male)

cat("\n", strrep("=", 60), "\n")
cat("TABLE 5: DIAGNOSTIC TESTS SUMMARY\n")
cat(strrep("=", 60), "\n")
print(table5_all, row.names = FALSE)


#========================================
# EXPORT TABLE 5 TO CSV
#========================================

out_path <- file.path(base_path, "data/processed/table5_diagnostics.csv")
write.csv(table5_all, out_path, row.names = FALSE)
cat("\n✓ Table 5 saved to:", out_path, "\n")


#========================================
# STEP 5: Robust Standard Errors (HC3)
# Dùng khi có heteroskedasticity
#========================================

cat("\n", strrep("=", 55), "\n")
cat("ROBUST STANDARD ERRORS (HC3) — Full Sample\n")
cat(strrep("=", 55), "\n")
print(coeftest(model_full, vcov = vcovHC(model_full, type = "HC3")))

cat("\n", strrep("=", 55), "\n")
cat("ROBUST STANDARD ERRORS (HC3) — Female\n")
cat(strrep("=", 55), "\n")
print(coeftest(model_female, vcov = vcovHC(model_female, type = "HC3")))

cat("\n", strrep("=", 55), "\n")
cat("ROBUST STANDARD ERRORS (HC3) — Male\n")
cat(strrep("=", 55), "\n")
print(coeftest(model_male, vcov = vcovHC(model_male, type = "HC3")))


#========================================
# STEP 6: Residual Plots
#========================================

par(mfrow = c(2, 3))
plot(model_full,   which = 1, main = "Full: Residuals vs Fitted")
plot(model_full,   which = 2, main = "Full: Normal Q-Q")
hist(residuals(model_full), breaks = 50, col = "steelblue",
     main = "Full: Residual Histogram", xlab = "Residuals")
plot(model_female, which = 2, main = "Female: Normal Q-Q")
plot(model_male,   which = 2, main = "Male: Normal Q-Q")
par(mfrow = c(1, 1))

cat("\n✓ Done! All diagnostics completed.\n")
cat("\nNOTE: Large samples (n > 1000) often reject normality\n")
cat("and heteroskedasticity tests even with minor violations.\n")
cat("Use Robust SE (HC3) results above for final inference.\n")
