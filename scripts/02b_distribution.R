#========================================
# 02b_distribution.R
# Purpose: Check distribution of ln(wage) and detect outliers
# Input: data/processed/data_model.RData
# Output: figures/histogram_lnwage.png
#         figures/qqplot_lnwage.png
#         figures/boxplot_lnwage.png
#========================================

# LOAD PACKAGES
library(tidyverse)   # for data manipulation and ggplot2 charts


# LOAD DATA
load("data/processed/data_model.RData")

# Quick check — make sure data loaded correctly
cat("Observations loaded:", nrow(data_model), "\n")  # should be 16,470


# ========================================
# PART 1: HISTOGRAM OF ln(wage)
# ========================================

# Calculate mean and median to add as reference lines on the chart
mean_lnwage   <- mean(data_model$ln_wage)
median_lnwage <- median(data_model$ln_wage)

cat("Mean of ln_wage:  ", round(mean_lnwage, 4), "\n")
cat("Median of ln_wage:", round(median_lnwage, 4), "\n")

# Draw histogram
histogram_plot <- ggplot(data_model, aes(x = ln_wage)) +
  
  # Draw the bars — binwidth controls how wide each bar is
  geom_histogram(binwidth = 0.1,
                 fill = "steelblue",
                 color = "white",
                 alpha = 0.8) +
  
  # Add a vertical line for the mean (red dashed)
  geom_vline(aes(xintercept = mean_lnwage),
             color = "red", linetype = "dashed", linewidth = 0.8) +
  
  # Add a vertical line for the median (green dashed)
  geom_vline(aes(xintercept = median_lnwage),
             color = "darkgreen", linetype = "dashed", linewidth = 0.8) +
  
  # Add text labels for the lines
  annotate("text", x = mean_lnwage + 0.15, y = Inf,
           label = paste("Mean =", round(mean_lnwage, 2)),
           color = "red", vjust = 2, size = 3.5) +
  
  annotate("text", x = median_lnwage - 0.15, y = Inf,
           label = paste("Median =", round(median_lnwage, 2)),
           color = "darkgreen", vjust = 4, size = 3.5) +
  
  # Add titles and axis labels
  labs(
    title    = "Distribution of Log Monthly Wage",
    subtitle = "Manufacturing sector, Red River Delta - LFS 2018",
    x        = "ln(Monthly Wage)",
    y        = "Frequency"
  ) +
  
  # Clean white background theme
  theme_minimal()

# Display the chart in RStudio Plots panel
print(histogram_plot)

# Save the chart to figures/ folder
ggsave("figures/histogram_lnwage.png",
       plot   = histogram_plot,
       width  = 8,
       height = 5,
       dpi    = 300)

cat("✓ Saved: figures/histogram_lnwage.png\n")


# ========================================
# PART 2: QQ-PLOT OF ln(wage)
# ========================================

# A QQ-plot compares the actual distribution to a normal distribution
# If points follow the diagonal line closely → distribution is approximately normal
# If points deviate (curve up/down at the ends) → distribution has heavy tails

qq_plot <- ggplot(data_model, aes(sample = ln_wage)) +
  
  # Draw the actual data points
  stat_qq(color = "steelblue", alpha = 0.4, size = 0.8) +
  
  # Draw the reference line — this is what perfect normality looks like
  stat_qq_line(color = "red", linewidth = 0.8) +
  
  # Add titles and axis labels
  labs(
    title    = "Normal Q-Q Plot of Log Monthly Wage",
    subtitle = "Points close to the red line indicate approximate normality",
    x        = "Theoretical Quantiles",
    y        = "Sample Quantiles"
  ) +
  
  theme_minimal()

# Display the chart
print(qq_plot)

# Save the chart
ggsave("figures/qqplot_lnwage.png",
       plot   = qq_plot,
       width  = 7,
       height = 6,
       dpi    = 300)

cat("✓ Saved: figures/qqplot_lnwage.png\n")


# ========================================
# PART 3: OUTLIER DETECTION
# ========================================

# Method 1: Basic summary statistics

cat("\n--- Summary of ln_wage ---\n")
summary(data_model$ln_wage)

# Calculate key statistics
q1  <- quantile(data_model$ln_wage, 0.25)   # 25th percentile
q3  <- quantile(data_model$ln_wage, 0.75)   # 75th percentile
iqr <- q3 - q1                               # Interquartile range

# IQR rule: values outside [Q1 - 1.5*IQR, Q3 + 1.5*IQR] are mild outliers
#           values outside [Q1 - 3.0*IQR, Q3 + 3.0*IQR] are extreme outliers
lower_mild    <- q1 - 1.5 * iqr
upper_mild    <- q3 + 1.5 * iqr
lower_extreme <- q1 - 3.0 * iqr
upper_extreme <- q3 + 3.0 * iqr

cat("\n--- IQR-based outlier boundaries ---\n")
cat("Q1:", round(q1, 4), "\n")
cat("Q3:", round(q3, 4), "\n")
cat("IQR:", round(iqr, 4), "\n")
cat("Mild outlier range:    [", round(lower_mild, 4), ",", round(upper_mild, 4), "]\n")
cat("Extreme outlier range: [", round(lower_extreme, 4), ",", round(upper_extreme, 4), "]\n")

# Count how many observations fall outside each boundary
n_mild_outliers    <- sum(data_model$ln_wage < lower_mild |
                            data_model$ln_wage > upper_mild)
n_extreme_outliers <- sum(data_model$ln_wage < lower_extreme |
                            data_model$ln_wage > upper_extreme)

cat("\nMild outliers (1.5×IQR):   ", n_mild_outliers,
    sprintf("(%.2f%%)\n", n_mild_outliers / nrow(data_model) * 100))
cat("Extreme outliers (3.0×IQR):", n_extreme_outliers,
    sprintf("(%.2f%%)\n", n_extreme_outliers / nrow(data_model) * 100))


# Method 2: Look at extreme values directly

cat("\n--- 10 lowest ln_wage values ---\n")
print(sort(data_model$ln_wage)[1:10])

cat("\n--- 10 highest ln_wage values ---\n")
print(sort(data_model$ln_wage, decreasing = TRUE)[1:10])

# Convert back from log to original wage for easier interpretation
cat("\n--- In original wage (thousand VND) ---\n")
cat("10 lowest wages: ",
    round(exp(sort(data_model$ln_wage)[1:10]), 0), "\n")
cat("10 highest wages:",
    round(exp(sort(data_model$ln_wage, decreasing = TRUE)[1:10]), 0), "\n")


# Method 3: Boxplot to visualize outliers

boxplot_wage <- ggplot(data_model, aes(y = ln_wage)) +
  
  # Draw the boxplot — dots outside the whiskers are outliers
  geom_boxplot(fill = "steelblue", alpha = 0.6,
               outlier.color = "red",
               outlier.size  = 1.5,
               outlier.alpha = 0.5) +
  
  # Flip so boxplot is horizontal — easier to read
  coord_flip() +
  
  labs(
    title    = "Boxplot of Log Monthly Wage",
    subtitle = "Red dots indicate potential outliers (beyond 1.5 × IQR)",
    y        = "ln(Monthly Wage)",
    x        = ""
  ) +
  
  theme_minimal()

print(boxplot_wage)

ggsave("figures/boxplot_lnwage.png",
       plot   = boxplot_wage,
       width  = 8,
       height = 4,
       dpi    = 300)

cat("✓ Saved: figures/boxplot_lnwage.png\n")


# ========================================
# PART 4: DECISION ON OUTLIERS
# ========================================

# We choose NOT to remove outliers for the following reasons:
#
# 1. SAMPLE SIZE: With 16,470 observations, a small number of extreme
#    values has minimal influence on OLS estimates.
#
# 2. DATA VALIDITY: The extreme wage values (very low or very high)
#    likely reflect real workers in the manufacturing sector —
#    part-time workers at the low end, skilled managers at the high end.
#    Removing them would introduce selection bias.
#
# 3. LOG TRANSFORMATION: Taking ln(wage) already compresses the right
#    tail of the wage distribution significantly, reducing the influence
#    of high-wage outliers on the regression results.
#
# 4. OLS ROBUSTNESS: We will use heteroskedasticity-robust standard errors
#    in the regression (see 04_diagnostics.R), which further reduces
#    sensitivity to extreme observations.
#
# CONCLUSION: Keep all observations. No outliers removed.

cat("\n════════════════════════════════════════════\n")
cat("OUTLIER DECISION: No observations removed\n")
cat("Reason: Log transformation reduces skewness;\n")
cat("        extreme values reflect real wage variation;\n")
cat("        robust SE used in regression.\n")
cat("Final sample size:", nrow(data_model), "observations\n")
cat("════════════════════════════════════════════\n")
