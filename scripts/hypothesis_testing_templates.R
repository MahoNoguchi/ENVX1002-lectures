# =============================================================================
# Hypothesis Testing Templates — ENVX1002
# =============================================================================
# Each section follows the HATPC framework:
#   H — Hypotheses       (state H0 and H1)
#   A — Assumptions      (check before running the test)
#   T — Test statistic   (run the test in R)
#   P — P-value          (read the output)
#   C — Conclusion       (statistical + scientific conclusion)
#
# Plotting uses ggplot2 (loaded once below) unless noted as "base R".
# Replace the example data / variable names with your own dataset.
# =============================================================================

library(ggplot2)   # for all visualisations (ggplot2)
library(patchwork) # for combining ggplot2 panels side-by-side
library(e1071)     # for skewness() and kurtosis() in Assumptions checks
# library(car)    # for leveneTest() — loaded in Section 2; install if needed


# =============================================================================
# 1. ONE-SAMPLE t-TEST
# =============================================================================
# Use when: comparing the mean of a single sample to a known/hypothesised value.
# Example question: "Is the mean TN concentration different from 500 µg/L?"
# =============================================================================

# --- Example data ------------------------------------------------------------
# Replace with your own numeric vector.
set.seed(42)
x <- rnorm(30, mean = 510, sd = 80)   # single sample of continuous measurements
mu0 <- 500                            # hypothesised population mean

# --- H: Hypotheses -----------------------------------------------------------
# Two-tailed (default — use unless you have a directional hypothesis):
#   H0: mu = mu0
#   H1: mu ≠ mu0
#
# One-tailed (greater):
#   H0: mu ≤ mu0
#   H1: mu  > mu0
#
# One-tailed (less):
#   H0: mu ≥ mu0
#   H1: mu  < mu0

# --- A: Assumptions ----------------------------------------------------------
# Assumption: the sample is approximately normally distributed.
# Check with a histogram and Q-Q plot (ggplot2).

df1 <- data.frame(x = x)

p_hist <- ggplot(df1, aes(x)) +
  geom_histogram(fill = "grey80", colour = "white", bins = 15) +
  labs(title = "Histogram", x = "Measurement", y = "Count") +
  theme_minimal()

p_qq <- ggplot(df1, aes(sample = x)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  labs(title = "Q-Q plot", x = "Theoretical", y = "Sample") +
  theme_minimal()

p_hist + p_qq   # display side-by-side (patchwork)

# Optional formal normality test (best for n < 50):
shapiro.test(x)
# If p > 0.05 → no strong evidence against normality → proceed with t-test.
# If p < 0.05 → consider a log/sqrt/reciprocal transformation, then recheck.

# Outlier detection and removal (apply if histogram / Q-Q plot suggests outliers)
# Method: IQR rule — values below Q1 − 1.5×IQR or above Q3 + 1.5×IQR are flagged.

q_x   <- quantile(x, probs = c(0.25, 0.75))
iqr_x <- IQR(x)
lower_x <- q_x[1] - 1.5 * iqr_x
upper_x <- q_x[2] + 1.5 * iqr_x

# Identify and inspect the outlying values:
x[x < lower_x | x > upper_x]          # print suspected outliers

# Remove outliers — creates a cleaned version of the vector:
x_clean <- x[x >= lower_x & x <= upper_x]

# Recheck normality on the cleaned data before proceeding:
shapiro.test(x_clean)

# Optional: visualise the cleaned data to confirm outliers are gone.
df1_clean <- data.frame(x = x_clean)
p_hist_clean <- ggplot(df1_clean, aes(x)) +
  geom_histogram(fill = "grey80", colour = "white", bins = 15) +
  labs(title = "Histogram (outliers removed)", x = "Measurement", y = "Count") +
  theme_minimal()
p_qq_clean <- ggplot(df1_clean, aes(sample = x)) +
  stat_qq() + stat_qq_line(colour = "red") +
  labs(title = "Q-Q plot (outliers removed)", x = "Theoretical", y = "Sample") +
  theme_minimal()
p_hist_clean + p_qq_clean

# Use x_clean in place of x for all subsequent steps (skewness, t-test, etc.).

# Skewness and kurtosis (e1071)
# Skewness: values between -0.5 and 0.5 are considered acceptably symmetrical.
#   > 0.5 or < -0.5 → moderate skew; > 1 or < -1 → high skew → consider transform.
# Kurtosis: excess kurtosis (e1071 default). 0 = normal tails.
#   > 1 (leptokurtic, heavy tails / peaked) → investigate outliers.
#   < -1 (platykurtic, light tails / flat) → consider transform.
skewness(x)
kurtosis(x)

# Transformation and back-transformation (apply if normality is violated)
# Common options: log (positive, right-skewed), sqrt (counts / mild skew),
#                 1/x (strong right skew — all values must be > 0).
# Apply the SAME transformation to the hypothesised mean (mu0).

x_log   <- log(x)          # natural log  — most common for environmental data
x_sqrt  <- sqrt(x)         # square root
x_recip <- 1 / x           # reciprocal

# Recheck normality on the transformed variable (shown for log):
shapiro.test(x_log)
skewness(x_log)
kurtosis(x_log)

# Run the one-sample t-test on the log scale:
mu0_log <- log(mu0)                # transform mu0 to the same scale
result_1samp_log <- t.test(
  x_log,
  mu          = mu0_log,
  alternative = "two.sided",
  conf.level  = 0.95
)
result_1samp_log

# Back-transform to the original scale:
# exp() reverses log(); the CI becomes a CI for the geometric mean.
exp(result_1samp_log$estimate)   # geometric mean of x
exp(result_1samp_log$conf.int)   # 95% CI on the original scale
# Interpret: "The geometric mean [variable] was [not] significantly different
#             from [mu0] (t_[df] = [t], p = [p]; 95% CI: [lower]–[upper])."

# For sqrt: back-transform by squaring CI limits.
#   result_sqrt <- t.test(x_sqrt, mu = sqrt(mu0), ...)
#   result_sqrt$conf.int ^ 2

# For reciprocal: back-transform by 1/CI limits (note: order of limits reverses).
#   result_recip <- t.test(x_recip, mu = 1/mu0, ...)
#   1 / rev(result_recip$conf.int)

# --- T: Test statistic -------------------------------------------------------
# base R — t.test()
result_1samp <- t.test(
  x,
  mu          = mu0,
  alternative = "two.sided",  # or "greater" / "less"
  conf.level  = 0.95
)
result_1samp
# Key output: t-statistic, df (= n-1), p-value, 95% CI, sample mean.

# --- P: P-value --------------------------------------------------------------
result_1samp$p.value   # extract p-value
result_1samp$conf.int  # extract confidence interval

# --- C: Conclusion -----------------------------------------------------------
# Statistical conclusion:
#   If p < 0.05 → reject H0 (evidence that mean ≠ mu0)
#   If p ≥ 0.05 → fail to reject H0 (insufficient evidence of a difference)
#
# Template:
#   "The mean [variable] was [not] significantly different from [mu0]
#    (t_[df] = [t], p = [p]; 95% CI: [lower]–[upper])."
#
# Scientific conclusion: interpret in the context of your research question.


# =============================================================================
# 2. TWO-SAMPLE t-TEST  (independent groups)
# =============================================================================
# Use when: comparing the means of two independent groups.
# Example question: "Does Red Bull increase heart rate compared to a control?"
# =============================================================================

# --- Example data ------------------------------------------------------------
# Replace with your own data frame. Requires:
#   - one column of continuous measurements (response)
#   - one column of group labels (group)
set.seed(7)
df2 <- data.frame(
  group    = rep(c("treatment", "control"), each = 20),
  response = c(rnorm(20, mean = 82, sd = 10),
               rnorm(20, mean = 76, sd = 10))
)

# --- H: Hypotheses -----------------------------------------------------------
# Two-tailed:
#   H0: mu_treatment = mu_control
#   H1: mu_treatment ≠ mu_control

# --- A: Assumptions ----------------------------------------------------------
# 1. Normality within each group.
# 2. Homogeneity of variance (equal variances) — checked with Levene's test.

# Histogram per group (ggplot2)
ggplot(df2, aes(x = response, fill = group)) +
  geom_histogram(binwidth = 5, colour = "black", alpha = 0.5) +
  facet_wrap(~group) +
  labs(title = "Histograms by group", x = "Response", y = "Count") +
  theme_classic()

# Q-Q plot per group (ggplot2)
ggplot(df2, aes(sample = response)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  facet_wrap(~group) +
  labs(title = "Q-Q plots by group") +
  theme_classic()

# Boxplot — also useful for checking equal spread (ggplot2)
ggplot(df2, aes(x = group, y = response, fill = group)) +
  geom_boxplot(alpha = 0.4) +
  labs(title = "Boxplot by group", x = "Group", y = "Response") +
  theme_classic()

# Shapiro-Wilk test per group (base R)
shapiro.test(df2$response[df2$group == "treatment"])
shapiro.test(df2$response[df2$group == "control"])

# Outlier detection and removal per group (apply if histogram / Q-Q plot suggests outliers)
# The IQR rule is applied separately within each group so that group-specific
# spread is respected.

is_outlier_iqr <- function(y) {
  q  <- quantile(y, probs = c(0.25, 0.75))
  iq <- IQR(y)
  y < (q[1] - 1.5 * iq) | y > (q[2] + 1.5 * iq)
}

# Flag outlier rows (TRUE = outlier):
outlier_flags <- ave(df2$response, df2$group,
                     FUN = function(y) is_outlier_iqr(y)) == 1

# Inspect the suspected outlying rows:
df2[outlier_flags, ]

# Remove outliers — creates a cleaned version of the data frame:
df2_clean <- df2[!outlier_flags, ]

# Recheck normality per group on the cleaned data:
shapiro.test(df2_clean$response[df2_clean$group == "treatment"])
shapiro.test(df2_clean$response[df2_clean$group == "control"])

# Optional: visualise the cleaned data.
ggplot(df2_clean, aes(x = response, fill = group)) +
  geom_histogram(binwidth = 5, colour = "black", alpha = 0.5) +
  facet_wrap(~group) +
  labs(title = "Histograms by group (outliers removed)", x = "Response", y = "Count") +
  theme_classic()

# Use df2_clean in place of df2 for all subsequent steps
# (skewness, Levene's test, t-test, etc.).

# Skewness and kurtosis per group (e1071)
# Confirm symmetry within each group before relying on the t-test.
tapply(df2$response, df2$group, skewness)
tapply(df2$response, df2$group, kurtosis)
# Interpret as for Section 1: |skewness| < 0.5 and kurtosis near 0 → normal-like.

# Transformation and back-transformation (apply if normality is violated)
# Apply the SAME transformation to BOTH groups; choose based on skewness/kurtosis.

df2$response_log  <- log(df2$response)   # natural log  — positive right-skewed data
df2$response_sqrt <- sqrt(df2$response)  # square root  — count data / mild skew

# Recheck normality on the transformed variable (shown for log):
tapply(df2$response_log, df2$group, shapiro.test)
tapply(df2$response_log, df2$group, skewness)
tapply(df2$response_log, df2$group, kurtosis)

# Run Welch's t-test on the log scale:
result_welch_log <- t.test(
  response_log ~ group,
  data        = df2,
  var.equal   = FALSE,
  alternative = "two.sided",
  conf.level  = 0.95
)
result_welch_log

# Back-transform to the original scale:
# On the log scale, the difference of means is a log-ratio; exp() gives a ratio
# of geometric means on the original scale.
exp(result_welch_log$estimate)   # geometric mean per group
exp(result_welch_log$conf.int)   # 95% CI for the ratio of geometric means
# Interpret: "The geometric mean [response] in [treatment] was [X]-fold the
#             geometric mean in [control] (95% CI: [lower]–[upper], p = [p])."

# For sqrt back-transformation: square the CI limits.
#   result_sqrt2 <- t.test(response_sqrt ~ group, data = df2, ...)
#   result_sqrt2$conf.int ^ 2

# Levene's test for equal variances (requires the car package)
# install.packages("car")   # uncomment if not installed
library(car)
leveneTest(response ~ group, data = df2)
# If p > 0.05 → variances are equal → use Student's t-test (var.equal = TRUE).
# If p < 0.05 → variances are unequal → use Welch's t-test (var.equal = FALSE,
#               which is also the default in R).

# --- T: Test statistic -------------------------------------------------------
# Student's t-test (equal variances assumed):
result_student <- t.test(response ~ group, data = df2,
                         var.equal  = TRUE,
                         alternative = "two.sided",
                         conf.level  = 0.95)
result_student

# Welch's t-test (unequal variances; R default):
result_welch <- t.test(response ~ group, data = df2,
                       var.equal  = FALSE,
                       alternative = "two.sided",
                       conf.level  = 0.95)
result_welch

# --- P: P-value --------------------------------------------------------------
result_student$p.value
result_student$conf.int

# --- C: Conclusion -----------------------------------------------------------
# Template:
#   "The mean [response] was [not] significantly different between the
#    [treatment] and [control] groups
#    (t_[df] = [t], p = [p]; 95% CI of difference: [lower]–[upper])."


# =============================================================================
# 3. PAIRED t-TEST
# =============================================================================
# Use when: two measurements are taken on the same subject (before/after,
#           matched pairs). The paired design accounts for individual variation.
# Example question: "Did heart rate increase after consuming Red Bull
#                    in the same students?"
# =============================================================================

# --- Example data ------------------------------------------------------------
# Replace with your own vectors. Each row must be the same subject.
set.seed(99)
n_pairs   <- 15
before    <- rnorm(n_pairs, mean = 70, sd = 8)
after     <- before + rnorm(n_pairs, mean = 5, sd = 4)  # slight increase
df3       <- data.frame(subject = 1:n_pairs, before, after)
df3$diff  <- df3$after - df3$before   # calculate pairwise differences

# --- H: Hypotheses -----------------------------------------------------------
# Two-tailed:
#   H0: mu_diff = 0   (no systematic change)
#   H1: mu_diff ≠ 0

# One-tailed (testing for an increase):
#   H0: mu_diff ≤ 0
#   H1: mu_diff  > 0

# --- A: Assumptions ----------------------------------------------------------
# Assumption: the DIFFERENCES between pairs are approximately normally
#             distributed (not the raw values individually).

p_diff_hist <- ggplot(df3, aes(x = diff)) +
  geom_histogram(fill = "steelblue", colour = "white", bins = 10) +
  labs(title = "Histogram of differences", x = "After − Before", y = "Count") +
  theme_minimal()

p_diff_qq <- ggplot(df3, aes(sample = diff)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  labs(title = "Q-Q plot of differences", x = "Theoretical", y = "Sample") +
  theme_minimal()

p_diff_hist + p_diff_qq

shapiro.test(df3$diff)   # formal normality test on the differences

# Outlier detection and removal on the pairwise differences
# (apply if histogram / Q-Q plot of differences suggests outliers)
# Because before/after values are paired, removing one difference means removing
# the ENTIRE row (both the before and after values for that subject).

q_diff   <- quantile(df3$diff, probs = c(0.25, 0.75))
iqr_diff <- IQR(df3$diff)
lower_diff <- q_diff[1] - 1.5 * iqr_diff
upper_diff <- q_diff[2] + 1.5 * iqr_diff

# Identify and inspect the outlying pairs:
df3[df3$diff < lower_diff | df3$diff > upper_diff, ]

# Remove the outlying pairs — creates a cleaned version of the data frame:
df3_clean <- df3[df3$diff >= lower_diff & df3$diff <= upper_diff, ]

# Recheck normality of the differences on the cleaned data:
shapiro.test(df3_clean$diff)

# Optional: visualise the cleaned differences.
p_diff_hist_clean <- ggplot(df3_clean, aes(x = diff)) +
  geom_histogram(fill = "steelblue", colour = "white", bins = 10) +
  labs(title = "Histogram of differences (outliers removed)",
       x = "After − Before", y = "Count") +
  theme_minimal()
p_diff_qq_clean <- ggplot(df3_clean, aes(sample = diff)) +
  stat_qq() + stat_qq_line(colour = "red") +
  labs(title = "Q-Q plot of differences (outliers removed)",
       x = "Theoretical", y = "Sample") +
  theme_minimal()
p_diff_hist_clean + p_diff_qq_clean

# Use df3_clean in place of df3 for all subsequent steps
# (skewness, transformation, t-test, etc.).

# Skewness and kurtosis of the differences (e1071)
# For the paired t-test, normality of the *differences* is what matters.
skewness(df3$diff)
kurtosis(df3$diff)
# Interpret as for Section 1: |skewness| < 0.5 and kurtosis near 0 → normal-like.

# Transformation and back-transformation (apply if normality of differences is violated)
#
# Option A — log-transform the raw before/after values, then re-derive differences.
#   Useful when before/after values are positive and right-skewed.
#   log(after) - log(before) = log(after/before) — a log-ratio of measurements.

df3$before_log <- log(df3$before)
df3$after_log  <- log(df3$after)
df3$diff_log   <- df3$after_log - df3$before_log   # = log(after / before)

# Recheck normality of the log-scale differences:
shapiro.test(df3$diff_log)
skewness(df3$diff_log)
kurtosis(df3$diff_log)

# Run the paired t-test on the log-scale differences:
result_paired_log <- t.test(
  df3$diff_log,
  mu          = 0,
  alternative = "two.sided",
  conf.level  = 0.95
)
result_paired_log

# Back-transform to the original scale:
# exp(diff_log) = after/before, so exp() converts log-differences to ratios.
exp(result_paired_log$estimate)   # geometric mean ratio  (after / before)
exp(result_paired_log$conf.int)   # 95% CI for the ratio on the original scale
# Interpret: "After [treatment], [variable] was on average [X]-fold its
#             pre-treatment value (95% CI: [lower]–[upper], p = [p])."

# Option B — signed square root of the raw differences (when only the
#   differences, not the raw values, are skewed):
#   df3$diff_sqrt <- sqrt(abs(df3$diff)) * sign(df3$diff)
#   result_sqrt_p <- t.test(df3$diff_sqrt, mu = 0, ...)
#   Back-transform: result_sqrt_p$conf.int ^ 2  # preserving the sign is optional

# --- T: Test statistic -------------------------------------------------------
# Method 1 — one-sample t-test on the differences (base R):
result_paired_m1 <- t.test(df3$diff,
                            mu          = 0,
                            alternative = "two.sided",
                            conf.level  = 0.95)
result_paired_m1

# Method 2 — built-in paired argument (base R):
result_paired_m2 <- t.test(df3$after, df3$before,
                            paired      = TRUE,
                            alternative = "two.sided",
                            conf.level  = 0.95)
result_paired_m2   # identical results to Method 1

# --- P: P-value --------------------------------------------------------------
result_paired_m1$p.value
result_paired_m1$conf.int

# --- C: Conclusion -----------------------------------------------------------
# Template:
#   "The mean difference in [response] between [after] and [before] was
#    [not] significantly different from zero
#    (t_[df] = [t], p = [p]; 95% CI of difference: [lower]–[upper])."


# =============================================================================
# END OF TEMPLATES
# =============================================================================
