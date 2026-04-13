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

# Skewness and kurtosis (e1071)
# Skewness: values between -0.5 and 0.5 are considered acceptably symmetrical.
#   > 0.5 or < -0.5 → moderate skew; > 1 or < -1 → high skew → consider transform.
# Kurtosis: excess kurtosis (e1071 default). 0 = normal tails.
#   > 1 (leptokurtic, heavy tails / peaked) → investigate outliers.
#   < -1 (platykurtic, light tails / flat) → consider transform.
skewness(x)
kurtosis(x)

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

# Skewness and kurtosis per group (e1071)
# Confirm symmetry within each group before relying on the t-test.
tapply(df2$response, df2$group, skewness)
tapply(df2$response, df2$group, kurtosis)
# Interpret as for Section 1: |skewness| < 0.5 and kurtosis near 0 → normal-like.

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

# Skewness and kurtosis of the differences (e1071)
# For the paired t-test, normality of the *differences* is what matters.
skewness(df3$diff)
kurtosis(df3$diff)
# Interpret as for Section 1: |skewness| < 0.5 and kurtosis near 0 → normal-like.

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
