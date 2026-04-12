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
# 4. WILCOXON SIGNED-RANK TEST
# =============================================================================
# Non-parametric alternative to the one-sample t-test AND the paired t-test.
# Use when: normality assumption is violated and transformation does not help.
# =============================================================================

# ----- 4a. ONE-SAMPLE Wilcoxon signed-rank test ------------------------------
# Example question: "Do lizards eat a median of 100 beetles per day?"

# --- Example data ------------------------------------------------------------
set.seed(5)
beetles <- c(89, 104, 78, 112, 95, 134, 88, 96, 110, 102,
             85,  99, 77, 125,  91)
mu0_beetles <- 100   # hypothesised median (or distribution centre)

# --- H: Hypotheses -----------------------------------------------------------
# Two-tailed:
#   H0: the distribution of [beetles] is centred at mu0 (= 100)
#   H1: the distribution of [beetles] is NOT centred at mu0

# --- A: Assumptions ----------------------------------------------------------
# - Independent observations
# - Differences from mu0 are roughly symmetric around zero
# (No normality required; use plots to check for gross asymmetry.)

p_beet_hist <- ggplot(data.frame(beetles), aes(x = beetles)) +
  geom_histogram(fill = "grey70", colour = "white", bins = 8) +
  geom_vline(xintercept = mu0_beetles, colour = "red", linetype = "dashed") +
  labs(title = "Histogram", x = "Beetles consumed", y = "Count") +
  theme_minimal()

p_beet_box <- ggplot(data.frame(beetles), aes(x = "", y = beetles)) +
  geom_boxplot(fill = "grey80") +
  geom_hline(yintercept = mu0_beetles, colour = "red", linetype = "dashed") +
  labs(title = "Boxplot", x = "", y = "Beetles consumed") +
  theme_minimal()

p_beet_hist + p_beet_box

# --- T: Test statistic -------------------------------------------------------
result_wilcox_1samp <- wilcox.test(
  beetles,
  mu          = mu0_beetles,
  alternative = "two.sided",
  conf.level  = 0.95
)
result_wilcox_1samp   # V = sum of positive signed ranks

# --- P: P-value --------------------------------------------------------------
result_wilcox_1samp$p.value

# --- C: Conclusion -----------------------------------------------------------
# Template:
#   "There was [no] significant evidence that the distribution of [beetles]
#    differs from a centre of [mu0] (V = [V], p = [p])."


# ----- 4b. PAIRED Wilcoxon signed-rank test ----------------------------------
# Example question: "Did chicken weight increase significantly after the diet?"

# --- Example data ------------------------------------------------------------
weight_before <- c(2.5, 3.5, 3.5, 3.4)
weight_after  <- c(4.0, 5.0, 5.0, 4.6)
diffs_weight  <- weight_after - weight_before

# --- H: Hypotheses -----------------------------------------------------------
# One-tailed (testing for an increase):
#   H0: the distribution of differences is centred at zero (no increase)
#   H1: the distribution of differences is centred above zero (increase)

# --- A: Assumptions ----------------------------------------------------------
# Differences should be roughly symmetric (not necessarily normal).

p_wdiff_hist <- ggplot(data.frame(d = diffs_weight), aes(x = d)) +
  geom_histogram(fill = "steelblue", colour = "white", bins = 6) +
  labs(title = "Histogram of differences", x = "After − Before", y = "Count") +
  theme_minimal()

p_wdiff_box <- ggplot(data.frame(d = diffs_weight), aes(x = "", y = d)) +
  geom_boxplot(fill = "steelblue", alpha = 0.5) +
  labs(title = "Boxplot of differences", x = "", y = "After − Before") +
  theme_minimal()

p_wdiff_hist + p_wdiff_box

shapiro.test(diffs_weight)   # optional — often sample size too small to be useful

# --- T: Test statistic -------------------------------------------------------
result_wilcox_paired <- wilcox.test(
  x           = weight_after,
  y           = weight_before,
  paired      = TRUE,
  alternative = "greater",   # testing for an increase
  conf.level  = 0.95
)
result_wilcox_paired   # V = sum of positive signed ranks of differences

# --- P: P-value --------------------------------------------------------------
result_wilcox_paired$p.value

# --- C: Conclusion -----------------------------------------------------------
# Template:
#   "There was [no] significant evidence of an increase in [weight] after
#    [treatment] (V = [V], p = [p])."


# =============================================================================
# 5. MANN-WHITNEY U TEST  (Wilcoxon rank-sum test)
# =============================================================================
# Non-parametric alternative to the two-sample t-test.
# Use when: normality assumption is violated for independent groups.
# Note: R calls this wilcox.test(); it reports W (the rank-sum statistic).
# =============================================================================

# --- Example data ------------------------------------------------------------
set.seed(21)
df5 <- data.frame(
  group    = rep(c("large", "small"), times = c(12, 14)),
  response = c(rnorm(12, mean = 115, sd = 30),
               rnorm(14, mean = 95,  sd = 25))
)

# --- H: Hypotheses -----------------------------------------------------------
# Two-tailed:
#   H0: median_large = median_small   (distributions have the same centre)
#   H1: median_large ≠ median_small

# --- A: Assumptions ----------------------------------------------------------
# - Independent samples
# - Both groups have a similar distribution shape
#   (use histograms and boxplots to compare shapes)

p_mw_hist <- ggplot(df5, aes(x = response, fill = group)) +
  geom_histogram(binwidth = 10, colour = "black", alpha = 0.5,
                 position = "identity") +
  facet_wrap(~group, ncol = 1) +
  labs(title = "Histograms by group", x = "Response", y = "Count") +
  theme_classic()

p_mw_box <- ggplot(df5, aes(x = group, y = response, fill = group)) +
  geom_boxplot(alpha = 0.4) +
  labs(title = "Boxplot by group", x = "Group", y = "Response") +
  theme_classic()

p_mw_hist + p_mw_box

# --- T: Test statistic -------------------------------------------------------
result_mw <- wilcox.test(
  response ~ group,
  data        = df5,
  alternative = "two.sided",
  conf.level  = 0.95
)
result_mw   # W = rank-sum statistic

# --- P: P-value --------------------------------------------------------------
result_mw$p.value

# --- C: Conclusion -----------------------------------------------------------
# Template:
#   "The distribution of [response] was [not] significantly different between
#    the [large] and [small] groups (W = [W], p = [p])."


# =============================================================================
# 6. CHI-SQUARED GOODNESS-OF-FIT TEST
# =============================================================================
# Use when: comparing observed counts of a categorical variable to expected
#           (theoretical) proportions.
# Example question: "Are butterfly colour frequencies equally distributed?"
# =============================================================================

# --- Example data ------------------------------------------------------------
observed_counts <- c(red = 48, blue = 62, green = 56, yellow = 34)
# Replace with your own named counts vector.
expected_probs  <- rep(0.25, 4)   # equal proportions (null hypothesis)
# For unequal expected proportions, e.g.: c(0.4, 0.3, 0.2, 0.1)

# --- H: Hypotheses -----------------------------------------------------------
# H0: p_red = p_blue = p_green = p_yellow = 0.25  (proportions match expected)
# H1: at least one observed proportion differs from expected

# --- A: Assumptions ----------------------------------------------------------
# 1. Observations are independent.
# 2. Expected count in each cell ≥ 5
#    (check: total_n × expected_probs — all values should be ≥ 5).
total_n <- sum(observed_counts)
expected_counts <- total_n * expected_probs
expected_counts   # inspect — all should be ≥ 5

# Barplot of observed vs expected (base R)
barplot(
  rbind(observed_counts, expected_counts),
  beside  = TRUE,
  legend.text = c("Observed", "Expected"),
  col     = c("steelblue", "grey70"),
  main    = "Observed vs Expected counts",
  xlab    = "Category",
  ylab    = "Count"
)

# --- T: Test statistic -------------------------------------------------------
result_gof <- chisq.test(
  x = observed_counts,
  p = expected_probs
)
result_gof          # chi-squared, df = k-1, p-value
result_gof$observed # observed frequencies
result_gof$expected # expected frequencies (verify assumption)
result_gof$residuals  # Pearson residuals — which cells deviate most?

# --- P: P-value --------------------------------------------------------------
result_gof$p.value

# --- C: Conclusion -----------------------------------------------------------
# Template:
#   "The observed proportions of [variable] were [not] significantly different
#    from the expected proportions (χ²_[df] = [chi], p = [p])."


# =============================================================================
# 7. CHI-SQUARED TEST OF INDEPENDENCE
# =============================================================================
# Use when: testing whether two categorical variables are associated.
# Example question: "Is deer age group independent of vegetation preference?"
# =============================================================================

# --- Example data ------------------------------------------------------------
# Provide data as a contingency table (matrix or table object).
deer_data <- matrix(
  c(20, 30, 10,
    10, 10, 20,
    10, 10, 10),
  nrow     = 3,
  byrow    = TRUE,
  dimnames = list(
    age_group  = c("young", "adult", "old"),
    vegetation = c("grass", "leaves", "bark")
  )
)
deer_data   # inspect contingency table

# --- H: Hypotheses -----------------------------------------------------------
# H0: age group is INDEPENDENT of vegetation preference
# H1: age group is NOT independent of vegetation preference

# --- A: Assumptions ----------------------------------------------------------
# 1. Observations are independent.
# 2. Expected frequency in each cell ≥ 1, and no more than 20% of cells
#    have expected frequency < 5.

# --- T: Test statistic -------------------------------------------------------
result_indep <- chisq.test(deer_data)
result_indep            # chi-squared, df = (r-1)*(c-1), p-value
result_indep$expected   # inspect expected frequencies — check assumption
result_indep$residuals  # Pearson residuals — which cells deviate most?

# Visualisation — mosaic plot (base R)
mosaicplot(
  deer_data,
  shade = TRUE,
  main  = "Mosaic plot: age group × vegetation preference"
)

# --- P: P-value --------------------------------------------------------------
result_indep$p.value

# --- C: Conclusion -----------------------------------------------------------
# Template:
#   "[Variable 1] was [not] significantly associated with [Variable 2]
#    (χ²_[df] = [chi], p = [p])."
#
# Note: if any expected cell counts are < 5, consider using Fisher's Exact
#       Test instead: fisher.test(contingency_table)


# =============================================================================
# END OF TEMPLATES
# =============================================================================
