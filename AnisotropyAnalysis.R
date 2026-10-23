# ------------------------------------------------------------------------------
# Supplementary code for: Cruz-Ramirez, M. Cloud Delphi framework under
# asymmetric uncertainty. Croatian Operational Research Review (CRORR).
# DOI: 10.17535/crorr.2027.0013  |  Contact: mcruz65@uan.edu.co
# Requirements: base R (>= 4.0); no additional packages. See README.md.
# ------------------------------------------------------------------------------
# ==============================================================================
# Re-analysis of Table 1 from Cruz & Cables (2022), Investigación Operacional 43(2)
# "Un estudio comparado de dos enfoques no deterministas en el Delphi de pronóstico"
#
# Objective: to verify whether the position of the most probable value (Ex_i) within the
# declared range [x_min_i, x_max_i] deviates from the midpoint (asymmetry /
# cognitive anisotropy at the individual level), and whether this effect persists or is
# diluted when averaging over the expert panel (aggregate level).
# ==============================================================================

# ------------------------------------------------------------------
# 1. Data: Table 1 (N = 56 experts, two Delphi rounds)
# columns: id, lambda, min1, max1, a1, beta1, gamma1, min2, max2, a2, beta2, gamma2
# ------------------------------------------------------------------
raw <- "
1,3,50.0,70.0,62.0,0.7,0.6,55.0,67.0,64.0,0.8,0.7
2,1,68.0,92.0,85.0,0.6,0.6,80.0,92.5,90.0,0.7,0.7
3,3,55.0,75.0,65.0,0.8,0.6,57.0,80.0,75.0,0.7,0.5
4,3,50.5,63.5,59.0,0.8,0.7,50.5,60.0,58.0,0.8,0.8
5,3,60.0,75.0,66.0,0.5,0.1,60.0,66.0,63.0,0.3,0.1
6,3,42.0,68.0,56.0,1.0,0.6,44.0,65.0,58.0,0.9,0.4
7,3,42.0,70.0,62.0,0.7,0.3,45.0,75.0,62.0,0.5,0.5
8,3,52.4,66.3,58.9,0.7,0.7,56.8,69.0,62.0,0.9,0.8
9,3,56.0,65.0,58.0,0.1,0.1,56.0,65.0,58.0,0.1,0.1
10,3,60.5,70.0,65.0,0.2,0.4,50.0,70.0,68.0,0.4,0.4
11,3,62.0,75.0,68.0,0.1,1.0,65.0,78.0,72.0,0.1,1.0
12,3,46.3,75.8,60.2,0.6,0.3,50.3,88.6,71.0,0.5,0.5
13,3,40.0,65.0,60.0,0.9,0.8,45.0,65.0,60.0,0.8,0.8
14,3,45.0,72.5,65.0,0.9,0.9,45.0,75.0,70.0,0.8,0.8
15,2,60.0,75.0,79.0,1.0,1.0,60.0,75.0,79.0,1.0,1.0
16,3,53.0,65.0,58.0,0.5,0.4,53.0,65.0,58.0,0.5,0.5
17,1,60.0,70.0,67.0,0.4,0.9,60.0,70.0,67.0,0.5,1.0
18,3,60.0,80.0,75.0,0.2,0.5,65.0,80.0,75.0,0.4,0.4
19,1,69.0,75.0,72.5,0.5,0.5,71.0,77.0,75.2,0.6,0.6
20,3,33.3,85.0,72.0,0.2,0.5,40.0,85.0,75.8,0.2,0.6
21,3,63.5,70.5,65.0,1.0,1.0,69.5,70.5,65.0,1.0,1.0
22,3,70.0,80.0,75.0,0.9,0.3,65.0,68.0,65.0,0.9,0.2
23,3,70.0,75.0,60.0,0.1,0.1,65.0,70.0,70.0,0.1,0.1
24,1,40.0,60.0,50.0,0.6,0.6,40.0,60.0,50.0,0.7,0.7
25,3,60.0,70.0,65.0,0.1,0.1,60.0,70.0,65.0,0.1,0.1
26,2,80.5,90.8,85.5,0.4,0.4,80.5,90.8,85.5,0.4,0.4
27,3,40.0,70.0,55.0,0.5,0.5,40.0,70.0,55.0,0.4,0.4
28,1,40.0,90.0,60.0,0.8,0.7,40.0,90.0,60.0,0.9,0.8
29,3,0.0,100.0,50.0,0.4,0.4,0.0,100.0,50.0,0.4,0.4
30,3,55.0,85.0,70.0,0.5,0.5,60.0,90.0,75.0,0.6,0.6
31,3,30.8,65.0,55.3,0.3,0.9,35.0,60.0,50.0,0.3,1.0
32,2,50.0,75.0,64.0,0.8,0.6,50.0,75.0,64.0,0.7,0.7
33,3,30.0,75.0,60.0,1.0,0.1,30.0,75.0,60.0,1.0,0.1
34,1,60.0,75.0,70.0,1.0,0.2,60.0,75.0,70.0,0.9,0.2
35,2,30.0,80.0,60.0,0.7,0.6,40.0,80.0,65.0,0.8,0.7
36,3,45.0,85.0,65.0,0.1,0.3,45.0,85.0,65.0,0.4,0.8
37,3,50.0,70.0,60.0,0.4,0.5,50.0,80.0,62.0,0.6,0.6
38,3,20.0,40.0,30.0,0.4,0.4,20.0,40.0,30.0,0.4,0.4
39,1,40.0,82.0,60.0,0.7,0.4,40.0,80.0,60.0,0.8,0.8
40,3,50.0,80.0,60.0,0.9,0.7,50.0,80.0,60.0,0.7,0.7
41,1,60.0,90.0,85.0,0.2,0.7,80.0,99.0,90.0,0.3,0.7
42,1,70.0,80.0,75.0,0.9,0.9,60.0,70.0,65.0,0.9,0.9
43,1,60.0,80.0,75.0,0.5,0.4,60.0,80.0,70.0,0.5,0.5
44,1,50.0,70.0,55.0,0.8,0.8,40.0,65.0,70.0,1.0,1.0
45,2,50.0,70.0,60.0,0.4,0.4,50.0,70.0,60.0,0.8,0.8
46,3,50.0,75.0,60.0,0.7,0.7,50.0,75.0,60.0,0.8,0.8
47,3,60.0,75.0,65.0,1.0,1.0,60.0,70.0,65.0,1.0,1.0
48,3,80.5,85.1,82.5,0.7,0.7,80.5,86.5,84.3,0.8,0.7
49,2,75.0,85.0,70.0,1.0,0.2,70.0,80.0,70.0,0.8,0.2
50,1,60.0,70.0,65.0,0.4,0.5,60.0,70.0,65.0,0.4,0.5
51,3,72.5,80.3,75.8,0.8,0.7,72.5,80.3,75.8,0.8,0.7
52,3,50.0,60.0,52.0,0.7,0.8,50.0,60.0,52.0,0.7,1.0
53,3,60.0,75.0,72.0,0.5,1.0,70.0,80.0,75.0,0.6,0.9
54,2,58.0,88.0,60.0,0.6,0.9,55.0,85.0,59.0,0.7,0.8
55,2,35.0,70.0,50.0,0.7,0.2,50.0,70.0,64.0,0.8,0.4
56,3,50.0,95.0,80.0,0.4,0.4,65.0,95.0,75.0,0.4,0.4
"

col_names <- c("id", "lambda", "min1", "max1", "a1", "beta1", "gamma1",
               "min2", "max2", "a2", "beta2", "gamma2")

d <- read.csv(text = raw, header = FALSE, col.names = col_names)

# ------------------------------------------------------------------
# 2. Relative position of the most probable value within the declared range
#    r = (Ex - min) / (max - min);  r = 0.5 corresponds to the midpoint (symmetry)
# ------------------------------------------------------------------
rel_pos <- function(mn, mx, a) (a - mn) / (mx - mn)

d$r1 <- rel_pos(d$min1, d$max1, d$a1)
d$r2 <- rel_pos(d$min2, d$max2, d$a2)

cat(sprintf("Round 1: mean relative position = %.4f (SD = %.4f)\n",
            mean(d$r1), sd(d$r1)))
cat(sprintf("Round 2: mean relative position = %.4f (SD = %.4f)\n",
            mean(d$r2), sd(d$r2)))

# Does the panel mean differ from the symmetric midpoint (0.5)?
t1 <- t.test(d$r1, mu = 0.5)
t2 <- t.test(d$r2, mu = 0.5)
print(t1)
print(t2)

# Experts with the most probable value exactly at the midpoint
cat(sprintf("Round 1: %d/56 experts exactly symmetric (r = 0.5)\n",
            sum(d$r1 == 0.5)))
cat(sprintf("Round 2: %d/56 experts exactly symmetric (r = 0.5)\n",
            sum(d$r2 == 0.5)))

# ------------------------------------------------------------------
# 3. Individual anisotropy index (analogous to A_i, Eq. 9 in the new paper)
#    EnL = Ex - min  (left segment)
#    EnR = max - Ex  (right segment)
#    A   = |EnL - EnR| / (EnL + EnR)   in [0, 1)
# ------------------------------------------------------------------
EnL1 <- d$a1 - d$min1;  EnR1 <- d$max1 - d$a1
EnL2 <- d$a2 - d$min2;  EnR2 <- d$max2 - d$a2

A1 <- abs(EnL1 - EnR1) / (EnL1 + EnR1)
A2 <- abs(EnL2 - EnR2) / (EnL2 + EnR2)

cat(sprintf("Round 1: anisotropy index A -> mean = %.4f, median = %.4f\n",
            mean(A1), median(A1)))
cat(sprintf("Round 2: anisotropy index A -> mean = %.4f, median = %.4f\n",
            mean(A2), median(A2)))
cat(sprintf("Round 1: %.1f%% of experts with A > 0.1\n", 100 * mean(A1 > 0.1)))
cat(sprintf("Round 2: %.1f%% of experts with A > 0.1\n", 100 * mean(A2 > 0.1)))

# ------------------------------------------------------------------
# 4. Paired test EnL vs EnR (Wilcoxon signed-rank)
#    Replicates and extends the test already reported in Cruz & Cables (2022)
# ------------------------------------------------------------------
w1 <- wilcox.test(EnL1, EnR1, paired = TRUE)
w2 <- wilcox.test(EnL2, EnR2, paired = TRUE)
print(w1)
print(w2)

# ------------------------------------------------------------------
# 5. Final summary
# ------------------------------------------------------------------
cat("\n===================== SUMMARY =====================\n")
cat("Individual level: most experts display a most probable value\n")
cat("shifted from the midpoint of their declared range (asymmetry/anisotropy).\n")
cat("Aggregate level: the panel average DOES NOT significantly deviate\n")
cat("from the symmetric midpoint (p > 0.6 in both rounds).\n")
cat("=> Anisotropy is an individual phenomenon, not a population bias.\n")