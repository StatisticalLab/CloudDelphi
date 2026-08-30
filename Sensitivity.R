## ==========================================================================
## Sensitivity Analysis of Model Design Constants
## Asymmetric Cloud Delphi (Split-Entropy)
##
## Evaluated Constants:
##   eps       -> epsilon (regularizer for A_i and W_i')
##   xi        -> minimum threshold for split-entropy (min(En_L, En_R) >= xi)
##   omega_den -> denominator 18 in Omega_i^(r)
##   k_floor   -> lower bound 2 of the max() operator in K_Dynamic
##   k_coef    -> coefficient 3 in (3 + 3 * C_i) [controls upper bound 6]
##
## This script recalculates the entire pipeline (split-entropy, hybrid
## hyper-entropy, weights, collective synthesis, and stopping criteria) for
## the real panel (N = 7, 3 rounds), first using the article's nominal values
## and then perturbing each constant by +/- a percentage, one at a time.
##
## YOU ONLY NEED TO EDIT SECTION "1. RAW PANEL DATA" with the actual values of
## x_min, x_max, Ex, and C for each expert in each round (the same ones used
## to construct Table 1 of the article).
## ==========================================================================

## --------------------------------------------------------------------------
## 1. RAW PANEL DATA  <-- REPLACE THIS WITH YOUR ACTUAL DATA
## --------------------------------------------------------------------------
## Each round is a list containing 4 vectors of length N (one per expert):
## xmin, xmax, Ex, C (C in {0.0, 0.1, ..., 1.0})

N <- 7

rounds_raw <- list(
  round1 = list(
    xmin = c(10, 30, 20,  5, 60, 10, 15),
    xmax = c(30, 70, 50, 20, 95, 60, 45),
    Ex   = c(20, 52, 35, 15, 80, 22, 30),
    C    = c(0.6, 0.1, 0.5, 0.5, 0.4, 0.0, 0.5)   # <-- PLACEHOLDER, review
  ),
  round2 = list(
    xmin = c(24, 30, 27, 25, 22, 32, 26),
    xmax = c(35, 55, 42, 39, 47, 56, 40),
    Ex   = c(30, 42, 34, 32, 36, 45, 33),
    C    = c(0.7, 0.5, 0.7, 0.7, 0.6, 0.6, 0.65)  # <-- PLACEHOLDER, review
  ),
  round3 = list(
    xmin = c(32, 35, 34, 33, 35, 36, 32),
    xmax = c(40, 45, 42, 41, 43, 46, 40),
    Ex   = c(36, 40, 38, 37, 39, 41, 36),
    C    = c(0.85, 0.8, 0.85, 0.88, 0.85, 0.78, 0.86)  # <-- PLACEHOLDER, review
  )
)

## IMPORTANT: The values above are placeholders to ensure the script runs
## without errors. Replace them with the actual x_min, x_max, Ex, and C
## values from your instrument (Figure 1) for each of the 7 experts across
## all rounds. Ex_i, En_L,i, and En_R,i from the published Table 1 can be used
## to verify that inputting the correct x_min/x_max/C reproduces the exact
## En_L, En_R values of Table 1 via your split_entropy() function.

## --------------------------------------------------------------------------
## 2. MODEL FUNCTIONS (Equations 6-16 of the article)
## --------------------------------------------------------------------------

split_entropy <- function(xmin, xmax, Ex, C, xi) {
  EnL <- (Ex - xmin) * (2 - C) / 3
  EnR <- (xmax - Ex) * (2 - C) / 3
  # Regularization: min(EnL, EnR) >= xi
  EnL <- pmax(EnL, xi)
  EnR <- pmax(EnR, xi)
  list(EnL = EnL, EnR = EnR)
}

k_dynamic <- function(EnL, EnR, C, eps, k_floor, k_coef) {
  A <- abs(EnL - EnR) / (EnL + EnR + eps)
  pmax((k_coef + k_coef * C) * (1 - 0.5 * A), k_floor)
}

he_base <- function(EnL, EnR, C, eps, k_floor, k_coef) {
  K <- k_dynamic(EnL, EnR, C, eps, k_floor, k_coef)
  (EnL + EnR) / (2 * K)
}

omega_term <- function(xmin_r, xmax_r, Ex_r, xmin_prev, xmax_prev, Ex_prev, omega_den) {
  (abs(xmin_r - xmin_prev) + abs(xmax_r - xmax_prev) + abs(Ex_r - Ex_prev)) / omega_den
}

## --------------------------------------------------------------------------
## 3. COMPLETE PIPELINE FOR 3 ROUNDS, GIVEN CONSTANTS
## --------------------------------------------------------------------------

run_model <- function(rounds_raw, eps = 1e-2, xi = 1e-3, omega_den = 18,
                      k_floor = 2, k_coef = 3, tau = 0.5, theta = 0.2, Rmax = 4) {

  R <- length(rounds_raw)
  out <- vector("list", R)
  prev <- NULL
  Is_prev <- NULL
  EnAvg_prev <- NULL
  stop_round <- NA

  for (r in seq_len(R)) {
    d <- rounds_raw[[r]]
    se <- split_entropy(d$xmin, d$xmax, d$Ex, d$C, xi)

    if (r == 1) {
      He <- he_base(se$EnL, se$EnR, d$C, eps, k_floor, k_coef)
    } else {
      He_b <- he_base(se$EnL, se$EnR, d$C, eps, k_floor, k_coef)
      Om <- omega_term(d$xmin, d$xmax, d$Ex, prev$xmin, prev$xmax, prev$Ex, omega_den)
      He <- He_b + Om
    }

    Wp <- d$C / (abs((d$Ex - mean(d$Ex)) / mean(d$Ex)) + (se$EnL + se$EnR) / 2 + He + eps)
    W <- Wp / sum(Wp)

    Ex_s <- sum(W * d$Ex)
    He_s <- sum(W * He)
    EnLs <- (Ex_s - min(d$Ex - 3 * se$EnL)) / 3
    EnRs <- (max(d$Ex + 3 * se$EnR) - Ex_s) / 3
    EnAvg_s <- (EnLs + EnRs) / 2
    EnAvg_ind <- mean(se$EnL + se$EnR) / 2        # = (1/(2N)) * sum(EnL + EnR), Eq. of I_s^(r)
    Is <- He_s / EnAvg_ind

    UpsE <- 0; UpsU <- 0; UpsR <- 0
    if (r > 1) {
      dEn <- abs(EnAvg_s - EnAvg_prev) / max(EnAvg_prev, eps)
      UpsE <- as.numeric(dEn < tau)
      UpsU <- as.numeric(abs(Is - Is_prev) < theta)
    }
    UpsR <- as.numeric(r >= Rmax)
    Upsilon <- as.numeric(UpsE | UpsU | UpsR)

    if (is.na(stop_round) && Upsilon == 1) stop_round <- r

    out[[r]] <- list(Ex_s = Ex_s, He_s = He_s, EnLs = EnLs, EnRs = EnRs,
                     EnAvg_s = EnAvg_s, Is = Is, Upsilon = Upsilon)

    prev <- d
    Is_prev <- Is
    EnAvg_prev <- EnAvg_s
  }

  list(rounds = out, stop_round = stop_round)
}

## --------------------------------------------------------------------------
## 4. BASELINE (Nominal constants from the article)
## --------------------------------------------------------------------------

baseline <- run_model(rounds_raw)
base_final <- baseline$rounds[[3]]
cat("== Baseline ==\n")
cat(sprintf("Ex_s(3) = %.4f | He_s(3) = %.4f | Stopping Round = %d\n\n",
            base_final$Ex_s, base_final$He_s, baseline$stop_round))

## --------------------------------------------------------------------------
## 5. ONE-AT-A-TIME PERTURBATION OF EACH CONSTANT
## --------------------------------------------------------------------------

pct_changes <- c(-0.5, -0.2, 0.2, 0.5)   # -50%, -20%, +20%, +50%

sensitivity_grid <- list(
  eps       = list(nominal = 1e-2, param = "eps"),
  xi        = list(nominal = 1e-3, param = "xi"),
  omega_den = list(nominal = 18,   param = "omega_den"),
  k_floor   = list(nominal = 2,    param = "k_floor"),
  k_coef    = list(nominal = 3,    param = "k_coef")
)

results <- data.frame()

for (cname in names(sensitivity_grid)) {
  nominal <- sensitivity_grid[[cname]]$nominal
  for (pc in pct_changes) {
    val <- nominal * (1 + pc)
    args <- list(rounds_raw = rounds_raw,
                 eps = 1e-2, xi = 1e-3, omega_den = 18, k_floor = 2, k_coef = 3)
    args[[cname]] <- val
    res <- do.call(run_model, args)
    final <- res$rounds[[3]]

    dEx_pct  <- 100 * (final$Ex_s - base_final$Ex_s) / base_final$Ex_s
    dHe_pct  <- 100 * (final$He_s - base_final$He_s) / base_final$He_s
    same_stop <- res$stop_round == baseline$stop_round

    results <- rbind(results, data.frame(
      constant = cname,
      perturbation_pct = pc * 100,
      used_value = val,
      delta_Ex_s_pct = round(dEx_pct, 3),
      delta_He_s_pct = round(dHe_pct, 3),
      stopping_round = res$stop_round,
      same_stopping_round = same_stop
    ))
  }
}

cat("== Sensitivity Results (One-at-a-time, relative to Round 3) ==\n")
print(results, row.names = FALSE)

cat("\n== Summary (Worst-case per constant) ==\n")
summary_tbl <- aggregate(cbind(abs_delta_Ex = abs(delta_Ex_s_pct),
                                abs_delta_He = abs(delta_He_s_pct)) ~ constant,
                         data = transform(results,
                                          abs_delta_Ex = abs(delta_Ex_s_pct),
                                          abs_delta_He = abs(delta_He_s_pct)),
                         FUN = max)
print(summary_tbl, row.names = FALSE)

## --------------------------------------------------------------------------
## 6. SAVE RESULTS FOR THE ARTICLE
## --------------------------------------------------------------------------
write.csv(results, "sensitivity_results.csv", row.names = FALSE)
cat("\nComplete results saved to sensitivity_results.csv\n")
