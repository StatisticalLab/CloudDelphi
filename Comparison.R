# ------------------------------------------------------------------------------
# Supplementary code for: Cruz-Ramirez, M. Cloud Delphi framework under
# asymmetric uncertainty. Croatian Operational Research Review (CRORR).
# DOI: 10.17535/crorr.2027.0013  |  Contact: mcruz65@uan.edu.co
# Requirements: base R (>= 4.0); no additional packages. See README.md.
# ------------------------------------------------------------------------------
# ==============================================================================
# COMPARATIVE SCRIPT: ASYMMETRIC CLOUD DELPHI VS. CLASSIC SYMMETRIC BASELINE
# ==============================================================================

# 1. PARAMETERS AND INPUT DATA
epsilon <- 0.01
xi      <- 0.001

Round1_Data <- c(15, 75, 20, 0.85, 10, 60, 52, 0.05, 30, 90, 35, 0.75, 10, 50, 15, 0.60, 20, 85, 80, 0.85, 15, 70, 22, 0.02, 25, 80, 30, 0.70)
Round2_Data <- c(25, 55, 30, 0.95, 35, 75, 42, 0.80, 30, 60, 34, 0.92, 28, 50, 32, 0.88, 32, 65, 36, 0.90, 40, 75, 45, 0.85, 28, 55, 33, 0.91)
Round3_Data <- c(30, 42, 36, 0.98, 34, 46, 40, 0.95, 32, 44, 38, 0.96, 31, 43, 37, 0.94, 33, 45, 39, 0.97, 35, 47, 41, 0.92, 30, 42, 36, 0.95)

# 2. PROPOSED ASYMMETRIC CORE ENGINE
CloudDelphi_Core <- function(CurrentData, ForegoingData = NULL, ForegoingEntropy = NULL, 
                             eps = epsilon, xi_param = xi) {
  Experts <- length(CurrentData) / 4
  Ex_i <- numeric(Experts); En_L <- numeric(Experts); En_R <- numeric(Experts)
  He_i <- numeric(Experts); Conf_i <- numeric(Experts)
  
  for (i in 1:Experts) {
    idx <- (i - 1) * 4 + 1
    ex_min  <- CurrentData[idx]; ex_max  <- CurrentData[idx + 1]
    ex_prob <- CurrentData[idx + 2]; ex_sec  <- CurrentData[idx + 3]
    
    Ex_i[i]   <- ex_prob
    Conf_i[i] <- ex_sec
    Insecurity_Factor <- 1 - ex_sec
    
    En_L[i] <- max(((ex_prob - ex_min) * (1 + Insecurity_Factor)) / 3, xi_param)
    En_R[i] <- max(((ex_max - ex_prob) * (1 + Insecurity_Factor)) / 3, xi_param)
    
    A_i <- abs(En_L[i] - En_R[i]) / (En_L[i] + En_R[i] + eps)
    K_Dynamic <- max((3 + (3 * ex_sec)) * (1 - (0.5 * A_i)), 2.0)
    He_base <- (En_L[i] + En_R[i]) / (2 * K_Dynamic)
    
    if (!is.null(ForegoingData) && length(ForegoingData) >= idx + 2) {
      var_min  <- abs(ex_min - ForegoingData[idx])
      var_max  <- abs(ex_max - ForegoingData[idx + 1])
      var_prob <- abs(ex_prob - ForegoingData[idx + 2])
      Omega_i  <- (var_min + var_max + var_prob) / 18
      He_i[i]  <- He_base + Omega_i
    } else {
      He_i[i]  <- He_base
    }
  }
  
  Ex_s_prov <- mean(Ex_i)
  Weight <- Conf_i / (abs((Ex_i - Ex_s_prov) / Ex_s_prov) + ((En_L + En_R)/2) + He_i + eps)
  Weight <- Weight / sum(Weight)
  
  Ex_s  <- weighted.mean(Ex_i, Weight)
  He_s  <- weighted.mean(He_i, Weight)
  En_Ls <- (Ex_s - min(Ex_i - 3 * En_L)) / 3
  En_Rs <- (max(Ex_i + 3 * En_R) - Ex_s) / 3  
  
  return(c(Ex_s = Ex_s, En_Ls = En_Ls, En_Rs = En_Rs, He_s = He_s))
}

# 3. CLASSIC SYMMETRIC BASELINE ENGINE
CloudDelphi_Symmetric_Baseline <- function(CurrentData, ForegoingData = NULL, 
                                           eps = epsilon, xi_param = xi) {
  Experts <- length(CurrentData) / 4
  Ex_i <- numeric(Experts); En_Sym <- numeric(Experts)
  He_i <- numeric(Experts); Conf_i <- numeric(Experts)
  
  for (i in 1:Experts) {
    idx <- (i - 1) * 4 + 1
    ex_min  <- CurrentData[idx]; ex_max  <- CurrentData[idx + 1]
    ex_prob <- CurrentData[idx + 2]; ex_sec  <- CurrentData[idx + 3]
    
    Ex_i[i]   <- ex_prob
    Conf_i[i] <- ex_sec
    Insecurity_Factor <- 1 - ex_sec
    
    En_Sym[i] <- max(((ex_max - ex_min) * (1 + Insecurity_Factor)) / 6, xi_param)
    K_Dynamic <- max(3 + (3 * ex_sec), 2.0)
    He_base <- En_Sym[i] / K_Dynamic
    
    if (!is.null(ForegoingData) && length(ForegoingData) >= idx + 2) {
      var_min  <- abs(ex_min - ForegoingData[idx])
      var_max  <- abs(ex_max - ForegoingData[idx + 1])
      var_prob <- abs(ex_prob - ForegoingData[idx + 2])
      Omega_i  <- (var_min + var_max + var_prob) / 18
      He_i[i]  <- He_base + Omega_i
    } else {
      He_i[i]  <- He_base
    }
  }
  
  Ex_s_prov <- mean(Ex_i)
  Weight <- Conf_i / (abs((Ex_i - Ex_s_prov) / Ex_s_prov) + En_Sym + He_i + eps)
  Weight <- Weight / sum(Weight)
  
  Ex_s  <- weighted.mean(Ex_i, Weight)
  He_s  <- weighted.mean(He_i, Weight)
  En_s  <- (max(Ex_i + 3 * En_Sym) - min(Ex_i - 3 * En_Sym)) / 6
  
  return(c(Ex_s = Ex_s, En_s = En_s, He_s = He_s))
}

# 4. METRICS COMPUTATION AND COMPARISON
# Asymmetric Model
R1_Asym <- CloudDelphi_Core(Round1_Data)
R2_Asym <- CloudDelphi_Core(Round2_Data, ForegoingData = Round1_Data)

R1_Asym_En <- (R1_Asym["En_Ls"] + R1_Asym["En_Rs"]) / 2
R2_Asym_En <- (R2_Asym["En_Ls"] + R2_Asym["En_Rs"]) / 2

# Symmetric Baseline Model
R1_Sym <- CloudDelphi_Symmetric_Baseline(Round1_Data)
R2_Sym <- CloudDelphi_Symmetric_Baseline(Round2_Data, ForegoingData = Round1_Data)

# Percentage Differences Relative to Asymmetric Model
PCT_Diff_En_R1 <- ((R1_Sym["En_s"] - R1_Asym_En) / R1_Asym_En) * 100
PCT_Diff_He_R1 <- ((R1_Sym["He_s"] - R1_Asym["He_s"]) / R1_Asym["He_s"]) * 100

PCT_Diff_En_R2 <- ((R2_Sym["En_s"] - R2_Asym_En) / R2_Asym_En) * 100
PCT_Diff_He_R2 <- ((R2_Sym["He_s"] - R2_Asym["He_s"]) / R2_Asym["He_s"]) * 100

# 5. CONSOLE OUTPUT PRINTING
cat("\n========================================================\n")
cat("COMPARATIVE ANALYSIS: ASYMMETRIC VS. SYMMETRIC BASELINE\n")
cat("========================================================\n")
cat(sprintf("Round 1 - Entropy Variation (En): %+.2f%%\n", PCT_Diff_En_R1))
cat(sprintf("Round 1 - Hyper-entropy Variation (He): %+.2f%%\n", PCT_Diff_He_R1))
cat(sprintf("Round 2 - Entropy Variation (En): %+.2f%%\n", PCT_Diff_En_R2))
cat(sprintf("Round 2 - Hyper-entropy Variation (He): %+.2f%%\n", PCT_Diff_He_R2))
cat("========================================================\n")