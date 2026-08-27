# ==============================================================================
# CLOUD DELPHI METHODOLOGY - CORE COMPUTATIONAL ENGINE
# ==============================================================================

# ------------------------------------------------------------------------------
# GLOBAL CONFIGURATION BLOCK (USER-ADJUSTABLE PARAMS)
# ------------------------------------------------------------------------------
# Mathematical, Analytical & Regularization Parameters
epsilon         <- 0.01   # Parameter (e) to preserve analytical stability against zeros
xi              <- 0.001  # Minimum structural entropy threshold (\xi) matching LaTeX bounds
tau             <- 0.50   # Convergence threshold (\tau) for collective entropy
theta           <- 0.20   # Stability threshold (\theta) for collective uncertainty
R_max           <- 4      # Maximum number of rounds allowed before halting (R_max)

# Simulation & Renderer Parameters
Drops_Expert    <- 1200   # Number of random drops per individual expert cloud
Drops_Synthetic <- 2500   # Number of random drops for the collective group cloud

# Discourse Scales
Discourse_Min   <- 0      # Lower boundary of the discourse universe
Discourse_Max   <- 100    # Upper boundary of the discourse universe
DecimalPlaces   <- 3      # Decimal precision for the console printout


# ------------------------------------------------------------------------------
# CORE ENGINE: CLOUD DELPHI ALGORITHM
# ------------------------------------------------------------------------------
CloudDelphi_Core <- function(CurrentData, ForegoingData = NULL, ForegoingEntropy = NULL, 
                             eps = epsilon, xi_param = xi) {
  
  if (length(CurrentData) %% 4 != 0) stop("Error: 'CurrentData' length must be a multiple of 4.")
  
  Experts <- length(CurrentData) / 4
  Ex_i <- numeric(Experts); En_L <- numeric(Experts); En_R <- numeric(Experts)
  He_i <- numeric(Experts); Conf_i <- numeric(Experts)
  
  for (i in 1:Experts) {
    idx <- (i - 1) * 4 + 1
    ex_min  <- CurrentData[idx]
    ex_max  <- CurrentData[idx + 1]
    ex_prob <- CurrentData[idx + 2]
    ex_sec  <- CurrentData[idx + 3]
    
    Ex_i[i]   <- ex_prob
    Conf_i[i] <- ex_sec
    Insecurity_Factor <- 1 - ex_sec
    
    # Base split-entropies derivation using the dynamic 3-sigma rule and \xi bound
    En_L[i] <- max(((ex_prob - ex_min) * (1 + Insecurity_Factor)) / 3, xi_param)
    En_R[i] <- max(((ex_max - ex_prob) * (1 + Insecurity_Factor)) / 3, xi_param)
    
    # Bounded Internal Anisotropy Index (A_i) implementation matching LaTeX
    A_i <- abs(En_L[i] - En_R[i]) / (En_L[i] + En_R[i] + eps)
    
    # Adaptive Fuzziness Modulator (K_Dynamic) within [2, 6) supremum configuration
    K_Dynamic <- max((3 + (3 * ex_sec)) * (1 - (0.5 * A_i)), 2.0)
    
    # Baseline hyper-entropy (He_base) matching LaTeX initialization formula
    He_base <- (En_L[i] + En_R[i]) / (2 * K_Dynamic)
    
    # Consensus Phase (r >= 2): Kinematic volatility penalty (\Omega_i)
    if (!is.null(ForegoingData) && length(ForegoingData) >= idx + 2) {
      var_min  <- abs(ex_min - ForegoingData[idx])
      var_max  <- abs(ex_max - ForegoingData[idx + 1])
      var_prob <- abs(ex_prob - ForegoingData[idx + 2])
      
      # The normalization factor 18 stems from a triple application of 3-sigma
      Omega_i  <- (var_min + var_max + var_prob) / 18
      He_i[i]  <- He_base + Omega_i
    } else {
      He_i[i]  <- He_base
    }
  }
  
  # Group Synthesis and Expert Weighting Engine (Convex Normalization)
  Ex_s_prov <- mean(Ex_i)
  Weight <- Conf_i / (abs((Ex_i - Ex_s_prov) / Ex_s_prov) + ((En_L + En_R)/2) + He_i + eps)
  Weight <- Weight / sum(Weight)
  
  Ex_s  <- weighted.mean(Ex_i, Weight)
  He_s  <- weighted.mean(He_i, Weight)
  
  En_Ls <- (Ex_s - min(Ex_i - 3 * En_L)) / 3
  En_Rs <- (max(Ex_i + 3 * En_R) - Ex_s) / 3  
  
  En_MeanCurrent <- (En_L + En_R) / 2
  DeltaEntropyIndex <- numeric(Experts + 1)
  
  if (!is.null(ForegoingEntropy)) {
    DeltaEntropyIndex[1:Experts] <- abs(ForegoingEntropy[1:Experts] - En_MeanCurrent) / max(ForegoingEntropy[1:Experts], eps)
    DeltaEntropyIndex[Experts + 1] <- abs(ForegoingEntropy[Experts + 1] - ((En_Ls + En_Rs)/2)) / max(ForegoingEntropy[Experts + 1], eps)
  }
  
  UncertaintyIndex <- c(He_i / En_MeanCurrent, He_s / ((En_Ls + En_Rs) / 2))
  
  Round <- rbind(Ex_Prob = c(Ex_i, Ex_s), En_Left = c(En_L, En_Ls), En_Right = c(En_R, En_Rs),
                 He_Hyper = c(He_i, He_s), Weight = c(Weight, 1.00), 
                 Delta_En = DeltaEntropyIndex, Unc_Index = UncertaintyIndex)
  colnames(Round) <- paste0("Exp_", c(1:Experts, "s"))
  return(Round)
}


# ------------------------------------------------------------------------------
# EVOLUTION ENGINE & GRAPHICS RENDERER
# ------------------------------------------------------------------------------
CloudDelphi_Evolution <- function(RoundsList, OutputDir = NULL, FilePrefix = "CloudDelphi_Evolution_Analysis",
                                  eps = epsilon, xi_param = xi, tau_val = tau, theta_val = theta, r_max = R_max) {
                                    
  TotalRounds <- length(RoundsList)
  if (TotalRounds == 0) stop("Error: The 'RoundsList' cannot be empty.")
  
  if (is.null(OutputDir)) OutputDir <- file.path(Sys.getenv(ifelse(.Platform$OS.type == "windows", "USERPROFILE", "HOME")), "Documents")
  if (!dir.exists(OutputDir)) dir.create(OutputDir, recursive = TRUE)
  
  EpsPath <- file.path(OutputDir, paste0(FilePrefix, ".eps"))
  PdfPath <- file.path(OutputDir, paste0(FilePrefix, ".pdf"))
  
  ResultsHistory <- list()
  ActiveRounds <- TotalRounds
  
  for (r in 1:TotalRounds) {
    ForegoingData <- if (r == 1) NULL else RoundsList[[r - 1]]
    ForegoingEntropy <- if (r == 1) NULL else (ResultsHistory[[r - 1]]["En_Left", ] + ResultsHistory[[r - 1]]["En_Right", ]) / 2
    
    ResultsHistory[[r]] <- CloudDelphi_Core(CurrentData = RoundsList[[r]], 
                                            ForegoingData = ForegoingData, 
                                            ForegoingEntropy = ForegoingEntropy,
                                            eps = eps, xi_param = xi_param)
    
    # Convergence and Disjunction logic matching formalized LaTeX conditions
    if (r > 1) {
      Upsilon_E <- ResultsHistory[[r]]["Delta_En", "Exp_s"] < tau_val
      Upsilon_U <- abs(ResultsHistory[[r]]["Unc_Index", "Exp_s"] - ResultsHistory[[r-1]]["Unc_Index", "Exp_s"]) < theta_val
      
      if (Upsilon_E || Upsilon_U || r >= r_max) {
        cat(sprintf("\n[SYSTEM] Disjunction reached in Round %d. Evolution halted.\n", r))
        ActiveRounds <- r
        break
      }
    }
  }
  
  # Inner Function: Dashboard Plots Rendering Engine
  GenerateDashboardPlots <- function() {
    old_par <- par(oma = c(5, 1, 1, 1), mar = c(4.5, 4.5, 3.5, 1.5), cex.lab = 1.1, cex.axis = 1.0)
    layout(matrix(1:ActiveRounds, nrow = 1, ncol = ActiveRounds))
    
    for (r in 1:ActiveRounds) {
      RoundMatrix <- ResultsHistory[[r]]
      Experts <- ncol(RoundMatrix) - 1
      
      plot(0, 0, type = "n", ylim = c(-0.02, 1.15), xlim = c(Discourse_Min, Discourse_Max),
           xlab = "Universe of Discourse (x)", ylab = expression(paste("Certainty Degree (", mu, ")")), 
           main = sprintf("Delphi Evaluation: Round %d", r), cex.main = 1.2)
      
      DrawCloudSpace <- function(Ex, En_L, En_R, He, Color, Size, N, IsSynthetic = FALSE) {
        X_i <- numeric(N); Mu_i <- numeric(N)
        for (j in 1:N) {
          En_Side <- ifelse(runif(1) > 0.5, En_R, En_L)
          En_Rand <- rnorm(1, mean = En_Side, sd = He)
          if(En_Rand <= 0) En_Rand <- xi_param  # Protected utilizing the structural parameters
          X_val <- rnorm(1, mean = Ex, sd = En_Rand)
          X_i[j] <- X_val
          Mu_i[j] <- exp(-((X_val - Ex)^2) / (2 * En_Rand^2))
        }
        points(X_i, Mu_i, col = Color, pch = ifelse(IsSynthetic, 4, 20), cex = Size)
        if (IsSynthetic) curve(exp(-((x - Ex)^2) / (2 * ifelse(x > Ex, En_R, En_L)^2)), add = TRUE, col = "black", lwd = 2.5)
      }
      
      # Drawing loops using user-adjustable drop bounds
      for (i in 1:Experts) DrawCloudSpace(RoundMatrix["Ex_Prob", i], RoundMatrix["En_Left", i], RoundMatrix["En_Right", i], RoundMatrix["He_Hyper", i], "grey65", 0.35, Drops_Expert)
      DrawCloudSpace(RoundMatrix["Ex_Prob", "Exp_s"], RoundMatrix["En_Left", "Exp_s"], RoundMatrix["En_Right", "Exp_s"], RoundMatrix["He_Hyper", "Exp_s"], "black", 0.55, Drops_Synthetic, IsSynthetic = TRUE)
      
      text(RoundMatrix["Ex_Prob", "Exp_s"], 1.12, "Exp_s", col = "black", cex = 1.0, font = 2)
      for (i in 1:Experts) {
        Y_Offset <- ifelse(i %% 2 == 0, 1.04, 1.08)
        text(RoundMatrix["Ex_Prob", i], Y_Offset, colnames(RoundMatrix)[i], col = "grey30", cex = 0.8)
      }
      
      stats_text <- c(
        sprintf("Crest (Ex): %s", format(round(RoundMatrix["Ex_Prob", "Exp_s"], 2), nsmall=2)),
        sprintf("Ambiguity (En): %s", format(round((RoundMatrix["En_Left", "Exp_s"] + RoundMatrix["En_Right", "Exp_s"])/2, 2), nsmall=2)),
        sprintf("Blurriness (He): %s", format(round(RoundMatrix["He_Hyper", "Exp_s"], 2), nsmall=2))
      )
      
      TargetX <- Discourse_Min + (Discourse_Max - Discourse_Min) * 0.72
      legend(x = TargetX, y = 0.75, title = "Group Consensus", legend = stats_text, 
             bty = "o", bg = "#F8F9FA", box.col = "grey75", cex = 0.85)
    }
    
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    legend("bottom", legend = c("Individual Expert Clouds (Empirical)", "Collective Synthetic Cloud (Group Drops)", "Collective Expected Profile (Mathematical Model)"),
           col = c("grey62", "black", "black"), lty = c(0, 0, 1), lwd = c(0, 0, 2.5), text.width = c(22, 22, 22), 
           pch = c(20, 4, NA), pt.cex = c(1.2, 1.1, 0), bty = "n", horiz = TRUE, cex = 1.0)
    par(old_par)
  }
  
  # Automated Graphics Exporter (EPS & PDF)
  postscript(file = EpsPath, width = 24, height = 9, pointsize = 14, onefile = FALSE, horizontal = FALSE, paper = "special")
  GenerateDashboardPlots()
  dev.off()
  
  pdf(file = PdfPath, width = 24, height = 9, pointsize = 14)
  GenerateDashboardPlots()
  dev.off()
  
  # Console Printer Engine
  NamedResults <- list()
  for (r in 1:ActiveRounds) {
    cat(paste0("\n>>> METRICS FOR ROUND ", r, " <<<\n"))
    print(round(ResultsHistory[[r]], DecimalPlaces))
    NamedResults[[paste0("Round_", r)]] <- round(ResultsHistory[[r]], DecimalPlaces)
  }
  return(NamedResults)
}


# ------------------------------------------------------------------------------
# EXECUTION PIPELINE
# ------------------------------------------------------------------------------
Round1_Data <- c(15, 75, 20, 0.85, 10, 60, 52, 0.05, 30, 90, 35, 0.75, 10, 50, 15, 0.60, 20, 85, 80, 0.85, 15, 70, 22, 0.02, 25, 80, 30, 0.70)
Round2_Data <- c(25, 55, 30, 0.95, 35, 75, 42, 0.80, 30, 60, 34, 0.92, 28, 50, 32, 0.88, 32, 65, 36, 0.90, 40, 75, 45, 0.85, 28, 55, 33, 0.91)
Round3_Data <- c(30, 42, 36, 0.98, 34, 46, 40, 0.95, 32, 44, 38, 0.96, 31, 43, 37, 0.94, 33, 45, 39, 0.97, 35, 47, 41, 0.92, 30, 42, 36, 0.95)

DelphiStudyRounds <- list(Round_1 = Round1_Data, Round_2 = Round2_Data, Round_3 = Round3_Data)

Delphi_Outputs <- CloudDelphi_Evolution(RoundsList = DelphiStudyRounds)
