# Cloud Delphi framework under asymmetric uncertainty — R scripts

Supplementary code for:

> Cruz-Ramírez, M. *Cloud Delphi framework under asymmetric uncertainty*.
> Croatian Operational Research Review (CRORR). DOI: 10.17535/crorr.2027.0013

Contact: mcruz65@uan.edu.co

These scripts implement the **asymmetric Cloud Delphi** model. Each expert's
judgment is a cloud with two entropies, a left one (`En_L`) and a right one
(`En_R`). Hyper-entropy (`He`) combines the expert's self-reported confidence
with inter-round volatility. The panel opinion is synthesized round by round,
and formal stopping criteria decide when the Delphi process ends. You can use
them with your own panel data; see *Using your own data* below.

---

## Files

| File | Purpose |
|---|---|
| `CloudDelphi.R` | **Main script.** Processes all Delphi rounds, applies the stopping criteria, prints the metrics of each round (Table 1 of the article) and draws the evolution of the clouds (Figure 2). |
| `Sensitivity.R` | One-at-a-time sensitivity analysis of the five design constants (ε, ξ, 18, 2, 3), each perturbed by ±20% and ±50%. |
| `sensitivity_results.csv` | Output of `Sensitivity.R` for the panel in the article. |
| `Comparison.R` | Compares the asymmetric model with the classic symmetric Cloud Delphi baseline. |
| `AnisotropyAnalysis.R` | Reanalysis of the published data of Cruz & Cables (2022) (N = 56) on individual cognitive anisotropy. |
| `Pseudocode.pdf` | Pseudocode of the two core algorithms (one round, and the multi-round loop). |

## Requirements

- R version 4.0 or later (tested with R 4.5.2).
- No additional packages: everything uses base R (`stats`, `graphics`, `grDevices`).

## Quick start

```r
source("CloudDelphi.R")
```

This reproduces the case study of the article (N = 7 experts, 3 rounds). It does three things:

1. It prints the metrics of each round to the console, in the same layout as Table 1 of the article.
2. It reports the round in which the process stops, for example
   `[SYSTEM] Disjunction reached in Round 3. Evolution halted.`
3. It saves `CloudDelphi_Evolution_Analysis.eps` and `.pdf` to your `Documents` folder (Figure 2 of the article).

A fixed seed (`set.seed(2026)`) makes the random cloud drops reproducible.

## Input data format

Each round is one numeric vector with **4 values per expert**, in this order:

```
x_min, x_max, Ex, C
```

| Value | Meaning | Constraint |
|---|---|---|
| `x_min` | minimum forecast | `x_min <= Ex` |
| `x_max` | maximum forecast | `Ex <= x_max` |
| `Ex` | most probable forecast (cloud crest) | inside `[x_min, x_max]` |
| `C` | self-reported confidence | in `[0, 1]` |

**About the confidence scale.** In the article's experiment, confidence was
rated from 0 to 100 and divided by 100. The article recommends the simpler
eleven-point scale {0.0, 0.1, …, 1.0}, because coarsening to it did not change
the results in any relevant way. Either scale works, as long as `C` ends up in
[0, 1]. Note that `C = 0` gives that expert a weight of exactly zero in that
round.

Example with 3 experts in one round:

```r
Round1_Data <- c(15, 75, 20, 0.80,    # expert 1
                 10, 60, 52, 0.10,    # expert 2
                 30, 90, 35, 0.70)    # expert 3
```

The experts must appear **in the same order in every round**, because
hyper-entropy compares each expert's answers with that same expert's answers
in the previous round.

## Using your own data

1. Open `CloudDelphi.R` and go to the section **EXECUTION PIPELINE** at the end.
2. Replace `Round1_Data`, `Round2_Data`, … with your own vectors, using the format above.
3. List the rounds in `DelphiStudyRounds`:
   ```r
   DelphiStudyRounds <- list(Round_1 = Round1_Data, Round_2 = Round2_Data)
   ```
4. If necessary, adjust the parameters in the **GLOBAL CONFIGURATION BLOCK** at the top:

| Parameter | Default | Meaning |
|---|---|---|
| `epsilon` | 0.01 | numerical stabilizer (anisotropy index, weights, ΔEn) |
| `xi` | 0.001 | minimum split-entropy |
| `tau` | 0.50 | stopping threshold for the relative variation of the synthetic entropy |
| `theta` | 0.20 | stopping threshold for the variation of the uncertainty index `I_s` |
| `R_max` | 4 | maximum number of rounds |
| `Discourse_Min`, `Discourse_Max` | 0, 100 | universe of discourse (x-axis of the plots) |
| `Drops_Expert`, `Drops_Synthetic` | 1200, 2500 | number of drops drawn per cloud |

The values of `tau`, `theta` and `R_max` depend on each study and should be
set by the coordinating group.

To change where the figures are saved, pass `OutputDir`:

```r
CloudDelphi_Evolution(RoundsList = DelphiStudyRounds, OutputDir = "C:/my/folder")
```

## Output

For each round, the console shows one matrix with a column per expert
(`Exp_1`, …) and a column `Exp_s` for the panel's synthetic cloud:

| Row | Meaning |
|---|---|
| `Ex_Prob` | expectation (crest) |
| `En_Left`, `En_Right` | left and right split-entropies |
| `He_Hyper` | hyper-entropy |
| `Weight` | normalized expert weight (`Exp_s` = 1) |
| `Delta_En` | relative change of the mean entropy with respect to the previous round (each expert relative to their own previous value) |
| `Unc_Index` | uncertainty index `He / mean entropy` |

The function returns the same matrices as a list (`Delphi_Outputs`).

## Model summary

- **Split-entropies:** `En_L = (Ex − x_min)(2 − C)/3`, `En_R = (x_max − Ex)(2 − C)/3`, each bounded below by `xi`.
- **Initial hyper-entropy:** `He = (En_L + En_R) / (2·K)`, with `K = max((3 + 3C)(1 − A/2), 2)` and anisotropy index `A = |En_L − En_R| / (En_L + En_R + ε)`.
- **Later rounds:** an inter-round volatility term `Ω = (|Δx_min| + |Δx_max| + |ΔEx|)/18` is added to `He`.
- **Weights:** proportional to `C`, and inversely proportional to the expert's relative distance from the panel mean plus their entropy and hyper-entropy.
- **Cloud drops:** split double randomization. The side is chosen with probability `En_L/(En_L + En_R)` or `En_R/(En_L + En_R)`, the entropy of that side is perturbed with `He`, and the drop is placed on that side of `Ex`.
- **Stopping:** the process stops when the synthetic entropy stabilizes (`ΔEn < tau`), the uncertainty index stabilizes (`|ΔI_s| < theta`), or `R_max` is reached.

See `Pseudocode.pdf` and the article for the full formulation.

## Other scripts

- **`Sensitivity.R`.** Edit section *1. RAW PANEL DATA* to use other data (the format is a list with `xmin`, `xmax`, `Ex`, `C` per round). It writes `sensitivity_results.csv` to the working directory (`getwd()`). In the CSV, `stopping_round = NA` means that the process did not stop within the available rounds.
- **`Comparison.R`.** Prints the percentage differences in entropy and hyper-entropy between the symmetric baseline and the asymmetric model for Rounds 1 and 2. In Round 3 every expert reported a symmetric range, so both models coincide.
- **`AnisotropyAnalysis.R`.** Uses the data of Cruz & Cables (2022), *Investigación Operacional*, 43(2), 102–119. Three experts per round reported a most-likely value outside their own range; they are retained as published.

## Version notes

Compared with the first version of this repository, this version:

- computes `Delta_En` for each expert relative to that expert's own previous entropy (it previously used the maximum over the panel);
- implements the split drop generator described in the article (Eq. 8) and fixes the random seed;
- runs `Sensitivity.R` on the actual panel data and defines `I_s` as in the article (`He_s` divided by the mean synthetic entropy).

## How to cite

Please cite the article above. If you use the code, please cite this Zenodo record as well.
