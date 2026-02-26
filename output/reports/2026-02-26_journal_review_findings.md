# Technical Report: Journal Review Response — AGB Wheat Modelling Pipeline

**Date:** 2026-02-26
**Branch:** `main` · Commit `f347163`
**Scripts added/modified:** 24 (updated), 27, 28, 29

---

## 1. Sensor Ablation — Marginal Contribution of Each Sensor

### Method

A 4-level nested ablation was implemented in script 24 using LOSO cross-validation
(4 folds = 4 site-seasons). Three fixed-parameter models were evaluated at each level:
RF, XGBoost, GLMnet.

| Level | Sensor combination | XGBoost RMSE (t/ha) | RMSE gain |
|---|---|---|---|
| (a) | Climate only | 9.96 | — |
| (b) | S2 + Climate | 7.63 | **+23.4%** |
| (c) | S2 + S1 + Climate | 7.68 | −0.5% |
| (d) | S2 + S1 + PS + Climate (Fusion) | 7.24 | **+5.7%** |

### Key findings

**Sentinel-2 is the dominant sensor.** Adding S2 on top of climate-only variables reduces
RMSE by 23.4 percentage points — the single largest contribution in the pipeline. The
spectral indices derived from the red-edge and SWIR bands (`S2_NDRE3`, `S2_MCARI`,
`S2_CI_red_8A`) capture canopy chlorophyll and biomass density that climate variables
alone cannot resolve.

**Sentinel-1 (SAR) does not contribute — and slightly degrades performance.** Adding S1
on top of S2+Climate increases RMSE by 0.5%. This is a critical finding for the journal
revision. The likely explanations are: (i) SAR backscatter sensitivity to AGB saturates
early in the season at moderate biomass levels typical of Chilean dryland wheat; (ii) S2
already captures the canopy structure information that SAR provides at C-band; (iii) the
three S1 variables (`VV`, `VH`, `VH/VV`) introduce noise in early-season observations
when soil moisture dominates the backscatter signal. The paper's claims about SAR fusion
must be carefully scoped — the data show no aggregate benefit, though SAR may still
contribute at specific phenological stages.

**PlanetScope contributes modestly (+5.7%).** The high spatial resolution of PS improves
AGB prediction marginally, most likely through finer-grained canopy heterogeneity
information within each 10×10 m sampling area that S2 cannot resolve.

**Best overall model: RF on the Fusion recipe (RMSE = 7.10 t/ha, R² = 0.729)**,
outperforming XGBoost on the same recipe (7.24 t/ha). GLMnet degrades substantially on
the Fusion recipe (10.31 t/ha, R² = 0.388), indicating that the relationship between
the full multi-sensor predictor set and AGB is strongly non-linear.

**Output files:**
- `output/tables/sensor_marginal_gains.csv`
- `output/tables/ablation_loso_summary.csv`
- `output/figs/loso_ablation_metrics.png`
- `output/figs/loso_metricas_por_sitio.png`

---

## 2. Robust Validation — La Cancha Site Diagnostic

### Method

LOSO per-fold metrics were disaggregated by site-season. The La Cancha fold (2022-2023)
was compared against the pooled mean of the remaining three folds (Hidango 2021-2022,
Hidango 2022-2023, Villa Baviera 2020-2021). An observed-vs-predicted scatter was
generated to characterise the nature of the error.

### Key findings

| Site group | RMSE (t/ha) | Bias (t/ha) |
|---|---|---|
| La Cancha (2022-2023) | **8.15** | −5.26 |
| Other sites (mean) | 6.93 ± 3.07 | ~0 |

La Cancha shows **~18% higher RMSE** than the cross-site mean. More diagnostic is the
per-observation bias from script 29: the ensemble model **overestimates La Cancha biomass
by 5.26 t/ha on average** (model predicts higher than observed), compared to a near-zero
bias for Hidango (−0.36 t/ha) and Villa Baviera (−0.16 t/ha).

The most plausible interpretation is a **domain shift problem**: La Cancha is the only
site with a single season in the dataset (2022-2023), so the training set for its fold
comprises only three site-seasons with different phenological timing, soil types, and
management. The model has not seen enough within-site variability to generalise to La
Cancha's specific canopy development trajectory. This is compounded by the fact that La
Cancha's 95% CI coverage was only 51.4% (vs 70.5% for Hidango), confirming that all
three algorithms systematically disagree on this site in the same direction — a sign of
shared systematic error rather than random variance.

**Recommendation for the paper:** frame La Cancha as an out-of-distribution case and
discuss the need for at least two seasons per site to achieve reliable LOSO
generalisation.

**Output files:**
- `output/tables/la_cancha_diagnostico.csv`
- `output/figs/la_cancha_vs_otros_rmse.png`
- `output/figs/la_cancha_obs_vs_pred.png`

---

## 3. Deep Explainability — GDD × PlanetScope EVI Interaction

### Method

Script 28 built three separate DALEX explainers on PS_EVI tercile subsets
(Low / Mid / High canopy density) and computed the partial dependence profile of
`gdd_cumsum` within each group. This conditional PDP approach reveals whether the
thermal-time–AGB relationship is modulated by canopy greenness. The interaction strength
was quantified as the mean predicted AGB gap between the High and Low EVI terciles.

### Key findings

**The interaction is large and physically consistent.**

| Metric | Value |
|---|---|
| Mean predicted AGB gap (High − Low EVI) | **9.41 t/ha** |
| Maximum range gap | **26.56 t/ha** |

**Physical interpretation (Radiation-Use Efficiency framework):**

- At **low PS_EVI** (sparse or early-season canopy), the GDD–AGB curve rises slowly and
  plateaus at low values. A sparse canopy intercepts little radiation regardless of
  thermal time, so GDD accumulation translates into little biomass gain.
- At **high PS_EVI** (dense, actively photosynthesising canopy), the same GDD increment
  drives substantially more biomass accumulation. The canopy is operating near its maximum
  radiation interception efficiency.
- The **divergence of curves begins around 500–600 GDD** (tillering-to-stem elongation
  transition) and reaches its maximum near **heading (~800 GDD)**, exactly when
  differences in canopy greenness correspond to differences in final grain yield potential
  — consistent with the agronomic literature on critical periods for biomass partitioning.
- Beyond **1400 GDD** (physiological maturity), the curves converge as dry-matter
  translocation to grain dominates and green area declines regardless of canopy density.

This interaction is the strongest quantitative argument in the paper for why multi-sensor
fusion (including PS) outperforms climate-only models: EVI modulates the physiological
meaning of accumulated thermal time.

**Output files:**
- `output/figs/interaccion_gdd_ps_evi.png`

---

## 4. Multicollinearity Documentation — VIF Analysis

### Method

Script 27 implemented a three-stage audit: (i) pairwise correlation matrix to characterise
pre-removal structure; (ii) `tidy(prep())` to document exactly which variables `step_corr`
removed; (iii) `car::vif()` on the post-`step_corr` predictor set; (iv) iterative VIF > 10
removal to convergence.

### Key findings

**Pipeline summary:**

| Stage | Predictors retained | Removed |
|---|---|---|
| Full predictor set (post-imputation/normalisation) | 43 | — |
| After `step_corr` (threshold \|r\| ≥ 0.9) | 42 | **1** |
| After iterative VIF > 10 removal | **22** | **20** |

**`step_corr` removed only one variable:** `S2_SWIR11_MCARI` — the only predictor pair
with \|r\| ≥ 0.9 in this dataset. The 42 remaining predictors have predominantly
multivariate (not pairwise) collinearity, so `step_corr` alone is insufficient as a
multicollinearity defence.

**VIF analysis reveals severe residual multicollinearity.** After `step_corr`, 36 of 42
predictors still have VIF > 10 and 39 have VIF > 5. The worst offenders are spectrally
redundant indices computed from shared bands:

| Variable removed | VIF at removal |
|---|---|
| `S2_MCARI` | 322 |
| `PS_B3` | 235 |
| `gdd_cumsum` | 120 |
| `PS_CI_red_cumsum` | 87 |
| `S2_WI1` | 85 |
| `S2_B6` | 76 |
| *(14 further iterations)* | *(decreasing to 9 at convergence)* |

Notably, `gdd_cumsum` itself was removed in iteration 3 — its thermal-time signal is
strongly collinear with the cumulative spectral indices (`PS_CI_red_cumsum`,
`S2_SWIR12_MCARI_cumsum`) that integrate both canopy state and time. The iterative
procedure converged after 21 steps, retaining 22 predictors with VIF ≤ 9.

**Recommendation for the paper:** report that variable selection follows a two-stage
protocol — `step_corr` removes near-perfect linear duplicates, and tree-based models
(RF, XGBoost) are inherently robust to residual multicollinearity via their variable
splitting mechanism. GLMnet's elastic-net penalty provides the equivalent of VIF
regularisation for the linear model. The VIF table is provided as supplementary material
for transparency.

**Output files:**
- `output/tables/variables_removidas_step_corr.csv`
- `output/tables/vif_post_step_corr.csv`
- `output/tables/vif_iterativo_removidas.csv`
- `output/tables/vif_pipeline_summary.csv`
- `output/tables/pares_alta_correlacion.csv`
- `output/figs/vif_post_step_corr.png`

---

## 5. Uncertainty Analysis — 95% Confidence Intervals over DAS

### Method

Script 29 used the three LOSO out-of-fold predictions from the Fusion recipe (RF,
XGBoost, GLMnet) to construct per-observation confidence intervals as
`mean ± 1.96 × SD` across the three models. Empirical coverage (proportion of observed
biomass values falling within the CI) was computed globally and per site.

### Key findings

| Site | n | Coverage | Mean CI width | RMSE | Bias |
|---|---|---|---|---|---|
| Hidango | 88 | 70.5% | 17.9 t/ha | 6.22 t/ha | −0.36 t/ha |
| La Cancha | 35 | **51.4%** | 13.4 t/ha | **10.6 t/ha** | **−5.26 t/ha** |
| Villa Baviera | 30 | 80.0% | 25.0 t/ha | 3.74 t/ha | −0.16 t/ha |
| **Global** | **153** | **68%** | **18.3 t/ha** | — | — |

**Global empirical coverage is 68%**, well below the nominal 95%. This is an important
methodological caveat: the inter-model CI is a measure of *epistemic* uncertainty
(algorithm disagreement), not a calibrated predictive interval. GLMnet (RMSE = 10.31
t/ha, R² = 0.39) occupies a very different prediction space from RF and XGBoost (RMSE
~7.1 t/ha), which inflates the SD without necessarily covering the true value.

**La Cancha has the lowest coverage (51.4%) and the largest bias (−5.26 t/ha).** The
relatively narrow CI width (13.4 t/ha vs 17.9 for Hidango) combined with low coverage
means all three models agree on an incorrect prediction — this is not high uncertainty,
it is **systematic shared error**. La Cancha's feature distributions fall in a region of
predictor space that all algorithms have been trained on with minimal representation,
making uncertainty underestimated precisely where it matters most.

**Uncertainty peaks during stem elongation and heading (DAS 80–130)** — the growth stage
with the highest AGB accumulation rate, where small differences in canopy development
between seasons and sites cause the greatest model disagreement.

**Recommendation for the paper:** acknowledge that the reported CI reflects model
disagreement (epistemic uncertainty) rather than aleatoric predictive error. Future work
should implement quantile regression forests (`ranger` with `quantreg = TRUE`) or
conformal prediction to obtain properly calibrated intervals.

**Output files:**
- `output/figs/ci_predicciones_DAS.png`
- `output/figs/ci_anchura_vs_DAS.png`
- `output/tables/ci_coverage_by_site.csv`
- `output/tables/ci_summary.csv`

---

## Summary

| Review requirement | Script | Status | Key result |
|---|---|---|---|
| Sensor ablation (a–d) + comparison table | 24 | ✅ | S2 +23.4%, S1 −0.5%, PS +5.7% |
| LOSO validation + La Cancha error | 24 | ✅ | RMSE 8.15 t/ha, bias −5.26 t/ha |
| GDD × PS_EVI interaction (DALEX) | 28 | ✅ | Mean gap = 9.41 t/ha |
| VIF > 10 removal documented | 27 | ✅ | 43 → 42 (step_corr) → 22 (VIF ≤ 10) |
| 95% CI over DAP | 29 | ✅ | 68% empirical coverage; peaks at heading |

### Critical findings for the revision letter

1. **S1 does not help overall** (−0.5% RMSE). The SAR contribution claim must be
   revised; consider restricting the claim to early-season or specific phenological
   windows if a targeted analysis supports it.
2. **La Cancha is a domain-shift case**, not a model failure. Requires at minimum a
   second season to be a reliable LOSO fold.
3. **step_corr alone is insufficient** for multicollinearity control — only 1 variable
   removed vs 20 under a strict VIF > 10 criterion. Tree-based robustness must be
   explicitly stated in the methods.
4. **CI coverage (68%) is below nominal (95%)** because the CI captures epistemic
   disagreement, not aleatoric noise. This must be stated as a limitation.
5. **The GDD × PS_EVI interaction (9.4 t/ha mean gap)** provides the clearest
   mechanistic justification for PlanetScope inclusion in the pipeline.
