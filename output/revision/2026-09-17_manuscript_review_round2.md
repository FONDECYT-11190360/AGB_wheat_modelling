# Simulated Peer Review, Round 2 (ARS academic-paper-reviewer, full mode)

**Manuscript:** "Explainable machine learning for wheat above-ground biomass estimation under spatial cross-validation: sensor contributions and exploratory harvest forecasting" (`manuscript/manuscript.qmd`, commit 693b10b plus uncommitted edits)
**Target venue (from YAML):** Computers and Electronics in Agriculture (Elsevier, 3p review format). The subtitle also labels it an EarthArXiv preprint.
**Review date:** 2026-09-17
**Line anchors** refer to `manuscript/manuscript.qmd`. **Code anchors** refer to `script/`. Every code or data claim below was checked against the repository during this review.

---

## Panel provenance disclosure

| Axis | Observed value |
|---|---|
| Role separation | Five role cards (Journal Fit, Methodology, Domain, Perspective, Devil's Advocate) plus synthesizer |
| Invocation context freshness | One shared context; seats written sequentially |
| Peer output visibility | Not blind; each seat could see earlier seats |
| Model family | Single family (Claude Opus 5) for all seats |
| Provider | Single provider |
| Accountable human | Not assigned (author self review) |

Correlated error disclosure: all seats share one model and one context, so agreement among seats is not independent corroboration. Treat consensus items as one careful reader's findings verified against code, not as five votes. Reviewer calibration status: `NOT_CALIBRATED`. No venue criteria binding was supplied (`criteria_binding_unavailable`), so venue fit comments are general.

## Phase 0: Reviewer configuration

| Seat | Configured identity |
|---|---|
| Journal Fit Reviewer (EIC) | Associate editor at Computers and Electronics in Agriculture, crop remote sensing and ML applications |
| Reviewer 1, Methodology | Statistician specialised in spatial cross validation, model selection bias and ML interpretability |
| Reviewer 2, Domain | Wheat agronomist with SAR and optical crop biomass retrieval experience |
| Reviewer 3, Perspective | Agricultural decision support and operational crop monitoring specialist |
| Devil's Advocate | Fixed seat: core argument challenge |

Field: agricultural remote sensing × applied machine learning. Paradigm: empirical, predictive modelling. Maturity: revised draft, several rounds of internal revision.

---

## Report 1: Journal Fit Reviewer

**Summary.** The paper evaluates 40 recipe × algorithm combinations plus a stacked ensemble for in season wheat AGB estimation from Sentinel-1, Sentinel-2, PlanetScope, weather and soil moisture across four site seasons in central Chile, under LOSO CV, with DALEX explanations and an exploratory stage 2 harvest self consistency analysis.

**Strengths.**
1. The move from a random 75/25 split to LOSO CV (lines 236, 288) is a real improvement over Round 1 and aligns with current expectations for spatial transferability claims.
2. Candid limitations on non nested tuning, habit confounding and the circular stage 2 target (lines 385 to 395).
3. Open code and data (line 411).

**Fit and significance concerns.**
- **Novelty is thin relative to the companion letter.** Table 6 (line 298) reproduces the same ablation numbers reported in the GRSL letter (line 60, `cover_letter_disclosure.md`). An editor will ask what the reader gains here beyond more recipe/algorithm permutations. The DALEX and stage 2 additions are the differentiators, but the manuscript itself concludes both are unreliable (lines 322, 373, 395). This leaves the contribution resting on a ranking whose basis is questioned below (Reports 2 and 5).
- **Headline claims are not yet robust enough for CEA.** "PlanetScope only" and "policy relevant accuracy" (lines 15, 377, 403) are both contradicted by verified code and data (see C1, M5).
- **Hedging overload.** The same caveat set (four folds, non nested tuning, habit confounding, circular stage 2) is restated in the abstract, Methods, Results, Discussion (twice) and Conclusion. The paper now reads as defensive. Consolidate into one limitations section and state findings plainly elsewhere.
- **Venue identity.** YAML targets CEA; subtitle says EarthArXiv; memory of earlier rounds says AGWAT. Settle before submission and check the CEA abstract length limit (current abstract is roughly 380 words, line 13 to 15).

**Recommendation signal:** Major Revision.

---

## Report 2: Reviewer 1, Methodology

### C1 (Critical). "Sensor only" recipes silently contain accumulated GDD
`script/14_modelado_con_ML.R` defines the single sensor recipes by removing only `pp_cumsum` and `sm_mm`:
```r
model_rec_ps <- model_rec_todo |> step_rm(pp_cumsum, sm_mm, starts_with(c('S1','S2')))
```
`gdd_cumsum` therefore remains in rec2 (S1), rec5 (S2) and rec7 (PS). The headline model "RF using only PlanetScope predictors" (lines 15, 220, 288, 292, 399) is in fact PlanetScope + ∑GDD. ∑GDD is a direct clock of season progress and needs station temperature plus observed phenology dates to switch base temperature (line 109). Consequences:
- Line 379 ("relies on PlanetScope imagery without requiring in situ soil moisture") is technically true but hides the dependence on station temperature and phenology observations.
- The claim that "Sentinel-1 backscatter alone carries little transferable information" (line 288) is actually "S1 + ∑GDD".
- Recipe descriptions at line 220 and Figure 4 caption (line 290) are wrong.

**Fix:** either rerun sensor only recipes without `gdd_cumsum`, or relabel them as "sensor + ∑GDD" throughout and add ∑GDD alone as an explicit baseline.

### C2 (Critical). Reported R² is not the R² defined in the Supplement
All tuning and reporting use `yardstick::rsq` (`script/14_modelado_con_ML.R` `metric_set(rsq, rmse, mae)`, `select_best(metric = "rsq")`; same in scripts 20 and 24). `rsq` is the squared Pearson correlation. Supplementary Eq. S3 defines the coefficient of determination, 1 − SSE/SST (`rsq_trad`). Squared correlation ignores systematic bias, which matters here because a held out site with a level shift (La Cancha underestimation, line 330) can still score a high `rsq`. Every R² in the paper (40 models, stage 2, ablation) is affected, and hyperparameters were selected on it.

**Fix:** report `rsq_trad` (can be negative under LOSO, which is informative), or rename the metric as squared correlation and correct Eq. S3. Reselect hyperparameters on RMSE, which is the operationally meaningful metric.

### C3 (Critical). The model ranking reverses depending on the metric
From the manuscript's own numbers:

| Model | R² (rsq) | RMSE | MAE |
|---|---|---|---|
| rec7_RF (stated best, line 288) | 0.78 ± 0.08 | 7.87 ± 2.08 | 5.90 ± 2.32 |
| rec1_bagMLP (rank 6, line 288) | 0.75 | **6.96** | **5.34** |
| Fusion RF, fixed params (`output/tables/ablation_loso_summary.csv`) | 0.73 | **7.10** | **5.53** |

By RMSE and MAE, the all predictors models beat the PlanetScope recipe. The ranking in Fig. 4 is by squared correlation only (C2), and all differences sit inside one fold SD. The statements "rec1 did not outperform the PlanetScope only recipes" (line 288), "recipes built on PlanetScope ... consistently outranked rec1" (line 369) and the Conclusion (line 399) are therefore not supported. Figure 5 compares rec7 against rec1_RF (RMSE 7.88) rather than the better rec1_bagMLP, which reads as a selective comparator.

**Fix:** rank on RMSE (primary) and report a paired, per fold comparison. State that no recipe is distinguishable from the top group given four folds.

### C4 (Critical). The Sentinel-1 ablation conclusion depends on the algorithm chosen
`ablation_loso_summary.csv` contains RF, XGBoost and GLM results, but Table 6 reports XGBoost only:

| Step | RF RMSE | XGBoost RMSE | GLM RMSE |
|---|---|---|---|
| Weather only (∑PP, SM, ∑GDD) | 10.08 | 9.96 | **8.17** |
| + S2 | 8.09 | 7.63 | 8.90 |
| + S1 | 7.81 (−3.5 %) | 7.68 (+0.5 %) | 8.95 |
| + PS | 7.10 (−9.0 %) | 7.24 (−5.7 %) | 10.31 |

Under RF, S1 improves RMSE by 3.5 %, seven times the XGBoost change in the opposite direction. Under GLM, adding sensors makes things worse and weather only is the best configuration. The Conclusion statement that S1 "adds essentially no skill" (line 399) is an XGBoost result, not a general one. In addition:
- The ablation cannot "corroborate" the rec7 ranking (lines 236, 288, 389) because PS alone is never tested and the addition order is fixed (S2 → S1 → PS).
- It is not "independent" (lines 288, 296): same data, same folds, same predictors.
- The companion letter uses three algorithms (line 60) while this paper reports one; readers of both will notice.

**Fix:** report all three algorithms in Table 6, drop "independent" and "corroborates", and phrase the S1 finding as algorithm dependent and within fold noise.

### C5 (Critical). Ensemble, DALEX and Figures 6 to 9 cannot be reproduced from the repository as described
- Methods (line 240) says the ensemble was refit on rec1 with all predictors. The code (`script/14_modelado_con_ML.R`, section 11) calls `add_candidates(biom_res)` on the whole workflow set, i.e. candidates from all eight recipes.
- The ensemble's own LOSO performance is never reported, yet it drives Figs 6, 7, 8, 9 and is the stage 2 target.
- The committed DALEX script (`script/14.1_DALEX_vip.R`) builds the ensemble explainer on `biom_train` from `initial_split(data)` (random 75 %, seed 987), i.e. on data the ensemble was trained on. Permutation importance on training data measures fit, not transferable signal.
- The files `variables_importance_estimacion_modelo_ensamblado_LOSO.png`, `partial_dependence_plots_estimation_LOSO.png` and `estimation_vs_observed_AGB_LOSO.png` (dated 12 June) are not produced by any committed script. The Data availability statement (line 411) is therefore incomplete.
- Figure 8 (line 326 to 330) compares the ensemble's in sample fit with its own training observations; the phrases "adjusted well" and "underestimated" describe fit, not validation.

**Fix:** commit the scripts that generated the LOSO figures; describe the ensemble as the code builds it; report its LOSO (or at least out of fold) metrics; compute DALEX importance on out of fold assessment sets.

### M2 (Major). Undocumented predictor preselection
Methods describe 89 VIs plus bands and cumulative sums (line 169), but the models receive a fixed list of 43 predictors (`vars`, script 14 line 7, commented "Variables a seleccionar del análisis de correlacion"). The only described filter (`step_corr`, line 224) removes one variable. How 89+ candidates became 43 is not described. If that screening used correlation with biomass on the full dataset, it leaks information across all LOSO folds and inflates every metric. Describe the procedure and, if outcome aware, move it inside the resampling loop.

### M3 (Major). A time only null model is missing, and zero AGB anchors inflate fit
- 21 of 153 samples are AGB = 0 at sowing (`dataset.rds`; S_SOS in Fig. 2). These are assigned values, not measurements, and are trivially predictable from ∑GDD or cumulative indices near zero.
- Cumulative sums (∑GDD, ∑PP, ∑VIs) all rise monotonically with time. Within a held out site season, most AGB variance is seasonal growth, so a model that only knows "days since sowing" would already score well. The weather only GLM reaches rsq 0.66 (ablation CSV).
- Kalman smoothing (line 165) uses observations after each sampling date, so predictors are not strictly contemporaneous.

**Fix:** add a DAS only and a ∑GDD only baseline; report metrics with and without S_SOS points; state that smoothing is two sided.

### M4 (Major). Effective sample size and uncertainty
153 samples come from 5 to 6 points per field on 29 unique site dates, with shared weather and a single soil moisture sensor per field. Points in the same field likely fall in the same or adjacent 20 m S2 pixels. Metrics are means of four fold values with SDs of about 2 to 3.5 t/ha. Please report per fold results (a table of 4 × top models), give an uncertainty statement for the ranking (e.g. paired fold differences or a bootstrap over samples within folds), and avoid ordinal claims that the data cannot support.

### M7 (Major). The "nested CV is infeasible" justification is weak
Lines 236 and 389 argue an inner loop is impossible with four site seasons. An inner LOSO over the three training site seasons (three inner folds) is feasible and standard. More simply: the fixed parameter fusion RF (RMSE 7.10) already outperforms every tuned model on RMSE, so tuning brings no measurable gain. Consider reporting fixed parameter results as the primary estimates, which removes the selection optimism problem entirely.

### Minor (methodology)
- Table 4 lists `tree_depth` for RF; the ranger spec tunes only `mtry` and `min_n`.
- "Ten random candidates per parameter combination" (line 236): `grid = 10` produces ten space filling candidates per workflow.
- Line 216 says the ensemble "combines the predictions for the five models"; see C5.
- Dropout loss in Fig. 6 is reported as 11.4 to 11.6 "RMSE units" while model RMSE is about 7. Report the full model loss so readers can see the increments, which appear tiny.
- Table 5: N defined as "among the five most important" (line 345) but text says "top six" (line 340). Ordering by mean rank places ∑PS_GRVI, the dominant predictor at three leads, fourth.

---

## Report 3: Reviewer 2, Domain

### M5 (Major). AGB magnitudes are implausible and the text understates them
`dataset.rds` gives AGB up to **43.5 t/ha** dry mass (La Cancha), with 7 samples above 30 t/ha and a mean of 12.2 t/ha. The text says "up to ~30 t/ha" (lines 330, 367, 381). Total above ground dry biomass for wheat rarely exceeds about 20 to 25 t/ha even in high yield systems. 43.5 t/ha implies 1.09 kg dry mass in a 0.25 m² quadrat. Please check drying completeness (70 °C for 3 days on whole heads), the quadrat to hectare scaling, and whether some values are fresh weight. Also:
- AGB definition conflict: "dead and living" (line 76) versus "living plant material" (glossary, line 448).
- RMSE 7.87 t/ha is 64 % of mean AGB. "Policy relevant accuracy" (lines 377, 403) needs a stated threshold and justification; as it stands it is not supported.
- Line 381 computes "roughly 15 %" relative error against the maximum (30 t/ha) instead of the mean, which flatters the result.

### M1 (Major). PlanetScope band description contradicts the code
Line 157 states only the four bands common to all Dove generations were used. `script/02_calcular_vis.R` maps the SuperDove `rededge` band to `B5`, and the model uses PS_B5, PS_TCARI_OSAVI, PS_MCARI_OSAVI and PS_NDRE_NDVI, all of which need red edge. PS_B5 is the second most important predictor in Fig. 6 and in Table 5. Table 3 also lists MCARI/TCARI as "S2-PS" with RE₁. For scenes from Dove Classic or Dove-R (likely most of Villa Baviera 2020 to 2021), these predictors do not exist and were filled by Kalman interpolation or KNN imputation. A red edge variable whose availability depends on acquisition year can act as a site or era indicator. Please document per site the SuperDove fraction and the imputation share of each PS red edge predictor, and test rec7 with only the four common bands.

### M8 (Major). Sentinel-1 processing and a factual error
- Line 115: "Sentinel-1A, the only satellite of the constellation operational during the study period, as Sentinel-1B stopped transmitting in 2022". Sentinel-1B failed on 23 December 2021, so it was operational for all of Villa Baviera 2020 to 2021 and most of Hidango 2021 to 2022. Correct this and state whether S1B scenes were used.
- Orbit direction, relative orbit and incidence angle handling are not described; mixing geometries adds variance of the same order as crop signal in C band.
- No speckle filter, and extraction is a single 10 m pixel with no buffer (line 165). Single pixel GRD values carry several dB of speckle. The conclusion that S1 adds little (line 399) is conditional on this minimal processing and should be phrased so, ideally with a sensitivity check using a multi pixel mean or a Lee/refined Lee filter.
- The U shaped PDP for VH/VV with values above 1 in linear scale (line 322) is unusual for vegetated fields and may indicate noise or bare soil and water artefacts.

### M9 (Major). Soil moisture description
- One sensor profile per field (line 80) means SM is constant across sampling points and across map pixels; say so, since it limits SM to a temporal signal.
- Conversion to depth "within the 0 to 55 cm" (line 82) is inconsistent with sensors at 15, 50 and 75 cm in Hidango.
- The "weather" group in all recipes includes SM (code), but line 220 lists weather and SM as if distinct. Rename the group to "weather + SM".

### Minor (domain)
- EVI is cited to Jiang et al. 2008 (EVI2); the three band EVI is Huete et al. 2002. RF is usually cited to Breiman 2001 rather than Ho 1995.
- Table 2 labels S2 band 8A as "RE"; it is the narrow NIR band.
- Line 52: "Wheat is the most consumed crop worldwide" needs a precise metric (calories, area, trade); and the paragraph conflates yield with AGB.
- Line 66: points "randomly selected and evenly distributed" is contradictory; describe the design.
- Physiological reading of PDPs: predicted AGB falling as PS_GNDVI rises (line 322) is consistent with post anthesis senescence at high biomass; say this explicitly instead of only invoking collinearity.
- Missing literature: S1 wheat biomass (Veloso et al. 2017, RSE; Wiseman et al. 2014), spatial validation (Ploton et al. 2020, Nat. Commun.; Meyer and Pebesma 2021, area of applicability; Roberts et al. 2017), correlated predictor importance (Strobl et al. 2008 conditional importance; Apley and Zhu 2020 ALE plots).

---

## Report 4: Reviewer 3, Perspective

1. **Deployability depends on ground data the paper downplays.** Every recipe carries ∑GDD (C1), which needs a nearby station and observed phenology dates for base temperature switching. Villa Baviera's station is 20.9 km away (Table 1). A satellite temperature product (ERA5-Land) test would directly support the "two deployment pathways" narrative (line 379).
2. **The cloud substitution role of S1 is asserted, not shown.** Lines 377 and 403 propose S1 as a cloud gap substitute. With PlanetScope daily revisit and Kalman gap filling, the realistic question is how many days without optical data the pipeline tolerates. A simple experiment masking optical scenes for 2, 4 and 6 weeks around key stages would turn a speculation into a result.
3. **AGB is an intermediate variable for decision makers.** Farmers and insurers act on grain yield. If yield or harvest index data exist for these fields, even a single table linking harvest AGB to yield would sharpen the decision support framing in line 381.
4. **Stage 2 framing risks misuse.** Lines 381 translates stage 2 RMSE into triage and insurance language while the Methods correctly state it is not forecast skill. Remove the decision paragraph for stage 2, or move all of stage 2 to the Supplement. The title's "exploratory harvest forecasting" invites readers to cite it as forecasting skill.
5. **Cost framing.** PlanetScope's commercial access is mentioned (line 379), but the gain it buys (5.7 % RMSE under XGBoost, C4) should be weighed against cost explicitly.

---

## Report 5: Devil's Advocate

### Strongest counter argument
The paper's central claim is that optical sensors, particularly PlanetScope, carry transferable information about wheat AGB across sites. A simpler explanation fits the evidence equally well: the models mainly learn **where in the season a sample sits**. Every recipe includes ∑GDD (C1); cumulative VIs and ∑PP are monotone in time; 21 of 153 targets are fixed zeros at sowing; Kalman smoothing uses future scenes. A weather only GLM already reaches squared correlation 0.66, and the reported metric (C2) rewards matching the seasonal shape even when levels are off. Under this reading, the "sensor contribution" is a modest increment on a seasonal clock, and the increments are within one fold SD and change sign across algorithms (C4) and metrics (C3). The explainability evidence cannot adjudicate, because importance was computed in sample on an ensemble built from all recipes (C5) and is flat across time correlated variables. Stage 2 is circular by design. Until a time only null model is reported and the ranking survives RMSE based selection, the defensible conclusion is narrower: satellite predictors add a small, algorithm dependent improvement over a phenological clock across four site seasons.

### Issue list

| ID | Severity | Dimension | Location | Issue |
|---|---|---|---|---|
| DA-1 | CRITICAL | Construct validity | lines 15, 220, 288, 399; script 14 recipes | "PlanetScope only" model contains ∑GDD |
| DA-2 | CRITICAL | Alternative explanation | lines 288 to 296, 369 | No time only null model; seasonal clock can explain most skill |
| DA-3 | CRITICAL | Cherry picking | line 288, Fig. 5, Table 6 | Ranking uses the one metric (rsq) and one ablation algorithm (XGBoost) that support the narrative; RMSE and RF/GLM disagree |
| DA-4 | MAJOR | Overgeneralization | lines 377, 399, 403 | "S1 adds no skill" and "policy relevant accuracy" stated as general findings |
| DA-5 | MAJOR | Logic chain | lines 318, 371 | Flat in sample importance used to support conclusions about transferable signal |
| DA-6 | MINOR | So what | title, line 336 | Stage 2 yields no usable finding yet occupies the title |

### Ignored alternative paths
Report fixed parameter fusion RF as the primary model; frame the study as sensor increments over a phenology baseline; drop stage 2 into a supplement.

### Missing stakeholder perspectives
Farm advisors who would need yield, not AGB; users in regions without stations or phenology records.

### Observations (non defects)
The LOSO design, open repository and explicit stage 2 circularity statement are genuine strengths and should be kept.

---

## Editorial Decision

**Decision: Major Revision**

**DA CRITICAL adjudication (visible, as required):**
- DA-1: **Validated** against `script/14_modelado_con_ML.R`. Blocks acceptance.
- DA-2: **Unresolved**; requires the null model analysis. Blocks acceptance.
- DA-3: **Validated** against the manuscript's own numbers and `ablation_loso_summary.csv`. Blocks acceptance.

**Consensus across seats.** The LOSO framework is appropriate and the limitations are honestly stated, but four verified mismatches between text and code (C1 GDD in sensor recipes, C2 metric definition, C5 ensemble composition and in sample DALEX, M1 PlanetScope bands) change the meaning of the headline results. Methodology, Domain and Devil's Advocate converge on the same core problem: the ranking and sensor claims are within noise and metric/algorithm dependent.

**Disagreement.** Journal Fit and Perspective lean toward moving stage 2 to the Supplement; Methodology treats it as acceptable if framed as is. Synthesis: keep stage 2 only if its decision paragraph (line 381) is removed and the title no longer foregrounds it.

**Why not Reject.** The dataset is unusual (four site seasons with destructive samples and three satellite sources), the code is open, and all critical issues are fixable with analyses the pipeline already supports.

**Why not Minor.** Correcting C1 to C4 will likely change the headline model, the abstract numbers and the Sentinel-1 conclusion.

---

## Revision Roadmap

Items are grouped by severity. Order within a group is not a priority ranking; the author decides triage.

### Critical (must address)
| ID | Action | Evidence anchor |
|---|---|---|
| C1 | Rerun or relabel sensor only recipes (∑GDD retained); add ∑GDD only baseline | script 14 `step_rm` lines; manuscript lines 15, 220, 288, 399 |
| C2 | Switch to `rsq_trad` or rename metric; reselect on RMSE | script 14/20/24 `metric_set(rsq, ...)`; Supplement Eq. S3 |
| C3 | Rank on RMSE, report per fold paired differences, soften "best" claims; fix Fig. 5 comparator | lines 288, 292, 369, 399 |
| C4 | Report RF, XGBoost, GLM in Table 6; drop "independent" and "corroborates" | `output/tables/ablation_loso_summary.csv`; lines 236, 288, 296, 389 |
| C5 | Describe ensemble as coded; report its LOSO metrics; DALEX on out of fold data; commit scripts that generate `_LOSO` figures | script 14 section 11; script 14.1 `initial_split`; lines 240, 316 to 330, 411 |

### Major
| ID | Action | Evidence anchor |
|---|---|---|
| M1 | Document PS red edge availability and imputation share; test four band only rec7 | script 02 `rededge ~ 'B5'`; line 157; Table 3 |
| M2 | Describe how 89+ candidates became 43; move outcome aware screening inside CV | script 14 line 6 to 7; lines 169 to 171 |
| M3 | DAS only baseline; metrics without S_SOS zeros; note two sided smoothing | 21 zero AGB rows in `dataset.rds`; line 165 |
| M4 | Per fold results table and uncertainty on ranking | lines 288, 296 |
| M5 | Verify AGB values up to 43.5 t/ha; fix "~30 t/ha"; justify or remove "policy relevant"; fix AGB definition | `dataset.rds`; lines 76, 330, 367, 377, 381, 448 |
| M6 | Remove stage 2 decision paragraph; reconsider title; fix Table 5 top five/six | lines 2, 336 to 361, 381 |
| M7 | Report fixed parameter models as primary or implement inner LOSO | lines 236, 389; ablation CSV |
| M8 | Correct S1B statement; document orbit/incidence/speckle; S1 sensitivity check | line 115, 163, 165 |
| M9 | Clarify SM single profile, depth integration, SM inside "weather" | lines 80 to 82, 220 |

### Minor
Table 4 `tree_depth` for RF; grid description; EVI and RF citations; B8A label; Fig. 6 full model loss; line 52 wheat claim; line 66 sampling design wording; Fig. 8 legend "Hidango S1/S2"; uncited "<10 %" insurance threshold (line 381); missing spatial CV and conditional importance literature; consolidate repeated caveats; confirm target journal and abstract length.

---

*Generated by ARS academic-paper-reviewer v1.11.1 (full mode). Read only review: the manuscript was not modified.*
