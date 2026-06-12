# Peer Review Report — Round 1

**Manuscript:** *An explainable machine learning framework for estimating and forecasting wheat above-ground biomass using Sentinel-1, Sentinel-2, PlanetScope, and in-situ weather data*
**Target journal:** Agricultural Water Management (Elsevier, Q1)
**Reviewer role:** Senior Research Scientist / Lead Reviewer
**Date:** 2026-06-12
**Recommendation:** **Major Revision (borderline reject-and-resubmit in current form)**

---

## Executive Summary

This is a technically competent, well-written multi-sensor remote-sensing/ML study with a genuinely useful two-stage design (in-season AGB estimation + 1–4 month harvest forecasting) and an honest grasp of its own data limitations. However, in its **current** form it is **not ready** for Agricultural Water Management, for three reasons that together would likely trigger a major-revision-bordering-reject decision. **First**, the headline performance (Stage 1: R²=0.91, RMSE=3.18 t/ha) rests entirely on a **single random 75/25 split of data in which multiple samples from the same four field-seasons appear in both train and test** — a textbook spatial/temporal leakage setup that almost any Q1 reviewer will flag, and which is **demonstrably optimistic**: the author has *already run* Leave-One-Site-Out (LOSO) validation whose results (best model R²≈0.73, RMSE≈7.1 t/ha — roughly double the reported error) **are not in the manuscript at all**. **Second**, a substantial body of completed, publication-quality analysis (LOSO, sensor ablation, VIF/multicollinearity audit, GDD×PS_EVI interaction, 95% CI bands) exists on disk but is entirely absent from the narrative — the manuscript under-reports its own strongest evidence. **Third**, the framing is mis-targeted for this journal: it reads as a remote-sensing/ML model-comparison paper, whereas Agricultural Water Management needs the soil-moisture / water-stress / irrigation-decision thread promoted from a side observation to the central contribution.

**Highest-priority fixes:** (1) make LOSO the headline validation and reconcile/retire the leaky 75/25 split; (2) pull the completed ablation, VIF, interaction, and CI analyses into the Results; (3) reframe abstract/intro/discussion around agricultural-water-management decision support.

### Pillar scores (1–10)

| Pillar | Score | One-line justification |
|---|---|---|
| Novelty | 6 | Useful S1+S2+PS+weather+SM fusion for AGB *and* multi-lead forecasting with XAI; but each ingredient is individually well-trodden. |
| Technical soundness | 4 | Leaky single split as the headline result; completed rigorous validation (LOSO/CI) omitted; several internal numeric contradictions. |
| Reproducibility | 7 | Public repo, named packages, clear pipeline; but no seeds/versions stated in-text, n=4 limits external reproduction, supplement (Table S1, Eqs S1–S3) not provided here. |
| Impact / AGWAT fit | 5 | Real practical relevance, but mis-framed for the journal and generalization claims oversold at n=4 sites. |
| **Overall readiness** | **5** | Strong bones, but the central validation is not defensible as written and the best evidence is missing. |

A **Go** requires ≥7.5 average with no pillar <6. This manuscript fails on technical soundness (4) and impact-fit (5); the gating issue is the validation design, not the writing.

---

## 1. Fit and positioning for Agricultural Water Management

The current framing is weighted toward (a) ML algorithm comparison ("40 models", model ranking) and (b) SAR remote-sensing novelty. That is the framing of the prior GRSL letter, not of Agricultural Water Management (AGWAT). AGWAT's editors desk-reject papers that lack **direct, practical linkages to agricultural water management**; a generic "precision agriculture / risk management" closing sentence is not enough.

The good news: the **water thread already runs through your own results** and is currently buried. Your own DALEX importance shows **soil moisture (SM) and ΣPP among the top predictors for estimation, and SM as the #1 predictor in 3 of 4 forecasting lead times** (Table 5). Sentinel-1 backscatter is explicitly motivated as a **soil-moisture proxy**. That *is* an agricultural-water story — but it is presented as an incidental finding about variable importance rather than as the contribution.

**Reframing required (must-fix):**
- Recast the abstract and Introduction around **water availability as the dominant driver of forecastable wheat biomass**, with multi-sensor ML as the *method*, not the headline.
- Add an explicit **water-management decision-support paragraph** in the Discussion: what irrigation/insurance/allocation decision does a 1–4 month AGB forecast actually inform, at what spatial unit, with what error tolerance? Quantify it (e.g., "an RMSE of X t/ha at a 2-month lead is/ is not adequate to trigger Y decision").
- Connect to the journal's literature: you already cite @PeroniVenancio2020 (AGWAT) and @Wang2023 (AGWAT). Lean into the soil-water-plant-atmosphere continuum framing those papers use.
- Reconsider the title: "explainable machine learning framework … using Sentinel-1/2/PlanetScope" foregrounds sensors. A title foregrounding **forecasting wheat biomass from soil moisture and radar under cloud** would fit AGWAT better.

---

## 2. Scientific rigor and validity of claims

### 2.1 The validation design is the central problem (CRITICAL)

The Stage-1 headline (R²=0.91, RMSE=3.18 t/ha, RF+rec3) comes from a **single random 75/25 split** (Methods §"Defining dataset recipes"). With only **four field-seasons** and 153 AGB samples, a random split places samples from *the same field, same season, often adjacent dates* in both train and test. Because covariates are daily-interpolated time series, two samples 10 days apart in the same field are near-duplicates. This is **spatial and temporal leakage**, and the reported metrics measure interpolation skill within seen fields, **not** the generalization the Discussion claims ("excellent potential generalization capacity").

This is not a hypothetical concern — **you have already measured the gap.** Your completed LOSO analysis (`output/tables/ablation_loso_summary.csv`, `output/figs/loso_ablation_metrics.png`) shows that under spatially-independent validation:

| Recipe (LOSO) | Best model | Mean R² | Mean RMSE (t/ha) |
|---|---|---|---|
| Fusion (all) | RF | **0.73** | **7.10** |
| S2+S1+Climate | RF | 0.66 | 7.81 |
| S2+Climate | XGBoost | 0.75 | 7.50 |
| Climate only | GLM | 0.66 | 8.17 |

The LOSO RMSE (~7 t/ha) is **more than double** the manuscript's 3.18 t/ha, and the best LOSO R² (~0.73) is far below 0.91. **A reviewer who finds this discrepancy — and the data are in your public repo — will lose trust in the paper.** You must pre-empt it.

**Required:** Promote LOSO to the **primary** reported validation. Keep the 75/25 split only as a secondary "within-site interpolation" benchmark, explicitly labeled as such, and explicitly state the leakage caveat. Report LOSO R²/RMSE/MAE with their across-fold SD (already in the CSV) in the abstract and Results. The honest, lower numbers are *still publishable and still useful* — and the gap between within-site and cross-site performance is itself a scientifically interesting, AGWAT-relevant result about transferability.

### 2.2 "41 models" vs "40 models" (MAJOR — internal contradiction)

Abstract says **"41 models"**; Methods, Results, and Conclusion say **40 models** (8 recipes × 5 algorithms). The "41st" appears to be the ensemble. Pick one convention and apply it everywhere (recommended: "40 base models plus a stacked ensemble").

### 2.3 Recipe definitions are internally contradictory (MAJOR)

The Methods define the recipes one way and the CLAUDE.md/results another:
- Methods §2.3.2: "*rec3* uses **Sentinel-2 and weather** … *rec6* uses **Sentinel-1** … *rec7* uses Sentinel-1 and weather."
- Results §3.1 and the entire Discussion/Conclusion: "*rec3*, **which uses S1 and weather**, ranks first" and "RF with *rec3* (S1+W)".
- Figure 5 / Figure 9 captions: "recipe 3 (Sentinel-1+weather)".

So rec3 is **defined as S2+weather in Methods but used as S1+weather everywhere else.** This is a showstopper-level inconsistency: a reader cannot tell which sensor combination produced your best model. Reconcile the recipe table against the actual code (`script/14_modelado_con_ML.R`) and fix every downstream reference. (Note: the same Methods sentence also says "*rec6* … S1" then "*rec7* … S1 and weather", which double-assigns S1 — the whole recipe enumeration needs re-checking against code.)

### 2.4 "GLMnet did poorly with rec3" contradicts "rec3 is best" (MAJOR)

Results §3.1: "The algorithm that had the poor performance was the GLMnet with *rec4*, using only weather, **and with *rec3***." But rec3 is simultaneously named the *best* recipe. Either GLMnet is uniquely bad on rec3 while tree models excel (plausible, but then say so explicitly), or this is a leftover error from the recipe-definition confusion in §2.3. Clarify.

### 2.5 Stage-2 sample construction risks circularity and leakage (MAJOR)

Stage-2 targets are "the AGB of the harvest date per site" with **100 random samples per site** (Methods §2.3.5). But the harvest-date AGB is itself a **model output from Stage 1** (the daily spatial estimation), not an independent observation. Forecasting a *modeled* target with covariates that fed the same model risks inflating R². Furthermore, with only 4 sites, "100 random samples per site" largely resample *within-field pixel variation around 4 harvest values* — the effective sample size for the quantity of interest (harvest biomass) is **4, not 400**. The Discussion acknowledges "low variability of AGB at harvest … only four sites," but the R²=0.94 headline does not transparently reflect that the model is mostly learning within-field spatial texture, not interannual/inter-site harvest variation. State explicitly what the Stage-2 target is, whether it is observed or modeled, and report a LOSO-style site-held-out forecasting metric.

### 2.6 No effect sizes / uncertainty on headline metrics (MAJOR)

Single-split point estimates (R²=0.91) are reported with no confidence interval. The CV folds and the LOSO across-fold SD give you this for free. You also already computed **95% CI bands from inter-model spread** (`output/tables/ci_coverage_by_site.csv`) — and they reveal that **coverage at La Cancha is only 51%** (nominal 95%) with a **−5.3 t/ha mean bias**, vs 70–80% at the other sites. That is an important honest result that belongs in the paper.

### 2.7 Soil moisture as a predictor — units and leakage (MINOR–MAJOR)

SM is repeatedly the top predictor, but SM is measured by a **single sensor profile at one central point per field** (Methods §2.2.1). Using a single point value as a spatial predictor across the whole field means SM contributes a per-field constant (or near-constant) that the model can exploit to *identify the field-season* — another subtle leakage/site-fingerprint pathway under random splitting. Under LOSO this advantage vanishes (consistent with the much lower LOSO scores). Discuss this explicitly.

---

## 3. Methodology completeness and clarity

Strengths: the data-acquisition, VI catalogue (Table 3, 53→89 VIs), preprocessing, GDD base-temperature switching, and DALEX permutation-importance descriptions are clear and largely reproducible. Specific gaps:

- **Multicollinearity handling is under-documented (MAJOR).** Methods says only "we applied a filter to eliminate highly correlated variables." With 89 cumulative VIs (which are, by construction, monotone and massively collinear), this is the single biggest methodological worry after leakage. You have **already done a full VIF audit** (`output/tables/vif_post_step_corr.csv`, `vif_iterativo_removidas.csv`, `corr_removal_summary.csv`, fig `vif_post_step_corr.png`). Pull it in: state the correlation threshold (`step_corr` cutoff), how many variables were removed, and the post-removal VIF. This directly addresses the "cumulative VIs cause redundancy" limitation you flag yourself.
- **Hyperparameter optimization is vague (MAJOR).** "Ten candidates per parameter" — was this a regular grid, Latin hypercube, or random search? Over how many total candidates per model? Which metric selected the winner? The trees parameter is "fixed to 1000"; was early stopping used for XGBoost? Provide the search strategy and the final selected hyperparameters per winning model (a supplementary table).
- **No random seed reported in-text (MAJOR for reproducibility).** A single 75/25 split is highly seed-sensitive at n=153. State the seed and, better, report split-robustness (e.g., repeated splits) — though LOSO largely supersedes this.
- **Ensemble construction underspecified (MINOR).** "combines the predictions for the five models" — is this the `stacks` stacked ensemble (meta-learner) or a simple average? The repo uses `stacks`; say so and cite the stacking weights or meta-model.
- **Daily interpolation method asymmetry (MINOR).** S1 is "duplicated to the midpoint" (nearest-neighbor hold) while S2/PS use Kalman smoothing. This inconsistency could bias the relative importance of S1 vs optical predictors; justify or harmonize.
- **GDD×PS_EVI interaction (MAJOR for completeness).** You computed a conditional-PDP interaction analysis (`output/figs/interaccion_gdd_ps_evi.png`). The current PDP section (Fig 7) only shows main effects. The interaction is a more sophisticated, more interesting result and should be added.

---

## 4. Results presentation

The figure set is generally good (study area, phenology/NDVI, flowchart, model ranking, obs-vs-pred, VIP, PDP, time series, monthly maps, lead-time metrics). But the **most credibility-enhancing results are missing**, all of which already exist as finished figures/tables on disk:

| Completed analysis | Files on disk | Where it should go |
|---|---|---|
| LOSO + ablation (R²/RMSE/MAE by sensor set, ±SD) | `loso_ablation_metrics.png`, `loso_metricas_por_sitio.png`, `ablation_loso_summary.csv`, `sensor_marginal_gains.csv` | **New headline Results subsection + abstract** |
| La Cancha diagnostic (mid-season underestimation) | `la_cancha_vs_otros_rmse.png`, `la_cancha_obs_vs_pred.png`, `la_cancha_diagnostico.csv` | Results + Discussion (you already mention the underestimation in Fig 8) |
| VIF / multicollinearity audit | `vif_post_step_corr.png`, `vif_*.csv` | Methods + Supplement |
| GDD×PS_EVI interaction | `interaccion_gdd_ps_evi.png` | Replace/augment PDP section |
| 95% CI bands vs DAS, coverage by site | `ci_predicciones_DAS.png`, `ci_anchura_vs_DAS.png`, `ci_*.csv` | Results (forecasting) |

The **sensor marginal-gains table** (`sensor_marginal_gains.csv`) is especially valuable and currently absent: it shows S2-over-climate gives **+23% RMSE improvement**, but **adding S1 to S2+climate gives −0.5% (no improvement)** and PS adds +5.7%. This **directly contradicts the manuscript's central claim that S1+weather (rec3) is the best recipe**. Under LOSO, the marginal value of S1 over an optical+climate baseline is essentially zero. This is a major finding the paper must confront, not omit — it may require softening the "S1 is the key sensor" narrative to "S1 substitutes for optical under cloud, but adds little when optical is available."

**Table 5 issues (MAJOR):**
- The caption says it is for "the XGBoost prediction model … and rec1 (all predictors)", but the text (Methods §2.3.6) says importance was computed for "the **best model for each lead time**." If the best model differs by lead time, a single XGBoost/rec1 attribution is inconsistent. Clarify.
- The "Mode rank" / "N" / "Scaled dropout loss" columns are hard to parse. SM and ΣPP both have "Mode rank = 1" but N=3 and N=1 respectively — explain how two variables can both be modal-rank-1. The negative scaled-dropout-loss values for S1/PS variables (−0.40, −0.46, −0.36, −0.57) suggest these variables, when permuted, *reduce* error on average — i.e., they may be noise. If a top-6 "important" variable has negative permutation importance, that undercuts calling it important. Re-examine and explain.

**Figure-prose concordance:** confirm Fig 4 caption ("forty models") matches whatever model count you settle on.

---

## 5. Discussion and limitations

The limitations section is comparatively honest — it flags (a) reliance on in-situ weather/SM, (b) only 3 locations / 4 site-seasons / mixed winter+spring varieties, (c) speckle and incidence-angle effects, (d) cumulative-VI redundancy. That candor is a genuine strength and is appreciated.

However, the Discussion **oversells generalization** in ways that will not survive Q1 scrutiny *given your own LOSO results*:
- "indicating excellent potential generalization capacity" — directly contradicted by LOSO (R² drops 0.91→0.73, RMSE doubles). Must be rewritten.
- "the model is likely to have adequate performance in regions that face cloudy weather" — but your ablation shows S1 adds ~0% over optical+climate; the cloud-resilience argument needs to rest on S1's ability to *substitute* when optical is missing, evidenced by the climate-only and S1-only recipes, not on S1 being the best predictor.
- The La Cancha mid-season underestimation is mentioned descriptively (Fig 8) but not diagnosed, despite your completed diagnostic showing La Cancha is the worst site (LOSO RMSE 10.6, CI coverage 51%, bias −5.3 t/ha). Bring the diagnostic in and offer a mechanism (e.g., Regosol shallow soils, different variety, station distance).

Three-locations-vs-four-site-seasons should be stated consistently (the text says both "three locations" and "four sites" in different places — clarify that it is 3 locations / 4 field-seasons).

---

## 6. Writing quality, structure, consistency

The prose is clear and the structure is sound. Concrete defects to fix:

- **"41 models" (abstract) vs "40 models" (body)** — §2.2 above.
- **4-month lead R²: 0.86 (abstract & Results) vs 0.84 (Conclusion).** "decreases to 0.84 at a four-month lead time" (Conclusion) contradicts "R²: 0.86–0.94" (abstract) and "0.86 at four months" (Results §3.5). Pick the correct value from `metrics_prediction_lead_times` output and sync all three.
- **Hidango / Hidalgo spelling.** Fig 1 caption and Fig 9(b) caption say "**Hidalgo**"; the body and site naming say "**Hidango**." Global find-replace to Hidango.
- **Author name: Molina-Roco vs Molina-Rocco.** Author block (line 8) = "Molina-**Roco**"; CRediT (line 324) = "Molina-**Rocco**". Fix to the correct spelling (one c or two) consistently — this is the kind of error that embarrasses at proof stage.
- **Ensemble citations:** `@Breiman1996a` (Bagging) and `@Breiman1996b` are **byte-identical duplicate entries** in references.bib (both "Bagging predictors", same DOI). `@Breiman1996c` (Stacked regressions) exists but is **never cited**, while the ensemble is cited to Breiman1996b+Wolpert1992. You likely meant to cite Breiman1996c (stacked regressions) for the ensemble. Remove the duplicate and fix the citation.
- **Uncited bib entries:** `@Liu2025b` (winter wheat × GDD) and `@Wang2023b` (S1/S2 soil-moisture retrieval) appear in references.bib but I do not find `@Liu2025b`/`@Wang2023b` cited in the text (note `@Liu2025` and `@Wang2023` *are* cited and are different entries — easy to confuse). Verify; the GDD discussion ("as expected and has been shown by [@Liu2025]") may have intended `@Liu2025b`.
- **Supplement referenced but not provided:** Table S1 (full VI list) and Eqs S1–S3 (MAE/RMSE/R²) are cited but no supplementary file is present in the review package. Required for submission.
- **Glossary** is useful and appropriate for AGWAT's broad readership — keep it, but add SM, ΣGDD, ΣPP, OOF, LOSO (once added), and VIF.
- **Declarations:** CRediT, competing-interest, funding, and gen-AI statements are all present — good. Funding text is duplicated across Acknowledgements and Funding Sources with slightly different wording; harmonize.

---

## 7. Novelty and contribution

**Genuinely novel / valuable:**
- The **combination** of S1 + S2 + PlanetScope + in-situ weather + in-situ soil moisture for wheat AGB in a **two-stage estimation-then-forecasting** pipeline with **multi-lead (1–4 month) harvest forecasting** and **model-agnostic XAI** is a reasonably fresh integration, especially for Mediterranean Chilean wheat systems that are under-represented in the literature.
- The **transferable, honest result** — that under spatially-independent validation, soil-moisture and climate dominate, optical adds the most marginal skill, and SAR mainly buys cloud-resilience rather than accuracy — would be a *more* novel and more defensible contribution than "we got R²=0.91."

**Incremental / over-claimed:**
- Each ingredient (S1+S2 fusion, cumulative VIs, RF/XGBoost for wheat AGB, DALEX importance) is individually well-established (you cite the relevant prior work: @Uribeetxebarria2023, @Li2024, @David2022, @AtkinsonAmorim2022, @Liu2024).
- The "41 models" framing reads as a brute-force sweep rather than a hypothesis-driven design. Reframe as a **structured sensor-ablation** (which you in fact ran) to test *which water-relevant information source matters* — that is a scientific question, not a leaderboard.

**Sharpened contribution statement (suggested):** "Using spatially-independent (leave-one-site-out) validation across four Mediterranean wheat field-seasons, we quantify how much forecastable wheat above-ground biomass is explained by soil-water information (in-situ soil moisture, accumulated precipitation, SAR-derived moisture proxies) versus optical canopy signals, and demonstrate cloud-resilient harvest forecasting at 1–4 month leads with explainable ML."

---

## 8. Hero figure suggestions

**Hero Figure 1 — "The transferability gap" (most important; new, but built from existing files).**
A two-panel figure that is the scientific spine of the revised paper:
- **Panel A:** obs-vs-predicted scatter under the **within-site 75/25 split** (R²≈0.91) beside obs-vs-predicted under **LOSO** (R²≈0.73), same axes, 1:1 line, colored by site. The visual collapse of the point cloud from tight to scattered tells the whole generalization story at a glance.
- **Panel B:** the ablation forest/dot plot you already have (`loso_ablation_metrics.png`) showing R²/RMSE by sensor set with across-fold error bars — annotated with the marginal-gain numbers (S2 over climate +23%, S1 over S2+climate ≈0%).
- Design: colorblind-safe site palette (cols4all `color_blind`), bold 1:1 reference, error bars shown, the −0% S1 gain annotated directly. Story: *honest cross-site skill, and which water-information source actually carries it.*

**Hero Figure 2 — "Forecasting water-stressed biomass ahead of harvest" (AGWAT-facing).**
Combine the lead-time skill decay with the soil-moisture importance and the CI bands:
- Left: R²/RMSE vs lead time (1–4 months) with **95% CI band width** overlaid (`ci_anchura_vs_DAS.png` content) — showing both skill and honest uncertainty growing with lead.
- Right: SM/ΣPP/S1 importance ranking across leads (cleaned-up Table 5 as a tile/heatmap), foregrounding soil water as the dominant forecasting signal.
- Story for an AGWAT reader: *months before harvest, soil-water state lets you forecast wheat biomass with quantified, widening uncertainty — actionable for irrigation/insurance decisions.*

---

## Prioritized Revision Roadmap

### MUST-FIX before submission (showstoppers)
1. **Replace the leaky 75/25 split as the headline with LOSO.** Report LOSO R²/RMSE/MAE ± across-fold SD in abstract and Results; keep the split only as an explicitly-labeled within-site interpolation benchmark with a stated leakage caveat. (Effort: Medium — analysis is done; this is rewriting + re-tabulating from `ablation_loso_summary.csv`.)
2. **Pull in the completed ablation + sensor marginal-gains results** and reconcile the narrative: state honestly that S1 adds ~0% over optical+climate under LOSO and reframe S1 as cloud-substitution, not best-predictor. (Effort: Medium.)
3. **Fix the rec3 definition contradiction** (S2+weather in Methods vs S1+weather everywhere else) against the actual code; re-audit the entire recipe enumeration. (Effort: Low but critical.)
4. **Reconcile all numeric inconsistencies:** 41 vs 40 models; 0.84 vs 0.86 four-month R²; "GLMnet poor with rec3" vs "rec3 best." (Effort: Low.)
5. **Reframe abstract/intro/discussion for AGWAT** around soil-water/water-stress/decision-support; add a quantified management-actionability paragraph. (Effort: Medium.)
6. **Document multicollinearity handling** using the completed VIF audit (threshold, # removed, post-VIF). (Effort: Low.)
7. **Provide the supplement** (Table S1, Eqs S1–S3, hyperparameter search + final values). (Effort: Low–Medium.)
8. **Fix author-name and place-name spellings** (Molina-Roco/Rocco; Hidango/Hidalgo) and the **duplicate/wrong ensemble citation** (Breiman1996a≡1996b duplicate; cite 1996c for stacking). (Effort: Low.)

### WOULD STRENGTHEN (high value, not strictly blocking)
9. **Add the La Cancha diagnostic** (worst site, 51% CI coverage, −5.3 t/ha bias) with a mechanistic explanation. (Effort: Low — figures exist.)
10. **Add the 95% CI bands** to the forecasting results and report per-site coverage honestly. (Effort: Low.)
11. **Add the GDD×PS_EVI interaction** to the explainability section. (Effort: Low.)
12. **Clarify Stage-2 target** (modeled vs observed harvest AGB) and effective sample size; add a site-held-out forecasting metric. (Effort: Medium.)
13. **Specify the stacked-ensemble construction** and report seed / split-robustness. (Effort: Low.)

### OPTIONAL / FUTURE WORK
14. Test satellite-derived SM/weather (CHIRPS, ERA5, S1-retrieved SM) as a step toward the generalization the Discussion promises. (Effort: High.)
15. More aggressive feature selection on cumulative VIs (e.g., Boruta / recursive elimination) to address redundancy. (Effort: Medium.)
16. Harmonize S1 vs optical interpolation methods and test sensitivity of relative importance to that choice. (Effort: Medium.)

---

### Bottom line
The science underneath this paper is sound and the author has *already done the hard, rigorous work* (LOSO, ablation, VIF, CI, interaction) that the manuscript needs — it simply is not in the draft. The revision is therefore mostly **disciplined re-reporting and reframing**, not new analysis: surface the honest cross-site validation, let the lower-but-defensible numbers carry an AGWAT-relevant water-information story, and fix the internal contradictions. Done well, this becomes a credible Q1 submission. Submitted as-is, it would draw a major-revision-or-reject on the validation design alone.
