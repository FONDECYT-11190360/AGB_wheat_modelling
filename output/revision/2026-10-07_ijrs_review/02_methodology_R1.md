# Peer Review Report

## Manuscript Information
- **Title**: Thermal time baselines for evaluating Sentinel-1, Sentinel-2 and PlanetScope predictors of wheat above-ground biomass under spatial cross-validation
- **Manuscript ID**: not assigned (author-stated target: International Journal of Remote Sensing, research article)
- **Review Date**: 2026-10-07
- **Review Round**: Round 1 review (simulated panel, seat R1)

criteria_binding_unavailable

No venue criteria manifest was supplied. Judgements below use general methodological standards for grouped cross-validation studies and make no claim about IJRS-specific criteria.

---

## Reviewer Information

### Reviewer Role
Peer Reviewer 1 (Methodology)

### Reviewer Identity
Statistician working on spatial cross-validation and model transferability for environmental machine learning (area of applicability, grouped CV, nested resampling). Blind spot declared: SAR physics and wheat agronomy; I do not judge whether the backscatter mechanism or the base temperatures are agronomically right, only whether the analysis supports the claims made with them.

### Review Focus
Whether the LOSSO-CV design with four folds can support the ranking and the "added value" statements; whether any preprocessing step lets information from the held-out site-season, or from the response, reach the models; and whether the comparisons between the linear thermal-time baseline and tree models are fair. I gave particular attention to whether the headline result (RF with S1 and ∑GDD best) is distinguishable from noise.

Verification performed: I read `script/31_loso_diseno_limpio.R`, `script/35_gdd_rf_xgb.R`, `script/07_procesar_datos.R`, and the output tables `rev2_loso_limpio_por_fold.csv`, `rev2_loso_limpio_resumen.csv`, `rev2_vip_heldout_rf_all.csv`, `rev2_s1gdd_vs_gdd_mismo_algoritmo.csv`. I ran read-only R on `data/processed/rds/dataset.rds` and on the cached out-of-fold predictions (`rev2_loso_limpio_resultados.rds`); nothing was written to the project. Numbers marked "reviewer computation" below come from those checks.

---

## Overall Assessment

### Recommendation
- [ ] **Accept**
- [ ] **Minor Revision**
- [x] **Major Revision**
- [ ] **Reject**

### Confidence Score
4 (core expertise for the validation and inference questions; adjacent for the SAR and agronomic interpretation).

### Summary Assessment
The manuscript evaluates Sentinel-1, Sentinel-2, PlanetScope, weather and soil moisture predictors of wheat AGB against a linear ∑GDD baseline, using leave-one-site-season-out cross-validation over four field seasons, 29 model configurations with fixed hyperparameters, and held-out permutation importance. The central message, that a phenological baseline evaluated under grouped CV should accompany satellite AGB models, is methodologically valuable, and the design is much cleaner than most papers in this area: preprocessing is fitted inside folds, hyperparameters are not selected on the evaluation folds, per-fold results are published, and a zero-AGB sensitivity analysis is reported. The weaknesses are in inference and in the controls. The decision rule "differences smaller than the between-fold SD are indistinguishable" uses the wrong variance for paired comparisons and is not a test. The "same algorithm" evidence for a genuine Sentinel-1 contribution compares against a degenerate step-function reference and lacks a noise control, even though the paper itself names tie-breaking as an alternative explanation. The ∑GDD baseline uses the observed tillering date of each field, including the held-out one, which conflicts with the operational claim and with the stated base temperature rule. The within-fold correlation filter removes ∑GDD from the All model in two folds, which biases both the All comparison and the importance analysis. The 21 zero-AGB rows are assigned at sowing, not measured, yet are counted as destructive samples and remain in training. These are repairable with reanalysis on existing data, so I recommend Major Revision.

---

## Strengths

### S1: Phenological baseline evaluated under the same grouped CV as the satellite models
The paper fits DAS and ∑GDD baselines under the identical LOSSO-CV folds and reports every satellite configuration relative to the ∑GDD baseline per fold (ΔRMSE and fold counts). This is the correct reference for attributing skill to remote sensing and is rarely done.
**Evidence Anchor**: `table: Table 5, columns ΔRMSE (%) and Folds, relative to "GDD LM" row`

### S2: All supervised preprocessing fitted within training folds; hyperparameters fixed a priori
KNN imputation, centring and scaling, the r > 0.9 correlation filter and the zero-variance filter are recipe steps inside `fit_resamples`, so their parameters are estimated on the three training site-seasons only (verified in `script/31_loso_diseno_limpio.R` lines 195 to 211). Fixed hyperparameters remove the optimistic bias of tuning on the reporting folds.
**Evidence Anchor**: `text: §2.3.4 "Using fixed hyperparameters means that the held-out site-season is never used to select a model configuration"`

### S3: Per-fold reporting and R² against the held-out mean
Metrics are computed per held-out site-season, R² is 1 − SSE/SST against the fold mean (so it can be negative), and all per-fold values are in Table S2. This lets a reader recompute paired comparisons, which I did.
**Evidence Anchor**: `table: Table S2, RMSE (R²) per held-out site-season`

### S4: Held-out permutation importance with grouped permutation
Importance is computed on the held-out site-season, not on training data, and group permutation is used to mitigate correlated predictors. Reporting the range across folds (Figure 7) shows the instability honestly.
**Evidence Anchor**: `figure: Figure 7, panels (a) and (b), bars = range across folds`

### S5: Same-algorithm ∑GDD controls were added
Fitting ∑GDD alone with RF and XGBoost is the right instinct for separating algorithm from sensor effects, and the paper correctly diagnoses the step-function behaviour of trees on 21 to 23 distinct ∑GDD values. My concern (W2) is that the control is incomplete, not that it is misguided.
**Evidence Anchor**: `table: Table 5, rows "GDD RF" and "GDD XGBoost"`

---

## Weaknesses

### W1: The "within between-fold SD means indistinguishable" rule uses the wrong variance and is not an inferential procedure
**Problem**: §2.3.4 treats differences smaller than the between-fold SD of RMSE as indistinguishable, and the abstract, ablation paragraph and Discussion rely on this. The between-fold SD of a single model's RMSE (1 to 4 t ha⁻¹) mostly reflects how hard each fold is (Villa Baviera is easy for every model, La Cancha hard for every model). Because all configurations are evaluated on the same four folds, the relevant variance for a comparison is that of the paired per-fold differences, which is much smaller. For S1+GDD RF versus the ∑GDD LM baseline the per-fold RMSE differences are −0.82, −0.12, −0.83 and +0.03 t ha⁻¹ (mean −0.44, SD 0.45; reviewer computation from Table S2). A paired t-test gives t = −1.92, df = 3, p = 0.15, and a cluster bootstrap that resamples sampling dates within each held-out fold gives a 95% interval for the mean ΔRMSE of −1.33 to +0.57 t ha⁻¹ (P(Δ ≥ 0) ≈ 0.21; reviewer computation, 2000 replicates). The conclusion "not distinguishable" survives, but for the right reason, and the same paired logic applied to the same-algorithm S1 comparison (W2) gives the opposite verdict (RF differences −3.80, −3.01, −0.38, −2.39; paired t = −3.26, p ≈ 0.047). The rule as stated would also declare large, perfectly consistent paired gains "indistinguishable". In addition, the fold count is uninformative as evidence: with four folds a sign test cannot reach p < 0.05 even at 4 of 4 (two-sided p = 0.125), and 3 of 4 gives p = 0.625.
**Evidence Anchor**: `text: §2.3.4 "treat differences smaller than the between-fold SD as indistinguishable"`
**Why it matters**: This rule is the paper's only inferential device, and it carries the abstract sentence "differences among the leading configurations were smaller than the variability among folds" and the ablation conclusion in §3.2. It is too lenient in some directions (it never tests anything) and too strict in others (it ignores pairing).
**Suggestion**: Replace the rule with paired analyses against the baseline: report per-fold paired differences in RMSE (absolute, not only relative), their mean and SD, and an uncertainty interval from a hierarchical bootstrap (resample sampling dates within held-out folds; optionally also resample folds, acknowledging that four is too few for the outer level). Alternatively fit a mixed model of per-sample squared error with configuration as fixed effect and fold and date as random effects. State the minimum detectable paired difference given the observed paired SD (roughly 0.9 to 1.0 t ha⁻¹ for 80% power at α = 0.05 with four folds, reviewer approximation) so readers know which differences the design could ever resolve. Drop "Folds" as evidence or report it with its sign-test p-value.
**Severity**: Major
**Confidence**: 5 (core expertise: paired CV comparisons)

### W2: The same-algorithm evidence for a "genuine Sentinel-1 contribution" lacks a noise control and uses a degenerate reference
**Problem**: The 30% (RF) and 23% (XGBoost) gains from adding S1 to ∑GDD are measured against tree models on a single predictor with only 21 to 23 distinct values per training set and no extrapolation beyond 1030 °C d, which the paper itself shows are poor (RMSE 8.19 and 8.28). Any additional predictor that varies among points sampled on the same date would let the trees average across sampling dates and smooth the step function. S1 is exactly such a predictor: in the data every sample has a unique S1_VH value (40 of 40 in Hidango North, 48 of 48 in Hidango South, etc.), while ∑GDD, ∑PP and SM are constant within a date (reviewer computation). The within-date association between S1 and AGB is weak (date-demeaned pooled r = 0.16 for VH, p = 0.05; r = −0.07 for VV, p = 0.38; reviewer computation), consistent with largely speckle-driven within-date variation from single-pixel, unfiltered, mixed-orbit data (§2.2.4). With mtry = min(7, p) = 4 the S1+GDD forest considers all four predictors at every split, so S1 splits also act as a regulariser. The Discussion acknowledges that "the present data cannot separate this effect from information about the canopy", but this can be separated with the existing data.
**Evidence Anchor**: `text: §4.2 "Comparisons under the same algorithm support a genuine Sentinel-1 contribution"`
**Why it matters**: The abstract and Conclusion present the 30% and 23% reductions as the main evidence that S1 adds information. If a random or permuted predictor yields a similar reduction, the S1 result is an algorithmic artefact and the paper's only positive satellite finding disappears.
**Suggestion**: Add negative controls under identical folds, recipe and hyperparameters: (a) ∑GDD plus three Gaussian noise predictors (several seeds); (b) ∑GDD plus S1 permuted within each sampling date (keeps the marginal and within-date structure, destroys the link to AGB); (c) ∑GDD plus S1 permuted across site-seasons. Report the S1+GDD gain relative to these controls with the paired framework of W1. Also add an LM on S1+GDD (and a monotone spline or GAM on ∑GDD with S1 terms), since the linear form is what made ∑GDD transfer well; this is the missing cell of the algorithm by predictor design. Until these controls are done, remove "genuine" and present the same-algorithm comparison as not separating sensor from regularisation effects.
**Severity**: Major
**Confidence**: 4 (core expertise: ML control design; adjacent: SAR speckle characteristics)

### W3: Headline ranking is the best of 29 configurations, without accounting for selection
**Problem**: S1+GDD RF is identified as "the best-performing configuration in this sample" after evaluating 29 configurations (1 DAS + 3 GDD + 3 W + 22 satellite) on the same four folds, and its 5.9% advantage is the most favourable of 28 comparisons with the baseline. The expected minimum of 28 noisy RMSE estimates is optimistically biased (winner's curse), and the runner up S1+W RF has mean ΔRMSE of +4.2% with only 1 of 4 folds better, showing how sensitive the ranking is to the summary chosen (mean RMSE versus mean relative difference).
**Evidence Anchor**: `table: Table 5, rows "S1+GDD RF" (ΔRMSE −5.9, Folds 3) and "S1+W RF" (RMSE 6.05, ΔRMSE +4.2, Folds 1)`
**Why it matters**: The abstract leads with the ranking; readers will cite "RF with S1 and ∑GDD gave the lowest error" as a finding when it is a post hoc selection.
**Suggestion**: Label the ranking explicitly as exploratory and selection-biased. Either quantify the optimism (for example, a nested procedure in which the configuration is chosen on three folds by inner LOSSO and evaluated on the fourth, giving an honest estimate of "pick the best configuration" performance) or report a simultaneous interval/bootstrap probability that each configuration is the best (rank stability across hierarchical bootstrap replicates). Report both mean RMSE and paired absolute differences so the ranking does not depend on the relative-difference averaging.
**Severity**: Major
**Confidence**: 5 (core expertise: model selection under resampling)

### W4: The ∑GDD baseline uses the observed tillering date of each field, including the held-out one, and the code does not match the stated rule
**Problem**: §2.2.2 states a base temperature of 2 °C from sowing to tillering and 6 °C "from stem elongation to ripening". In `script/07_procesar_datos.R` (lines 59 to 75) the base switches from 2 to 6 °C at the observed TRILLERING date taken from the field phenology records, and 6 °C applies from tillering onwards (the interval between tillering and stem elongation is not treated separately). Hence ∑GDD at a held-out field encodes a field-observed phenological date. The Implications section then claims the baseline "requires only daily air temperature and the sowing date".
**Evidence Anchor**: `text: §4.5 "it requires only daily air temperature and the sowing date"`
**Why it matters**: The baseline is the yardstick for every comparison, and part of its advantage over DAS (RMSE 6.24 vs 7.85) may come from phenology information obtained in the held-out field. This is not response leakage, but it is information that would not be available at a new field without visits, so the operational claim and the fairness of "thermal time versus satellite" both depend on it. It also gives ∑GDD a field-specific kink that satellite predictors must compete with.
**Suggestion**: Correct the text to match the code (or vice versa). Rerun the baselines and the S1+GDD, PS+GDD, S2+GDD and All sets with ∑GDD computed from a single base temperature, or with the base temperature switch triggered by a fixed thermal time threshold estimated on training folds only. Report whether the baseline ranking and the "thermal time dominates" conclusion change.
**Severity**: Major
**Confidence**: 4 (verified in code; agronomic choice of base temperatures outside my expertise)

### W5: The 21 AGB = 0 rows are assigned values, not destructive samples, and they remain in training
**Problem**: The abstract and §2.2.1 report "153 destructive samples". In `script/07_procesar_datos.R` (lines 35 to 47) one row with `biomasa = 0` is created for each sampling point at the sowing date, giving 21 assigned zeros; only 132 rows are destructive measurements. The sensitivity analysis excludes these rows from evaluation only (`filter(biomasa > 0)` on the out-of-fold predictions, script 31 line 243); the models are still trained with them. Soil moisture is missing precisely on these rows in three of four fields (sensor installed after sowing; reviewer computation), so the zero rows also carry imputed SM.
**Evidence Anchor**: `text: frontmatter.tex abstract "153 destructive samples, 2020 to 2023"`
**Why it matters**: Assigned zeros at the lowest ∑GDD strongly anchor the intercept and slope of the linear baseline and a block of training leaves in the trees; this favours any season-progress predictor, which is the very mechanism the paper warns about. The sample count is misreported, which affects every n-based statement.
**Suggestion**: Report 132 destructive samples plus 21 assigned sowing values throughout. Add a sensitivity analysis in which the zero rows are removed from training as well as evaluation, and report the baseline and leading configurations under that design. Explain the rationale for including assigned zeros (and for their predictor values, which are nonzero: ∑GDD ≈ 7 to 8 °C d and nonzero cumulative indices at the sowing date).
**Severity**: Major
**Confidence**: 5 (verified in code and data)

### W6: The within-fold correlation filter removes ∑GDD from the All model in two folds, biasing the All comparison and the importance analysis
**Problem**: `step_corr` (threshold 0.9) removes, from each highly correlated pair, the predictor with the larger mean absolute correlation; it has no notion of scientific priority. In the La Cancha and Villa Baviera folds it removed ∑GDD from the All set (Table S3), so in those folds the All model had no thermal time and relied on cumulative indices as proxies. Group permutation importance of ∑GDD (0.41 t ha⁻¹) is then an average over two folds where the predictor was absent from the model; there its importance is structurally near zero, and is only nonzero at all because the permuted raw column still enters the KNN imputation distance before `step_corr` (CSV minimum −0.02 t ha⁻¹).
**Evidence Anchor**: `table: Table S3, rows "La Cancha" and "Villa Baviera", first removed predictor ∑GDD`
**Why it matters**: The conclusion "RF with all 54 predictors did not improve on the baseline" and the importance statement "its most important optical predictors were cumulative indices" are partly produced by a preprocessing rule that deleted the baseline predictor from the All model in half the folds. The All model therefore does not test "satellite plus thermal time" consistently.
**Suggestion**: Exempt ∑GDD (and arguably the W variables) from the correlation filter, applying it only among satellite predictors, and rerun All, S2+W, S2+W+S1 and the importance analysis. Report importance per fold together with whether the predictor was retained in that fold, rather than averaging over folds where it was removed.
**Severity**: Major
**Confidence**: 5 (verified in code and Table S3)

### W7: Importance and ALE for correlated cumulative predictors are interpreted beyond what they support, and the headline model is never interpreted
**Problem**: Permutation within the held-out fold pairs, for example, a late-season cumulative index with an early-season ∑GDD, creating combinations far outside the data; group permutation mitigates but does not remove this for between-group correlations (∑GDD with ∑S2 NDRE3, r = 0.80 to 0.83; ∑GDD with ∑PS CI_red, r = 0.75; reviewer computation). ALE is computed on a model refitted to all 153 rows with a correlation filter estimated on all data, so it describes an in-sample model that differs from the four fold models whose importance selected the top six predictors. Finally, interpretation is done only for All RF, while the paper's positive claim concerns S1+GDD RF; the near-zero S1 group importance in All RF is used in §4.2 as evidence about S1, although mtry = 7 of 54 limits S1's role in that model.
**Evidence Anchor**: `text: §2.3.5 "computed for the RF model fitted to all 153 samples"`
**Why it matters**: Objective iii is to identify predictors that drive held-out performance; the evidence offered speaks to a different model (All RF in sample) than the one whose performance is highlighted.
**Suggestion**: Use conditional permutation within sampling date or within ∑GDD strata (permute a predictor only among samples of the same date and fold), which directly asks whether a predictor carries information beyond season progress. Compute ALE per fold on the fold models and show their spread. Apply the same held-out and conditional importance to S1+GDD RF, which would also help resolve W2.
**Severity**: Major
**Confidence**: 4 (core expertise: interpretation under dependence)

### W8: Pseudo-replication within sampling dates and the effective sample size are not acknowledged
**Problem**: The 153 rows come from only 29 sampling dates (8, 8, 7 and 6 per field; reviewer computation). Within a date, ∑GDD, ∑PP and SM are identical, and the within-date SD of AGB (1.6 to 3.7 t ha⁻¹) is small relative to the seasonal range. Each held-out RMSE is therefore effectively estimated from six to eight date clusters, and training sets contain 21 to 23 distinct values of the dominant predictor.
**Evidence Anchor**: `absence: §2.3.4 and §4.6, expected a statement of the number of distinct sampling dates per fold and its effect on metric precision; checked §2.2.1, §2.3.4, §3.1, §4.6`
**Why it matters**: Per-fold metrics are noisier than n = 30 to 48 suggests, which compounds W1 and W3, and the step-function failure of tree models on ∑GDD is a direct consequence of this structure.
**Suggestion**: Report the number of dates per fold, use date-level resampling for uncertainty (W1), and consider reporting date-aggregated metrics alongside sample-level ones.
**Severity**: Minor
**Confidence**: 5 (verified in data)

### W9: Hidango North and Hidango South share site, station and soil, so two folds are not full site transfers
**Problem**: When Hidango North is held out, Hidango South (same site, same weather station 1 km away, same soil unit, adjacent season) is in training, and vice versa. Two of the four folds therefore test new field and season at a known site, while the La Cancha and Villa Baviera folds test new sites.
**Evidence Anchor**: `table: Table 1, row "Hidango North and South", station 260 shared`
**Why it matters**: The framing is transfer "to fields not used for training"; the S1+GDD gains over the baseline occur mainly in Hidango North and La Cancha, so the composition of folds matters for interpretation.
**Suggestion**: Add a leave-one-site-out sensitivity (three folds: Hidango, La Cancha, Villa Baviera) for the baselines and leading configurations, and state explicitly that the Hidango folds are within-site transfers.
**Severity**: Minor
**Confidence**: 4 (core expertise: grouped CV design)

### W10: Residual all-sample screening and undocumented provenance of the 54 candidate predictors
**Problem**: The first-stage correlation screen used all samples (acknowledged in §4.6). The code also shows that the 54 candidates were formed by "restoring" predictors to an earlier 39-predictor set that had itself been screened on all data, with comments such as "S2_EVI RESTORED (Enhanced VI, very important!)" (`script/31_loso_diseno_limpio.R` lines 49 to 70), which suggests that restoration decisions were informed by earlier analyses of the same AGB data.
**Evidence Anchor**: `text: §2.3.2 "This screening used all samples but only the correlations among predictors; AGB was not used."`
**Why it matters**: The leakage is unsupervised and mainly favours the satellite sets, so it is conservative for the headline that the baseline wins, but the statement that AGB was never used in predictor choice is not fully verifiable from the documented procedure.
**Suggestion**: Document the exact procedure that produced the 54 candidates, including any restoration decisions and their rationale, or regenerate the candidate set by a rule applied without reference to earlier model results (for example, the within-fold filter only, starting from all 89 VIs and bands).
**Severity**: Minor
**Confidence**: 3 (inferred from code comments; the authors can clarify)

### W11: Imputation, two-sided smoothing and cumulative sums: no cross-fold leakage, but look-ahead and structural missingness should be stated precisely
**Problem**: I checked these for leakage. KNN imputation is fitted within folds; for held-out rows it borrows SM from training site-seasons, because SM is missing on the sowing rows (three fields) and on one additional date in Villa Baviera (10 of 30 rows). Imputation therefore transfers another field's soil moisture to the held-out field, not information about AGB. The two-sided Kalman smoother and the cumulative sums operate within each field's own time series, so they do not leak across folds, but they use imagery acquired after the sampling date.
**Evidence Anchor**: `text: §2.3.3 "Soil moisture was the only predictor with missing daily values (12.5% to 33% of samples per site-season)"`
**Why it matters**: Readers may misread "in-season estimation" as real-time; and imputed SM on exactly the zero rows interacts with W5.
**Suggestion**: State that SM missingness is structural (sensor installation after sowing), that held-out SM is imputed from other fields, and that the satellite predictors at a date use future acquisitions; consider a causal (forward-only) filter as a sensitivity for the leading configurations.
**Severity**: Minor
**Confidence**: 5 (verified in code and data)

### W12: Single seed and software versions not reported
**Problem**: RF and XGBoost were run with one seed; XGBoost with row subsampling can vary by a few tenths of t ha⁻¹, which is the size of the leading differences. The code pins xgboost 1.7.11.1 because of failures in 3.2, but the Software section reports no package versions.
**Evidence Anchor**: `absence: §2.4 Software, expected package versions (ranger, xgboost, recipes, DALEX) and seed handling; checked §2.3.4, §2.4, Data availability`
**Why it matters**: Reproducibility of differences of 0.1 to 0.4 t ha⁻¹.
**Suggestion**: Repeat the leading configurations over 10 or more seeds and report the seed spread; list package versions.
**Severity**: Minor
**Confidence**: 4 (core expertise: computational reproducibility)

---

## Detailed Comments

### Title & Abstract
- The title is accurate. The abstract sentence "Under the same algorithm, adding S1 to ∑GDD reduced RMSE by about 30% with RF and 23% with XGBoost" should be qualified until the noise control of W2 is reported.
- "With four fields, differences among the leading configurations were smaller than the variability among folds" should be rewritten in terms of paired differences and their uncertainty (W1).
- "153 destructive samples" should be corrected (W5).

### Introduction
- The objectives are clear and testable. Objective iii ("identify the predictors ... that drive held-out performance") is broader than what the interpretation analysis currently delivers (W7).

### Methodology / Research Design
- §2.2.2: base temperature description does not match the code (W4).
- §2.3.2: the within-fold filter removing ∑GDD needs a design fix, not only disclosure (W6).
- §2.3.4: replace the SD rule with paired inference (W1); report the number of configurations compared and treat the ranking as exploratory (W3).
- §2.3.4: the AGB = 0 sensitivity should also exclude these rows from training (W5).
- §2.3.5: ALE on a full-data refit; prefer per-fold ALE and conditional permutation (W7).
- The design table (which sets get LM, RF, XGBoost) is clear from §2.3.4, but the absence of LM for S1+GDD and of any GAM or spline baseline leaves the key algorithm by predictor cell empty (W2).

### Results / Findings
- §3.1 explains the tree failure on ∑GDD well. This explanation is precisely why the RF/XGBoost ∑GDD models are a weak reference for the S1 effect (W2).
- §3.2: "Excluding the 21 samples ... did not change their order": in `rev2_loso_limpio_resumen.csv` the top six keep their order, but positions 7 to 10 change (W LM, All RF, PS+GDD RF and XGBoost reorder). Say "did not change the order of the leading configurations".
- §3.3: the ∑GDD importance description ("close to zero in the other three folds, including the La Cancha and Villa Baviera folds, where the correlation filter had removed ∑GDD") should say that in those two folds the model could not use ∑GDD at all (W6).

### Discussion
- §4.2 and §4.3 treat the same-algorithm comparison as separating sensor from algorithm. It separates the algorithm, but not the regularising effect of any within-date-varying predictor (W2).
- §4.6 states "No formal statistical test can reliably separate configurations". Paired tests are possible but have low power; say so and report the minimum detectable difference (W1).
- The suggested residual model (linear ∑GDD plus a tree on residuals) is a good idea and cheap to run with existing data; I would encourage including it rather than deferring it.

### Conclusion
- The conclusion that thermal time carries most transferable signal is supported by the data as analysed, subject to W4 and W5 not changing it. The S1 statement should be weakened as in W2 and W3.

### References
- Not assessed in depth (outside my seat). The cited CV literature (Roberts 2017, Meyer 2021, Ploton 2020) is appropriate.

---

## Questions for Authors

1. If ∑GDD is computed with a single base temperature (no switch at the observed tillering date), does the linear ∑GDD baseline still outperform DAS and every single-sensor model, and does S1+GDD RF still beat it in three of four folds?
2. What RMSE does RF obtain with ∑GDD plus three noise predictors, and with ∑GDD plus S1 permuted within sampling date, under the same folds and hyperparameters? How does that compare with the 5.80 t ha⁻¹ of S1+GDD RF?
3. How were the 54 candidate predictors produced from the 89 VIs and bands, and were any "restored" predictors chosen after inspecting earlier model results on the same AGB data?
4. When the 21 assigned zero rows are removed from training as well as evaluation, how do the ∑GDD LM baseline and the leading configurations perform?

---

## Minor Issues

### Language / Grammar
- §3.2: "did not change their order" overstated (see Results comments).
- §2.2.2 and §4.5: harmonise the description of when the 6 °C base applies.

### Figures and Tables
- Table 5: add a column with the mean paired absolute ΔRMSE and its SD (or interval).
- Figure 4: mean ± SD error bars invite exactly the misreading in W1; consider plotting per-fold paired differences from the baseline instead of, or in addition to, marginal SDs.
- Figure 7: mark, for each fold, whether ∑GDD was retained by the correlation filter.

### Layout
- None within my scope.

---

## Criterion-Bound Judgements

Calibration status: `NOT_CALIBRATED`

| Dimension | Criterion source | Judgement | Evidence anchor(s) | Rationale | Uncertainty / scope limit | Decision bearing? |
|---|---|---|---|---|---|---|
| Originality | general standards | NOT_ASSESSED | | Outside methodology seat | | No |
| Methodological Rigor | general standards for grouped CV studies | PARTLY_MEETS | W1, W2, W4, W6 | Clean within-fold design and fixed hyperparameters; inference rule invalid, key negative control missing, baseline uses held-out phenology, filter removes ∑GDD from All in two folds | Repairs feasible with existing data | Yes |
| Evidence Sufficiency | general standards | PARTLY_MEETS | W1, W3, W5, W8 | Four folds and 29 dates limit power; ranking is post hoc selection; sample count misreported | Inherent to data; authors already hedge partly | Yes |
| Argument Coherence | general standards | PARTLY_MEETS | W2, W7 | "Genuine S1 contribution" conflicts with the paper's own tie-breaking caveat; interpretation done on a different model than the one highlighted | | Yes |
| Writing Quality | general standards | NOT_ASSESSED | | Outside seat | | No |
| Literature Integration | general standards | NOT_ASSESSED | | Outside seat | | No |
| Significance & Impact | general standards | NOT_ASSESSED | | Outside seat | | No |

The recommendation rests on the unresolved Methodological Rigor and Evidence Sufficiency items. All are repairable by reanalysis of the existing data (paired inference, noise controls, single-base ∑GDD, training without assigned zeros, ∑GDD exempt from the correlation filter, conditional importance). None requires new field data. The baseline-dominance conclusion is likely to survive, but the Sentinel-1 statements in the abstract and Conclusion may not.

---

## Recommendation signal

**Major Revision.** The design is sound in its core (grouped CV, in-fold preprocessing, fixed hyperparameters, baseline reporting), and the main message is plausible and useful. However, the only inferential rule is invalid for paired comparisons, the evidence for a genuine Sentinel-1 effect lacks the noise control the paper itself implies, the yardstick ∑GDD encodes held-out phenology contrary to the stated method, the All comparison is distorted by removal of ∑GDD in two folds, and the sample count includes 21 assigned zeros. Each requires reanalysis before the claims can be accepted. I do not make the final decision.
