# Response to Round 2 simulated review (2026-09-17)

Review: `output/revision/2026-09-17_manuscript_review_round2.md`
Manuscript: `manuscript/manuscript.qmd` (pre-revision copy of the text not kept in git; compare against commit 693b10b)
Analysis code: `script/30_revision_round2_diagnosticos.R` (diagnostics of the original pipeline), `script/31_loso_diseno_limpio.R` (clean analysis reported in the manuscript), `script/32_flowchart_revision.R` (Figure 3)

## Summary

Checking the review items against the code revealed problems larger than those listed in the review. The original results could not support the paper's claims, so, as agreed with the authors, the manuscript was rewritten around a clean leave-one-site-season-out analysis with phenological baselines. The stacked ensemble, the ensemble maps, and the harvest forecasting stage (stage 2) were removed.

### Additional problems found while addressing the review

| Problem | Evidence | Effect |
|---|---|---|
| Script 14 applies `step_corr` to all 43 candidates before `step_rm`, so each recipe's effective predictors differ among folds | `output/tables/rev2_script14_variables_efectivas_por_fold.csv`: ∑GDD is absent from every recipe when Villa Baviera or La Cancha is held out; rec7 has 9 to 15 predictors depending on the fold | Recipe labels were not meaningful; sensor comparisons were confounded |
| PlanetScope red edge predictors (PS_B5, PS_TCARI_OSAVI, PS_MCARI_OSAVI, PS_NDRE_NDVI) are 100% missing for Hidango 2021-2022 and Villa Baviera 2020-2021 and were filled by KNN imputation | `output/tables/rev2_na_predictores_por_sitio.csv` | Imputed values act as site indicators; rec7_RF traditional R² in the Villa Baviera fold was −0.34 |
| The stacked ensemble contains a degenerate member (rec6_XGBoost mod02: sample_size 0.2, min_n 35) whose out-of-fold predictions are constant within each fold, so it works as a site offset | `output/tables/rev2_ensamble_miembros.csv`: weight 1.10, intercept −17.1, within-fold SD 0 | Final ensemble overpredicts in sample (RMSE 11.4 t/ha, R² < 0); Figs 6 to 9 and the stage 2 target were affected |
| Stage 2 models have no skill | `output/tables/rev2_stage2_metricas.csv`: traditional R² −4 to −10 per fold for every lead | Stage 2 removed |
| A ∑GDD linear model beats the headline model | `output/tables/rev2_loso_limpio_resumen.csv`: RMSE 6.24 vs 7.87 t/ha | Central claim of the paper reversed |
| Stage 2 script 20 used `step_corr(starts_with("S2|PS|S1"))`, which selects no columns | script 20 | Correlation filter never ran in stage 2 (moot after removal) |
| NASA Earthdata password hard-coded in `script/01_descargar_imagenes.R` line 12 | script 01 | **Not changed. Rotate the password and purge it from git history.** |

## Item-by-item responses

### Critical

**C1. Sensor-only recipes contain ∑GDD.** Confirmed, and worse than stated: because of the step order (see above), ∑GDD was present in only two of four folds. The new analysis removes predictors before preprocessing, defines single-sensor sets without ∑GDD, adds explicit sensor+GDD and sensor+W sets, and adds DAS and ∑GDD baselines (Methods 2.3.2, Table 4).

**C2. R² was squared correlation.** Confirmed. All reported R² values are now 1 − SSE/SST per held-out site-season (Methods 2.3.4, Supplementary Eq. S3 text). For reference, the original headline rec7_RF had rsq 0.78 but traditional R² 0.14 (`rev2_ranking_40_modelos_sel_rsq.csv`).

**C3. Ranking depends on the metric.** Confirmed. Correction to the review: the numbers quoted as rec1_bagMLP (R² 0.75, RMSE 6.96, MAE 5.34) in the old manuscript belong to rec6_bagMLP; rec1_bagMLP had RMSE 7.89. Tuning and model selection were dropped; all models use fixed hyperparameters, and per-fold results are reported (Table 5, Supplementary Table S2).

**C4. Ablation depends on algorithm.** Confirmed. Table 6 now reports RF and XGBoost for the clean sequential ablation; the text states that the Sentinel-1 effect changes sign between algorithms (RF −3.6%, XGBoost +4.3%) and that all steps are smaller than the between-fold SD. "Independent" and "corroborates" were removed.

**C5. Ensemble and DALEX not reproducible or valid.** Confirmed, plus the degenerate member. The ensemble was removed. Explainability now uses the RF model with all predictors, with permutation importance computed on held-out site-seasons (individual predictors and sensor groups, Fig. 7) and ALE profiles (Fig. 8), all produced by script 31.

### Major

**M1. PlanetScope red edge.** The four red edge predictors were excluded (Section 2.2.3 text, Table 4).

**M2. Predictor preselection.** Not resolved. The screening from 89+ candidates to 43 is not in any committed script. A TODO comment was placed in Methods 2.3.2 and the limitation is stated in Section 4.4. **Authors must describe the procedure and confirm AGB was not used.**

**M3. Time-only baseline and zero AGB anchors.** DAS and ∑GDD baselines added; metrics excluding the 21 AGB = 0 samples are reported (Section 3.2). Two-sided Kalman smoothing is now stated in Section 2.2.4.

**M4. Uncertainty.** Mean ± SD across folds, per-fold comparison with the baseline (ΔRMSE and folds improved), and Supplementary Table S2 with every fold.

**M5. AGB magnitudes.** Authors confirmed the values. The text now reports the true range (0 to 43.5 t/ha, seven samples above 30 t/ha) and states that values above 30 t/ha exceed those commonly reported for wheat. "Policy relevant accuracy" was removed; the Discussion states that RMSE 5.80 t/ha (47% of mean AGB) is too coarse for field-level decisions. Glossary AGB definition aligned with Methods.

**M6. Stage 2.** Removed (no skill; target derived from the defective ensemble). Title no longer mentions forecasting.

**M7. Non-nested tuning.** Resolved by using fixed hyperparameters; nested tuning listed as future work.

**M8. Sentinel-1.** S1B statement corrected (failed 23 December 2021). Speckle, incidence angle, and single-pixel extraction listed as limitations and future work. Orbit handling is still undocumented (see open items).

**M9. Soil moisture.** Layer-thickness conversion, exclusion of the 75 cm sensor, single profile per field, and the imputed missing days are now described.

### Minor

| Item | Action |
|---|---|
| Table 4 `tree_depth` for RF | Old hyperparameter table removed; fixed values in text. The `tree_depth` fix was applied before removal |
| Grid description | Moot (no tuning) |
| EVI citation | Huete2002 in Table 3; Breiman2001 cited for RF |
| B8A label | "Narrow NIR" in Table 2 and Table 3 caption |
| Fig. 6 full model loss | New Fig. 7 reports increases relative to the held-out RMSE of 6.77 t/ha |
| "Most consumed crop" | "one of the main staple crops" |
| Sampling design wording | Rewritten |
| Fig. 8 legend "Hidango S1/S2" | Figure removed |
| Uncited insurance threshold | Paragraph removed |
| Missing literature | Added Ploton2020, Meyer2021, Roberts2017, Strobl2008, Apley2020, Veloso2017, Huete2002, Breiman2001 (verify DOIs, e.g. with /ars-citation-check) |
| Repeated caveats | Consolidated in Section 4.4 |
| Venue and abstract length | Still CEA; abstract is about 300 words, check the guide for authors |
| Table 5 top five/six | Table removed with stage 2 |

## Open items for the authors

1. Describe the predictor preselection (TODO comment in Methods 2.3.2).
2. Rotate the Earthdata password and remove it from the repository history.
3. State Sentinel-1 orbit direction and whether S1B scenes were used for 2020 to 2021.
4. Supplementary author affiliation for F. Zambrano (Universidad San Sebastián) differs from the main text (Universidad de Las Américas).
5. The GRSL companion letter uses the recipe ordering and red edge predictors identified here as problematic; consider whether it needs a correction before acceptance.
6. The EarthArXiv preprint (commit 693b10b) reports the superseded results; post a new version with a note on the changes.
7. The AI-use declaration lists ChatGPT-3.5 and Copilot only; update it if AI tools were used for this revision.
8. Introduction paragraph 1 still moves between yield and AGB; consider tightening.
9. Scripts 14 to 29 still produce the superseded results; consider moving them to an archive folder.

## Reproducibility note

xgboost 3.2 (installed for R 4.6) cannot load the saved 1.7.x models and failed with memory alignment errors when fitting. Scripts 30 and 31 were run with xgboost 1.7.11.1 from an isolated library, selected with the environment variable `XGB17_LIB`. Packages tidymodels, stacks, baguette, kknn, glmnet, DALEX, DALEXtra, and brulee were installed into the R 4.6 user library.
