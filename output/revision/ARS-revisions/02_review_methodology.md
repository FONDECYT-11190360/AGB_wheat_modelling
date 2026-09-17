# Peer Review Report

## Manuscript Information
- **Title**: How much do satellite sensors add to thermal time? A leave-one-site-season-out evaluation of Sentinel-1, Sentinel-2, and PlanetScope predictors for wheat above-ground biomass
- **Manuscript ID**: ARS-Review-2026-09-17
- **Review Date**: 2026-09-17
- **Review Round**: Round 1 review

---

## Reviewer Information

### Reviewer Role
Peer Reviewer 1 (Methodology)

### Reviewer Identity
Associate Professor of Statistics and Data Science with specialization in machine learning for spatial and agricultural data. Published extensively on spatial cross-validation, hyperparameter tuning strategies, and model transferability. Background in environmental statistics and precision agriculture modeling.

### Review Focus
This review evaluates the statistical and machine learning methodology of the study, focusing on the cross-validation design, potential data leakage, hyperparameter choices, model evaluation metrics, and the validity of the comparison between predictor sets.

---

## Overall Assessment

### Recommendation
- [X] **Major Revision** — Substantial revisions needed, re-review required after revision

### Confidence Score
5 — Completely within my area of expertise, I am very confident in my assessment

### Summary Assessment

This manuscript employs a rigorous leave-one-site-season-out cross-validation (LOSO-CV) design to evaluate wheat AGB estimation models, which is the appropriate validation scheme for assessing transferability to new locations and seasons. The use of fixed hyperparameters to avoid optimistic bias from hyperparameter selection on the same folds used for performance reporting is methodologically sound for a small-sample study (n=4 site-seasons). The comprehensive evaluation across 14 predictor sets with three algorithms (LM, RF, XGBoost) provides valuable insights into sensor contributions.

However, the study has three critical methodological issues that must be resolved before publication: (1) The TODO comment at line 218 indicates that the predictor screening process from 53 VIs to 43 final predictors is not documented, and it is unclear whether AGB was used in this screening; if so, this constitutes data leakage that invalidates all reported performance metrics. (2) The within-fold correlation filter (|r| ≥ 0.9) is applied after removing predictors outside each set, which means different predictors are retained in different folds; while the authors acknowledge this, they do not discuss whether this fold-specific predictor selection biases the comparison among predictor sets. (3) The paper reports mean±SD metrics across four folds and uses informal comparisons ("smaller than the between-fold SD") without acknowledging that formal significance testing is underpowered; while the authors correctly avoid p-values, they should more explicitly caution against overinterpreting small differences.

With these issues addressed, the paper would provide a valuable methodological template for remote sensing validation studies.

---

## Strengths

### S1: Appropriate use of grouped cross-validation
The leave-one-site-season-out cross-validation design is the correct validation scheme for testing model transferability to new locations and seasons. Grouping all samples from one site-season together prevents optimistic performance estimates that arise when samples from the same field appear in both training and test sets.

**Evidence Anchor**: `text: Methods §2.3.4 "Performance was evaluated with LOSO-CV. The data were partitioned into four folds, each containing all samples of one site-season (Hidango 2021-2022, Hidango 2022-2023, La Cancha 2022-2023, and Villa Baviera 2020-2021)"`

### S2: Fixed hyperparameters to avoid selection bias
The decision to use fixed hyperparameters rather than tuning them is methodologically sound for a study with only four folds. Tuning hyperparameters on the same folds used to report performance would introduce optimistic bias.

**Evidence Anchor**: `text: Methods §2.3.4 "Using fixed hyperparameters means that the held-out site-season is never used to select a model configuration, which avoids the optimistic bias of selecting hyperparameters on the same folds used to report performance"`

### S3: Transparent reporting of per-fold results
The decision to report per-fold results in Supplementary Table S2 in addition to mean±SD metrics is excellent for transparency. This allows readers to assess between-fold variability and identify outlier site-seasons.

**Evidence Anchor**: `text: Methods §2.3.4 "we report the mean and standard deviation (SD) of each metric across the four folds"; text: "per-fold results (Supplementary Material Table S2)"`

### S4: Appropriate metric choices
The use of R², RMSE, and MAE is appropriate for regression tasks. The computation of R² against the mean of the held-out site-season (allowing negative values when models predict worse than the mean) is the correct definition for cross-validation.

**Evidence Anchor**: `text: Methods §2.3.4 "For each held-out site-season we computed the coefficient of determination as R² = 1 − SSE/SST"`

### S5: Group-level permutation importance
The decision to compute permutation importance for predictor groups (jointly permuting all predictors of a sensor) addresses the well-known issue that permuting one of several correlated predictors spreads importance across the group and generates unrealistic predictor combinations.

**Evidence Anchor**: `text: Methods §2.3.5 "Because permuting one of several correlated predictors spreads importance across the group and generates unrealistic predictor combinations (Strobl 2008), importance was also computed for predictor groups, permuting all predictors of a group jointly (∑GDD, ∑PP, SM, S1, S2, and PS)"`

---

## Weaknesses

### W1: Undocumented predictor screening process (data leakage risk)
**Problem**: The TODO comment at line 218 states: "describe how the 43 candidate predictors in script 14 (`vars`) were screened from the full set of bands, indices, and cumulative indices, and confirm that AGB was not used in that screening. If AGB was used, the screening must be repeated inside the cross-validation folds." The manuscript mentions 53 VIs were computed (line 169), but Table 4 lists only 43 candidate predictors (39 after excluding 4 PS red edge predictors). The screening process from 53 to 43 is not documented. If AGB or any test-fold data was used to select these 43 predictors, this constitutes data leakage: the held-out site-season would have influenced which predictors are included, inflating performance estimates.

**Evidence Anchor**: `text: Methods §2.3.2, line 218 "<!-- TODO (authors): describe how the 43 candidate predictors in script 14 (vars) were screened from the full set of bands, indices, and cumulative indices, and confirm that AGB was not used in that screening. If AGB was used, the screening must be repeated inside the cross-validation folds. -->"`

**Why it matters**: Data leakage is one of the most common and serious errors in predictive modeling. If predictors were selected based on their correlation with AGB across all 153 samples (including test folds), then each test fold has "seen" the response variable through the predictor selection process. This inflates apparent performance. For a paper whose main conclusion is that satellite predictors add minimal value beyond thermal time, if that conclusion rests on leaked predictors, it could be completely reversed by proper methodology. This is not a minor technical detail; it is a fundamental validity threat.

**Suggestion**: The authors must provide a complete description of how the 43 predictors were selected. If the screening was based on AGB correlation, R² with AGB, or any statistic computed on the full dataset, the entire analysis must be repeated with predictor selection performed within each training fold only (i.e., predictor screening becomes part of the preprocessing pipeline that is re-estimated for each fold). Alternatively, if predictor screening was AGB-independent (e.g., based solely on literature recommendations, sensor availability, multicollinearity among VIs, or domain knowledge), this must be explicitly stated and the screening criteria documented. A third option is to use all 53 VIs without screening, though this increases the risk of overfitting with 153 samples.

**Severity**: Critical — this single issue, if predictor selection involved AGB or test data, invalidates the entire study's performance estimates and prevents publication.

**Confidence**: 5 — core expertise: data leakage detection in cross-validation

### W2: Fold-specific predictor sets from within-fold correlation filter
**Problem**: The manuscript states: "Predictors outside each set were removed first. The remaining predictors were then processed within each training fold only: missing values were imputed by k-nearest neighbors, predictors were centered and scaled, one predictor of each pair with Pearson |r| ≥ 0.9 was removed, and predictors with zero variance were dropped" (lines 252-255). Later: "Because the correlation filter was estimated on each training fold, the retained predictors differed slightly among folds" (line 254). This means that the "All" set does not contain the same predictors across folds; for example, in one fold ∑GDD is removed due to correlation with cumulative indices (line 255), while in other folds it is retained. While this is technically correct (the correlation filter should be estimated on training data only), it introduces a subtle confound: when comparing the "All" set across folds, you are comparing slightly different models.

**Evidence Anchor**: `text: Methods §2.3.3 "Because the correlation filter was estimated on each training fold, the retained predictors differed slightly among folds. In the All set, the filter removed two to four predictors per fold, and in the fold where La Cancha 2022-2023 was held out it removed ∑GDD"`

**Why it matters**: The across-fold comparison assumes that performance differences are due to the held-out site-season's characteristics, not due to which predictors happened to pass the correlation filter in that fold's training set. If ∑GDD is removed in one fold because it correlates with cumulative indices in the training data, but those cumulative indices happen to transfer poorly to the held-out fold, then that fold will perform worse not because of its site characteristics but because the most important predictor was excluded. The paper does not discuss whether this fold-specific predictor variation systematically biased any comparisons.

**Suggestion**: (1) Report which predictors were removed by the correlation filter in each fold for the "All" set and the major predictor sets (S2, PS). This could be a supplementary table. (2) Discuss whether the folds where ∑GDD was removed (La Cancha fold) had worse performance, and whether this could be due to removing the most important predictor rather than site characteristics. (3) Consider a sensitivity analysis: re-run the "All" set with a pre-specified correlation filter applied to all 153 samples (accepting the minor leakage of correlation structure) to check whether the rank order of folds changes. If the rank order is stable, this increases confidence that fold-specific predictor sets are not driving the results.

**Severity**: Major — this fold-specific predictor variation could bias comparisons and is not adequately discussed; substantial discussion revision needed.

**Confidence**: 5 — core expertise: cross-validation design and feature selection

### W3: Statistical inference with n=4 folds not explicitly cautioned
**Problem**: The paper reports mean±SD across four folds and uses informal comparisons like "differences smaller than the between-fold SD" (line 261) and "smaller than the variability among held-out site-seasons" (line 396) to caution against overinterpreting small differences. However, it does not explicitly state that formal significance testing is impossible or misleading with only four folds. Some readers may interpret the ∆RMSE percentages and the "Folds" column in Table 5 (showing how many of four folds beat the baseline) as implying statistical significance.

**Evidence Anchor**: `text: Methods §2.3.4 "With only four folds, formal significance tests are not informative; we therefore report per-fold results (Supplementary Material Table S2) and treat differences smaller than the between-fold SD as indistinguishable"`

**Why it matters**: While the paper correctly avoids p-values and acknowledges that "with only four folds, formal significance tests are not informative," this caveat appears only once in the Methods. Readers skimming the Results or Discussion might see statements like "improved on the baseline in three of four site-seasons (mean RMSE reduction of 5.9%)" and interpret "three of four" as statistically significant, when in fact a 3-1 split has insufficient power to detect anything. The risk is that readers will overinterpret model rankings.

**Suggestion**: (1) Add a sentence to the Results section explicitly stating that with n=4 folds, no configuration is statistically distinguishable from any other, and observed rankings should be considered exploratory. (2) In the Discussion, when interpreting the S1+GDD result, add a sentence like: "However, with only four site-seasons, we cannot rule out that this ranking would reverse in a larger sample." (3) In the Conclusion, restate the n=4 limitation and that findings should be confirmed on independent site-seasons.

**Severity**: Major — readers may overinterpret model rankings; substantial clarification needed in Results and Discussion.

**Confidence**: 5 — core expertise: statistical inference with small samples

### W4: Lack of sensitivity analysis for key modeling choices
**Problem**: The paper makes several modeling choices that could affect conclusions but does not test their sensitivity: (1) Fixed hyperparameters: the authors state that hyperparameters are "reasonable defaults" but acknowledge that "tuning them with an inner cross-validation loop could change the ranking of the ML models; the linear baselines have no hyperparameters and would be unaffected" (lines 401-403). (2) Correlation threshold: the |r| ≥ 0.9 threshold is standard, but the paper does not test 0.8 or 0.95. (3) Kalman smoothing for optical VIs: the paper uses Kalman smoothing (two-sided filter) for S2 and PS but step-function extension for S1. Why not use the same interpolation method for all sensors?

**Evidence Anchor**: `text: Discussion §4.4 "The fixed hyperparameters may not be optimal for every predictor set, and tuning them with an inner cross-validation loop could change the ranking of the ML models"`

**Why it matters**: The paper's conclusion that "satellite predictors provided at most modest and configuration-dependent gains" hinges on the finding that the S1+GDD improvement was not reproduced with XGBoost (Table 5: RF −5.9%, XGBoost +4.3%). If this algorithm sensitivity is due to suboptimal hyperparameters for XGBoost with S1 data, then tuning could stabilize the conclusion. Similarly, if using Kalman smoothing for S1 instead of step-function extension improved S1's performance, it would strengthen (or weaken) the conclusion about SAR's value.

**Suggestion**: (1) Conduct a sensitivity analysis on at least one key choice, such as the correlation threshold (test 0.8 and 0.95) or the interpolation method for S1 (test Kalman smoothing). Report whether the main conclusions (thermal time dominates; S1+GDD is best) hold. (2) If a full sensitivity analysis is infeasible given computational constraints, discuss in the limitations which choices are most likely to affect conclusions. For example: "The step-function temporal extension for Sentinel-1 may have introduced more noise than Kalman smoothing, potentially reducing S1's apparent contribution."

**Severity**: Major — the sensitivity of conclusions to modeling choices is not tested; readers cannot assess robustness.

**Confidence**: 4 — adjacent field: applying general ML best practices rather than agriculture-specific expertise

### W5: Imbalance in model complexity across predictor sets
**Problem**: The linear model (LM) is used only for DAS, GDD, and W sets, while RF and XGBoost are used for all other sets. This creates an unfair comparison: the baselines are restricted to linear relationships, while satellite models can capture nonlinear patterns. For example, if the AGB-GDD relationship is nonlinear (which is physiologically plausible; AGB accumulation may saturate at high GDD), then an RF model on GDD alone might outperform the linear GDD baseline, making the satellite contribution appear smaller.

**Evidence Anchor**: `text: Methods §2.3.4 "The DAS and ∑GDD baselines and the weather set were fitted with ordinary least squares linear regression (LM). All sets except the two baselines were fitted with random forest (RF) and extreme gradient boosting (XGBoost)"`

**Why it matters**: The comparison between the linear GDD baseline (R²=0.53, RMSE=6.24 t/ha) and the RF S1+GDD model (R²=0.58, RMSE=5.80 t/ha) conflates two sources of improvement: (1) the value of S1 predictors, and (2) the value of a nonlinear model. If RF on GDD alone reached R²=0.56, then S1 would be contributing only 0.02 R² points, not 0.05. Without testing RF on GDD alone, we cannot isolate the satellite contribution from the algorithmic contribution.

**Suggestion**: Add two model configurations to Table 5: (1) RF with GDD only, and (2) XGBoost with GDD only. Compare their performance to the linear GDD baseline and to the S1+GDD models. If RF/XGBoost on GDD alone substantially improves on the linear baseline, discuss this in the Results and adjust the interpretation of the satellite contribution. Alternatively, if this analysis is not feasible, discuss this as a limitation: "The baseline uses a linear model while satellite models use nonlinear algorithms, so the reported satellite contribution may include an algorithmic effect."

**Severity**: Major — the comparison conflates algorithmic and predictor contributions; substantial revision needed.

**Confidence**: 5 — core expertise: model comparison design

---

## Coverage Receipt

**Covers**: Strengths

| Dimension examined | What you checked | Basis for "nothing found" |
|--------------------|------------------|---------------------------|
| Statistical testing | Checked whether p-values or confidence intervals were reported for model comparisons | No formal statistical tests were performed, which is appropriate given n=4 folds. The authors correctly avoid inferential claims, so no strength found for "proper statistical inference" (that would require a larger sample). |
| Reproducibility | Checked whether code and data availability was stated | Code and data are available on GitHub (Data and code availability section), but this is expected for journals in this field, not a distinguishing strength. |

---

## Detailed Comments

### Methodology / Research Design

**Cross-validation design**:
- The LOSO-CV design is excellent and appropriate for the research question. The four folds (one per site-season) test transferability to new locations and seasons.
- The choice to compute R² against the held-out fold's mean (rather than the global mean) is correct for cross-validation.
- The use of fixed hyperparameters to avoid selection bias is appropriate given the small number of folds.

**Predictor selection and preprocessing**:
- **Critical issue**: The TODO at line 218 about predictor screening must be resolved (see Weakness W1).
- The within-fold preprocessing pipeline (imputation, scaling, correlation filter) is generally appropriate, but the fold-specific predictor retention introduces a confound (see Weakness W2).
- The k-NN imputation for missing values (12.5-33% for soil moisture) is reasonable. The paper does not report the k value used; this should be specified.
- The correlation threshold of |r| ≥ 0.9 is standard, but sensitivity to this choice is not tested (see Weakness W4).

**Model choices**:
- The restriction of LM to baselines only creates an unfair comparison (see Weakness W5).
- The fixed hyperparameters for RF (1000 trees, min node size 5, min(7,p) predictors per split) and XGBoost (500 trees, depth 4, lr 0.05, min node size 5, row subsample 0.8, min(7,p) predictors per split) are reasonable defaults from the companion letter, but their optimality is not validated.
- The choice of min(7,p) predictors sampled per split is interesting. For the "All" set with ~39 predictors, this is 7; for the "S1" set with 3 predictors, this is 3 (i.e., all predictors at every split, reducing to bagging). The paper does not discuss whether this p-dependent choice creates a systematic bias favoring smaller predictor sets.

**Evaluation metrics**:
- R², RMSE, and MAE are appropriate. The paper reports all three, which is good practice.
- The decision to report metrics both with and without AGB=0 samples is transparent and appropriate.
- The ∆RMSE metric (relative difference from the GDD baseline) is useful for comparison. The "Folds" column (how many folds beat the baseline) is informative but could be misinterpreted as a significance test (see Weakness W3).

**Model interpretation**:
- The use of permutation importance on held-out data is methodologically sound. Computing group-level importance (jointly permuting sensors) is an excellent approach to address correlated predictors.
- The use of ALE rather than partial dependence plots is appropriate given correlated predictors. The interpretation that monotonic ALE profiles indicate "measures of season progress" is well-reasoned.

### Results / Findings
- The Results section clearly reports mean±SD metrics for all configurations (Table 5) and per-fold results (Supplementary Table S2).
- Figures 5 and 6 effectively visualize observed vs. predicted AGB and temporal trajectories.
- The finding that all models compress the AGB range (underestimating high values, overestimating low values in Hidango 2022-2023) is a common regression pattern and is clearly shown.
- The permutation importance results (Figure 7) are clearly presented. The finding that group importance exceeds the sum of individual importance is correctly interpreted as redundancy among correlated predictors.

### Discussion
- The physiological interpretation of why GDD carries most transferable signal is appropriate and clearly explained.
- The comparison with UAV/hyperspectral studies (R²=0.81-0.91) and the attribution to validation scheme differences is a valuable contribution to the literature.
- The recommendation that AGB studies report phenological baselines is strong and actionable.
- **Limitation acknowledgment is excellent**: The paper transparently acknowledges the small number of folds (n=4), confounding of geography and wheat habit, fixed hyperparameters, predictor screening uncertainty, SAR preprocessing choices, and other issues.
- **Missing**: Discussion of whether the fold-specific correlation filter biased comparisons (see Weakness W2), and whether the linear-vs-nonlinear model comparison confounds algorithmic and predictor contributions (see Weakness W5).

---

## Questions for Authors

1. **Predictor screening (Critical)**: Line 218 contains a TODO asking you to confirm that AGB was not used in screening the 43 candidate predictors from the full 53 VIs. Please provide the complete screening methodology. If AGB correlation was used, will you re-run the analysis with within-fold predictor selection?

2. **Fold-specific correlation filter**: In the La Cancha fold, ∑GDD was removed by the correlation filter, while in other folds it was retained. Did this fold have worse performance? Could you report which predictors were removed in each fold for the "All" set, and discuss whether this affected the cross-fold comparison?

3. **Linear vs. nonlinear models for baseline**: You compare a linear GDD model (R²=0.53) to nonlinear RF/XGBoost models with satellite data. Have you tested RF or XGBoost on GDD alone to isolate the satellite contribution from the algorithmic contribution? If not, can you discuss this as a limitation?

4. **k-NN imputation parameter**: What value of k was used for k-nearest neighbors imputation of missing soil moisture values? This should be reported in the Methods.

---

## Minor Issues

### Methodology reporting
- Line 254: The paper states the correlation filter removes "one predictor of each pair with Pearson |r| ≥ 0.9" but does not specify which predictor of each pair is removed (the one with lower variance? random? the one with lower correlation to AGB?). This should be specified for reproducibility.
- Supplementary Material: Equations S1-S3 are referenced for R², RMSE, and MAE definitions. Confirm these equations are correct (especially the R² formula for cross-validation).

---

## Criterion-Bound Judgements

Calibration status: `NOT_CALIBRATED`

| Dimension | Criterion source | Judgement | Evidence anchor(s) | Rationale | Uncertainty / scope limit | Decision bearing? |
|---|---|---|---|---|---|---|
| Originality | General | NOT_ASSESSED | N/A | Methodology review focuses on statistical rigor, not novelty | Outside reviewer scope | No |
| Methodological Rigor | General | PARTLY_MEETS | text: Line 218 TODO; text: §2.3.4 fixed hyperparameters; text: §2.3.3 fold-specific correlation filter | LOSO-CV design and fixed hyperparameters are rigorous. Critical unresolved issue: predictor screening process (line 218) may involve data leakage. Major issue: fold-specific correlation filter creates predictor set variation not adequately discussed. | Rigor can be upgraded to MEETS if screening is AGB-independent | Yes (Critical if leakage; Major for filter discussion) |
| Evidence Sufficiency | General | PARTLY_MEETS | table: Table 5 shows 32 configurations; text: §4.4 "four site-seasons" | Comprehensive evaluation across predictor sets and algorithms. Limited by small sample (n=4 folds) acknowledged in limitations. | Statistical power for model comparison is low | Yes (Major) |
| Argument Coherence | General | MEETS | text: Results and Discussion flow logically | Methods → Results → Discussion flow is logical. Interpretation of findings is well-reasoned. | None | No |
| Writing Quality | General | NOT_ASSESSED | N/A | Not the focus of methodology review | Outside reviewer scope | No |
| Literature Integration | General | NOT_ASSESSED | N/A | Not the focus of methodology review | Outside reviewer scope | No |
| Significance & Impact | General | NOT_ASSESSED | N/A | Methodology review focuses on validity, not impact | Outside reviewer scope | No |

**Recommendation rationale**: The cross-validation design and fixed hyperparameters are methodologically sound. However, **two decision-bearing criteria are only partly met**: (1) **Methodological Rigor** is undermined by the unresolved predictor screening process (Critical if AGB was used; Major if not clearly documented) and the fold-specific correlation filter discussion gap (Major). (2) **Evidence Sufficiency** is limited by n=4 folds (Major, acknowledged by authors). Additionally, the comparison between linear baselines and nonlinear satellite models (W5) and lack of sensitivity analysis (W4) are Major methodological weaknesses. Resolving the TODO about predictor screening is mandatory. With these issues addressed, the paper would provide a rigorous methodological template for satellite validation studies.
