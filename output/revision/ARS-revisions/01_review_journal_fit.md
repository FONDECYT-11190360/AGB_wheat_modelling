# Peer Review Report

## Manuscript Information
- **Title**: How much do satellite sensors add to thermal time? A leave-one-site-season-out evaluation of Sentinel-1, Sentinel-2, and PlanetScope predictors for wheat above-ground biomass
- **Manuscript ID**: ARS-Review-2026-09-17
- **Review Date**: 2026-09-17
- **Review Round**: Round 1 review

---

## Reviewer Information

### Reviewer Role
Journal-Fit Reviewer

### Reviewer Identity
Associate Editor at *Computers and Electronics in Agriculture*, with 15 years of experience in precision agriculture and agricultural remote sensing. Background in agricultural engineering, with expertise in operational applications of satellite data for crop monitoring and decision support systems.

### Review Focus
This review evaluates the manuscript's fit with *Computers and Electronics in Agriculture*, focusing on whether the comparison of satellite sensors against a thermal time baseline addresses the journal's operational technology emphasis, whether the findings are sufficiently novel and actionable for the journal's readership of agricultural engineers and precision agriculture practitioners, and whether the validation rigor supports the conclusions.

---

## Overall Assessment

### Recommendation
- [X] **Major Revision** — Substantial revisions needed, re-review required after revision

### Confidence Score
5 — Completely within my area of expertise, I am very confident in my assessment

### Summary Assessment

This manuscript addresses an important and timely question for precision agriculture: whether satellite remote sensing adds value beyond simple thermal time accumulation for wheat biomass estimation. The study uses a rigorous leave-one-site-season-out cross-validation approach across four Mediterranean field-seasons in central Chile with 153 destructive samples. The main finding that accumulated growing degree days (∑GDD) alone reaches R² = 0.53 and outperforms most satellite-based models is sobering and valuable for the field. The best configuration (RF with Sentinel-1 + ∑GDD, R² = 0.58) provides only a 5.9% RMSE improvement that is not reproduced with XGBoost.

The paper's greatest strength is its methodological honesty and the explicit comparison against phenological baselines, which is rarely done in remote sensing studies. The finding that "most transferable information is captured by thermal time" challenges common assumptions in the field and has direct operational relevance.

However, major revisions are needed in three areas: (1) the TODO comment at line 218 indicates unresolved methodological uncertainty about predictor screening that could constitute data leakage; (2) the operational implications section is underdeveloped relative to the journal's applied focus; and (3) the fairness of the satellite sensor preprocessing choices needs stronger justification or acknowledgment of limitations. The conclusion that satellite data adds minimal value is potentially transformative for operational practice, but it must rest on a bulletproof methodological foundation.

---

## Strengths

### S1: Rigorous spatial cross-validation design
The use of leave-one-site-season-out cross-validation (LOSO-CV) is appropriate for testing model transferability to new locations and seasons. This validation scheme directly addresses the common problem of optimistic performance estimates from random or within-site splits.

**Evidence Anchor**: `text: Methods §2.3.4 "Performance was evaluated with LOSO-CV. The data were partitioned into four folds, each containing all samples of one site-season"`

### S2: Explicit phenological baseline comparison
The paper establishes and evaluates two phenological baselines (days after sowing, accumulated GDD) under the same cross-validation scheme as the satellite models. This allows for a fair assessment of whether satellite predictors add information beyond seasonal progress, which is rarely done in AGB estimation studies.

**Evidence Anchor**: `text: Introduction "Without a phenological baseline evaluated under the same validation scheme, it is not possible to tell whether satellite predictors add information beyond this seasonal trend"`

### S3: Operational relevance of the baseline finding
The discussion explicitly addresses the operational advantages of the ∑GDD baseline for field-level applications, noting that it requires only daily air temperature and sowing date, which are more accessible than satellite data preprocessing.

**Evidence Anchor**: `text: Discussion §4.3 "From an operational point of view, the ∑GDD baseline has important advantages: it requires only daily air temperature and the sowing date, and it can be computed for any field with a nearby weather station"`

### S4: Transparent reporting of limitations
The manuscript acknowledges key limitations including the small number of site-seasons (n=4), potential confounding of geography and wheat habit, and that the dataset was analyzed in earlier versions with different modeling choices.

**Evidence Anchor**: `text: Discussion §4.4 "The main limitation is the small number of independent units for evaluating transferability. With four site-seasons, each model is trained on three"`

### S5: Multi-sensor and multi-algorithm comparison
The paper compares three satellite missions (Sentinel-1, Sentinel-2, PlanetScope) with three algorithms (LM, RF, XGBoost) across 14 predictor sets, providing a comprehensive view of sensor contributions under fixed hyperparameters.

**Evidence Anchor**: `table: Table 5 — 32 model configurations evaluated with mean ± SD metrics across LOSO-CV folds`

---

## Weaknesses

### W1: Unresolved predictor screening uncertainty (data leakage risk)
**Problem**: The TODO comment at line 218 states "describe how the 43 candidate predictors in script 14 (`vars`) were screened from the full set of bands, indices, and cumulative indices, and confirm that AGB was not used in that screening. If AGB was used, the screening must be repeated inside the cross-validation folds." This is a critical methodological gap. If AGB was used to screen predictors before cross-validation, this constitutes data leakage and invalidates the performance estimates.

**Evidence Anchor**: `text: Methods §2.3.2, line 218 "<!-- TODO (authors): describe how the 43 candidate predictors in script 14 (vars) were screened from the full set of bands, indices, and cumulative indices, and confirm that AGB was not used in that screening. If AGB was used, the screening must be repeated inside the cross-validation folds. -->"`

**Why it matters**: Data leakage is a fundamental flaw that makes all reported performance metrics unreliable. Predictor selection using the response variable inflates apparent model performance because predictors are chosen for their association with the outcome. This is especially problematic for a paper whose main conclusion is that satellite predictors add minimal value; if that conclusion rests on leaked predictors, it would be reversed by proper methodology.

**Suggestion**: The authors must clarify exactly how the 43 predictors were selected from the larger set. If AGB or any LOSO-CV test-fold data was used, the entire analysis must be repeated with predictor screening performed within each training fold only, or the 53 VIs mentioned in line 169 must be used without screening. If predictor screening was AGB-independent (e.g., based only on literature, sensor availability, or correlation among predictors), this must be explicitly stated with the screening criteria documented.

**Severity**: Critical — this single issue, if not resolved satisfactorily, makes the paper's conclusions unreliable and prevents acceptance.

**Confidence**: 5 — core expertise: machine learning validation and data leakage detection

### W2: Insufficient operational decision context
**Problem**: The paper states that the best RMSE of 5.80 t/ha "corresponds to about 47% of the mean observed AGB, which is too coarse for field-level decisions such as variable-rate fertilization or insurance assessment" (Discussion §4.3). However, this blanket statement lacks operational context: what error level *would* be acceptable for these applications? Is 47% error typical for current operational systems? Are there intermediate use cases (e.g., regional monitoring, irrigation scheduling) where this accuracy would be sufficient?

**Evidence Anchor**: `text: Discussion §4.3 "The best RMSE obtained here (5.80 t/ha) corresponds to about 47% of the mean observed AGB, which is too coarse for field-level decisions"`

**Why it matters**: For a journal focused on agricultural engineering applications, readers need to understand whether the reported performance is "not yet good enough but promising" or "fundamentally insufficient for any foreseeable precision agriculture application." The current framing suggests the latter, but without benchmarks from operational systems or decision thresholds from agronomic literature, this claim is unsubstantiated.

**Suggestion**: Add 2-3 sentences contextualizing the 5.80 t/ha RMSE: (1) compare with error levels of current operational biomass/yield monitoring systems if available; (2) cite or estimate decision thresholds for specific applications (e.g., "variable-rate N application requires AGB estimates within ±X t/ha to optimize fertilizer use"); (3) identify any intermediate-precision use cases where the model could be operationally valuable (e.g., identifying severely stressed vs. healthy fields at the farm scale).

**Severity**: Major — materially weakens the paper's contribution to operational precision agriculture without addressing this context gap; substantial rewriting needed.

**Confidence**: 5 — core expertise: precision agriculture decision support systems

### W3: Justification for Sentinel-1 preprocessing choices
**Problem**: The manuscript acknowledges that Sentinel-1 data were used "without speckle filtering or incidence-angle normalization" and that this "may have reduced the contribution of SAR data" (Discussion §4.4). However, the paper does not justify why these standard SAR preprocessing steps were omitted. Since the main conclusion is that satellite data adds minimal value, and Sentinel-1 alone was the weakest predictor set (RMSE 10.92-12.19 t/ha), readers may question whether this conclusion would hold with proper SAR preprocessing.

**Evidence Anchor**: `text: Discussion §4.4 "Satellite predictors were extracted from the single pixel containing each sampling point, without speckle filtering or incidence-angle normalization for Sentinel-1, which may have reduced the contribution of SAR data"`

**Why it matters**: Speckle noise is a well-known characteristic of SAR data that degrades the signal-to-noise ratio, and multi-look speckle filtering is standard preprocessing for agricultural applications. Incidence angle affects backscatter intensity, and normalization is commonly applied for multi-temporal analysis. Omitting these steps without justification raises the question of whether the study is evaluating what satellite data *can* do under best practices, or what happens with minimal preprocessing.

**Suggestion**: Either (1) justify the preprocessing choices based on computational constraints, data availability, or experimental design (e.g., "to test the value of freely available, minimally processed data accessible to farmers"); (2) present a sensitivity analysis showing that speckle filtering does/does not improve transferability on a subset of the data; or (3) acknowledge more explicitly in the abstract, conclusion, and implications that the findings apply specifically to minimally preprocessed satellite data and that different conclusions might emerge with optimized SAR preprocessing.

**Severity**: Major — the paper's core claim about satellite data adding minimal value may not generalize to properly preprocessed SAR data; substantial discussion revision needed.

**Confidence**: 4 — adjacent field: applying general remote sensing standards rather than SAR-specific expertise

### W4: Limited discussion of sensor-specific transferability patterns
**Problem**: Table 5 and Figure 4 show that different predictor sets have markedly different per-fold transferability (e.g., PlanetScope SD of RMSE > 4 t/ha), but the text does not explore *which* site-seasons each sensor failed on or why. The sequential ablation (Table 6) shows Sentinel-1's contribution sign depends on the algorithm, but there is no hypothesis about why.

**Evidence Anchor**: `table: Table 5 — PlanetScope alone: RMSE = 8.57 ± 4.15 t/ha (SD larger than many models' mean); Sentinel-1+GDD with XGBoost has +4.3% ∆RMSE while RF has −5.9%`

**Why it matters**: For operational users, understanding *when* and *why* satellite data fails to transfer is as important as knowing the average performance. If PlanetScope fails specifically in one site-season due to the Dove/SuperDove transition, or if Sentinel-1 helps in water-limited seasons but not others, this would inform sensor selection for specific contexts.

**Suggestion**: Add 1-2 paragraphs to the Discussion analyzing per-fold performance patterns. Use Supplementary Table S2 (per-fold results) to identify which site-seasons were outliers for which sensor. Propose hypotheses tied to field conditions (e.g., did Sentinel-1 help more in drier site-seasons? did PlanetScope's high variance occur in the seasons with sensor transitions?). If no clear pattern emerges, state this explicitly as a finding ("sensor transferability was not systematically related to observable field characteristics, suggesting...").

**Severity**: Major — missing operational guidance that would help practitioners decide when satellite data is worth collecting.

**Confidence**: 5 — core expertise: agricultural remote sensing for decision support

---

## Detailed Comments

### Title & Abstract
- The title clearly conveys the paper's question and approach. The phrase "how much do satellite sensors add to thermal time" immediately signals the baseline comparison framework.
- The abstract is comprehensive and well-structured, stating the methods (LOSO-CV, 153 samples, 14 predictor sets, 3 algorithms), main findings (∑GDD R²=0.53 outperforms single sensors; best RF S1+GDD R²=0.58 with 5.9% gain), and the key implication (most transferable signal is thermal time). The final sentence ("AGB studies should report phenological baselines...") is a strong, actionable recommendation.
- Minor: The abstract does not mention that the 5.9% gain with RF+S1+GDD was not reproduced with XGBoost, which is important for assessing robustness. Consider adding "but this gain was not reproduced with XGBoost" after "mean RMSE reduction of 5.9%".

### Introduction
- The introduction establishes the motivation clearly: global food security pressures, the need for AGB monitoring, and the proliferation of satellite-based estimation methods.
- The literature review covers key sensor types (multispectral, SAR, LiDAR, hyperspectral) and ML approaches, with appropriate recent citations.
- The gap identification is excellent: "Without a phenological baseline evaluated under the same validation scheme, it is not possible to tell whether satellite predictors add information beyond this seasonal trend" (lines 56-57). This is the paper's core contribution.
- The three objectives are clear and well-motivated. Objective (i) establishes the baselines, (ii) compares sensors, and (iii) interprets via permutation importance and ALE.
- The disclosure about the companion letter (lines 60-61) is transparent and appropriate, noting that "some sensor-level conclusions therefore differ" due to methodological adjustments.

### Materials and Methods

#### Study Area
- The description of the four site-seasons is clear, with useful metadata (location, wheat variety, soil type, climate). The choice of spring vs. winter wheat adds valuable diversity to the transferability test.
- Figure 1 effectively shows the field boundaries and sampling point layouts.

#### Field Data
- The AGB measurement protocol is standard and well-described (0.25 m² harvest, 70°C drying, conversion to t/ha). The temporal distribution across phenological stages (Figure 2) is well-visualized.
- The soil moisture measurement approach is clearly described. The acknowledgment that each field has a single sensor profile (so SM varies in time but is identical for all points within a field, line 82) is an important limitation for within-field transferability.

#### Weather Data
- The GDD calculation is clearly described with appropriate base temperatures (2°C early, 6°C reproductive, citing Del Pozo 1987). The choice of base temperatures is agronomically grounded.
- Table 1 lists the meteorological stations with distances to fields (1.03 to 20.89 km). The 20.89 km distance for Villa Baviera is noted but not discussed as a potential source of error.

#### Remote Sensing Data
- The description of Sentinel-1, Sentinel-2, and PlanetScope missions is comprehensive with sensor specifications in Table 2.
- The PlanetScope sensor transition (Dove to SuperDove) and the exclusion of red edge predictors for two site-seasons (lines 156-158) is clearly documented. The decision to exclude four PS predictors that were entirely missing for two site-seasons is appropriate for avoiding imputation artifacts.
- The image counts per site-season are documented (185 S1, 208 S2, 180 PS total).

#### Preprocessing
- The preprocessing steps are clearly described for each sensor. For Sentinel-2 and PlanetScope, quality masking using SCL and UDM is appropriate.
- The temporal interpolation approach differs by sensor: S1 uses step-function extension (duplicating each acquisition until the midpoint to the next), while S2 and PS use Kalman smoothing. The acknowledgment that Kalman smoothing uses observations both before and after each date, making it suitable for in-season estimation but requiring a causal filter for real-time applications (lines 164-166), is an important caveat for operational use.
- **Critical limitation acknowledged but not resolved**: "Satellite predictors were extracted from the single pixel containing each sampling point, without speckle filtering or incidence-angle normalization for Sentinel-1" (Discussion §4.4). This is noted as a limitation but the Methods section does not explain why these standard steps were omitted. See Weakness W3.

#### Vegetation Indices
- Table 3 lists 21 retained VIs with formulas and references. The selection is appropriate for wheat AGB estimation.
- **Unresolved**: The TODO comment at line 218 indicates that the screening process from 53 candidate VIs to the final 43 predictors in Table 4 is not documented. This must be resolved (see Weakness W1).

### Modeling Framework

#### Overview
- Figure 3 provides a clear flowchart of the modeling approach. The visual clearly shows the LOSO-CV structure and the 14 predictor sets.

#### Predictor Sets
- Table 4 comprehensively lists the 14 predictor sets and the 39 candidate predictors (after excluding 4 PS red edge predictors). The rationale for the sets is clear: two baselines, weather, single sensors, sensor+GDD, sensor+weather, and all predictors.
- The sequential ablation (W, S2+W, S2+W+S1, All) reproduces the companion letter's design for comparability.

#### Preprocessing
- The within-fold preprocessing pipeline is appropriate: predictors outside each set removed first, then k-NN imputation, centering/scaling, correlation filter (|r| ≥ 0.9), zero-variance removal.
- The acknowledgment that the correlation filter was estimated on each training fold, so retained predictors differed slightly among folds (lines 253-255), is transparent. The example that ∑GDD was removed in the La Cancha fold due to high correlation with cumulative indices is important for interpreting that fold's results.
- Soil moisture was the only predictor with missing values (12.5-33% per site-season), which is reasonable given sensor installation logistics.

#### Models and Cross-Validation
- Fixed hyperparameters (not tuned) are clearly justified: "Using fixed hyperparameters means that the held-out site-season is never used to select a model configuration, which avoids the optimistic bias" (lines 258-260). This is methodologically sound for a small-sample transferability study.
- The hyperparameter values are documented (RF: 1000 trees, min node size 5, min(7,p) predictors per split; XGBoost: 500 trees, depth 4, lr 0.05, min node size 5, row subsample 0.8, min(7,p) predictors). These are reasonable defaults.
- The LOSO-CV scheme is clearly described. The choice to report per-fold results (Supplementary Table S2) in addition to mean±SD is excellent for transparency.
- The decision to compute metrics with and without AGB=0 samples (21 of 153) is appropriate and reported in Results §3.2.

#### Explainability
- The use of permutation importance and ALE profiles from DALEX is appropriate. The acknowledgment that permuting one of several correlated predictors spreads importance across the group (citing Strobl 2008) is methodologically sophisticated.
- The decision to compute group-level importance (permuting all predictors of a sensor jointly) addresses the correlated-predictor issue.

### Results

#### Observed AGB and Baselines
- The observed AGB range (0 to 43.5 t/ha, mean 12.2) is clearly documented with per-site-season maxima. The note that seven samples above 30 t/ha all came from two site-seasons is important context for the underprediction of high AGB.
- The ∑GDD baseline performance (R²=0.53, RMSE=6.24 t/ha) is clearly reported with per-fold details. The finding that it outperformed DAS (R²=0.19, RMSE=7.85 t/ha) demonstrates the value of thermal time over calendar time.
- The poor performance of the weather set (W: RMSE=7.01 to 7.55 t/ha, worse than ∑GDD alone) is surprising and worth more discussion. Why doesn't adding precipitation and soil moisture help?

#### Satellite Predictors
- Table 5 is comprehensive and clearly formatted, with mean±SD metrics, ∆RMSE relative to the ∑GDD baseline, and the number of folds in which each configuration beat the baseline.
- The finding that single sensors transferred worse than the ∑GDD baseline is clearly stated. The Sentinel-1 alone result (RMSE=10.92 to 12.19 t/ha) is surprisingly poor and deserves more discussion of potential causes.
- The S1+GDD with RF result (R²=0.58, RMSE=5.80 t/ha) is the best overall and improved on the baseline in 3 of 4 folds. The acknowledgment that XGBoost with the same predictors did not improve (RMSE=6.42 t/ha, +4.3%) is important for assessing robustness.
- The sequential ablation (Table 6) shows inconsistent sensor contributions: S2 +3.7% (RF) or +0.2% (XGBoost); S1 −3.6% (RF) or +4.3% (XGBoost); PS −9.7% (RF) or −7.2% (XGBoost). The text notes these are smaller than the between-fold SD (2.5 to 4.1 t/ha), which appropriately cautions against overinterpreting.
- Figures 5 and 6 effectively visualize observed vs. predicted AGB and temporal trajectories. The compression of AGB range (underestimating high values, overestimating low-to-mid values in Hidango 2022-2023) is a common ML regression pattern.

#### Predictor Importance and Effects
- Figure 7 shows that permuting the PlanetScope group increased held-out RMSE by 1.36 t/ha (range −0.13 to 2.53), S2 by 0.99 t/ha, and ∑GDD by 0.93 t/ha. The S1 group had almost no effect (0.07 t/ha), which is inconsistent with S1+GDD being the best predictor set. This deserves discussion: does the "All" model's high predictor count (39) dilute S1's contribution?
- At the individual level, ∑GDD ranked first, followed by cumulative indices (PS GRVI, PS EVI, S2 NDRE3*, PS CI_red). The interpretation that cumulative indices behave as "measures of season progress" is supported by the monotonic ALE profiles in Figure 8.
- Figure 8 clearly shows the ALE profiles for the six most important predictors. All are monotonic or nearly so, consistent with the interpretation that they track seasonal development rather than condition.

### Discussion

#### Thermal Time as Main Signal
- The discussion clearly interprets the finding that ∑GDD carried most transferable information. The physiological explanation ("AGB accumulates with thermal time through most of the season") is appropriate.
- The comparison with UAV/hyperspectral studies (R²=0.81 to 0.91) appropriately attributes the difference to validation scheme (random/within-site splits vs. spatial CV).
- The recommendation that "studies of crop AGB estimation report a thermal-time or phenological baseline evaluated under the same validation scheme" is strong and actionable.

#### Satellite Predictors
- The interpretation that Sentinel-1's modest gain may be due to C-band backscatter responding to canopy structure and water content (citing Veloso 2017) is plausible. The caution that the gain disappeared with XGBoost and was smaller than the between-fold SD is appropriate scientific hedging.
- The interpretation that optical cumulative indices "acted mainly as alternative measures of season progress" is supported by the ALE profiles.
- The acknowledgment of PlanetScope's Dove/SuperDove transition and the imputation of red edge predictors is transparent. The statement that excluding red edge predictors "removes the most obvious source of such inconsistency" (line 390) is appropriate.
- The discussion of differences from the companion letter (lines 391-392) is transparent and appropriate.

#### Implications
- **Needs strengthening**: See Weakness W2. The statement that 5.80 t/ha RMSE is "too coarse for field-level decisions" lacks operational context.

#### Limitations
- The limitations section is comprehensive and transparent, acknowledging the small number of site-seasons (n=4), confounding of geography and wheat habit, analysis of the dataset in earlier work, fixed hyperparameters, predictor screening uncertainty, SAR preprocessing choices, single-profile soil moisture, and AGB values above 30 t/ha.
- The acknowledgment that "results excluding [AGB=0 samples] are reported in Section 3.2" is appropriate transparency.

### Conclusion
- The conclusion clearly summarizes the three main findings: (1) ∑GDD alone reached R²=0.53 and outperformed DAS and single sensors; (2) the best configuration (RF with S1+GDD) reached R²=0.58 with a 5.9% gain not reproduced by XGBoost; (3) most important optical predictors were cumulative indices tracking season progress.
- The five future work recommendations are specific and appropriate: larger sample size, gridded temperature products, improved S1 preprocessing, nested CV for hyperparameter tuning, and independent harvest samples for forecasting.

---

## Questions for Authors

1. **Predictor screening methodology (Critical)**: Line 218 contains a TODO asking authors to "describe how the 43 candidate predictors in script 14 (`vars`) were screened from the full set of bands, indices, and cumulative indices, and confirm that AGB was not used in that screening." Please provide a complete description of the screening process. If AGB or any LOSO-CV test-fold data was used, how do you plan to address the data leakage issue?

2. **Operational decision thresholds**: You state that 5.80 t/ha RMSE is "too coarse for field-level decisions such as variable-rate fertilization or insurance assessment." What error level would be acceptable for these applications based on agronomic decision thresholds? Are there any intermediate-precision use cases (e.g., regional monitoring, irrigation timing) where the reported accuracy would be sufficient?

3. **Sentinel-1 preprocessing rationale**: Why were speckle filtering and incidence-angle normalization omitted for Sentinel-1? Was this a deliberate design choice to test minimally processed data, or a practical constraint? If you were to apply standard SAR preprocessing, do you expect the conclusion that "satellite data adds minimal value" would still hold?

4. **Per-fold failure patterns**: Table 5 and Supplementary Table S2 show large differences in which site-seasons each sensor failed on. Did you analyze whether sensor performance correlated with observable field characteristics (water stress, cloud cover, sensor transitions)? If so, what patterns emerged? If not, would this analysis strengthen the operational guidance?

---

## Minor Issues

### Language / Grammar
- Line 30, "classoption: ["review", "number", "sort,compress"]" — the "review" class option typically produces double-column format with line numbers. Confirm this is the intended submission format for *Computers and Electronics in Agriculture*.
- Line 295, Table 5: "S1 & RF & $-$0.40 $\pm$ 0.69 & 10.92 $\pm$ 2.18" — the negative R² should be briefly explained in the table caption or footnote for readers unfamiliar with this metric (R²<0 when model predicts worse than the mean).

### Citation Format
- Citations generally follow Elsevier-Harvard style appropriately.
- Line 60: the companion letter is cited as "[@Zambrano2026]" but the references.bib entry should be checked to ensure it's formatted as "in review" or "submitted" rather than a published year.

### Figures and Tables
- Table 2: The wavelength for Sentinel-1 (5.546e7 nm ≈ 5.55 cm) is presented in scientific notation for consistency with optical sensors, but the caption note explaining this is helpful.
- Table 6: The header row has "LM & Linear Model..." text repeated in rows 347, 350, 353, 356, which appears to be a LaTeX compilation error. Please check the .tex output.
- Figure 3: The flowchart is clear. Consider adding a legend or caption note explaining the abbreviations (LOSO-CV, DALEX, ALE) for readers less familiar with ML terminology.
- Figure 7: The caption states "Points show the mean increase in held-out RMSE" but does not define what the bars represent. Please add "and bars the range across folds" to the caption.

### Layout
- The manuscript is well-formatted overall. Ensure that line numbers are present in the submitted version if required by the journal's review format.

---

## Criterion-Bound Judgements

Calibration status: `NOT_CALIBRATED`

| Dimension | Criterion source | Judgement | Evidence anchor(s) | Rationale | Uncertainty / scope limit | Decision bearing? |
|---|---|---|---|---|---|---|
| Originality | General | MEETS | text: Introduction "Without a phenological baseline evaluated under the same validation scheme, it is not possible to tell whether satellite predictors add information beyond this seasonal trend" | The explicit comparison against thermal time baselines under the same spatial CV scheme is novel in the wheat AGB remote sensing literature. The finding that satellite data adds minimal value challenges common assumptions. | Limited to Mediterranean wheat systems; generalizability to other crops/climates uncertain | No (strengths offset the TODO concern) |
| Methodological Rigor | General | PARTLY_MEETS | text: Line 218 "<!-- TODO (authors): describe how the 43 candidate predictors ... -->"; text: §2.3.4 "Using fixed hyperparameters means that the held-out site-season is never used to select a model configuration" | The LOSO-CV design and fixed hyperparameters are rigorous. However, the unresolved predictor screening process (line 218 TODO) introduces potential data leakage risk that must be addressed. | Predictor screening must be clarified before full rigor can be assessed | Yes (Critical if leakage confirmed) |
| Evidence Sufficiency | General | PARTLY_MEETS | text: Results §3.1 "Under LOSO-CV, the linear model on ∑GDD alone reached R² = 0.53 ± 0.34"; table: Table 5 — 32 model configurations with per-fold metrics | Comprehensive evaluation across 14 predictor sets and 3 algorithms with per-fold reporting. Limited by small number of site-seasons (n=4), acknowledged in limitations. | Four site-seasons may not capture full range of conditions where satellite data would add value | Yes (Major) |
| Argument Coherence | General | MEETS | text: Discussion §4.1 "Accumulated thermal time carried most of the information about wheat AGB that transferred to site-seasons not used for training" | The argument flows logically from physiological mechanisms to empirical findings to operational implications. The interpretation that cumulative optical indices track season progress is well-supported by ALE profiles. | None | No |
| Writing Quality | General | MEETS | text: throughout | The manuscript is clearly written with appropriate technical detail, transparent limitations, and minimal grammatical errors. The abstract effectively summarizes the study. | None | No |
| Literature Integration | General | MEETS | text: Introduction §1 — cites 70+ references spanning crop growth models, remote sensing methods, spatial CV, and thermal time | The literature review appropriately covers remote sensing approaches (multispectral, SAR, LiDAR, hyperspectral), ML methods, and spatial cross-validation. The gap identification (lack of phenological baselines) is well-positioned. | Limited engagement with crop physiology literature on GDD-biomass relationships | No |
| Significance & Impact | General | MEETS | text: Discussion §4.1 "We therefore recommend that studies of crop AGB estimation report a thermal-time or phenological baseline evaluated under the same validation scheme" | The recommendation to report phenological baselines could shift methodological norms in the field. The operational finding that ∑GDD is sufficient for many applications has practical value. | Significance depends on resolving the TODO and preprocessing concerns | Yes (Major) |

**Recommendation rationale**: The paper addresses an important question with a rigorous validation design and transparent reporting. The finding that thermal time outperforms most satellite-based models is valuable for operational practice. However, **three decision-bearing criteria are only partly met**: (1) **Methodological Rigor** is undermined by the unresolved predictor screening process (line 218 TODO), which poses a Critical data leakage risk; (2) **Evidence Sufficiency** is limited by the small number of site-seasons (n=4), a Major limitation acknowledged by authors; and (3) **Significance & Impact** depends on whether the satellite preprocessing choices (no speckle filtering for S1, Dove/SuperDove transition for PS) are adequately justified or acknowledged, a Major concern for generalizability. Resolving the TODO is mandatory for acceptance. The operational context and SAR preprocessing issues require substantial discussion revision. With these revisions, the paper would make a strong contribution to *Computers and Electronics in Agriculture*.
