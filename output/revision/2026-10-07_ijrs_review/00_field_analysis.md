# Field Analysis Report (Phase 0)

Review mode: `full`. Target journal stated by the author in session: International Journal of Remote Sensing (IJRS), research article. No #683 ReviewTargetContext / criteria binding manifest was supplied, so every seat discloses `criteria_binding_unavailable`; journal-fit comments rely on the IJRS formatting guidelines (files.taylorandfrancis.com/style_tres.pdf) and general knowledge of the journal, and make no claim of formal venue-criteria alignment.

## Paper basic information
- **Title**: Thermal time baselines for evaluating Sentinel-1, Sentinel-2 and PlanetScope predictors of wheat above-ground biomass under spatial cross-validation
- **Abstract length**: about 287 words
- **Full text length**: about 10 500 words including tables and captions (references excluded)
- **Number of references**: 83
- **Files**: `manuscript/ijrs/manuscript_ijrs.qmd`, `manuscript/ijrs/frontmatter.tex`, `manuscript/ijrs/supplementary_ijrs.qmd`, rendered `manuscript_ijrs.pdf`, `supplementary_ijrs.pdf`

## Field analysis

| Dimension | Result |
|---|---|
| Primary discipline | Agricultural remote sensing (crop biophysical variable retrieval) |
| Secondary disciplines | Spatial machine learning and model validation; crop physiology / agrometeorology (thermal time); SAR remote sensing of vegetation |
| Research paradigm | Quantitative |
| Methodology type | Statistical modelling / machine learning; comparative ablation under grouped (leave-one-site-season-out) cross-validation; model interpretation (permutation importance, ALE) |
| Target journal tier | IJRS: Q2 broad remote sensing applications journal (IF about 2.8 to 3.1), author-stated |
| Paper maturity | Pre-submission (rejected at European Journal of Agronomy, reframed and reformatted for IJRS; complete structure, T&F Interact layout, supplementary material) |

## Recommended target journals (top 3, informational only)
1. International Journal of Remote Sensing: author-stated target; applications scope fits the evaluation-protocol message.
2. Precision Agriculture: field-scale sensing plus agronomy readership.
3. Remote Sensing Applications: Society and Environment: applied validation studies.

## Reviewer Configuration Cards

### Card #1
**Role**: EIC
**Display role**: Journal-Fit Reviewer
**Identity**: Associate Editor of IJRS handling agricultural applications, background in optical and SAR crop monitoring, handles many ML-biomass submissions per year.
**Review focus**:
1. Whether the contribution (phenological baseline + spatial validation as an evaluation protocol) is novel enough for IJRS readers, beyond a negative local case study.
2. Whether the remote sensing component (sensor processing, SAR treatment) meets the journal's technical bar.
3. Length, structure, and IJRS conventions (style, figures, units, references).
**Will particularly care about**: whether a four-field study can support general recommendations for the field.
**Possible blind spots**: statistical detail of the cross-validation design.

### Card #2
**Role**: Peer Reviewer 1 (Methodology)
**Identity**: Statistician working on spatial cross-validation and model transferability for environmental ML (area of applicability, grouped CV, nested resampling).
**Review focus**:
1. LOSSO-CV design with four folds: statistical power, the use of mean ± SD over four folds, absence of formal tests, and the "differences within SD" logic.
2. Information leakage: first-stage correlation screening on all samples, within-fold step_corr removing ∑GDD in two folds, KNN imputation of soil moisture, two-sided Kalman smoothing.
3. Fairness of comparisons: LM baseline vs tree models, fixed hyperparameters, same-algorithm ∑GDD controls, R² definition, exclusion of AGB = 0 samples, permutation importance and ALE on correlated predictors.
**Will particularly care about**: whether the headline ranking (S1+GDD RF best) is distinguishable from noise.
**Possible blind spots**: SAR physics and agronomic plausibility.

### Card #3
**Role**: Peer Reviewer 2 (Domain)
**Identity**: Senior researcher in SAR and optical crop monitoring (Sentinel-1/Sentinel-2 wheat biomass, LAI and phenology retrieval), familiar with the cereal backscatter literature.
**Review focus**:
1. Sentinel-1 preprocessing (GRD from GEE, mixed relative orbits, no speckle filter, no incidence angle normalisation, single pixel, S1B loss) and whether conclusions about S1 are fair.
2. Optical processing: PlanetScope Dove/SuperDove harmonisation, S2 VI choice, cumulative indices, Kalman smoothing.
3. Literature coverage: wheat AGB retrieval, S1+S2 fusion, spatial CV in remote sensing, phenology-based baselines, recent references.
**Will particularly care about**: whether the poor S1 and PS transfer reflects sensor limits or processing choices.
**Possible blind spots**: statistical inference with four folds.

### Card #4
**Role**: Peer Reviewer 3 (Cross-disciplinary / practical)
**Identity**: Crop physiologist and crop modeller (APSIM/radiation use efficiency, thermal time phenology) who works with insurance and advisory services.
**Review focus**:
1. Agronomic validity: GDD base temperatures, AGB values above 30 t ha⁻¹ (implausible for wheat?), winter vs spring wheat confounding, sampling design (within-season progressions).
2. Whether a crop model or RUE approach would be the more relevant baseline, and how satellite data should be assimilated.
3. Practical value: errors of about 47 to 51% of mean AGB, operational use, gridded temperature, real-time causal filtering.
**Will particularly care about**: whether "thermal time dominates" is a remote sensing finding or a known agronomic fact restated.
**Possible blind spots**: SAR processing specifics.

### Seat #5 (fixed)
Devil's Advocate: no configuration card; challenges the core argument, cherry-picking, overgeneralisation, alternative explanations.

## Review strategy recommendations
- The paper is a negative/benchmark result; reviewers should judge whether the evaluation protocol is the contribution and whether four site-seasons can carry it.
- Tension expected between R1 (statistical caution) and R3 (agronomic triviality of the thermal time result).
- Session note: the reviewing assistant co-authored the IJRS reframing in this session. To limit self-review bias, each Phase 1 seat runs as a fresh subagent with no access to this conversation and no view of peer outputs.
