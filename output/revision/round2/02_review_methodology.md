# Review 2: Methodology Reviewer

**Role**: Associate Professor of Agricultural Statistics  
**Expertise**: Spatial cross-validation, machine learning validation, experimental design  
**Recommendation**: **MAJOR REVISION**  
**Confidence**: 5/5 (High)

---

## Summary

LOSO-CV design is appropriate for testing spatial transferability. Fixed hyperparameters correctly avoid overfitting. Within-fold preprocessing is methodologically sound. However, **critical concerns about predictor screening and statistical inference with n=4 must be addressed**.

---

## Strengths

1. Appropriate validation strategy (LOSO-CV with grouped folds)
2. Fixed hyperparameters avoid overfitting (correct for n=4)
3. Within-fold preprocessing (correlation filter, imputation, scaling on training only)
4. Transparent reporting (per-fold results, SD alongside means)
5. Traditional R² metric computed correctly

---

## Critical Issues

### C1. Predictor Screening Undocumented

**Evidence**: Line 218 TODO  
**Concern**: If predictors selected using AGB correlation in full dataset → data leakage  
**Required**: Document procedure; if AGB used, move screening inside CV

### C2. Statistical Inference with n=4 Understated

**Evidence**: Only 4 folds; Table 5 SDs (1-4 t/ha) exceed mean differences  
**Concern**: Power to detect real differences very low; ranking highly uncertain  
**Required**: Expand §4.4 discussion; emphasize tentative ranking

### C3. Linear vs. Nonlinear Confound

**Evidence**: ∑GDD (LM) vs. S1+GDD (RF/XGBoost)  
**Concern**: Improvement could be S1 information OR RF's nonlinearity on GDD  
**Required**: Test RF on GDD alone OR acknowledge confound

---

## Major Issues

**M1. Correlation Filter Behavior Poorly Explained**  
- Which predictor of correlated pair removed? (First? Higher variance?)
- How many predictors varied across folds?
- La Cancha fold: ∑GDD removed—affect comparability?

**M2. Imputation Rate Not Justified**  
- k-NN: k value not stated
- SM 33% missing—substantial; bias possible
- No sensitivity analysis

**M3. AGB=0 Samples Handled Inconsistently**  
- Main results include 21 AGB=0 samples
- Supplementary excludes them
- Why two versions?

**M4. Hyperparameter Values Not Justified**  
- Values reasonable but not justified
- Companion letter citation insufficient

---

## Minor Issues

- Negative R² explanation terse
- Permutation importance details sparse
- ALE computed on full dataset (not out-of-fold)
- Per-fold variability visualization missing

---

## Statistical Assessment

**CV mechanics**: ✓ Correct  
**Metrics**: ✓ Appropriate  
**Reporting**: ✓ Transparent  
**Power**: Low (n=4)  
**Confounding**: Algorithm × predictor not acknowledged

---

## Recommendation

Validation strategy is sound. Authors avoid common pitfalls. However, **three critical concerns block acceptance**: predictor screening unknown, n=4 implications understated, algorithm confounded with predictors.

**Decision**: Major Revision
