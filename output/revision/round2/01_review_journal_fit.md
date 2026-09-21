# Review 1: Journal-Fit Reviewer

**Role**: Associate Editor, *Computers and Electronics in Agriculture*  
**Expertise**: Agricultural systems engineering, remote sensing applications in agriculture  
**Recommendation**: **MAJOR REVISION**  
**Confidence**: 5/5 (High)

---

## Summary

This manuscript evaluates multi-sensor satellite data contribution to wheat AGB estimation against a thermal time baseline using rigorous LOSO-CV. The core finding—that ∑GDD captures most transferable signal—is novel and valuable. The phenological baseline comparison fills an important gap. However, **critical methodological issues must be resolved**, particularly undocumented predictor screening (line 218 TODO) that may invalidate cross-validation results.

---

## Strengths

1. **Novel phenological baseline approach** — Comparing satellite models against ∑GDD is uncommon and provides essential context
2. **Rigorous spatial validation** — LOSO-CV with fixed hyperparameters correctly tests transferability
3. **Transparent limitations** — Small sample size, confounding acknowledged
4. **Clear operational implications** — Recommendation to report phenological baselines is actionable
5. **Strong reproducibility** — Code and data openly available

---

## Critical Issues (Must Fix)

### C1. Undocumented Predictor Screening (Line 218)

**Evidence**: Line 218 TODO comment about screening procedure  
**Problem**: Potential data leakage invalidates all CV results if AGB was used for screening  
**Location**: §2.3.2, Table 4  
**Required**: Document screening; if AGB used, re-run with screening inside CV folds

### C2. SAR Preprocessing Justification Missing

**Evidence**: No speckle filtering or incidence angle normalization (lines 162-163)  
**Problem**: Omission may explain S1's poor performance; can't tell if sensor or preprocessing issue  
**Location**: §2.2.4, §4.4  
**Required**: Justify omission, OR reprocess, OR state limitation prominently in Methods

### C3. Linear vs. Nonlinear Model Confound

**Evidence**: ∑GDD uses LM, satellites use RF/XGBoost  
**Problem**: Can't tell if improvement is from satellites or algorithm nonlinearity  
**Location**: §2.3.4, Table 5  
**Required**: Test RF on ∑GDD alone, OR acknowledge confound prominently

---

## Major Issues (Must Address)

**M1. Operational Context Missing** — "Too coarse" needs quantitative thresholds  
**M2. Fold-Specific Correlation Filter** — Behavior not documented  
**M3. Small Sample Size (n=4)** — Implications understated  

---

## Minor Issues (Consider)

- PlanetScope sensor transition not tested
- High AGB values (>30 t/ha) unexplained
- Within-season sampling may favor GDD

---

## Recommendation

Novel finding with rigorous methods, but **three critical issues block acceptance**. With revisions, this will be a strong contribution to *Computers and Electronics in Agriculture*.

**Decision**: Major Revision
