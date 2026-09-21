# R1 Completion Summary — Statistical Inference with n=4

**Date**: 2026-09-18  
**Required Revision**: R1 — Statistical inference with n=4 understated  
**Status**: ✅ COMPLETED

---

## Changes Made

### 1. Expanded Discussion §4.3 (Study Limitations)

**Location**: Lines 405-407  
**Before**: 3 sentences (brief mention)  
**After**: 8 sentences (comprehensive discussion)

**Key additions**:
- Explicit statement: "With n=4, most differences between configurations are within one standard deviation of the mean and may not represent true performance differences."
- Statistical power limitation explained: "severely limiting statistical power to distinguish among configurations"
- Noise vs signal: "most apparent performance differences are within the noise of the cross-validation procedure"
- No formal testing possible: "No formal statistical test can reliably separate configurations under these conditions"
- Ranking uncertainty: "the ranking of models should be treated as highly uncertain"
- Scope limitation: "Statements such as 'X was the best configuration' refer only to performance in this particular sample of four site-seasons and should not be generalized"
- Geographic/variety/soil confounding expanded

### 2. Softened Ranking Claims Throughout

**Abstract** (line 13):
- "The best configuration" → "The best-performing configuration in this sample"
- Added: "With only four site-seasons, statistical power was limited"

**Conclusion** (line 413):
- "The best configuration" → "The best-performing configuration in this sample"

**Results** (line 330):
- "was the best configuration overall" → "performed best in this sample"

**Discussion** (lines 385, 389, 401):
- "The performance of the best configuration" → "The performance of the best-performing configuration in this sample"
- "The best configuration combined" → "The best-performing configuration in this sample combined"
- "The best RMSE obtained here" → "The lowest RMSE obtained in this sample"
- "performed best" → "performed best in this sample"

### 3. Updated Other Limitation Text

**Line 409**: Updated predictor count and screening description
- "The 43 candidate predictors were screened from a larger set" → 
- "The 54 candidate predictors were selected from the literature based on their demonstrated utility for crop biomass estimation, and highly correlated predictors (r > 0.9) were filtered independently within each cross-validation training fold to avoid data leakage."

---

## Acceptance Criterion Met

✅ **Discussion §4.3 expanded to ≥5 sentences** (now 8 sentences)  
✅ **Explicit statement about n=4 implications** included  
✅ **Ranking uncertainty acknowledged** throughout  
✅ **Strong claims qualified** ("best" → "best in this sample")

---

## Editorial Decision Requirements

### From R1 Editorial Decision:

**Required Fix**:
- [x] Expand Discussion §4.3 from 2 sentences to full paragraph
- [x] State explicitly: "With n=4, most differences between configurations are within one standard deviation of the mean and may not represent true performance differences."
- [x] Avoid strong ranking claims ("X was best") — use "X performed well in this sample"

**Note**: The editorial decision also suggested "Add to Results: Report SD alongside means in all tables"

SD values are already reported in Table 5 and throughout Results section (e.g., "R² = 0.58 ± 0.34, RMSE = 5.80 ± 1.95 t/ha"), so this requirement was already met.

---

## Summary

R1 has been fully addressed through:
1. **Substantial expansion** of the n=4 limitation discussion (3→8 sentences)
2. **Explicit statements** about statistical power, noise, and ranking uncertainty
3. **Comprehensive qualifying** of all ranking claims throughout the manuscript
4. **Clear scope statements** that results apply to this specific sample only

The manuscript now appropriately acknowledges the severe limitations imposed by having only 4 site-seasons for cross-validation, and all performance claims are properly qualified.

---

**Completed**: 2026-09-18  
**Next Required Revision**: R2 (Operational decision context)
