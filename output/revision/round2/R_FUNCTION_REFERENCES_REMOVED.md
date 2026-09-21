# R Function References Removed from Manuscript

**Date**: 2026-09-21  
**Action**: Replaced R-specific function names with plain language descriptions  
**Rationale**: Improve accessibility for readers unfamiliar with R/tidymodels

---

## Changes Made

### 1. Manuscript - Methods §2.3.2 (Line ~218)

**BEFORE**:
> "Within each LOSO-CV training fold, highly correlated predictors (Pearson r > 0.9) were removed via step_corr() to reduce multicollinearity."

**AFTER**:
> "Within each LOSO-CV training fold, highly correlated predictors (Pearson r > 0.9) were removed to reduce multicollinearity."

**Change**: Removed `step_corr()` function reference

---

### 2. Manuscript - Methods §2.2.4 (Line ~163)

**BEFORE**:
> "Extraction was performed at each sampling point's coordinates as the value of the raster cell containing the point (nearest-pixel extraction via the {terra} package, with no spatial averaging over a buffer);"

**AFTER**:
> "Extraction was performed at each sampling point's coordinates as the value of the raster cell containing the point (nearest-pixel extraction, with no spatial averaging over a buffer);"

**Change**: Removed `{terra}` package reference (kept method description)

---

### 3. Manuscript - Software Section (Line ~268)

**BEFORE**:
> "To access satellite data, we employed the {earthdatalogin} package [@Boettiger2023]. For smoothing the time series of vegetation indices, we applied the {imputeTS} package [@Moritz2017]. We used the {terra} package [@Hijmans2023] and the {sf} package [@Pebesma2018]..."

**AFTER**:
> "To access satellite data, we employed the earthdatalogin package [@Boettiger2023]. For smoothing the time series of vegetation indices, we applied the imputeTS package [@Moritz2017]. We used the terra package [@Hijmans2023] and the sf package [@Pebesma2018]..."

**Change**: Removed curly braces `{}` around all package names (standardized formatting)

**Packages affected**:
- earthdatalogin
- imputeTS
- terra
- sf
- tidyverse
- tidymodels
- DALEX
- tmap

---

### 4. Supplementary Note S1 (Lines 5, 9)

**BEFORE**:
> "The exact number of predictors retained in each LOSO-CV fold after correlation filtering (step_corr, Pearson r > 0.9)..."
> 
> "...the step_corr() function from the {recipes} package was applied independently..."

**AFTER**:
> "The exact number of predictors retained in each LOSO-CV fold after correlation filtering (Pearson r > 0.9)..."
> 
> "...highly correlated predictor pairs were identified and one from each pair was removed independently..."

**Change**: Removed `step_corr()` and `{recipes}` package references; replaced with method description

---

## Rationale

### Why Remove Function Names?

1. **Accessibility**: Not all readers use R or are familiar with tidymodels syntax
2. **Focus on method**: What matters is the statistical procedure, not the implementation
3. **Journal standards**: Most agricultural journals prefer general descriptions over software-specific terminology
4. **Reproducibility**: Citations to R packages (which remain) are sufficient for reproduction

### What Stays vs. What Goes

**KEEP**:
- ✅ Software/package citations in bibliography
- ✅ "R programming language" mention
- ✅ Package names in Software section (without curly braces)
- ✅ Statistical method descriptions (correlation filtering, etc.)

**REMOVE**:
- ❌ Function names in parentheses (e.g., "via step_corr()")
- ❌ Curly braces around package names {package}
- ❌ R-specific syntax or notation in main text
- ❌ Implementation details not essential to understanding method

---

## Guidelines for Future Writing

### Methods Section:
- ❌ "Predictors were normalized using step_normalize()"
- ✅ "Predictors were normalized to have zero mean and unit variance"

### Data Processing:
- ❌ "We used the {terra} package to extract values"
- ✅ "Values were extracted at sampling point coordinates" (cite package in Software section)

### Statistical Analysis:
- ❌ "Models were fitted with fit_resamples()"
- ✅ "Models were fitted using cross-validation"

### Software Section (OK to name packages):
- ✅ "We used the terra package [@citation] for raster processing"
- ✅ "Statistical modeling was performed with the tidymodels framework [@citation]"

---

## Files Modified

1. ✅ `manuscript/manuscript.qmd`
   - Methods §2.3.2 (correlation filtering)
   - Methods §2.2.4 (extraction method)
   - Software section (package name formatting)

2. ✅ `output/supplementary/Note_S1_step_corr_per_fold.md`
   - Methodology section
   - Opening description

---

## Impact

### Readability: ✅ Improved
- Text flows better without technical function names
- Accessible to readers across programming languages
- Focus on concepts rather than implementation

### Scientific Content: ✅ Unchanged
- All statistical procedures still clearly described
- Methods remain fully reproducible from descriptions + citations
- No loss of methodological precision

### Journal Standards: ✅ Better Aligned
Agricultural journals typically prefer:
- General statistical terminology
- Method descriptions over function names
- Software citations in dedicated section

---

## Summary

**Total changes**: 4 locations
- 2 function name removals (step_corr)
- 1 package reference removal ({terra})
- 8 curly brace removals ({package} → package)

**Principle applied**: Describe what the method **does**, not what the **function is called**

**Result**: More accessible manuscript without losing methodological rigor

---

**Completed**: 2026-09-21  
**Status**: All R-specific function references replaced with method descriptions
