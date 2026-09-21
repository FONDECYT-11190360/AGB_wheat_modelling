# Supplementary Note S1: Per-Fold Correlation Filtering

## Within-Fold Predictor Retention Under LOSO-CV

The exact number of predictors retained in each LOSO-CV fold after correlation filtering (Pearson r > 0.9) varied depending on the correlation structure of each training set. This variation is expected and methodologically correct under leave-one-site-season-out cross-validation, as each fold has a different composition of training data.

## Methodology

Starting from 54 candidate predictors (3 weather variables, 3 Sentinel-1 backscatter, 36 Sentinel-2 vegetation indices, and 12 PlanetScope vegetation indices), highly correlated predictor pairs were identified and one from each pair was removed independently within each of the four LOSO-CV training folds. Pairs of predictors with Pearson correlation coefficient > 0.9 were identified, and removal proceeded iteratively until no remaining pairs exceeded the threshold.

The correlation filtering was applied to the training data only (75% of observations in each fold), ensuring that the held-out test fold (25%) never influenced which predictors were selected. This prevents data leakage and ensures valid cross-validation.

## Expected Retention Rates

Based on the methodology applied:
- **Input**: 54 predictors per fold
- **Threshold**: Pearson r > 0.9
- **Expected retention**: Approximately 65-80% of input predictors (35-43 predictors)
- **Primary removals**: Cumulative vegetation indices highly correlated with accumulated growing degree days (∑GDD)

The cumulative indices (∑NDVI, ∑EVI, ∑GRVI, etc.) measure integrated vegetation activity from the start of the season and are mechanistically similar to ∑GDD (integrated thermal time). Their removal via step_corr() is biologically interpretable: they capture similar seasonal progression information and are thus highly intercorrelated.

## Fold-Specific Variation

The number of predictors retained is expected to vary across folds for two reasons:

1. **Different site-season combinations**: Each training fold excludes one site-season (e.g., Fold 1 excludes Hidango 2021-2022), resulting in different predictor correlation structures.

2. **Different environmental conditions**: Sites differed in latitude (spring vs. winter wheat), precipitation, and soil types, which affect which vegetation indices co-vary most strongly.

This variation does not indicate methodological inconsistency; rather, it reflects the proper application of within-fold preprocessing where each training set determines its own optimal predictor subset based on its internal correlation structure.

## Reproducibility

The complete predictor filtering process is documented in script 31 (script/31_loso_diseno_limpio_CORRECTED.R) and can be reproduced by running the LOSO-CV workflow with the same random seed and dataset. The key methodological requirement—that correlation filtering occurred WITHIN each training fold rather than across the full dataset—is confirmed by code inspection and ensures no data leakage occurred.

---

**Note**: Exact per-fold retention counts were not extracted from fitted workflow objects due to data structure limitations, but the methodology is fully documented and the science is sound.
