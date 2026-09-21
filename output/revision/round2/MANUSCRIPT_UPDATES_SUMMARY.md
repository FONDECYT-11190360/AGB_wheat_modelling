# Manuscript Updates Summary — Addressing Blocking Issues

**Date**: 2026-09-18  
**Manuscript**: manuscript/manuscript.qmd  
**Status**: Blocking Issues B1, B2, B3 addressed via Path A/C/B respectively

---

## Changes Made

### 1. Blocking Issue B1: Predictor Screening Undocumented (Data Leakage Risk)

**Decision**: Path A — Document screening procedure (no AGB used)

**Location**: Methods §2.3.2 "Candidate predictors and predictor sets" (lines 216-222)

**Changes**:
- ✅ **Removed**: Line 218 TODO comment
- ✅ **Added**: Two-paragraph methodology description:
  - Paragraph 1: Literature-based VI selection (53 indices), 4 PS red edge exclusion
  - Paragraph 2: Within-fold correlation filtering (step_corr, r > 0.9) to avoid data leakage
- ✅ **Updated**: Predictor counts (S2: 23→36, PS: 10→12, All: 39→54)
- ✅ **Updated**: Table 4 to list all restored VIs

**Restored VIs** (15 total):
- S2: OSAVI, OSAVI2, SR, EVI, GNDVI, SIPI, CI_red, MCARI/OSAVI, TCARI/OSAVI, TCARI/OSAVI2, NDRE1, NDRE2, MCARI/OSAVI_8A
- PS: OSAVI, CI_red

**Evidence that no re-analysis was needed**: 
Corrected analysis already completed (scripts 31, 24), results nearly identical (mean |ΔRMSE| = 3.2%), demonstrating robustness.

---

### 2. Blocking Issue B2: SAR Preprocessing Unjustified

**Decision**: Path C — Acknowledge as major limitation (prominent disclosure)

**Location**: Methods §2.2.2 "Preprocessing of remote sensing data" (line 163)

**Changes**:
- ✅ **Added**: Prominent limitation statement in Methods:
  > "**Important limitation**: Sentinel-1 data were used without speckle filtering or incidence angle normalization, which are standard preprocessing steps for SAR vegetation applications. This may have affected S1 performance, and S1 results should be interpreted cautiously."

- ✅ **Added**: Caveat in Abstract (line 13):
  > "S1 results should be interpreted cautiously as data were used without speckle filtering or incidence angle normalization."

**Rationale**: 
Prominent disclosure in both Methods and Abstract ensures readers understand this limitation upfront. S1 conclusions qualified throughout manuscript.

---

### 3. Blocking Issue B3: Linear vs. Nonlinear Algorithm Confound

**Decision**: Path B — Acknowledge confound in Discussion

**Location**: Discussion §4 (new subsection after "Added value of satellite predictors")

**Changes**:
- ✅ **Added**: New subsection "Algorithm and predictor effects" (after line 393):
  > "An important limitation of this study is that the $\sum$GDD baseline uses linear regression while satellite models use RF and XGBoost, making it difficult to separate the contribution of satellite information from the contribution of model nonlinearity. When S1+GDD (RF) outperforms $\sum$GDD (LM), the improvement could reflect: (a) Sentinel-1 information, (b) RF's ability to capture nonlinear relationships in thermal time, or (c) both. To isolate the sensor contribution, future work should compare the same algorithm across predictor sets, for example by testing RF on $\sum$GDD alone. Until this is done, the configuration-dependent gains reported here may reflect both satellite information and model nonlinearity, and the marginal contribution of satellite predictors beyond a nonlinear thermal time model remains uncertain."

- ✅ **Updated**: Abstract conclusion (line 13):
  > "satellite predictors provided at most modest, configuration-dependent gains **that may reflect both satellite information and model nonlinearity**."

**Rationale**: 
Full paragraph in Discussion explains the confound, future directions, and implications. Abstract qualified to avoid overclaiming.

---

## Summary of Text Changes

| Section | Lines | Change Type | Description |
|---------|-------|-------------|-------------|
| **Abstract** | 13 | ADDED | SAR preprocessing caveat + algorithm confound qualifier |
| **Methods §2.2.2** | 163 | ADDED | Prominent SAR preprocessing limitation |
| **Methods §2.3.2** | 216-222 | REMOVED + ADDED | TODO removed; predictor selection methodology added (2 paragraphs) |
| **Methods §2.3.2** | 220-224 | UPDATED | Predictor counts (36 S2, 12 PS, 54 All) |
| **Methods Table 4** | 237-239 | UPDATED | Full VI lists for S2 and PS |
| **Discussion §4** | After 393 | ADDED | New subsection "Algorithm and predictor effects" |

---

## Next Steps (Required Revisions R1-R5)

The blocking issues are now resolved. The following required revisions remain:

### R1. Statistical Inference with n=4 Understated
**Action needed**: Expand Discussion §4.3 from 2 sentences to full paragraph (≥5 sentences)
**Current location**: Line ~401 ("The main limitation is the small number...")
**Estimated effort**: 1-2 hours

### R2. Operational Decision Context Missing
**Action needed**: Add Discussion paragraph with quantitative operational thresholds
**Suggested location**: End of §4.4 "Implications for AGB monitoring"
**Estimated effort**: 2-3 hours (literature search + writing)

### R3. Fold-Specific Correlation Filter Behavior Undocumented
**Action needed**: Create Supplementary Table showing per-fold predictor counts after step_corr()
**Data source**: Extract from script logs
**Estimated effort**: 1-2 hours

### R4. Within-Season Sampling Design Implications Not Discussed
**Action needed**: Add Discussion paragraph on sampling design implications; qualify scope in Abstract
**Suggested location**: New subsection before §4.4
**Estimated effort**: 2-3 hours

### R5. Vegetation Index Selection Criteria Not Stated
**Action needed**: Add Methods subsection documenting VI selection criteria with references
**Suggested location**: §2.2.3 or as new subsection before §2.3.2
**Estimated effort**: 1-2 hours

---

## Suggested Revisions (Optional)

### R6. High AGB Values Not Explained
**Action needed**: Add study site characteristics (irrigation, fertilization, variety)
**Suggested location**: Methods §2.1
**Estimated effort**: 2-3 hours

### R7. Crop Physiology Literature Integration
**Action needed**: Add Introduction paragraph on physiological basis for GDD-AGB relationship
**Estimated effort**: 1-2 hours

---

## Files to Update for Response Letter

When preparing your response to reviewers, reference:

1. **This summary** — Overview of all blocking issue resolutions
2. **REVIEWER_RESPONSE_B1.md** — Detailed B1 response with impact assessment
3. **Corrected analysis outputs**:
   - `data/processed/modelos/rev2_loso_limpio_resultados_CORRECTED.rds`
   - `output/tables/comparison_original_vs_corrected.csv`
4. **Review documents**:
   - `output/revision/round2/06_editorial_decision.md`
   - Individual reviewer reports (01-05)

---

## Verification Checklist

- [x] B1 resolved: Predictor selection documented, TODO removed
- [x] B2 resolved: SAR limitation disclosed in Methods + Abstract
- [x] B3 resolved: Algorithm confound acknowledged in Discussion + Abstract
- [x] All predictor counts updated (36 S2, 12 PS, 54 All)
- [x] Table 4 updated with complete VI lists
- [ ] R1: n=4 limitation expanded (pending)
- [ ] R2: Operational thresholds added (pending)
- [ ] R3: Supplementary Table created (pending)
- [ ] R4: Sampling design discussed (pending)
- [ ] R5: VI selection criteria documented (pending)

---

**Last Updated**: 2026-09-18  
**Status**: Blocking issues resolved; 5 required revisions remain  
**Next Action**: Address R1-R5 required revisions

