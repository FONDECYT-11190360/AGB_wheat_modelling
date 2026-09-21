# Peer Review Round 2 — Post-Correction Review

**Manuscript**: How much do satellite sensors add to thermal time? A leave-one-site-season-out evaluation of Sentinel-1, Sentinel-2, and PlanetScope predictors for wheat above-ground biomass

**Review Date**: 2026-09-18  
**Review Type**: Full Academic Review (5-reviewer panel)  
**Target Journal**: *Computers and Electronics in Agriculture*

---

## Review Status

**DECISION**: **MAJOR REVISION**

**Panel Configuration**:
- **Reviewer 1 (Journal-Fit)**: Associate Editor, *Computers and Electronics in Agriculture*
- **Reviewer 2 (Methodology)**: Associate Professor of Agricultural Statistics
- **Reviewer 3 (Domain)**: Senior Research Scientist, Remote Sensing for Agriculture (15+ years)
- **Reviewer 4 (Perspective)**: Professor of Crop Physiology, wheat phenology expert
- **Reviewer 5 (Devil's Advocate)**: Fixed role challenging core arguments

---

## Decision Summary

**Recommendation Breakdown**:
- Major Revision: 4 reviewers (Journal-Fit, Methodology, Domain, Devil's Advocate)
- Minor Revision: 1 reviewer (Perspective)
- **Consensus**: Major Revision required

**Key Strengths** (Unanimous):
1. Novel phenological baseline comparison (rarely done in remote sensing)
2. Rigorous LOSO-CV design with fixed hyperparameters
3. Transparent reporting of limitations
4. Important finding challenging common assumptions
5. Exemplary reproducibility (open code/data)

**Blocking Issues** (3 Critical, Must Fix):
1. **B1. Predictor screening undocumented** (line 218 TODO) — potential data leakage (5/5 reviewers)
2. **B2. SAR preprocessing unjustified** — no speckle filtering/angle normalization (4/5 reviewers)
3. **B3. Linear vs. nonlinear confound** — algorithm effect confounded with predictor effect (3/5 reviewers)

**Required Revisions** (5 Major):
- R1. Statistical inference with n=4 implications understated
- R2. Operational decision context missing (quantitative thresholds)
- R3. Fold-specific correlation filter behavior not documented
- R4. Within-season sampling design may favor ∑GDD
- R5. Vegetation index selection criteria not stated

**Suggested Revisions** (2):
- R6. High AGB values (>30 t/ha) not explained
- R7. Crop physiology literature could be better integrated

---

## Key Findings from Review Panel

### Consensus Points (All 5 Reviewers Agree)

**Critical Issue: Line 218 TODO**
- Every reviewer identified the TODO comment as a critical validity threat
- If predictors were screened using AGB from full dataset → data leakage
- This invalidates all cross-validation results
- **Must be resolved before publication**

**Strength: Phenological Baseline Comparison**
- All reviewers praised this as novel and valuable
- Rarely done in agricultural remote sensing literature
- Provides essential context for interpreting satellite model performance

**Strength: LOSO-CV Design**
- Appropriate for testing spatial transferability
- Fixed hyperparameters correctly avoid overfitting with n=4
- Within-fold preprocessing executed properly

### Strong Consensus (4/5 Reviewers)

**Critical: SAR Preprocessing**
- Sentinel-1 used without speckle filtering or incidence angle normalization
- These are standard preprocessing steps for SAR vegetation applications
- Omission may explain why S1 performed worst
- Current conclusion "S1 was weakest" may reflect preprocessing, not sensor capability

**Major: Statistical Inference with n=4**
- Only 4 site-seasons means low statistical power
- Most configuration differences are within noise (SD > mean difference)
- Ranking of models is highly uncertain
- Current discussion of this limitation is too brief

### Moderate Consensus (3/5 Reviewers)

**Within-Season Sampling Design**
- All samples are within-season progressions
- Design tests "Can satellites track phenology?" not "Can satellites detect condition?"
- ∑GDD advantage may be specific to this sampling design
- Satellites may add more value for between-season comparisons

**Algorithm Confound**
- ∑GDD baseline uses linear regression
- Satellite models use RF/XGBoost
- Can't separate satellite information from algorithm nonlinearity
- Needs testing RF on ∑GDD alone, or prominent acknowledgment

---

## Reviewer-Specific Perspectives

### Journal-Fit Reviewer (Major Revision)
**Focus**: Journal fit, operational relevance, methodological validity  
**Main Concerns**: 
- Data leakage risk (line 218 TODO) is critical validity threat
- SAR preprocessing must be justified or corrected
- Operational context needs quantitative thresholds

**Quote**: "This manuscript addresses an important question with rigorous methods and reports findings that challenge common assumptions. However, three critical methodological issues block acceptance."

### Methodology Reviewer (Major Revision)
**Focus**: Statistical validity, cross-validation rigor, inference  
**Main Concerns**:
- Predictor screening potentially invalid
- n=4 site-seasons limits statistical power severely
- Algorithm vs. predictor effects confounded

**Quote**: "LOSO-CV design is appropriate and fixed hyperparameters correctly avoid overfitting. However, critical concerns about predictor screening and statistical inference with n=4 must be addressed."

### Domain Reviewer (Major Revision)
**Focus**: Sensor evaluation fairness, preprocessing standards, literature  
**Main Concerns**:
- SAR preprocessing is not optional—it's standard practice
- VI selection process not documented (reproducibility issue)
- Results not benchmarked against spatial CV crop studies

**Quote**: "Without speckle filtering and angle normalization, the conclusion that 'Sentinel-1 alone was the weakest predictor set' may reflect preprocessing choices, not sensor capabilities."

### Perspective Reviewer (Minor Revision)
**Focus**: Crop physiology, biological plausibility, within-season design  
**Main Assessment**: Thermal time dominance is physiologically expected and well-supported  
**Main Concerns**:
- Within-season sampling may systematically favor ∑GDD
- High AGB values need agronomic explanation
- Crop physiology literature could be better integrated

**Quote**: "From a crop physiology perspective, this manuscript is sound and makes an important contribution. The thermal time baseline is appropriate and the finding is well-supported."

**Why Minor not Major**: Views methodological issues as clarifications rather than validity threats; accepts that ∑GDD dominance is expected given crop biology.

### Devil's Advocate (Challenge Role)
**Focus**: Core argument validity, alternative explanations, logical gaps  
**Strongest Counter-Argument**: "Core conclusion may be artifact of within-season sampling design rather than evidence that satellites lack value. Design tests phenology tracking, not condition assessment."

**3 CRITICAL Challenges** (all validated):
- DA-C1: Predictor screening data leakage (consensus → B1)
- DA-C2: Within-season sampling design bias (validated → R4)
- DA-C3: SAR preprocessing fairness (consensus → B2)

---

## Critical Issues Requiring Re-Analysis

### If Any of These Apply, Major Re-Work Needed:

**Scenario 1**: Predictors were screened using AGB correlation in full dataset
- **Impact**: Data leakage invalidates all results
- **Required**: Move screening inside CV folds, re-run entire analysis
- **Timeline**: 4-6 weeks (re-run + manuscript updates)

**Scenario 2**: Authors choose to reprocess Sentinel-1 with proper preprocessing
- **Impact**: S1 results may improve substantially
- **Required**: Apply speckle filter + angle normalization, re-run S1-containing models
- **Timeline**: 2-3 weeks (reprocessing + re-analysis)

**Scenario 3**: Authors choose to test RF on ∑GDD alone (isolate algorithm effect)
- **Impact**: Can properly attribute S1+GDD improvement
- **Required**: Add ∑GDD (RF) and ∑GDD (XGBoost) configurations
- **Timeline**: 1-2 weeks (additional experiments)

### If None Apply, Clarification-Only Revision:

- Predictor screening was literature-based only (no AGB)
- SAR preprocessing limitation acknowledged prominently in Methods
- Algorithm confound acknowledged in Discussion/Abstract
- **Timeline**: 1-2 weeks (text revisions only)

---

## Response Strategy

### Must Address (Blocking)

1. **B1 (Predictor Screening)**:
   - Document complete screening procedure
   - Confirm AGB never used for selection
   - If AGB was used: re-run analysis with screening inside CV

2. **B2 (SAR Preprocessing)**:
   - Choose: (a) reprocess S1, (b) justify with literature, or (c) state limitation prominently
   - Move from Discussion to Methods

3. **B3 (Algorithm Confound)**:
   - Choose: (a) test RF on ∑GDD alone, or (b) acknowledge confound in Abstract/Discussion

### Must Address (Required)

- R1: Expand n=4 limitations discussion (from 2 sentences to full paragraph)
- R2: Cite quantitative operational thresholds
- R3: Document correlation filter behavior per fold
- R4: Discuss within-season sampling implications; clarify claim scope
- R5: State VI selection criteria explicitly

### Should Consider (Suggested)

- R6: Explain high AGB values with agronomic context
- R7: Integrate crop physiology literature (McMaster & Wilhelm 1997, etc.)

---

## Files in This Directory

1. **00_README.md** (this file) — Overview and summary
2. **01_review_journal_fit.md** — Journal-Fit Reviewer report
3. **02_review_methodology.md** — Methodology Reviewer report
4. **03_review_domain.md** — Domain Reviewer report
5. **04_review_perspective.md** — Perspective Reviewer (Crop Physiology) report
6. **05_review_devils_advocate.md** — Devil's Advocate report
7. **06_editorial_decision.md** — Editorial Decision Letter + Detailed Revision Roadmap

---

## Comparison with Round 1 Review (2026-06-12)

### What Changed

**Round 1 Status**: Same manuscript reviewed earlier, identified same Issue B1 (line 218 TODO)

**Data Leakage Fix Completed** (2026-09-17):
- Created corrected scripts (31, 24) with all 53 VIs
- Ran corrected LOSO analysis
- Results nearly identical (mean |ΔRMSE| = 3.2%)
- **Manuscript NOT YET UPDATED** with corrected methodology

**Current Review** (2026-09-18):
- Reviews the SAME manuscript text (TODO still present at line 218)
- Identifies same data leakage issue
- **Critical difference**: Corrected analysis already exists but not incorporated

### What This Means

**Good News**:
- Corrected analysis already completed
- Results are robust (3.2% average change)
- Main conclusions validated

**Immediate Action Required**:
- Update manuscript Methods §2.3.2 with corrected predictor selection text
- Remove line 218 TODO
- Confirm all Table/Figure values match corrected analysis

**After Manuscript Update**:
- B1 will be resolved (predictor screening documented, no data leakage)
- B2 and B3 still require attention (SAR preprocessing, algorithm confound)
- Required revisions R1-R5 still need addressing

---

## Recommended Next Steps

### Priority 1: Update Manuscript Text (Immediate)

Use the corrected methodology description prepared in `REVIEWER_RESPONSE_B1.md`:

```markdown
Vegetation index selection followed a two-stage approach to avoid data 
leakage. First, we identified 53 indices from the literature (Table 3) 
that have demonstrated utility for crop biomass estimation. Four 
PlanetScope red edge predictors were excluded as they were entirely 
missing for two site-seasons, leaving 53 predictors for modeling.

Within each LOSO-CV training fold, highly correlated predictors 
(Pearson r > 0.9) were removed via step_corr() to reduce multicollinearity. 
This within-fold filtering was applied independently to each training set, 
ensuring the held-out test fold never influenced predictor selection.
```

### Priority 2: Address SAR Preprocessing (B2)

**Recommended Path**: Acknowledge as major limitation (Path C)
- Move limitation from Discussion to Methods
- Quantify expected impact (cite SAR literature)
- Revise conclusions to avoid over-generalizing S1 weakness

**Rationale**: Reprocessing S1 would take several weeks; acknowledging limitation is acceptable if prominently stated.

### Priority 3: Address Algorithm Confound (B3)

**Recommended Path**: Acknowledge confound in Discussion + Abstract (Path B)
- Add paragraph to §4.2 explaining confound
- Revise Abstract to note "configuration-dependent gains that may reflect both satellite information and model nonlinearity"

**Rationale**: Testing RF on ∑GDD alone is quick (~1 week) but acknowledgment is acceptable.

### Priority 4: Address Required Revisions R1-R5

These are primarily text additions/clarifications, not re-analysis.

**Estimated Timeline**:
- Manuscript updates (all text revisions): 1-2 weeks
- Author response letter: 3-5 days
- **Total**: 2-3 weeks for revision submission

---

## Key Quotes from Reviewers

### On Strengths

> "The phenological baseline comparison is novel and valuable for the precision agriculture community. Explicitly comparing satellite models against ∑GDD is uncommon in remote sensing literature and provides essential context." — Journal-Fit Reviewer

> "The validation strategy is sound, and the authors correctly avoid common pitfalls (tuning on test data, pooled R²)." — Methodology Reviewer

> "This study addresses a timely question and uses rigorous methods. The finding that thermal time dominates is important and challenges remote sensing optimism." — Domain Reviewer

> "From a crop physiology perspective, the finding that ∑GDD dominates is entirely expected and well-supported—AGB accumulation is fundamentally driven by thermal time." — Perspective Reviewer

### On Critical Issues

> "Data leakage is a fundamental validity threat. Reviewers and readers cannot assess the validity of results without knowing whether test data influenced predictor selection." — Methodology Reviewer

> "SAR preprocessing is not optional—it's standard practice. Without it, the conclusion that 'Sentinel-1 alone was the weakest predictor set' may reflect preprocessing choices, not sensor capabilities." — Domain Reviewer

> "The core claim—'satellite sensors add little beyond thermal time'—may be correct for within-season tracking but incorrect for between-field condition assessment. Satellites weren't given a task they're designed for." — Devil's Advocate

---

## Verdict Summary

**Scientific Merit**: High — Novel comparison, rigorous design, challenges assumptions  
**Methodological Validity**: Conditional — Blocking issues must be resolved first  
**Transparency**: Excellent — Limitations openly acknowledged  
**Reproducibility**: Exemplary — Open code and data  

**Overall Assessment**: Important contribution pending resolution of three critical methodological issues. With revisions, this will be a strong paper for *Computers and Electronics in Agriculture*.

**Revision Feasibility**: High — Most issues are clarifications; corrected analysis already exists; no fundamental redesign needed.

**Publication Timeline** (estimated):
- Revision submission: 2-3 weeks
- Re-review: 2-3 weeks
- Acceptance (if revisions adequate): ~6 weeks total

---

**Review Panel Completed**: 2026-09-18  
**Status**: Awaiting author revision
