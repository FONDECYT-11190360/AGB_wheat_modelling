# Editorial Decision Letter + Revision Roadmap

**Journal**: *Computers and Electronics in Agriculture*  
**Manuscript ID**: (Not yet assigned)  
**Title**: How much do satellite sensors add to thermal time? A leave-one-site-season-out evaluation of Sentinel-1, Sentinel-2, and PlanetScope predictors for wheat above-ground biomass  
**Decision Date**: 2026-09-18

---

## DECISION: MAJOR REVISION

Dear Dr. Zambrano,

Thank you for submitting your manuscript to *Computers and Electronics in Agriculture*. We have completed the peer review process, and I am pleased to inform you that the reviewers recognize this work as an important contribution to precision agriculture research. However, your manuscript requires **major revisions** before it can be accepted for publication.

---

## Summary Assessment

Your manuscript addresses a critical question rarely examined in the agricultural remote sensing literature: how much do satellite sensors actually add beyond simple phenological baselines? The novelty of explicitly benchmarking multi-sensor configurations against thermal time (∑GDD) provides essential context that is often missing from remote sensing studies. Your rigorous LOSO-CV design correctly tests spatial transferability, and your transparent reporting of limitations is commendable.

**However, three critical methodological issues must be resolved before publication**:

1. **Undocumented predictor screening** (line 218 TODO) creates potential data leakage
2. **SAR preprocessing omissions** may invalidate S1 conclusions
3. **Algorithm confound** prevents separating satellite information from model nonlinearity

These issues, along with five required revisions and two suggested improvements detailed below, must be addressed comprehensively in your revision.

---

## Reviewer Consensus and Disagreements

### Unanimous Consensus (All 5 Reviewers)

**Strength: Phenological Baseline Comparison**  
Every reviewer praised the thermal time baseline as novel and valuable. This is rarely done in agricultural remote sensing literature.

> "The phenological baseline comparison is novel and valuable for the precision agriculture community. Explicitly comparing satellite models against ∑GDD is uncommon in remote sensing literature and provides essential context." — Journal-Fit Reviewer

**Critical Issue: Line 218 TODO (Data Leakage Risk)**  
All five reviewers independently identified the TODO comment at line 218 as a critical validity threat. If predictors were screened using AGB from the full dataset, this constitutes data leakage that invalidates all cross-validation results.

**Strength: LOSO-CV Design**  
All reviewers agreed the validation strategy is sound, with correctly fixed hyperparameters and within-fold preprocessing.

### Strong Consensus (4/5 Reviewers)

**Critical: SAR Preprocessing**  
Four reviewers (Journal-Fit, Methodology, Domain, Devil's Advocate) flagged that Sentinel-1 was used without speckle filtering or incidence angle normalization. These are standard preprocessing steps for SAR vegetation applications.

> "SAR preprocessing is not optional — it's standard practice. Without it, the conclusion that 'Sentinel-1 alone was the weakest predictor set' may reflect preprocessing choices, not sensor capabilities." — Domain Reviewer

**Major: n=4 Statistical Power**  
Four reviewers noted that only 4 site-seasons means most model differences are within noise, and ranking uncertainty is high.

### Moderate Consensus (3/5 Reviewers)

**Within-Season Sampling Design**  
Three reviewers (Perspective, Devil's Advocate, Domain) noted that all samples are within-season progressions, which may systematically favor temporal predictors like ∑GDD.

**Algorithm Confound**  
Three reviewers (Methodology, Domain, Devil's Advocate) noted that the ∑GDD baseline uses linear regression while satellite models use RF/XGBoost, preventing attribution of improvements.

### Single Dissent: Decision Severity

**Minor vs. Major Revision**  
Four reviewers recommended **Major Revision**. One reviewer (Perspective, Crop Physiology) recommended **Minor Revision**, viewing the methodological issues as clarifications rather than validity threats. Their rationale: thermal time dominance is physiologically expected regardless of these issues, so the biological finding stands.

**Editorial Arbitration**: The Editorial Decision is **Major Revision** based on:
1. Data leakage is a fundamental validity threat requiring resolution
2. SAR preprocessing affects interpretability of a key finding
3. Multiple reviewers raised the same critical concerns independently

---

## Blocking Issues (3 CRITICAL — Must Fix)

Publication is **blocked** until these three issues are resolved. Each requires either re-analysis or prominent methodological clarification.

### B1. Predictor Screening Undocumented (Data Leakage Risk)

**Source**: All 5 reviewers (unanimous consensus)  
**Severity**: CRITICAL — fundamental validity threat  
**Evidence**: Line 218 TODO comment: "describe how the 43 candidate predictors...were screened...and confirm that AGB was not used"

**Problem**:  
If predictors were screened using AGB correlation in the full dataset before cross-validation, this is data leakage. Test fold data influenced predictor selection, invalidating all CV results.

**Required Action** (choose one path):

**Path A (if screening was literature-based only, no AGB used)**:
- Document complete screening procedure in Methods §2.3.2
- Confirm explicitly that AGB was never used for predictor selection
- Explain which 53→43 criteria were applied (e.g., availability, literature support)

**Path B (if AGB was used for screening)**:
- Re-run entire analysis with screening moved inside CV folds
- Use `step_corr()` or similar within each training fold independently
- Update all results tables and figures
- Acknowledge in Results what changed

**Acceptance Criterion**: Methods section clearly documents predictor selection process with explicit statement that held-out test data never influenced selection.

---

### B2. SAR Preprocessing Unjustified

**Source**: 4/5 reviewers (strong consensus)  
**Severity**: CRITICAL — affects interpretability of key finding  
**Evidence**: Methods §2.2.1 states S1 was used "as-is" without speckle filtering or incidence angle normalization

**Problem**:  
Speckle filtering and incidence angle normalization are standard preprocessing for SAR vegetation applications (Ulaby & Long 2014; Steele-Dunne et al. 2017). Without them, the conclusion "Sentinel-1 alone was the weakest predictor set" may reflect preprocessing choices rather than sensor capabilities.

**Required Action** (choose one path):

**Path A (ideal, if time permits)**:
- Reprocess S1 with Lee speckle filter + angle normalization
- Re-run S1-containing models
- Update results

**Path B (justification with literature)**:
- Add Methods subsection explaining why preprocessing was omitted
- Cite studies showing minimal impact in similar contexts
- Quantify expected impact magnitude

**Path C (acknowledge as major limitation)**:
- Move limitation from Discussion to Methods (prominent disclosure)
- State: "S1 was used without speckle filtering or angle normalization, which are standard preprocessing steps. This may have affected S1 performance."
- Revise conclusions to avoid over-generalizing S1 weakness
- Add to Abstract: "S1 results should be interpreted cautiously given preprocessing limitations"

**Acceptance Criterion**: Either S1 is properly preprocessed, or the limitation is prominently disclosed in Methods + Abstract with revised conclusions.

---

### B3. Linear vs. Nonlinear Algorithm Confound

**Source**: 3/5 reviewers (moderate consensus)  
**Severity**: CRITICAL — attribution ambiguity  
**Evidence**: ∑GDD baseline uses Linear Model; satellite models use RF/XGBoost

**Problem**:  
When S1+GDD (RF) outperforms ∑GDD (LM), the improvement could be: (a) S1 information, (b) RF's nonlinearity capturing ∑GDD better, or (c) both. Current design confounds predictor effect with algorithm effect.

**Required Action** (choose one path):

**Path A (ideal, isolates algorithm effect)**:
- Add ∑GDD (RF) and ∑GDD (XGBoost) configurations
- Compare: ∑GDD (LM) vs. ∑GDD (RF) = algorithm effect
- Compare: ∑GDD (RF) vs. S1+GDD (RF) = S1 marginal contribution
- Update Results and Discussion with attribution

**Path B (acknowledge confound)**:
- Add Discussion paragraph explaining confound
- State: "Configuration-dependent gains may reflect both satellite information and model nonlinearity. To isolate sensor contribution, future work should compare the same algorithm across predictor sets."
- Revise Abstract to note: "S1+GDD outperformed ∑GDD, though gains may partly reflect model nonlinearity rather than satellite information alone"
- Avoid claiming satellite sensors "added X% improvement" without qualification

**Acceptance Criterion**: Either algorithm effect is isolated via Path A, or confound is acknowledged in Discussion + Abstract with qualified conclusions.

---

## Required Revisions (5 Major, Must Address)

These revisions are required for acceptance but do not block the decision. Address all five in your revision.

### R1. Statistical Inference with n=4 Understated

**Source**: 4/5 reviewers  
**Severity**: Major  
**Current State**: Limitation mentioned briefly in Discussion (2 sentences)

**Problem**:  
With only 4 site-seasons, most configuration differences are within 1 SD (noise). Model ranking is highly uncertain. Current discussion understates this limitation.

**Required Fix**:
- Expand Discussion §4.3 from 2 sentences to full paragraph
- State explicitly: "With n=4, most differences between configurations are within one standard deviation of the mean and may not represent true performance differences."
- Add to Results: Report SD alongside means in all tables
- Avoid strong ranking claims ("X was best") — use "X performed well in this sample"

**Acceptance Criterion**: Discussion §4.3 expanded to ≥5 sentences explicitly addressing ranking uncertainty.

---

### R2. Operational Decision Context Missing

**Source**: 3/5 reviewers  
**Severity**: Major  
**Current State**: Practical implications mentioned qualitatively

**Problem**:  
Paper argues satellite sensors "add little," but doesn't provide operational context for what accuracy thresholds stakeholders need.

**Required Fix**:
- Add Discussion paragraph: "What RMSE is acceptable for farmers/insurers/government?"
- Cite quantitative operational thresholds from literature or stakeholder interviews
- Contextualize your RMSE values against those thresholds
- Example: "If insurance payout decisions require ±2 t/ha accuracy, our best model (RMSE=5.8) would not meet that threshold, suggesting both ∑GDD and satellite models require improvement for that use case."

**Acceptance Criterion**: Discussion includes quantitative operational thresholds with citations.

---

### R3. Fold-Specific Correlation Filter Behavior Undocumented

**Source**: 2/5 reviewers  
**Severity**: Major (reproducibility)  
**Current State**: `step_corr(threshold=0.9)` mentioned; which predictors removed per fold not reported

**Problem**:  
Each LOSO fold may have removed different predictors. This is methodologically correct but must be documented for reproducibility.

**Required Fix**:
- Add Supplementary Table: Per-fold predictor counts after `step_corr()`
- Report which predictors were removed most frequently
- Confirm in Methods: "Correlation filtering was applied independently per fold; consequently, each fold retained a different subset of predictors (range: X–Y predictors)"

**Acceptance Criterion**: Supplementary Table showing per-fold predictor counts after filtering.

---

### R4. Within-Season Sampling Design Implications Not Discussed

**Source**: 3/5 reviewers  
**Severity**: Major (scope)  
**Current State**: Not mentioned

**Problem**:  
All 153 samples are within-season progressions (sowing → ripening within fields). Any predictor tracking "how far into season?" performs well. ∑GDD is precisely this. Satellites may add more value for between-season comparisons (e.g., detecting stress at same thermal time across fields).

**Required Fix**:
- Add Discussion paragraph: "Sampling Design and Generalizability"
- State: "All samples were within-season progressions. This design tests whether satellite sensors can track phenological development within a growing season, but may not fully capture their value for detecting condition differences (e.g., drought stress) at the same phenological stage across fields or seasons."
- Clarify scope in Abstract: "Within-season tracking" or similar qualifier
- Avoid overgeneralizing to "satellites lack value" — findings apply to this sampling design

**Acceptance Criterion**: Discussion paragraph added; Abstract qualified with "within-season" or equivalent scope.

---

### R5. Vegetation Index Selection Criteria Not Stated

**Source**: 3/5 reviewers  
**Severity**: Major (reproducibility)  
**Current State**: Table 3 lists 53 VIs; selection criteria not stated

**Problem**:  
How were these 53 VIs chosen from the hundreds in literature? Criteria must be explicit for reproducibility.

**Required Fix**:
- Add Methods subsection: "Vegetation Index Selection"
- State criteria: "VIs were selected based on: (1) demonstrated utility for crop biomass in peer-reviewed literature (≥3 citations), (2) computational feasibility from S1/S2/PS bands, (3) conceptual diversity (greenness, red edge, structure, moisture)"
- Cite key references justifying each VI category
- Acknowledge: "This selection represents a subset of available VIs and was made prior to any AGB analysis"

**Acceptance Criterion**: Methods section explicitly states VI selection criteria with references.

---

## Suggested Revisions (2, Recommended but Not Required)

### R6. High AGB Values Not Explained

**Source**: 2/5 reviewers  
**Severity**: Moderate  
**Evidence**: Seven samples >30 t/ha (max 43.5 t/ha)

**Observation**:  
43.5 t/ha is exceptional for wheat. Context needed.

**Suggested Fix**:
- Add Methods subsection: "Study Site Characteristics"
- Provide irrigation status, fertilization intensity, wheat variety
- Verify measurement method for high values
- Explain agronomic reasons for exceptional productivity

---

### R7. Crop Physiology Literature Integration

**Source**: 1/5 reviewers  
**Severity**: Minor  
**Current State**: GDD-biomass relationship cited briefly (Del Pozo 1987)

**Observation**:  
Physiological basis for ∑GDD dominance could be strengthened.

**Suggested Fix**:
- Add Introduction paragraph on physiological basis
- Cite: McMaster & Wilhelm 1997, Porter & Gawith 1999
- Explain: Thermal time → phenological stage → LAI → light interception → photosynthesis → biomass
- Frame ∑GDD baseline as physiologically grounded, not just empirical

---

## Devil's Advocate CRITICAL Issues — Adjudication

The Devil's Advocate reviewer raised three CRITICAL challenges. All three have been validated by other reviewers and are incorporated into Blocking Issues above:

| DA Issue | Status | Incorporated As |
|----------|--------|-----------------|
| **DA-C1**: Predictor screening data leakage | ✓ VALIDATED (5/5 consensus) | **B1** |
| **DA-C2**: Within-season sampling bias | ✓ VALIDATED (3/5 corroboration) | **R4** |
| **DA-C3**: SAR preprocessing fairness | ✓ VALIDATED (4/5 consensus) | **B2** |

**Strongest Counter-Argument** (Devil's Advocate):  
"The claim 'satellites add little beyond thermal time' may be an artifact of within-season sampling design rather than evidence satellites lack value. Current design tests whether satellites can track phenology (they can), not whether they can detect condition differences at the same thermal time across fields (untested)."

**Editorial Response**:  
This counter-argument is **partially validated**. The within-season sampling design does systematically favor temporal predictors. However, the finding that satellites add modest value **within this design** remains valid and publishable, provided scope is clearly stated (see R4).

---

## Revision Roadmap

### Phase 1: Resolve Blocking Issues (Priority 1)

**Timeline**: 1-2 weeks  
**Action**: Choose paths for B1, B2, B3

| Issue | Recommended Path | Estimated Effort |
|-------|------------------|------------------|
| **B1** | Path A (if no AGB used) | 2-3 hours (text revision) |
|        | Path B (if AGB used) | 4-6 weeks (re-analysis) |
| **B2** | Path C (acknowledge limitation) | 3-4 hours (text revision) |
| **B3** | Path B (acknowledge confound) | 2-3 hours (text revision) |

**Recommended Strategy** (if no re-analysis needed):  
- B1 Path A: Document screening procedure
- B2 Path C: Acknowledge SAR limitation prominently
- B3 Path B: Acknowledge algorithm confound
- **Total time**: 1 week for text revisions

### Phase 2: Address Required Revisions (Priority 2)

**Timeline**: 3-5 days  
**Action**: Address R1-R5

| Revision | Action | Estimated Effort |
|----------|--------|------------------|
| **R1** | Expand n=4 limitation discussion | 1-2 hours |
| **R2** | Add operational threshold context | 2-3 hours (literature search + writing) |
| **R3** | Create Supplementary Table (fold-specific predictors) | 1-2 hours (extract from logs) |
| **R4** | Add within-season sampling discussion | 2-3 hours |
| **R5** | Document VI selection criteria | 1-2 hours |

**Total time**: 3-5 days

### Phase 3: Consider Suggested Revisions (Optional)

**Timeline**: 2-3 days  
**Action**: R6 (high AGB explanation), R7 (physiology literature)

---

## Response Letter Requirements

Your response letter must address:

1. **For each Blocking Issue (B1-B3)**: State which path you chose and provide evidence
2. **For each Required Revision (R1-R5)**: State where in manuscript changes were made (section/line numbers)
3. **For Suggested Revisions**: State whether addressed, and if not, why
4. **For Devil's Advocate challenges**: Respond to strongest counter-argument

Use this format:

```
Issue B1 (Predictor Screening):
- Path chosen: A (no AGB used)
- Evidence: Methods §2.3.2 (lines XXX-XXX) now documents selection procedure
- Changes: [list specific text added]

Issue R1 (n=4 Statistical Power):
- Addressed: Yes
- Location: Discussion §4.3 (lines XXX-XXX)
- Changes: Expanded from 2 sentences to 6 sentences addressing ranking uncertainty
```

---

## Re-Review Process

Upon receiving your revised manuscript and response letter:

1. **Initial Editorial Check**: I will verify that all Blocking Issues have been addressed
2. **Peer Re-Review**: If blocking issues are resolved, manuscript will be sent to 2-3 original reviewers
3. **Focus**: Re-review will focus on whether revisions adequately address identified issues
4. **Timeline**: 2-3 weeks for re-review

**Likely Outcome if Revisions Are Adequate**: Accept with minor revisions or Accept

---

## Final Comments

Your manuscript makes an important contribution by challenging common assumptions in agricultural remote sensing. The finding that thermal time dominates is valuable and physiologically grounded. The rigorous LOSO-CV design and transparent reporting of limitations are commendable.

The required revisions are substantial but feasible. Most can be addressed through text clarifications rather than re-analysis, provided predictor screening did not involve AGB (B1 Path A) and you choose acknowledgment paths for B2 and B3.

I look forward to receiving your revised manuscript. If you have questions about the decision or need clarification on any reviewer comment, please contact the editorial office.

Sincerely,

**Associate Editor**  
*Computers and Electronics in Agriculture*

---

## Appendix: Reviewer Reports

See separate files:
- `01_review_journal_fit.md` — Journal-Fit Reviewer report
- `02_review_methodology.md` — Methodology Reviewer report
- `03_review_domain.md` — Domain Reviewer report
- `04_review_perspective.md` — Perspective Reviewer report
- `05_review_devils_advocate.md` — Devil's Advocate report

---

**Decision Date**: 2026-09-18  
**Review Round**: 2 (Post-Correction)  
**Panel Composition**: 5 reviewers (4 dynamic + 1 fixed Devil's Advocate)  
**Consensus Level**: Strong (4/5 Major Revision, 1/5 Minor Revision)
