# Academic Research Skills Review Panel — Manuscript Review Results

**Manuscript**: How much do satellite sensors add to thermal time? A leave-one-site-season-out evaluation of Sentinel-1, Sentinel-2, and PlanetScope predictors for wheat above-ground biomass

**Review Date**: 2026-09-17  
**Review Round**: Round 1  
**Skill Version**: academic-research-skills v3.21.1 / academic-paper-reviewer v1.11.1  
**Review Mode**: Full (5-reviewer panel)

---

## Review Files (Read in Order)

### Phase 0: Panel Configuration
- **`00_field_analysis.md`** (8.8 KB)
  - Field analysis and reviewer configuration
  - Paper classification: Agricultural Remote Sensing / Precision Agriculture
  - Quantitative research with Statistical Modeling / Machine Learning
  - Target journal: *Computers and Electronics in Agriculture* (Q1)
  - Paper maturity: Pre-submission

### Phase 1: Independent Reviewer Assessments

- **`01_review_journal_fit.md`** (34 KB)
  - **Reviewer**: Associate Editor, *Computers and Electronics in Agriculture*
  - **Recommendation**: Major Revision
  - **Confidence**: 5/5
  - **Key concerns**: Predictor screening data leakage risk (Critical), insufficient operational context (Major), SAR preprocessing not justified (Major)

- **`02_review_methodology.md`** (28 KB)
  - **Reviewer**: Associate Professor of Statistics, spatial cross-validation expert
  - **Recommendation**: Major Revision
  - **Confidence**: 5/5
  - **Key concerns**: Undocumented predictor screening (Critical), fold-specific correlation filter (Major), linear vs. nonlinear comparison (Major), statistical inference with n=4 (Major)

- **`03_review_domain.md`** (28 KB)
  - **Reviewer**: Senior research scientist, agricultural remote sensing (20+ years)
  - **Recommendation**: Major Revision
  - **Confidence**: 5/5
  - **Key concerns**: Omission of standard SAR preprocessing (Critical), PlanetScope sensor transition not tested (Major), inadequate spatial CV literature benchmarks (Major)

- **`04_review_crop_physiology.md`** (25 KB)
  - **Reviewer**: Professor of Crop Physiology, wheat phenology and thermal time expert
  - **Recommendation**: Minor Revision
  - **Confidence**: 5/5
  - **Key concerns**: Within-season sampling may favor GDD (Minor), high AGB values not explained (Minor), limited crop physiology literature (Minor)

- **`05_review_devils_advocate.md`** (22 KB)
  - **Reviewer**: Fixed adversarial seat (stress-testing core arguments)
  - **No overall recommendation** (findings only)
  - **Confidence**: 4/5
  - **Key concerns**: 3 Critical issues (predictor screening, within-season sampling bias, SAR preprocessing) + 7 Major issues

### Phase 2: Editorial Synthesis

- **`06_editorial_decision.md`** (40 KB)
  - **Decision**: **Major Revision**
  - **3 Blocking Issues** that must be resolved before acceptance
  - **7 Required Revisions** (must fix)
  - **7 Suggested Revisions** (should fix or consider)
  - Detailed consensus analysis and disagreement resolution
  - Complete revision roadmap with acceptance criteria

---

## Quick Summary

### Panel Consensus (All 5 Reviewers Agree)

✅ **Strengths**:
- LOSO-CV design is rigorous and appropriate
- Phenological baseline comparison is novel and valuable
- Transparent reporting of limitations is commendable
- Fixed hyperparameters are methodologically sound for n=4 folds

❌ **Critical Issue** (Blocking Acceptance):
- **Predictor screening data leakage risk** — The TODO at line 218 must be resolved. All 5 reviewers identified this as critical.

### Three Blocking Issues

**B1. Predictor Screening** (All 5 reviewers)
- Must document how 43 predictors were selected from 53 VIs
- If AGB was used → data leakage → invalidates all results → must re-run analysis

**B2. SAR Preprocessing** (EIC, R2, DA)
- Sentinel-1 used without speckle filtering or incidence angle normalization
- Must: (a) test the impact, (b) justify the choice, or (c) acknowledge limitation prominently

**B3. Linear vs. Nonlinear Comparison** (R1, DA)
- GDD baseline uses OLS, satellite models use RF/XGBoost
- Must: test RF/XGBoost on GDD alone, or discuss this confound as limitation

### Required Revisions (R1–R7, Must Fix)

1. **R1**: Document predictor screening, confirm no data leakage
2. **R2**: Justify SAR preprocessing omissions or test their impact
3. **R3**: Test nonlinear models on GDD baseline or discuss confound
4. **R4**: Add operational decision context with cited thresholds
5. **R5**: Discuss fold-specific correlation filter impact
6. **R6**: Add spatial CV crop AGB literature benchmarks
7. **R7**: Discuss sensor-specific per-fold transferability patterns

### Suggested Revisions (S1–S7, Should Fix)

1. **S1**: Test PlanetScope sensor transition radiometric impact
2. **S2**: Discuss within-season sampling design implications
3. **S3**: Explain agronomic conditions for high AGB values (>30 t/ha)
4. **S4**: Integrate crop physiology literature on GDD-biomass
5. **S5**: Justify vegetation index selection criteria
6. **S6**: Report k value for k-NN imputation
7. **S7**: Clarify correlation filter predictor removal logic

---

## Recommended Action Plan

### Immediate (Pre-Revision)

1. **Read `06_editorial_decision.md` first** — this synthesizes all reviewer feedback
2. **Review blocking issues B1–B3** — these must be resolved for acceptance
3. **Check the predictor screening process** (line 218 TODO) — this is the #1 priority

### For Revision

1. **Address all Required Revisions (R1–R7)** with specific acceptance criteria
2. **Consider Suggested Revisions (S1–S7)** — these strengthen the paper
3. **Prepare point-by-point response letter** following the template referenced in the editorial decision
4. **Use track changes** or color highlighting for all manuscript modifications

### Response Letter Structure

For each revision item (R1–R7, S1–S7):
- Quote the reviewer's concern
- Explain your response/change
- Reference specific manuscript changes (page/line numbers)
- Provide acceptance evidence where applicable

---

## Key Takeaways for Authors

**What the panel loved**:
- The rigorous LOSO-CV validation design
- The explicit phenological baseline comparison (rarely done in remote sensing)
- The transparent acknowledgment of limitations
- The recommendation that AGB studies report phenological baselines

**What needs fixing before acceptance**:
- Resolve the predictor screening process (TODO line 218) — data leakage risk
- Justify or test SAR preprocessing choices — generalizability concern
- Isolate satellite contribution from algorithmic contribution — comparison fairness
- Strengthen operational context and literature benchmarking

**The panel's verdict**:
> "With these revisions, the paper would make a strong contribution to *Computers and Electronics in Agriculture* by providing a rigorous methodological template for satellite validation studies and an important empirical finding about the dominance of thermal time under spatial cross-validation."

---

## File Sizes

- **Total review package**: 186 KB (7 files)
- **Largest file**: Editorial Decision (40 KB)
- **Average reviewer report**: 27 KB

---

## Citation

If you find this review helpful in improving your manuscript, the review was conducted using:

**Academic Research Skills (ARS) Plugin**  
Version: 3.21.1  
Skill: academic-paper-reviewer v1.11.1  
Mode: Full (5-reviewer panel)  
Date: 2026-09-17

---

## Questions?

Each review file is standalone and can be read independently, but the editorial decision (`06_editorial_decision.md`) provides the authoritative synthesis and revision roadmap. Start there for the complete picture.
