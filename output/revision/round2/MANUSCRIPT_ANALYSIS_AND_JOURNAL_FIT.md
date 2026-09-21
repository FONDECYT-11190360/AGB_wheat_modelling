# Manuscript Analysis and Journal Recommendations

**Date**: 2026-09-21  
**Manuscript**: Thermal time dominates satellite predictors for within-season wheat biomass estimation under spatial cross-validation

---

## NOVELTY ASSESSMENT

### Primary Novelty (High Impact) ⭐⭐⭐⭐⭐

**Explicit phenological baseline comparison**
- **What's new**: Directly compares satellite ML models against thermal time (∑GDD) baseline under same validation
- **Why it matters**: Remote sensing papers rarely include phenological baselines; most claim skill without this context
- **Impact**: Challenges common assumption that satellite sensors always improve over simple baselines
- **Strength**: Finding is provocative but rigorously demonstrated

### Secondary Novelty (Strong) ⭐⭐⭐⭐

**Spatial transferability focus with LOSO-CV**
- **What's new**: Tests site-season-level generalization (not random splits)
- **Why it matters**: Addresses major limitation of within-site validation common in crop monitoring
- **Impact**: More realistic assessment of operational performance
- **Strength**: Four site-seasons is small but method is exemplary

### Methodological Contributions ⭐⭐⭐⭐

1. **Multi-sensor evaluation** (S1, S2, PlanetScope) in single framework
2. **Fixed hyperparameters** across LOSO-CV (avoids overfitting with n=4)
3. **Within-fold preprocessing** (proper data leakage prevention)
4. **Transparent reporting** of limitations (n=4, SAR preprocessing, algorithm confound)

### Novel Finding ⭐⭐⭐⭐⭐

**Thermal time dominance for within-season tracking**
- Counter to conventional wisdom in remote sensing community
- Well-qualified (scope explicit, limitations acknowledged)
- Robust to methodological corrections (3.2% mean change)
- Has implications for operational systems and resource allocation

### Scope Qualification (Post-Revision) ⭐⭐⭐⭐

- **Before revision**: Could be read as "satellites don't work for wheat"
- **After revision**: Clearly scoped to within-season phenological tracking
- **Improvement**: Prevents overgeneralization while preserving finding
- **Impact**: Increases credibility and acceptance probability

---

## QUALITY ASSESSMENT

### Methodological Rigor ⭐⭐⭐⭐⭐

**Strengths**:
- ✅ Proper spatial cross-validation (LOSO-CV)
- ✅ Fixed hyperparameters (no data leakage from tuning)
- ✅ Within-fold preprocessing (no test data influence)
- ✅ Multiple algorithms tested (LM, RF, XGBoost)
- ✅ Phenological baseline included (rare in remote sensing)

**Addressed Weaknesses** (post-revision):
- ✅ Data leakage corrected (predictor screening documented)
- ✅ SAR preprocessing limitation disclosed prominently
- ✅ Algorithm confound acknowledged
- ✅ n=4 statistical power limitation expanded

**Overall**: Methods are rigorous and limitations transparently reported

### Reproducibility ⭐⭐⭐⭐⭐

- ✅ Open code and data (GitHub repository)
- ✅ VI selection criteria documented
- ✅ Hyperparameters specified
- ✅ Within-fold filtering explained
- ✅ Analysis scripts numbered and documented

**Rating**: Exemplary reproducibility

### Writing Quality ⭐⭐⭐⭐

**Strengths**:
- Clear structure and logical flow
- Appropriate technical detail
- Transparent about limitations
- Well-qualified conclusions

**Minor issues**:
- Some sections could be more concise
- R2 citations need verification
- Supplementary Note S1 needs formatting

**Overall**: Well-written, publication-ready after minor revisions

### Figures and Tables ⭐⭐⭐⭐

**Strengths**:
- Clear visualization of results
- Appropriate use of error bars (SD)
- Comprehensive predictor tables
- Effective use of faceting

**Could improve**:
- Some figures mentioned but not verified in revision
- Legend clarity in some plots

### Data Quality ⭐⭐⭐⭐

**Strengths**:
- 153 destructive AGB samples (substantial field effort)
- Four site-seasons spanning 3 years
- Multiple wheat habits (winter/spring)
- Multi-sensor coverage (S1, S2, PS)
- Weather and soil moisture included

**Limitations** (acknowledged):
- Small n for site-season level (4)
- Geographic confounding (location/soil/habit)
- High AGB values not fully explained
- Within-season sampling only

**Overall**: High-quality dataset with appropriate caveats

---

## IMPACT POTENTIAL

### Scientific Impact ⭐⭐⭐⭐⭐

**Why this matters**:
1. **Challenges assumptions**: Questions satellite-only approaches
2. **Provides context**: Phenological baseline comparison
3. **Improves methods**: Demonstrates proper spatial CV
4. **Honest assessment**: Transparent about what works and what doesn't
5. **Actionable**: Clear implications for operational systems

### Practical Impact ⭐⭐⭐⭐

**Who benefits**:
- Precision agriculture practitioners (∑GDD baseline is operational)
- Satellite data providers (identifies where sensors add value)
- Agricultural agencies (resource allocation for monitoring systems)
- Researchers (benchmark for future studies)

### Citation Potential ⭐⭐⭐⭐

**Likely to be cited for**:
1. Phenological baseline comparison methodology
2. LOSO-CV implementation example
3. Multi-sensor evaluation framework
4. Transparent reporting of limitations
5. Counter-narrative to satellite optimism

---

## JOURNAL RECOMMENDATIONS

### Selection Criteria

Based on manuscript characteristics:
- **Topic**: Agricultural remote sensing, wheat biomass, precision agriculture
- **Methods**: Spatial cross-validation, machine learning, multi-sensor
- **Finding**: Challenges assumptions, methodological contribution
- **Quality**: High rigor, transparent reporting, open data/code
- **Impact**: Scientific and practical implications

---

## TOP 5 JOURNAL RECOMMENDATIONS

### 1. Remote Sensing of Environment ⭐⭐⭐⭐⭐

**Impact Factor**: ~13.5 (2023)  
**Publisher**: Elsevier  
**Scope**: Leading journal in remote sensing science

**FIT ASSESSMENT**:

**Strengths** (Why it fits):
- ✅ Multi-sensor evaluation is core theme
- ✅ Methodological rigor matches journal standards
- ✅ Spatial transferability focus valued
- ✅ Challenges assumptions (journal publishes critical assessments)
- ✅ Agricultural applications well-represented

**Challenges**:
- ⚠️ Negative finding (thermal time dominates) might need strong framing
- ⚠️ n=4 site-seasons on lower end for top-tier journal
- ⚠️ Geographic scope limited (Chile only)

**Recommendation**: **STRONG FIT** if framed as methodological contribution (phenological baseline comparison) rather than just negative finding

**Positioning strategy**:
- Emphasize novelty of baseline comparison
- Frame as "reality check" for remote sensing community
- Highlight spatial CV methodology
- Present as benchmark for future studies

**Probability of acceptance**: 60-70% (after revision)

---

### 2. Computers and Electronics in Agriculture ⭐⭐⭐⭐⭐

**Impact Factor**: ~8.3 (2023)  
**Publisher**: Elsevier  
**Scope**: Agricultural technology and precision farming

**FIT ASSESSMENT**:

**Strengths** (Why it fits):
- ✅ Perfect scope match (precision agriculture)
- ✅ Practical implications emphasized
- ✅ Multi-sensor + ML is core topic
- ✅ Operational focus (∑GDD baseline usability)
- ✅ **Already targeted in revision** (current submission plan)

**Challenges**:
- ⚠️ None significant

**Recommendation**: **EXCELLENT FIT** — This is the ideal journal for this manuscript

**Positioning strategy**:
- Emphasize operational implications (∑GDD vs satellite)
- Highlight cost-benefit (simple baseline vs complex models)
- Focus on within-season monitoring application
- Present as decision support for practitioners

**Probability of acceptance**: 75-85% (after addressing R2-R5 revisions)

**Status**: Currently planned submission target ✅

---

### 3. ISPRS Journal of Photogrammetry and Remote Sensing ⭐⭐⭐⭐

**Impact Factor**: ~12.7 (2023)  
**Publisher**: Elsevier  
**Scope**: Photogrammetry, remote sensing methods

**FIT ASSESSMENT**:

**Strengths** (Why it fits):
- ✅ Methodological rigor valued highly
- ✅ Multi-sensor integration core topic
- ✅ Spatial validation methods emphasized
- ✅ Agricultural applications accepted
- ✅ LOSO-CV methodology is novel contribution

**Challenges**:
- ⚠️ More methods-focused than application-focused
- ⚠️ May need stronger emphasis on sensor evaluation
- ⚠️ Agricultural topic less common than urban/environmental

**Recommendation**: **GOOD FIT** with methods-focused framing

**Positioning strategy**:
- Lead with LOSO-CV methodology
- Emphasize multi-sensor evaluation framework
- Frame as sensor comparison study
- Phenological baseline as "reference method"

**Probability of acceptance**: 55-65%

---

### 4. Agricultural and Forest Meteorology ⭐⭐⭐⭐⭐

**Impact Factor**: ~6.2 (2023)  
**Publisher**: Elsevier  
**Scope**: Agricultural meteorology, crop-climate interactions

**FIT ASSESSMENT**:

**Strengths** (Why it fits):
- ✅ **Growing degree days (∑GDD) is core topic** — perfect thematic fit
- ✅ Crop phenology and thermal time central to journal
- ✅ Remote sensing + meteorology integration valued
- ✅ Within-season crop monitoring is key scope
- ✅ Finding that ∑GDD dominates is HIGH INTEREST for this audience

**Challenges**:
- ⚠️ Less emphasis on remote sensing methods
- ⚠️ May need more crop physiology discussion

**Recommendation**: **EXCELLENT FIT** — Underrated choice, could be ideal

**Positioning strategy**:
- **Lead with thermal time/phenology angle** (not remote sensing)
- Emphasize crop physiology (∑GDD → phenology → biomass)
- Frame satellite sensors as "augmenting meteorological data"
- Highlight weather + soil moisture predictors
- Add more crop physiology literature (R7 suggestion)

**Probability of acceptance**: 70-80%

**Special note**: This journal's audience will EXPECT thermal time to dominate (crop physiology perspective), so the finding is less controversial and more "physiologically grounded validation"

---

### 5. European Journal of Agronomy ⭐⭐⭐⭐

**Impact Factor**: ~5.2 (2023)  
**Publisher**: Elsevier  
**Scope**: Crop science, agronomy, crop production

**FIT ASSESSMENT**:

**Strengths** (Why it fits):
- ✅ Wheat production focus (core crop)
- ✅ Mediterranean agriculture well-represented
- ✅ Within-season crop monitoring valued
- ✅ Practical implications for growers
- ✅ Thermal time and phenology central topics

**Challenges**:
- ⚠️ Remote sensing less emphasized than other journals
- ⚠️ May need more agronomic context (irrigation, fertilization)
- ⚠️ Smaller impact factor

**Recommendation**: **SOLID FIT** as backup option

**Positioning strategy**:
- Lead with wheat production angle
- Emphasize Mediterranean agriculture
- Frame as "monitoring tool evaluation for growers"
- Add agronomic context (high AGB explanation, R6)
- De-emphasize remote sensing methods, emphasize application

**Probability of acceptance**: 65-75%

---

## JOURNAL RANKING SUMMARY

### By Fit to Manuscript Content:

1. **Computers and Electronics in Agriculture** — Best overall fit ⭐⭐⭐⭐⭐
2. **Agricultural and Forest Meteorology** — Thematic perfect match ⭐⭐⭐⭐⭐
3. **Remote Sensing of Environment** — High impact, good fit ⭐⭐⭐⭐
4. **European Journal of Agronomy** — Solid backup ⭐⭐⭐⭐
5. **ISPRS J. Photogrammetry** — Methods-focused fit ⭐⭐⭐⭐

### By Impact Factor (if priority):

1. **Remote Sensing of Environment** (IF ~13.5)
2. **ISPRS J. Photogrammetry** (IF ~12.7)
3. **Computers and Electronics in Agriculture** (IF ~8.3)
4. **Agricultural and Forest Meteorology** (IF ~6.2)
5. **European Journal of Agronomy** (IF ~5.2)

### By Acceptance Probability:

1. **Agricultural and Forest Meteorology** (70-80%)
2. **Computers and Electronics in Agriculture** (75-85%)
3. **European Journal of Agronomy** (65-75%)
4. **Remote Sensing of Environment** (60-70%)
5. **ISPRS J. Photogrammetry** (55-65%)

---

## FINAL RECOMMENDATION

### Primary Choice: Computers and Electronics in Agriculture ✅

**Why**:
- Perfect scope match
- Already revision target
- High acceptance probability
- Good impact factor
- Practical focus matches manuscript

**Action**: Continue with current plan

---

### Alternative Choice: Agricultural and Forest Meteorology 🌟

**Why**:
- **Thermal time/∑GDD is core journal theme**
- Audience expects thermal time to dominate (physiologically grounded)
- Finding is validation, not controversy
- High acceptance probability
- Could add optional R6-R7 improvements

**Action**: Consider for future submission if CEA rejects

**Frame shift needed**: Lead with phenology/thermal time, not remote sensing

---

### Backup Choice: Remote Sensing of Environment

**Why**:
- Highest impact factor
- Methodological contribution valued
- Challenges assumptions (journal publishes critical work)

**Risk**: n=4 may be seen as insufficient for top-tier journal

**Action**: Submit here if seeking maximum impact and willing to risk rejection

---

## MANUSCRIPT STRENGTHS FOR ANY JOURNAL

1. ✅ Novel comparison (phenological baseline)
2. ✅ Rigorous spatial validation (LOSO-CV)
3. ✅ Multi-sensor evaluation
4. ✅ Transparent limitations
5. ✅ Open data and code
6. ✅ Robust finding (validated through corrections)
7. ✅ Clear scope (within-season tracking)
8. ✅ Practical implications

## AREAS FOR IMPROVEMENT (Optional, Journal-Dependent)

1. **For Agricultural and Forest Meteorology**: Add crop physiology literature (R7)
2. **For European Journal of Agronomy**: Add agronomic context (R6)
3. **For Remote Sensing of Environment**: Strengthen methodological contribution framing
4. **For all**: Verify R2 citations before submission

---

**Analysis completed**: 2026-09-21  
**Recommendation**: Submit to **Computers and Electronics in Agriculture** (current plan) ✅  
**Alternative**: **Agricultural and Forest Meteorology** (excellent thematic fit)
