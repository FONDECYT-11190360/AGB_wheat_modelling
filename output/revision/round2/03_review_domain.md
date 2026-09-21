# Review 3: Domain Reviewer

**Role**: Senior Research Scientist, Remote Sensing for Agriculture  
**Expertise**: 15+ years in crop monitoring, SAR and optical sensor integration  
**Recommendation**: **MAJOR REVISION**  
**Confidence**: 5/5 (High)

---

## Summary

Multi-sensor comparison is comprehensive and valuable. Phenological baseline comparison is novel. Transparent reporting of sensor limitations is commendable. However, **critical sensor preprocessing choices (especially SAR) not justified, and vegetation index selection not documented**. These gaps prevent evaluation of whether satellites had a fair test.

---

## Strengths

1. Multi-sensor integration (S1, S2, PS) in one study
2. Phenological baseline comparison (novel in remote sensing)
3. Transparent sensor limitations (PS transition, S1 preprocessing)
4. VI selection from literature (references provided)
5. Explainability analysis (importance + ALE)

---

## Critical Issues

### C1. SAR Preprocessing Unjustified

**Evidence**: No speckle filtering, no incidence angle normalization  
**Domain Concern**: 
- Standard steps for SAR vegetation applications
- C-band backscatter varies 2-5 dB across swath (angle effect)
- Without these: S1 signal noisy and non-comparable

**Impact**: S1 worst (RMSE 10.92-12.19), importance ≈ 0  
**Required**: Reprocess OR justify with literature OR state limitation in Methods

### C2. VI Selection Not Documented

**Evidence**: 89 VIs → 43 → 39 but process not documented (line 218 TODO)  
**Concern**: Can't reproduce; don't know if AGB used for screening  
**Required**: Complete flowchart of VI selection; confirm no data leakage

### C3. Spatial CV Literature Benchmarks Missing

**Evidence**: Only Roberts 2017 cited; Meyer 2021, Ploton 2020 in Discussion only  
**Concern**: R²=0.58 not contextualized against spatial CV crop studies  
**Required**: Add benchmarks or state if none exist

---

## Major Issues

**M1. PlanetScope Sensor Transition Not Tested**  
Dove vs. SuperDove radiometric differences not assessed; confounded with site

**M2. VI Selection Criteria Not Stated**  
"Relevance" too vague; why OSAVI not NDVI in S2?

**M3. Cumulative VI Interpretation**  
If ∑VI behaves like ∑GDD, why use satellites? Test daily VIs only?

**M4. Soil Moisture Zero Spatial Variance**  
SM varies only temporally (single profile per field)—state in Methods

---

## Minor Issues

- S2 cloud masking: % pixels masked not reported
- PS UDM details missing
- Kalman smoothing parameters not stated
- GDD base temperature switch not justified

---

## Literature Coverage

**Strong**: AGB with ML, SAR for AGB, VI selection  
**Gaps**: Spatial CV crop examples, SAR preprocessing justification

---

## Recommendation

Timely question, rigorous methods, important finding. However, **three critical domain issues block acceptance**: SAR preprocessing unjustified, VI selection undocumented, spatial CV benchmarks missing.

**Decision**: Major Revision
