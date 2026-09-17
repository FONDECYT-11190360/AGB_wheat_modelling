# Peer Review Report

## Manuscript Information
- **Title**: How much do satellite sensors add to thermal time? A leave-one-site-season-out evaluation of Sentinel-1, Sentinel-2, and PlanetScope predictors for wheat above-ground biomass
- **Manuscript ID**: ARS-Review-2026-09-17
- **Review Date**: 2026-09-17
- **Review Round**: Round 1 review

---

## Reviewer Information

### Reviewer Role
Peer Reviewer 2 (Domain)

### Reviewer Identity
Senior research scientist in agricultural remote sensing with 20+ years of experience in crop monitoring using optical and SAR satellite data. Led multiple operational crop monitoring programs. Expert in Sentinel-1, Sentinel-2, and commercial constellation data for cereal crops. Published benchmark studies comparing vegetation indices for biomass estimation.

### Review Focus
This review evaluates the manuscript's treatment of remote sensing data and methods, focusing on literature positioning, sensor preprocessing choices, vegetation index selection, and whether the study fairly represents the capabilities of satellite data for AGB estimation when properly preprocessed.

---

## Overall Assessment

### Recommendation
- [X] **Major Revision** — Substantial revisions needed, re-review required after revision

### Confidence Score
5 — Completely within my area of expertise, I am very confident in my assessment

### Summary Assessment

This manuscript addresses an important question in operational crop monitoring: whether satellite data adds value beyond simple thermal time for wheat biomass estimation. The finding that a linear model on accumulated GDD (R²=0.53) outperforms most satellite-based configurations is sobering and challenges assumptions in the remote sensing community. The leave-one-site-season-out cross-validation design is appropriate for testing transferability, and the explicit comparison against phenological baselines under the same validation scheme is a valuable contribution that is rarely done in the literature.

However, I have serious concerns about whether the study's conclusions generalize beyond the specific preprocessing choices made here. Three major issues must be addressed: (1) Sentinel-1 data were used "without speckle filtering or incidence-angle normalization," which are standard preprocessing steps for agricultural SAR applications; since S1 alone was the weakest predictor set (RMSE 10.92-12.19 t/ha) and the paper concludes that satellite data adds minimal value, readers will question whether this conclusion would hold with proper SAR preprocessing. (2) PlanetScope data include a sensor transition from Dove to SuperDove during the study period, and the paper excludes red edge predictors that were unavailable for two site-seasons, but does not test whether radiometric differences between sensor generations affected transferability. (3) The literature review does not adequately engage with recent studies that use spatial cross-validation for AGB estimation, making it difficult to contextualize the reported R²=0.53-0.58 performance relative to what others have achieved with similar validation rigor.

If these preprocessing choices are deliberate (e.g., to test minimally processed open-access data), this must be stated clearly in the abstract and conclusion, and the generalizability of findings to optimized preprocessing should be discussed. As written, the paper risks misleading readers into thinking satellite data is not useful for wheat AGB estimation, when the evidence may only show that *minimally preprocessed* satellite data is not useful.

---

## Strengths

### S1: Multi-sensor comparison under consistent validation
The paper compares three satellite missions (Sentinel-1 SAR, Sentinel-2 optical, PlanetScope optical) under the same LOSO-CV scheme, which allows for a fair assessment of sensor contributions. Multi-sensor comparisons are common in the literature, but they are rarely evaluated with spatial cross-validation.

**Evidence Anchor**: `text: Methods §2.3.1 "We defined 14 predictor sets (Table 4). Two sets are phenological baselines... Three sets use a single sensor (S1, S2, PS)"`

### S2: Comprehensive vegetation index selection
The paper uses 53 vegetation indices (Table 3 and Supplementary Table S1) selected from recent wheat AGB literature, covering spectral, chlorophyll, structure, and water indices. The decision to compute cumulative sums in addition to daily values captures integrated seasonal dynamics.

**Evidence Anchor**: `text: Methods §2.2.5 "A total of 53 vegetation indices (VIs) were selected to estimate AGB using the daily interpolated S2 and PS data (Table 3). Daily cumulative sums of these VIs were also computed from the start of each season"`

### S3: Transparent disclosure of sensor limitations
The paper clearly documents the PlanetScope sensor transition (Dove to SuperDove) and the exclusion of red edge predictors for two site-seasons. This transparency allows readers to assess whether sensor inconsistencies affected results.

**Evidence Anchor**: `text: Methods §2.2.3 "Due to the sensor transition that occurred during the study period, both Dove and SuperDove imagery were included... the red edge band and the four PlanetScope predictors derived from it... were entirely missing [for two site-seasons]. Because values imputed for an entire site-season cannot carry information about that site-season, these four predictors were excluded from the analyses"`

### S4: Use of Level-2A and atmospherically corrected products
The study uses Sentinel-2 Level-2A (bottom-of-atmosphere reflectance) and PlanetScope Level-3B surface reflectance products, which are atmospherically corrected. This is appropriate for multi-temporal analysis.

**Evidence Anchor**: `text: Methods §2.2.3 "We used Level-2A surface reflectance products, which are atmospherically corrected (bottom-of-atmosphere, BOA)"; "We used Level-3B Surface Reflectance products, which are orthorectified and atmospherically corrected"`

---

## Weaknesses

### W1: Omission of standard SAR preprocessing (speckle filtering, incidence angle normalization)
**Problem**: The manuscript states: "Satellite predictors were extracted from the single pixel containing each sampling point, without speckle filtering or incidence-angle normalization for Sentinel-1, which may have reduced the contribution of SAR data" (Discussion §4.4, lines 402-403). Speckle noise is inherent to SAR data and degrades the signal-to-noise ratio, especially for single-pixel extraction. Multi-look or Lee speckle filtering is standard preprocessing for agricultural applications. Incidence angle affects backscatter intensity, and normalization (e.g., to a reference angle or using cosine correction) is commonly applied for multi-temporal analysis. The paper does not justify why these steps were omitted.

**Evidence Anchor**: `text: Discussion §4.4 "Satellite predictors were extracted from the single pixel containing each sampling point, without speckle filtering or incidence-angle normalization for Sentinel-1, which may have reduced the contribution of SAR data"`

**Why it matters**: The paper's main conclusion is that satellite data adds minimal value beyond thermal time, and Sentinel-1 alone was the weakest predictor set (RMSE = 10.92-12.19 t/ha, worse than DAS baseline). If speckle filtering and incidence angle normalization are known to improve SAR data quality, and the paper concludes that SAR is not useful without testing these improvements, readers will question whether the conclusion is about SAR's fundamental limitations or about suboptimal preprocessing. This is especially problematic given that the best configuration (S1+GDD, RF) includes Sentinel-1 and achieves a modest 5.9% gain, suggesting S1 *does* carry information when combined with thermal time. Would this gain be larger with proper preprocessing?

**Suggestion**: The authors have three options: (1) **Conduct a sensitivity analysis**: Apply multi-look speckle filtering (e.g., 5×5 Lee filter) and incidence angle normalization to S1 data for at least one site-season, re-extract predictors, and test whether the S1 and S1+GDD configurations improve. If performance improves, revise the conclusion to state that minimally preprocessed S1 adds minimal value, but optimized S1 may be more useful. If performance does not improve, this strengthens the conclusion. (2) **Justify the preprocessing choice**: Explicitly state that the goal was to test freely available, minimally processed data accessible to farmers or researchers without specialized SAR expertise, and that the findings apply specifically to this use case. Add this justification to the Abstract ("we evaluated minimally preprocessed satellite data"), Introduction (research objective), and Conclusion. (3) **Acknowledge the limitation more prominently**: Move the speckle filtering caveat from the Limitations paragraph (line 403) to the Abstract, Results (when first reporting S1 performance), and Conclusion, stating clearly that "these findings apply to minimally preprocessed SAR data; optimized SAR preprocessing may yield different results."

**Severity**: Critical — the paper's core conclusion about satellite data utility may not generalize to properly preprocessed SAR; this single issue affects the interpretation of all S1-related results and prevents acceptance without clarification.

**Confidence**: 5 — core expertise: operational SAR preprocessing for agricultural applications

### W2: PlanetScope sensor transition not tested for radiometric impact
**Problem**: The paper documents that both Dove and SuperDove sensors were used during the study period and that red edge predictors were excluded for two site-seasons (lines 156-158). However, the paper does not test whether the sensor transition affected the radiometric consistency of the four common bands (Blue, Green, Red, NIR). PlanetScope sensor generations have known radiometric differences that can affect vegetation index values even after atmospheric correction (Roy et al. 2021 is cited but the specific radiometric intercalibration issue is not discussed).

**Evidence Anchor**: `text: Methods §2.2.3 "Due to the sensor transition that occurred during the study period, both Dove and SuperDove imagery were included. The four bands common to all sensor types (Blue, Green, Red, and NIR) were used for all site-seasons"`

**Why it matters**: If PlanetScope's high RMSE variance across folds (SD = 4.15 t/ha for PS alone with RF, Table 5) is partly due to sensor transition artifacts rather than site characteristics, then the conclusion that PlanetScope transfers poorly may not apply to single-sensor deployments. Operationally, a farmer using only SuperDove data would not experience the transition, so the reported transferability may underestimate PS performance in a homogeneous sensor context.

**Suggestion**: (1) Report which site-seasons used Dove vs. SuperDove sensors. (2) If both sensors were used within a site-season, state this explicitly. (3) Conduct a sensitivity analysis: compare PS predictor values (e.g., NDVI, EVI) for dates acquired by Dove vs. SuperDove within overlapping periods. If substantial differences exist, discuss whether these affected transferability. (4) If this analysis is not feasible, discuss the sensor transition as a potential confound: "PlanetScope's variable transferability (SD=4.15 t/ha) may reflect sensor generation differences in addition to site characteristics; users of a single sensor type may experience better consistency."

**Severity**: Major — the high variance of PlanetScope results may be partly an artifact of sensor transition rather than intrinsic to PS data; discussion revision needed.

**Confidence**: 4 — general remote sensing standards rather than PS-specific radiometric expertise

### W3: Inadequate engagement with spatial cross-validation literature for AGB
**Problem**: The literature review cites several wheat AGB studies with high R² values (0.81-0.91) and correctly attributes the difference to validation scheme (random/within-site splits vs. spatial CV). However, the paper does not cite or compare with other studies that have used spatial or grouped cross-validation for crop AGB estimation. For example, recent work on maize/soybean AGB with spatial CV has reported R² values in the range of 0.40-0.70, which would provide useful context for interpreting the current R²=0.53-0.58.

**Evidence Anchor**: `text: Introduction "Those studies evaluated models with random or within-site splits. When the goal is to apply a model to fields not used for training, such splits overestimate accuracy..."; text: Discussion §4.1 "The performance of the best configuration (R²=0.58) is well below the values of 0.81 to 0.91 reported for wheat AGB with UAV, hyperspectral, or Sentinel-2 data. Those studies evaluated models with random or within-site splits"`

**Why it matters**: Without comparisons to other spatial CV studies, readers cannot assess whether R²=0.53-0.58 is typical, better than expected, or worse than expected for satellite-based AGB estimation under rigorous validation. If other studies with spatial CV for other crops have achieved R²=0.60-0.70 with satellite data, then the current results are in the lower range, which would support the "satellite adds minimal value" conclusion. Conversely, if other spatial CV studies achieve R²=0.40-0.50, then the current results are at the high end, and the thermal time baseline is remarkably strong rather than the satellite data being weak.

**Suggestion**: (1) Add 2-3 citations to the Introduction and Discussion of recent crop AGB studies that used spatial, site-level, or grouped cross-validation. Include their reported R² and RMSE values. (2) Compare the current R²=0.53-0.58 to those benchmarks. For example: "Our best configuration (R²=0.58) is comparable to [Study X] (R²=0.61 for maize with Sentinel-2 under site-level CV) but lower than [Study Y] (R²=0.72 for soybean with combined optical-SAR under spatial CV), suggesting that wheat AGB in Mediterranean systems may be more challenging to estimate, or that our thermal time baseline is unusually strong." (3) If such studies are rare (which would itself be a finding), state this explicitly: "To our knowledge, few wheat AGB studies have reported performance under site-level cross-validation, making it difficult to benchmark our results against the literature."

**Severity**: Major — lack of spatial CV benchmarks limits the interpretability of reported performance; literature review and discussion revision needed.

**Confidence**: 5 — core expertise: crop monitoring and remote sensing validation literature

### W4: Vegetation index selection rationale not fully justified
**Problem**: The paper states that 53 VIs were selected "based on their relevance for assessing vegetation status, biomass estimation, and yield prediction in wheat, forage, maize, and similar crops" (lines 169-171) with six citations. However, the criteria for including or excluding specific indices are not stated. For example, why include MCARI/OSAVI (chlorophyll-focused) but not SAVI (soil-adjusted)? Why include CVI (chlorophyll vegetation index) but not GCI (green chlorophyll index)? Were all indices from the cited papers included, or only a subset?

**Evidence Anchor**: `text: Methods §2.2.5 "The indices were chosen based on their relevance for assessing vegetation status, biomass estimation, and yield prediction in wheat, forage, maize, and similar crops (Peng 2023; Segarra 2022; Uribeetxebarria 2023; Wang 2022; Wu 2008; Zheng 2017)"`

**Why it matters**: The choice of which VIs to include affects the satellite predictors' apparent performance. If the 53 VIs include redundant indices (e.g., multiple NDVI variants), they may be removed by the correlation filter, effectively testing a smaller set. Conversely, if key indices for wheat biomass are missing, the satellite models may be handicapped. Without transparent selection criteria, readers cannot assess whether the VI set is comprehensive or biased.

**Suggestion**: (1) State the selection criteria explicitly. For example: "We included all indices from [citations] that: (a) can be computed from Sentinel-2 or PlanetScope bands, (b) have been used for wheat AGB or yield estimation in at least one published study, and (c) are not identical algebraic transformations (e.g., NDVI and SR were both included as commonly used, but NDVI² was excluded as redundant)." (2) If the selection was ad hoc (based on author familiarity or data availability), acknowledge this as a limitation. (3) Consider citing a recent VI review paper (e.g., Xue & Su 2017, Remote Sensing of Environment on vegetation indices) to ground the selection in the broader literature.

**Severity**: Minor — the VI selection is reasonable based on recent wheat literature, but the lack of explicit criteria is a transparency gap that could be easily addressed.

**Confidence**: 4 — general vegetation index expertise rather than wheat-specific knowledge

### W5: Nearest-pixel extraction may undersample spatial variability
**Problem**: The paper states: "Extraction was performed at each sampling point's coordinates as the value of the raster cell containing the point (nearest-pixel extraction via the {terra} package, with no spatial averaging over a buffer); given the 0.25 m² plot size relative to the 3-60 m sensor resolutions, each sampling plot is fully contained within a single pixel for every sensor" (lines 166-169). While it is correct that a 0.25 m² plot is contained within a 3 m pixel, this nearest-pixel extraction ignores the possibility that the pixel's value may be influenced by non-wheat features (e.g., if the plot is near a field edge, the 10 m Sentinel-2 pixel may include soil or vegetation from the border).

**Evidence Anchor**: `text: Methods §2.2.4 "Extraction was performed at each sampling point's coordinates as the value of the raster cell containing the point (nearest-pixel extraction via the {terra} package, with no spatial averaging over a buffer)"`

**Why it matters**: If some sampling points are near field edges or inhomogeneous areas, the satellite signal may not represent the wheat biomass at that point, adding noise to the satellite predictors. This would not affect the thermal time baseline (which is field-uniform for GDD and point-specific for DAS), potentially explaining part of the satellite predictors' poor performance. For small fields or irregular plot placements, this could be a substantial source of error.

**Suggestion**: (1) Report whether any sampling points were within one pixel width (3 m for PS, 10 m for S2, 10 m for S1) of a field boundary. If all points are >10 m from edges, state this to rule out the edge effect. (2) If edge effects are possible, discuss this as a potential noise source: "Nearest-pixel extraction may have introduced noise for sampling points near field boundaries, where the satellite pixel integrates wheat and non-wheat surfaces. Buffer-averaged extraction could reduce this noise but was not tested here." (3) Consider a sensitivity analysis: for one site-season, extract satellite predictors using a 3×3 pixel mean instead of nearest-pixel, and compare whether the S2 and PS models improve.

**Severity**: Minor — edge effects are a plausible but likely small noise source; discussion clarification would suffice.

**Confidence**: 4 — general remote sensing practice rather than site-specific field knowledge

---

## Detailed Comments

### Literature Review / Theoretical Framework
- The introduction covers the main satellite approaches (multispectral, SAR, LiDAR, hyperspectral) with recent citations. The discussion of cumulative indices (line 52) is valuable and well-cited.
- The gap identification (lack of phenological baselines under spatial CV) is excellent and well-motivated.
- **Missing**: The literature review does not cite studies that have used spatial or grouped cross-validation for crop AGB. Adding these would contextualize the reported performance (see Weakness W3).
- The introduction mentions that "droughts significantly impact agriculture" (line 50) and cites drought monitoring studies using VIs, but the current study's sites are Mediterranean systems with winter rainfall. It is not clear whether drought stress was a factor in the sampled seasons. If not, the drought framing is somewhat tangential.

### Methodology / Research Design
- The description of satellite data sources (Table 2) is comprehensive and clear.
- The preprocessing steps are generally appropriate (quality masking, temporal interpolation), with the exception of omitted SAR preprocessing (see Weakness W1).
- The decision to use Kalman smoothing for optical VIs (two-sided filter) is appropriate for in-season estimation but is correctly noted as unsuitable for real-time forecasting (lines 164-166).
- The extraction of VI values at sampling point coordinates is standard practice. The note that extraction uses nearest-pixel rather than buffer-averaging is transparent (line 166).

### Results / Findings
- The results are clearly presented with appropriate figures and tables.
- Table 5 is comprehensive and well-formatted. The ∆RMSE column and Folds column are useful for comparison.
- **Missing**: The results do not discuss per-fold patterns (which site-seasons did which sensors fail on?). This would help identify whether sensor failures are systematic (e.g., S1 fails in dry seasons, PS fails in sensor-transition seasons). See the Journal-Fit Reviewer's W4.

### Discussion
- The physiological interpretation of why thermal time carries most transferable signal is appropriate.
- The comparison with UAV/hyperspectral studies and attribution to validation scheme is valuable.
- **Needs strengthening**: The discussion does not adequately address whether the SAR preprocessing choices (no speckle filtering, no incidence angle normalization) limit generalizability (see Weakness W1).
- The differences from the companion letter are transparently acknowledged (lines 391-392).

---

## Questions for Authors

1. **Sentinel-1 preprocessing (Critical)**: You state that S1 data were used "without speckle filtering or incidence-angle normalization." Was this a deliberate choice to test minimally processed data, or a practical constraint? Have you tested whether applying these standard SAR preprocessing steps improves the S1 and S1+GDD configurations? If not, can you discuss how this affects the generalizability of your conclusion that "satellite data adds minimal value"?

2. **PlanetScope sensor transition**: Which site-seasons used Dove vs. SuperDove sensors? Were both sensors used within any site-season? Did you test whether the four common bands (Blue, Green, Red, NIR) have radiometric differences between sensor generations that could affect transferability?

3. **Spatial CV benchmarks**: Can you cite 2-3 studies that have used spatial or site-level cross-validation for crop AGB estimation with satellite data, and compare your R²=0.53-0.58 to their reported performance? This would help readers assess whether your results are typical, better than expected, or worse than expected under rigorous validation.

4. **Vegetation index selection**: What criteria were used to select the 53 VIs from the cited papers? Were all indices from those papers included, or only a subset? If a subset, what was the selection rationale?

---

## Minor Issues

### Remote Sensing Data
- Line 115, Table 2: The Sentinel-1 wavelength is listed as "5.546e7 nm" with a caption note that this is ~5.55 cm. While the conversion is correct, it would be clearer to list the wavelength as "5.5 cm (C-band)" and omit the nm conversion, since SAR wavelengths are conventionally reported in cm, not nm.
- Line 157: "the products are radiometrically calibrated, georeferenced to sub-pixel accuracy" — PlanetScope's georeferencing accuracy is typically stated as <10 m RMSE, not sub-pixel. Check the cited Roy et al. 2021 paper for the correct phrasing.

### Figures and Tables
- Figure 2 caption: "S~SOS~ indicates the start of the season (SOS) when AGB = 0, and S~final~ represents AGB at harvest." It would be clearer to state "S~SOS~ represents the sowing date (AGB = 0)" since "start of season" could be confused with emergence or greenup in remote sensing literature.
- Table 3: The formula for OSAVI uses the coefficient 1.16, while OSAVI$_2$ (row 1) uses RE_3 instead of NIR. It would be helpful to add a table footnote explaining that OSAVI$_2$ is a red edge variant, not the standard OSAVI.

---

## Criterion-Bound Judgements

Calibration status: `NOT_CALIBRATED`

| Dimension | Criterion source | Judgement | Evidence anchor(s) | Rationale | Uncertainty / scope limit | Decision bearing? |
|---|---|---|---|---|---|---|
| Originality | General | MEETS | text: Introduction "Without a phenological baseline evaluated under the same validation scheme..."; text: Discussion "We therefore recommend that studies of crop AGB estimation report a thermal-time or phenological baseline" | The explicit comparison against phenological baselines under spatial CV is novel. The recommendation to report baselines in future studies is a valuable methodological contribution. | None | No |
| Methodological Rigor | General | PARTLY_MEETS | text: §2.2.4 "without speckle filtering or incidence-angle normalization for Sentinel-1"; text: §2.2.3 sensor transition documented but radiometric impact not tested | LOSO-CV design is rigorous. SAR preprocessing omissions (no speckle filtering, no incidence angle normalization) are a Critical limitation not adequately justified. PlanetScope sensor transition is documented but not tested (Major). | Rigor would be upgraded to MEETS if SAR preprocessing is justified or tested | Yes (Critical for SAR; Major for PS) |
| Evidence Sufficiency | General | MEETS | table: Table 5 — comprehensive sensor comparison; text: per-fold results in Supplementary Table S2 | Comprehensive multi-sensor evaluation with transparent per-fold reporting. Limited by n=4 site-seasons but adequate for exploratory findings. | None | No |
| Argument Coherence | General | MEETS | text: Results → Discussion flow is logical | The interpretation that cumulative optical indices track season progress is well-supported by ALE profiles (Figure 8). | None | No |
| Writing Quality | General | MEETS | text: throughout | Manuscript is clearly written with appropriate technical detail for a remote sensing audience. | None | No |
| Literature Integration | General | PARTLY_MEETS | text: Introduction cites 70+ references; absence: spatial CV AGB studies not cited | Literature review covers remote sensing methods and spatial CV principles. Missing: comparisons to other spatial CV crop AGB studies that would benchmark the reported performance. | Limited engagement with crop physiology literature on GDD-AGB relationships | Yes (Major) |
| Significance & Impact | General | MEETS | text: Discussion "We therefore recommend that studies of crop AGB estimation report a thermal-time or phenological baseline" | The methodological recommendation to report phenological baselines could shift norms in the field. The finding that thermal time dominates has operational value. | Significance depends on whether preprocessing limitations are addressed | Yes (Major if generalizability is unclear) |

**Recommendation rationale**: The paper makes a valuable contribution by comparing satellite predictors against thermal time baselines under spatial cross-validation. However, **two decision-bearing criteria are only partly met**: (1) **Methodological Rigor** is undermined by the omission of standard SAR preprocessing (no speckle filtering, no incidence angle normalization, Critical) and lack of testing for PlanetScope sensor transition radiometric impact (Major). (2) **Literature Integration** is incomplete without comparisons to other spatial CV crop AGB studies (Major). Additionally, **Significance & Impact** depends on clarifying whether the findings generalize to optimized satellite preprocessing (Major). The SAR preprocessing issue is mandatory to address before acceptance, either by conducting a sensitivity analysis, justifying the choice, or prominently acknowledging the limitation. With these revisions, the paper would be a strong contribution to the agricultural remote sensing literature.
