## Perspective Review Report (Peer Reviewer 3)

criteria_binding_unavailable

Target venue (author stated): International Journal of Remote Sensing, research article. No criteria authority was supplied, so no venue alignment claim is made.

### Reviewer Identity

Crop physiologist and crop modeller (APSIM, radiation use efficiency models, thermal time phenology) who works with crop insurance and agricultural advisory services. I read this paper from outside remote sensing. SAR processing specifics are outside my competence and I do not assess them.

### Overall Recommendation

Major Revision

### Confidence Score

4 (high on agronomy, phenology, GDD computation and practical use; low on SAR and on ML implementation details)

Confidence is an uncertainty/scope disclosure only; it never changes consensus counts, severity, decision bearing, or arbitration.

### Calibration Status

`NOT_CALIBRATED`

### Criterion-Bound Judgements

| Dimension / criterion | Criterion source | Judgement | Evidence anchors | Rationale | Uncertainty or scope limit | Decision bearing? |
|---|---|---|---|---|---|---|
| Agronomic validity of the thermal time predictor | R3 configuration card, focus 1 | DOES_NOT_MEET | `dataset: script/06_descargar_clima.R line 79; data/processed/rds/dataset.rds gdd_cumsum` | Tmin is replaced by a constant 1 °C in the code; base temperature switch differs from Methods | Verified for Villa Baviera with station 103 data; other stations inferred from same code | yes, the title predictor is miscomputed |
| Plausibility of the AGB reference data | R3 card, focus 1 | PARTLY_MEETS | `dataset: data/processed/rds/dataset.rds, La Cancha 2022-09-09` | Values above 30 t ha⁻¹ and implied crop growth rates exceed physiological ceilings | No yield or harvest index available to me | yes |
| Choice of baseline relative to crop modelling practice | R3 card, focus 2 | PARTLY_MEETS | `text: §1 para 4 "Accumulated thermal time has this property by construction"` | A linear GDD baseline is a useful floor, but RUE and crop model baselines are the agronomic reference and are absent | none identified | yes |
| Novelty of "thermal time dominates" | R3 card, particular concern | PARTLY_MEETS | `text: §4.1 "This is expected from crop physiology"` | The headline restates a known agronomic relation; the evaluation design does not isolate condition signal | none identified | yes |
| Practical and operational value | R3 card, focus 3 | MEETS | `text: §4.5 "too coarse for field-level decisions such as variable-rate fertilisation or insurance assessment"` | The authors are candid that the result is not operational; some operational claims (gridded temperature) are untested | none identified | no |
| SAR processing | R3 card, blind spot | NOT_ASSESSED | — | Outside reviewer competence | — | no |

Do not total, weight, average, or mechanically map these judgements to the recommendation.

### Summary Assessment

The manuscript asks a question that crop modellers have wanted remote sensing papers to ask for years: does a satellite model beat a simple model of season progress when applied to a field it has never seen? The answer reported here, that a linear model on accumulated growing degree days (∑GDD) is hard to beat across four Chilean wheat fields, is plausible and the authors present it with commendable restraint. From my perspective three problems stand between this and publication. First, the thermal time predictor that gives the paper its title is not computed as the Methods state: the code replaces daily minimum temperature by a constant 1 °C, and the base temperature switch happens at tillering, not stem elongation. Second, several AGB observations are physiologically implausible (mean 24.9 t ha⁻¹ two weeks into stem elongation at La Cancha; implied crop growth rates near 52 to 55 g m⁻² d⁻¹), and these points dominate the error. Third, "thermal time dominates" is a known agronomic fact restated: within a season, AGB follows thermal time almost by definition. The remote sensing question that matters to agronomists, insurers and advisers is whether satellites detect departures from the expected thermal time trajectory, and the current pooled evaluation does not test that. A radiation use efficiency baseline, which the authors could build from their own station data, would make the comparison agronomically meaningful.

### Strengths

1. **S1: The baseline question is the right one.** Fitting a phenological baseline under the same field holdout scheme as the satellite models is rare in this literature and directly useful to practitioners who must decide whether to pay for imagery.
   - **Evidence Anchor**: `text: §1 para 4 "Satellite AGB studies rarely report a phenological baseline evaluated under the same validation scheme as their satellite models"`
2. **S2: Same algorithm comparisons separate sensor from model form.** Fitting ∑GDD alone with RF and XGBoost, and showing that tree models degrade a monotonic driver into a step lookup, is an instructive result that crop modellers will recognise.
   - **Evidence Anchor**: `table: Table 5, rows GDD LM / GDD RF / GDD XGBoost`
3. **S3: Candid framing of the sampling design and operational limits.** The authors state that the design favours season progress predictors and that 47% relative error is too coarse for fertilisation or insurance decisions. This honesty is uncommon and valuable.
   - **Evidence Anchor**: `text: §4.4 "The within-season design used here systematically favors any predictor that tracks"`
4. **S4: Calendar versus thermal time comparison.** Showing that DAS transfers worse than ∑GDD across a sowing date range from May to September is a sensible sanity check.
   - **Evidence Anchor**: `table: Table 5, rows DAS LM and GDD LM`

### Weaknesses

### W1: ∑GDD is computed with daily minimum temperature fixed at 1 °C

**Problem**: Methods give GDD = (Tmax + Tmin)/2 − Tbase. In `script/06_descargar_clima.R` line 79 the daily mean is `(min(temp_minima.na.rm=T)+max(temp_maxima, na.rm=T))/2`. The typo (`.` instead of `,`) passes a named argument `temp_minima.na.rm = TRUE` to `min()`, which returns 1. Every daily mean is therefore (1 + Tmax)/2. I verified this against station 103 (Parral Norte) for 15 and 16 September 2020: Tmin/Tmax were 3.5/17.5 and 7.3/18.8 °C, so correct GDD (Tbase 2 °C) would be 8.5 and 11.0; the stored increments in `dataset.rds` are 7.25 and 7.90, exactly (1 + Tmax)/2 − 2.
**Evidence Anchor**: `dataset: data/processed/rds/dataset.rds gdd_cumsum for villa_baviera 2020-09-15 (7.25) versus script/06_descargar_clima.R line 79`
**Why it matters**: The paper is titled and framed around thermal time. The predictor actually used is a Tmax index with a damped amplitude, so every reported °C d value (1213 °C d at La Cancha, the 450 to 600 °C d ALE step, the 1030 °C d training maximum) is wrong, and the DAS versus ∑GDD comparison and all GDD sets must be rerun. Conclusions may survive, since any monotonic heat sum tracks season progress, but this cannot be assumed.
**Suggestion**: Fix the aggregation, recompute ∑GDD from hourly data (or from correct daily Tmin/Tmax, following McMaster and Wilhelm 1997, method 1 or 2 stated explicitly), rerun all analyses, and add a short check table of seasonal ∑GDD per site.
**Severity**: Critical
**Confidence**: 5 (verified by reading the code, reproducing the R behaviour, and checking against downloaded station data)

### W2: Base temperature scheme differs from Methods and depends on field observed phenology

**Problem**: Methods state Tbase 2 °C from sowing to tillering and 6 °C from stem elongation to ripening, leaving tillering to stem elongation undefined. In `script/07_procesar_datos.R` (lines 59 to 70) the switch to 6 °C happens on the observed tillering date. The two stage Tbase is also unusual: most wheat phenology and growth work uses a single base near 0 °C (Porter and Gawith 1999; McMaster and Wilhelm 1997), and APSIM type models use cardinal temperature response functions rather than abrupt switches.
**Evidence Anchor**: `text: §2.2.2 "6°C was used from stem elongation to ripening"`
**Why it matters**: (a) The description is not reproducible as written. (b) The switch date comes from field phenology observations, so the "temperature only" baseline actually carries observed phenology, which is not available for unsampled fields; this matters for the operational claim that ∑GDD "requires only daily air temperature and the sowing date". (c) A 4 °C jump in Tbase at tillering mid winter strongly compresses winter thermal time in May sown fields relative to the September sown spring field, which interacts with the habit confounding in W7.
**Suggestion**: Describe the implemented rule exactly. Add a sensitivity run with a single Tbase of 0 °C (and optionally a cardinal temperature function), and report whether the baseline ranking holds. If phenology dependent Tbase is kept, state that it needs observed or modelled tillering dates.
**Severity**: Major
**Confidence**: 5 (core expertise: thermal time phenology; code read directly)

### W3: Several AGB observations exceed physiological limits for wheat

**Problem**: Seven samples exceed 30 t ha⁻¹ (max 43.5). More telling is timing: La Cancha has a mean of 24.9 t ha⁻¹ (one quadrat at 37.7) on 9 September 2022, two weeks after the recorded start of stem elongation and three weeks before booting. From 4 August to 9 September mean AGB rose from 6.3 to 24.9 t ha⁻¹, a crop growth rate of about 52 g m⁻² d⁻¹; Villa Baviera rose from 9.0 to 20.0 t ha⁻¹ in 20 days (about 55 g m⁻² d⁻¹). With radiation use efficiency of roughly 3 g MJ⁻¹ of intercepted PAR as an upper bound for C3 cereals (Sinclair and Muchow 1999) and late winter PAR in central Chile, attainable rates are of the order of 20 to 30 g m⁻² d⁻¹ in September. Post peak, mean AGB then falls by 6 to 10 t ha⁻¹ within three to five weeks (Hidango North 28.2 to 21.8; La Cancha 33.8 to 23.7), which is far more than leaf shedding during ripening explains.
**Evidence Anchor**: `dataset: data/processed/rds/dataset.rds, la_cancha 2022-2023, fecha 2022-09-09 (mean 24.9, max 37.7 t ha⁻¹, gdd_cumsum 590)`
**Why it matters**: These points dominate the largest errors (the authors note this in §4.6) and therefore drive the RMSE ranking among configurations. If they reflect fresh weight contamination, incomplete drying, edge or row placement effects in a 0.25 m² quadrat, or a unit conversion slip, the comparison between sensors is partly a comparison of how each model handles measurement error.
**Suggestion**: Report grain yield and harvest index per field as a plausibility check (an AGB of 24 to 34 t ha⁻¹ at maturity implies grain yields of roughly 10 to 14 t ha⁻¹ at typical harvest indices, which should be stated and verified against farm records). Describe quadrat placement relative to rows, cutting height and drying verification. Flag implied crop growth rates above an RUE ceiling computed from the stations' own solar radiation records, and rerun the main comparison with those dates excluded or down weighted.
**Severity**: Major
**Confidence**: 4 (core expertise: crop growth physiology; radiation ceiling is an order of magnitude estimate, not computed from site radiation)

### W4: "153 destructive samples" includes 21 assumed zeros

**Problem**: `script/07_procesar_datos.R` appends AGB = 0 at the sowing date for each sampling point. These are assumptions, not harvested quadrats. The abstract and §2.2.1 describe all 153 as collected destructive samples, while §2.3.4 acknowledges 21 zero samples at sowing.
**Evidence Anchor**: `text: Abstract "153 destructive samples, 2020 to 2023"`
**Why it matters**: Readers judge the field effort and the error budget from this number. The zeros also carry no information about sensors or condition, and they inflate pooled R².
**Suggestion**: State "132 destructive samples plus 21 assumed zero AGB values at sowing" in the abstract and §2.2.1, and present the zero excluded metrics as primary or at least side by side in Table 5.
**Severity**: Major
**Confidence**: 5 (code and text read directly)

### W5: "Thermal time dominates" restates a known agronomic relation; the evaluation cannot isolate the remote sensing signal

**Problem**: Within a season, AGB accumulation tracks thermal time; this is the foundation of every crop model since the 1970s. In this dataset the site season × sampling date means explain 92% of AGB variance (89% after removing the zeros; my one way ANOVA on `dataset.rds`), and within date residual SD is 3.7 t ha⁻¹. A predictor that only knows where a sample sits in the season can reach most of the explainable variance, and the leftover within date spread is of similar magnitude to plausible quadrat sampling error. The pooled LOSSO metrics therefore mostly measure trajectory reconstruction.
**Evidence Anchor**: `text: §4.1 "This is expected from crop physiology: AGB accumulates with thermal time through most of the season"`
**Why it matters**: The paper's main lesson for IJRS readers depends on whether satellites add condition information beyond season progress. §4.4 recognises this but no analysis tests it, so the headline risks being read as a remote sensing finding when it is a property of the sampling design and of crop physiology.
**Suggestion**: Add an analysis on the anomaly scale that the data already permit: (a) evaluate each model on the residual from the ∑GDD baseline (does the satellite model predict which held out fields, or which dates, lie above or below the thermal time expectation?); (b) report metrics stratified by growth stage or ∑GDD window; (c) report within date skill (correlation of predictions with observations among the five or six points sampled on the same day), which no season progress predictor can achieve. Reframe the headline accordingly: what is new is the size of the satellite increment over a known physiological baseline, not the dominance of thermal time itself.
**Severity**: Major
**Confidence**: 4 (core expertise on the physiology; the variance partition is my own quick computation)

### W6: The agronomic reference baseline (RUE or crop model) is missing, and the linear GDD form is physiologically wrong at the ends of the season

**Problem**: Crop modellers would not use a straight line in ∑GDD as the reference. AGB versus thermal time is sigmoidal or expolinear (Goudriaan and Monteith 1990), and the linear baseline predicts negative AGB near sowing (Figure 5, blue lines start near −2 t ha⁻¹) and keeps rising after the observed plateau. The standard way to combine satellites with weather is Monteith's light use efficiency formulation, AGB = RUE × ∑(fAPAR × PAR) (Monteith 1977), with fAPAR from NDVI or LAI, or assimilation of satellite LAI into WOFOST, APSIM, AquaCrop or similar models (Jin et al. 2018; Huang et al. 2019). The AgroMet stations used here record solar radiation (the raw station files include `solar_radiation_wm2`), so an RUE baseline is feasible with data already in hand. The cumulative VIs that rank highest in importance (Figure 7) are crude, radiation free versions of ∑fAPAR.
**Evidence Anchor**: `figure: Figure 5, GDD | LM out of fold trajectories below zero at DAS 0 and rising beyond the observed plateau`
**Why it matters**: Without an RUE baseline the paper cannot say whether satellites failed, or whether they were used in a form that discards the physics (no radiation, no efficiency term). The more relevant question for the agronomy and insurance community is whether satellite fAPAR improves an RUE model over a thermal time model, which is a well posed remote sensing contribution.
**Suggestion**: Add (i) a logistic or expolinear ∑GDD baseline fitted on training folds, and (ii) an RUE baseline with ∑(fAPAR × PAR) using S2 or PS NDVI derived fAPAR and station radiation, with a single RUE fitted in training folds. Discuss data assimilation into crop models in the Introduction and Discussion as the established alternative to pure ML.
**Severity**: Major
**Confidence**: 4 (core expertise: RUE and crop models; effect size on results unknown)

### W7: Wheat habit and sowing season are confounded with one fold, which drives the DAS versus GDD contrast

**Problem**: Villa Baviera is the only spring wheat, sown in September, and the ∑GDD baseline performs best there (R² 0.83). Thermal time without vernalisation or photoperiod terms treats winter and spring habits identically, and the two stage Tbase (W2) amplifies the difference between May and September sowings.
**Evidence Anchor**: `text: §4.1 "in particular between the winter wheat sites, sown in May, and the spring wheat site of Villa Baviera"`
**Why it matters**: The claim that thermal time transfers better than calendar time rests substantially on one fold that differs in habit, location and season. The authors note confounding in §4.6, but §4.1 still interprets the DAS versus GDD gap physiologically.
**Suggestion**: Show the DAS versus GDD comparison per fold, and temper §4.1 to say the advantage is driven mainly by the spring sown field. Consider photothermal time as a sensitivity check.
**Severity**: Minor
**Confidence**: 4 (core expertise: wheat phenology)

### W8: Operational claims about gridded temperature and real time use are untested

**Problem**: §4.5 says ∑GDD "can be computed for any field with a nearby weather station or a gridded temperature product such as ERA5-Land". Villa Baviera's station is 20.9 km away, the GDD rule needs an observed tillering date (W2), the sowing date must be known, and the optical predictors use two sided Kalman smoothing. Reanalysis temperature in central Chile's coastal and Andean foothill terrain can carry biases that accumulate in a seasonal heat sum.
**Evidence Anchor**: `text: §4.5 "a gridded temperature product such as ERA5-Land"`
**Why it matters**: Advisory and insurance users need to know which inputs are available at decision time for an unsampled field. Sowing date and phenology are usually the hardest to obtain.
**Suggestion**: Either test ∑GDD from ERA5-Land (Muñoz-Sabater et al. 2021) against station ∑GDD for the four fields, or soften the sentence. List explicitly the inputs needed in real time (sowing date, phenology for the Tbase switch, causal filter for VIs), and mention satellite based sowing or emergence detection as a route.
**Severity**: Minor
**Confidence**: 4 (practical experience with advisory data chains)

### W9: Relative error is expressed against a mean that includes zeros and the early season

**Problem**: RMSE of 5.8 to 6.2 t ha⁻¹ is expressed as 47 to 51% of mean AGB. For the decisions named (variable rate N, insurance), what matters is error at the relevant stage. Nitrogen diagnosis via the critical N dilution curve needs AGB between roughly 1 and 10 t ha⁻¹ during stem elongation (Justes et al. 1994), where an absolute error of 6 t ha⁻¹ is larger than the signal; insurance needs yield, not AGB.
**Evidence Anchor**: `text: §4.5 "corresponds to about 47% of the mean observed AGB"`
**Why it matters**: Practitioners will read 47% as uniform; the stage specific picture is worse early and possibly better late, and that determines usefulness.
**Suggestion**: Report RMSE and relative RMSE by growth stage (tillering, stem elongation to flowering, grain filling), and link each to the decision it would serve.
**Severity**: Minor
**Confidence**: 4 (core expertise: N management and insurance applications)

### Detailed Comments

#### Assumption Audit

- **Explicit assumptions**: ∑GDD is treated as a temperature only descriptor of season progress. In implementation it also encodes observed tillering dates and a nonstandard base temperature, and it currently omits Tmin (W1, W2).
- **Implicit assumptions**: (a) The destructive AGB values are accurate enough to rank models whose differences are 0.2 to 1 t ha⁻¹; W3 and the within date SD of 3.7 t ha⁻¹ question this. (b) A pooled R² across the full season is the relevant skill measure; for users, skill on deviations from the expected trajectory is what matters (W5). (c) A linear relation of AGB with thermal time is an adequate null model (W6).
- **Paradigmatic assumptions**: The paper stays inside the empirical regression paradigm, comparing ML predictor sets. Crop science would frame the problem as state estimation in a dynamic growth model with satellites constraining canopy state. This framing would also give physically constrained predictions (no negative AGB, no growth beyond the radiation ceiling).

#### Cross-Disciplinary Connections

- **Parallel research**: Remote sensing data assimilation into crop models (Jin et al. 2018; Huang et al. 2019) and simulation calibrated empirical approaches such as SCYM (Lobell et al. 2015) address the same transferability problem by putting crop physiology into the model rather than learning it from four fields.
- **Borrowing opportunities**: Light use efficiency (Monteith 1977; Sinclair and Muchow 1999) gives a physically bounded baseline and a plausibility screen for the field data. Expolinear growth functions (Goudriaan and Monteith 1990) give a better shaped thermal time baseline. The critical N dilution curve (Justes et al. 1994; Lemaire et al. 2008) defines the AGB accuracy that N advice actually needs.
- **Methodological borrowing**: Residual (anomaly) evaluation relative to the phenological expectation, and stage stratified error reporting, are routine in crop model evaluation and would sharpen the remote sensing question here.

#### Practical Impact

- **Real-world application**: The take home for practitioners, "start from a thermal time curve and demand that satellites beat it on new fields", is useful and transferable to other crops. The paper should also tell them what accuracy is needed for which decision (W9).
- **Implementation feasibility**: Inputs at decision time (sowing date, phenology, causal filtering, representative temperature) are the practical bottleneck and deserve one explicit paragraph (W8).
- **Stakeholders**: Insurers and advisers in Chile mostly work with yield and with stage specific canopy status; neither is evaluated here. Mentioning how AGB estimates would connect to yield (harvest index) and N status would help these readers.

#### Broader Implications

- **Ethical dimensions**: No concerns identified. The candid statement that current accuracy is insufficient for insurance protects against premature use in loss assessment, which could harm farmers.
- **Social impact**: Not a central issue for this paper.
- **Future directions**: Larger multi season networks with stage stratified sampling; RUE and crop model assimilation baselines; satellite detection of sowing date to remove the main operational input barrier.

### Cross-Disciplinary Reading Recommendations

- Goudriaan, J., and J. L. Monteith. 1990. "A Mathematical Function for Crop Growth Based on Light Interception and Leaf Area Expansion." Annals of Botany 66: 695 to 701. Shape of the AGB versus time relation for a better baseline.
- Huang, J., et al. 2019. "Assimilation of Remote Sensing into Crop Growth Models: Current Status and Perspectives." Agricultural and Forest Meteorology 276 to 277: 107609. Established alternative to pure ML.
- Jin, X., L. Kumar, Z. Li, H. Feng, X. Xu, G. Yang, and J. Wang. 2018. "A Review of Data Assimilation of Remote Sensing and Crop Models." European Journal of Agronomy 92: 141 to 152.
- Justes, E., B. Mary, J. M. Meynard, J. M. Machet, and L. Thelier Huché. 1994. "Determination of a Critical Nitrogen Dilution Curve for Winter Wheat Crops." Annals of Botany 74: 397 to 407. Defines AGB range and accuracy relevant to N advice.
- Lobell, D. B., D. Thau, C. Seifert, E. Engle, and B. Little. 2015. "A Scalable Satellite Based Crop Yield Mapper." Remote Sensing of Environment 164: 324 to 333. Crop model informed transferable satellite estimation.
- McMaster, G. S., and W. W. Wilhelm. 1997. "Growing Degree Days: One Equation, Two Interpretations." Agricultural and Forest Meteorology 87: 291 to 300. Explicit GDD computation choices (W1, W2).
- Monteith, J. L. 1977. "Climate and the Efficiency of Crop Production in Britain." Philosophical Transactions of the Royal Society B 281: 277 to 294. RUE framework.
- Muñoz-Sabater, J., et al. 2021. "ERA5-Land: A State of the Art Global Reanalysis Dataset for Land Applications." Earth System Science Data 13: 4349 to 4383.
- Porter, J. R., and M. Gawith. 1999. "Temperatures and the Growth and Development of Wheat: A Review." European Journal of Agronomy 10: 23 to 36. Base temperatures for wheat.
- Sinclair, T. R., and R. C. Muchow. 1999. "Radiation Use Efficiency." Advances in Agronomy 65: 215 to 265. Ceiling for crop growth rate plausibility checks.

### Questions for Authors

1. After correcting Tmin (W1), does ∑GDD LM remain the best baseline and does S1+GDD RF still improve on it in three of four folds?
2. What grain yield and harvest index were recorded in each field? Do they support maturity AGB of 22 to 34 t ha⁻¹?
3. Were AGB samples weighed after confirming constant dry weight, and how were quadrats placed relative to rows?
4. Phenology dates in `script/07_procesar_datos.R` are taken from a column named `date_sentinel`. Were stage dates observed in the field or assigned to the nearest Sentinel acquisition? This affects the Tbase switch.
5. Does any satellite configuration show positive skill on residuals from the ∑GDD baseline, or on the ranking of points sampled on the same date?
6. The ALE of ∑GDD is flat beyond about 600 °C d while the LM baseline is linear to 1200 °C d. Would a plateau baseline outperform both?

### Minor Issues

- §2.2.2: state the GDD method (daily mean from Tmin/Tmax versus hourly integration) and whether an upper threshold was applied.
- §2.2.1: specify dry weight verification and quadrat placement; 0.25 m² is small for drilled wheat and contributes to within date variance.
- Figure 2: labels "S" for sowing and "S₁…Sₙ" for samples remain easy to confuse; consider "Sow".
- §2.1: "temporal distribution of sampling dates" would benefit from a table of sampling dates with Zadoks stage per field, which agronomists will want.
- The 20.9 km station distance at Villa Baviera should be discussed as a source of ∑GDD error.

---

Recommendation signal: **Major Revision**. The baseline question is valuable and honestly framed, but the central predictor is miscomputed (W1, Critical but repairable by rerunning), several AGB observations exceed physiological limits (W3), and the analysis does not yet distinguish a remote sensing finding from the known physiology of thermal time (W5, W6). All are fixable with existing data. I do not make the final decision.
