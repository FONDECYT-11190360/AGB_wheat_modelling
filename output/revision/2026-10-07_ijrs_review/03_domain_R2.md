# Domain Review Report (Peer Reviewer 2)

## Manuscript Information

- **Title**: Thermal time baselines for evaluating Sentinel-1, Sentinel-2 and PlanetScope predictors of wheat above-ground biomass under spatial cross-validation
- **Target venue**: International Journal of Remote Sensing, research article (author stated)
- **Review date**: 2026-10-07
- **Review round**: Round 1 (simulated panel)
- **Criteria binding**: `criteria_binding_unavailable` (no venue criteria manifest supplied; no venue alignment claim is made here)

## Reviewer Identity

Senior researcher in SAR and optical crop monitoring (Sentinel-1 and Sentinel-2 retrieval of wheat biomass, LAI and phenology), familiar with the cereal C-band backscatter literature. Blind spot declared: statistical inference with four folds is left to Reviewer 1.

## Review Focus

Whether the poor transfer of Sentinel-1 (S1) and PlanetScope (PS) predictors, and the modest performance of Sentinel-2 (S2), reflect sensor limits or processing choices; whether the physical explanation of S1 behaviour is sound; and whether the literature on wheat AGB retrieval, SAR and optical fusion, physically based retrieval, spatial validation and phenological baselines is adequately covered. I read the manuscript, the supplement, and the processing scripts `01`, `02`, `03`, `04`, `05`, `07` and `31`, and I inspected the extracted predictor tables (`data/processed/rds/vi_extract.rds`, `data/processed/rds/dataset.rds`) to verify processing claims.

## Overall Recommendation

**Major Revision**

## Confidence Score

4 (core expertise in S1/S2/PS processing and crop backscatter; the four fold inference questions are outside my remit)

Confidence is an uncertainty/scope disclosure only; it never changes consensus counts, severity, decision bearing, or arbitration.

## Calibration Status

`NOT_CALIBRATED`

## Criterion-Bound Judgements

| Dimension / criterion | Criterion source | Judgement | Evidence anchors | Rationale | Uncertainty or scope limit | Decision bearing? |
|---|---|---|---|---|---|---|
| Domain accuracy of sensor processing | Reviewer card focus (1) and (2) | DOES_NOT_MEET | dataset: `dataset.rds` S2 and PS reflectance per site season; text: section 2.2.4 | Two radiometric errors (S2 baseline 04.00 offset; implausible PS reflectance at Villa Baviera) are aligned with the fold structure and contaminate every optical result | Verified on extracted predictor tables, not on raw scenes (raw rasters not in the repository) | Yes, repairable by reprocessing and rerunning |
| Fairness of conclusions about S1 | Reviewer card focus (1) | PARTLY_MEETS | text: section 2.2.4 and 4.2 | Limitations are disclosed honestly, but the S1+GDD gain has an untested noise explanation and the S1 data are processed below field practice | Gain sizes are within fold noise (R1 remit) | Yes |
| Physical interpretation | Reviewer card focus (1), (2) | PARTLY_MEETS | text: section 4.2, 4.3 | VV/VH phenology narrative is broadly correct; cumulative index interpretation is partly an artefact of how the sums are built | none identified | Yes |
| Literature integration | Reviewer card focus (3) | PARTLY_MEETS | absence: Introduction and Discussion | Spatial CV is cited, but the SAR crop literature, physically based retrieval, PlanetScope harmonisation and the spatial validation debate are thin or absent | Search limited to well known works | No alone; yes jointly with W5 |
| Contribution / significance | Reviewer card focus | MEETS | text: Abstract last sentence | A phenological baseline under grouped CV is a useful and under reported benchmark for the RS community | Contingent on corrected inputs | No |

Do not total, weight, average, or mechanically map these judgements to the recommendation.

## Summary Assessment

The manuscript asks a good and under examined question: how much do S1, S2 and PS predictors add to accumulated thermal time for in-season wheat AGB at fields held out of training? The reframing around a phenological baseline under leave-one-site-season-out validation is, in my view, the paper's real contribution, and the authors are commendably candid about the S1 preprocessing shortcuts and the small number of site seasons. However, my verification of the processing chain found two radiometric defects that directly undermine the sensor comparisons. First, S2 Level-2A data from the Planetary Computer acquired after 25 January 2022 (processing baseline 04.00) were scaled by 1/10000 without removing the +1000 digital number offset, so Hidango South and La Cancha (and the end of Hidango North) carry a reflectance bias of about +0.1 in every band, which compresses NDVI to a maximum of about 0.65 to 0.72 there versus 0.93 at the other two sites (visible in figure 2). Second, PS values for Villa Baviera are not plausible surface reflectance (median green about 0.42, EVI up to about 20, cumulative EVI six times larger than elsewhere). Both artefacts split the four folds into radiometrically incompatible groups, which is exactly what LOSSO-CV penalises. The conclusions that S2 and PS "transfer worse than thermal time", the ablation, and the importance and ALE results for the All model therefore cannot be accepted as statements about the sensors until the data are corrected and the analysis rerun. The S1 conclusions are more defensible but still rest on unfiltered single pixels from mixed orbits, and the S1+GDD gain has a plausible noise based explanation that should be tested directly. I recommend Major Revision; the defects are repairable, but the results section may change substantially.

## Strengths

### S1: A phenological baseline evaluated under the same grouped validation as the satellite models

Reporting DAS and ∑GDD baselines with the same LOSSO-CV and, importantly, fitting ∑GDD with RF and XGBoost so that sensor and algorithm effects can be separated is a design choice that much of the wheat AGB remote sensing literature lacks. It gives readers a reference point for how much of a reported R² is seasonal trend.

**Evidence Anchor**: text: section 2.3.4 "Fitting $\sum$GDD alone with RF and XGBoost allows each satellite set combined with $\sum$GDD to be compared with thermal time under the same algorithm"

### S2: Honest disclosure of S1 preprocessing shortcuts and of the S1B loss

The Methods state explicitly that several relative orbits were mixed, that no speckle filter or incidence angle normalisation was applied, and that S1B failed in December 2021. This transparency allows the reader to judge the S1 results, which is rare.

**Evidence Anchor**: text: section 2.2.4 "Sentinel-1 data were used without speckle filtering or incidence angle normalization, which are standard preprocessing steps for SAR vegetation applications"

### S3: Physically reasonable account of why S1 alone cannot locate samples in the season

The non monotonic VV response after stem elongation and the volume driven VH and VH/VV response are correctly described and consistent with the cereal backscatter literature; this is a sound explanation for why S1 helps only when ∑GDD supplies season position.

**Evidence Anchor**: text: section 4.2 "VV backscatter decreases after stem elongation, as the vertical stems attenuate the vertically polarised signal"

### S4: Removal of PS red edge predictors that were missing for whole site seasons

Excluding predictors that would otherwise be imputed for an entire held-out unit is correct reasoning and avoids a subtle leakage of imputed structure.

**Evidence Anchor**: text: section 2.2.3 "Because values imputed for an entire site-season cannot carry information about that site-season, these four predictors were excluded"

## Weaknesses

### W1: Sentinel-2 reflectance after 25 January 2022 still carries the processing baseline 04.00 offset

**Problem**: `script/02_calcular_vis.R` converts S2 L2A digital numbers with `band / 10000` and no offset correction. Since 25 January 2022 (processing baseline 04.00), L2A products carry `BOA_ADD_OFFSET = -1000`, so surface reflectance is (DN − 1000)/10000; the Planetary Computer serves these data unharmonised and documents a `harmonize_to_old` step for exactly this reason (Microsoft Planetary Computer, "Sentinel-2 baseline change" example notebook; ESA STEP forum notice "Introduction of additional radiometric offset in PB04.00 products"). In the modelling table, the 5% quantile of S2 B2 is 0.022 at Hidango North (pre-change) and 0.018 at Villa Baviera, but 0.126 at Hidango South and 0.116 at La Cancha (post-change); the median of S2 B1 is 0.028 and 0.030 versus 0.136 and 0.135. Within Hidango North, the B2 floor jumps from 0.018 before 25 January 2022 to 0.118 after. The 95% quantile of S2 NDVI is 0.93 at the two pre-change sites and 0.62 to 0.71 at the two post-change sites; figure 2 shows exactly this (peak NDVI about 0.64 at Hidango South and about 0.72 at La Cancha, versus about 0.92 elsewhere). The reflectance floor filter (values < 0.01 discarded) cannot catch this bias.
**Evidence Anchor**: dataset: `data/processed/rds/dataset.rds` and `vi_extract.rds`, S2 B1, B2, NDVI distributions by site season (pre-change floor about 0.02, post-change floor about 0.12)
**Why it matters**: The bias is a site-season constant aligned with the fold structure: two folds are "old radiometry" and two are "new radiometry". Under LOSSO-CV, a model trained on a mix and tested on the other group extrapolates across a radiometric step, so the S2 RMSE values (8.08 and 7.93 t ha⁻¹), the S2+GDD, S2+W, S2+W+S1 and All results, the ablation in table 6, and the reading that S2 adds nothing transferable are all confounded by a processing error, not a sensor limit. Ratio indices (NDVI, SR, CI_red, NDRE) are distorted nonlinearly by an additive offset, and every cumulative S2 predictor (for example ∑S2 B1, ∑NDRE3) acquires a site specific slope. Figure 2 also visually suggests lower canopy vigour at Hidango South and La Cancha, which readers will misinterpret.
**Suggestion**: Subtract 1000 from DN for all L2A items whose `s2:processing_baseline` is ≥ 04.00 (or apply the Planetary Computer harmonisation), recompute all indices, cumulative sums and figure 2, and rerun the full LOSSO-CV. Report the correction in section 2.2.4. Consider also checking for the Sen2Cor version change between seasons.
**Severity**: Critical
**Confidence**: 5, core expertise: Sentinel-2 L2A radiometry; verified directly on the extracted predictor values

### W2: PlanetScope values at Villa Baviera are not plausible surface reflectance

**Problem**: In the modelling table, PS reflectance at Villa Baviera has a median green (B3) of 0.417 versus 0.046 to 0.062 at the other site seasons; the 5% to 95% range of blue is 0.37 to 0.65 and of red 0.19 to 0.77, values impossible for a wheat canopy (blue reflectance of green vegetation is typically 0.02 to 0.05). PS EVI reaches about 20.8 and ∑PS EVI about 742, against maxima near 1 and 97 to 124 elsewhere, and PS NDVI at Villa Baviera peaks at about 0.64 while S2 NDVI over the same points peaks at 0.93. The magnitudes are consistent with a different product or scaling (for example a radiance asset matched by `grep('Analytic', ...)` in `02_calcular_vis.R`, or 4-band Dove-C data with an incorrect scale factor) rather than with Dove versus SuperDove spectral differences. The manuscript states that Level 3B surface reflectance was used for all fields.
**Evidence Anchor**: dataset: `data/processed/rds/dataset.rds`, Villa Baviera PS_B3 median 0.417, PS_EVI max 20.8, PS_EVI_cumsum max 742
**Why it matters**: PS predictors in the Villa Baviera fold cannot transfer from or to the other three folds, which by itself explains much of the PS failure (negative mean R², SD of RMSE above 4 t ha⁻¹) and the PS+W, PS+GDD and All results. It also contaminates the interpretation: the two most important individual predictors of the All model are ∑PS GRVI and ∑PS EVI, and the ∑PS EVI ALE profile in figure 8 spans 0 to about 750 with a step "almost entirely within its lowest values", which is the signature of one site season sitting far outside the others' range rather than an agronomic effect.
**Suggestion**: Verify the asset type and scale factor of every Villa Baviera PS scene (product metadata, `AnalyticMS_SR` versus `AnalyticMS`), rebuild PS predictors from surface reflectance, cross check PS against S2 at near coincident dates for every site season (a scatterplot of PS versus S2 red and NIR reflectance per site season would belong in the supplement), and rerun the analysis. If harmonisation across Dove-C, Dove-R and SuperDove is then still a concern, apply Planet's scene level normalisation and harmonisation tool or an S2 based cross calibration (Kington and Collison 2022, Planet white paper; Houborg and McCabe 2018, Remote Sensing of Environment 209).
**Severity**: Critical
**Confidence**: 5, core expertise: PlanetScope radiometry; verified directly on the extracted predictor values

### W3: The S1+GDD gain has an untested noise based explanation

**Problem**: The headline configuration (RF, S1+GDD) improves on RF with ∑GDD alone by about 30%. The authors note that S1 values vary among points sampled on the same date and may simply break the step functions that trees fit to the 21 to 23 distinct ∑GDD values. With unfiltered single 10 m GRD pixels, the per-pixel speckle standard deviation is on the order of 1.5 to 2 dB in IW GRDH data, so a large share of the within-date S1 variance is speckle. Speckle is effectively a random tie-breaker, and a random continuous covariate can let RF interpolate between ∑GDD steps. The manuscript acknowledges that "the present data cannot separate this effect", but the separation is cheap to test.
**Evidence Anchor**: text: section 4.2 "part of this gain may come from breaking the steps that tree models fit to $\sum$GDD, and the present data cannot separate this effect"
**Why it matters**: The only configuration that beats the baseline, and the abstract's statement that adding S1 reduces RMSE by about 30% with RF and 23% with XGBoost, may reflect an algorithmic artefact rather than canopy information. If so, the main positive message about S1 for wheat AGB is unsupported.
**Suggestion**: Add control runs under the same LOSSO-CV: (a) ∑GDD plus three Gaussian noise columns, (b) ∑GDD plus S1 permuted within sampling date (keeps the marginal distribution, removes point level structure), (c) ∑GDD plus S1 permuted across dates within site season, and (d) ∑GDD plus S1 averaged per sampling date. If (a) or (b) reproduces most of the gain, the S1 claim must be withdrawn. Also report S1 temporal profiles per site season against Zadoks stage, which domain readers will expect.
**Severity**: Major
**Confidence**: 4, core expertise: SAR speckle statistics; RF behaviour on tied predictors is adjacent

### W4: Sentinel-1 processing is below field practice in ways that bias the S1 comparison toward failure

**Problem**: The S1 series mixes at least two relative orbits with different incidence angles (and the code takes all `COPERNICUS/S1_GRD` acquisitions, so possibly ascending and descending passes), uses sigma0 without radiometric terrain flattening on undulating terrain at Hidango, applies no speckle filter, and extracts a single pixel per plot. Median VV differs by about 3 dB between the two Hidango fields (−11.1 versus −14.1 dB), a difference of the same order as the seasonal VV dynamic in wheat; the manuscript does not show whether it is crop, soil moisture, or acquisition geometry. Temporal "interpolation" is a nearest acquisition hold, so on any given day the predictor may come from either geometry. The Discussion lists the remedies, but the analysis does not apply any of them, although all are routine in GEE (incidence angle normalisation to a reference angle with a cosine or linear correction, single relative orbit selection, Refined Lee or multi-temporal filtering, and averaging over a 3 × 3 window or the field area). The standard practice is documented in the S1 crop literature (Veloso et al. 2017, Remote Sensing of Environment 199; Vreugdenhil et al. 2018, Remote Sensing 10, 1396; Harfenmeister, Spengler and Weltzien 2019, Remote Sensing 11, 1569).
**Evidence Anchor**: text: section 2.2.4 "Each series therefore combined at least two relative orbits, and with them different incidence angles"
**Why it matters**: The paper draws a sensor level conclusion (S1 alone is the weakest predictor set; S1 group importance is near zero in the All model) from data that do not represent what S1 can deliver. Readers in the SAR community will discount the S1 results, and the IJRS audience will expect at least one processed variant.
**Suggestion**: Rerun S1 with (i) one relative orbit per site (or each orbit normalised to a common incidence angle), (ii) a speckle filter, and (iii) a spatial mean over a window or field buffer. Report S1 alone, S1+GDD and All under both the current and the improved processing, so that "processing choice" and "sensor limit" are separated. If the improved S1 still fails, the conclusion becomes much stronger. Also note that IW GRDH has about 20 × 22 m resolution at 10 m pixel spacing; table 2 should say "10 m pixel spacing".
**Severity**: Major
**Confidence**: 5, core expertise: Sentinel-1 crop monitoring

### W5: The ∑GDD baseline uses an observed in-field phenological date, and the description does not match the code

**Problem**: The text says base temperature is 2 °C from sowing to tillering and 6 °C from stem elongation to ripening, and the Discussion says the baseline "requires only daily air temperature and the sowing date". In `script/07_procesar_datos.R`, the switch from 2 °C to 6 °C occurs at the observed tillering date recorded in the field for each site season (`fenologia %in% c('SOWING','TRILLERING')`), and the period from tillering to stem elongation, which the text leaves undefined, uses 6 °C.
**Evidence Anchor**: text: section 4.5 "it requires only daily air temperature and the sowing date"
**Why it matters**: The baseline against which every sensor is judged uses a ground observed phenological event at the held-out field, information not available operationally and not available to the satellite models. This flatters the baseline relative to the sensors, and the operational claim is inaccurate. It also creates a site specific kink in ∑GDD that RF can exploit.
**Suggestion**: Describe the rule exactly as implemented, and add a baseline with a single base temperature (0 °C is common for wheat) or with the switch date predicted from thermal time, so that the baseline needs only temperature and sowing date. Report whether the ranking of sensors versus baseline changes.
**Severity**: Major
**Confidence**: 4, core expertise in crop phenology and thermal time; verified in code

### W6: Cumulative indices are constructed in a way that guarantees they act as clocks, and the interpretation presents this as a finding

**Problem**: Cumulative sums start at sowing and integrate the raw daily VI, including the bare soil and senescent baseline (S2 NDVI is about 0.2 at sowing). ∑VI therefore grows roughly as DAS multiplied by the mean VI, and cumulative bands such as ∑S2 B1 (the 60 m coastal aerosol band) have no canopy meaning at all and are essentially calendar time with a site specific slope; with W1 uncorrected that slope differs by a factor of about five between pre- and post-change sites. The Discussion then concludes that cumulative indices "acted mainly as alternative measures of season progress", which follows from their construction rather than from the data.
**Evidence Anchor**: text: section 4.2 "cumulative indices whose ALE profiles increased monotonically, which indicates that they acted mainly as alternative measures of season progress"
**Why it matters**: The paper's general recommendation about cumulative indices is sound in spirit, but as written it is partly circular and it penalises optical sensors through a feature engineering choice. A fairer test of optical information is an integral above a soil or start-of-season baseline, or a VI integral expressed per unit thermal time, which removes the clock component (in the spirit of the integrated VI approaches cited from Peroni Venancio et al. 2020).
**Suggestion**: Replace cumulative sums with integrals of (VI − VI at sowing) or with ∑VI/∑GDD, drop cumulative raw bands such as ∑B1, and show the correlation of each cumulative predictor with DAS and ∑GDD per site season. Then report whether optical cumulative predictors add to ∑GDD.
**Severity**: Major
**Confidence**: 4, core expertise in optical time series features

### W7: The optical time series processing is described inaccurately

**Problem**: Several statements do not match the implementation. (a) "Kalman smoothing": `imputeTS::na_kalman` in `03_suavizar_vis.R` only imputes missing values; observed values pass through unchanged, so no smoothing occurs, and the state space model is run on the acquisition index rather than on calendar time, so irregular spacing is ignored. Daily values are then linear interpolations. It is applied to the indices, not to the "spectral bands" as stated. (b) S2 data were assembled with `gdalcubes` into 5-day composites (`dt = "P5D"`) on a 0.0001° geographic grid with nearest neighbour resampling of the 20 m and 60 m bands; the "208 S2 images" are therefore 5-day cube slices (about 300 days / 5 = 60 for Hidango North), not acquisitions. (c) All rasters are reprojected to EPSG:32719 with terra's default bilinear resampling before extraction, so the statement "value of the raster cell containing the point ... no spatial averaging" is not correct. (d) Specific scenes were removed manually per site (`03_suavizar_vis.R`) and VIs were clamped to [−5, 50], neither of which is reported.
**Evidence Anchor**: text: section 2.2.4 "spectral bands were interpolated using a Kalman smoothing approach"
**Why it matters**: Reproducibility and correct interpretation of the optical predictors; the unsmoothed series retain residual cloud and shadow contamination, which adds noise specifically to daily S2 and PS predictors.
**Suggestion**: Rewrite section 2.2.4 to describe gap filling (not smoothing), the 5-day compositing and grid, the reprojection method, the scene exclusions with reasons, and the clamping. If smoothing is intended, use a time aware smoother (for example Whittaker or Savitzky-Golay on calendar days) and report it.
**Severity**: Major
**Confidence**: 5, verified in code

### W8: Positional uncertainty, not plot size, governs single pixel extraction

**Problem**: The manuscript justifies nearest pixel extraction because a 0.25 m² plot is "fully contained within a single pixel". The relevant quantity is the positional error of the plot coordinates (handheld GNSS typically a few metres) combined with sensor geolocation error (S2 multi-temporal registration of the order of 10 m at 2σ before the 2021 GRI reprocessing, PS orthorectification below 10 m RMSE), together with the 20 to 22 m true resolution of S1 GRD. With 3 m PS pixels, the extracted pixel may not contain the plot.
**Evidence Anchor**: text: section 2.2.4 "each sampling plot is fully contained within a single pixel for every sensor"
**Why it matters**: Point to pixel mismatch adds error that is largest for PS (finest pixels) and S1 (speckle), the two sensors judged weakest, and it weakens the sensor level conclusions.
**Suggestion**: State the GNSS device and accuracy; repeat extraction with a buffer (for example 10 m for S2 and PS, 30 m for S1) and report sensitivity of the main results. Cite Frazier and Hemingway (2021, Remote Sensing 13, 3930) for PS geolocation.
**Severity**: Minor
**Confidence**: 4, core expertise

### W9: The upper AGB values are implausible for wheat and need verification

**Problem**: Seven samples exceed 30 t ha⁻¹ dry matter (maximum 43.5 t ha⁻¹ in La Cancha), and figure 2 shows within-date spreads of about 15 to 20 t ha⁻¹ among five points and large drops in mean AGB between consecutive late season dates (for example Hidango North from about 28 to 22 t ha⁻¹; La Cancha from about 34 to 25 t ha⁻¹). Total above ground dry matter of high yielding wheat at maturity rarely exceeds 20 to 25 t ha⁻¹. A 0.25 m² quadrat multiplies any border or row placement error by 40.
**Evidence Anchor**: figure: figure 2, Hidango North S5 and La Cancha S5 points above 30 t ha⁻¹
**Why it matters**: The authors note these samples dominate the largest errors; if they reflect sampling error (edge effects, row count in a small quadrat, incomplete drying) rather than crop state, they inflate RMSE for every model and reduce the ability of any sensor to show skill. They also make the comparison to literature RMSE values (section 4.5) misleading.
**Suggestion**: Report quadrat placement relative to rows, drying protocol checks, and whether fresh or dry mass was recorded; flag or justify these samples, and report results with them down-weighted or removed as a sensitivity analysis. Consider using a larger sampling area in future campaigns.
**Severity**: Major
**Confidence**: 3, adjacent expertise: field agronomy rather than remote sensing

### W10: Key domain literature is missing

**Problem**: The Introduction and Discussion do not cite (a) physically based or semi-empirical SAR retrieval, notably the water cloud model (Attema and Ulaby 1978, Radio Science 13, 357 to 364), which is the standard framework for separating soil and canopy contributions to C-band backscatter and directly addresses the soil moisture confound the authors raise; (b) the S1 cereal monitoring studies that define the field's expectations for VV, VH and VH/VV over wheat (Vreugdenhil et al. 2018; Harfenmeister et al. 2019; Mandal et al. 2020, Remote Sensing of Environment 247, 111954, on dual-pol radar vegetation indices); (c) hybrid or radiative transfer based optical retrieval as an alternative to purely empirical VI regressions (Verrelst et al. 2015, ISPRS Journal of Photogrammetry and Remote Sensing 108, 273 to 290); (d) PlanetScope harmonisation (Houborg and McCabe 2018; Kington and Collison 2022); and (e) the debate on spatial cross-validation and prediction domain (Wadoux et al. 2021, Ecological Modelling 457, 109692; Meyer and Pebesma 2022, Nature Communications 13, 2208), which bears directly on how LOSSO-CV error should be interpreted for four non-random sites.
**Evidence Anchor**: absence: Introduction and Discussion — expected water cloud model, S1 wheat backscatter studies, RTM based retrieval, PS harmonisation and spatial CV critique references; checked Introduction, sections 2.2.3 to 2.2.4, Discussion 4.1 to 4.6, references.bib
**Why it matters**: The paper's main inference concerns what sensors can contribute; without these references, the reader cannot judge whether the S1 and PS failures are consistent with or contrary to the established literature. Several current citations are also tangential (drought monitoring self-citations; Chave et al. 2014, a tropical tree allometry paper, for the definition of crop AGB; grassland and forage S1 studies cited as evidence for crops).
**Suggestion**: Add a short paragraph on SAR retrieval of cereal biomass (water cloud model and empirical C-band studies) and on PS cross sensor consistency, and engage with Wadoux et al. (2021) when justifying LOSSO-CV. Replace Chave et al. (2014) with a crop specific definition.
**Severity**: Minor
**Confidence**: 4, core expertise; references verified to exist

### W11: Questionable band and index choices for S2

**Problem**: The candidate set includes S2 B1 (60 m coastal aerosol) and the "Water Index" NIR/B9, where B9 (945 nm) is the 60 m water vapour band used by Sen2Cor for atmospheric retrieval; neither is a recognised canopy variable at plot scale, and the 60 m pixels are 240 times the plot area. The classical water index is R900/R970 (Peñuelas et al. 1997), which S2 cannot reproduce. The table 4 S2 list also omits MCARI/OSAVI2, which is in the code (36 predictors in code, 35 listed).
**Evidence Anchor**: table: table 3, row 7 "WI" defined as NIR/Water vapour
**Why it matters**: Atmospheric bands at 60 m add noise and, when cumulative, act as clocks (W6); this lowers the apparent value of S2.
**Suggestion**: Remove B1, B9 based indices and their cumulative sums, or justify them physically; reconcile table 4 with the code.
**Severity**: Minor
**Confidence**: 4, core expertise

## Detailed Comments

### Literature Review

- **Coverage**: Strong on the spatial CV motivation (Roberts 2017, Ploton 2020, Meyer 2021) and adequate on VI provenance, weak on SAR crop retrieval and PS radiometry (W10). The introduction's second paragraph is a generic survey of sensors (LiDAR, hyperspectral) that is not used in the study and could be shortened in favour of S1/S2 fusion work on wheat.
- **Integration quality**: The motivation in the third Introduction paragraph is well argued; the earlier paragraphs enumerate rather than synthesise.
- **Research gap argument**: Persuasive. The lack of a phenological baseline under grouped validation is a real gap.

### Theoretical Framework

- **Appropriateness**: Thermal time as a driver of crop development and biomass accumulation is the right framework for the baseline. The SAR interpretation would benefit from the water cloud framing (vegetation attenuation versus soil contribution), which is what the VV decline after stem elongation physically represents.
- **Application depth**: Section 4.5 sensibly limits the conclusion to within-season tracking; this caveat should also appear in the abstract.

### Academic Argument Quality

- **Factual accuracy**: Sentinel-1C launch date and S1B failure date are correct. Table 2 lists S1 as 10 m resolution; GRD IW has 10 m pixel spacing and about 20 × 22 m resolution. The statement that GRD products are "terrain-corrected using a digital elevation model" applies to the GEE ingestion (orthorectification), not to the ESA GRD product, and is not radiometric terrain correction.
- **Argument logic**: The conclusion that S2 and PS "did not transfer well on their own" cannot be drawn until W1 and W2 are fixed. The S1 conclusion needs the controls in W3.
- **Terminology precision**: "Kalman smoothing" should be "Kalman gap filling" (W7). "Nearest-pixel extraction" is inaccurate given bilinear reprojection.

### Methods and Results from the domain angle

- Figure 2 is the most informative figure for a remote sensing reader and should show S1 VH/VV and PS NDVI alongside S2 NDVI; it would have revealed W1 and W2.
- Adding an S1 incidence angle (or relative orbit) column to the supplement would let readers judge the geometry mix.
- The Dove-C to SuperDove transition is attributed to "radiometric calibration" differences; once W2 is resolved, the paper should state which sensor generation covered each site season.

### Conclusion

- The final recommendation to report a phenological baseline is valuable and should survive revision. The secondary messages about which sensors add value should be rewritten after reprocessing.

### References

- Several references serve rhetorical rather than evidential roles (drought monitoring self-citations in the first paragraph). Recent (2022 to 2026) S1+S2 wheat biomass studies under spatially independent validation should be sought and cited if they exist; I did not find a definitive one to recommend and do not invent one.

## Questions for Authors

1. For each S2 item, what was the `s2:processing_baseline`, and was the −1000 offset removed anywhere before `02_calcular_vis.R`? If not, how do the main results change after correction?
2. Which PlanetScope asset (surface reflectance or radiance) and which sensor generation were used for Villa Baviera, and why are its blue and red values above 0.3?
3. Does ∑GDD plus a random or within-date permuted covariate reproduce the RF S1+GDD gain under the same LOSSO-CV?
4. What GNSS device and accuracy were used to locate the 0.25 m² quadrats, and how were quadrats placed relative to crop rows?

## Minor Issues

### Figures and Tables

- Table 2: the S1 wavelength expressed as 5.546e7 nm is awkward; give 5.55 cm in a footnote.
- Table 4: S2 list length does not match the stated 36 (MCARI/OSAVI2 missing).
- Figure 8: axis ranges for ∑PS EVI and ∑PS CI_red will change after W2 is fixed; consider showing per site season rug colours.

### Citation Format

- Chave et al. (2014) is a pantropical tree allometry paper and is an odd source for the definition of crop AGB.

### Language

- Section 2.2.3: "PS~B5~" style subscripts render inconsistently with the table notation.

## Confidential Note to the Editor (outside the domain remit)

While verifying processing claims, I noticed that `script/01_descargar_imagenes.R` in the publicly linked repository contains a plaintext NASA Earthdata password in the `edl_netrc()` call. I have not reproduced it here. The authors should rotate the credential, remove it from the repository history, and read it from an environment variable before the data availability statement points readers to the repository.

## Recommendation Signal

**Major Revision.** Two Critical defects (W1, W2) are processing errors in the S2 and PS inputs that are aligned with the fold structure, so the sensor comparisons, ablation and interpretability results cannot currently be attributed to the sensors. They are repairable by reprocessing and rerunning the existing pipeline, and the paper's central methodological message (report a thermal time baseline under grouped validation) does not depend on them. Combined with the S1 processing and noise control issues (W3, W4) and the baseline definition (W5), the results and discussion will need substantial rewriting and a second review. I make no final decision; that rests with the editor.
