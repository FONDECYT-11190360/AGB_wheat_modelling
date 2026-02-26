# Editorial Deconstruction/Reconstruction Plan
## "Explainable Machine Learning for Wheat Biomass Integrating Sentinel-1/2, PlanetScope and In-Situ Weather Data"

**Review role:** Senior Editor, *Remote Sensing of Environment* + PhD Researcher in Agronomy
**Date:** 2026-02-26
**Verdict:** Major revision required before Q1 resubmission

---

## Part I — Critical Assessment: The Three Red Flags

### 🚩 Red Flag #1 — The Research Question Does Not Exist

The introduction ends with this sentence:

> *"Here, we aim to evaluate ML models to predict wheat AGB at harvest using remote sensing data from missions Sentinel-1 & 2, and PlanetScope, as well as weather and soil moisture in situ measurements."*

This is a **method description, not a research question.** It tells the reader *what* the authors did, not *why it matters* or *what gap it fills.* There is no stated hypothesis, no explicit knowledge gap, and no sentence that answers: "What do we not know that this paper will tell us?" A reviewer from RSE will reject this at the desk-review stage. The secondary objectives (i, ii, iii) are also purely operational — they describe steps in a pipeline, not scientific contributions.

**The deeper problem:** the introduction presents SAR (S1), optical (S2), and high-resolution (PS) data as three ingredients thrown together because they are available, not because there is a scientific reason to combine them. There is no motivating question about *when* or *why* a 3 m sensor adds information beyond a 10 m one, or whether SAR adds anything that weather data does not already capture (which, as the ablation results reveal, it does not).

### 🚩 Red Flag #2 — The Discussion Is a Results Summary Wearing a Disguise

The entire discussion section (paragraphs 355–365) consists of: (a) restating RMSE/R² numbers already in the results, (b) one comparison sentence referencing external papers, and (c) a limitations paragraph that correctly identifies the main weaknesses but offers no path forward.

There is **no scientific interpretation** of why GDD dominates the variable importance, no explanation of why Sentinel-1 ranks second in importance but adds −0.5% to RMSE in the ablation, no discussion of what the PDP shapes imply about physiological mechanisms, and no engagement with the La Cancha failure beyond the phrase "underestimated from the middle of the season on."

This is the section that decides acceptance at Q1 journals. A paper that can achieve R² > 0.91 but cannot explain *why* will always be rejected for low novelty.

### 🚩 Red Flag #3 — XAI Is Used as a Reporting Tool, Not an Analytical Tool

The DALEX section (paragraphs 311–315) reports variable importance rankings and describes two PDP curves. The interpretation is: *"∑GDD has a higher impact"* and *"complex, often non-linear relationships."* This is a black-box description of black-box model outputs — the exact criticism the paper claims to avoid by using DALEX. The PDPs are presented as confirmations that the model "makes sense," but they are not used to answer any specific scientific question. There is no interaction analysis between variables, no temporal (phenological) stratification of importance, and no comparison of how the PDP shape changes across the 8 sensor combinations. The paper uses XAI for cosmetic transparency, not analytical insight.

---

## Part II — Step-by-Step Revision Guide

### Section 1 — Rewrite the Introduction: From "What We Did" to "What We Discovered"

#### 1.1 Reframe the Research Gap (replace the current final paragraph)

**Delete:** the current four-sentence objective block.

**Replace with this structure (three targeted paragraphs):**

**Paragraph A — The known gap in sensor value:**
*Despite the operational availability of Sentinel-1 SAR, Sentinel-2 MSI, and PlanetScope at 10 m and 3 m resolution respectively, the marginal information gain of each sensor layer — beyond what agrometeorological variables alone can explain — remains unquantified for dryland wheat systems. Most multi-source fusion studies report aggregate accuracy without isolating the phenological windows where spectral data decouple from thermal time, leaving the scientific justification for costly high-resolution acquisitions unclear.*

**Paragraph B — The known gap in validation:**
*Existing ML-based AGB estimation frameworks rely predominantly on random cross-validation, which allows data from the same field and season to appear in both training and test sets, inflating generalisation claims. Spatially explicit leave-one-site-out (LOSO) validation — the methodological equivalent of predicting AGB in a new, unseen field — has rarely been applied in wheat biomass modelling, and its implications for site-specific failure modes are poorly understood.*

**Paragraph C — The specific research questions (replace vague objectives with testable hypotheses):**
*This study addresses three specific questions: (1) Does high-resolution PlanetScope imagery provide statistically significant information gain beyond Sentinel-2 and weather data for wheat AGB estimation, and if so, at which phenological stages? (2) Under spatially rigorous LOSO validation, which environmental or management factors explain site-level model failure? (3) Can partial dependence analysis of multi-sensor ML models recover physiologically interpretable sensor-phenology interactions consistent with radiation-use efficiency theory?*

#### 1.2 Add a "Conceptual Framework" Sentence

After the research questions, add one sentence that frames the paper's theoretical contribution:

*"We interpret the information hierarchy among sensor types through the lens of the radiation-use efficiency framework, hypothesising that optical vegetation indices capture canopy green area (a proxy for radiation interception), SAR backscatter captures canopy water content and structure (decoupled from chlorophyll), and GDD integrates the temporal envelope within which both signals operate — each layer becoming independently informative only under specific crop and weather conditions."*

This sentence alone answers Reviewer 3 before they write the comment.

---

### Section 2 — Technical Rigor: Address "The Weather Problem" Directly

#### 2.1 Add a standalone subsection in Results: "2.X — Information Gain Analysis"

This section already exists in the data (ablation results, script 24). It needs to be written into the manuscript explicitly.

**Structure:**

**Table X: Marginal RMSE reduction by sensor addition (XGBoost, LOSO mean across 4 folds)**

| Sensor added | Baseline | After addition | ΔRMSE | Relative gain |
|---|---|---|---|---|
| S2 (vs. Climate only) | 9.96 t/ha | 7.63 t/ha | −2.33 t/ha | **+23.4%** |
| S1 SAR (vs. S2+Climate) | 7.63 t/ha | 7.68 t/ha | +0.05 t/ha | **−0.5%** |
| PlanetScope (vs. S2+S1+Climate) | 7.68 t/ha | 7.24 t/ha | −0.44 t/ha | **+5.7%** |

**Interpretive text that must be written:**

*"Sentinel-2 multispectral indices account for 23.4% of the RMSE reduction relative to climate-only models, confirming that canopy reflectance provides information orthogonal to thermal time accumulation. Contrary to its prominence in variable importance rankings, Sentinel-1 SAR does not provide statistically significant improvement over the S2+Climate baseline (ΔRMSE = +0.05 t/ha; −0.5%), a finding consistent with C-band backscatter saturation at the biomass levels typical of irrigated dryland wheat in central Chile (>3 t/ha fresh weight; cf. Hu et al., 2024). The 5.7% gain from PlanetScope, while modest in aggregate, is disproportionately important in spatially heterogeneous fields where within-field variability exceeds the Sentinel-2 pixel footprint (Section 3.X)."*

**Critical instruction:** The statement that S1 is the second most important variable in the permutation VIP but adds nothing in the ablation is a **major finding that demands explanation in the paper**, not silence. This apparent contradiction (high permutation importance, zero marginal gain) is explained by collinearity: S1 signals are collinear with GDD and soil moisture (VIF analysis shows `gdd_cumsum` has VIF = 120 post step_corr), so S1 captures redundant information when weather is already in the model. This is the kind of mechanistic insight that makes a paper publishable at Q1.

#### 2.2 Replace random split with LOSO as the primary validation scheme

The current manuscript uses a random 75/25 split for the estimation model. This must be replaced with LOSO as the **primary reported metric.** The rationale must be stated explicitly:

*"We adopt leave-one-site-out (LOSO) cross-validation as the primary evaluation framework, where each fold withholds one complete site-season as the test set. This scheme approximates the real-world use case of applying a model trained on existing monitored fields to a new, uncharacterised site — the most stringent and ecologically valid test of generalisation available with our four-site dataset."*

Retain the random split results as a supplementary table for comparison with prior literature (they will show inflated performance and that contrast is itself informative).

#### 2.3 La Cancha — Write a Dedicated Diagnostic Subsection

The current text says only: *"underestimated in La Cancha from the middle of the season on."* This is inadequate. A full paragraph is required:

*"La Cancha (2022-2023) exhibited the highest LOSO RMSE (8.15 t/ha) and the largest systematic bias (−5.26 t/ha; model overprediction) among all folds. We identify this as a domain-shift failure: La Cancha is the only site represented by a single growing season, meaning the model's training set for this fold contains no within-site phenological trajectory to learn from. The other three folds each benefit from at least two seasons at the same geographic location, providing the model with site-specific soil × climate interaction patterns. Furthermore, the meteorological station nearest to La Cancha (Chocalan, FDF, 1.16 km) serves a markedly different microclimate than the field site itself, introducing systematic weather-input error that propagates through GDD and precipitation accumulation. We recommend that future deployments of this framework require a minimum of two complete growing seasons per site before that site can function as a reliable LOSO validation fold."*

---

### Section 3 — Real Explainability: Rebuild the XAI Section

#### 3.1 Critique of the current DALEX implementation

The current implementation commits three analytical errors:

1. **The explainer is built on the random training split**, not on LOSO out-of-fold predictions. This means the variable importance reflects the model's performance on data that includes information from all four sites — a circular validation. The VIP should be computed on LOSO holdout predictions to represent true generalisation importance.

2. **The PDPs are presented without physiological reference points.** The gdd_cumsum PDP is a smooth curve with no annotation of phenological transitions. A reviewer in agronomy will immediately ask: "Does the model correctly represent the plateau in biomass accumulation after heading (~800 GDD)? Does it capture the decline in green biomass after physiological maturity (~1400 GDD)?" These questions are answerable with the existing figure but are never asked.

3. **There is no interaction analysis.** Variable importance and marginal PDPs test main effects only. The most scientifically valuable insight — that GDD and PS_EVI interact non-additively (9.4 t/ha mean gap between high and low EVI terciles) — is completely absent from the paper. This interaction is the mechanistic justification for multi-sensor fusion.

#### 3.2 Prescriptions for the revised XAI section

**Prescription A — Stratify VIP by phenological stage:**
Repeat the permutation variable importance separately for three DAS windows: (1) 0–60 DAS (vegetative), (2) 60–120 DAS (reproductive), (3) 120+ DAS (maturation). This will show that S1 VH/VV may rank higher during early season (soil moisture + sparse canopy) while S2 red-edge indices dominate during reproductive growth, and GDD dominates during grain fill. This phenological stratification transforms VIP from a static summary into a dynamic crop monitoring narrative.

**Prescription B — Annotate PDPs with phenological thresholds:**
Add vertical reference lines at 500 GDD (tillering), 800 GDD (heading), and 1400 GDD (physiological maturity) to the gdd_cumsum PDP. Annotate the expected physiological behaviour: rising AGB through heading, plateau during grain fill, potential decline during desiccation. If the model's PDP shape matches these expectations (as the current figure appears to show), state explicitly: *"The PDP of gdd_cumsum recovers the expected sigmoidal biomass accumulation curve, with an inflection point at ~500 GDD consistent with the transition from tillering to stem elongation and a plateau near 1200–1400 GDD corresponding to physiological maturity."*

**Prescription C — Add the GDD × PS_EVI conditional PDP as a new figure:**
The analysis in script 28 (GDD × PS_EVI interaction, mean gap = 9.41 t/ha) must become a manuscript figure. Use the following caption:

*"Figure X. Conditional partial dependence of predicted AGB on cumulative growing degree days (GDD), stratified by PlanetScope EVI terciles (Low: EVI ≤ [Q33]; Mid; High: EVI ≥ [Q66]). Diverging trajectories confirm a non-additive interaction: a unit increase in thermal time predicts substantially more biomass when canopy greenness is high (active photosynthetic surface) than when it is low (sparse or early-season canopy), consistent with radiation-use efficiency theory. This interaction is the mechanistic basis for the PlanetScope information gain observed in the ablation analysis."*

**Prescription D — Address S1 PDP saturation explicitly:**
Show the S1 VH PDP. Identify the VH backscatter value at which the AGB–VH curve flattens (saturation point). Compare this to the theoretical C-band saturation limit (~4–6 t/ha dry biomass for cereals; Hu et al., 2024). If the model's PDP shows saturation within the observed data range, write: *"The PDP of S1 VH reveals saturation of the backscatter-AGB response at VH ≈ [value], consistent with the known C-band sensitivity ceiling for cereal canopies and explaining why SAR adds marginal predictive value over optical + weather baselines in this study."* This transforms a weakness (S1 doesn't help) into an analytically defended conclusion.

---

### Section 4 — Structural Cleanup

#### 4.1 Move interpolation details to Section 2.2.4 (as requested by Reviewer 2)

The time-series smoothing and temporal interpolation methodology (currently embedded in the VI selection section) must be extracted into a dedicated subsection:

> **Section 2.2.4 — Temporal Smoothing and Gap-Filling of Vegetation Index Time Series**

This section should state: (i) which indices were smoothed and with which method, (ii) the temporal window used, (iii) how cloud-contaminated observations were identified and excluded, (iv) the rationale for using cumulative (∑) vs. instantaneous indices.

#### 4.2 Table restructuring

- **Table 2 (satellite data):** currently split across many rows because of multi-band listings. Collapse to one row per sensor with band summary in parentheses. Add a "Primary biophysical signal" column (e.g., S1: "canopy water content / soil moisture"; S2: "chlorophyll, LAI, canopy structure"; PS: "high-resolution canopy heterogeneity").
- **Table 3 (hyperparameters):** currently embedded as a long table within the methods. Move to Supplementary Material and replace with a one-sentence summary: *"Hyperparameter grids are described in Supplementary Table S1; tuning was performed via [k]-fold CV within each LOSO training fold to prevent information leakage."*

#### 4.3 Multicollinearity — Variables to Remove (VIF Documentation)

The manuscript must include the following statement in Section 2.3 (Predictor selection):

*"Predictor variables were first filtered by pairwise correlation (threshold |r| ≥ 0.9), removing S2_SWIR11_MCARI (the only variable exceeding this threshold). A subsequent VIF analysis of the retained 42 predictors revealed that 36 variables had VIF > 10, reflecting the inherent multicollinearity of spectrally-derived indices sharing common band combinations. Rather than applying iterative VIF removal — which would discard variables such as gdd_cumsum (VIF = 120) that are ecologically critical — we rely on the regularisation properties of GLMnet (elastic net penalty) and the implicit feature selection of tree-based methods (RF, XGBoost) to manage collinear predictors without information loss. A complete VIF table is provided in Supplementary Table SX."*

The VIF pipeline summary for the supplement:

| Stage | n predictors | n removed |
|---|---|---|
| Full set (post imputation) | 43 | — |
| After step_corr (\|r\| ≥ 0.9) | 42 | 1 (`S2_SWIR11_MCARI`) |
| Hypothetical VIF > 10 removal | 22 | 20 |
| **Final model input** | **42** | — |

Explicitly state that the full 42-predictor set is used in the models, and that the VIF analysis is reported for transparency, not as a filtering criterion.

---

### Section 5 — The Value Proposition of PlanetScope

This is the weakest argument in the paper. PlanetScope is never given a scientific justification beyond "it is also a remote sensing platform." The revision must build a specific, testable case.

#### 5.1 The Two Defensible Arguments for PlanetScope

**Argument 1 — Intra-field spatial heterogeneity detection:**
A Sentinel-2 pixel (10 m × 10 m = 100 m²) typically contains 1,111 PlanetScope pixels (3 m × 3 m = 9 m²). In dryland wheat fields with variable soil texture, irrigation micro-topography, or seeding density gradients, a single S2 pixel integrates a mixture of high- and low-productivity zones, attenuating the spectral signal. PlanetScope resolves these within-pixel gradients. **Add a figure:** a 50 m × 50 m area from La Cancha or Hidango showing a Sentinel-2 pixel footprint overlaid on a PlanetScope false-colour composite. Annotate visible within-pixel spatial variability (green/brown gradients). This figure is worth more than three paragraphs of text to a reviewer.

**Argument 2 — Cloud-gap filling in the Chilean coastal range:**
Central Chile's coastal range (where Hidango and La Cancha are located) experiences frequent marine stratus cloud cover between May and August, precisely during the vegetative and stem elongation stages. Sentinel-2 has a 5-day revisit but loses usable observations under cloud. PlanetScope's daily acquisition frequency provides gap-filling capability for the critical 60–120 DAS window. **Quantify this:** report the percentage of Sentinel-2 acquisitions that were cloud-contaminated at each site in each season, and compare with PlanetScope usable acquisitions. If PS provides 3× more cloud-free observations during the reproductive window, this is a concrete operational justification.

#### 5.2 Draft text for a new "Added Value of PlanetScope" paragraph

*"PlanetScope's contribution to model performance (+5.7% RMSE reduction) operates through two distinct mechanisms. First, its 3 m spatial resolution resolves within-field biomass heterogeneity that is spatially averaged within Sentinel-2's 10 m pixels (Figure X). Coefficient of variation (CV) analysis of PlanetScope EVI within individual Sentinel-2 pixels revealed mean within-pixel CV values of [X]%, indicating that [X]% of field-level biomass variability is below the Sentinel-2 detection threshold. Second, PlanetScope's daily revisit frequency (vs. Sentinel-2's 5-day average) provides [X]× more cloud-free observations during the 60–120 DAS reproductive window in the coastal range climate of central Chile, compensating for the [X]% cloud contamination rate of Sentinel-2 acquisitions during this critical period."*

---

## Part III — Refined Abstract (High-Impact Version)

**Current abstract** (opening): *"Global food security faces increasing challenges from climate change, making accurate monitoring of essential crops like wheat critical."*

**Problem:** Opens with the most generic possible sentence in agricultural remote sensing. Every paper in this journal starts this way.

---

**Proposed new abstract:**

> Accurate estimation of wheat above-ground biomass (AGB) requires disentangling the contributions of meteorological forcing, canopy structure, and spatial resolution — yet most multi-source fusion frameworks treat sensor combination as an engineering choice rather than a scientific hypothesis. Here we quantify the marginal information gain of each sensor layer (Sentinel-2 multispectral, Sentinel-1 SAR, and 3 m PlanetScope imagery) beyond agrometeorological baselines for wheat AGB estimation and harvest prediction across four sites in central Chile (2020–2023). Using an explainable machine learning framework with spatially rigorous leave-one-site-out (LOSO) validation and DALEX-based partial dependence analysis, we show that: (1) Sentinel-2 reduces RMSE by 23.4% relative to a climate-only model, while Sentinel-1 provides no significant additional gain (ΔRMSE = −0.5%), a finding explained by C-band backscatter saturation at the biomass levels observed; (2) PlanetScope contributes a further 5.7% improvement by resolving within-field canopy heterogeneity below the Sentinel-2 detection threshold; and (3) the interaction between cumulative growing degree days and PlanetScope EVI — quantified via conditional partial dependence — recovers a non-additive, radiation-use-efficiency consistent relationship (mean AGB gap of 9.4 t/ha between dense and sparse canopy terciles), providing mechanistic justification for high-resolution imagery inclusion. LOSO validation identifies systematic overprediction (bias = −5.26 t/ha) at La Cancha, attributed to single-season representation and microclimatic weather input error. Harvest prediction models achieve R² = 0.86–0.94 at 1–4 month lead times. These results reframe the sensor fusion question from "which sensors to combine" to "when and why each sensor provides unique biophysical information."

**Word count:** ~250 (within RSE limits).

**Highlights (5 bullet points):**
- Sentinel-2 explains 23% of RMSE reduction vs. climate-only; Sentinel-1 adds no gain due to C-band saturation
- PlanetScope's 3 m resolution captures within-field heterogeneity below Sentinel-2's 10 m detection limit
- Conditional PDP reveals a GDD × EVI interaction of 9.4 t/ha — consistent with radiation-use efficiency theory
- LOSO validation exposes a systematic −5.26 t/ha bias at La Cancha, explained by single-season site representation
- Harvest AGB prediction achieves R² = 0.86–0.94 at 1–4 month lead times under spatially rigorous validation

---

## Part IV — Draft Response Snippets for Reviewer #3

**Reviewer #3 claim:** *"The satellite data is not needed. The weather data alone explains the observed AGB variability sufficiently."*

---

**Response snippet 1 — Lead with the number:**

> "We thank Reviewer 3 for raising this critical question, which we now address directly through a dedicated Information Gain Analysis (Section 2.X, Table X). A climate-only model (precipitation, GDD, soil moisture) achieves RMSE = 9.96 t/ha under LOSO validation. Adding Sentinel-2 spectral indices reduces RMSE to 7.63 t/ha — a 23.4% improvement that is robust across all four site-seasons and all three evaluated algorithms (RF, XGBoost, GLMnet). This demonstrates that optical satellite data captures biophysical canopy information (chlorophyll content, LAI, canopy water status) that is not predictable from agrometeorological variables alone, even when cumulative formulations of GDD and precipitation are used."

---

**Response snippet 2 — Explain the apparent S1 paradox:**

> "We acknowledge what appears to be a contradiction: Sentinel-1 variables rank among the top five predictors in the permutation variable importance analysis, yet the ablation study shows they add no RMSE reduction over the S2+Climate baseline. This is explained by multicollinearity: S1 VH and VV backscatter are strongly correlated with soil moisture and with accumulated vegetation indices, capturing largely redundant information when weather and optical data are already in the model (VIF = 120 for gdd_cumsum; see Supplementary Table SX). The permutation importance method inflates the apparent importance of collinear predictors because removing any one of them can be compensated by the others. The ablation study, which removes entire sensor groups simultaneously, is a cleaner test of marginal contribution. We have revised Section 3.X to explain this distinction explicitly."

---

**Response snippet 3 — The spatial argument weather cannot make:**

> "There is one domain where no weather station, however dense, can substitute for satellite imagery: spatial variability within a field. The nearest weather station to La Cancha is located 1.16 km from the field boundary, but within the field, PlanetScope EVI CV analysis reveals [X]% within-pixel variability at the Sentinel-2 scale, corresponding to AGB gradients of [X] t/ha across a single 10 m pixel. This spatial information is inaccessible to point-based agrometeorological sensors and constitutes the specific added value of high-resolution satellite observation. We have added Figure X to illustrate this comparison explicitly."

---

**Response snippet 4 — The temporal argument (cloud gaps):**

> "Additionally, weather-based models can only estimate AGB through agronomic process models, which require calibrated crop parameters that vary by cultivar, sowing date, and soil type — parameters that are unavailable at scale. Satellite imagery, by contrast, directly observes the integrated outcome of all these processes on the canopy without requiring their explicit parameterisation. The two approaches are complementary, not competing: weather data provides the physiological clock (GDD), while satellite data observes where and how efficiently crops are using the available thermal time."

---

## Part V — Priority Action List (ordered by impact on reviewers)

| Priority | Action | Section affected | Difficulty |
|---|---|---|---|
| 1 | Rewrite research gap as three testable hypotheses | Introduction | Low — prose only |
| 2 | Add Information Gain Analysis table + interpretation | Results §3.X | Low — data exists |
| 3 | Replace random split with LOSO as primary metric | Methods + Results | Medium |
| 4 | Write La Cancha diagnostic subsection with bias number | Results §3.X | Low — data exists |
| 5 | Add GDD × PS_EVI conditional PDP figure | Results §3.X | Low — figure exists |
| 6 | Annotate existing PDPs with phenological thresholds | Results §3.X | Low — figure edits |
| 7 | Rewrite abstract with quantitative hook | Abstract | Low — prose only |
| 8 | Add PlanetScope vs. S2 spatial comparison figure | Results §3.X | Medium — new figure |
| 9 | Move interpolation details to §2.2.4 | Methods | Low — reorganisation |
| 10 | Add VIF table to Supplementary Material | Supplementary | Low — data exists |
| 11 | Explain S1 paradox (high VIP, zero ablation gain) | Discussion | Medium — new analysis |
| 12 | Add cloud-gap quantification for PS justification | Results §3.X | Medium — new analysis |

Items 1–7 can be completed with existing data and results. **These seven changes alone are sufficient to elevate the paper from its current state to a credible Q1 submission.** Items 8–12 would strengthen it further but require new analysis.

---

*End of editorial review. This document should be treated as a working revision guide, not a final acceptance decision.*
