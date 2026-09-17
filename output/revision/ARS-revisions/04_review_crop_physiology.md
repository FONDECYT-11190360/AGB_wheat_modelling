# Peer Review Report

## Manuscript Information
- **Title**: How much do satellite sensors add to thermal time? A leave-one-site-season-out evaluation of Sentinel-1, Sentinel-2, and PlanetScope predictors for wheat above-ground biomass
- **Manuscript ID**: ARS-Review-2026-09-17
- **Review Date**: 2026-09-17
- **Review Round**: Round 1 review

---

## Reviewer Information

### Reviewer Role
Peer Reviewer 3 (Cross-disciplinary/Practical — Crop Physiology)

### Reviewer Identity
Professor of Crop Physiology and Agronomy, specializing in wheat phenology, thermal time modeling, and crop growth simulation. Background in field-based studies of Mediterranean wheat systems. Experience with GDD-based decision support tools for wheat growers in semi-arid regions.

### Review Focus
This review evaluates the physiological plausibility of the study's findings, focusing on the GDD-biomass relationship, the appropriateness of GDD base temperatures, the realism of observed AGB values for Mediterranean wheat, and whether the conclusion that thermal time dominates is physiologically meaningful or indicates a modeling artifact.

---

## Overall Assessment

### Recommendation
- [X] **Minor Revision** — Minor revisions needed, no re-review after revision

### Confidence Score
5 — Completely within my area of expertise, I am very confident in my assessment

### Summary Assessment

This manuscript addresses a fundamental question in crop monitoring: does satellite remote sensing add information beyond simple thermal time for wheat biomass estimation? From a crop physiology perspective, the finding that accumulated growing degree days (∑GDD) alone achieves R²=0.53 and outperforms most satellite-based models is both expected and valuable. Wheat biomass accumulation is physiologically driven by thermal time through most of the growing season, so it is entirely plausible that ∑GDD carries most of the transferable signal across site-seasons with different temperature regimes and phenological calendars. The study's main contribution is demonstrating this empirically under rigorous spatial cross-validation, which challenges the remote sensing community's implicit assumption that satellite data is always superior to simple phenological indicators.

The GDD base temperatures (2°C for early stages, 6°C for reproductive stages) are appropriate for wheat and well-cited (Del Pozo 1987). The observed AGB values (0 to 43.5 t/ha, mean 12.2 t/ha) are realistic for Mediterranean wheat systems, though the seven samples above 30 t/ha are at the high end and deserve discussion of whether they represent exceptional field conditions or measurement artifacts.

I have only minor concerns: (1) The study design samples multiple phenological stages within each site-season (Figure 2), but does not discuss whether the within-season sampling design might inflate the ∑GDD baseline's performance relative to the satellite predictors. If most samples are from mid-to-late season (where AGB is high and variable, and GDD has accumulated substantially), then ∑GDD will appear very predictive simply because it tracks the sampling timeline. (2) The paper states that the 5.80 t/ha RMSE is "too coarse for field-level decisions" but does not cite agronomic decision thresholds for variable-rate fertilization or other applications. From a crop management perspective, what error level would be acceptable? (3) The discussion could better integrate crop physiology literature on the GDD-biomass relationship, particularly studies that have quantified how much biomass variability at a given thermal time is due to water stress, nitrogen availability, or cultivar differences. This would help readers understand whether satellite data's modest contribution reflects limited crop condition information, or whether the study's site-seasons happened to have low stress variability.

With these minor additions, the paper would provide a valuable bridge between the remote sensing and crop physiology communities.

---

## Strengths

### S1: Physiologically grounded GDD baseline
The use of accumulated growing degree days with appropriate base temperatures (2°C for early stages, 6°C for reproductive stages) is well-grounded in wheat physiology literature. The citation of Del Pozo 1987 is appropriate for Chilean wheat systems.

**Evidence Anchor**: `text: Methods §2.2.2 "A base temperature of 2°C was applied for the early growth stages (sowing to tillering), while 6°C was used for reproductive stages (stem elongation to ripening) (Del Pozo 1987)"`

### S2: Comprehensive phenological sampling across growth stages
The study samples AGB at multiple phenological stages from sowing to ripening (Figure 2), capturing the full trajectory of biomass accumulation. The sampling dates are well-distributed across Zadoks stages, ensuring that models are trained on the full range of phenological development.

**Evidence Anchor**: `figure: Figure 2 shows AGB sampling dates across phenological stages from Sowing to Ripening, with NDVI time series indicating canopy development`

### S3: Transparent reporting of realistic AGB values
The observed AGB values (0 to 43.5 t/ha, mean 12.2 t/ha) are realistic for Mediterranean wheat systems. The per-site-season maxima (35.3, 22.6, 43.5, 23.3 t/ha) reflect the diversity of site productivity and seasonal conditions.

**Evidence Anchor**: `text: Results §3.1 "Observed AGB ranged from 0 to 43.5 t/ha (mean 12.2 t/ha, SD 10.6 t/ha). The maximum per site-season was 35.3 t/ha in Hidango 2021-2022, 22.6 t/ha in Hidango 2022-2023, 43.5 t/ha in La Cancha 2022-2023, and 23.3 t/ha in Villa Baviera 2020-2021"`

### S4: Physiologically interpretable finding that cumulative optical indices track season progress
The ALE profiles (Figure 8) show that the most important optical predictors (cumulative GRVI, EVI, NDRE3*, CI_red) have monotonic increasing effects on predicted AGB. The interpretation that "cumulative indices behaved as measures of season progress" is physiologically sound: these indices integrate canopy greenness over time, which is tightly coupled to thermal time accumulation in the absence of severe stress.

**Evidence Anchor**: `text: Discussion §4.2 "The most important optical predictors in the model with all predictors were cumulative indices whose ALE profiles increased monotonically, which indicates that they acted mainly as alternative measures of season progress"`

### S5: Integration of spring and winter wheat systems
The inclusion of both winter wheat (Irafen-INIA, Pantera-INIA) and spring wheat (Llareta-INIA) across different sowing and harvest dates provides valuable diversity for testing thermal time transferability. The finding that ∑GDD transferred better than DAS (R²=0.53 vs. 0.19) demonstrates the value of thermal time in accounting for different temperature regimes.

**Evidence Anchor**: `text: Study Area §2.1 "A variety of spring wheat was planted in Villa Baviera (Llareta-INIA), while the other sites had winter wheat (Irafen-INIA and Pantera-INIA)"`

---

## Weaknesses

### W1: Potential within-season sampling bias favoring GDD baseline
**Problem**: The study samples multiple phenological stages within each site-season (5-6 sampling dates per site-season, Figure 2). Most samples appear to be from mid-to-late season (Stem Elongation to Ripening), when AGB is high and variable. Because ∑GDD monotonically increases through the season, and sampling is spread across phenological stages, ∑GDD will naturally be highly correlated with AGB within each site-season simply by tracking the sampling timeline. The satellite predictors also vary within season, but they have more complex relationships (e.g., NDVI saturates at high canopy cover, declines during senescence). The comparison therefore favors the monotonic GDD signal. A fairer comparison would sample only one or two key stages (e.g., anthesis and pre-harvest) and test whether satellite data adds information about crop condition *at a given phenological stage*.

**Evidence Anchor**: `figure: Figure 2 shows 5-6 sampling dates per site-season, distributed from Sowing to Ripening; text: Results "A total of 153 AGB wheat samples were collected: 88 in Hidango (40 in 2021-2022 and 48 in 2022-2023), 35 in La Cancha, and 30 in Villa Baviera"`

**Why it matters**: The high performance of the ∑GDD baseline (R²=0.53 under LOSO-CV) may partly reflect the within-season sampling design rather than demonstrating that ∑GDD is sufficient for field-to-field transferability at a given growth stage. In operational crop monitoring, the goal is often to estimate current biomass (or forecast final yield) at a specific point in the season, when GDD is known but does not vary much across fields. For example, if farmers want to know which fields have low biomass at anthesis (to decide on supplemental nitrogen), ∑GDD will not differentiate fields sampled on the same date; satellite data would be needed. The current design conflates temporal (within-season) and spatial (across-field) variation, making it unclear whether satellite data's poor performance is because it fails to track season progress or because it fails to detect crop condition differences at a given stage.

**Suggestion**: (1) Report the distribution of sampling dates and AGB values within each site-season. Are most samples from late season? (2) Conduct a sensitivity analysis: subset the data to samples from a single phenological stage (e.g., only Ear Emergence to Milk Development, Zadoks 50-75) and re-run the models. At a fixed phenological stage, ∑GDD should have much lower variance, and satellite data's relative contribution should increase if it carries crop condition information. (3) If this analysis is not feasible, discuss this as a limitation: "The within-season sampling design may favor the ∑GDD baseline, which monotonically tracks season progress. In operational applications where biomass must be estimated at a specific growth stage, satellite data may provide more value than our across-stage results suggest."

**Severity**: Minor — the finding that ∑GDD dominates is still valuable, but the interpretation could be refined with this caveat; discussion revision would suffice.

**Confidence**: 5 — core expertise: wheat phenology and GDD-biomass relationships

### W2: Observed AGB values above 30 t/ha not discussed
**Problem**: The paper reports that observed AGB reached 43.5 t/ha in La Cancha 2022-2023, and that seven samples above 30 t/ha came from La Cancha and Hidango 2021-2022 (Results §3.1). These values are at the high end for wheat biomass. Typical wheat aboveground biomass at maturity is 15-25 t/ha for high-yielding Mediterranean systems, though values above 30 t/ha are possible under exceptional conditions (irrigated, high nitrogen, dense stands). The paper does not discuss whether these high values represent realistic field conditions, measurement error, or whether the 0.25 m² sampling plots happened to capture particularly dense patches.

**Evidence Anchor**: `text: Results §3.1 "the seven samples above 30 t/ha all came from Hidango 2021-2022 and La Cancha 2022-2023"`

**Why it matters**: High AGB outliers can disproportionately affect model performance and interpretation. Figure 5 shows that all models underpredict values above 30 t/ha, and the Discussion states "The observed AGB values above 30 t/ha exceed those commonly reported for wheat and dominate the largest errors" (line 404). If these values are measurement artifacts or represent micro-site variability (e.g., sampling near a fertigation line), they should be flagged or excluded. If they are realistic, the Discussion should explain what field conditions (irrigation, nitrogen rates, cultivar, plant density) produced these exceptional values.

**Suggestion**: (1) Add 1-2 sentences to the Study Area or Results section explaining what agronomic conditions (irrigation, nitrogen management, plant density) might have produced the >30 t/ha values. Were La Cancha and Hidango 2021-2022 irrigated? What nitrogen rates were applied? (2) If field management data are not available, discuss this as a limitation: "The observed AGB values above 30 t/ha are at the high end for wheat systems. Without detailed management data, we cannot confirm whether these represent typical within-field variability or exceptional conditions. Excluding these outliers did not change model rankings (Results §3.2)." (3) Consider reporting the results with and without the >30 t/ha samples to assess their influence.

**Severity**: Minor — the high values are acknowledged in the Limitations and do not appear to change conclusions, but agronomic context would strengthen the paper.

**Confidence**: 5 — core expertise: wheat biomass ranges in Mediterranean systems

### W3: Limited integration of crop physiology literature on GDD-biomass relationships
**Problem**: The paper cites Del Pozo 1987 for GDD base temperatures and includes general statements about why AGB accumulates with thermal time (Discussion §4.1: "AGB accumulates with thermal time through most of the season"). However, it does not engage with the crop physiology literature on how much biomass variability at a given thermal time is due to environmental stress (water, nitrogen), genetic differences, or management. For example, studies of wheat biomass using crop growth models (e.g., APSIM-Wheat, DSSAT-CERES-Wheat) have quantified the relative contributions of thermal time, water stress, and nitrogen stress to biomass accumulation. This literature would help interpret why satellite data's contribution is modest: is it because the sampled site-seasons had low stress variability, or because crop condition signals are inherently weak relative to phenological progression?

**Evidence Anchor**: `text: Introduction mentions crop growth models (line 51) but does not cite GDD-biomass physiology literature; text: Discussion §4.1 "AGB accumulates with thermal time through most of the season, so within a site-season most of the variance of AGB is explained by where a sample lies in the season"`

**Why it matters**: Without this physiological context, readers may interpret the finding that "satellite data adds minimal value" as a general truth, when it may be specific to the study's site-seasons. If the four site-seasons happened to have low water stress (Mediterranean winter rainfall, no extreme drought) and uniform management (similar nitrogen rates, no severe disease), then there would be little crop condition variability for satellite data to detect, and thermal time would naturally dominate. Conversely, if the site-seasons had high stress variability, the finding would be more generalizable. The paper acknowledges the small number of site-seasons (n=4) but does not discuss whether they are representative of the range of stress conditions where satellite data is typically advocated.

**Suggestion**: (1) Add 2-3 citations to the Introduction or Discussion of crop physiology or modeling studies that have quantified the GDD-biomass relationship and the role of environmental stress in modulating it. For example: "Crop growth models such as APSIM-Wheat partition biomass variance into thermal time (the primary driver, accounting for ~60-80% of variance) and environmental modifiers like water and nitrogen stress (contributing the remaining 20-40%) [cite]. Satellite vegetation indices are hypothesized to detect these condition differences [cite]." (2) In the Discussion, add 1-2 sentences discussing whether the sampled site-seasons likely had low or high stress variability: "The Mediterranean climate with winter rainfall suggests that severe water stress was unlikely during the main growth period, though late-season drought may have affected some site-seasons. If stress variability was low, satellite data's modest contribution is expected; in more variable environments, its value may be higher." (3) Alternatively, if weather data are available, report whether any site-seasons experienced water stress (e.g., using cumulative precipitation deficit or crop evapotranspiration).

**Severity**: Minor — the finding is valuable as-is, but crop physiology context would improve interpretability and generalizability; discussion revision would suffice.

**Confidence**: 5 — core expertise: wheat physiology and crop modeling

---

## Detailed Comments

### Introduction
- The introduction clearly motivates the study in the context of food security, wheat production, and the need for operational AGB monitoring.
- The discussion of thermal time is appropriate but could be strengthened with more crop physiology references (see Weakness W3).
- The gap identification (lack of phenological baselines under spatial CV) is excellent.

### Study Area
- The description of the four site-seasons is clear. The inclusion of both winter and spring wheat is valuable for testing thermal time transferability.
- The soil type descriptions (Regosols, Luvisols, Nitisols) are appropriate and well-cited (FAO 2014).
- The climate classification (Csb, Mediterranean with winter rainfall) is correct.
- **Missing**: The paper does not report whether the fields were irrigated or rainfed, or what nitrogen fertilization rates were applied. This agronomic context would help interpret the observed AGB values and stress variability (see Weakness W2).

### Field Data
- The AGB measurement protocol (0.25 m² harvest, 70°C drying, conversion to t/ha) is standard and appropriate.
- The soil moisture measurement with TEROS 12 sensors is well-described. The conversion to water depth (mm) by layer thickness is correct.
- The note that each field has a single sensor profile (so SM is field-uniform at each date, line 82) is an important limitation for within-field variability.

### Weather Data
- The GDD calculation is clearly described. The use of 2°C base temperature for early stages and 6°C for reproductive stages is appropriate for wheat and well-cited (Del Pozo 1987). These are reasonable choices for Mediterranean wheat systems.
- **Minor note**: Some wheat phenology models use 0°C base for vegetative stages and 5-8°C for reproductive stages. The choice of 2°C and 6°C is on the conservative side (higher base = less GDD accumulation), which would tend to *underestimate* the strength of the GDD-AGB relationship. The paper could briefly note that alternative base temperatures exist, but the chosen values are standard for Chilean systems.

### Results
- The finding that ∑GDD outperformed DAS (R²=0.53 vs. 0.19) is expected and valuable. Thermal time accounts for different temperature regimes, while calendar time does not.
- The finding that adding precipitation and soil moisture to ∑GDD (weather set W) did not improve performance (RMSE=7.01-7.55 vs. 6.24 t/ha) is interesting. This suggests that within-season water availability variability was not a major driver of biomass variability in these site-seasons, supporting the hypothesis that stress variability was low (see Weakness W3).
- The ALE profile of ∑GDD (Figure 8) shows a monotonic increase with a plateau around 600 °C d. This is physiologically plausible: wheat biomass accumulation rate is highest during stem elongation and early grain filling, then slows as the canopy senesces. The plateau could represent late-season samples where thermal time continues to accumulate but biomass is no longer increasing.

### Discussion
- The physiological explanation for why thermal time dominates is appropriate and clearly stated.
- The recommendation to report phenological baselines is valuable and actionable for the remote sensing community.
- **Needs strengthening**: The discussion could better integrate crop physiology literature on GDD-biomass relationships and the role of environmental stress (see Weakness W3).

---

## Questions for Authors

1. **Within-season sampling bias**: Most samples appear to be from mid-to-late season (Figure 2). Have you tested whether the ∑GDD baseline's strong performance is partly due to the within-season sampling design (i.e., GDD monotonically tracks sampling timeline)? If you subset to a single phenological stage (e.g., anthesis to grain filling), does satellite data's relative contribution increase?

2. **High AGB values**: What agronomic conditions (irrigation, nitrogen rates, cultivar, plant density) produced the AGB values above 30 t/ha in La Cancha and Hidango 2021-2022? Were these fields irrigated?

3. **Stress variability**: Did any of the site-seasons experience water or nitrogen stress? If weather data are available, can you report cumulative precipitation or a water stress index for each site-season? This would help readers assess whether the finding that "satellite data adds minimal value" applies broadly or is specific to low-stress conditions.

---

## Minor Issues

### Terminology
- The term "above-ground biomass" is used consistently throughout. In some crop physiology literature, "aboveground dry matter" or "total aboveground dry weight" are preferred to avoid confusion with "biomass" in the broader ecological sense (living tissue only). The definition in line 76 ("total dry mass of dead and living plant organs") is correct and clarifies that both living and dead tissue are included.

### Figures
- Figure 2: The phenological stage labels are clear. The NDVI time series effectively illustrates canopy development. The subscripted sampling dates (S~1~, S~2~, etc.) are well-explained in the caption.
- Figure 8: The ALE profiles are clear. The rug marks showing observed predictor values are helpful. The y-axis label "ALE (t/ha)" is appropriate.

---

## Criterion-Bound Judgements

Calibration status: `NOT_CALIBRATED`

| Dimension | Criterion source | Judgement | Evidence anchor(s) | Rationale | Uncertainty / scope limit | Decision bearing? |
|---|---|---|---|---|---|---|
| Originality | General | MEETS | text: Introduction "Without a phenological baseline evaluated under the same validation scheme..."; text: Discussion recommendation to report baselines | Comparing satellite predictors against thermal time baselines under spatial CV is novel. The methodological recommendation is a valuable contribution. | None | No |
| Methodological Rigor | General | MEETS | text: §2.2.2 appropriate GDD base temperatures; text: §2.3.4 LOSO-CV design | GDD base temperatures are physiologically appropriate and well-cited. LOSO-CV design is rigorous. | Within-season sampling design may favor GDD (Minor) | No |
| Evidence Sufficiency | General | MEETS | text: 153 samples across 4 site-seasons, multiple phenological stages | Sufficient sampling across phenological stages and site-seasons for exploratory findings. Limited by n=4 site-seasons for generalizability, acknowledged in limitations. | None | No |
| Argument Coherence | General | MEETS | text: physiological interpretation of GDD dominance is clearly explained | The physiological rationale for why thermal time carries most signal is sound and well-articulated. | Could be strengthened with crop physiology literature | No |
| Writing Quality | General | MEETS | text: throughout | Manuscript is clearly written with appropriate technical detail. | None | No |
| Literature Integration | General | PARTLY_MEETS | text: Introduction cites crop growth models but limited crop physiology literature on GDD-biomass | Remote sensing literature is well-integrated. Crop physiology literature on GDD-biomass relationships and environmental stress modifiers is underrepresented. | Limited engagement with crop modeling literature | No (Minor) |
| Significance & Impact | General | MEETS | text: Discussion recommendation to report phenological baselines; operational implications discussed | The finding that thermal time dominates has operational relevance for low-cost monitoring. The methodological recommendation could shift practices in remote sensing community. | Generalizability to high-stress environments uncertain | No |

**Recommendation rationale**: From a crop physiology perspective, the paper's finding that accumulated thermal time carries most of the transferable wheat AGB signal is both expected and valuable. The GDD base temperatures are appropriate, the observed AGB values are realistic for Mediterranean systems, and the interpretation that cumulative optical indices track season progress is physiologically sound. **One criterion is only partly met**: **Literature Integration** could be strengthened by engaging with crop physiology and modeling literature on GDD-biomass relationships and environmental stress modifiers (Minor). Additionally, the within-season sampling design may favor the GDD baseline (Minor), and the high AGB values deserve agronomic context (Minor). These are minor issues that can be addressed with discussion revisions. The paper makes a valuable contribution by empirically demonstrating thermal time's dominance under rigorous spatial cross-validation.
