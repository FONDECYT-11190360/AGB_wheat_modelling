# Peer Review Report

## Manuscript Information
- **Title**: Thermal time baselines for evaluating Sentinel-1, Sentinel-2 and PlanetScope predictors of wheat above-ground biomass under spatial cross-validation
- **Manuscript ID**: not assigned (simulated IJRS submission)
- **Review Date**: 2026-10-07
- **Review Round**: Round 1 review

criteria_binding_unavailable

No formal venue criteria binding was supplied. This report makes no claim of formal alignment with IJRS acceptance criteria. Where it cites IJRS conventions, the source is the public T&F formatting sheet (`style_tres.pdf`, consulted 2026-10-07), used as a style reference only.

---

## Reviewer Information

### Reviewer Role
Journal-Fit Reviewer (seat EIC)

### Reviewer Identity
Associate Editor, International Journal of Remote Sensing (Taylor & Francis), handling agricultural applications. Background in optical and SAR crop monitoring; handles many machine-learning crop biomass submissions per year.

### Review Focus
Whether the contribution (a phenological thermal-time baseline evaluated under leave-one-site-season-out validation, offered as an evaluation protocol) is novel and useful enough for IJRS readers beyond a local negative case study; whether the remote sensing component meets the journal's technical bar; and length, structure and IJRS conventions. I look in particular at whether four fields can support general recommendations. Statistical detail of the CV design is outside my focus and I defer it to the methodology seat.

---

## Overall Assessment

### Recommendation
- [ ] Accept
- [ ] Minor Revision
- [x] **Major Revision**
- [ ] Reject

### Confidence Score
4 (mostly within my expertise; the agronomic plausibility point in W3 is adjacent expertise and carries confidence 3)

Confidence is an uncertainty and scope disclosure only.

### Calibration Status
`NOT_CALIBRATED`

### Summary Assessment
The manuscript asks how much Sentinel-1, Sentinel-2 and PlanetScope predictors add to accumulated growing degree days (∑GDD) for within-season wheat AGB estimation at fields not used for training. Using four field-seasons in central Chile and 14 predictor sets fitted with LM, RF and XGBoost under LOSSO-CV, it finds that a linear ∑GDD model (R² = 0.53, RMSE = 6.24 t ha⁻¹) is matched or beaten by only two of 22 satellite configurations, and recommends that satellite AGB studies report a phenological baseline under field-level hold-out. I verified the headline numbers against `output/tables/rev2_loso_limpio_resumen.csv` and `rev2_s1gdd_vs_gdd_mismo_algoritmo.csv`; they match table 5 and the text.

The topic fits IJRS and the message is useful: our agricultural submissions routinely report R² above 0.8 from random splits with no season-progress baseline. The paper is candid about its scope. Its weaknesses are of three kinds. First, the contribution is pitched as a general protocol but is not formalised or shown to matter beyond these four fields, and the claim that baselines are "rarely" reported is asserted without evidence. Second, the Sentinel-1 processing, which underlies the only positive finding, is below the technical standard IJRS expects for SAR vegetation work. Third, the field data raise plausibility questions (dry AGB up to 43.5 t ha⁻¹; 21 assigned zeros counted as destructive samples), and a closely related letter on the same dataset is disclosed only in the cover letter. All of these are repairable, so I recommend major revision.

---

## Strengths

### S1: Same-algorithm thermal-time control separates sensor information from model form
Fitting ∑GDD alone with RF and XGBoost, and comparing each sensor+GDD set with it, is a simple and transferable design device. It reveals that the apparent S1+GDD gain over the linear baseline is partly an artefact of tree models handling a near-discrete thermal-time predictor badly. Few submissions I handle make this distinction.
**Evidence Anchor**: `table: Table 5 rows GDD|RF (8.19 ± 1.72) and GDD|XGBoost (8.28 ± 1.73) against GDD|LM (6.24 ± 2.25)`

### S2: Transparent reporting at the level of the held-out unit
Per-fold metrics (table S2), fold counts against the baseline, and a sensitivity analysis excluding AGB = 0 samples give readers what they need to judge the ranking themselves. This reporting standard is itself part of the paper's recommended practice, and the paper follows it.
**Evidence Anchor**: `text: §2.3.4 "we report per-fold results (Supplementary Material table S2) and treat differences smaller than the between-fold SD as indistinguishable"`

### S3: Explicit scoping of the thermal-time finding
Section 4.4 states that the design favours any predictor of season progress and that the result should not be read as evidence against satellite sensors generally. This is the correct framing for a four-field study.
**Evidence Anchor**: `text: §4.4 "should therefore be interpreted as applying specifically to within-season biomass tracking, not as general evidence that satellite sensors lack value"`

### S4: Open data and code
The analysis is reproducible from a public repository, and supplementary text names the script and CSV that produce table S2.
**Evidence Anchor**: `text: Data availability statement "openly available at the following GitHub repository"`

---

## Weaknesses

### W1: The evaluation protocol is claimed as the general contribution but is neither formalised nor shown to generalise
**Problem**: The Introduction and cover letter position the paper's value for IJRS readers as an evaluation protocol for satellite biomass models, applicable "to other crops and sensors". In the text the protocol amounts to three recommendations in one Discussion paragraph (§4.5, second paragraph). It is not stated as a procedure, not tested on any dataset other than the four fields, and its components (grouped hold-out, a null or baseline model, per-unit metrics) are individually established in the spatial CV literature the paper cites (Roberts et al. 2017; Meyer et al. 2021; Ploton et al. 2020). The new element, a phenological baseline fitted under the same split and with the same algorithm, plus a zero-biomass sensitivity check, is valuable but currently implicit.
**Evidence Anchor**: `text: §1 final paragraph "Beyond the case study, the analysis provides an evaluation protocol for satellite biomass models that can be applied to other crops and sensors"`
**Why it matters**: Without a formal protocol and some demonstration beyond one small dataset, an IJRS reader receives a local negative case study with a recommendation attached. That is publishable at a lower bar elsewhere; for IJRS the general contribution has to be carried by the paper, not by the cover letter.
**Suggestion**: (a) State the protocol as a numbered procedure or boxed checklist in Methods (hold-out unit, baseline set, same-algorithm baseline, per-unit R² definition, zero-sample sensitivity, reporting of fold counts), and refer to it from Discussion and Conclusion. (b) Strongly preferred: apply the baseline step to at least one independent public dataset or to the reported setup of a published wheat AGB study, to show the baseline changes conclusions outside Chile. If neither is feasible, reframe title and abstract as a case study with methodological implications and remove the "applied to other crops and sensors" claim.
**Severity**: Major
**Confidence**: 4, core expertise: editorial assessment of RS crop modelling contributions

### W2: The novelty claim that baselines are "rarely" reported rests on no evidence, and the related literature on baselines is missing
**Problem**: The gap statement is asserted, not shown. The Introduction devotes two long paragraphs (§1 paragraphs 2 and 3) to generic sensor and ML descriptions while citing no work on baseline or benchmark models in remote sensing yield and biomass estimation (for example, trend or climatology benchmarks used in operational yield forecasting, or GDD-based normalisation of VI time series), and no work on Sentinel-1 backscatter dynamics for wheat beyond Veloso et al. (2017).
**Evidence Anchor**: `text: §1 paragraph 4 "Satellite AGB studies rarely report a phenological baseline evaluated under the same validation scheme as their satellite models"`
**Why it matters**: The "rarely" claim is the paper's stated reason to exist. Editors and referees will test it, and if baselines are common in adjacent yield forecasting work the originality argument weakens.
**Suggestion**: Add a compact, reproducible literature tally (for example, 20 to 40 recent satellite or UAV wheat AGB and ML papers, with columns for split type, presence of a phenological or null baseline, and reported R²), as a table in the main text or supplement. Shorten §1 paragraphs 2 and 3 to make room, and add the benchmark literature from yield forecasting and the C-band wheat literature to the gap argument.
**Severity**: Major
**Confidence**: 4, core expertise: RS crop monitoring literature

### W3: Field AGB values and the counting of zero samples need verification before the error magnitudes can be interpreted
**Problem**: Reported dry AGB reaches 43.5 t ha⁻¹, and the manuscript itself notes values above 30 t ha⁻¹ exceed those commonly reported for wheat. From `data/processed/rds/dataset.rds`, the La Cancha mean rises from 6.3 t ha⁻¹ (4 August 2022) to 24.9 t ha⁻¹ (9 September 2022, stem elongation per figure 2), about 0.5 t ha⁻¹ d⁻¹ in late winter, and several fields lose 6 to 10 t ha⁻¹ of mean dry AGB late in the season. Separately, the 153 "destructive samples" in the abstract include 21 AGB = 0 values assigned at the sowing date, so 132 samples were harvested.
**Evidence Anchor**: `text: §4.6 "The observed AGB values above 30 t ha$^{-1}$ exceed those commonly reported for wheat and dominate the largest errors"`
**Why it matters**: The paper's practical conclusion (RMSE about 47 to 51% of mean AGB, "too coarse for field-level decisions") and every RMSE comparison depend on the scale of the response. If some values reflect a fresh-weight, drying or area conversion problem, the absolute errors change, and an IJRS reader will question the dataset before the method. Counting assigned zeros as destructive samples overstates sample size in the abstract and inflates the headline R².
**Suggestion**: Confirm the drying protocol and the 0.25 m² conversion for the high values, compare per-stage AGB with published ranges for these cultivars or for Mediterranean wheat, and either correct or justify the values explicitly in §2.2.1. Report the sample as 132 destructive samples plus 21 assigned zeros at sowing in the abstract and §2.2.1, and give the AGB > 0 metrics alongside the headline numbers in the abstract.
**Severity**: Major
**Confidence**: 3, adjacent field: crop physiology, applying general agronomic ranges

### W4: Sentinel-1 processing is below the IJRS technical bar, yet S1 carries the paper's only positive result
**Problem**: The S1 series mixes relative orbits and incidence angles, is not speckle filtered, is not incidence normalised, and is extracted from a single 10 m pixel, with nearest-neighbour temporal filling. The authors acknowledge this, but the consequence is that the S1+GDD result and the "S1 alone is the weakest set" result are both conditioned on avoidable noise. Table 2 and §2.2.4 spend space on generic mission descriptions (acquisition modes, swath width, the C-band wavelength written as 5.546e7 nm) while omitting the parameters a SAR reviewer needs: orbit directions and relative orbit numbers per site, incidence angle range, GEE preprocessing (thermal noise removal, terrain flattening or not), and whether values were averaged in linear or dB space.
**Evidence Anchor**: `text: §2.2.5 "Sentinel-1 data were used without speckle filtering or incidence angle normalization, which are standard preprocessing steps for SAR vegetation applications"`
**Why it matters**: A title that names Sentinel-1 and a Discussion that argues a mechanism for its contribution (§4.2) invite SAR referees to judge the processing. Acknowledging a limitation that is cheap to remove will not pass at IJRS. The fix may also change the S1 findings in either direction, which is itself informative.
**Suggestion**: Re-run S1 with a single relative orbit per site (or incidence normalisation to a reference angle), a speckle filter or field-level spatial averaging over a buffer of several pixels, and report orbit and angle metadata in a table replacing the generic rows of table 2. If the result changes, report both. Apply the same multi-pixel extraction test to S2 and PS, and state how Dove and SuperDove radiometry was handled (harmonised product or not).
**Severity**: Major
**Confidence**: 4, core expertise: SAR and optical crop monitoring

### W5: A closely related letter on the same AGB dataset is disclosed only in the cover letter
**Problem**: The cover letter states that a letter on the same dataset, reporting a sequential sensor ablation with three algorithms under leave-one-site-out validation, is under review or published at IEEE GRSL, and that an earlier version is on EarthArXiv. The manuscript cites neither, and table 6 of the present paper is itself a sequential sensor ablation.
**Evidence Anchor**: `absence: Introduction, Methods, Discussion and reference list — expected a citation of and differentiation from the related GRSL letter and the EarthArXiv preprint; checked manuscript_ijrs.qmd full text, frontmatter.tex, supplementary_ijrs.qmd`
**Why it matters**: T&F policy requires related work by the authors on the same data to be visible to referees. Overlap that appears only in the cover letter, with the letter's status left as a placeholder, will trigger a redundant publication query and could delay or end handling.
**Suggestion**: Cite the letter and the preprint in the manuscript, state in one or two sentences in §1 or §2.3 what this paper adds (baselines, single-sensor and sensor+GDD sets, same-algorithm controls, held-out group importance, ALE), and consider moving table 6 to the supplement if it substantially reproduces the letter. Upload the letter as a confidential file for the editor. Complete the placeholders in the cover letter.
**Severity**: Major
**Confidence**: 5, core expertise: editorial policy

### W6: Recommendations in the Discussion and Abstract are worded more generally than four fields support
**Problem**: The paper scopes its finding well in §4.4 and §4.6, but the Abstract's closing sentence and §4.5 issue unconditional recommendations, and §4.2 offers a physical mechanism for the S1 gain that the authors themselves say the data cannot separate from a tree-model artefact. The three practices in §4.5 are reasonable as methodological norms; the empirical support for the specific claim that cumulative VIs "need particular care" comes from one RF model on four fields.
**Evidence Anchor**: `text: §4.5 "These results suggest three practices for evaluating satellite AGB models"`
**Why it matters**: My particular concern for this paper: a four-field study can justify a recommendation to report a baseline, because that recommendation costs little and protects against a known bias, but it cannot justify claims about which sensors or index types do or do not carry transferable information in general.
**Suggestion**: Separate in §4.5 and the Conclusion the methodological recommendation (justified by principle and illustrated here) from the empirical findings (specific to four Mediterranean field-seasons, one spring wheat site). Move the S1 mechanism in §4.2 to a clearly hypothetical sentence, and delete the repeated caution sentences elsewhere once this separation is made.
**Severity**: Minor
**Confidence**: 4, core expertise: editorial assessment

### W7: Length and redundancy
**Problem**: The rendered manuscript runs to 46 double-spaced pages with about 11 000 words before the references, eight figures and six tables. The Introduction's sensor overview (§1 paragraphs 2 and 3), the mission descriptions in §2.2.4, and the repeated limitation statements (the four-fold caveat appears in §3.2, §4.1, §4.2, §4.5, §4.6 and the Conclusion; §4.6 first paragraph says the same thing four times) add length without content.
**Evidence Anchor**: `text: §4.6 "With n=4, most differences between configurations are within one standard deviation of the mean and may not represent true performance differences"`
**Why it matters**: IJRS readers and referees will read a focused paper on evaluation more favourably; the current length dilutes the main message. Please check the current IJRS instructions for authors for any word limit; I have not verified a figure.
**Suggestion**: Target a reduction of roughly 20 to 25%: compress §1 paragraphs 2 and 3 to one paragraph, replace generic mission text with the processing parameters requested in W4, state the four-fold caveat once in Methods and once in Limitations, and consider moving table 6 and figure 8 to the supplement.
**Severity**: Minor
**Confidence**: 4, core expertise: editorial assessment

### W8: IJRS style conventions are not yet consistently applied
**Problem**: Several items depart from the T&F IJRS formatting sheet (listed in Minor Issues below): Oxford spelling with -ize endings is required, but the text mixes -ise forms and US forms; units use a solidus and a centred dot; equations are unnumbered; figure part references and abbreviations do not follow house style; the reference list abbreviates journal titles and includes issue numbers.
**Evidence Anchor**: `text: §2.2.1 "Soil water content (cm³/cm³) from each sensor was converted to water depth (mm)"`
**Why it matters**: These are cheap to fix and affect the first impression at triage.
**Suggestion**: Apply the corrections listed under Minor Issues in one pass.
**Severity**: Minor
**Confidence**: 5, core expertise: IJRS house style

---

## Detailed Comments

### Journal Fit
The topic, satellite estimation of crop biophysical variables with S1, S2 and PlanetScope, is squarely in IJRS scope, and the evaluation message is relevant to a large share of the agricultural ML submissions the journal receives. Fit is reduced by two things: the remote sensing processing in its current form (W4), and the fact that the most transferable result is a non-remote-sensing predictor. The paper needs to make clear what the remote sensing reader takes away. The Discussion's sentence that S1 became useful only together with ∑GDD because C-band backscatter is non-monotonic through the season is the kind of remote sensing insight that would justify IJRS; it deserves support from S1 time series plots per field (VV, VH, VH/VV against ∑GDD), which are not shown.

### Originality
Source of originality: a new combination (phenological baseline under field-level hold-out, with a same-algorithm control) applied to new multisensor data. The individual ingredients are established; the combination applied to satellite crop AGB is useful and, as far as I know, uncommon, but the paper must show this (W2). The same-algorithm control (S1) is the most original element and could be promoted in the abstract.

### Significance
If the conclusions hold, they matter for how the RS crop-biomass community reports accuracy, which is broad, international interest. The empirical findings about specific sensors are local and of modest significance given four fields and one spring wheat site. Significance rests on W1 being addressed.

### Structural Coherence
Title, abstract, objectives and conclusion are consistent: all centre on baselines versus satellite predictors. The Introduction is the weak link: two paragraphs review sensors and high-R² ML results before the actual research problem appears in paragraph 4. The three objectives map cleanly onto §3.1, §3.2 and §3.3. Terminology alternates between "field", "site", "site-season" and "field-season" (abstract "four Mediterranean fields"; §2.1 "four wheat fields at three sites"; conclusion "four Mediterranean field-seasons"); choose one term for the CV unit and use it throughout.

### Title and Abstract
The title is accurate and informative, though long; IJRS also requests a short running head, which is absent. The abstract (about 210 words) is clear and quantitative. It should (a) report 132 destructive samples plus 21 assigned zeros, (b) give the AGB > 0 result for the baseline and best model, and (c) name the same-algorithm control explicitly as a design contribution.

### Conclusion
The Conclusion restates results accurately and lists sensible future work. It repeats the abstract nearly sentence for sentence; it would be stronger if it stated the protocol (once formalised, W1) and separated the methodological recommendation from the site-specific findings (W6).

### References
Citation coverage of spatial CV is adequate; coverage of benchmark and baseline practice in yield and biomass estimation, and of C-band wheat backscatter, is thin (W2). Format issues are listed below.

---

## Questions for Authors

1. Were any of the AGB values above 25 t ha⁻¹ (notably La Cancha on 9 September and 18 November 2022) re-weighed or checked for fresh-to-dry conversion, and what dry matter fraction do they imply relative to the other dates?
2. Which relative orbits and orbit directions contribute to each site's S1 series, and does the S1+GDD result persist when only one relative orbit is used?
3. What exactly does the GRSL letter report, and which results in this manuscript (especially table 6) overlap with it?
4. Can the baseline step be applied to any published or public wheat AGB dataset, so that the protocol is shown to change conclusions outside these four fields?

---

## Minor Issues

### Language and spelling (IJRS uses Oxford spelling: -ize endings, analyse)
- §2.2.5 and §4.2 "normalisation", §4.2 "polarised", §4.5 "fertilisation" (twice): use normalization, polarized, fertilization.
- US forms to change to UK: §4.4 "favors" (favours), §2.3.3 "k-nearest neighbors" (neighbours), figure 2 caption "canopy vigor" (vigour).
- Abbreviations: IJRS places the comma before, not after, "e.g." and "i.e." (§1 "(e.g., NDVI, EVI, etc.)"; §4.4 "(e.g., 800°C·d)").
- Season ranges such as "2021-2022" and "3-60 m" (§2.2.5) should use an unspaced en rule.
- Figure 2 caption: "Seeding Growth" should be "Seedling growth".

### Units and numbers
- §2.2.1 "cm³/cm³": use m³ m⁻³ (or cm³ cm⁻³) without a solidus.
- §4.4 "800°C·d": avoid the centred dot and match the "°C d" form used in §3.1.
- §2.2.5 "dividing by 10,000": IJRS uses "10 000".
- Table 2: express the S1 wavelength as 5.55 cm in a footnote rather than "5.546e7 nm".
- §2.2.2 the GDD equation is unnumbered; IJRS requires numbered, right-aligned equations, cited as "equation (1)". In §2.3.4 write "equations (S1)–(S3)" instead of "Eqs. S1 to S3".

### Citation format
- In-text: IJRS uses "et al." for three or more authors (rendered text shows "Dietz, Zörb, and Geilfus 2021"; "Segarra, Araus, and Kefauver 2022") and chronological order within a citation group (§1 "Hu et al. 2024; Chao et al. 2019").
- Reference list: journal titles must be spelt out (rendered list shows "J. R. Stat. Soc. Ser. B Stat. Methodol.", "Int. J. Remote Sens.", "Remote Sens."), issue numbers omitted ("Agronomy 10 (11)"), and the stray "Accessed 2026-09-21" removed from Arumugam et al. (2020). Lower-priority if the journal accepts free format at first submission.

### Figures and tables
- §3.3 "@fig-7 panel b" renders as "figure 7 panel b"; IJRS style is "figure 7(b)".
- Figure 4: the x-axis label "Mean ± SD across the four LOSSO folds" is shared by an RMSE panel with units and a unitless R² panel; label each panel's axis.
- Figure 2: the AGB sampling label "S" collides with the Zadoks "S" (Sowing); the caption explains it, but different symbols would remove the need.
- Supply figures at the resolution the T&F sheet requests (600 dpi for detailed figures) as separate files at submission.

### Layout and declarations
- Add a short running head.
- Data availability: archive a versioned release with a DOI (for example Zenodo) and state whether PlanetScope-derived values can be redistributed under the Planet licence.

---

## Criterion-Bound Judgements

Calibration status: `NOT_CALIBRATED`

Criterion source for every row: the configured seat focus for this panel (journal fit, contribution, RS technical bar, IJRS conventions) and the public T&F IJRS formatting sheet as a style reference. No formal venue criteria binding (`criteria_binding_unavailable`).

| Dimension | Criterion source | Judgement | Evidence anchor(s) | Rationale | Uncertainty / scope limit | Decision bearing? |
|---|---|---|---|---|---|---|
| Originality | Seat focus (1) | PARTLY_MEETS | text: §1 final paragraph "an evaluation protocol for satellite biomass models"; table: Table 5 GDD RF/XGBoost rows | Same-algorithm baseline is a useful new combination; protocol not formalised and gap not evidenced (W1, W2) | Novelty judged without a systematic literature search | yes, central to IJRS value |
| Methodological Rigor | Seat focus (2), RS processing only | PARTLY_MEETS | text: §2.2.5 "without speckle filtering or incidence angle normalization" | S1 processing below SAR standard; optical extraction single-pixel (W4) | CV statistics deferred to methodology seat | yes, repairable |
| Evidence Sufficiency | Seat focus, generalisation | PARTLY_MEETS | dataset: dataset.rds La Cancha 2022-09-09 mean 24.9 t ha⁻¹ | Four field-seasons support a reporting norm, not sensor-level generalisations; AGB plausibility unverified (W3, W6) | Agronomic ranges applied as adjacent expertise | yes |
| Argument Coherence | Seat focus (1) | MEETS | text: §4.4 scoping paragraph | Title, objectives, results and conclusion align; terminology for CV unit inconsistent | none identified | no |
| Writing Quality | Seat focus (3), IJRS style sheet | PARTLY_MEETS | text: §2.2.1 "cm³/cm³" | Clear prose; length and repetition (W7); style deviations (W8) | IJRS word limit not verified | no, minor |
| Literature Integration | Seat focus (1) | PARTLY_MEETS | text: §1 paragraph 4 "rarely report a phenological baseline" | Spatial CV literature cited; baseline/benchmark and C-band wheat literature thin; related own letter uncited (W2, W5) | none identified | yes, W5 is a policy matter |
| Significance & Impact | Seat focus (1) | PARTLY_MEETS | text: Abstract final sentence "Satellite AGB studies should report a phenological baseline" | Broad potential impact on reporting practice, conditional on W1 | none identified | yes |

The recommendation follows from four unresolved decision-bearing items, all repairable within one revision: the contribution must be made general and evidenced (W1, W2), the S1 processing must reach the journal's technical standard (W4), the AGB data and sample count must be verified and reported accurately (W3), and the related GRSL letter must be cited and differentiated in the manuscript (W5). Strengths in transparency and design (S1 to S3) do not offset these.

## Recommendation Signal
**Major Revision.** The manuscript addresses a real reporting problem in satellite crop-biomass ML and is in IJRS scope, and its transparent, same-algorithm baseline design is a genuine asset. It is not yet an IJRS paper because its general contribution is asserted rather than demonstrated, the Sentinel-1 analysis behind its only positive result uses avoidable low-standard processing, the field data need verification, and overlap with a related letter must be handled in the manuscript itself. None of these requires a new field campaign. I am one of five seats and do not make the final decision.
