# Editorial Decision Package

## Calibration Resolution

`calibration_status: NOT_CALIBRATED`

Current runtime boundary: this package is not upgraded from any candidate or prose named profile. `PROFILE_MEASURED` is unavailable because no closed profile artifact or replay validator is bound to this panel's execution topology.

Run mode: standard Synthesis Protocol (no v3.6.2 sprint contract, so no dimension verdict or fired condition arithmetic is emitted). Criteria binding: `criteria_binding_unavailable` on all five cards, so no claim of alignment with formal IJRS acceptance criteria is made anywhere in this package.

## Manuscript Information

- **Title**: Thermal time baselines for evaluating Sentinel-1, Sentinel-2 and PlanetScope predictors of wheat above-ground biomass under spatial cross-validation
- **Target venue**: International Journal of Remote Sensing, research article (author stated; simulated submission)
- **Manuscript ID**: not assigned
- **Files reviewed**: `manuscript/ijrs/manuscript_ijrs.qmd`, `manuscript/ijrs/frontmatter.tex`, `manuscript/ijrs/supplementary_ijrs.qmd`
- **Decision date**: 2026-10-07
- **Review round**: Round 1

## Review Panel Provenance (#540/#740)

- **Typed artifact**: none supplied. No replay valid `review-panel-provenance/1.0` artifact was handed to the synthesizer, so the carrier check fails closed. **[PROVENANCE-ARTIFACT-MISSING]**
- **Artifact SHA-256 / Panel ID / Normalized manifest SHA-256 / Execution topology SHA-256**: `unknown`
- **Fresh-context scope**: `within_panel_attempt_only` (does not compare retries or prior rounds)

| Provenance axis | Artifact-verified status | Dispatcher attestation (not artifact verified) |
|---|---|---|
| Role-separated | `unknown` | true (five configured seats: EIC, R1, R2, R3, DA) |
| Within-panel invocation-context separation | `unknown` | true (each seat ran as a separate fresh subagent context) |
| Blind to peer outputs | `unknown` | true (no seat had access to another seat's output or to the authoring conversation) |
| Model-family distinct | `unknown` | false (all seats Claude) |
| Provider distinct | `unknown` | false (all seats Anthropic) |
| Human-reviewer distinct | `unknown` | false (no accountable human reviewer on the panel) |

- **Binary independence claim**: not computed. Role diversity establishes only role separation.
- **Correlated-error disclosure**: All five seats and this synthesizer run on the same model family from the same provider. Their errors may be correlated: agreement among seats is weaker evidence than agreement among independent human experts, and a blind spot shared by the model family would be invisible to this panel. No cross model check was run (`ARS_CROSS_MODEL` unset), so Step 4b was skipped and no `[CROSS-MODEL-CHECKPOINT]` line is emitted. No accountable human reviewer took part.
- **Self-review disclosure**: The dispatching session co-wrote the IJRS version of this manuscript. That is why every seat was isolated from the authoring conversation. The synthesizer was likewise given only the reviewer cards, the Phase 0 report, the manuscript, and the dispatcher's factual verification notes listed below.
- **Dispatcher verification used as evidence**: the dispatching session independently checked four factual claims against code and data (Tmin bug, Sentinel-2 offset, PlanetScope Villa Baviera reflectance, plaintext credential). All four were CONFIRMED. Where this package relies on them they are cited as "dispatcher verification", not as review comments.

---

# Part 1: Editorial Decision Letter

Dear Authors,

Thank you for preparing your manuscript "Thermal time baselines for evaluating Sentinel-1, Sentinel-2 and PlanetScope predictors of wheat above-ground biomass under spatial cross-validation" for the International Journal of Remote Sensing. It was assessed by five role separated review seats: a Journal-Fit Reviewer (EIC), a methodology reviewer (R1), a SAR and optical domain reviewer (R2), a crop physiology and crop modelling reviewer (R3), and a Devil's Advocate (DA). Their execution provenance is reported above and is not reduced to an independence claim.

## Decision: Major Revision

`editorial_decision=major_revision`

All four scoring seats recommend Major Revision. The panel agrees that the central question (does a satellite model beat a simple season progress baseline at a field it has never seen) is useful for IJRS readers, and that the design has real strengths: preprocessing fitted inside folds, fixed hyperparameters, per fold reporting, and the same algorithm ∑GDD control. However, three Critical input defects, each confirmed by the dispatcher against code and data, mean that every numerical result in the manuscript was computed from corrupted predictors. Several further Major issues affect the baseline definition, the inference rule, and the evidence for the only positive satellite finding. All seats judge these repairable with existing data, which is why the decision is Major Revision rather than Reject. The revised manuscript will need a complete rerun and will be sent for re-review.

## Blocking Issues (immutable source order)

| Transport ref | Blocking issue | Source reviewer(s) | Evidence anchor | Resolving roadmap item |
|---|---|---|---|---|
| R14 | Sentinel-2 L2A reflectance after 25 January 2022 still carries the processing baseline 04.00 offset (+0.1), aligned with the fold structure | R2 (W1); dispatcher verification CONFIRMED | `dataset: data/processed/rds/dataset.rds and vi_extract.rds, S2 B1, B2, NDVI distributions by site season (pre-change floor about 0.02, post-change floor about 0.12)` | R14 |
| R15 | PlanetScope values at Villa Baviera are not plausible surface reflectance | R2 (W2); dispatcher verification CONFIRMED | `dataset: data/processed/rds/dataset.rds, Villa Baviera PS_B3 median 0.417, PS_EVI max 20.8, PS_EVI_cumsum max 742` | R15 |
| R18 | Daily Tmin is fixed at 1 °C by a coding error, so every ∑GDD value and every model using it is affected | R3 (W1); dispatcher verification CONFIRMED | `dataset: data/processed/rds/dataset.rds gdd_cumsum for villa_baviera 2020-09-15 (7.25) versus script/06_descargar_clima.R line 79` | R18 |

All other must_fix items (R1 to R19 below) are also required for acceptance; the three rows above are listed because, until they are corrected, no other result in the paper can be evaluated.

## Reviewer Summary

| Reviewer | Role | Recommendation | Confidence (scope disclosure only) |
|---|---|---|---|
| Journal-Fit Reviewer (EIC) | Associate Editor, agricultural applications, optical and SAR crop monitoring | Major Revision | 4 |
| Reviewer 1 | Statistician, spatial CV and model transferability | Major Revision | 4 |
| Reviewer 2 | SAR and optical crop monitoring | Major Revision | 4 |
| Reviewer 3 | Crop physiologist and crop modeller | Major Revision | 4 |
| Devil's Advocate | Fixed adversarial seat | N/A, findings only | N/A, per finding only |

### Step 1a: Reviewer Summary Matrix

| Dimension | Journal-Fit Reviewer (EIC) | R1 (Methodology) | R2 (Domain) | R3 (Cross-disciplinary) |
|---|---|---|---|---|
| Overall recommendation | Major Revision | Major Revision | Major Revision | Major Revision |
| Confidence / scope disclosure | 4; W3 agronomic plausibility at 3 (adjacent); CV statistics deferred to R1 | 4; SAR physics and agronomy declared out of scope | 4; four fold inference declared out of scope; raw rasters not in repository | 4; SAR processing declared out of scope |
| Key strengths | Same algorithm ∑GDD control (S1); per fold reporting (S2); explicit scoping in §4.4 (S3); open code (S4) | Baseline under identical folds (S1); in fold preprocessing and fixed hyperparameters (S2); per fold R² against held out mean (S3); held out group permutation (S4); same algorithm controls (S5) | Baseline under grouped CV (S1); honest S1 disclosure (S2); sound S1 phenology account (S3); exclusion of whole season missing PS red edge (S4) | Right baseline question (S1); same algorithm comparison (S2); candid limits (S3); DAS versus ∑GDD check (S4) |
| Key weaknesses | → Step 1b (W1 to W8) | → Step 1b (W1 to W12) | → Step 1b (W1 to W11, plus confidential note) | → Step 1b (W1 to W9) |
| Severity tags | 5 Major, 3 Minor | 7 Major, 5 Minor | 2 Critical, 6 Major, 3 Minor | 1 Critical, 5 Major, 3 Minor |
| # of questions | 4 | 4 | 4 | 6 |
| # of minor issues | about 20 (style, units, citations, figures, layout) | 6 | 5 | 5 |

DA (not part of consensus counts): 1 CRITICAL (C1), 8 MAJOR (M1 to M8), 5 MINOR (m1 to m5).

## Consensus Analysis

Consensus is computed per sub claim over the four non-DA seats (EIC, R1, R2, R3). A seat that did not mention a sub claim is counted as silent, not as agreeing. The full Step 1b inventory follows the letter.

### Points of Agreement

**[CONSENSUS-4]**: none. No sub claim was raised or corroborated by all four scoring seats.

**[CONSENSUS-3]** (three seats agree, the fourth is silent):

1. **SC-6: Dry AGB values above 30 t ha⁻¹ (maximum 43.5) and the implied growth rates need verification.** Raised by EIC (W3), R2 (W9) and R3 (W3), all Major; R1 silent. R3 adds the growth rate argument (La Cancha about 52 g m⁻² d⁻¹ between 4 August and 9 September 2022, above an RUE ceiling) and the late season drops of 6 to 10 t ha⁻¹. DA M8 makes the same point. → R2.
2. **SC-7: "153 destructive samples" includes 21 AGB = 0 values assigned at sowing; 132 were harvested.** Raised by EIC (W3), R1 (W5) and R3 (W4), all Major; R2 silent. R1 and R3 verified this in `script/07_procesar_datos.R`. DA M4 concurs. → R3.
3. **SC-8: The assigned zeros favour season progress predictors and inflate the headline metrics, so AGB > 0 metrics should be reported next to the headline numbers.** Raised by EIC (W3) and R3 (W4), corroborated by R1 (W5); all Major; R2 silent. → R3.
4. **SC-24: The ∑GDD base temperature rule described in §2.2.2 does not match the code.** Raised by R1 (W4), R2 (W5) and R3 (W2), all Major; EIC silent. → R11.
5. **SC-25: The base temperature switch is triggered by the observed tillering date of each field, including the held out one, which contradicts the §4.5 claim that the baseline needs only air temperature and the sowing date.** Raised by R1 (W4), R2 (W5) and R3 (W2), all Major; EIC silent. All three request a rerun with a single base temperature or a switch predicted from thermal time learned on training folds. DA M3 concurs. → R11.

### Corroborated findings (two seats, no conflict)

- **SC-9 / SC-10: Sentinel-1 processing is below field practice (mixed relative orbits and incidence angles, no speckle filter, no incidence normalisation, single pixel), and the SAR acquisition metadata are not reported.** EIC (W4) and R2 (W4), both Major. R1 describes the S1 data as speckle dominated (W2) but does not request reprocessing, so it is counted as silent here; R3 declared SAR out of scope. → R4.
- **SC-14: Abstract and §4.5 recommendations are worded more generally than four fields support.** EIC (W6, Minor) and R2 (Theoretical Framework comment, letter fallback). DA m2 concurs. → S6.
- **SC-18: "Thermal time dominates" restates a known agronomic relation; the remote sensing take away is not yet clear.** R3 (W5, Major) and EIC (Journal Fit comment, letter fallback). → R7.
- **SC-19: S1 time series per field against ∑GDD or Zadoks stage are missing.** EIC (Journal Fit comment) and R2 (W3). → S8.
- **SC-21: The S1+GDD gain needs negative controls (noise predictors, S1 permuted within sampling date).** R1 (W2) and R2 (W3), both Major. DA M1 concurs. → R9.
- **SC-28: Held out importance and ALE for correlated cumulative predictors do not support the interpretive claims.** R1 (W7, Major: correlated permutation, full data ALE) and R2 (W6, Major: cumulative sums act as clocks by construction, so the interpretation is circular; W2 notes the ∑PS EVI ALE step is a Villa Baviera artefact). → R13.
- **SC-33: Look ahead in the two sided filter, structural soil moisture missingness, and the inputs required in real time should be stated.** R1 (W11) and R3 (W8), both Minor. → S16.
- **SC-43: An anomaly scale evaluation (residual from the ∑GDD baseline, hybrid linear plus tree model, within date skill) is feasible with existing data and is needed to separate "satellites add little" from "this design cannot detect what satellites add".** R3 (W5, Major) and R1 (Discussion comment encouraging the residual model, letter fallback). DA M7 concurs. → R19.

### Single reviewer findings (retained and evaluated on their evidence)

These carry no consensus label. Several are nonetheless must_fix because the anchored evidence and, in four cases, the dispatcher verification establish them.

- EIC: SC-1/SC-2 protocol not formalised or shown to generalise (Major); SC-4 benchmark literature from yield forecasting missing (Major); SC-13 related GRSL letter and EarthArXiv preprint not cited in the manuscript (Major); SC-16 length (Minor); SC-17 IJRS style (Minor).
- R1: SC-20 between fold SD rule is not an inferential procedure (Major, confidence 5; DA M2 concurs); SC-22 no linear model on S1+GDD or satellite predictors (Major; DA C1 concurs); SC-23 winner's curse (Major; DA m3); SC-26 zeros remain in training (Major); SC-27 correlation filter removes ∑GDD from the All model in two folds (Major, confidence 5; DA M6); SC-29 interpretation done on All RF rather than S1+GDD RF (Major); SC-30 pseudo replication within 29 sampling dates (Minor); SC-31 Hidango folds are within site transfers (Minor; DA m4); SC-32 provenance of the 54 candidates (Minor); SC-34 seeds and versions (Minor).
- R2: SC-35 Sentinel-2 offset (Critical, dispatcher CONFIRMED); SC-36 PlanetScope Villa Baviera (Critical, dispatcher CONFIRMED); SC-37 cumulative index construction (Major); SC-38 optical processing described inaccurately (Major, verified in code); SC-39 missing SAR, RTM, PS harmonisation and spatial CV literature (Minor); SC-40 B1 and B9 based predictors, table 4 mismatch (Minor); SC-41 plaintext credential (confidential note, dispatcher CONFIRMED).
- R3: SC-42 Tmin fixed at 1 °C (Critical, dispatcher CONFIRMED); SC-44 linear ∑GDD baseline wrong in shape at both ends (Major); SC-45 RUE baseline absent (Major); SC-46 habit confounding of DAS versus ∑GDD (Minor); SC-47 ERA5-Land claim untested (Minor); SC-48 error by growth stage (Minor; DA m5).

## Points of Disagreement

The Journal-Fit Reviewer is the designated arbiter of SPLITs, but the EIC card is itself a party to four of the five below and did not address the opposing positions. Each is therefore resolved on the protocol's evidence first and expertise first principles, and the rationale is stated so the authors can contest it.

**Disagreement 1 (SC-3): Is the claim that phenological baselines are "rarely" reported adequately supported?**
- **EIC view (W2, Major)**: The gap is asserted, not shown; it is the paper's stated reason to exist and needs a reproducible literature tally plus the benchmark literature from yield forecasting.
- **R2 view (Detailed Comments, Research gap argument)**: "Persuasive. The lack of a phenological baseline under grouped validation is a real gap." R1 (S1) and R3 (S1) also accept the rarity premise in their strengths, without addressing its documentation.
- **Disagreement type**: Existence disagreement.
- **Editor's Resolution**: should_fix. The authors must either document the claim (a compact tally of recent wheat AGB studies by split type and baseline presence) or narrow the wording to what is cited.
- **Resolution Rationale**: Both sides agree the gap is probably real; they differ on whether it is demonstrated. On evidence, the manuscript cites no source for "rarely", and DA m1 points out that weather only benchmarks are routine in yield forecasting, which bears on how broadly the claim can be stated. The remedy EIC asks for does not conflict with R2's position. Because three seats accept the premise, the item is not made must_fix, but the authors must respond to it.

**Disagreement 2 (SC-5): Severity of the missing C-band wheat backscatter literature.**
- **EIC view (W2, Major)**: Part of a Major gap argument weakness.
- **R2 view (W10, Minor)**: The SAR crop literature is missing; "No alone; yes jointly with W5" in its criterion table.
- **Disagreement type**: Severity disagreement.
- **Editor's Resolution**: should_fix; both transported severities retained on the roadmap row.
- **Resolution Rationale**: On expertise, the domain owner for SAR literature is R2, who rates it Minor in isolation. The literature addition is required, but by itself it does not alter any result.

**Disagreement 3 (SC-11): Severity of the single pixel extraction for S2 and PlanetScope.**
- **EIC view (W4, Major)**: Apply the multi pixel extraction test to S2 and PS as part of the RS technical bar.
- **R2 view (W8, Minor)**: Positional uncertainty rather than plot size governs extraction; repeat with buffers as a sensitivity analysis.
- **Disagreement type**: Severity disagreement.
- **Editor's Resolution**: should_fix for S2 and PS. For S1, spatial averaging is already required within R4.
- **Resolution Rationale**: Expertise first defers to R2 on optical extraction. The S1 component, where speckle makes single pixels most damaging, is covered by the corroborated must_fix R4.

**Disagreement 4 (SC-12): Severity of the PlanetScope cross generation harmonisation statement.**
- **EIC view (W4, Major)**: State how Dove and SuperDove radiometry was handled.
- **R2 view (W2, Critical parent)**: After the Villa Baviera rebuild, state the sensor generation per site season and harmonise if needed.
- **Disagreement type**: Severity disagreement arising from different parent bundles.
- **Editor's Resolution**: should_fix as a standalone reporting item; the Critical defect itself is handled by must_fix R15, whose acceptance criteria include stating the sensor generation per site season.
- **Resolution Rationale**: R2's Critical rating attaches to the implausible Villa Baviera values (SC-36), not to the harmonisation statement. Both severities are transported unchanged.

**Disagreement 5 (SC-15): How serious is the statement that same algorithm comparisons "support a genuine Sentinel-1 contribution"?**
- **EIC view (W6, Minor)**: Move the S1 mechanism in §4.2 to a clearly hypothetical sentence.
- **R1 view (W2, Major)** and **R2 view (W3, Major)**: The claim is unsupported until noise controls are run. R1 computed that every sample has a unique S1_VH value while ∑GDD is constant within a date, and that the within date association of VH with AGB is weak (r = 0.16).
- **Disagreement type**: Severity disagreement.
- **Editor's Resolution**: must_fix (R6). Until R9 is done, the word "genuine" and the 30% and 23% figures must leave the abstract and Conclusion, or be explicitly qualified.
- **Resolution Rationale**: Evidence first: R1's within date computations and the paper's own §4.2 concession support a tie breaking explanation. Expertise first: the question sits within R1's (control design) and R2's (speckle statistics) domains. DA M1 makes the same argument independently.

## DA-CRITICAL Adjudication

`da_critical_adjudications: [C1=VALIDATED]`

**C1: The baseline's advantage is attributed to thermal time, but predictor content is confounded with model family.**
- **DA argument**: ∑GDD is the only predictor fitted with a linear model; every satellite set is fitted only with trees. Under matched algorithms, the paper's Table 5 shows S2 alone roughly equal to ∑GDD alone (RF 8.08 versus 8.19; XGBoost 7.93 versus 8.28) and All RF about 14% better than ∑GDD RF (7.04 versus 8.19). The paper applies same algorithm logic to credit S1 but not to test the headline. Remedy: fit LM, ridge or a monotone GAM on single satellite predictors that track season progress, on sensor group PCA scores, and on ∑GDD+S1 and ∑GDD+best optical index.
- **Corroboration**: R1 W2 independently identifies the missing linear S1+GDD model and GAM as "the missing cell of the algorithm by predictor design", and R1's Methodology comments repeat it. R3 W6 independently questions the linear ∑GDD form as the reference. EIC did not address the confound; its S1 strength (the same algorithm control is the paper's most original element) is the logic C1 extends.
- **Journal-Fit Reviewer assessment**: not given in the EIC card (silent on this point). The editor adjudicates on evidence.
- **Adjudication: VALIDATED.** The design gap is real and is established by arithmetic on the manuscript's own table: no satellite predictor was ever given the model form that made ∑GDD transfer well, so the evidence cannot attribute the advantage to thermal time rather than to linear form. The direction of the eventual conclusion is not settled: the satellite tree rows the DA used were computed from Sentinel-2 and PlanetScope inputs that R2 showed, and the dispatcher confirmed, to be radiometrically corrupted, and the ∑GDD rows from the Tmin defect. The numbers must be recomputed after R14, R15 and R18.
- **Required author response**: R10 (fit the missing linear satellite models) and revised wording of the abstract, §4.1 and Conclusion if a linear season progress satellite predictor matches the ∑GDD LM.
- **Decision bearing**: VALIDATED, so it blocks Accept. It is consistent with, and does not by itself change, the Major Revision decision.

The DA's MAJOR and MINOR findings are not counted in consensus. They are mapped onto roadmap items as corroboration: M1 → R6, R9; M2 → R8; M3 → R11; M4 → R3, S11; M5 → S21; M6 → R12; M7 → R19; M8 → R2; m1 → R1, S2; m2 → S6; m3 → S9; m4 → S12; m5 → S11, S20.

## Decision Rationale

The four scoring seats independently recommend Major Revision. Under the decision matrix in `editorial_decision_standards.md` (Major, Major, Major, Major), this gives Major Revision, and arbitration found no outlier to reconcile.

A stricter decision was considered. Three Critical findings (R2 W1, R2 W2, R3 W1), each confirmed by the dispatcher, show that the Sentinel-2 inputs for two and a half of four site seasons, the PlanetScope inputs for one site season, and every ∑GDD value are wrong. All results in Tables 5 and 6, Table S2, and Figures 4 to 8 will change, and R2 warns that the results section "may change substantially". Reject (resubmit encouraged) would be appropriate if these defects were unfixable or required new field data. They do not: every seat states that the issues are repairable with existing data, and the paper's methodological message (report a phenological baseline under grouped validation) does not depend on the corrupted values (R2 Recommendation Signal; R1 Criterion table; R3 Recommendation signal). A more lenient decision is unavailable because five CONSENSUS-3 sub claims, a validated DA-CRITICAL, and several Major single reviewer findings in the methodology owner's domain (SC-20, SC-27) remain unresolved.

The revision therefore has two layers. First, correct the inputs (R14, R15, R18) and the baseline definition (R11), then rerun the full LOSSO-CV. Second, change what the paper claims and how it supports those claims: paired inference (R8), negative controls and linear satellite models for the S1 and thermal time attributions (R9, R10), a fair All model (R12), interpretation that answers objective iii (R13), anomaly scale evaluation (R19), honest sample reporting (R3), verified AGB values (R2), and a contribution stated as either a formal protocol or a case study (R1). Two editorial matters are independent of the science and must be fixed before any submission: the related GRSL letter must be cited and differentiated in the manuscript (R5), and the plaintext credential must be removed from the public repository and rotated (R17).

If, after the rerun, the ranking or the thermal time conclusion changes, the authors should rewrite the paper around the corrected results rather than defend the current text. The panel expects the baseline recommendation to survive (R1, R2), but not necessarily the Sentinel-1 statements.

## Closing

We encourage you to consider the reviewers' comments carefully and submit a substantially revised manuscript. The revised manuscript will undergo another round of review. Please respond to every roadmap item using `templates/revision_response_template.md`, mark all changes, and include a cross reference of new section and table locations. A decision to decline an item is permitted but must be stated and argued; it remains visible to the re-review.

Sincerely,
Handling Editor (simulated, editorial synthesizer seat)

---

# Step 1b: Weakness Sub-Claim Inventory

Positions: `raised`, `corroborated`, `disputed`; seats not listed for a sub claim are `not-mentioned`. Severity is transported from the parent weakness; `[SEVERITY-SOURCE: letter-fallback]` marks a derived severity where the card gave none. Confidence is the per finding self report and has no weight. Ordering is by source seat (EIC, R1, R2, R3, DA), then weakness number.

| sub_claim_id | parent_weakness | reviewer_id | position | evidence_pointer | severity | confidence | disposition |
|---|---|---|---|---|---|---|---|
| SC-1 | EIC W1 protocol | EIC | raised | `text: §1 final paragraph "Beyond the case study, the analysis provides an evaluation protocol..."` | major | 4 | single reviewer |
| SC-2 | EIC W1 protocol (generality beyond four fields) | EIC | raised | same as SC-1 | major | 4 | single reviewer |
| SC-3 | EIC W2 gap claim unevidenced | EIC | raised | `text: §1 paragraph 4 "Satellite AGB studies rarely report..."` | major | 4 | SPLIT |
| SC-3 | | R2 | disputed | Detailed Comments, "Research gap argument: Persuasive" | minor [SEVERITY-SOURCE: letter-fallback] | 4 [CONFIDENCE-SOURCE: report-level] | |
| SC-4 | EIC W2 benchmark literature from yield forecasting | EIC | raised | same as SC-3 | major | 4 | single reviewer |
| SC-5 | EIC W2 C-band wheat literature | EIC | raised | same as SC-3 | major | 4 | SPLIT |
| SC-5 | R2 W10 missing SAR crop literature | R2 | corroborated (lower severity) | `absence: Introduction and Discussion, expected water cloud model, S1 wheat backscatter studies...` | minor | 4 | |
| SC-6 | EIC W3 AGB plausibility | EIC | raised | `text: §4.6 "The observed AGB values above 30 t ha⁻¹ exceed..."` | major | 3 | CONSENSUS-3 (R1 silent) |
| SC-6 | R2 W9 | R2 | corroborated | `figure: figure 2, Hidango North S5 and La Cancha S5 points above 30 t ha⁻¹` | major | 3 | |
| SC-6 | R3 W3 | R3 | corroborated | `dataset: dataset.rds, la_cancha 2022-09-09 (mean 24.9, max 37.7 t ha⁻¹)` | major | 4 | |
| SC-7 | EIC W3 sample count | EIC | raised | `text: §4.6` (W3 anchor); abstract "153 destructive samples" | major | 3 | CONSENSUS-3 (R2 silent) |
| SC-7 | R1 W5 | R1 | corroborated | `text: frontmatter.tex abstract "153 destructive samples, 2020 to 2023"` | major | 5 | |
| SC-7 | R3 W4 | R3 | corroborated | `text: Abstract "153 destructive samples, 2020 to 2023"` | major | 5 | |
| SC-8 | EIC W3 zeros inflate headline; AGB>0 in abstract | EIC | raised | as SC-7 | major | 3 | CONSENSUS-3 (R2 silent) |
| SC-8 | R1 W5 zeros anchor baseline | R1 | corroborated | as SC-7 | major | 5 | |
| SC-8 | R3 W4 zero excluded metrics side by side | R3 | raised | as SC-7 | major | 5 | |
| SC-9 | EIC W4 S1 below IJRS bar | EIC | raised | `text: §2.2.5 "Sentinel-1 data were used without speckle filtering or incidence angle normalization..."` | major | 4 | corroborated (2) |
| SC-9 | R2 W4 | R2 | raised | `text: section 2.2.4 "Each series therefore combined at least two relative orbits..."` | major | 5 | |
| SC-10 | EIC W4 SAR metadata missing from Table 2 | EIC | raised | as SC-9 | major | 4 | corroborated (2) |
| SC-10 | R2 W4 and Detailed Comments (incidence or orbit column; 10 m pixel spacing; GRD terrain correction wording) | R2 | corroborated | as SC-9 | major | 5 | |
| SC-11 | EIC W4 multi pixel test for S2 and PS | EIC | raised | as SC-9 | major | 4 | SPLIT |
| SC-11 | R2 W8 positional uncertainty, buffer extraction | R2 | corroborated (lower severity) | `text: section 2.2.4 "each sampling plot is fully contained within a single pixel..."` | minor | 4 | |
| SC-12 | EIC W4 Dove and SuperDove handling | EIC | raised | as SC-9 | major | 4 | SPLIT |
| SC-12 | R2 W2 harmonisation suggestion | R2 | corroborated (higher severity) | `dataset: dataset.rds, Villa Baviera PS_B3 median 0.417...` | critical | 5 | |
| SC-13 | EIC W5 related GRSL letter | EIC | raised | `absence: Introduction, Methods, Discussion and reference list...` | major | 5 | single reviewer |
| SC-14 | EIC W6 recommendations too general | EIC | raised | `text: §4.5 "These results suggest three practices..."` | minor | 4 | corroborated (2) |
| SC-14 | R2 Theoretical Framework ("this caveat should also appear in the abstract") | R2 | corroborated | Detailed Comments, Application depth | minor [SEVERITY-SOURCE: letter-fallback] | 4 [CONFIDENCE-SOURCE: report-level] | |
| SC-15 | EIC W6 S1 mechanism stated beyond evidence | EIC | raised | as SC-14 | minor | 4 | SPLIT |
| SC-15 | R1 W2 "genuine" S1 claim | R1 | disputed (severity) | `text: §4.2 "Comparisons under the same algorithm support a genuine Sentinel-1 contribution"` | major | 4 | |
| SC-15 | R2 W3 | R2 | disputed (severity) | `text: section 4.2 "part of this gain may come from breaking the steps..."` | major | 4 | |
| SC-16 | EIC W7 length | EIC | raised | `text: §4.6 "With n=4, most differences..."` | minor | 4 | single reviewer |
| SC-17 | EIC W8 IJRS style | EIC | raised | `text: §2.2.1 "Soil water content (cm³/cm³)..."` | minor | 5 | single reviewer |
| SC-18 | EIC Journal Fit (most transferable result is non RS) | EIC | corroborated | Detailed Comments, Journal Fit | major [SEVERITY-SOURCE: letter-fallback] | 4 [CONFIDENCE-SOURCE: report-level] | corroborated (2) |
| SC-18 | R3 W5 thermal time dominance is known agronomy | R3 | raised | `text: §4.1 "This is expected from crop physiology..."` | major | 4 | |
| SC-19 | EIC Journal Fit (S1 time series plots) | EIC | raised | Detailed Comments, Journal Fit | minor [SEVERITY-SOURCE: letter-fallback] | 4 [CONFIDENCE-SOURCE: report-level] | corroborated (2) |
| SC-19 | R2 W3 (S1 temporal profiles against Zadoks) | R2 | corroborated | as SC-15 R2 row | major | 4 | |
| SC-20 | R1 W1 SD rule, paired inference | R1 | raised | `text: §2.3.4 "treat differences smaller than the between-fold SD as indistinguishable"` | major | 5 | single reviewer (EIC S2 cites the sentence as transparency but defers statistics to R1; not counted as a dispute) |
| SC-21 | R1 W2 noise controls | R1 | raised | as SC-15 R1 row | major | 4 | corroborated (2) |
| SC-21 | R2 W3 noise controls | R2 | raised | as SC-15 R2 row | major | 4 | |
| SC-22 | R1 W2 missing LM on S1+GDD and GAM | R1 | raised | as SC-15 R1 row | major | 4 | single reviewer + DA C1 |
| SC-23 | R1 W3 winner's curse | R1 | raised | `table: Table 5, rows "S1+GDD RF" and "S1+W RF"` | major | 5 | single reviewer |
| SC-24 | R1 W4 text and code mismatch | R1 | raised | `text: §4.5 "it requires only daily air temperature and the sowing date"` | major | 4 | CONSENSUS-3 (EIC silent) |
| SC-24 | R2 W5 | R2 | raised | same | major | 4 | |
| SC-24 | R3 W2 | R3 | raised | `text: §2.2.2 "6°C was used from stem elongation to ripening"` | major | 5 | |
| SC-25 | R1 W4 held out phenology in baseline | R1 | raised | as SC-24 | major | 4 | CONSENSUS-3 (EIC silent) |
| SC-25 | R2 W5 | R2 | raised | as SC-24 | major | 4 | |
| SC-25 | R3 W2 | R3 | raised | as SC-24 | major | 5 | |
| SC-26 | R1 W5 zeros remain in training; imputed SM on zero rows | R1 | raised | as SC-7 R1 row | major | 5 | single reviewer |
| SC-27 | R1 W6 step_corr removes ∑GDD | R1 | raised | `table: Table S3, rows "La Cancha" and "Villa Baviera"` | major | 5 | single reviewer |
| SC-28 | R1 W7 importance and ALE on correlated predictors | R1 | raised | `text: §2.3.5 "computed for the RF model fitted to all 153 samples"` | major | 4 | corroborated (2) |
| SC-28 | R2 W6 cumulative indices as clocks; interpretation circular | R2 | corroborated | `text: section 4.2 "cumulative indices whose ALE profiles increased monotonically..."` | major | 4 | |
| SC-29 | R1 W7 headline model never interpreted | R1 | raised | as SC-28 | major | 4 | single reviewer |
| SC-30 | R1 W8 pseudo replication | R1 | raised | `absence: §2.3.4 and §4.6, number of distinct sampling dates per fold` | minor | 5 | single reviewer |
| SC-31 | R1 W9 Hidango folds within site | R1 | raised | `table: Table 1, row "Hidango North and South", station 260 shared` | minor | 4 | single reviewer |
| SC-32 | R1 W10 provenance of 54 candidates | R1 | raised | `text: §2.3.2 "This screening used all samples..."` | minor | 3 | single reviewer |
| SC-33 | R1 W11 look ahead and structural missingness | R1 | raised | `text: §2.3.3 "Soil moisture was the only predictor with missing daily values..."` | minor | 5 | corroborated (2) |
| SC-33 | R3 W8 real time inputs | R3 | corroborated | `text: §4.5 "a gridded temperature product such as ERA5-Land"` | minor | 4 | |
| SC-34 | R1 W12 seeds and versions | R1 | raised | `absence: §2.4 Software, package versions and seed handling` | minor | 4 | single reviewer |
| SC-35 | R2 W1 S2 PB04.00 offset | R2 | raised | `dataset: dataset.rds and vi_extract.rds, S2 B1, B2, NDVI by site season` | critical | 5 | single reviewer; dispatcher CONFIRMED |
| SC-36 | R2 W2 PS Villa Baviera not SR | R2 | raised | `dataset: dataset.rds, Villa Baviera PS_B3 median 0.417, PS_EVI max 20.8` | critical | 5 | single reviewer; dispatcher CONFIRMED |
| SC-37 | R2 W6 cumulative construction | R2 | raised | as SC-28 R2 row | major | 4 | single reviewer |
| SC-38 | R2 W7 optical processing described inaccurately | R2 | raised | `text: section 2.2.4 "spectral bands were interpolated using a Kalman smoothing approach"` | major | 5 | single reviewer |
| SC-39 | R2 W10 missing WCM, RTM, PS harmonisation, spatial CV debate literature; Chave et al. | R2 | raised | as SC-5 R2 row | minor | 4 | single reviewer |
| SC-40 | R2 W11 B1 and B9 predictors; table 4 mismatch | R2 | raised | `table: table 3, row 7 "WI" defined as NIR/Water vapour` | minor | 4 | single reviewer |
| SC-41 | R2 confidential note, plaintext credential | R2 | raised | `dataset: script/01_descargar_imagenes.R, edl_netrc() call` (credential not reproduced) | critical [SEVERITY-SOURCE: letter-fallback] | 4 [CONFIDENCE-SOURCE: report-level] | single reviewer; dispatcher CONFIRMED |
| SC-42 | R3 W1 Tmin = 1 °C | R3 | raised | `dataset: dataset.rds gdd_cumsum villa_baviera 2020-09-15 (7.25) versus script/06_descargar_clima.R line 79` | critical | 5 | single reviewer; dispatcher CONFIRMED |
| SC-43 | R3 W5 anomaly scale evaluation | R3 | raised | as SC-18 R3 row | major | 4 | corroborated (2) |
| SC-43 | R1 Discussion (residual model "cheap to run... encourage including it") | R1 | corroborated | Detailed Comments, Discussion | minor [SEVERITY-SOURCE: letter-fallback] | 4 [CONFIDENCE-SOURCE: report-level] | |
| SC-44 | R3 W6 linear ∑GDD shape wrong at ends | R3 | raised | `figure: Figure 5, GDD LM trajectories below zero at DAS 0` | major | 4 | single reviewer |
| SC-45 | R3 W6 RUE baseline absent | R3 | raised | as SC-44 | major | 4 | single reviewer |
| SC-46 | R3 W7 habit confounding | R3 | raised | `text: §4.1 "in particular between the winter wheat sites..."` | minor | 4 | single reviewer |
| SC-47 | R3 W8 ERA5-Land claim untested | R3 | raised | `text: §4.5 "a gridded temperature product such as ERA5-Land"` | minor | 4 | single reviewer |
| SC-48 | R3 W9 error by growth stage | R3 | raised | `text: §4.5 "corresponds to about 47% of the mean observed AGB"` | minor | 4 | single reviewer |

Note on severity splits: where one side's severity is a letter fallback (derived by the synthesizer), the difference is not treated as a reviewer stated disagreement, because the reviewer did not recommend a severity. Where both sides stated explicit tags that differ materially (SC-5, SC-11, SC-12, SC-15), the sub claim is classified SPLIT and arbitrated above.

Surface form parity check: no sub claim was down rated for informal wording or credited for technical phrasing. The four dispatcher verified claims are credited because they were checked against code and data, not because of how they were written. The R1 and DA numerical computations (paired differences, bootstrap intervals, Table 5 arithmetic) were computed on outputs produced from the corrupted inputs; their logic is accepted, and the numbers must be recomputed after the rerun.

---

# Part 2: Revision Roadmap

`schema_version: revision-roadmap/1.0` (reviewer owned core). No bound block manifest was supplied, so `proposed_targets` are given as section locators in `manuscript/ijrs/manuscript_ijrs.qmd` and the supplement, with allowed operations limited to `replace_block`, `insert_after`, `delete_block`. Items are in immutable source order. `R<n>` and `S<n>` are transport references, not work ranks. Author triage is collected later in a separate sidecar.

## Required Revisions (Must Fix)

| Transport ref | Revision Item | Sub-Claim(s) | Severity | Evidence Anchor | Confidence | Source | Obligation class | Cost scope | Bounded consequence |
|---|---|---|---|---|---|---|---|---|---|
| R1 | Formalise the evaluation protocol as a stated procedure, or reframe as a case study with methodological implications | SC-1, SC-2 | major | `text: §1 final paragraph "...evaluation protocol for satellite biomass models that can be applied to other crops and sensors"` | 4, editorial assessment | EIC (DA m1) | must_fix | section: §1 final paragraph, §2.3, §4.5, Conclusion, title and abstract | claim_scope_unsupported → claim: "evaluation protocol... applied to other crops and sensors" |
| R2 | Verify AGB values above 30 t ha⁻¹ and implied growth rates; justify, correct, or run a sensitivity analysis without them | SC-6 | major (EIC, R2, R3) | `dataset: dataset.rds, la_cancha 2022-09-09 (mean 24.9, max 37.7 t ha⁻¹)`; `figure: figure 2` | EIC 3, R2 3, R3 4 | EIC, R2, R3 (DA M8) | must_fix | re_analysis: field records, drying and scaling check, sensitivity rerun; section §2.2.1, §4.6 | evidence_gap_remains → dataset: dataset.rds AGB > 30 t ha⁻¹ |
| R3 | Report 132 destructive samples plus 21 assigned zeros, and report AGB > 0 metrics next to the headline values | SC-7, SC-8 | major (EIC, R1, R3) | `text: Abstract "153 destructive samples, 2020 to 2023"` | EIC 3, R1 5, R3 5 | EIC, R1, R3 (DA M4) | must_fix | section: abstract, §2.2.1, Table 5 or supplement table | reporting_requirement_unmet → section: abstract and §2.2.1 |
| R4 | Reprocess Sentinel-1 (single relative orbit or incidence normalisation, speckle filter or spatial mean) and report SAR acquisition metadata; report current and improved processing | SC-9, SC-10 | major (EIC, R2) | `text: §2.2.5 "Sentinel-1 data were used without speckle filtering or incidence angle normalization..."` | EIC 4, R2 5 | EIC, R2 | must_fix | re_analysis: S1 extraction and LOSSO rerun; section §2.2.4, Table 2 | evidence_gap_remains → claim: S1 alone weakest set; S1+GDD gain |
| R5 | Cite and differentiate the related GRSL letter and EarthArXiv preprint in the manuscript | SC-13 | major | `absence: Introduction, Methods, Discussion and reference list, expected citation of the related GRSL letter and EarthArXiv preprint` | 5, editorial policy | EIC | must_fix | section: §1 or §2.3, reference list, cover letter | editorial_conformance_unmet → manuscript |
| R6 | Remove "genuine" Sentinel-1 contribution wording and the 30% and 23% figures from abstract and Conclusion unless R9 controls support them | SC-15 | minor (EIC) / major (R1, R2): SPLIT, arbitrated must_fix | `text: §4.2 "Comparisons under the same algorithm support a genuine Sentinel-1 contribution"` | EIC 4, R1 4, R2 4 | EIC, R1, R2 (DA M1) | must_fix | sentence: abstract, §4.2, §4.3, Conclusion | claim_scope_unsupported → claim: "genuine Sentinel-1 contribution" |
| R7 | Restate the headline as the size of the satellite increment over a known physiological baseline, not the dominance of thermal time itself | SC-18 | major (R3); EIC major [SEVERITY-SOURCE: letter-fallback] | `text: §4.1 "This is expected from crop physiology..."` | R3 4; EIC 4 [CONFIDENCE-SOURCE: report-level] | R3, EIC | must_fix | section: abstract, §1 objectives, §4.1, Conclusion | interpretive_ambiguity_remains → claim: "Thermal time carried most of the AGB signal" |
| R8 | Replace the between fold SD rule with paired per fold differences against the baseline, an uncertainty interval, and the minimum detectable difference; drop fold counts as evidence or give their sign test p value | SC-20 | major | `text: §2.3.4 "treat differences smaller than the between-fold SD as indistinguishable"` | 5, paired CV comparisons | R1 (DA M2) | must_fix | re_analysis: paired and hierarchical bootstrap on OOF predictions; section §2.3.4, Table 5, Figure 4 | acceptance_criterion_unmet → section: §2.3.4 decision rule |
| R9 | Run negative controls for the S1+GDD gain: ∑GDD plus noise predictors (several seeds), ∑GDD plus S1 permuted within sampling date, ∑GDD plus S1 permuted across site seasons | SC-21 | major (R1, R2) | `text: §4.2 "Comparisons under the same algorithm support a genuine Sentinel-1 contribution"` | R1 4, R2 4 | R1, R2 (DA M1) | must_fix | re_analysis: additional LOSSO runs with identical folds, recipe and hyperparameters | evidence_gap_remains → table: Table 5 S1+GDD rows |
| R10 | Fit the missing algorithm by predictor cells: LM (or ridge or monotone GAM) on S1+GDD, on single season progress satellite predictors, and on sensor group summaries | SC-22; DA C1 | major (R1); DA CRITICAL | `table: Table 5 rows GDD RF 8.19, GDD XGBoost 8.28, S2 RF 8.08, S2 XGBoost 7.93, All RF 7.04` | R1 4; DA 4 | R1, DA | must_fix | re_analysis: linear and GAM fits under LOSSO; Table 5 | claim_scope_unsupported → claim: advantage attributable to thermal time rather than linear form |
| R11 | Describe the ∑GDD base temperature rule exactly as implemented, and rerun baselines and GDD sets with a single base temperature or a switch predicted from thermal time on training folds | SC-24, SC-25 | major (R1, R2, R3) | `text: §4.5 "it requires only daily air temperature and the sowing date"`; `text: §2.2.2 "6°C was used from stem elongation to ripening"` | R1 4, R2 4, R3 5 | R1, R2, R3 (DA M3) | must_fix | re_analysis: recompute ∑GDD and rerun; section §2.2.2, §4.5 | method_reproducibility_unresolved → section: §2.2.2 |
| R12 | Exempt ∑GDD (and arguably the weather variables) from the within fold correlation filter; rerun All, S2+W, S2+W+S1 and importance; report retention per fold | SC-27 | major | `table: Table S3, rows "La Cancha" and "Villa Baviera", first removed predictor ∑GDD` | 5, verified in code and Table S3 | R1 (DA M6) | must_fix | re_analysis: recipe change and rerun; Table S3, Figure 7 | evidence_gap_remains → claim: "RF with all 54 predictors did not improve on the baseline" |
| R13 | Rework predictor interpretation: conditional permutation within sampling date or ∑GDD strata, per fold ALE on fold models, and an interpretation of cumulative predictors that accounts for their construction | SC-28 | major (R1, R2) | `text: §2.3.5 "computed for the RF model fitted to all 153 samples"`; `text: section 4.2 "cumulative indices whose ALE profiles increased monotonically..."` | R1 4, R2 4 | R1, R2 | must_fix | re_analysis: importance and ALE rerun; §2.3.5, §3.3, §4.2, Figures 7 and 8 | claim_scope_unsupported → claim: objective iii, predictors that drive held out performance |
| R14 | Remove the processing baseline 04.00 offset from Sentinel-2 L2A data, recompute all S2 indices, cumulative sums and figure 2, and rerun the full LOSSO-CV | SC-35 | critical | `dataset: dataset.rds and vi_extract.rds, S2 B1, B2, NDVI distributions by site season (pre-change floor about 0.02, post-change floor about 0.12)` | 5, verified on extracted values; dispatcher CONFIRMED | R2 | must_fix | re_analysis: reprocess S2 from `script/02_calcular_vis.R` onward; §2.2.4 | acceptance_criterion_unmet → dataset: S2 predictors after 25 January 2022 |
| R15 | Rebuild Villa Baviera PlanetScope predictors from surface reflectance, cross check PS against S2 per site season, state sensor generation per site season, and rerun | SC-36 | critical | `dataset: dataset.rds, Villa Baviera PS_B3 median 0.417, PS_EVI max 20.8, PS_EVI_cumsum max 742` | 5, verified on extracted values; dispatcher CONFIRMED | R2 (DA M5 consistent) | must_fix | re_analysis: reprocess PS for Villa Baviera; supplementary PS versus S2 scatterplot; §2.2.3 | acceptance_criterion_unmet → dataset: PS predictors, Villa Baviera 2020-2021 |
| R16 | Rewrite §2.2.4 so it matches the implementation: Kalman gap filling (not smoothing) on indices, 5 day composites and grid, reprojection method, manual scene exclusions, clamping | SC-38 | major | `text: section 2.2.4 "spectral bands were interpolated using a Kalman smoothing approach"` | 5, verified in code | R2 | must_fix | section: §2.2.4 | method_reproducibility_unresolved → section: §2.2.4 |
| R17 | Remove the plaintext Earthdata credential from `script/01_descargar_imagenes.R` and from repository history, rotate it, and read it from an environment variable before the data availability statement points to the repository | SC-41 | critical [SEVERITY-SOURCE: letter-fallback] | `dataset: script/01_descargar_imagenes.R, edl_netrc() call (credential not reproduced)` | 4 [CONFIDENCE-SOURCE: report-level]; dispatcher CONFIRMED | R2 (confidential note) | must_fix | other: repository history and credential rotation (no manuscript change) | editorial_conformance_unmet → manuscript: Data availability statement |
| R18 | Fix the Tmin aggregation in `script/06_descargar_clima.R` line 79, recompute daily GDD and ∑GDD for all stations, add a check table of seasonal ∑GDD per site, and rerun all analyses | SC-42 | critical | `dataset: dataset.rds gdd_cumsum for villa_baviera 2020-09-15 (7.25) versus script/06_descargar_clima.R line 79` | 5, verified against station data; dispatcher CONFIRMED | R3 | must_fix | re_analysis: climate pipeline from script 06 onward; §2.2.2, all °C d values in text | acceptance_criterion_unmet → dataset: clima.rds daily GDD |
| R19 | Add anomaly scale evaluation: skill on residuals from the ∑GDD baseline, a hybrid linear ∑GDD plus residual model under LOSSO, metrics by growth stage or ∑GDD window, and within date skill | SC-43 | major (R3); R1 minor [SEVERITY-SOURCE: letter-fallback] | `text: §4.1 "This is expected from crop physiology: AGB accumulates with thermal time..."` | R3 4; R1 4 [CONFIDENCE-SOURCE: report-level] | R3, R1 (DA M7) | must_fix | re_analysis: residual and hybrid models on existing OOF framework; new results subsection | evidence_gap_remains → claim: satellites add little beyond season progress |

### Required Item Details

**R1: Formalise the evaluation protocol or reframe as a case study**
- **Problem**: The paper claims a general evaluation protocol but presents it as three recommendations in one Discussion paragraph, untested beyond four fields; its components are individually established.
- **Source**: EIC W1 ("Without a formal protocol and some demonstration beyond one small dataset, an IJRS reader receives a local negative case study with a recommendation attached"); DA m1.
- **Requirement**: State the protocol as a numbered procedure in Methods (hold out unit, baseline set, same algorithm baseline, R² definition, zero sample sensitivity, fold count reporting) and refer to it from Discussion and Conclusion; preferably apply the baseline step to one independent dataset. Otherwise reframe title and abstract as a case study and remove "applied to other crops and sensors".
- **Acceptance criteria**: The manuscript either contains an explicit numbered protocol referenced from Discussion and Conclusion (with or without an external demonstration), or no longer claims a general protocol in title, abstract, §1 and Conclusion.

**R2: Verify AGB values above 30 t ha⁻¹**
- **Problem**: Seven samples exceed 30 t ha⁻¹ (maximum 43.5); implied growth rates of about 52 to 55 g m⁻² d⁻¹ exceed an RUE ceiling; late season mean drops of 6 to 10 t ha⁻¹; these points dominate the largest errors.
- **Source**: EIC W3, R2 W9, R3 W3 (CONSENSUS-3); DA M8.
- **Requirement**: Document drying protocol, fresh versus dry mass, 0.25 m² scaling, quadrat placement and cutting height; report grain yield and harvest index per field where available; rerun the key comparisons with flagged samples excluded or down weighted.
- **Acceptance criteria**: §2.2.1 documents the verification of high values and a sensitivity analysis excluding or down weighting flagged samples is reported for the baseline and leading configurations.

**R3: Correct the sample count and report AGB > 0 metrics with the headline**
- **Problem**: 21 of the 153 "destructive samples" are AGB = 0 values assigned at sowing; they favour season progress predictors and inflate pooled metrics.
- **Source**: EIC W3, R1 W5, R3 W4 (CONSENSUS-3); DA M4.
- **Requirement**: State "132 destructive samples plus 21 assigned zero values at sowing" in abstract and §2.2.1, and give AGB > 0 metrics for the baseline and leading configurations in the abstract and alongside Table 5 (full table in the supplement).
- **Acceptance criteria**: No passage describes 153 destructive samples, and AGB > 0 metrics appear in the abstract and for every Table 5 row in the main text or supplement.

**R4: Reprocess Sentinel-1 and report SAR metadata**
- **Problem**: The S1 series mixes relative orbits and incidence angles, is not speckle filtered or incidence normalised, uses a single pixel and nearest acquisition filling; Table 2 gives generic mission text instead of processing parameters.
- **Source**: EIC W4, R2 W4 (corroborated).
- **Requirement**: Rerun with one relative orbit per site or incidence normalisation, a speckle filter or spatial mean over a window or field buffer; report S1 alone, S1+GDD and All under current and improved processing; replace generic Table 2 rows with orbit direction, relative orbit numbers, incidence angle range, GEE preprocessing steps and averaging space (linear or dB); state 10 m pixel spacing.
- **Acceptance criteria**: Results for S1 alone, S1+GDD and All are reported under both processings, and Table 2 or a supplementary table lists orbit, incidence angle and preprocessing parameters per site.

**R5: Cite and differentiate the related GRSL letter**
- **Problem**: A related letter on the same AGB dataset (sequential sensor ablation under leave one site out validation) and an EarthArXiv preprint are disclosed only in the cover letter; Table 6 is itself a sequential ablation.
- **Source**: EIC W5.
- **Requirement**: Cite both in the manuscript, state in one or two sentences what this paper adds, consider moving Table 6 to the supplement if it substantially reproduces the letter, provide the letter to the editor confidentially, and complete cover letter placeholders.
- **Acceptance criteria**: The manuscript cites the letter and preprint with an explicit differentiation statement, and the cover letter contains no placeholders.

**R6: Withdraw the "genuine Sentinel-1 contribution" wording pending controls**
- **Problem**: The abstract and Conclusion present the 30% and 23% reductions as evidence that S1 adds information, while §4.2 concedes the gain may come from breaking tree step functions.
- **Source**: EIC W6, R1 W2, R2 W3 (SPLIT on severity, arbitrated Major); DA M1.
- **Requirement**: Remove "genuine" and the 30% and 23% figures from abstract and Conclusion, and present the S1 mechanism as a hypothesis, unless the R9 controls show S1 outperforming them.
- **Acceptance criteria**: Every statement crediting S1 with canopy information is either supported by R9 results or explicitly worded as a hypothesis.

**R7: Restate the headline relative to known physiology**
- **Problem**: That AGB tracks thermal time within a season is established crop physiology; the paper's remote sensing contribution is the size of the satellite increment, which the current framing does not foreground.
- **Source**: R3 W5; EIC Journal Fit comment.
- **Requirement**: Reword abstract, objectives, §4.1 and Conclusion so the finding is the measured satellite increment over a physiological baseline, using the R19 results.
- **Acceptance criteria**: Abstract and Conclusion state the satellite increment over the baseline as the finding, and the dominance of thermal time is presented as expected rather than as a result.

**R8: Replace the between fold SD rule with paired inference**
- **Problem**: The rule uses the variance of single model RMSE across folds, which reflects fold difficulty, for comparisons that are paired; it is not a test, and four fold sign counts cannot reach p < 0.05.
- **Source**: R1 W1; DA M2.
- **Requirement**: Report per fold paired absolute ΔRMSE against the baseline with mean, SD and a hierarchical bootstrap interval (dates within held out folds), state the minimum detectable paired difference, apply one decision rule throughout, and drop or qualify fold counts.
- **Acceptance criteria**: §2.3.4 defines a paired decision rule, and Table 5 reports paired absolute differences with an uncertainty interval for every configuration.

**R9: Negative controls for the S1+GDD gain**
- **Problem**: Every sample has a unique S1 value while ∑GDD is constant within a date; any within date varying predictor could smooth the tree step function, so the gain may be algorithmic.
- **Source**: R1 W2, R2 W3 (corroborated); DA M1.
- **Requirement**: Under identical folds, recipe and hyperparameters, fit ∑GDD plus Gaussian noise (several seeds), ∑GDD plus S1 permuted within sampling date, and ∑GDD plus S1 permuted across site seasons, with RF and XGBoost; compare with S1+GDD using the R8 paired framework.
- **Acceptance criteria**: Control results are reported in Table 5 or the supplement, and the S1 claims in abstract and Conclusion are consistent with how S1+GDD compares with the controls.

**R10: Fit linear models on satellite predictors and on S1+GDD**
- **Problem**: ∑GDD is the only predictor ever fitted with a linear model, so the evidence cannot separate thermal time from linear form (DA C1, VALIDATED).
- **Source**: R1 W2 and Methodology comments; DA C1.
- **Requirement**: Fit LM (or ridge or a monotone GAM) on S1+GDD, on single season progress satellite predictors, on sensor group summaries such as PCA scores, and on ∑GDD plus the best optical index, under LOSSO.
- **Acceptance criteria**: Table 5 contains linear model rows for satellite predictor sets and S1+GDD, and the abstract, §4.1 and Conclusion attribute the baseline's advantage in a way consistent with those rows.

**R11: Correct and neutralise the ∑GDD base temperature rule**
- **Problem**: Text says 2 °C to tillering and 6 °C from stem elongation; code switches at the observed tillering date of each field, including the held out one, so the baseline carries field phenology.
- **Source**: R1 W4, R2 W5, R3 W2 (CONSENSUS-3); DA M3.
- **Requirement**: Describe the implemented rule exactly; rerun baselines and GDD sets with a single base temperature (for example 0 °C) or a switch triggered by a thermal time threshold learned on training folds; state operational inputs accurately in §4.5. Also answer R3 Q4 on whether stage dates were field observed or assigned to the nearest Sentinel acquisition.
- **Acceptance criteria**: §2.2.2 matches the code, a baseline that uses no held out phenology is reported, and §4.5 lists the inputs that baseline actually needs.

**R12: Protect ∑GDD from the correlation filter**
- **Problem**: In the La Cancha and Villa Baviera folds, `step_corr` removed ∑GDD from the All set, so All is not a superset of the baseline and ∑GDD importance averages over folds where it was absent.
- **Source**: R1 W6; DA M6.
- **Requirement**: Apply the filter only among satellite predictors, rerun All, S2+W, S2+W+S1 and the importance analysis, and report per fold retention.
- **Acceptance criteria**: ∑GDD is retained in every fold of every set that includes it, and Figure 7 or Table S3 shows per fold retention.

**R13: Rework predictor interpretation**
- **Problem**: Permutation pairs late season cumulative indices with early ∑GDD values; ALE is computed on a full data refit; cumulative sums act as clocks by construction, so the interpretation is partly circular.
- **Source**: R1 W7, R2 W6 (corroborated).
- **Requirement**: Use conditional permutation within sampling date or ∑GDD strata, compute ALE per fold on fold models with their spread, and qualify any statement that cumulative indices "acted as season progress" in light of how they are built.
- **Acceptance criteria**: Importance and ALE in §3.3 and Figures 7 and 8 come from fold models with conditional permutation, and §4.2 does not present a construction property of cumulative sums as an empirical finding.

**R14: Correct the Sentinel-2 baseline 04.00 offset**
- **Problem**: S2 L2A DN were scaled by 1/10000 without removing the +1000 offset introduced on 25 January 2022, giving a reflectance bias of about +0.1 for late Hidango 2021-2022, Hidango 2022-2023 and La Cancha 2022-2023, aligned with the fold structure.
- **Source**: R2 W1; dispatcher verification CONFIRMED (5th percentile of S2_B2 and S2_B4 about 0.02 before 25 January 2022, 0.11 to 0.23 after).
- **Requirement**: Subtract 1000 from DN for items with processing baseline ≥ 04.00 (or apply the Planetary Computer harmonisation), recompute indices, cumulative sums and figure 2, rerun the full analysis, and report the correction in §2.2.4.
- **Acceptance criteria**: S2 reflectance floors are comparable across all site seasons in a supplementary check, and all S2 dependent results are recomputed from corrected inputs.

**R15: Rebuild PlanetScope predictors for Villa Baviera**
- **Problem**: Villa Baviera PS values (median blue about 0.46, red about 0.30, NIR about 0.87; EVI up to about 20.8) are not plausible surface reflectance.
- **Source**: R2 W2; dispatcher verification CONFIRMED; DA M5 is consistent.
- **Requirement**: Verify asset type and scale factor per scene, rebuild from surface reflectance, add a PS versus S2 red and NIR cross check per site season to the supplement, state the PS sensor generation per site season, and rerun.
- **Acceptance criteria**: Villa Baviera PS reflectance falls in the range of the other site seasons and of coincident S2, and all PS dependent results are recomputed.

**R16: Describe the optical processing as implemented**
- **Problem**: §2.2.4 describes Kalman smoothing of spectral bands, single pixel extraction without averaging, and 208 S2 images; the code performs Kalman gap filling of indices only, uses 5 day composites on a 0.0001° grid, applies bilinear reprojection, removes scenes manually and clamps VIs to [−5, 50].
- **Source**: R2 W7.
- **Requirement**: Rewrite §2.2.4 to state these steps and reasons, or change the processing to match the text and report it.
- **Acceptance criteria**: Every processing step in the code from `02_calcular_vis.R` to `05_extraer_vis.R` is described in §2.2.4 or the supplement, and no described step is absent from the code.

**R17: Remove and rotate the plaintext credential**
- **Problem**: The public repository cited in the data availability statement contains a plaintext NASA Earthdata credential.
- **Source**: R2 confidential note; dispatcher verification CONFIRMED.
- **Requirement**: Rotate the credential, purge it from repository history, and read it from an environment variable or netrc file.
- **Acceptance criteria**: No credential string is present in any file or commit reachable from the public repository, and the old credential has been revoked.

**R18: Fix the Tmin aggregation and recompute ∑GDD**
- **Problem**: `min(temp_minima.na.rm=T)` evaluates to 1, so daily Tmin is 1 °C everywhere; every ∑GDD value, every °C d figure in the text, and every GDD model is affected.
- **Source**: R3 W1; dispatcher verification CONFIRMED (stored daily GDD for Villa Baviera, 1 to 10 October 2020, equals (1 + Tmax)/2 − Tbase on 10 of 11 days against freshly downloaded station 103 data).
- **Requirement**: Correct the aggregation, recompute daily GDD for all stations, state the GDD method (McMaster and Wilhelm 1997 method), add a seasonal ∑GDD check table per site, and rerun all analyses.
- **Acceptance criteria**: Daily GDD reproduces (Tmax + Tmin)/2 − Tbase from station data for a documented sample of days at every station, and all °C d values and GDD based results are recomputed.

**R19: Add anomaly scale evaluation**
- **Problem**: Site season by date means explain about 92% of AGB variance, so pooled LOSSO metrics mainly measure trajectory reconstruction; the paper cannot distinguish "satellites add little" from "this design cannot detect what satellites add".
- **Source**: R3 W5; R1 Discussion comment; DA M7.
- **Requirement**: Evaluate whether satellite predictors explain held out residuals of the ∑GDD baseline, fit the hybrid linear ∑GDD plus residual model under LOSSO, report metrics by growth stage or ∑GDD window, and report within date skill.
- **Acceptance criteria**: A results subsection reports residual skill, the hybrid model, stage stratified metrics and within date correlation for the baseline and leading satellite sets.

## Suggested Revisions (Should Fix / Consider)

| Transport ref | Revision Item | Sub-Claim(s) | Severity | Evidence Anchor | Confidence | Source | Obligation class | Cost scope | Bounded consequence |
|---|---|---|---|---|---|---|---|---|---|
| S1 | Document or narrow the claim that phenological baselines are "rarely" reported (literature tally or narrower wording) | SC-3 | major (EIC) / minor [SEVERITY-SOURCE: letter-fallback] (R2): SPLIT | `text: §1 paragraph 4 "Satellite AGB studies rarely report a phenological baseline..."` | EIC 4; R2 4 [CONFIDENCE-SOURCE: report-level] | EIC, R2 | should_fix | section: §1 paragraph 4 or supplementary table | claim_scope_unsupported → claim: "rarely report" |
| S2 | Add benchmark and baseline literature from operational yield forecasting to the gap argument | SC-4 | major | as S1 | 4 | EIC (DA m1) | should_fix | section: §1 | reader_traceability_reduced → section: §1 |
| S3 | Add C-band wheat backscatter literature | SC-5 | major (EIC) / minor (R2): SPLIT | as S1; `absence: Introduction and Discussion` (R2 W10) | EIC 4, R2 4 | EIC, R2 | should_fix | section: §1, §4.2 | reader_traceability_reduced → section: §4.2 |
| S4 | Buffer or multi pixel extraction sensitivity for S2 and PS; state GNSS device and accuracy | SC-11 | major (EIC) / minor (R2): SPLIT | `text: section 2.2.4 "each sampling plot is fully contained within a single pixel..."` | EIC 4, R2 4 | EIC, R2 | should_fix | re_analysis: extraction with buffers; §2.2.4 | evidence_gap_remains → claim: single pixel extraction adequate |
| S5 | State how Dove and SuperDove radiometry was handled and harmonise if needed | SC-12 | major (EIC) / critical (R2 parent): SPLIT | `dataset: dataset.rds, Villa Baviera PS_B3 median 0.417` | EIC 4, R2 5 | EIC, R2 | should_fix | section: §2.2.3 | method_reproducibility_unresolved → section: §2.2.3 |
| S6 | Separate the methodological recommendation from site specific empirical findings in abstract, §4.5 and Conclusion; add the within season qualifier to the abstract | SC-14 | minor | `text: §4.5 "These results suggest three practices..."` | EIC 4; R2 4 [CONFIDENCE-SOURCE: report-level] | EIC, R2 (DA m2) | should_fix | sentence: abstract; section §4.5, Conclusion | claim_scope_unsupported → section: abstract |
| S7 | Reduce length by about 20 to 25% (compress §1 paragraphs 2 and 3, state the four fold caveat once in Methods and once in Limitations) | SC-16 | minor | `text: §4.6 "With n=4, most differences..."` | 4 | EIC | should_fix | section: §1, §2.2.4, §4 | editorial_conformance_unmet → manuscript |
| S8 | Apply IJRS style (Oxford spelling, units, numbered equations, citation and reference format, running head, figure part references) | SC-17 | minor | `text: §2.2.1 "Soil water content (cm³/cm³)..."` | 5 | EIC | should_fix | sentence: throughout; reference list | editorial_conformance_unmet → manuscript |
| S9 | Add S1 time series per field (VV, VH, VH/VV against ∑GDD or Zadoks stage) | SC-19 | minor [SEVERITY-SOURCE: letter-fallback] (EIC) / major (R2 parent) | EIC Journal Fit comment; `text: section 4.2` (R2 W3) | EIC 4 [CONFIDENCE-SOURCE: report-level]; R2 4 | EIC, R2 | should_fix | section: new figure in §3 or supplement | reader_traceability_reduced → figure: S1 profiles |
| S10 | Label the best configuration ranking as exploratory, state the number of configurations, and quantify selection optimism (nested selection or rank stability) | SC-23 | major | `table: Table 5, rows "S1+GDD RF" and "S1+W RF"` | 5 | R1 (DA m3) | should_fix | sentence: abstract, §3.2; optional re_analysis | claim_scope_unsupported → claim: "S1+GDD performed best" |
| S11 | Sensitivity analysis removing the 21 zero rows from training as well as evaluation; explain their nonzero predictor values and imputed SM | SC-26 | major | `text: frontmatter.tex abstract "153 destructive samples"` | 5 | R1 (DA M4) | should_fix | re_analysis: rerun without zero rows | evidence_gap_remains → table: Table 5 |
| S12 | Apply held out and conditional importance to S1+GDD RF, the model behind the positive claim | SC-29 | major | `text: §2.3.5 "computed for the RF model fitted to all 153 samples"` | 4 | R1 | should_fix | re_analysis: importance for S1+GDD RF | interpretive_ambiguity_remains → claim: S1 contribution |
| S13 | Report sampling dates per fold, use date level resampling, consider date aggregated metrics | SC-30 | minor | `absence: §2.3.4 and §4.6, number of distinct sampling dates per fold` | 5 | R1 (DA M4, m5) | should_fix | section: §2.3.4; re_analysis optional | reporting_requirement_unmet → section: §2.3.4 |
| S14 | State that the Hidango folds are within site transfers; add a three fold leave one site out sensitivity | SC-31 | minor | `table: Table 1, row "Hidango North and South", station 260 shared` | 4 | R1 (DA m4) | should_fix | re_analysis: three fold rerun for baselines and leading sets | claim_scope_unsupported → claim: transfer to fields not used for training |
| S15 | Document how the 54 candidates were produced, including "restored" predictors, or regenerate them by a rule blind to earlier results | SC-32 | minor | `text: §2.3.2 "This screening used all samples..."` | 3 | R1 | should_fix | section: §2.3.2 | method_reproducibility_unresolved → section: §2.3.2 |
| S16 | State look ahead from two sided filtering, structural SM missingness and cross field imputation, and the inputs needed in real time; consider a causal filter sensitivity | SC-33 | minor | `text: §2.3.3 "Soil moisture was the only predictor with missing daily values..."` | R1 5, R3 4 | R1, R3 | should_fix | section: §2.3.3, §4.5 | interpretive_ambiguity_remains → claim: in season estimation |
| S17 | Repeat leading configurations over 10 or more seeds; list package versions | SC-34 | minor | `absence: §2.4 Software, package versions and seed handling` | 4 | R1 | should_fix | re_analysis: seed repeats; section §2.4 | method_reproducibility_unresolved → section: §2.4 |
| S18 | Rebuild cumulative optical predictors above a start of season baseline or per unit thermal time; drop cumulative raw bands such as ∑B1 | SC-37 | major | `text: section 4.2 "cumulative indices whose ALE profiles increased monotonically..."` | 4 | R2 | should_fix | re_analysis: feature rebuild and rerun | evidence_gap_remains → claim: optical cumulative predictors add nothing beyond ∑GDD |
| S19 | Add water cloud model, RTM based retrieval, PS harmonisation and spatial CV critique literature; replace Chave et al. (2014) | SC-39 | minor | `absence: Introduction and Discussion, expected water cloud model...` | 4 | R2 | should_fix | section: §1, §2.3.4, §4 | reader_traceability_reduced → section: §1 |
| S20 | Remove or justify B1 and B9 based predictors; reconcile Table 4 with the code (MCARI/OSAVI2) | SC-40 | minor | `table: table 3, row 7 "WI" defined as NIR/Water vapour` | 4 | R2 | consider | section: Tables 3 and 4; optional re_analysis | method_reproducibility_unresolved → table: Table 4 |
| S21 | Report per fold wins and the median next to the mean; analyse the Villa Baviera PS failure and discuss Hidango South as evidence on condition sensing | — (DA M5) | major (DA band) | `table: Table S2 / rev2_loso_limpio_por_fold.csv, PS RF villa_baviera 14.62, S2 RF la_cancha 12.86, PS RF hidango_2022-2023 4.91 vs GDD LM 6.69` | 4 | DA | should_fix | section: abstract, §3.2, §4 | interpretive_ambiguity_remains → claim: "Every model built on one sensor transferred worse than this baseline" |
| S22 | Add a logistic or expolinear ∑GDD baseline fitted on training folds | SC-44 | major | `figure: Figure 5, GDD LM out of fold trajectories below zero at DAS 0` | 4 | R3 | should_fix | re_analysis: additional baseline | evidence_gap_remains → table: Table 5 baselines |
| S23 | Add an RUE baseline (∑ fAPAR × PAR with station radiation) and discuss crop model assimilation as the established alternative | SC-45 | major | as S22 | 4 | R3 | should_fix | re_analysis: RUE model; section §1, §4 | evidence_gap_remains → claim: satellites failed versus used in physics free form |
| S24 | Show DAS versus ∑GDD per fold and temper §4.1 regarding the spring sown Villa Baviera field | SC-46 | minor | `text: §4.1 "in particular between the winter wheat sites..."` | 4 | R3 | should_fix | section: §4.1; supplementary table | claim_scope_unsupported → claim: thermal time transfers better than calendar time |
| S25 | Test ∑GDD from ERA5-Land against station ∑GDD, or soften the operational sentence | SC-47 | minor | `text: §4.5 "a gridded temperature product such as ERA5-Land"` | 4 | R3 | consider | sentence: §4.5, or re_analysis | claim_scope_unsupported → claim: ERA5-Land suitability |
| S26 | Report RMSE and relative RMSE by growth stage and link each to the decision it would serve; report the within date noise floor | SC-48 | minor | `text: §4.5 "corresponds to about 47% of the mean observed AGB"` | 4 | R3 (DA m5) | should_fix | re_analysis: stage stratified metrics; §4.5 | interpretive_ambiguity_remains → claim: 47% relative error |

The minor issue lists in each card (EIC: style, units, citations, figures, layout; R1: figure and table suggestions; R2: Table 2, Table 4, Figure 8, subscripts; R3: GDD method statement, dry weight verification, sampling date table, station distance) are not decomposed here. The authors should address them in the response letter under S8 or the nearest related item.

## Source-Traceability Checklist (immutable source order; not a work order)

- [ ] R1, obligation `must_fix`: Formalise the protocol or reframe as a case study
- [ ] R2, obligation `must_fix`: Verify AGB values above 30 t ha⁻¹ and run a sensitivity analysis
- [ ] R3, obligation `must_fix`: Report 132 destructive samples plus 21 assigned zeros; give AGB > 0 metrics with the headline
- [ ] R4, obligation `must_fix`: Reprocess Sentinel-1 and report SAR metadata
- [ ] R5, obligation `must_fix`: Cite and differentiate the GRSL letter and preprint
- [ ] R6, obligation `must_fix`: Withdraw "genuine S1 contribution" wording pending controls
- [ ] R7, obligation `must_fix`: Restate the headline relative to known physiology
- [ ] R8, obligation `must_fix`: Replace the between fold SD rule with paired inference
- [ ] R9, obligation `must_fix`: Negative controls for the S1+GDD gain
- [ ] R10, obligation `must_fix`: Linear models on satellite predictors and S1+GDD
- [ ] R11, obligation `must_fix`: Correct and neutralise the ∑GDD base temperature rule
- [ ] R12, obligation `must_fix`: Protect ∑GDD from the correlation filter
- [ ] R13, obligation `must_fix`: Rework predictor importance and ALE interpretation
- [ ] R14, obligation `must_fix`: Correct the Sentinel-2 baseline 04.00 offset and rerun
- [ ] R15, obligation `must_fix`: Rebuild Villa Baviera PlanetScope predictors and rerun
- [ ] R16, obligation `must_fix`: Describe the optical processing as implemented
- [ ] R17, obligation `must_fix`: Remove and rotate the plaintext credential
- [ ] R18, obligation `must_fix`: Fix the Tmin aggregation and recompute ∑GDD
- [ ] R19, obligation `must_fix`: Add anomaly scale evaluation
- [ ] S1 to S19, S21 to S24, S26, obligation `should_fix`: as tabulated above
- [ ] S20, S25, obligation `consider`: as tabulated above

## Journal-Supplied Deadline

- **Exact deadline from source letter**: NOT PROVIDED. No deadline, duration or work estimate is inferred.

## Response Letter Instructions

Please respond with `templates/revision_response_template.md`, item by item, including: the response and change for each Required Revision; a response to each Suggested Revision (adopted, or the reason for not adopting); tracked or colour marked changes; and a cross reference table of new section, table and figure locations. Because R14, R15 and R18 change every numerical result, please also include a before and after table of the main Table 5 rows so reviewers can see what moved.

---

# Part 3: Reviewer Report Summary (Appendix)

**Journal-Fit Reviewer (EIC)**: Major Revision | Confidence 4. The topic fits IJRS and the same algorithm baseline is a genuine asset, but the general contribution is asserted rather than demonstrated, S1 processing is below the journal's technical bar, the AGB data need verification, and the related GRSL letter must be handled in the manuscript.

**Reviewer 1 (Methodology)**: Major Revision | Confidence 4. The grouped CV design is clean, but the only inferential rule uses the wrong variance, the S1 evidence lacks a noise control, the baseline uses held out phenology, the correlation filter removes ∑GDD from the All model in two folds, and assigned zeros are counted as samples.

**Reviewer 2 (Domain)**: Major Revision | Confidence 4. Two Critical radiometric defects (Sentinel-2 baseline 04.00 offset; implausible PlanetScope reflectance at Villa Baviera) are aligned with the fold structure and contaminate every optical result; S1 processing and the S1+GDD noise explanation also need work.

**Reviewer 3 (Perspective)**: Major Revision | Confidence 4. The title predictor is miscomputed (Tmin fixed at 1 °C), several AGB values exceed physiological limits, and the analysis does not yet separate a remote sensing finding from the known physiology of thermal time.

**Devil's Advocate**: N/A, findings only. Key challenge (C1, VALIDATED): the evidence isolates a linear model rather than thermal time, because no satellite predictor was ever fitted with the model form that made ∑GDD transfer.

---

## Machine-readable lines

```
calibration_status: NOT_CALIBRATED
criteria_binding: criteria_binding_unavailable
review_panel_provenance: artifact_missing (all axes unknown at artifact level; dispatcher attested: role_separated=true, fresh_context=true, blind_to_peer_outputs=true, model_family_distinct=false, provider_distinct=false, human_distinct=false)
cross_model_check: not_run (ARS_CROSS_MODEL unset)
da_critical_adjudications: [C1=VALIDATED]
editorial_decision=major_revision
must_fix: [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17, R18, R19]
blocking: [R14, R15, R18]
```
