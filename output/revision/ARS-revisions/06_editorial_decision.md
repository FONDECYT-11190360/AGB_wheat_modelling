# Editorial Decision

## Manuscript Information
- **Title**: How much do satellite sensors add to thermal time? A leave-one-site-season-out evaluation of Sentinel-1, Sentinel-2, and PlanetScope predictors for wheat above-ground biomass
- **Manuscript ID**: ARS-Review-2026-09-17
- **Submission Date**: 2026-09-17
- **Decision Date**: 2026-09-17
- **Review Round**: Round 1

## Review Panel Provenance

This decision is based on a five-reviewer panel assessment conducted under academic-paper-reviewer v1.11.1 in full mode. The panel comprised four configured roles (Journal-Fit Reviewer, Methodology Reviewer, Domain Reviewer, Crop Physiology Reviewer) plus the fixed Devil's Advocate seat. Each reviewer examined the manuscript independently from their assigned perspective without access to peer outputs before committing their reports.

**Panel composition**:
- **Journal-Fit Reviewer**: Associate Editor perspective, precision agriculture and operational remote sensing focus
- **Reviewer 1 (Methodology)**: Statistical methodology, machine learning validation, spatial cross-validation expertise
- **Reviewer 2 (Domain)**: Agricultural remote sensing, multi-sensor crop monitoring, Sentinel-1/2/PlanetScope expertise
- **Reviewer 3 (Crop Physiology)**: Wheat phenology, thermal time modeling, Mediterranean wheat systems
- **Devil's Advocate**: Fixed adversarial seat, stress-testing core arguments and detecting logical fallacies

Calibration status: `NOT_CALIBRATED` — current live reviews operate without a wired candidate profile; quality rubrics provide criterion-bound narrative guidance only.

---

## Decision

### **Major Revision** — Substantial revisions needed, re-review required after revision

---

## Blocking Issues (3, immutable source order)

| Transport ref | Blocking issue | Source reviewer(s) | Evidence anchor | Resolving roadmap item |
|---------------|----------------|--------------------|-----------------|------------------------|
| B1 | Undocumented predictor screening process with potential data leakage — The TODO comment at line 218 indicates the process for screening 53 VIs to 43 candidate predictors is not documented. If AGB was used in this screening, this constitutes data leakage that invalidates all reported performance metrics. | EIC, R1, DA (C1) | `text: Methods §2.3.2, line 218 "<!-- TODO (authors): describe how the 43 candidate predictors in script 14 (vars) were screened..."` | R1 |
| B2 | Omission of standard SAR preprocessing (speckle filtering, incidence angle normalization) without justification or sensitivity analysis — Sentinel-1 data were used without these standard agricultural SAR preprocessing steps. Since S1 alone was the weakest predictor set and the paper concludes satellite data adds minimal value, this preprocessing choice critically limits generalizability. | EIC, R2, DA (C3) | `text: Discussion §4.4 "without speckle filtering or incidence-angle normalization for Sentinel-1, which may have reduced the contribution of SAR data"` | R2 |
| B3 | Comparison between linear baselines and nonlinear satellite models conflates algorithmic and predictor contributions — The GDD baseline uses OLS while satellite sets use RF/XGBoost. Without testing RF/XGBoost on GDD alone, the satellite contribution cannot be isolated from the nonlinear modeling contribution. | R1, DA (M1) | `text: Methods §2.3.4 "The DAS and ∑GDD baselines...were fitted with ordinary least squares linear regression (LM). All sets except the two baselines were fitted with random forest (RF) and extreme gradient boosting (XGBoost)"` | R3 |

---

## Reviewer Summary

| Reviewer | Role | Recommendation | Confidence |
|----------|------|---------------|------------|
| Journal-Fit Reviewer | Associate Editor, *Computers and Electronics in Agriculture*, precision agriculture focus | Major Revision | 5 |
| Reviewer 1 | Associate Professor of Statistics, spatial cross-validation and ML transferability expertise | Major Revision | 5 |
| Reviewer 2 | Senior research scientist, agricultural remote sensing, 20+ years with Sentinel missions | Major Revision | 5 |
| Reviewer 3 | Professor of Crop Physiology, wheat phenology and thermal time modeling | Minor Revision | 5 |
| Devil's Advocate | Fixed adversarial seat | N/A — findings only | 4 |

---

## Consensus Analysis

### Points of Agreement (Consensus)

**[CONSENSUS-5]** (All 4 scoring reviewers + Devil's Advocate agree):

1. **The predictor screening process must be documented** — All five reviewers identified the TODO comment at line 218 as a critical unresolved methodological issue. If AGB was used to screen predictors before cross-validation, this constitutes data leakage that invalidates the study's performance estimates. (EIC W1 Critical; R1 W1 Critical; R2 did not explicitly flag but acknowledged methodological rigor is partly unmet; R3 acknowledged methodological rigor meets standards; DA C1 Critical).

2. **The LOSO-CV design is rigorous and appropriate** — All four scoring reviewers praised the leave-one-site-season-out cross-validation design as the correct validation scheme for testing model transferability to new locations and seasons. (EIC S1; R1 S1; R2 states "appropriate for testing transferability"; R3 states "rigorous").

3. **The phenological baseline comparison is a valuable contribution** — All four reviewers recognized the explicit comparison against thermal time baselines under the same validation scheme as novel and important. The recommendation that future AGB studies report phenological baselines was highlighted as actionable. (EIC S2; R1 criterion judgment "Originality MEETS"; R2 criterion judgment "Originality MEETS"; R3 S1, criterion judgment "Originality MEETS").

4. **Transparent reporting of limitations** — All four reviewers commended the manuscript's unusual transparency about limitations, including acknowledgment of small sample size (n=4 site-seasons), SAR preprocessing omissions, predictor set fold variation, and prior analysis of the dataset with different choices. (EIC S4; R1 Discussion comment "limitation acknowledgment is excellent"; R2 Discussion comment; R3 overall positive; DA Observation 1 "strong methodological transparency").

**[CONSENSUS-4]** (All 4 scoring reviewers agree):

5. **The small number of site-seasons (n=4) limits statistical power** — Three reviewers explicitly identified this as a major limitation (EIC criterion judgment "Evidence Sufficiency PARTLY_MEETS"; R1 W3 Major; R2 criterion judgment "Evidence Sufficiency MEETS but acknowledged in limitations"). R3 judged "Evidence Sufficiency MEETS" and did not raise this as a weakness, accepting it as adequate for exploratory findings. Consensus: The limitation is real but transparently reported.

6. **Fixed hyperparameters are methodologically sound for n=4 folds** — All four reviewers agreed that using fixed hyperparameters rather than tuning them is appropriate given the small number of folds, as tuning would introduce optimistic bias. (EIC did not comment on this specifically but accepted the overall methodology; R1 S2 Strength; R2 accepted the methodology; R3 accepted the methodology; R1 W4 notes lack of sensitivity analysis but accepts the fixed-hyperparameter rationale).

**[CONSENSUS-3]** (3/4 scoring reviewers agree, the 4th silent):

7. **SAR preprocessing omissions are a critical limitation** — Three reviewers (EIC, R2, DA) identified the omission of speckle filtering and incidence angle normalization for Sentinel-1 as a critical or major issue that limits generalizability. R1 did not focus on SAR preprocessing specifics, being more concerned with general cross-validation design. Consensus: This is a serious limitation that must be addressed. (EIC W3 Major; R2 W1 Critical; DA C3 Critical; R1 silent).

8. **Operational decision context is underdeveloped** — Three reviewers (EIC, R3, DA) noted that the statement that 5.80 t/ha RMSE is "too coarse for field-level decisions" lacks agronomic context or citation of decision thresholds. R1 did not comment on operational applications. Consensus: The operational framing needs strengthening. (EIC W2 Major; R3 did not raise as weakness but noted "From a crop management perspective, what error level would be acceptable?"; DA M6 Major; R1 silent).

### Points of Disagreement

**Disagreement 1: Severity of within-season sampling bias**
- **Devil's Advocate view**: The within-season sampling design (5-6 phenological stages per site-season) systematically favors the GDD baseline because GDD monotonically increases through the season and the model learns a temporal relationship rather than testing spatial transferability at a given growth stage. This is a **Critical** design flaw (DA C2).
- **R3 view**: The within-season sampling may favor the GDD baseline, but the finding that thermal time dominates is still physiologically meaningful and valuable. This is a **Minor** limitation that deserves discussion clarification but does not invalidate the study (R3 W1 Minor).
- **EIC and R1 view**: Did not identify this specific issue, focusing instead on other methodological concerns.
- **R2 view**: Did not identify this specific issue, focusing on remote sensing preprocessing.
- **Disagreement type**: Severity disagreement
- **Editor's Resolution**: The within-season sampling design is a **significant limitation** that should be discussed, but it does not rise to the level of a blocking issue. The study's design tests whether GDD predicts AGB across phenological stages within and across site-seasons, which is a valid (if different) question from "does satellite data differentiate fields at a given phenological stage?" The finding that GDD dominates is both physiologically expected (R3) and methodologically informative. However, the discussion should acknowledge that operational applications often require field comparisons at a fixed growth stage, where GDD variance is minimal and satellite data's value may be higher.
- **Resolution Rationale**: R3's crop physiology expertise supports that the finding is physiologically plausible. DA's concern about temporal confounding is valid but overstates the severity: the LOSO-CV still tests transferability to new site-seasons (the primary research question), and the within-season samples are not the same as within-site random splits that would be truly problematic. The issue is better framed as a scope limitation (the design tests a specific use case) rather than a fatal flaw. **Classification: Should Fix (S2)** to add discussion clarification about the within-season sampling design and its implications for operational applications.

**Disagreement 2: Overall recommendation (Minor vs. Major Revision)**
- **R3 view**: The paper merits **Minor Revision** because the physiological findings are sound, the GDD baseline is appropriately chosen, and the main issues are discussion clarifications and agronomic context additions (R3 overall recommendation: Minor Revision).
- **EIC, R1, R2 view**: The paper requires **Major Revision** because critical methodological issues must be resolved (predictor screening, SAR preprocessing) and substantial discussion revision is needed for operational context, spatial CV benchmarks, and sensor-specific interpretations (all three recommended Major Revision).
- **Disagreement type**: Decision severity disagreement
- **Editor's Resolution**: **Major Revision** is appropriate. While R3 correctly identifies that the crop physiology aspects are sound, the three blocking issues (B1: predictor screening, B2: SAR preprocessing, B3: linear vs. nonlinear comparison) require substantial methodological clarification, justification, or additional analysis beyond what a Minor Revision typically entails. The predictor screening TODO (B1) is acknowledged by all reviewers as critical and must be definitively resolved. The SAR preprocessing issue (B2) requires either a sensitivity analysis, explicit justification of the minimal-preprocessing choice, or prominent acknowledgment in the abstract/conclusion that findings apply to minimally preprocessed data. The linear-vs-nonlinear comparison (B3) requires either testing RF/XGBoost on GDD alone or discussing this confound as a limitation. These are not Minor-level discussion additions.
- **Resolution Rationale**: The blocking issues require substantive methodological work (e.g., clarifying the predictor screening process, potentially re-running analyses if leakage is found, or conducting SAR preprocessing sensitivity analysis) or substantial framing changes (if the authors choose to justify minimal preprocessing rather than test it). Minor Revision is reserved for cases where the core methodology is sound and only discussion/presentation refinements are needed. Here, the core methodology has unresolved questions (B1, B2, B3) that exceed Minor Revision scope.

**Disagreement 3: Whether fold-specific correlation filter is a major flaw**
- **R1 view**: The fold-specific correlation filter creates predictor set variation across folds (e.g., ∑GDD removed in La Cancha fold), which could bias cross-fold comparisons. This is a **Major** methodological concern requiring discussion and possibly sensitivity analysis (R1 W2 Major).
- **EIC, R2, R3, DA view**: Did not identify this as a major issue. EIC and R2 acknowledged the fold-specific preprocessing is mentioned in the manuscript but did not flag it as problematic. R3 did not comment. DA mentioned it as M3 Major but did not prioritize it as highly as C1-C3.
- **Disagreement type**: Existence disagreement (whether this is a problem at all)
- **Editor's Resolution**: The fold-specific correlation filter is **technically correct** (the filter should be estimated on training data only) but introduces a subtle interpretability challenge. The manuscript transparently reports this (Methods §2.3.3), and R1's concern is that readers may not fully appreciate its implications for comparing the "All" set across folds. However, this is more of a transparency and discussion issue than a fundamental flaw. The fact that ∑GDD was removed in one fold is interesting and deserves follow-up discussion (did that fold perform worse? is this due to removing the most important predictor or due to site characteristics?), but it does not invalidate the study.
- **Resolution Rationale**: R1's expertise in cross-validation design is authoritative, and the concern is legitimate. However, the manuscript already acknowledges the fold-specific predictor variation, and the issue affects interpretation more than validity. **Classification: Should Fix (S3)** to add discussion of which predictors were removed in each fold (especially the La Cancha fold where ∑GDD was removed) and whether this affected the cross-fold comparison.

---

## Decision Rationale

This manuscript addresses an important and timely question for precision agriculture: whether satellite remote sensing adds value beyond simple thermal time for wheat biomass estimation. The study's main finding that accumulated growing degree days (∑GDD) alone achieves R²=0.53 and outperforms most satellite-based models is both sobering and valuable for the field. The best satellite configuration (RF with Sentinel-1 + ∑GDD, R²=0.58) provides only a 5.9% RMSE improvement that is not reproduced with XGBoost, suggesting that satellite data's contribution is modest and configuration-dependent.

The panel achieved strong consensus on the study's core methodological strengths: the leave-one-site-season-out cross-validation design is rigorous and appropriate for testing transferability (CONSENSUS-5 point 2); the explicit comparison against phenological baselines under the same validation scheme is novel and important (CONSENSUS-5 point 3); and the transparent reporting of limitations is commendable (CONSENSUS-5 point 4). From a crop physiology perspective (R3), the finding that thermal time dominates is both physiologically expected and empirically valuable.

However, three **blocking issues** prevent acceptance at this stage and require Major Revision:

1. **Predictor screening (B1)**: All five reviewers identified the TODO comment at line 218 as a critical unresolved issue. The process for screening 53 VIs to 43 candidate predictors is not documented, and it is unknown whether AGB or test-fold data was used. If so, this constitutes data leakage that inflates performance estimates and could reverse the conclusion that satellite data adds minimal value. This issue is mandatory to resolve before publication (CONSENSUS-5 point 1).

2. **SAR preprocessing (B2)**: Three reviewers (EIC, R2, DA) identified the omission of speckle filtering and incidence angle normalization for Sentinel-1 as a critical limitation. Since S1 alone was the weakest predictor set (RMSE 10.92-12.19 t/ha) and the paper concludes satellite data adds minimal value, readers will question whether this conclusion generalizes to properly preprocessed SAR. The authors must either (a) conduct a sensitivity analysis testing these preprocessing steps, (b) explicitly justify the minimal-preprocessing choice as a deliberate design to test freely available data, or (c) prominently acknowledge in the abstract and conclusion that findings apply specifically to minimally preprocessed SAR data (CONSENSUS-3 point 7).

3. **Linear vs. nonlinear model comparison (B3)**: The GDD baseline uses ordinary least squares while satellite sets use RF and XGBoost, conflating algorithmic and predictor contributions. Without testing RF/XGBoost on GDD alone, the reported satellite contribution may include an algorithmic effect. The authors should either test nonlinear models on GDD alone or discuss this confound as a limitation.

Beyond these blocking issues, the manuscript requires substantial discussion revision to address operational context (CONSENSUS-3 point 8; EIC W2, R3 question, DA M6), spatial CV benchmarks for crop AGB (R2 W3), sensor-specific transferability patterns (EIC W4), and the within-season sampling design's implications (Disagreement 1 resolution; DA C2, R3 W1).

The decision between Minor and Major Revision was considered carefully (Disagreement 2). While R3's recommendation of Minor Revision reflects the soundness of the crop physiology aspects, the three blocking issues require substantive methodological clarification or additional analysis that exceeds Minor Revision scope. The predictor screening issue (B1) must be definitively resolved, which may require re-running analyses if leakage is found. The SAR preprocessing issue (B2) requires either sensitivity analysis or substantial framing changes. These are not minor editorial refinements.

**With these revisions, the paper would make a strong contribution to *Computers and Electronics in Agriculture* by providing a rigorous methodological template for satellite validation studies and an important empirical finding about the dominance of thermal time for wheat AGB estimation under spatial cross-validation.**

---

## Required Revisions (Must Fix)

| Transport ref | Revision Item | Sub-Claim(s) | Severity | Evidence Anchor | Confidence | Source Reviewer | Obligation class | Cost scope | Bounded consequence |
|---|--------------|--------------|----------|-----------------|------------|----------------|------------------|------------|---------------------|
| R1 | Document predictor screening process and confirm no data leakage | — | Critical | `text: line 218 TODO` | 5 | EIC, R1, DA | must_fix | re-analysis if leakage found, or documentation (Methods §2.3.2) | Leakage invalidates all performance estimates |
| R2 | Justify SAR preprocessing omissions or test their impact | — | Critical | `text: §4.4 SAR preprocessing` | 5 | EIC, R2, DA | must_fix | sensitivity analysis or discussion (Abstract, Methods, Conclusion) | Findings may not generalize to optimized SAR |
| R3 | Test nonlinear models on GDD baseline or discuss confound | — | Major | `text: §2.3.4 model choices` | 5 | R1, DA | must_fix | add 2 configurations (RF GDD, XGB GDD) or discussion (Limitations) | Satellite contribution may include algorithmic effect |
| R4 | Add operational decision context with cited thresholds | — | Major | `text: §4.3 "too coarse"` | 5 | EIC, DA | must_fix | discussion (§4.3, 2-3 sentences) | Operational relevance is unclear |
| R5 | Discuss fold-specific correlation filter impact on comparisons | — | Major | `text: §2.3.3 filter variation` | 5 | R1 | must_fix | discussion or supplementary table (which predictors removed per fold) | Cross-fold comparison interpretability |
| R6 | Add spatial CV crop AGB literature benchmarks | — | Major | `absence: Introduction, Discussion` | 5 | R2 | must_fix | literature review (Introduction, Discussion, 2-3 citations) | Performance is not contextualized |
| R7 | Discuss sensor-specific per-fold transferability patterns | — | Major | `table: Table 5, Suppl. Table S2` | 5 | EIC | must_fix | discussion (Results or Discussion, 1-2 paragraphs) | Operational guidance on when sensors fail |

### Required Item Details

**R1: Document predictor screening process and confirm absence of data leakage**
- **Problem**: The TODO comment at line 218 indicates that the process for screening 53 VIs to 43 candidate predictors is not documented. It is unknown whether AGB or any test-fold data was used in this screening. If so, this constitutes data leakage: the held-out site-seasons would have influenced which predictors are included, inflating performance estimates and potentially reversing the conclusion that satellite predictors add minimal value.
- **Source**: Identified by all five reviewers. EIC W1 (Critical), R1 W1 (Critical), R2 acknowledged methodological rigor concerns, R3 accepted methodology as sound, DA C1 (Critical).
- **Requirement**: The authors must provide a complete description of how the 43 predictors were selected from the 53 VIs. If the screening was based on AGB correlation, R² with AGB, or any statistic computed on the full dataset, the entire analysis must be repeated with predictor selection performed within each training fold only (i.e., predictor screening becomes part of the within-fold preprocessing pipeline). Alternatively, if predictor screening was AGB-independent (e.g., based solely on literature recommendations, sensor band availability, or multicollinearity among VIs), this must be explicitly stated in Methods §2.3.2 with the screening criteria documented. A third option is to use all 53 VIs without screening, though this increases overfitting risk with 153 samples.
- **Acceptance criteria**: Methods §2.3.2 contains a clear statement of the screening process. If AGB-dependent screening was used, Supplementary Material includes per-fold screening results and revised Table 5 with corrected performance estimates. If AGB-independent, the screening criteria are documented and justified. The TODO comment is removed.

**R2: Justify SAR preprocessing omissions or test their impact**
- **Problem**: The manuscript acknowledges that Sentinel-1 data were used "without speckle filtering or incidence-angle normalization," which "may have reduced the contribution of SAR data" (Discussion §4.4). These are standard preprocessing steps for agricultural SAR applications. Since S1 alone was the weakest predictor set (RMSE 10.92-12.19 t/ha) and the paper concludes that satellite data adds minimal value, readers will question whether this conclusion generalizes to properly preprocessed SAR.
- **Source**: EIC W3 (Major), R2 W1 (Critical), DA C3 (Critical).
- **Requirement**: The authors must choose one of three options: **(1) Sensitivity analysis**: Apply multi-look speckle filtering (e.g., 5×5 Lee filter) and incidence angle normalization to S1 data for at least one site-season, re-extract predictors, and test whether the S1 and S1+GDD configurations improve. Report results in Supplementary Material. If performance improves, revise the conclusion to state that minimally preprocessed S1 adds minimal value but optimized S1 may be more useful. **(2) Justify the preprocessing choice**: Explicitly state that the goal was to test freely available, minimally processed data accessible to farmers or researchers without specialized SAR expertise. Add this justification to the Abstract ("we evaluated minimally preprocessed satellite data"), Introduction (research objective), Methods (SAR preprocessing subsection), and Conclusion. **(3) Acknowledge the limitation prominently**: Move the speckle filtering caveat from Limitations to the Abstract, Results (when first reporting S1 performance), and Conclusion, stating clearly that "these findings apply to minimally preprocessed SAR data; optimized SAR preprocessing may yield different results."
- **Acceptance criteria**: One of the three options is implemented. If option (1), Supplementary Material includes sensitivity analysis results. If option (2) or (3), Abstract and Conclusion contain explicit framing that findings apply to minimally preprocessed SAR.

**R3: Test nonlinear models on GDD baseline or discuss the confound**
- **Problem**: The GDD baseline uses ordinary least squares (LM) while satellite predictor sets use random forest and XGBoost. This conflates two sources of improvement: (1) the value of satellite predictors, and (2) the value of nonlinear modeling. The comparison between linear GDD (R²=0.53, RMSE=6.24 t/ha) and RF S1+GDD (R²=0.58, RMSE=5.80 t/ha) attributes the 0.05 R² gain to Sentinel-1, but if RF on GDD alone reaches R²=0.56, then S1 contributes only 0.02 R² points.
- **Source**: R1 W5 (Major), DA M1 (Major).
- **Requirement**: The authors should choose one of two options: **(1) Test nonlinear models on GDD baseline**: Add two configurations to Table 5: RF with GDD only, and XGBoost with GDD only. Compare their performance to the linear GDD baseline and to the S1+GDD models. If RF/XGBoost on GDD alone substantially improves on the linear baseline, discuss this in Results and adjust the interpretation of the satellite contribution. **(2) Discuss the confound as a limitation**: Add 2-3 sentences to Discussion §4.4 Limitations: "The baseline uses a linear model while satellite models use nonlinear algorithms, so the reported satellite contribution may include an algorithmic effect. Testing nonlinear models on GDD alone would isolate the satellite contribution from the algorithmic contribution, but this was not feasible in the current study."
- **Acceptance criteria**: Table 5 includes RF GDD and XGBoost GDD configurations with updated interpretation in Results/Discussion, OR Limitations section contains explicit acknowledgment of the linear-vs-nonlinear confound.

**R4: Add operational decision context with cited agronomic thresholds**
- **Problem**: The paper states that 5.80 t/ha RMSE "corresponds to about 47% of the mean observed AGB, which is too coarse for field-level decisions such as variable-rate fertilization or insurance assessment" (Discussion §4.3). However, this blanket statement lacks agronomic context: what error level would be acceptable for these applications? Are there citations or guidelines for decision thresholds? Is 47% error typical for current operational systems?
- **Source**: EIC W2 (Major), R3 implicit question in review, DA M6 (Major).
- **Requirement**: Add 2-3 sentences to Discussion §4.3 contextualizing the 5.80 t/ha RMSE: (1) Compare with error levels of current operational biomass/yield monitoring systems if literature is available. (2) Cite or estimate decision thresholds for specific applications (e.g., "variable-rate nitrogen application requires AGB estimates within ±X t/ha to optimize fertilizer use," citing agronomic literature). (3) Identify any intermediate-precision use cases where the model could be operationally valuable (e.g., identifying severely stressed vs. healthy fields at the farm or regional scale, irrigation scheduling).
- **Acceptance criteria**: Discussion §4.3 contains 2-3 additional sentences with at least one citation providing operational context for the 5.80 t/ha RMSE.

**R5: Discuss fold-specific correlation filter impact on cross-fold comparisons**
- **Problem**: The within-fold correlation filter (|r| ≥ 0.9) removes different predictors in different folds. In the La Cancha fold, ∑GDD was removed due to correlation with cumulative indices, while in other folds it was retained. This means the "All" set does not contain the same predictors across folds, introducing a confound: performance differences could be due to which predictors passed the filter, not due to site characteristics. If the most important predictor (∑GDD) is removed in one fold, that fold will perform worse regardless of site characteristics.
- **Source**: R1 W2 (Major), DA M3 (Major).
- **Requirement**: (1) Report which predictors were removed by the correlation filter in each fold for the "All" set and the major predictor sets (S2, PS). This could be a supplementary table. (2) Discuss whether the folds where ∑GDD was removed (La Cancha fold) had worse performance, and whether this could be due to removing the most important predictor rather than site characteristics. (3) Optionally, conduct a sensitivity analysis: re-run the "All" set with a pre-specified correlation filter applied to all 153 samples (accepting the minor leakage of correlation structure) to check whether the rank order of folds changes. If the rank order is stable, this increases confidence that fold-specific predictor sets are not driving the results.
- **Acceptance criteria**: Discussion or Supplementary Material contains information about which predictors were removed per fold and discusses whether this affected cross-fold comparison. Supplementary table listing retained predictors per fold for "All" set is recommended.

**R6: Add spatial cross-validation crop AGB literature benchmarks**
- **Problem**: The paper compares its R²=0.58 to UAV/hyperspectral studies (R²=0.81-0.91) and correctly attributes the difference to validation scheme. However, it does not cite other studies that have used spatial or site-level cross-validation for crop AGB with satellite data. Without these benchmarks, readers cannot assess whether R²=0.53-0.58 is typical, better than expected, or worse than expected for satellite-based AGB estimation under rigorous validation.
- **Source**: R2 W3 (Major), DA M5 (Major).
- **Requirement**: Add 2-3 citations to the Introduction and Discussion of recent crop AGB studies that used spatial, site-level, or grouped cross-validation. Include their reported R² and RMSE values. Compare the current R²=0.53-0.58 to those benchmarks. For example: "Our best configuration (R²=0.58) is comparable to [Study X] (R²=0.61 for maize with Sentinel-2 under site-level CV) but lower than [Study Y] (R²=0.72 for soybean with combined optical-SAR under spatial CV), suggesting..." If such studies are rare, state this explicitly: "To our knowledge, few wheat AGB studies have reported performance under site-level cross-validation, making it difficult to benchmark our results against the literature."
- **Acceptance criteria**: Introduction and/or Discussion contains 2-3 citations to spatial CV crop AGB studies with performance benchmarks, or explicit statement that such studies are rare.

**R7: Discuss sensor-specific per-fold transferability patterns**
- **Problem**: Table 5 and Figure 4 show that different predictor sets have markedly different per-fold transferability (e.g., PlanetScope SD of RMSE > 4 t/ha, Sentinel-1+GDD contribution sign depends on algorithm), but the text does not explore which site-seasons each sensor failed on or why. For operational users, understanding when and why satellite data fails to transfer is as important as knowing the average performance.
- **Source**: EIC W4 (Major).
- **Requirement**: Add 1-2 paragraphs to the Discussion analyzing per-fold performance patterns. Use Supplementary Table S2 (per-fold results) to identify which site-seasons were outliers for which sensor. Propose hypotheses tied to field conditions (e.g., did Sentinel-1 help more in drier site-seasons? did PlanetScope's high variance occur in the seasons with sensor transitions? did the sequential ablation show Sentinel-1's contribution is positive with RF but negative with XGBoost?). If no clear pattern emerges, state this explicitly as a finding: "sensor transferability was not systematically related to observable field characteristics, suggesting..."
- **Acceptance criteria**: Discussion contains 1-2 paragraphs analyzing per-fold sensor performance patterns with hypotheses tied to field conditions or sensor characteristics.

---

## Suggested Revisions (Should Fix)

| Transport ref | Revision Item | Sub-Claim(s) | Severity | Evidence Anchor | Confidence | Source Reviewer | Obligation class | Cost scope | Bounded consequence |
|---|--------------|--------------|----------|-----------------|------------|----------------|------------------|------------|---------------------|
| S1 | Test PlanetScope sensor transition radiometric impact | — | Major | `text: §2.2.3 Dove/SuperDove` | 4 | R2 | should_fix | sensitivity analysis or discussion (Methods §2.2.3, Discussion) | PS transferability may partly reflect sensor artifacts |
| S2 | Discuss within-season sampling design and its operational implications | — | Minor | `figure: Figure 2` | 5 | R3, DA | should_fix | discussion (Results or Discussion, 2-3 sentences) | Within-season sampling may favor GDD; operational use case is different |
| S3 | Explain agronomic conditions producing high AGB values (>30 t/ha) | — | Minor | `text: Results AGB>30 t/ha` | 5 | R3, DA | consider | study area or discussion (1-2 sentences) | High values lack agronomic context |
| S4 | Integrate crop physiology literature on GDD-biomass relationships and environmental stress | — | Minor | `text: Introduction, Discussion` | 5 | R3 | consider | literature review (Introduction or Discussion, 2-3 citations) | Physiological context would improve interpretability |
| S5 | Justify vegetation index selection criteria | — | Minor | `text: §2.2.5 VI selection` | 4 | R2 | consider | methods (§2.2.5, 1-2 sentences) | VI selection transparency |
| S6 | Report k value for k-NN imputation | — | Minor | `text: §2.3.3 imputation` | 5 | R1 | consider | methods (§2.3.3, 1 number) | Reproducibility |
| S7 | Clarify which predictor of each correlated pair is removed by correlation filter | — | Minor | `text: §2.3.3 correlation filter` | 5 | R1 | consider | methods (§2.3.3, 1 sentence) | Reproducibility |

---

## Revision Roadmap

### Source-traceability checklist

- [ ] R1 — obligation `must_fix`: Document predictor screening process (line 218 TODO) and confirm no data leakage. If AGB-dependent, re-run analysis with within-fold screening. If AGB-independent, document criteria.
- [ ] R2 — obligation `must_fix`: Justify SAR preprocessing omissions (no speckle filtering, no incidence angle normalization) OR test their impact via sensitivity analysis OR acknowledge prominently in Abstract/Conclusion that findings apply to minimally preprocessed SAR.
- [ ] R3 — obligation `must_fix`: Test RF/XGBoost on GDD baseline alone to isolate satellite contribution from algorithmic contribution, OR discuss this confound as a limitation.
- [ ] R4 — obligation `must_fix`: Add operational decision context to Discussion §4.3 with cited agronomic thresholds for variable-rate fertilization, insurance, or other applications. Contextualize 5.80 t/ha RMSE.
- [ ] R5 — obligation `must_fix`: Discuss or report (Supplementary table) which predictors were removed by correlation filter in each fold, especially La Cancha fold where ∑GDD was removed. Discuss whether this affected cross-fold comparison.
- [ ] R6 — obligation `must_fix`: Add 2-3 citations to spatial CV crop AGB studies with performance benchmarks to Introduction/Discussion. Compare R²=0.53-0.58 to those benchmarks. If rare, state explicitly.
- [ ] R7 — obligation `must_fix`: Add 1-2 paragraphs to Discussion analyzing per-fold sensor performance patterns. Which site-seasons did which sensors fail on? Propose hypotheses tied to field conditions or sensor characteristics.
- [ ] S1 — obligation `should_fix`: Test PlanetScope sensor transition (Dove vs. SuperDove) radiometric impact on the four common bands, or discuss as a potential confound for PS high variance.
- [ ] S2 — obligation `should_fix`: Discuss within-season sampling design (5-6 phenological stages per site-season) and its implications. Operational use case often requires field comparison at a fixed growth stage, where GDD has minimal variance.
- [ ] S3 — obligation `consider`: Explain agronomic conditions (irrigation, nitrogen, cultivar, plant density) that produced AGB values >30 t/ha in La Cancha and Hidango 2021-2022.
- [ ] S4 — obligation `consider`: Integrate crop physiology literature on GDD-biomass relationships and environmental stress modifiers. Discuss whether low stress variability in the site-seasons could explain satellite data's modest contribution.
- [ ] S5 — obligation `consider`: State the criteria for selecting 53 VIs from the cited papers. Were all indices included, or only a subset? If subset, what was the rationale?
- [ ] S6 — obligation `consider`: Report the k value used for k-nearest neighbors imputation of missing soil moisture values (Methods §2.3.3).
- [ ] S7 — obligation `consider`: Specify which predictor of each correlated pair (|r| ≥ 0.9) is removed by the correlation filter (Methods §2.3.3). Is it the one with lower variance, lower correlation to AGB, or random?

---

## Journal-Supplied Deadline (Optional Transport)

- **Exact deadline from source letter**: NOT PROVIDED

---

## Response Letter Instructions

Please use the format in `templates/revision_response_template.md` to respond to every reviewer comment item by item.

**Must include**:
1. Response and revision description for each Required Revision (R1-R7)
2. Response for each Suggested Revision (S1-S7) — adopted or reason for not adopting
3. Change markup (mark all changes in the revised manuscript with track changes or color highlighting)
4. Cross-reference table mapping old page/line numbers to revised manuscript locations

---

## Closing

We appreciate your submission to *Computers and Electronics in Agriculture* and the effort you have invested in this rigorous cross-validation study. The review panel unanimously recognizes the value of your methodological contribution, particularly the explicit comparison against phenological baselines under leave-one-site-season-out cross-validation, which is rarely done in the remote sensing literature. The finding that accumulated growing degree days carries most of the transferable information about wheat AGB is both physiologically plausible and operationally relevant.

However, three critical methodological issues currently prevent acceptance and require Major Revision:

1. The unresolved predictor screening process (line 218 TODO) must be clarified to rule out data leakage.
2. The omission of standard SAR preprocessing (speckle filtering, incidence angle normalization) must be justified, tested, or prominently acknowledged as limiting generalizability.
3. The comparison between linear baselines and nonlinear satellite models must be addressed to isolate satellite contributions from algorithmic effects.

Additionally, the manuscript requires substantial discussion revision to contextualize operational decision thresholds, benchmark performance against spatial CV crop AGB literature, analyze sensor-specific per-fold transferability patterns, and clarify the implications of the within-season sampling design.

With these revisions, we believe your manuscript will make a strong contribution to the precision agriculture and agricultural remote sensing communities by providing both a rigorous methodological template for satellite validation studies and an important empirical finding about the dominance of thermal time under spatial cross-validation.

We look forward to receiving your revised manuscript and detailed response letter addressing each of the Required and Suggested Revisions.

---

**Editorial Decision: Major Revision**

Sincerely,

Editorial Team  
*Computers and Electronics in Agriculture*  
Academic Research Skills Review Panel v1.11.1
