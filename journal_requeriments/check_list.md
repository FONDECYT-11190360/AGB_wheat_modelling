
# Checklist for Authors

To help authors prepare high-quality, impactful submissions, this checklist outlines key expectations for papers submitted to IEEE Geoscience and Remote Sensing Letters (GRSL). It reflects the journal’s standards for scientific rigor, clarity, reproducibility, and relevance to the geoscience and remote sensing community.

The goal is to ensure that each submission not only presents a novel and well-motivated contribution, but also demonstrates strong experimental design, appropriate evaluation practices, and clear, concise communication. By following this checklist, authors can better align their work with the journal’s editorial priorities and increase the likelihood of a successful review process.

Authors are strongly encouraged to consult this checklist before submission to verify that all core elements – technical content, evaluation methodology, writing quality, formatting, and metadata – are complete and in line with GRSL’s expectations.

## Contribution

- Contributions must be novel, unpublished, and within the scope of Geoscience and Remote Sensing (see more information about the scope of GRSL here).
- Submissions must offer clearly motivated contributions – such as methodological innovations, conceptual insights, or substantive findings – that advance understanding. Merely better empirical results without explanation or justification are insufficient.
- Contributions must go beyond incremental improvements or the straightforward application of existing methods to new data without adaptation.

## Evaluation

- Experiments should be based on publicly available datasets whenever possible.
- Use modern, publicly available datasets that reflect the current state of the art.
- Datasets yielding near-perfect performance (e.g., >90% accuracy in semantic tasks) are likely unsuitable for a comparative analysis of different methods. Avoid using outdated or overly simple datasets such as MSTAR (SAR object recognition), Flevoland (PolSAR segmentation), and Indian Pines (HSI segmentation).
- Dataset descriptions can be brief (and leverage references to other sources) but must be complete, including study area, acquisition parameters, source, and preprocessing steps.
- Include comparisons to reasonable baselines and available state-of-the-art methods.
- Provide both quantitative and qualitative assessments of results.
- If feasible, report mean and standard deviation over multiple runs.
- Report the number of learnable parameters and computational cost during training and inference, when appropriate.
- Hyperparameter settings must be justified – explain how they were chosen.
- While additionally leveraging simulated data is acceptable, experiments must include real measurements. Only using synthetic or simulated data is insufficient unless use of real data is truly not possible.
- Papers proposing new methods to synthesize or enhance data should illustrate the realism of the produced data by showing improvements in downstream tasks.
- General claims require general evidence – not just a single case study.
- Negative or inconclusive results are acceptable if supported by strong experimental design and analysis.

## Pansharpening

- Assess performance at both reduced and full resolution.
- Use standard quality metrics, such as ERGAS, SAM, and Q2 (at reduced resolution).
- Include CS and MRA results as reference methods.

## HSI Classification

- Experiments using only datasets such as Indian Pines, Salinas, Kennedy Space Center, Pavia University/Center, or Botswana are not acceptable. These small, single-image datasets were acquired for other purposes using old-generation AVIRIS sensors, which exhibit characteristics (e.g., dominant thermal noise, lack of signal-dependent noise) incompatible with modern sensors.

## HSI Anomaly detection

- Avoid datasets with visually obvious anomalies (e.g., anomalies visible in RGB), as they do not reflect realistic detection challenges.
- Input images should be large enough (e.g., 400×400 pixels or larger) to ensure reliable estimation of false alarm probability.

## SAR Classification

- Experiments using only datasets such as Flevoland, San Francisco, and Oberpfaffenhofen are not recommended. These small, single-image datasets were acquired for other purposes using old-generation sensors.

## Core Content & Structure

- The related work, methodology, and experiments must be described completely, without omitting relevant details.
- All critical content – including methodology, experiments, and results – must fit within the 5-page limit. If not, the work may be better suited for a different journal.

### Abstract

    The abstract should concisely summarize the motivation, contribution, type of data, and main findings, including quantitative results.

### Introduction

- The research gap, contribution, and justification of novelty must be explicitly stated.
- The introduction should be concise and free of methodological or result details.

### Related work

- The related work section must be well-structured and should not simply list papers. It should discuss a relevant selection of works, including their strengths, limitations, and relation to the proposed approach.
- References should be recent when appropriate – especially in rapidly evolving areas with frequent publications.
- When discussing the state of the art, cite top-performing or recent peer-reviewed works published in top-tier venues.
- Avoid referencing papers without critical comparison or contextual discussion.

### Method

- The methodological description must be clear and complete.
- Focus on your own contributions; avoid repeating published work unless essential for understanding.

### Experiments

- Methodological developments should not be presented in the results section.
- Results must be clearly presented and discussed.
- Mathematical definitions of standard performance metrics do not need to be explicitly provided.
- A discussion of limitations and open questions is strongly recommended.

### Conclusion

- The conclusion should go beyond the abstract – do not repeat it or merely summarize the methodology.
- Present the main findings and offer a concrete outlook for future work.
- Avoid vague phrases like “more work is needed” without specific directions.
- Clearly state the limitations of your work and potential trade-offs or assumptions that might affect generalization.

## Formatting & Submission

- The paper must follow the journal’s formatting template.
- It must not exceed 5 pages, including references and (optional) author biographies.
- Supplemental material is optional and should be limited to 2–3 pages, containing only non-essential content. If supplemental material is included, it must be explicitly referenced in the main text (e.g., “see Supplementary Figure 1”).

## Language

- The paper must use correct spelling, grammar, sentence structure, and technical terminology.
- Use articles (“the”, “a”, etc.) appropriately.
- Use transitional words (e.g., However, Additionally, Consequently) sparingly and only when necessary.
- Prefer clear, concise sentences, and minimize use of passive voice and past tense, unless justified.

## Citations

- Cite references correctly, e.g., “Author1 et al. (Year) proposes … [1]” rather than “[1] proposes…” or “As proposed in [1]…”.
- Only cite works that appear in the reference list, and ensure all cited works are included.
- References must be complete (including all bibliographic details) and follow the required format. Providing DOIs is highly recommended.
- All referenced papers must be publicly accessible.
- Peer-reviewed sources are preferred. If gray literature (e.g., an arXiv preprint) is cited, include a DOI or working URL.
- Whenever possible, cite the original source rather than a secondary reference.
- Self-citations should be used only when clearly necessary.

## Equations

- Equations must be integrated into the text flow, as part of complete sentences.
- End equations with a comma or period, depending on the sentence.
- Number all equations consistently.
- Equations must be visually clear – do not include them as images.
- Ensure equations are mathematically correct, with all variables and operators clearly defined.
- Use consistent symbols throughout the paper.

## Figures

- Figures must be appropriately sized and easily readable, even when printed. Thus, once placed in the paper, figure text should not be significantly smaller than text in the body of the paper.
- Prefer vector graphics over raster images for plots and diagrams.
- If using raster images, ensure sufficient resolution and no compression artifacts.
- Figure captions must be descriptive and understandable without reading the main text.
- Include color legends and scales where appropriate.
- Use color palettes accessible to colorblind readers.
- All figures must be referenced and discussed in the text.

## Resubmissions

- Resubmissions must include a response letter that addresses all editorial comments (editor-in-chief, associate editor, reviewers) point by point.
- Provide both a clean version and a marked version highlighting all changes.
- Address comments not only in the response letter but also by making corresponding changes in the paper.

# Metadata

## Title

- The paper title should be concise, specific, and reflect the main contribution. 
- Avoid overly generic or overly long titles.
- Avoid acronyms that are not widely known.

## Author list

- All authors must have made meaningful intellectual contributions. The author list must reflect the actual contributors.
- Ensure the author list is identical in the submission system and the submitted PDF.
- Adding authors after the review process has started – or even after acceptance – is generally not permitted, especially for authors from different institutions, unless strongly justified and approved by the editorial board.

## Keywords

- Keywords must be informative and specific. Avoid overly generic terms (e.g., deep learning, remote sensing) unless they are central to the paper.

## Source code

- Whenever possible, authors should make the source code publicly available to support reproducibility.
- Code, data splits, and experiment settings should be described in sufficient detail to allow reproduction of results.
- Random seeds, software versions, and environment details (e.g., PyTorch version, hardware) should be provided when relevant.

