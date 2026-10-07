Francisco Zambrano
Facultad de Medicina Veterinaria y Agronomía
Universidad de Las Américas, Santiago, Chile
frzambra@gmail.com

[Date]

Editor-in-Chief
European Journal of Agronomy

Dear Editor,

We submit the manuscript "Thermal time dominates satellite predictors for within-season wheat biomass estimation under spatial cross-validation" for consideration as an original research paper in the European Journal of Agronomy.

Satellite imagery combined with machine learning is now a common route to estimating crop aboveground biomass (AGB). Most published models, however, are evaluated with random splits or splits within fields, and few are compared with a phenological baseline. Because AGB increases through most of the season, any predictor that tracks season progress can correlate strongly with AGB without carrying information about crop condition. Our study asks how much Sentinel-1, Sentinel-2 and PlanetScope predictors, precipitation and soil moisture add to accumulated growing degree days (∑GDD) when wheat AGB is estimated in fields that were not used for training.

The study is based on 153 destructive AGB samples collected from sowing to ripening in four wheat fields at three locations in central Chile (Hidango North, Hidango South, La Cancha and Villa Baviera), over three growing seasons from 2020 to 2023, covering winter and spring wheat. We evaluated 14 predictor sets with linear regression, random forest and XGBoost, holding out one field and season at a time (LOSSO cross validation) with fixed hyperparameters, and we interpreted the models with permutation importance on held out data and accumulated local effects.

The main findings are:

1. A linear model on ∑GDD alone reached R² = 0.53 (RMSE = 6.24 t/ha) in held out fields and outperformed days after sowing (R² = 0.19).
2. Every model based on a single satellite sensor transferred worse than this thermal time baseline.
3. The best configuration, random forest with Sentinel-1 and ∑GDD (R² = 0.58, RMSE = 5.80 t/ha), improved on the baseline in three of four fields, but the gain was not reproduced with XGBoost and was smaller than the variability among fields.
4. RF with all predictors (RMSE = 7.04 t/ha) did not improve on the baseline.

We believe the manuscript fits the journal's scope in agroclimatology and modelling and in precision and digital agriculture. Its contribution is agronomic. It shows that, for tracking biomass during the season, thermal time carries most of the information that transfers between fields, and that apparent satellite skill can largely reflect crop development. It also identifies a sampling design issue: samples that follow biomass from sowing to ripening favor any predictor of season progress, and the added value of satellite data should be tested by comparing fields at the same growth stage. We recommend that AGB studies report a thermal time baseline under validation that holds out whole fields before attributing accuracy to remote sensing. The study meets the journal's requirement for field data from several locations and seasons, and we state its main limitation openly: four field seasons give limited statistical power to rank the leading configurations.

A preprint of an earlier version of this manuscript is available on EarthArXiv [preprint DOI]. In addition, a related letter based on the same AGB dataset is under review at IEEE Geoscience and Remote Sensing Letters ("Multi-Sensor Fusion for Wheat Above-Ground Biomass Estimation: Sensor Contribution Analysis Under Leave-One-Site-Out Spatial Validation"). That letter reports a sequential sensor ablation with three algorithms and additional diagnostics: an analysis of the interaction between GDD and PlanetScope EVI, an assessment of differences between sites, and an uncertainty estimate from the spread among models. The present manuscript addresses a different question, the value of satellite predictors relative to thermal time. To answer it, it introduces calendar time and thermal time baselines, single sensor sets, sensor sets combined with thermal time, permutation importance for predictor groups and accumulated local effects. It also changes the preprocessing: predictors outside each set are removed before the correlation filter within each fold, and the PlanetScope red edge predictors, which were missing for two of the four fields, are excluded. Under these choices, each step of the sequential ablation changes RMSE by less than 8%, much less than the variability among fields, and both works conclude that no sensor provides a robust improvement. We will gladly provide the related manuscript on request.

This manuscript is not under consideration elsewhere, and all authors have approved its submission. The authors declare no competing interests. The use of generative AI tools is declared in the manuscript. The data and code are openly available at https://github.com/FONDECYT-11190360/AGB_wheat_modelling.

Thank you for considering our work.

Sincerely,

Francisco Zambrano, on behalf of all authors
