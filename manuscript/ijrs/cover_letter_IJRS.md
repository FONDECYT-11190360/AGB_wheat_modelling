Francisco Zambrano
Facultad de Medicina Veterinaria y Agronomía
Universidad de Las Américas, Santiago, Chile
frzambra@gmail.com

[Date]

Editor-in-Chief
International Journal of Remote Sensing

Dear Editor,

We submit the manuscript "Thermal time baselines for evaluating Sentinel-1, Sentinel-2 and PlanetScope predictors of wheat above-ground biomass under spatial cross-validation" for consideration as a research article in the International Journal of Remote Sensing.

Satellite imagery combined with machine learning is now a common way to estimate crop above-ground biomass (AGB), and published models often report R² above 0.8. Two features of how these models are evaluated make such values hard to interpret. Most studies use random splits or splits within fields, which overestimate accuracy at new fields. Few studies compare their satellite models with a phenological baseline, although AGB increases through most of the season and any predictor that tracks season progress, including cumulative vegetation indices, correlates strongly with it. Our study measures how much Sentinel-1 backscatter, Sentinel-2 and PlanetScope reflectance and vegetation indices, precipitation and soil moisture add to accumulated growing degree days (∑GDD) when wheat AGB is estimated at fields not used for training.

The study uses 153 destructive AGB samples collected from sowing to ripening in four wheat fields in central Chile between 2020 and 2023, with daily time series built from 185 Sentinel-1, 208 Sentinel-2 and 180 PlanetScope images. We evaluated 14 predictor sets with linear regression, random forest (RF) and XGBoost, holding out one field and season at a time with fixed hyperparameters, and interpreted the models with permutation importance on held-out data and accumulated local effects.

The main findings are:

1. A linear model on ∑GDD alone reached R² = 0.52 (RMSE = 6.26 t ha⁻¹) in held-out fields.
2. Every model built on a single satellite sensor transferred worse than this baseline. Sentinel-1 alone was the weakest set.
3. RF with Sentinel-2, Sentinel-1 and weather predictors gave the lowest mean error (R² = 0.58, RMSE = 6.07 t ha⁻¹) and improved on the baseline in three of four fields, a gain of about 5% that was smaller than the variability among fields. Sentinel-2 was the only sensor that reduced error consistently when added to weather predictors.
4. In RF with all 54 predictors (RMSE = 6.11 t ha⁻¹), ∑GDD was the most important predictor in every fold, and the leading optical predictors were cumulative indices whose effects followed season progress.

We believe the manuscript suits the journal's readership because its main contribution concerns how remote sensing models of crop biomass are evaluated. It shows, with real multisensor data, that a large part of the apparent skill of satellite AGB models can come from the seasonal trend, and it proposes a simple protocol: validate by holding out whole fields or seasons, report a phenological baseline under the same scheme, and report metrics for each held-out unit. The Discussion also examines why C-band backscatter added information only in combination with thermal time, in terms of wheat scattering behaviour and Sentinel-1 preprocessing. We state the main limitation openly: four field seasons give limited power to rank the leading configurations.

A preprint of an earlier version of this manuscript is available on EarthArXiv [preprint DOI]. A related letter based on the same AGB dataset [is under review / was published] at IEEE Geoscience and Remote Sensing Letters ("Multi-Sensor Fusion for Wheat Above-Ground Biomass Estimation: Sensor Contribution Analysis Under Leave-One-Site-Out Spatial Validation"). That letter reports a sequential sensor ablation with three algorithms, an analysis of the interaction between GDD and PlanetScope EVI, an assessment of differences between sites and an uncertainty estimate. The present manuscript addresses a different question, the value of satellite predictors relative to thermal time, and adds calendar and thermal time baselines, single sensor sets, sensor sets combined with thermal time, group permutation importance and accumulated local effects. It also changes the preprocessing so that the correlation filter is estimated within each training fold and PlanetScope red edge predictors missing for two fields are excluded. Both works conclude that no sensor provides a robust improvement. We will gladly provide the related manuscript on request.

This manuscript is not under consideration elsewhere, and all authors have approved its submission. The authors report no competing interests. The use of generative AI tools is declared in the manuscript. The data and code are openly available at https://github.com/FONDECYT-11190360/AGB_wheat_modelling.

Thank you for considering our work.

Sincerely,

Francisco Zambrano, on behalf of all authors
