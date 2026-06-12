# Revier3 revision

1. Potential spatial and temporal data leakage

The manuscript describes a standard 75/25 random split for training and testing. In spatiotemporal datasets, random splitting frequently leads to data leakage, as observations from the same field on the same day (or adjacent days/pixels) are highly correlated. If training and testing sets contain data from the same site or the same time series, the model's performance metrics (such as the reported R2 > 0.91) will be artificially inflated and will not accurately reflect the model's ability to generalize to new, unseen fields or seasons. The authors should implement a more robust validation strategy, such as spatial block cross-validation or leave-one-site/season-out cross-validation, to ensure that the test set is truly independent.

2. Unclear sampling and pseudoreplication

The authors state that 100 random samples per site were taken for prediction modeling. However, the methodology section indicates that only 153 actual AGB samples were collected in total across all sites and seasons (e.g., 30 in Villa Baviera). It is unclear how 100 samples per site were generated from this limited pool. If the authors sampled multiple pixels within the same field and assigned the same field-level AGB or interpolated AGB to them, this constitutes pseudoreplication, which artificially inflates the sample size and leads to overly optimistic performance metrics. Please clarify the exact nature of these 100 samples. If they are not independent biological replicates, the analysis must be adjusted to reflect the true number of independent observations, or the limitations of this sampling approach must be explicitly justified.

3. Contradictory recipe definitions across sections

Section 2.3.2 defines the modeling recipes as rec2 for Sentinel-2, rec3 for Sentinel-2 and weather, rec4 for PlanetScope, and rec6 for Sentinel-1. However, Section 3.1.1, Figure 4, and Figure 5 contradict this by defining rec3 as Sentinel-1 and weather, rec4 as only weather, and rec6 as Sentinel-2 and weather.

4. Unrepresentative weather data

Table 1 indicates that the meteorological station for Villa Baviera is located nearly 21 km away from the field. Given that cumulative precipitation and growing degree days (GDD) are identified as top predictors, using weather data from such a distance introduces significant uncertainty, as microclimates and precipitation events can vary drastically over 20 km. The authors should validate the in-situ weather data for this site against a high-resolution gridded meteorological product to ensure it accurately reflects the conditions at the field level, or perform a sensitivity analysis excluding this site to confirm that the variable importance hierarchy remains stable.

5. Confounding of wheat variety and site

The study design perfectly confounds wheat variety (spring vs. winter) with geographic location, as Villa Baviera is the only site utilizing spring wheat. Consequently, it is impossible to determine whether the model's performance and the identified variable importances at this site are driven by the physiological differences between spring and winter wheat, or by site-specific environmental factors. The authors should explicitly address this confounding variable in the discussion and clarify whether the model is learning generalized wheat phenology or merely site-specific offsets.

6. Contradiction in scalability claims

The authors claim that the reliance on public data makes the application highly scalable. However, the highest performing models heavily rely on in-situ soil moisture measurements (TEROS 12 sensors) and commercial PlanetScope imagery, neither of which are universally available public datasets. While the authors acknowledge the limitation of in-situ data in the discussion, the assertion of "easy expansion" overstates the current framework's operational readiness. The manuscript should be revised to align the claims of scalability with the actual inputs required to achieve the reported accuracy, perhaps by explicitly detailing the expected performance drop if only globally available public data were used.

7. Text contradicts Table 5 on variable ranks

Section 3.2 claims that the Sentinel-1 variables VV, VH/VV, and VH all "ranked second across all four lead times". However, Table 5 shows that only S1_VH has a mode rank of 2, while S1_VV and S1_VH/VV both have a mode rank of 3.

8. Incomplete hyperparameter tuning details

While Table 4 lists the hyperparameters tuned for each algorithm, the manuscript does not report the actual search spaces (e.g., the minimum and maximum bounds, or the specific grid/distribution of the ten candidates) used for the optimization. Without the specific ranges for critical parameters like tree_depth, learn_rate, or cost, the modeling pipeline cannot be fully reproduced. Please provide the exact search spaces and distributions used for all tuned hyperparameters.

9. Missing spatial extraction methodology

The manuscript integrates remote sensing data with varying spatial resolutions (e.g., 3m for PlanetScope, 10m for Sentinel-1, and 10-60m for Sentinel-2). However, the exact method used to extract these pixel values at the sampling locations is not described. It is unclear whether the authors used a single nearest-neighbor pixel, a bilinear interpolation, or a spatial buffer average around the 0.25 m² sampling plots. Because the sampling area is much smaller than the resolution of Sentinel-1 and Sentinel-2, the choice of extraction method can significantly impact the signal-to-noise ratio and the resulting model performance. Please specify the spatial extraction technique used to ensure reproducibility.

10. Text references wrong figure for daily AGB

Section 3.1.4 directs the reader to Figure 9 to see the "averaged daily AGB for each site" along with "in-situ AGB measurements." However, Figure 9 displays monthly spatial maps of estimated AGB, while the daily temporal variation and in-situ measurements are actually shown in Figure 8.


11. Text reports different RMSE/MAE than Figure 5

Section 3.1.2 states that the ensemble model has an RMSE of 3.65 t/ha and the RF model has an RMSE of 3.18 t/ha. However, Figure 5 shows the ensemble model has an RMSE of 4.35 t/ha (and an MAE of 3.65 t/ha, indicating the text confused the two metrics) and the RF model has an RMSE of 3.21 t/ha.

12. Text contradicts figures on peak AGB value

Section 3.1.4 states that the AGB for Hidango season 2022-2023 reached 22.50 t/ha. However, both Figure 2 and Figure 8 show that the maximum AGB for Hidango 2022-2023 (bright red line/points) peaked at approximately 16 t/ha.

13. Figure 6 caption contradicts plotted variables

The caption for Figure 6 states that the plot shows "The six most important variables", but the figure itself displays a bar chart with 20 different variables on the y-axis.

14. Contradictory cross-validation repetitions

Section 2.3.6 describes the cross-validation procedure by stating the model performance is evaluated "ten times", but the very next sentence explicitly states "we define no repetitions per fold", which directly contradicts the claim of evaluating it ten times.