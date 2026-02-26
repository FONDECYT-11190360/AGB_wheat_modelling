# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

R project for Above-Ground Biomass (AGB) modeling and prediction of wheat crops in Chile. It combines multi-source satellite imagery (Sentinel-1 SAR, Sentinel-2 optical, PlanetScope) with climate and soil moisture data to estimate current biomass and predict future biomass (1–4 months ahead) using machine learning.

**Study sites** (referenced throughout as `cod_id`):
- `hidango_2021-2022`, `hidango_2022-2023`
- `la_cancha_2022-2023`
- `villa_baviera_2020-2021`

## Running Scripts

Open `data_wheat.Rproj` in RStudio. All file paths are relative to the project root. Scripts are run interactively — there is no build system or test runner. Run a script from the R console with:

```r
source('script/08_armar_dataset.R')
```

Or open the file in RStudio and use Ctrl+Shift+Enter to run all.

## Pipeline Architecture

Scripts are numbered and must be run in order. The pipeline has three logical phases:

### Phase 1 — Data Acquisition and VI Computation (01–06)
| Script | Purpose |
|--------|---------|
| `01_descargar_imagenes.R` | Download Sentinel-1, Sentinel-2 (via STAC/Planetary Computer), and WorldClim BioMap rasters |
| `02_calcular_vis.R` | Compute spectral/vegetation indices for S2, PlanetScope, and S1 |
| `03_suavizar_vis.R` | Smooth VI time series |
| `04_acumular_vis.R` | Compute cumulative sums of VIs |
| `05_extraer_vis.R` | Extract VI values at sampling point locations |
| `06_descargar_clima.R` | Download daily climate data from AgroMet stations via `agrometR` |

### Phase 2 — Dataset Construction and Biomass Estimation Models (07–14)
| Script | Purpose |
|--------|---------|
| `07_procesar_datos.R` | Process raw tabular data: phenology dates, biomass measurements, climate (GDD/precip cumsum), soil moisture |
| `08_armar_dataset.R` | Join all processed data into `data/processed/rds/dataset.rds` |
| `09_modelar_biomasa.R` | Exploratory RF model for potential predictor |
| `14_modelado_con_ML.R` | Full workflow: XGBoost, LightGBM, RF, SVM, GLMnet, bagMLP, KNN via `tidymodels` + `stacks` ensemble |

Script 14 defines **8 preprocessing recipes** (`rec1`–`rec8`) corresponding to different sensor combinations:
- `rec1`: all predictors; `rec2`: S1 only; `rec3`: S1 + climate; `rec4`: climate only; `rec5`: S2; `rec6`: S2 + climate; `rec7`: PlanetScope; `rec8`: PS + climate

### Phase 3 — Spatial Prediction and Lead-Time Forecasting (10–13, 15–23)
| Scripts | Purpose |
|---------|---------|
| `10–13` | Create base rasters, rasterize meteorological data, assemble predictor raster stacks |
| `15`, `17`, `18` | Apply models to raster stacks → biomass prediction maps |
| `19` | Generate tabular datasets for 1–4 month lead-time prediction |
| `20` | Train lead-time models (same algorithm set, target = `cosecha`) |
| `14.x`, `16`, `20.1`, `21–23` | Variable importance (DALEX/VIP), partial profiles, metrics plots, maps |

## Key Data Files

| Path | Description |
|------|-------------|
| `data/processed/sitios.gpkg` | Site polygons; layers named `a_{cod_id}` |
| `data/processed/rds/dataset.rds` | Final joined dataset for biomass estimation |
| `data/processed/rds/data_indices_prediccion_lead_{n}_mes.rds` | Predictor datasets for n-month lead models |
| `data/processed/modelos/workflow_estimacion.rds` | Full `workflow_set` results from script 14 |
| `data/processed/modelos/modelo_ensamblado.rds` | Stacked ensemble model |
| `data/processed/modelos/{recipe}_{model}.rds` | Individual fitted models (estimation) |
| `data/processed/modelos/prediccion_{recipe}_{model}_lead_{n}.rds` | Lead-time models |
| `data/processed/raster/predictores/{cod_id}/` | Per-date predictor raster stacks |
| `data/processed/raster/predicciones/{cod_id}/` | Per-date AGB prediction rasters |

## Naming Conventions

- **Vegetation index prefixes**: `S2_` (Sentinel-2), `PS_` (PlanetScope), `S1_` (SAR backscatter)
- **Cumulative variables**: suffix `_cumsum` (e.g., `S2_NDVI_cumsum`, `pp_cumsum`, `gdd_cumsum`)
- **Model IDs**: `{recipe}_{ModelType}` (e.g., `rec1_XGBoost`, `rec6_RF`)
- **Output figures**: `output/figs/`

## Key Packages

```r
# Spatial
terra, sf, gdalcubes, rstac, earthdatalogin

# ML modeling
tidymodels, stacks, bonsai,   # LightGBM
baguette,                      # bagged MLP
xgboost, ranger, kernlab, glmnet, kknn

# Interpretability
vip, DALEX

# Data / utilities
tidyverse, glue, agrometR
```

`agrometR` provides Chilean AgroMet station data. `earthdatalogin` requires NASA Earthdata credentials (`edl_netrc()`). Planetary Computer access uses `sign_planetary_computer()` from `rstac`.
