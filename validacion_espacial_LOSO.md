# Validación Espacial LOSO y Análisis Interpretativo

## Motivación

El pipeline original (script 14) usa un split aleatorio 75/25 (`initial_split(data, seed=987)`), que mezcla observaciones de los 4 sitio-temporadas en entrenamiento y prueba. Esto infla las métricas porque la autocorrelación espacial local no se controla. Este conjunto de scripts implementa una validación más rigurosa para publicación Q1.

---

## Scripts implementados

| Script | Propósito |
|--------|-----------|
| `24_LOSO_ablacion.R` | Validación cruzada Leave-One-Site-Out + estudio de ablación por combinación de sensores |
| `25_error_temporal_DAS.R` | Análisis de residuales en función de los días después de siembra (DAS) |
| `26_pdp_fisiologico.R` | Perfiles de dependencia parcial (PDP) con anotaciones agronómicas |

---

## Script 24 — LOSO + Ablación

### Diseño de folds

- **Unidad LOSO:** `sitio_temporada` (4 grupos: hidango 21-22, hidango 22-23, la_cancha 22-23, villa_baviera 20-21)
- Cada fold deja fuera un sitio-temporada completo → 4 folds
- Implementado con `group_vfold_cv(data, group = "sitio_temporada")`

### Recetas de ablación

| Receta | Sensores incluidos | Propósito |
|--------|--------------------|-----------|
| `baseline` | S2 + Clima | Línea base óptica |
| `radar` | S2 + S1 + Clima | Aporte de SAR |
| `fusion` | S2 + S1 + PS + Clima | Fusión completa |

Los pasos `step_impute_knn`, `step_normalize` y `step_corr` se estiman **únicamente sobre el fold de entrenamiento** (sin filtración al fold de validación).

### Modelos (hiperparámetros fijos)

Los parámetros son valores razonables del dominio, **no extraídos del tuneo de script 14**, para evitar filtración entre folds.

| Modelo | Parámetros principales |
|--------|------------------------|
| Random Forest | `trees=1000, mtry=7, min_n=5` |
| XGBoost | `trees=500, depth=4, lr=0.05, sample=0.8, mtry=7` |
| GLMnet | `penalty=0.01, mixture=0.5` |

### Salidas

| Archivo | Descripción |
|---------|-------------|
| `data/processed/modelos/loso_metrics_per_fold.rds` | 108 filas (9 workflows × 4 folds × 3 métricas) |
| `data/processed/modelos/loso_oof_predictions.rds` | Predicciones out-of-fold con sitio/fecha/temporada |
| `output/tables/ablation_loso_summary.csv` | Tabla resumen: media y SD de cada métrica por receta × modelo |
| `output/figs/loso_ablation_metrics.png` | Dot-plot con barras de error por receta y modelo |
| `output/figs/loso_metricas_por_sitio.png` | Mapa de calor RMSE por sitio-temporada (XGBoost) |

### Indicador clave

Se calcula y reporta automáticamente el **aporte marginal de PlanetScope**:

```
ps_improvement_pct = (RMSE_radar - RMSE_fusion) / RMSE_radar × 100
```

Si < 5%, se emite un mensaje `CRITICAL FINDING` indicando que PS podría ser prescindible.

---

## Script 25 — Error temporal por DAS

### Fuentes de datos

- `loso_oof_predictions.rds` (filtrado a `fusion_XGBoost`)
- `fechas_fenologia.rds` — fechas de siembra y transiciones fenológicas

### Metodología

1. Join con fechas de siembra → `DAS = fecha_obs - fecha_siembra`
2. Asignación de etapa fenológica vigente: join many-to-many con `stage_dates`, filtro `fecha_stage <= fecha`, y `slice_max(fecha_stage)` para quedarse con la transición más reciente
3. Cálculo del DAS medio por transición (líneas de referencia verticales)

### Figura: `residuales_vs_DAS.png`

- Eje X: DAS (días después de siembra)
- Eje Y: residual = predicho − observado (t/ha)
- Puntos coloreados por etapa fenológica
- Suavizador LOESS (tendencia general del sesgo)
- Línea horizontal en y = 0
- Líneas verticales punteadas en DAS medio de cada transición fenológica

---

## Script 26 — PDPs con interpretación agronómica

### Modelo base

`modelo_ensamblado.rds` (stacks ensemble de script 14), mismo split seed=987.

### Predictores analizados (top 4 por importancia, script 14.1)

| Predictor | Comportamiento fisiológico esperado |
|-----------|-------------------------------------|
| `gdd_cumsum` | Tendencia monotónica creciente hasta ~800 GDD (heading), meseta o ligera caída hacia madurez (~1400 GDD) |
| `S1_VH` | Incremento con biomasa aérea (dispersión volumétrica del dosel cerrado) |
| `S1_VV` | Respuesta a estructura del cultivo; menos sensible a volumen foliar que VH |
| `S1_VH_VV` | Ratio que discrimina cultivo denso de suelo desnudo; caída en senescencia |

### Anotaciones agronómicas en figura

| Predictor | Referencia | Valor |
|-----------|------------|-------|
| `gdd_cumsum` | Heading | ≈ 800 GDD |
| `gdd_cumsum` | Madurez | ≈ 1400 GDD |
| `S1_VH` | Umbral dosel denso | 0.05 |
| `S1_VH_VV` | Señal senescencia | 0.25 |

### Figura: `pdp_top4_interpretacion_agronomica.png`

4 paneles (2×2), ejes X libres, líneas de referencia agronómicas en rojo, etiquetas de variables con notación matemática parseada.

---

## Verificación post-ejecución

| Script | Verificación |
|--------|-------------|
| 24 | `loso_metrics_per_fold.rds` tiene 108 filas; `ablation_loso_summary.csv` tiene 9 filas; `ps_improvement_pct` impreso en consola |
| 25 | Rango DAS cubre ~0–300 días; todas las etapas fenológicas presentes en la leyenda |
| 26 | `gdd_cumsum` muestra tendencia monotónica hasta heading; `S1_VH` muestra pendiente positiva |
