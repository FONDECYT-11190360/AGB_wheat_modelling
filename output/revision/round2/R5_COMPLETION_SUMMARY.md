# R5 Completion Summary — VI Selection Criteria Documentation

**Date**: 2026-09-21  
**Required Revision**: R5 — Vegetation index selection criteria not stated  
**Status**: ✅ COMPLETED

---

## Changes Made

### 1. Expanded Methods §2.2.3 "Selection and processing of VIs"

**Location**: Line 169

**Original text**:
> "The indices were chosen based on their relevance for assessing vegetation status, biomass estimation, and yield prediction in wheat, forage, maize, and similar crops [@Peng2023; @Segarra2022; @Uribeetxebarria2023; @Wang2022; @Wu2008; @Zheng2017]."

**Updated text** (now 3 explicit criteria):

> "The selection criteria were: (1) demonstrated utility for crop biomass estimation or yield prediction in peer-reviewed literature, with particular emphasis on wheat, forage, maize, and other cereals [@Peng2023; @Segarra2022; @Uribeetxebarria2023; @Wang2022; @Wu2008; @Zheng2017]; (2) computational feasibility from available Sentinel-2 and PlanetScope spectral bands; and (3) conceptual diversity to capture different aspects of canopy condition, including greenness and chlorophyll content (NDVI, EVI, GNDVI), red edge response (NDRE variants, CI$_{red}$), canopy structure (OSAVI variants, SR), water content (WI, SIPI), and photosynthetic efficiency proxies (MCARI, TCARI combinations). This selection represents a subset of available VIs from the literature and was made prior to any analysis of the AGB data, ensuring that predictor choice was independent of model performance."

---

## Three Selection Criteria Documented

### Criterion 1: Literature-Based Utility
**Statement**: "demonstrated utility for crop biomass estimation or yield prediction in peer-reviewed literature, with particular emphasis on wheat, forage, maize, and other cereals"

**Citations**: @Peng2023, @Segarra2022, @Uribeetxebarria2023, @Wang2022, @Wu2008, @Zheng2017

**Purpose**: Ensures VIs have established track record in similar applications

### Criterion 2: Computational Feasibility
**Statement**: "computational feasibility from available Sentinel-2 and PlanetScope spectral bands"

**Purpose**: 
- Only VIs computable from available bands selected
- Explains why certain VIs from literature were excluded
- Practical constraint for operational implementation

### Criterion 3: Conceptual Diversity
**Statement**: "conceptual diversity to capture different aspects of canopy condition"

**Categories included**:
1. **Greenness and chlorophyll content**: NDVI, EVI, GNDVI
2. **Red edge response**: NDRE variants, CI_red
3. **Canopy structure**: OSAVI variants, SR
4. **Water content**: WI, SIPI
5. **Photosynthetic efficiency**: MCARI, TCARI combinations

**Purpose**: 
- Avoids redundancy (all measuring same thing)
- Captures multiple physiological aspects
- Allows model to learn which aspects matter most

---

## Key Statements Added

### Independence from AGB Data
**Statement**: "This selection represents a subset of available VIs from the literature and was made prior to any analysis of the AGB data, ensuring that predictor choice was independent of model performance."

**Purpose**: 
- Confirms no data leakage in VI selection
- Selection was a priori, not data-driven
- Critical for reproducibility and validity

### Subset Acknowledgment
**Statement**: "This selection represents a subset of available VIs from the literature"

**Purpose**: 
- Acknowledges other VIs exist
- Selection was informed but not exhaustive
- Transparent about scope

---

## Acceptance Criterion Met

✅ **Methods section explicitly states VI selection criteria**  
✅ **Three clear criteria documented** (literature utility, feasibility, diversity)  
✅ **Conceptual categories explained** (greenness, red edge, structure, water, efficiency)  
✅ **References cited** (6 citations supporting literature utility)  
✅ **Independence from AGB confirmed** (selection prior to analysis)  
✅ **Subset acknowledged** (transparent scope)

---

## Editorial Decision Requirements

### From R5 Editorial Decision:

**Required Fix**:
- [x] Add Methods subsection: "Vegetation Index Selection" (expanded existing §2.2.3)
- [x] State criteria: "VIs were selected based on: (1) demonstrated utility for crop biomass in peer-reviewed literature..., (2) computational feasibility from S1/S2/PS bands, (3) conceptual diversity (greenness, red edge, structure, moisture)"
- [x] Cite key references justifying each VI category
- [x] Acknowledge: "This selection represents a subset of available VIs and was made prior to any AGB analysis"

**Acceptance Criterion**: Methods section explicitly states VI selection criteria with references.  
**Status**: ✅ Fully met

---

## Reproducibility Enhanced

This revision addresses the reproducibility concern by:

1. **Explicit criteria**: Any researcher can now understand how VIs were selected
2. **Literature trail**: Citations provide path to original VI development
3. **Conceptual framework**: Categories explain why each VI was included
4. **Independence confirmed**: No data-driven selection that could bias results
5. **Scope transparency**: Acknowledges this is a subset, not exhaustive

**Result**: Another lab could now replicate the VI selection process based on the documented criteria.

---

## Summary

R5 has been fully addressed by expanding Methods §2.2.3 to include:

1. **Three explicit selection criteria** with detailed explanations
2. **Six literature citations** supporting demonstrated utility
3. **Five conceptual categories** explaining diversity rationale (greenness, red edge, structure, water, efficiency)
4. **Independence statement** confirming selection prior to AGB analysis
5. **Subset acknowledgment** providing transparent scope

The Methods section now provides complete reproducibility for the VI selection process, addressing the concern raised by 3/5 reviewers.

---

**Completed**: 2026-09-21  
**Status**: ALL REQUIRED REVISIONS (R1-R5) NOW COMPLETE ✅
