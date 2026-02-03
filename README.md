# Global Alpine Biodiversity Analysis 🏔️

![R](https://img.shields.io/badge/R-4.3.1-276DC3?style=flat&logo=r&logoColor=white)
![Status](https://img.shields.io/badge/status-under_review-yellow?style=flat)
![Data](https://img.shields.io/badge/data-figshare-0066CC?style=flat&logo=figshare&logoColor=white)
![License](https://img.shields.io/badge/license-TBD-lightgrey?style=flat)
![Taxa](https://img.shields.io/badge/taxa-mammals%20%7C%20birds%20%7C%20reptiles%20%7C%20plants-green?style=flat)

This repo contains all R code used to process and analyse data for the outputs of the article **"Contrasting patterns of alpine biodiversity across mountains and taxa"** (Schultz et. al, under review). 

This work has been developed for the [Past, Present and Future of Alpine Biomes Worldwide (PPF) project](https://mountainsinmotion.w.uib.no/)! 🏔🌎

---

## 📁 Project Structure

```
LS_global_alpine_biodiversity/
│
├── 📜 ___Init_project___.R          # ← START HERE: Initial setup & package installation
├── 📜 00_Config_file.R               # Project configuration & data paths
│
├── 📂 R/
│   ├── 📂 01_Data_processing/        # Data sourcing, restructuring & validation prep
│   │   ├── 📂 01_Mammals/            # MDD data → mountain overlap → elevation binding
│   │   ├── 📂 02_Birds/              # BirdLife data → mountain overlap → elevation
│   │   ├── 📂 03_Reptiles/           # GARD data → mountain overlap → elevation
│   │   └── 📂 04_Plants/             # GIFT checklists → elevation ranges
│   │
│   ├── 📂 02_Main_analyses/          # Core richness & diversity analyses
│   │   ├── 📂 01_post_validation_processing/  # Expert-validated data compilation
│   │   ├── 📂 02_Plants/             # Alpine categorization & richness (GIFT regions)
│   │   ├── 📂 03_Vertebrates/        # Alpine categorization & richness (mountains)
│   │   └── 📂 Mountains/             # Lapse rates & temperature-elevation models
│   │
│   ├── 📂 03_Figures_and_tables/     # Manuscript visualizations & tables
│   │   ├── 📂 Main/                  # Main text figures
│   │   └── 📂 Supplements/           # Supplementary materials
│   │
│   └── 📂 Functions/                 # Custom R functions
│       ├── calculate.overlaps.R
│       ├── extract.elevational.ranges.R
│       ├── lapse.rate.functions.R
│       └── relative.absolute.richness.*.R
│
└── 📂 renv/                          # Package dependency management
    └── activate.R
```

> **Template Credit:** This repo follows a template structure for R projects developed by [Ondrej Mottl](https://ondrejmottl.github.io/) and [Suzette Flantua](https://sflantua.github.io/)

---

## 🚀 Getting Started

1. **Run** [`___Init_project___.R`](R/___Init_project___.R) to install required packages
2. **Download data** from [figshare](https://figshare.com/s/3d4dcca576f6a73668b8)
3. **Configure** [`00_Config_file.R`](R/00_Config_file.R) with your data storage path
4. **Explore** the pipeline starting from `01_Data_processing/`

---

## 📊 Data Sources

| Data Type | Source | Reference |
|-----------|--------|-----------|
| 🦘 **Mammal Distributions** | Mammal Diversity Database (MDD) | [Marsh et al., 2022](https://onlinelibrary.wiley.com/doi/10.1111/jbi.14330) |
| 📈 **Mammal Elevations** | Handbook of Mammals of the World | [Machine readable version](https://github.com/jhpoelen/hmw) |
| 🦎 **Reptile Distributions** | Global Assessment Reptile Distribution | [Caetano et al., 2022 & Roll et al., 2017](https://datadryad.org/stash/dataset/doi:10.5061/dryad.9cnp5hqmb) |
| 📈 **Reptile Elevations** | SquamBase | [Meiri, 2024](https://onlinelibrary.wiley.com/doi/10.1111/geb.13812) |
| 🐦 **Bird Distributions** | BirdLife International | [After formal application](https://www.birdlife.org/) |
| 📈 **Bird Elevations** | - | [Quintero & Jetz, 2018](https://www.nature.com/articles/nature25794) |
| 🌱 **Plant Checklists** | GIFT Database | [Global Inventory of Floras and Traits](https://biogeomacro.github.io/GIFT/) |
| ⛰️ **Mountain Inventory** | GMBA | [Snethlage et al., 2022](https://ilias.unibe.ch/goto_ilias3_unibe_cat_1000515.html) |

### Data Availability

To run the code, download the data folder from [figshare](https://figshare.com/s/3d4dcca576f6a73668b8) and set your data storage path in the [`00_Config_file.R`](R/00_Config_file.R).

---


## 📖 Detailed Workflow

### 1️⃣ Data Processing

#### Mammals, Birds & Reptiles

**Pipeline:** Source Distribution Data → Overlap with Mountains → Bind Elevation Ranges → Extract DEM Elevations → Expert Validation Prep

- **Mammals**: MDD data overlapped with mountain ranges, elevations from HMW & DEM
- **Birds**: BirdLife data overlapped with mountains, elevations from Quintero & Jetz 2018
- **Reptiles**: GARD data overlapped with mountains, elevations from SquamBase

#### Plants

**Pipeline:** Source GIFT Regions → Filter Alpine Areas → Join Elevational Data → Extract Polygons & References

All plant data sourced from GIFT database. See detailed tutorials:
- [GIFT R package basics](https://biogeomacro.github.io/GIFT/articles/GIFT.html)
- [GIFT advanced users guide](https://biogeomacro.github.io/GIFT/articles/GIFT_advanced_users.html)

### 2️⃣ Main Analyses

#### Post-Validation Processing
Compile and clean expert-validated species checklists for all taxa.

#### Plants Analysis
1. **Categorize species** relative to upper forest line (UFL):
   - Alpine specialist
   - UFL-alpine
   - Mid-montane
   - Broad-montane
   - Alpine generalist
2. **Calculate** absolute & relative richness per GIFT region
3. **Compute** proportions of each category

#### Vertebrates Analysis
1. **Categorize species** relative to UFL (same categories as plants)
2. **Calculate** absolute & relative richness per mountain range
3. **Compute** proportions of each category

#### Mountains Analysis
Calculate lapse rates for each mountain range and GIFT region:
- **Model**: `temperature ~ elevation`
- **Conversion**: `Elevation Change = ΔTemp / Lapse Rate × 1000`

### 3️⃣ Figures & Tables

All visualizations and tables for the manuscript and supplementary materials.

---

## 💰 Funding

This research has been developed for the PPF-Alpine project and is funded by the [Trond Mohn Stiftelse](https://mohnfoundation.no/) and University of Bergen, Norway.

---

## 📧 Contact

If you have any questions regarding the project, this repository or the publication, please do not hesitate to contact us! 

**Email:** lotta.schultz@outlook.com

---

[![PPF](https://img.shields.io/badge/PPF-Alpine_Project-4A90E2?style=for-the-badge)](https://mountainsinmotion.w.uib.no/)


