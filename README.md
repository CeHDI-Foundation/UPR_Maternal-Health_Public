# UPR Recommendations and Maternal Health

Analysis code accompanying a study of Universal Periodic Review (UPR) recommendations related to the right to health, and their association with country-level maternal health outcomes (maternal mortality ratio, skilled birth attendance, and contraceptive prevalence).

## Background

The [Universal Periodic Review](https://www.ohchr.org/en/hr-bodies/upr/upr-main) is a UN Human Rights Council mechanism in which all UN member states review one another's human rights records. Recommendations issued in each review are catalogued in the [Universal Human Rights Index (UHRI)](https://uhri.ohchr.org/en/our-data-api).

This repository:

1. Downloads the full UHRI recommendations dataset and classifies recommendations into thematic groupings (maternal health, abortion, sexual and reproductive health, gender-based violence, health systems, etc.).
2. Links the recommendation counts received by each country to longitudinal outcome data drawn from WHO, World Bank, UNDP, UNFPA, V-Dem, and other sources.
3. Fits multilevel models (with `lme4` / `glmmTMB`) examining whether the number of maternal-health-related recommendations a country receives is associated with subsequent trends in maternal mortality ratio (MMR), skilled birth attendance (SBA), and contraceptive prevalence (CPR).
4. Produces the descriptive tables and forest plots used in the publication.

## Repository layout

```
.
├── code/
│   ├── 01_prep_geo_code.R                              # Build country geometries + region/grouping lookups
│   ├── 02_prep_UHRI_recommendation_definitions.qmd     # Download UHRI dataset and classify recommendations by theme
│   ├── 03_prep_datatable_code.R                        # Merge recommendation counts with indicator data into the analysis tables
│   ├── 03b_prep_read_datatable.R                       # Helper sourced by downstream scripts to load the analysis tables
│   ├── 03c_analysis_descriptive_rec_stats.R            # Descriptive summaries of recommendations per country / cycle
│   ├── 04a_analysis_MLM_forest_MH.R                    # Multilevel model: maternal-health recs vs. MMR + full thematic groupings vs. MMR
│   ├── 04b_analysis_MLM_forest_SBA.R                   # Multilevel model: maternal-health recs vs. SBA + full thematic groupings vs. SBA
│   ├── 04c_analysis_MLM_forest_CPR.R                   # Multilevel model: maternal-health recs vs. CPR + full thematic groupings vs. CPR
│   ├── 04d_analysis_descriptive_table.R                # Country-level descriptive (Table 1+2) statistics
│   ├── 05_plotting_forestplots_MLM_combined.R          # Combined forest plot across the three outcomes (Figure 1)
│   ├── DAG.R                                           # Directed acyclic graph used to guide covariate selection
│   ├── external_data_OData.R                           # Pull indicator data from WHO GHO and other OData/API sources
│   ├── external_data_GBD.R                             # Pull Global Burden of Disease data
│   └── theme_labels.R                                  # Display labels for thematic variables
├── data/                                               # Raw inputs (CSV/XLSX) and cached API pulls under data/API_data/
├── output/                                             # Intermediate and final RDS files; publication figures
└── UPR_Maternal-Health_Public.Rproj
```

## Data sources

External datasets used (downloaded into `data/` or pulled programmatically into `data/API_data/`):

- **UHRI** — UPR recommendations (`https://dataex.ohchr.org/uhri/export-results/export-full-en.xlsx`)
- **WHO Global Health Observatory** — MMR, SBA, ANC4, institutional births, UHC sub-indices, immunization coverage, etc., via OData
- **World Bank** — GDP per capita (PPP), literacy rates, contraceptive prevalence, Worldwide Governance Indicators
- **UNDP Human Development Report 2025** — HDI and related composite indices
- **UNFPA** — Contraceptive prevalence (any method)
- **V-Dem** — Varieties of Democracy indicators
- **World Bank FCS list, OACPS, CARICOM, COMESA, ECSA, South Centre** — Country grouping memberships
- **`wpp2024`** — UN World Population Prospects 2024 (population, fertility, life expectancy)
- **`necountries`** — Country geometries for mapping

## Requirements

- R (≥ 4.2 recommended)
- [`pacman`](https://cran.r-project.org/package=pacman) to manage package installation: `install.packages("pacman")`

Each script loads its dependencies via `pacman::p_load(...)` at the top, so packages will be installed on first run. Two GitHub-only packages are installed via `pacman::p_load_gh()` in `external_data_OData.R`:

- `vdeminstitute/vdemdata`
- `PPgp/wpp2024`

## Usage

The scripts are numbered in the order they should be run. A typical end-to-end run looks like:

1. **`code/01_prep_geo_code.R`** — builds `output/state_geo_enhanced.rds` (and `state_geo2_enhanced.rds`) used downstream.
2. **`code/external_data_OData.R`** — pulls indicator data from WHO GHO and other APIs; saves each object to `data/API_data/*.rds`. (Re-run periodically to refresh; the file warns at the top that it clears the workspace.)
3. **`code/02_prep_UHRI_recommendation_definitions.qmd`** — render this Quarto document to download the UHRI dataset and apply the thematic classification; produces `output/UHRI_UPR_enhanced.rds`.
4. **`code/03_prep_datatable_code.R`** — merges recommendation counts with indicator data into the wide and long analysis tables (`output/dat_table_<date>.rds`, `output/dat_table_long_<date>.rds`). After running, update the dated filenames in `code/03b_prep_read_datatable.R`.
5. **`code/03c_analysis_descriptive_rec_stats.R`** and **`code/04d_analysis_descriptive_table.R`** — descriptive statistics.
6. **`code/04a_*.R`, `04b_*.R`, `04c_*.R`** — multilevel models for MMR, SBA, and CPR.
7. **`code/05_plotting_forestplots_MLM_combined.R`** — produces the combined forest plot in `output/publication_figures/`.

The pre-processed data files are already available in the repository, and rather than regenerating the files from scratch the analysis can already be run from the **`code/03c_analysis_descriptive_rec_stats.R`** file onwards.

## Notes

- Some intermediate files include a date in their filename (e.g. `dat_table_2026-05-26.rds`). When regenerating these, update the filename references in `code/03b_prep_read_datatable.R`.
- The repository uses [`here::here()`](https://here.r-lib.org/) for all paths; open the project via `UPR_Maternal-Health_Public.Rproj` so that the project root is set correctly.

## Contact

[Anshu Uppal](https://github.com/anshu-uppal)