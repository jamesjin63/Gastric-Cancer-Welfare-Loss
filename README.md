# Welfare-Based Economic Burden of Gastric Cancer (VLW) — Analysis Code

This repository contains the analytical R scripts for the study:

**Global, regional, and national welfare-based economic burden of gastric cancer: estimates for 2021 and projections to 2050**

## Overview of the analytical workflow
1. Forecast DALYs to 2050 using exponential smoothing.
2. Estimate country-level VLW in 2021 and 2050 using a VSL-based willingness-to-pay framework.
3. Aggregate results to SDI and regional levels and compute GDP share.
4. Produce age–sex–country summaries and global age–sex profiles.
5. Generate summary figures and maps.

## Script inventory
- `ES 2050.R` — Exponential smoothing models and DALY forecasts to 2050.
- `VLW-new.R` — 2021 VLW estimation and summary tables (country, SDI, region, global).
- `VLW 2050.R` — 2050 VLW estimation and summary tables (country, SDI, region, global).
- `VLW-age group.R` — Age–sex–country VLW estimation and exports.
- `VLW-figure.R` — Figure generation for regional/SDI summaries and mapping.

## Data inputs (expected)
These scripts assume the following files exist at the referenced paths:
- `20250625极其重要 整理的 DALY 和各种经济指标 表1.xlsx`
- `204.csv` (GBD DALYs, 2021)
- `2050.csv` (forecasted DALYs to 2050)
- `21.csv` (region and SDI labels)
- `HALE.csv`
- Map assets referenced in `VLW-figure.R` (e.g., `mapdf_2024_China`, `namex`), loaded into the R session

## Outputs (examples)
The scripts write multiple CSVs and PDFs, including:
- `results.csv`
- `Country 204.csv`
- `total_summary.csv`
- `sdi_summary.csv`
- `Region21_summary.csv`
- `Global_summary.csv`
- `country_age_*_summary.csv`, `global_age_*_summary.csv`
- `21 region.pdf`, `5 SDI.pdf`

## Reproducibility
1. Start R (or RStudio) in this folder.
2. Confirm all input files exist at the paths referenced in each script.
3. Run the scripts in order:
   - `ES 2050.R`
   - `VLW-new.R`
   - `VLW 2050.R`
   - `VLW-age group.R`
   - `VLW-figure.R`

## Notes
- Some scripts use absolute paths (via `setwd()` or direct file references). Update these paths if your directory structure differs.
- Map objects used in `VLW-figure.R` must be loaded in the R session (e.g., via a prior `load()` call).
