# Gastric Cancer Welfare Loss (VLW) Study Code

This folder contains R scripts used for the study:

**Global, regional, and national welfare-based economic burden of gastric cancer: estimates for 2021 and projections to 2050**

## Contents
- `ES 2050.R` – Exponential smoothing forecasts to 2050.
- `VLW-new.R` – 2021 VLW calculation and summaries (country, region, SDI, global).
- `VLW 2050.R` – 2050 VLW calculation and summaries (country, region, SDI, global).
- `VLW-age group.R` – Age–sex–country VLW calculations and exports.
- `VLW-figure.R` – Figure generation for regional/SDI summaries and maps.

## Data inputs (expected)
These scripts assume the following files exist in their referenced paths:
- `20250625极其重要 整理的 DALY 和各种经济指标 表1.xlsx`
- `204.csv` (GBD DALY data, 2021)
- `2050.csv` (forecasted DALYs to 2050)
- `21.csv` (region/SDI labels)
- `HALE.csv`
- Map assets referenced in `VLW-figure.R` (e.g., `mapdf_2024_China`, `namex`, `mapdf_2024_China` objects loaded in session)

## Outputs
The scripts write multiple CSVs and PDFs including:
- `results.csv`
- `Country 204.csv`
- `total_summary.csv`
- `sdi_summary.csv`
- `Region21_summary.csv`
- `Global_summary.csv`
- `country_age_*_summary.csv`, `global_age_*_summary.csv`
- `21 region.pdf`, `5 SDI.pdf`

## How to run
1. Open R (or RStudio) in this folder.
2. Ensure all input files are present at the paths referenced in each script.
3. Run scripts in this order:
   - `ES 2050.R`
   - `VLW-new.R`
   - `VLW 2050.R`
   - `VLW-age group.R`
   - `VLW-figure.R`

## Notes
- Some scripts use absolute paths via `setwd()` or direct file references. Update those paths if your directory structure differs.
- Map objects used in `VLW-figure.R` must already be loaded in the R session (e.g., via a prior `load()` call).
