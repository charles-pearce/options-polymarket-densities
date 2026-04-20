# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Project Does

This is a **financial forecasting research project** comparing prediction market probabilities (Polymarket) against options-implied risk-neutral probability densities for weekly equity options. The analysis covers 10 stocks (AAPL, AMZN, GOOGL, META, MSFT, NFLX, NVDA, OPEN, PLTR, TSLA) with 3 density extraction methods: single lognormal, 3-component lognormal mixture, and nonparametric Shimko spline.

## Running the Pipeline

There is no build system. Scripts are executed directly in order:

### Stage 1: Data Collection (Python)
```bash
# Requires ThetaData terminal running locally on port 25503
python theta_data.py          # Fetch options EOD data → data/{TICKER}/raw/
python get-polymarket-data.py # Fetch Polymarket bracket prices → data/{TICKER}/raw/
python data-cleaning.py       # Unify both sources → data/{TICKER}/{TICKER}_{DATE}_{td,pm}.csv
```

### Stage 2: Analysis (R)
```r
# Main orchestrator — runs full comparison pipeline for all tickers/DTEs
source("all-code/comparing/get comparisons.R")

# Individual density pipeline (parallel, 10 clusters)
source("all-code/comparing/complete_pipeline.R")
```

### Stage 3: Figures (R)
Each script in `charts-figures/` generates one or more publication figures and can be run independently after Stage 2 outputs exist.

## Architecture

### Data Flow
```
ThetaData API (localhost:25503) ─┐
                                  ├─► data-cleaning.py ─► data/{TICKER}/{TICKER}_{DATE}_{td,pm}.csv
Polymarket CLOB API ──────────────┘
                                               │
                    ┌──────────────────────────┘
                    ▼
         all-code/comparing/complete_pipeline.R
              (parallel, DTE 1–4)
              ├── all-code/Options/am_density.R       (American option RND)
              ├── all-code/PM/am pm density.R         (lognormal mixture fit)
              └── all-code/Non_parametric/shimko-density.R
                    │
                    ▼
         all-code/comparing/get comparisons.R
         (Brier/log scores, Hellinger/Wasserstein distance, calibration)
                    │
                    ▼
         charts-figures/*.R  (publication figures)
```

### Key Files
- [all-code/comparing/get comparisons.R](all-code/comparing/get%20comparisons.R) — main driver; sets tickers, DTE range, calls pipeline, outputs accuracy metrics and calibration results
- [all-code/comparing/complete_pipeline.R](all-code/comparing/complete_pipeline.R) — parallel (10-cluster) per-date processing loop
- [all-code/Options/am_density.R](all-code/Options/am_density.R) — risk-neutral density from American options via `RND` package
- [all-code/PM/am pm density.R](all-code/PM/am%20pm%20density.R) — fits 1- and 3-component lognormal mixtures to Polymarket bracket data using `optim()`
- [all-code/Non_parametric/shimko-density.R](all-code/Non_parametric/shimko-density.R) — Shimko spline nonparametric density extraction
- [data-cleaning.py](data-cleaning.py) — aligns ThetaData and Polymarket outputs; only keeps weeks with exactly 5 trading days and matching dates in both sources

### External Dependencies
- **ThetaData terminal** must be running locally on `localhost:25503` before `theta_data.py` runs
- **Python**: `httpx`, `requests`, `pandas`, `pandas_market_calendars`, `yfinance`
- **R**: `RND`, `transport`, `tidyverse`, `patchwork`, `foreach`, `doParallel`, `doSNOW`

### Calibration
Logistic recalibration is computed both per-stock and globally (pooled). Bootstrap confidence intervals are used throughout. Mincer-Zarnowitz regression tests forecast rationality.

### Output Structure
- `data/{TICKER}/` — cleaned CSVs per ticker per expiration date
- `results/` — comparison metrics, distance calculations, calibration outputs
- `charts-figures/` outputs — figures written to a figures directory (paths set within each script)
