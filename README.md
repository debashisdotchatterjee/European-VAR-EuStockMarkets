# European-VAR-EuStockMarkets

# European Capital Markets VAR Analysis (EuStockMarkets)

Reproducible codebase for the paper:

**Dynamic Interdependence and Volatility Transmission in European Capital Markets: A Vector Autoregression (VAR) Analysis**

This repository implements a complete empirical pipeline (data → preprocessing → VAR estimation → diagnostics → spillover analysis → forecasting verification) using the classic **`EuStockMarkets`** dataset in R.

---

## 1. What this project does

Using daily closing prices of four major European stock indices (1991–1998):

- **DAX** (Germany)  
- **SMI** (Switzerland)  
- **CAC** (France)  
- **FTSE** (United Kingdom)

the code:

1. Converts price **levels** into **log-returns**
2. Verifies stationarity (ADF / KPSS)
3. Fits a multivariate **VAR(p)** model (lag selection by AIC)
4. Runs **diagnostics**:
   - Portmanteau serial correlation test  
   - Multivariate ARCH test  
   - Multivariate normality (Jarque–Bera, skewness, kurtosis)  
   - OLS-CUSUM stability check
5. Conducts **system-level Granger causality**
6. Computes **Impulse Response Functions (IRF)** with bootstrap confidence intervals
7. Computes **Forecast Error Variance Decomposition (FEVD)**
8. Performs **out-of-sample forecasting verification** (VAR vs independent univariate AR) with RMSE comparison
9. Saves all plots and tables into reproducible output folders.

---


If your current file names differ, keep the README text and just update the paths.

---

## 2. Requirements

### R version
- R **>= 4.1** (recommended: latest stable)

### Core R packages



- `vars` (VAR, IRF, FEVD, diagnostics)
- `tseries` (ADF)
- `urca` (KPSS alternative; optional depending on your implementation)
- `ggplot2` (plots)
- `reshape2` or `tidyr` / `dplyr` (data wrangling)
- `forecast` (AR baselines; optional)
- `gridExtra` / `patchwork` (plot arrangement; optional)
- `strucchange` (OLS-CUSUM; if used)

Install (example):

```r
install.packages(c(
  "vars","tseries","ggplot2","dplyr","tidyr",
  "reshape2","forecast","strucchange"
))




A recommended layout (adapt to your repository as needed):

