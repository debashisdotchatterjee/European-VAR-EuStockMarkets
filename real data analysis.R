# ============================================================
# EuStockMarkets VAR Analysis Pipeline (COMPLETE, RUN-ONCE)
# - No interactive "Hit <Return>" pauses
# - Saves MANY plots + tables automatically
# - Also prints key results to console
# ============================================================

# -----------------------------
# 0) Packages
# -----------------------------
pkgs <- c(
  "datasets", "stats",
  "tidyverse",
  "tseries", "urca",
  "vars", "forecast",
  "kableExtra",
  "corrplot",
  "zoo"
)

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

invisible(lapply(pkgs, library, character.only = TRUE))

# -----------------------------
# 1) Output folders
# -----------------------------
OUT_DIR   <- "simulation_results_realdata"
PLOT_DIR  <- file.path(OUT_DIR, "plots")
TAB_DIR   <- file.path(OUT_DIR, "tables")
RDATA_DIR <- file.path(OUT_DIR, "rdata")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TAB_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(RDATA_DIR, showWarnings = FALSE, recursive = TRUE)

message("Saving outputs to: ", normalizePath(OUT_DIR))

# -----------------------------
# 1.1) Helpers
# -----------------------------
save_plot <- function(p, filename, w = 10, h = 6, dpi = 300) {
  ggplot2::ggsave(
    filename = file.path(PLOT_DIR, filename),
    plot = p, width = w, height = h, dpi = dpi
  )
}

save_table <- function(df, filename_stub, caption = NULL, digits = 4) {
  # CSV
  utils::write.csv(df, file.path(TAB_DIR, paste0(filename_stub, ".csv")), row.names = FALSE)
  
  # HTML
  html_file <- file.path(TAB_DIR, paste0(filename_stub, ".html"))
  kbl <- knitr::kable(df, format = "html", digits = digits, caption = caption) |>
    kableExtra::kable_styling(full_width = FALSE,
                              bootstrap_options = c("striped", "hover", "condensed"))
  kableExtra::save_kable(kbl, file = html_file)
  
  invisible(TRUE)
}

# -----------------------------
# 2) Load data + tidy frames
# -----------------------------
data("EuStockMarkets", package = "datasets")
X_levels <- EuStockMarkets
stopifnot(ncol(X_levels) == 4)

colnames(X_levels) <- c("DAX", "SMI", "CAC", "FTSE")

df_levels <- as.data.frame(X_levels) |>
  dplyr::mutate(t = 1:dplyr::n()) |>
  dplyr::relocate(t)

df_levels_long <- df_levels |>
  tidyr::pivot_longer(cols = c("DAX","SMI","CAC","FTSE"),
                      names_to = "Index", values_to = "Price")

# -----------------------------
# 3) Log-returns
# -----------------------------
# log-returns: diff(log P)
ret_mat <- apply(log(X_levels), 2, diff)
ret_mat <- as.matrix(ret_mat)
colnames(ret_mat) <- c("DAX", "SMI", "CAC", "FTSE")

df_ret <- as.data.frame(ret_mat) |>
  dplyr::mutate(t = 2:nrow(X_levels)) |>
  dplyr::relocate(t)

df_ret_long <- df_ret |>
  tidyr::pivot_longer(cols = c("DAX","SMI","CAC","FTSE"),
                      names_to = "Index", values_to = "Return")

# -----------------------------
# 4) Plots: levels, returns, rolling vol, correlation heatmap
# -----------------------------
p_levels <- ggplot2::ggplot(df_levels_long, ggplot2::aes(x = t, y = Price)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~Index, scales = "free_y", ncol = 2) +
  ggplot2::labs(
    title = "EuStockMarkets: Index Levels (Closing Prices)",
    x = "Time index (trading days)", y = "Level"
  )
save_plot(p_levels, "01_levels.png", w = 12, h = 7)

p_returns <- ggplot2::ggplot(df_ret_long, ggplot2::aes(x = t, y = Return)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~Index, scales = "free_y", ncol = 2) +
  ggplot2::labs(
    title = "EuStockMarkets: Daily Log-Returns",
    x = "Time index (trading days)", y = "log-return"
  )
save_plot(p_returns, "02_log_returns.png", w = 12, h = 7)

# Rolling volatility (20-day rolling SD)
roll_k <- 20
df_roll <- df_ret |>
  tidyr::pivot_longer(cols = c("DAX","SMI","CAC","FTSE"),
                      names_to = "Index", values_to = "Return") |>
  dplyr::group_by(Index) |>
  dplyr::arrange(t) |>
  dplyr::mutate(RollSD = zoo::rollapply(Return, width = roll_k,
                                        FUN = stats::sd, fill = NA, align = "right")) |>
  dplyr::ungroup()

p_roll <- ggplot2::ggplot(df_roll, ggplot2::aes(x = t, y = RollSD)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~Index, scales = "free_y", ncol = 2) +
  ggplot2::labs(
    title = paste0("Rolling Volatility (", roll_k, "-day SD) of Log-Returns"),
    x = "Time index (trading days)", y = "Rolling SD"
  )
save_plot(p_roll, "03_rolling_volatility.png", w = 12, h = 7)

# Correlation heatmap of returns
cor_ret <- stats::cor(df_ret[, c("DAX","SMI","CAC","FTSE")], use = "pairwise.complete.obs")

png(file.path(PLOT_DIR, "04_correlation_heatmap.png"), width = 1100, height = 900, res = 160)
corrplot::corrplot(cor_ret, method = "color", type = "upper", addCoef.col = "black",
                   tl.cex = 1.2, number.cex = 0.9)
dev.off()

# -----------------------------
# 5) Tables: descriptive stats + correlation
# -----------------------------
desc_stats <- df_ret |>
  dplyr::select(DAX, SMI, CAC, FTSE) |>
  dplyr::summarise(dplyr::across(
    dplyr::everything(),
    list(
      mean = mean,
      sd = sd,
      min = min,
      q05 = ~stats::quantile(.x, 0.05),
      median = stats::median,
      q95 = ~stats::quantile(.x, 0.95),
      max = max
    ),
    .names = "{.col}_{.fn}"
  )) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Metric", values_to = "Value") |>
  tidyr::separate(Metric, into = c("Index","Stat"), sep = "_(?=[^_]+$)") |>
  tidyr::pivot_wider(names_from = Stat, values_from = Value) |>
  dplyr::arrange(Index)

save_table(desc_stats, "01_descriptive_stats", "Descriptive statistics of log-returns", digits = 6)

cor_tbl <- as.data.frame(cor_ret) |>
  tibble::rownames_to_column("Index")
save_table(cor_tbl, "02_return_correlation", "Correlation matrix of log-returns", digits = 3)

# -----------------------------
# 6) Stationarity tests: ADF + KPSS
# -----------------------------
adf_results <- purrr::map_dfr(c("DAX","SMI","CAC","FTSE"), function(v) {
  x <- df_ret[[v]]
  res <- tseries::adf.test(x)
  tibble::tibble(
    Index = v,
    ADF_statistic = unname(res$statistic),
    p_value = res$p.value,
    method = res$method
  )
})

kpss_results <- purrr::map_dfr(c("DAX","SMI","CAC","FTSE"), function(v) {
  x <- df_ret[[v]]
  res <- tseries::kpss.test(x, null = "Level")
  tibble::tibble(
    Index = v,
    KPSS_statistic = unname(res$statistic),
    p_value = res$p.value,
    method = res$method
  )
})

save_table(adf_results, "03_adf_tests", "ADF unit-root tests for log-returns", digits = 6)
save_table(kpss_results, "04_kpss_tests", "KPSS stationarity tests (level) for log-returns", digits = 6)

cat("\nADF tests:\n"); print(adf_results)
cat("\nKPSS tests:\n"); print(kpss_results)

# -----------------------------
# 7) VAR lag selection + estimation
# -----------------------------
Y <- df_ret[, c("DAX","SMI","CAC","FTSE")]

lag_max <- 10
var_sel <- vars::VARselect(Y, lag.max = lag_max, type = "const")
saveRDS(var_sel, file.path(RDATA_DIR, "VARselect.rds"))

cat("\nVAR Lag Selection criteria:\n")
print(var_sel$criteria)

p_selected <- which.min(var_sel$criteria["AIC(n)", ])
cat("\nSelected lag by AIC:", p_selected, "\n")

var_fit <- vars::VAR(Y, p = p_selected, type = "const")
saveRDS(var_fit, file.path(RDATA_DIR, "VAR_fit.rds"))

cat("\nVAR Summary:\n")
print(summary(var_fit))

# -----------------------------
# 8) Diagnostics: serial correlation, ARCH, normality, stability
# -----------------------------
diag_serial <- vars::serial.test(var_fit, lags.pt = 16, type = "PT.asymptotic")
diag_arch   <- vars::arch.test(var_fit, lags.multi = 12)
diag_norm   <- vars::normality.test(var_fit)
diag_stab   <- vars::stability(var_fit, type = "OLS-CUSUM")

saveRDS(list(serial = diag_serial, arch = diag_arch, normality = diag_norm, stability = diag_stab),
        file.path(RDATA_DIR, "VAR_diagnostics.rds"))

cat("\n--- Serial correlation test ---\n"); print(diag_serial)
cat("\n--- ARCH test ---\n"); print(diag_arch)
cat("\n--- Normality test ---\n"); print(diag_norm)

png(file.path(PLOT_DIR, "05_stability_ols_cusum.png"), width = 1100, height = 800, res = 150)
plot(diag_stab)
dev.off()

# -----------------------------
# 9) Granger causality (system-level from each market)
# -----------------------------
cause_list <- lapply(colnames(Y), function(xname) vars::causality(var_fit, cause = xname))
names(cause_list) <- colnames(Y)
saveRDS(cause_list, file.path(RDATA_DIR, "Granger_causality.rds"))

granger_tbl <- purrr::map_dfr(names(cause_list), function(v) {
  test <- cause_list[[v]]$Granger
  tibble::tibble(
    Cause = v,
    Statistic = as.numeric(test$statistic),
    p_value = as.numeric(test$p.value),
    df1 = as.integer(test$parameter[1]),
    df2 = as.integer(test$parameter[2])
  )
})

save_table(granger_tbl, "05_granger_system", "Granger causality (system-level) from each market", digits = 6)

cat("\nGranger causality (system-level):\n")
print(granger_tbl)

# -----------------------------
# 10) IRF + FEVD (NON-INTERACTIVE SAVING; no 'Hit <Return>')
# -----------------------------
set.seed(123)
irf_h <- 10
irf_fit <- vars::irf(var_fit, n.ahead = irf_h, boot = TRUE, ci = 0.95, runs = 500)
saveRDS(irf_fit, file.path(RDATA_DIR, "IRF.rds"))

# Save IRF plots as PDF (best) + PNG (optional)
pdf(file.path(PLOT_DIR, "06_irf_all.pdf"), width = 12, height = 8)
plot(irf_fit)
dev.off()

png(file.path(PLOT_DIR, "06_irf_all.png"), width = 1600, height = 1100, res = 170)
plot(irf_fit)
dev.off()

# FEVD
fevd_fit <- vars::fevd(var_fit, n.ahead = 10)
saveRDS(fevd_fit, file.path(RDATA_DIR, "FEVD.rds"))

pdf(file.path(PLOT_DIR, "07_fevd.pdf"), width = 12, height = 8)
plot(fevd_fit)
dev.off()

png(file.path(PLOT_DIR, "07_fevd.png"), width = 1600, height = 1100, res = 170)
plot(fevd_fit)
dev.off()

# -----------------------------
# 11) Forecast evaluation: VAR vs independent AR (out-of-sample RMSE)
# -----------------------------
h_test <- 50
Tn <- nrow(Y)

train_idx <- 1:(Tn - h_test)
test_idx  <- (Tn - h_test + 1):Tn

Y_train <- Y[train_idx, ]
Y_test  <- Y[test_idx,  ]

var_fit_tr <- vars::VAR(Y_train, p = p_selected, type = "const")
var_fc <- predict(var_fit_tr, n.ahead = h_test)

var_hat <- sapply(colnames(Y), function(v) var_fc$fcst[[v]][, "fcst"])
var_hat <- as.matrix(var_hat)
colnames(var_hat) <- colnames(Y)

ar_hat <- matrix(NA_real_, nrow = h_test, ncol = ncol(Y))
colnames(ar_hat) <- colnames(Y)

for (j in seq_along(colnames(Y))) {
  v <- colnames(Y)[j]
  fit_ar <- forecast::auto.arima(Y_train[[v]])
  fc_ar  <- forecast::forecast(fit_ar, h = h_test)
  ar_hat[, j] <- as.numeric(fc_ar$mean)
}

rmse <- function(a, f) sqrt(mean((a - f)^2, na.rm = TRUE))

rmse_tbl <- tibble::tibble(
  Index = colnames(Y),
  RMSE_VAR = sapply(1:ncol(Y), function(j) rmse(Y_test[[j]], var_hat[, j])),
  RMSE_AR  = sapply(1:ncol(Y), function(j) rmse(Y_test[[j]], ar_hat[, j]))
) |>
  dplyr::mutate(Improvement_pct = 100 * (RMSE_AR - RMSE_VAR) / RMSE_AR)

save_table(rmse_tbl, "06_rmse_var_vs_ar", "Out-of-sample RMSE: VAR vs independent AR", digits = 8)

cat("\nRMSE comparison:\n")
print(rmse_tbl)

# Forecast plots
df_fc <- tibble::tibble(t = df_ret$t[test_idx]) |>
  dplyr::bind_cols(as.data.frame(Y_test) |> stats::setNames(paste0(colnames(Y), "_actual"))) |>
  dplyr::bind_cols(as.data.frame(var_hat) |> stats::setNames(paste0(colnames(Y), "_VAR"))) |>
  dplyr::bind_cols(as.data.frame(ar_hat)  |> stats::setNames(paste0(colnames(Y), "_AR")))

for (v in colnames(Y)) {
  d <- df_fc |>
    dplyr::transmute(
      t,
      actual = .data[[paste0(v, "_actual")]],
      VAR = .data[[paste0(v, "_VAR")]],
      AR  = .data[[paste0(v, "_AR")]]
    ) |>
    tidyr::pivot_longer(cols = c("actual","VAR","AR"), names_to = "Series", values_to = "Value")
  
  p <- ggplot2::ggplot(d, ggplot2::aes(x = t, y = Value, linetype = Series)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = paste0("Forecast comparison (last ", h_test, " obs): ", v),
      x = "Time index", y = "Return"
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  save_plot(p, paste0("08_forecast_", v, ".png"), w = 11, h = 5)
}

# -----------------------------
# 12) Save main objects
# -----------------------------
save(
  df_levels, df_ret, desc_stats, cor_ret,
  adf_results, kpss_results, var_sel, var_fit,
  granger_tbl, irf_fit, fevd_fit, rmse_tbl,
  file = file.path(RDATA_DIR, "workspace_objects.RData")
)

message("\nDONE.")
message("Plots:  ", normalizePath(PLOT_DIR))
message("Tables: ", normalizePath(TAB_DIR))
message("RData:  ", normalizePath(RDATA_DIR))
