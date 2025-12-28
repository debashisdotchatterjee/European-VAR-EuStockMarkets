# ==============================================================================
# Simulation and Verification of VAR Methodology for European Capital Markets
# ==============================================================================
# This script performs:
# 1. Data Loading & Transformation (Log-Returns)
# 2. Model Estimation: Proposed VAR vs. Baseline Univariate AR (Generic)
# 3. Model Verification: Out-of-sample Forecasting Comparison (RMSE)
# 4. Structural Analysis: Impulse Response Functions (IRF) & Granger Causality
# 5. Output: Generates colorful plots and tables in 'simulation_results/' folder
# ==============================================================================

# --- 1. Setup and Library Loading ---
if (!require("vars")) install.packages("vars", quiet=TRUE)
if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE)
if (!require("reshape2")) install.packages("reshape2", quiet=TRUE)
if (!require("forecast")) install.packages("forecast", quiet=TRUE)
if (!require("gridExtra")) install.packages("gridExtra", quiet=TRUE)
if (!require("tseries")) install.packages("tseries", quiet=TRUE)

library(vars)
library(ggplot2)
library(reshape2)
library(forecast)
library(gridExtra)
library(tseries)

# Create output directory
dir.create("simulation_results", showWarnings = FALSE)
cat("Directory 'simulation_results' created/verified.\n\n")

# --- 2. Data Preparation ---
cat("--- 1. Data Preparation ---\n")
data("EuStockMarkets")
# Log-Returns Calculation: r_t = ln(P_t) - ln(P_{t-1})
# This ensures stationarity as discussed in the paper
returns <- diff(log(EuStockMarkets)) * 100 # Multiplied by 100 for percentage terms

# Split into Train (Estimation) and Test (Verification) sets
# We hold out the last 50 days for forecast verification
n_total <- nrow(returns)
n_forecast <- 50
train_data <- returns[1:(n_total - n_forecast), ]
test_data  <- returns[(n_total - n_forecast + 1):n_total, ]

cat(sprintf("Total Observations: %d\n", n_total))
cat(sprintf("Training Set: %d\n", nrow(train_data)))
cat(sprintf("Test Set: %d\n", nrow(test_data)))

# --- 3. Visual Analysis (Figure Generation) ---
cat("\n--- 2. Generating Exploratory Plots ---\n")

# Convert to DF for ggplot
dates <- time(EuStockMarkets)[-1] # Remove first date due to differencing
df_returns <- data.frame(Date = dates, returns)
df_long <- melt(df_returns, id.vars = "Date", variable.name = "Index", value.name = "Return")

# Plot 1: Volatility Clustering
p1 <- ggplot(df_long, aes(x = Date, y = Return, color = Index)) +
  geom_line(alpha = 0.7, linewidth = 0.4) +
  facet_wrap(~Index, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(title = "Volatility Clustering in European Log-Returns (1991-1998)",
       subtitle = "Visual evidence of non-constant variance (ARCH effects)",
       y = "Daily Log-Return (%)", x = "Year") +
  theme(legend.position = "none", plot.title = element_text(face="bold")) +
  scale_color_brewer(palette = "Set1")

ggsave("simulation_results/01_volatility_clustering.png", p1, width = 10, height = 8)
print(p1)

# Plot 2: Correlation Heatmap
cormat <- cor(train_data)
melted_cormat <- melt(cormat)
p2 <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(Var2, Var1, label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() + 
  coord_fixed() +
  labs(title = "Correlation Matrix of Returns",
       subtitle = "High contemporaneous correlation justifies Multivariate (VAR) approach",
       x="", y="")

ggsave("simulation_results/02_correlation_matrix.png", p2, width = 6, height = 5)
print(p2)

# --- 4. Model Estimation & Comparison ---
cat("\n--- 3. Model Estimation: VAR vs. Univariate AR ---\n")

# A. Proposed Methodology: Vector Autoregression (VAR)
# Lag Selection
lag_selection <- VARselect(train_data, lag.max = 10, type = "const")
optimal_lag <- lag_selection$selection["AIC(n)"]
cat(sprintf("Optimal Lag Length selected by AIC: %d\n", optimal_lag))

# Fit VAR Model
var_model <- VAR(train_data, p = optimal_lag, type = "const")
cat("VAR Model Estimated Successfully.\n")

# B. Baseline "Generic" Methodology: Univariate AR models
# We fit 4 separate AR models, ignoring the cross-market effects
ar_dax  <- Arima(train_data[, "DAX"], order = c(optimal_lag, 0, 0))
ar_smi  <- Arima(train_data[, "SMI"], order = c(optimal_lag, 0, 0))
ar_cac  <- Arima(train_data[, "CAC"], order = c(optimal_lag, 0, 0))
ar_ftse <- Arima(train_data[, "FTSE"], order = c(optimal_lag, 0, 0))
cat("Baseline Univariate AR Models Estimated.\n")

# --- 5. Methodology Verification: Forecast Accuracy ---
cat("\n--- 4. Verification: Out-of-Sample Forecasting Comparison ---\n")

# Predict with VAR
var_pred <- predict(var_model, n.ahead = n_forecast)

# Predict with Univariate AR
ar_pred_dax  <- forecast(ar_dax, h = n_forecast)$mean
ar_pred_smi  <- forecast(ar_smi, h = n_forecast)$mean
ar_pred_cac  <- forecast(ar_cac, h = n_forecast)$mean
ar_pred_ftse <- forecast(ar_ftse, h = n_forecast)$mean

# Function to calculate RMSE
calc_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Comparison Table
indices <- c("DAX", "SMI", "CAC", "FTSE")
rmse_var <- c()
rmse_ar  <- c()

for(i in indices) {
  # VAR prediction extraction
  var_p <- var_pred$fcst[[i]][, "fcst"]
  
  # AR prediction extraction
  if(i == "DAX") ar_p <- ar_pred_dax
  if(i == "SMI") ar_p <- ar_pred_smi
  if(i == "CAC") ar_p <- ar_pred_cac
  if(i == "FTSE") ar_p <- ar_pred_ftse
  
  actual <- test_data[, i]
  
  rmse_var <- c(rmse_var, calc_rmse(actual, var_p))
  rmse_ar  <- c(rmse_ar, calc_rmse(actual, ar_p))
}

comparison_df <- data.frame(
  Index = indices,
  RMSE_Proposed_VAR = round(rmse_var, 4),
  RMSE_Baseline_AR = round(rmse_ar, 4),
  Improvement_Pct = round((1 - rmse_var/rmse_ar) * 100, 2)
)

print(comparison_df)
write.csv(comparison_df, "simulation_results/03_forecast_accuracy_table.csv")

cat("\nINTERPRETATION: Positive Improvement % indicates the VAR model reduces error compared to the generic model.\n")

# --- 6. Structural Analysis (IRF & Causality) ---
cat("\n--- 5. Structural Analysis: Impulse Response Functions ---\n")

# Plot IRFs: How does a shock in DAX affect others?
# We extract data manually to make prettier ggplot graphs than standard R base plots
irf_dax <- irf(var_model, impulse = "DAX", response = c("SMI", "CAC", "FTSE"), 
               n.ahead = 10, boot = TRUE)

# Helper to format IRF data for plotting
extract_irf_data <- function(irf_obj, impulse_name) {
  responses <- irf_obj$irf[[impulse_name]]
  lower <- irf_obj$Lower[[impulse_name]]
  upper <- irf_obj$Upper[[impulse_name]]
  
  time_steps <- 0:(nrow(responses)-1)
  df_list <- list()
  
  for(col in colnames(responses)) {
    tmp <- data.frame(
      Lag = time_steps,
      Response = responses[, col],
      Lower = lower[, col],
      Upper = upper[, col],
      Target = col
    )
    df_list[[col]] <- tmp
  }
  do.call(rbind, df_list)
}

df_irf <- extract_irf_data(irf_dax, "DAX")

p3 <- ggplot(df_irf, aes(x = Lag, y = Response)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "red", alpha = 0.2) +
  geom_line(color = "darkred", size = 1) +
  facet_wrap(~Target, scales = "fixed") +
  theme_bw() +
  labs(title = "Impulse Response Analysis: Shocks from DAX (Germany)",
       subtitle = "Response of other markets to a 1 S.D. innovation in DAX Returns",
       y = "Response (Log-Returns)", x = "Days after Shock")

ggsave("simulation_results/04_irf_dax_shock.png", p3, width = 10, height = 5)
print(p3)

cat("\n--- 6. Granger Causality Tests ---\n")
# Test if DAX Granger-causes CAC
gc_dax_cac <- causality(var_model, cause = "DAX")
print(gc_dax_cac$Granger)

# Test if FTSE Granger-causes SMI
gc_ftse_smi <- causality(var_model, cause = "FTSE")
print(gc_ftse_smi$Granger)

# Capture results in a dataframe for display
causality_results <- data.frame(
  Null_Hypothesis = c("DAX does not Granger-cause other variables", 
                      "FTSE does not Granger-cause other variables"),
  F_Statistic = c(gc_dax_cac$Granger$statistic, gc_ftse_smi$Granger$statistic),
  P_Value = c(gc_dax_cac$Granger$p.value, gc_ftse_smi$Granger$p.value),
  Conclusion = c(ifelse(gc_dax_cac$Granger$p.value < 0.05, "Reject H0 (Causal)", "Fail to Reject"),
                 ifelse(gc_ftse_smi$Granger$p.value < 0.05, "Reject H0 (Causal)", "Fail to Reject"))
)

print(causality_results)
write.csv(causality_results, "simulation_results/05_granger_causality.csv")

cat("\n=================================================================\n")
cat("  SIMULATION COMPLETE \n")
cat("  All artifacts saved to: /simulation_results/ \n")
cat("=================================================================\n")

