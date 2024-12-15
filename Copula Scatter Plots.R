library(quantmod)
library(copula)
library(ggplot2)
library(gridExtra)  # For arranging multiple ggplots

# Function to fit the copula and return pseudo-observations
fit_copula <- function(tickers, start_date, end_date, copula_type) {
  # Fetch stock data
  getSymbols(tickers, src = "yahoo", from = start_date, to = end_date)
  
  # Merge and extract closing prices
  stock_data <- merge(Cl(get(tickers[1])), Cl(get(tickers[2])))
  colnames(stock_data) <- tickers
  
  # Calculate log returns
  calculate_log_returns <- function(data) {
    log_returns <- diff(log(data))
    return(as.data.frame(log_returns[-1, ]))  # Remove the first NA row
  }
  
  log_returns <- calculate_log_returns(stock_data)
  
  # Transform returns to uniform margins using pobs (pseudo-observations)
  u <- pobs(log_returns)
  
  # Fit the specified copula
  if (copula_type == "gaussian") {
    fitted_copula <- fitCopula(normalCopula(dim = 2), u, method = "ml")
  } else if (copula_type == "gumbel") {
    fitted_copula <- fitCopula(gumbelCopula(dim = 2), u, method = "ml")
  } else if (copula_type == "clayton") {
    fitted_copula <- fitCopula(claytonCopula(dim = 2), u, method = "ml")
  } else {
    stop("Invalid copula type specified.")
  }
  
  # Loglikelihood values
  log_likeli <- logLik(fitted_copula)
  return(list(cop = fitted_copula, density = u, log = log_likeli))
}

# Function to plot scatter and density of a fitted copula
plot_scatter_with_density <- function(randoms, copula_name) {
  # Convert the random samples to a data frame for ggplot
  randoms_df <- as.data.frame(randoms)
  colnames(randoms_df) <- c("V", "MA")
  
  # Plotting with ggplot2
  ggplot(randoms_df, aes(x = V, y = MA)) +
    geom_point(alpha = 0.5) +
    labs(title = copula_name,
         x = "Random Samples of V",
         y = "Random Samples of MA") +
    theme_minimal()
}

# Specify tickers and date range
tickers <- c("V", "MA")
start_date <- "2020-01-01"
end_date <- "2024-01-01"

# Fit copulas and get pseudo-observations
gaussian_res <- fit_copula(tickers, start_date, end_date, "gaussian")
gumbel_res <- fit_copula(tickers, start_date, end_date, "gumbel")
clayton_res <- fit_copula(tickers, start_date, end_date, "clayton")

# Plot densities for each copula
gaussian_plot <- plot_scatter_with_density(rCopula(1000, gaussian_res$cop@copula),"Gaussian Copula")
gumbel_plot <- plot_scatter_with_density(rCopula(1000, gumbel_res$cop@copula),"Gumbel Copula")
clayton_plot <- plot_scatter_with_density(rCopula(1000, clayton_res$cop@copula),"Clayton Copula")

# Combine plots into one figure
combined_plot <- grid.arrange(gaussian_plot, gumbel_plot, clayton_plot, nrow=1, top="Scatter plots")

# Extract log-likelihood values
log_likelihoods <- data.frame(
  Copula = c("Gaussian", "Gumbel", "Clayton"),
  LogLikelihood = c(gaussian_res$log, gumbel_res$log, clayton_res$log)
)

# Print the loglikelihood values
print(log_likelihoods)

# Print the combined plot
print(combined_plot)