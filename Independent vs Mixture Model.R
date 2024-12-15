library(quantmod)
library(copula)
library(urca)
library(pracma)

# Function to fetch stock data,
extract_df <- function(stocks, start_date, end_date){
  adjusted_col1 <- paste0(stocks[1], '.Open')
  adjusted_col2 <- paste0(stocks[2], '.Open')
  
  # Fetch the adjusted prices for the given periods
  s1_data <- getSymbols(stocks[1], src='yahoo', auto.assign=FALSE, from=start_date, to=end_date)
  s2_data <- getSymbols(stocks[2], src='yahoo', auto.assign=FALSE, from=start_date, to=end_date)
  
  # Calculate daily returns for stock 1 and stock 2 (only for training and buffer data)
  s1_returns <- dailyReturn(s1_data[, adjusted_col1])
  s2_returns <- dailyReturn(s2_data[, adjusted_col2])
  
  # Combine the returns into data frames for training and buffer periods
  data <- data.frame(s1 = s1_returns, s2 = s2_returns)
  
  return(data)
}

# EM Algorithm
em <- function(data) {
  # Step 1: Calculate log returns
  calculate_log_returns <- function(data) {
    log_returns <- diff(log(data))
    return(as.data.frame(log_returns[-1, ]))  # Remove the first NA row
  }
  
  #log_returns <- calculate_log_returns(stock_data)
  
  # Transform returns to uniform margins using pobs (pseudo-observations)
  u <- pobs(data)
  
  # Step 2: Initialize the copulas
  gaussian_cop <- normalCopula(dim = 2)
  gumbel_cop <- gumbelCopula(dim = 2)
  clayton_cop <- claytonCopula(dim = 2)
  
  # Initial weights for each copula
  initial_weights <- c(1/3, 1/3, 1/3)
  
  # Initial parameter estimates for each copula based on the uniform data
  initial_params <- list(
    gaussian = fitCopula(gaussian_cop, u, method = "ml")@estimate,
    gumbel = fitCopula(gumbel_cop, u, method = "ml")@estimate,
    clayton = fitCopula(clayton_cop, u, method = "ml")@estimate
  )
  
  # Step 3: Function to calculate the density for each copula
  copula_density <- function(u, copula, params) {
    copula <- setTheta(copula, params)  # Set the copula parameters
    return(dCopula(u, copula))
  }
  
  # Step 4: E-step - Calculate responsibilities (gamma values)
  calc_responsibilities <- function(u, weights, params) {
    n <- nrow(u)
    K <- length(weights)  # Number of copulas
    gamma <- matrix(0, n, K)
    
    # Densities for each copula
    densities <- list(
      gaussian = copula_density(u, gaussian_cop, params$gaussian),
      gumbel = copula_density(u, gumbel_cop, params$gumbel),
      clayton = copula_density(u, clayton_cop, params$clayton)
    )
    
    # Calculate responsibilities
    for (k in 1:K) {
      gamma[, k] <- weights[k] * densities[[k]]
    }
    
    # Normalize to make them probabilities
    gamma <- gamma / rowSums(gamma)
    return(gamma)
  }
  
  # Step 5: M-step - Update weights and parameters
  update_parameters <- function(u, gamma) {
    new_weights <- colMeans(gamma)
    
    new_params <- list()
    new_params$gaussian <- fitCopula(gaussian_cop, u, weights = gamma[, 1], method = "ml")@estimate
    new_params$gumbel <- fitCopula(gumbel_cop, u, weights = gamma[, 2], method = "ml")@estimate
    new_params$clayton <- fitCopula(clayton_cop, u, weights = gamma[, 3], method = "ml")@estimate
    
    return(list(weights = new_weights, params = new_params))
  }
  
  # Step 6: Function to calculate the log-likelihood of the mixed copula model
  calc_log_likelihood <- function(u, weights, params) {
    densities <- list(
      gaussian = copula_density(u, gaussian_cop, params$gaussian),
      gumbel = copula_density(u, gumbel_cop, params$gumbel),
      clayton = copula_density(u, clayton_cop, params$clayton)
    )
    
    # Mixed density based on weights
    mixed_density <- weights[1] * densities$gaussian + 
      weights[2] * densities$gumbel + 
      weights[3] * densities$clayton
    
    # Log-likelihood
    log_likelihood <- sum(log(mixed_density))
    return(log_likelihood)
  }
  
  # Step 7: EM algorithm implementation
  em_algorithm <- function(u, initial_weights, initial_params, tol = 1e-6, max_iter = 100) {
    weights <- initial_weights
    params <- initial_params
    log_likelihoods <- c()
    
    for (iter in 1:max_iter) {
      # E-step
      gamma <- calc_responsibilities(u, weights, params)
      
      # M-step
      updates <- update_parameters(u, gamma)
      weights <- updates$weights
      params <- updates$params
      
      # Calculate log-likelihood
      log_likelihood <- calc_log_likelihood(u, weights, params)
      log_likelihoods <- c(log_likelihoods, log_likelihood)
      
      # Check for convergence
      if (iter > 1 && abs(log_likelihoods[iter] - log_likelihoods[iter - 1]) < tol) {
        cat("Convergence reached at iteration:", iter, "\n")
        break
      }
    }
    
    return(list(weights = weights, params = params, log_likelihood = log_likelihoods))
  }
  
  # Run the EM algorithm
  result <- em_algorithm(u, initial_weights, initial_params)
  
  
  # Calculate final AIC using the last log-likelihood and optimized weights
  final_log_likelihood <- calc_log_likelihood(u, result$weights, result$params)
  num_params <- 3  # Three weights to estimate
  final_aic <- -2 * final_log_likelihood + 2 * num_params
  
  # Return results
  return(list(
    weights = result$weights,
    final_params = result$params,
    logLik = final_log_likelihood
  ))
}

# Fit copulas, and return the best-fitted copula with error handling
compare_copulas <- function(df) {
  data_matrix <- as.matrix(df)
  
  # Transform the data to uniform margins
  u <- pobs(data_matrix)
  
  # Initialize variables to store best copula details
  best_copula <- NULL
  best_loglik <- -Inf
  best_copula_name <- NULL
  
  # Define a list of copula models to try
  copula_models <- list(
    Gaussian = normalCopula(),
    Gumbel = gumbelCopula(),
    Clayton = claytonCopula()
  )
  
  # Fit each copula and calculate log-likelihood
  for (copula_name in names(copula_models)) {
    copula_model <- copula_models[[copula_name]]
    
    tryCatch({
      fitted_copula <- fitCopula(copula_model, u, method = "ml")
      loglik <- logLik(fitted_copula)
      
      # Check if this copula is the best so far
      if (loglik > best_loglik) {
        best_loglik <- loglik
        best_copula <- fitted_copula
        best_copula_name <- copula_name
      }
      
    }, error = function(e) {
      warning(paste("Error fitting", copula_name, "copula:", e$message))
    })
  }
  
  # Check if a best copula was found
  if (is.null(best_copula)) {
    stop("Failed to fit any copula to the data.")
  }
  
  # Output and return the best copula
  #cat("Best fitted copula based on log-likelihood:", best_copula_name, "\n")
  
  return(list(best_fit_copula=best_copula, best_fit_name=best_copula_name))
}

selected_pairs <- list(
  c("V", "MA"),
  c("MSFT", "MA"),
  c("MSFT", "V"),
  c("AMZN", "ADBE"),
  c("V", "ACN"),
  c("V", "LIN"),
  c("V", "ADBE"),
  c("MSFT", "LIN"),
  c("MA", "ACN"),
  c("MA", "ADBE"),
  c("MA", "LIN"),
  c("MSFT", "ACN"),
  c("PYPL", "ADBE"),
  c("ACN", "LIN"),
  c("MSFT", "NEE"),
  c("V", "PYPL"),
  c("V", "NEE"),
  c("MSFT", "ADBE"),
  c("MA", "NEE")
)

start_date <- "2020-01-01"
end_date <- "2023-01-01"
final_result <- data.frame(Stock1=character(), Stock2=character(), LogLikelihood=numeric(), Best_fit=character(), best_fit_logLikelihood=numeric(), Mixture_logLikelihood=numeric())

for (pair in selected_pairs){
  df <- extract_df(pair, start_date, end_date)
  
  em_final <- em(df)
  independent_final <- compare_copulas(df)
  
  # Store the results in a temporary data frame
  temp_result <- data.frame(
    Stock1 = pair[1],
    Stock2 = pair[2],
    Best_fit = independent_final$best_fit_name,
    best_fit_logLikelihood = logLik(independent_final$best_fit_copula),
    Mixture_logLikelihood = em_final$logLik
  )
  
  # Append the temporary result to the list
  final_result <- rbind(final_result, temp_result)
}

print(final_result)

# Export the data frame to a CSV file
write.csv(final_result, file = "Independent vs Mixture Model.csv", row.names = FALSE)