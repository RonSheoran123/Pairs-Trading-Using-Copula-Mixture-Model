library(quantmod)
library(copula)
library(urca)
library(KFAS)
library(pracma)

# Transaction Fee Parameters
fixed_fee <- 1  # Fixed fee per transaction
percentage_fee <- 0.001  # Percentage of the value of the trade

# Risk-Free Rate Parameters (e.g., 3% annual rate)
risk_free_annual <- 0.03

# Function to fetch stock data
extract_df <- function(stocks, start, end, type) {
  adjusted_col1 <- paste0(stocks[1], '.Open')
  adjusted_col2 <- paste0(stocks[2], '.Open')
  
  data1 <- getSymbols(stocks[1], src='yahoo', auto.assign=FALSE, from=start, to=end)
  data2 <- getSymbols(stocks[2], src='yahoo', auto.assign=FALSE, from=start, to=end)
  
  if (type=="raw") {
    df <- data.frame(s1=data1, s2=data2)
  }
  else {
    returns1 <- dailyReturn(data1[, adjusted_col1])
    returns2 <- dailyReturn(data2[, adjusted_col2])
    df <- data.frame(s1=returns1, s2=returns2)
  }
  
  return(df)
}

# Function to compute transaction fee for a trade
calculate_transaction_fee <- function(trade_value) {
  fee <- fixed_fee + (trade_value * percentage_fee)
  return(fee)
}

# Function for Gaussian Copula
gaussian_copula <- function(u) {
  gaussian_cop <- normalCopula(dim = 2)
  fit_gaussian <- fitCopula(gaussian_cop, u, method = "ml")
  
  # Extract goodness of fit metrics
  return(list(copula = fit_gaussian@copula,
              logLik = logLik(fit_gaussian)))
}

# Function for Gumbel Copula
gumbel_copula <- function(u) {
  gumbel_cop <- gumbelCopula()
  fit_gumbel <- fitCopula(gumbel_cop, u, method = "ml")
  
  # Extract goodness of fit metrics
  return(list(copula = fit_gumbel@copula,
              logLik = logLik(fit_gumbel)))
}

clayton_copula <- function(u) {
  clayton_cop <- claytonCopula()
  fit_gumbel <- fitCopula(clayton_cop, u, method = "ml")
  
  # Extract goodness of fit metrics
  return(list(copula = fit_gumbel@copula,
              logLik = logLik(fit_gumbel)))
}

calculate_log_returns <- function(data) {
  log_returns <- diff(log(data))
  return(as.data.frame(log_returns[-1, ]))  # Remove the first NA row
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

# Rolling function with transaction fee, volatility, and Sharpe ratio including risk-free rate
rolling <- function(stocks, total_df, test_df, p1, p2, window, multiple) {
  conditional_probability <- function(x, y, var, copula) {
    f_x_y <- pCopula(c(x, y), copula=copula)
    if(var == 1) {
      f_y <- pCopula(c(1, y), copula=copula)
      return(f_x_y / f_y)
    } else {
      f_x <- pCopula(c(x, 1), copula=copula)
      return(f_x_y / f_x)
    }
  }
  
  cointegration_test <- function(df) {
    jtest <- ca.jo(df, type = "trace", ecdet = "const", K = 2)
    summary(jtest)
    hedge_ratio <- -jtest@V[, 1][2] / jtest@V[, 1][1]
    return(hedge_ratio)
  }
  
  money <- 100
  position <- 0
  stocks_of_s1 <- 0
  stocks_of_s2 <- 0
  equity <- c(money)
  transaction_fees <- c()
  return_matrix <- matrix(, nrow=nrow(test_df), ncol=5)
  
  weights <- em(tail(total_df, n=window))$weights
  u <- pobs(tail(total_df, n=window))
  cop <- mixCopula(c(gaussian_copula(u)$copula, 
                     gumbel_copula(u)$copula, 
                     clayton_copula(u)$copula), 
                     weights)
  count <- 0
  time_fitted <- 1
  
  for (i in 2:nrow(test_df)) {
    if (count >= window) {
      weights <- em(tail(total_df, n=window))$weights
      u <- pobs(tail(total_df, n=window))
      cop <- mixCopula(c(gaussian_copula(u)$copula, 
                         gumbel_copula(u)$copula, 
                         clayton_copula(u)$copula), 
                       weights)
      count <- 0
      time_fitted <- time_fitted + 1
    } else {
      count <- count + 1
    }
    
    current_return1 <- (test_df[i, 1] / test_df[i-1, 1]) - 1
    current_return2 <- (test_df[i, 2] / test_df[i-1, 2]) - 1
    new_row <- data.frame(s1=current_return1, s2=current_return2)
    colnames(new_row) <- colnames(total_df)
    total_df <- rbind(total_df, new_row)
    
    hedge_ratio <- cointegration_test(tail(total_df, n=window))
    mat <- pobs(tail(total_df, n=window))
    
    condition1 <- conditional_probability(mat[nrow(mat), 1], mat[nrow(mat), 2], 1, cop)
    condition2 <- conditional_probability(mat[nrow(mat), 1], mat[nrow(mat), 2], 2, cop)
    
    return_matrix[i, 1] <- condition1
    return_matrix[i, 2] <- condition2
    return_matrix[i, 3] <- hedge_ratio
    #return_matrix[i, 4] <- logLik(compare_copulas(tail(total_df, n=window)))
    
    # Opening rules
    if ((condition1 <= p1 && condition2 >= p2) && position == 0) {
      trade_value <- as.numeric((test_df[i, 1] * multiple) + (hedge_ratio * multiple * test_df[i, 2]))
      fee <- calculate_transaction_fee(trade_value)
      stocks_of_s1 <- stocks_of_s1 + (1 * multiple)
      stocks_of_s2 <- stocks_of_s2 - (hedge_ratio * multiple)
      money <- as.numeric(money - (test_df[i, 1] * multiple) + (hedge_ratio * multiple * test_df[i, 2]))
      position <- 1
      transaction_fees <- c(transaction_fees, fee)
      equity <- c(equity, money)
    } 
    else if ((condition1 >= p2 && condition2 <= p1) && position == 0) {
      trade_value <- as.numeric((test_df[i, 1] * multiple) + (hedge_ratio * multiple * test_df[i, 2]))
      fee <- calculate_transaction_fee(trade_value)
      stocks_of_s1 <- stocks_of_s1 - (1 * multiple)
      stocks_of_s2 <- stocks_of_s2 + (hedge_ratio * multiple)
      money <- as.numeric(money + (test_df[i, 1] * multiple) - (hedge_ratio * multiple * test_df[i, 2]))
      position <- -1
      transaction_fees <- c(transaction_fees, fee)
      equity <- c(equity, money)
    } 
    else if ((condition1 > 0.5 && condition2 > 0.5) && position != 0) {
      trade_value <- as.numeric(abs(stocks_of_s1 * test_df[i, 1]) + abs(stocks_of_s2 * test_df[i, 2]))
      fee <- calculate_transaction_fee(trade_value)
      money <- as.numeric(money + as.numeric((stocks_of_s1 * test_df[i, 1]) + (stocks_of_s2 * test_df[i, 2])) - fee)
      position <- 0
      stocks_of_s1 <- 0
      stocks_of_s2 <- 0
      equity <- c(equity, money)
    }
    return_matrix[i, 4] <- stocks_of_s1
    return_matrix[i, 5] <- stocks_of_s2
  }
  
  # Calculate volatility (standard deviation of returns)
  daily_returns <- diff(equity) / head(equity, -1)
  volatility <- sd(daily_returns)
  
  CAGR <- sqrt(money / 100) - 1 
  
  # Calculate Sharpe Ratio (with risk-free rate)
  sharpe_ratio <- CAGR * sqrt(252) / volatility
  
  return(list(
    returns = money,
    transaction_fees = transaction_fees,
    volatility = volatility,
    cagr = CAGR,
    equity = equity,
    times = time_fitted,
    sharpe = sharpe_ratio,
    daily = daily_returns,
    mat = return_matrix
  ))
}

selected_pairs <- list(
  c("AMZN", "ADBE"), c("V", "ACN"), c("V", "ADBE"), c("MA", "LIN"),
  c("V", "PYPL"), c("HON", "LIN"), c("MA", "DHR"), c("NEE", "LIN"),
  c("AMZN", "NFLX"), c("HD", "TXN"), c("MA", "COST"), c("WMT", "LIN"),
  c("V", "COST"), c("AMZN", "PYPL"), c("WMT", "ACN"), c("GOOGL", "ADBE"),
  c("HD", "LIN"), c("HD", "MA"), c("HD", "ADBE"), c("NFLX", "ADBE"),
  c("HON", "TXN"), c("MCD", "LIN"), c("UNH", "ADBE"), c("MCD", "NEE"),
  c("AMZN", "GOOGL"), c("NKE", "COST"), c("ACN", "COST"), c("HON", "DHR"),
  c("AAPL", "MSFT"), c("MCD", "ACN"), c("UNH", "NFLX"), c("WMT", "COST"),
  c("CSCO", "ACN"), c("HD", "COST"), c("CSCO", "LIN"), c("MCD", "DHR"),
  c("CSCO", "HON"), c("NKE", "NEE"), c("MCD", "UNP"), c("CSCO", "DHR"),
  c("WMT", "TXN"), c("MRK", "LLY"), c("HON", "COST"), c("NKE", "ACN"),
  c("GOOGL", "NFLX"), c("NKE", "LIN")
)



test_start <- '2021-01-01'
test_end <- '2023-01-01'
buffer_start <- '2019-01-01'
buffer_end <- '2020-12-31'
p1 <- 0.1
p2 <- 0.9
window <- 250
multiple <- 36.5


final_df <- data.frame(stock1=character(), stock2=character(), volatility=numeric(), sharpe_ratio=numeric(), cagr=numeric())

for(pair in selected_pairs){
  total_df <- extract_df(pair, buffer_start, buffer_end, "returns")
  test_df <- extract_df(pair, test_start, test_end, "raw")
  
  results <- rolling(pair, total_df, test_df, p1, p2, window, multiple)
  
  temp_df <- data.frame(
              stock1=pair[1],
              stock2=pair[2],
              volatility=results$volatility,
              sharpe_ratio=results$sharpe,
              cagr=results$cagr
  )
  
  final_df <- rbind(final_df, temp_df)
}

print(final_df)

# Export the data frame to a CSV file
write.csv(final_df, file = "Strategy Result.csv", row.names = FALSE)