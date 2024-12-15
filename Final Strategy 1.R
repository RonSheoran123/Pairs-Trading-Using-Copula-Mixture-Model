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

# Function to fit copulas and return the best-fit copula
compare_copulas <- function(df) {
  data_matrix <- as.matrix(df)
  u <- pobs(data_matrix)
  
  best_copula <- NULL
  best_loglik <- -Inf
  best_copula_name <- NULL
  
  copula_models <- list(
    Gaussian = normalCopula(),
    Gumbel = gumbelCopula(),
    Clayton = claytonCopula()
  )
  
  for (copula_name in names(copula_models)) {
    copula_model <- copula_models[[copula_name]]
    
    tryCatch({
      fitted_copula <- fitCopula(copula_model, u, method = "ml")
      loglik <- logLik(fitted_copula)
      
      if (loglik > best_loglik) {
        best_loglik <- loglik
        best_copula <- fitted_copula
        best_copula_name <- copula_name
      }
      
    }, error = function(e) {
      warning(paste("Error fitting", copula_name, "copula:", e$message))
    })
  }
  
  if (is.null(best_copula)) {
    stop("Failed to fit any copula to the data.")
  }
  
  cat("Best fitted copula based on log-likelihood:", best_copula_name, "\n")
  return(best_copula)
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
  
  cop <- compare_copulas(tail(total_df, n=window))@copula
  count <- 0
  time_fitted <- 1
  
  for (i in 2:nrow(test_df)) {
    if (count >= window) {
      cop <- compare_copulas(tail(total_df, n=window))@copula
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

stocks <- c("AMZN", "ADBE")
test_start <- '2021-01-01'
test_end <- '2022-12-31'
buffer_start <- '2019-01-01'
buffer_end <- '2020-12-31'
p1 <- 0.1
p2 <- 0.9
window <- 250
multiple <- 36.5

total_df <- extract_df(stocks, buffer_start, buffer_end, "returns")
test_df <- extract_df(stocks, test_start, test_end, "raw")

backtest <- rolling(stocks, total_df, test_df, p1, p2, window, multiple)

print(backtest)