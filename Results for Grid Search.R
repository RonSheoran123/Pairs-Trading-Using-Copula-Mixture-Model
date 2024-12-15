library(quantmod)
library(copula)
library(urca)

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



mixed_copula <- function(data) {
  data_matrix <- as.matrix(data)
  
  # Transform the data to uniform margins
  u <- pobs(data_matrix)
  # Fit Gaussian, Gumbel, and Clayton copulas
  gaussian_fit <- gaussian_copula(u)
  gumbel_fit <- gumbel_copula(u)
  clayton_fit <- clayton_copula(u)
  
  # Initialize variables to track the best weights and log-likelihood
  w1 <- 0.01
  w2 <- 0.01
  w1_f <- 0
  w2_f <- 0
  w3_f <- 0
  max_logLik <- -1e9
  
  # Iterate over weight combinations for w1 and w2
  while (w1 < 1) {
    w2 <- 0.01  # Reset w2 for each w1 iteration
    while (w2 < 1) {
      if (w1 + w2 < 1) {
        w3 <- 1 - w1 - w2
        
        mix <- mixCopula(c(gaussian_fit$copula, gumbel_fit$copula, clayton_fit$copula), w=c(w1,w2,w3))
        
        total_logLik <- loglikCopula(getTheta(mix), u, mix)
        
        if(total_logLik > max_logLik){
          w1_f <- w1
          w2_f <- w2
          w3_f <- w3
          max_logLik <- total_logLik
        }
      }
      w2 <- w2 + 0.01
    }
    w1 <- w1 + 0.01
  }
  
  return(list(logLik = max_logLik,
              weights = c(w1_f, w2_f, w3_f)))
}

selected_pairs <- list(c("V", "MA"), c("AAPL","AMZN"))
start_date <- "2020-01-01"
end_date <- "2023-01-01"
final_result_list <- list()  # Initialize a list to store results

for (pair in selected_pairs){
  df <- extract_df(pair, start_date, end_date)
  
  start_time <- Sys.time()
  final <- mixed_copula(df)
  end_time <- Sys.time()
  
  # Store the results in a temporary data frame
  temp_result <- data.frame(
    Stock1 = pair[1],
    Stock2 = pair[2],
    w1 = final$weights[1],
    w2 = final$weights[2],
    w3 = final$weights[3],
    LogLikelihood = final$logLik,
    Time_taken = end_time - start_time
  )

  # Append the temporary result to the list
  final_result_list[[length(final_result_list) + 1]] <- temp_result
}
print(final_result_list)