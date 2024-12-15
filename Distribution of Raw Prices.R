# Load required libraries
library(quantmod)
library(ggplot2)
library(reshape2)

# Define the function to plot ECDF of returns
plot_stock_ecdf <- function(stocks, start_date, end_date) {
  # Initialize an empty list to store returns
  raw_list <- list()
  
  # Loop through each stock symbol to get data and calculate returns
  for (stock in stocks) {
    # Retrieve stock data
    stock_data <- getSymbols(stock, src = 'yahoo', auto.assign = FALSE, from = start_date, to = end_date)
    
    # Extract the Open prices
    raw_prices <- stock_data[, paste0(stock, ".Open")]
    
    # Store the returns in the list
    raw_list[[stock]] <- raw_prices
  }
  
  # Combine returns into a single data frame
  raw_df <- do.call(merge, raw_list)
  colnames(raw_df) <- stocks
  
  # Convert to a data frame for ggplot2
  raw_df <- data.frame(Date = index(raw_df), coredata(raw_df))
  raw_melted <- melt(raw_df, id.vars = 'Date', variable.name = 'Stock', value.name = 'Returns')
  
  # Plot the ECDF using ggplot2
  ggplot(raw_melted, aes(x = Returns, color = Stock)) +
    stat_ecdf(geom = "step") +
    labs(
      title = paste("ECDF of Raw Prices for", paste(stocks, collapse = ", ")),
      x = "Daily Prices",
      y = "ECDF",
      color = "Stock"
    ) +
    theme_minimal() +
    scale_color_manual(values = rainbow(length(stocks))) +
    theme(
      legend.title = element_text(size = 16),   # Increase legend title size
      legend.text = element_text(size = 14),    # Increase legend item text size
      axis.title.x = element_text(size = 16),   # Increase X-axis title size
      axis.title.y = element_text(size = 16),   # Increase Y-axis title size
      axis.text.x = element_text(size = 12),    # Increase X-axis tick size
      axis.text.y = element_text(size = 12),    # Increase Y-axis tick size
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # Center and bold plot title
    )
}

# Example Usage
stocks <- c('UL', 'KO', 'PEP')
plot_stock_ecdf(stocks, '2015-01-01', '2020-01-01')