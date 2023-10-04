apple <- data$AAPL
microsoft <- data$MSFT
ibm <- data$IBM
jpm <- data$JPM
wfc <- data$WFC
gspc <- data$GSPC

# Finding the frequencies of the all the datasets
# Create a data frame
df <- data.frame(Category = data$AAPL)

# Find frequency
freq_df <- table(df$Category)

# Print frequency
print(freq_df)

# Frequency number = 1

# Finding Frequence for the rest
frequency(apple)
frequency(microsoft)
frequency(ibm)
frequency(jpm)
frequency(wfc)
frequency(gspc)

# Calculations of the mean of the 5 stock price data sets
mean_appl <- mean(apple)
mean_msft <- mean(microsoft)
mean_ibm <- mean(ibm)
mean_jpm <- mean(jpm)
mean_wfc <- mean(wfc)
mean_gspc <- mean(gspc)

highest_return <-max(mean_appl, mean_msft, mean_ibm, mean_jpm,
                     mean_wfc, mean_gspc)
highest_return # mean_apple

print(mean_appl)
print(mean_msft)
print(mean_ibm)
print(mean_jpm)
print(mean_wfc)
print(mean_gspc)

# Finding the Covariance of AAPL of MSFT
cov(data$AAPL, data$MSFT)

# Correlation between APPL and JPM
cor(data$AAPL, data$JPM)

# Time series Portfolio
apple_timeSeries <- data$AAPL
jpm_timeSeries <- data$JPM

# Task K
# Construct the Portfolio
portfolio = 0.5 * apple + 0.5 * jpm
mean(apple)
mean(jpm)
sd(apple)
sd(jpm)

# Task 2
market_portfolio <- data$GSPC
mean(market_portfolio)
sd(market_portfolio)
skewness <- function(n, dataSeries){
  n <- length(dataSeries)
  mean_data <- mean(dataSeries)
  sd_data <- sd(dataSeries)
  skewness_value <- (n / ((n - 1) * (n - 2))) * sum(((dataSeries - mean_data) / sd_data) ^ 3)
}
print(skewness(35, market_portfolio))

kurtosis_func <- function(data, n){
  n <- length(data)
  mean_data <- mean(data)
  sd_data <- sd(data)
  kurtosis_value <- ((n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))) * sum(((data - mean_data) / sd_data) ^ 4) 
  kurtosis_value <- kurtosis_value - (3 * (n - 1)^2) / ((n - 2) * (n - 3))
}
print(kurtosis_func(market_portfolio, 35))
