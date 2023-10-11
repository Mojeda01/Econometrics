library(moments)

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

# Task 2 - a
market_portfolio <- data$GSPC
mean(market_portfolio)
sd(market_portfolio)

# Skewness
skewness_func <- function(n, dataSeries){
  n <- length(dataSeries)
  mean_data <- mean(dataSeries)
  sd_data <- sd(dataSeries)
  skewness_value <- (n / ((n - 1) * (n - 2))) * sum(((dataSeries - mean_data) / sd_data) ^ 3)
}

#Excess Kurtosis
kurtosis_func <- function(data, n){
  n <- length(data)
  mean_data <- mean(data)
  sd_data <- sd(data)
  kurtosis_value <- ((n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))) * sum(((data - mean_data) / sd_data) ^ 4) 
  kurtosis_value <- kurtosis_value - (3 * (n - 1)^2) / ((n - 2) * (n - 3))
}
print(kurtosis_func(market_portfolio, 35))
print(skewness_func(35, market_portfolio))

# Task 2 - b
normal_dist <- rnorm(market_portfolio, mean=0.09618735, sd=0.1713567)
## Kurtosis and skewness from the normal distribution
kurtosis_norm <- kurtosis_func(normal_dist, 35)
kurtosis_norm

skewness_norm <- skewness(normal_dist)
skewness_norm

# Task 2 - c
cov_wsf_gspc <- cov(data$WFC, data$GSPC)
cov_wsf_gspc

# Task 2 - d
marketsd <- sd(data$GSPC)

beta_portfolio <- function(covariance, market_std) {
  
  # Calculate beta
  beta <- covariance / market_std
  
  return(beta)
}
WFC_beta <- beta_portfolio(cov_wsf_gspc, marketsd)

# Task 2 - e
cov_appl_gspc <- cov(data$AAPL, data$GSPC)
apple_beta <- beta_portfolio(cov_appl_gspc, marketsd)

# Task 2 - f
portfolio_2_apple = 0.75 * data$WFC + 0.25 * data$AAPL
mean(portfolio_2_apple)

# Task 2 - g
average_portfolio_return <- mean(portfolio_2_apple) # Average portfolio return
risk_free_rate <- 0.02 # Risk free rate
portfolio_beta <- 0.75 * WFC_beta + 0.25 * apple_beta # Portfolio Beta
return_sp500 <- data$GSPC # Return of S&P 500

alpha <- (average_portfolio_return - risk_free_rate) - portfolio_beta*(mean(return_sp500) - risk_free_rate)
alpha_percentage <- alpha*100
alpha_percentage

# task 2 - h
treynor_ratio <- (average_portfolio_return - risk_free_rate) / portfolio_beta
treynor_ratio


# Task 3
# Task 3 - a
# When doing a simple log of the market returns, you run the risk of NaN's produced,
# so we need a way to validate the data and then present the log return of the market
valid_data_gspc <- data$GSPC[!is.na(data$GSPC) & data$GSPC > 0]
log_values_gspc <- log(valid_data_gspc)
log_return_gspc <- mean(log_values_gspc) # log return = log(1+R)

valid_data_msft <- data$MSFT[!is.na(data$MSFT) & data$MSFT > 0]
log_values_msft <- log(valid_data_msft)
log_return_msft <- mean(log_values_msft)

print(log_return_gspc) # log return of S&P 500
print(log_return_msft) # log return of MSFT

# Task 3 - b
plot(log_values_gspc, log_values_msft, xlab="S&P 500 Log Returns", ylab="Microsoft Corporation Log Returns",
     frame.plot=FALSE , lwd=2, title("Scatterplots showing the log returns of the S&P 500 (X) and Microsoft (Y)"),
     col="lightblue")
# Task 3 - C - view document

# Task 4 - D
# For this task we are performing a linear regression where MSFT is the predictor variable and S&P 500 Index is our depedendent var
# values.
# Fit the linear regression model
linear_reg_model <- lm(data$GSPC ~ data$MSFT)
summary(linear_reg_model)




























