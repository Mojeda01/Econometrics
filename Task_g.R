apple <- data$AAPL
microsoft <- data$MSFT
ibm <- data$IBM
jpm <- data$JPM
wfc <- data$WFC

# Fit the linear regression model
model <- lm(data$ ~ GSPC, data=data)

# Extract coefficients and standard errors
beta_hat <- coef(model)["GSPC"]
se_beta_hat <- summary(model)$coefficients["GSPC", "Std. Error"]

# Calculate the t-statistic for the hypothesis H0: beta = 1
t_statistic <- (beta_hat - 1) / se_beta_hat

# Print the t-statistic
print(t_statistic)

# Check if we should reject the null hypothesis at 5% significance level
critical_t <- qt(0.975, df.residual(model))  # Two-tailed test, so we use 0.975 for 95% confidence
if (abs(t_statistic) > critical_t) {
  print("Reject the null hypothesis that beta is 1.")
} else {
  print("Fail to reject the null hypothesis that beta is 1.")
}