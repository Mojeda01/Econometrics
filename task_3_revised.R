apple <- data$AAPL
microsoft <- data$MSFT
ibm <- data$IBM
jpm <- data$JPM
wfc <- data$WFC
gspc <- data$GSPC #S&P500

# 3.a Compute the log return for GSPC and the log return for MSFT in percent.
# Report the arithmetic mean of each series.
# Calculate the sequence of Pt values based on cumulative returns
data$GSPC_Pt <- cumprod(1 + data$GSPC)
data$MSFT_Pt <- cumprod(1 + data$MSFT)

# Compute the log returns
data$GSPC_Log_Return <- c(NA, diff(log(data$GSPC_Pt))) * 100
data$MSFT_Log_Return <- c(NA, diff(log(data$MSFT_Pt))) * 100

# Calculate the arithmetic means
mean_GSPC <- mean(data$GSPC_Log_Return, na.rm = TRUE)
mean_MSFT <- mean(data$MSFT_Log_Return, na.rm = TRUE)

# Print the results
print(paste("Arithmetic mean of log return for GSPC:", mean_GSPC))
print(paste("Arithmetic mean of log return for MSFT:", mean_MSFT))

# 3.b Create a scatterplot with log returns to GSPC on the horizontal axis and log
# returns to MSFT on the vertical axis. Include labels and a title.
plot(data$GSPC_Log_Return, data$MSFT_Log_Return, 
     main="Scatterplot of Log Returns: GSPC vs. MSFT",
     xlab="Log Returns to GSPC", 
     ylab="Log Returns to MSFT", 
     pch=19, col="blue", cex=0.5)

# Add a grid for better visualization
grid(col="gray")

# Task 3.c already done

# Task 3.d - Estimate the beta for MSFT using the lm() function in R. Copy your regression
# results into your solution paper.

# Perform linear regression
regression_model <- lm(MSFT_Log_Return ~ GSPC_Log_Return, data=data, na.action=na.omit)

# Display the summary of the regression
summary(regression_model)















