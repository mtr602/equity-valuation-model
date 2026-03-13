
library(quantmod)
library(PerformanceAnalytics)

# Get price data
getSymbols(c("AAPL", "SPY"), src = "yahoo", from = "2018-01-01")

# Compute daily returns
aapl_ret <- dailyReturn(Ad(AAPL))
spy_ret  <- dailyReturn(Ad(SPY))

# Align datasets
returns <- na.omit(merge(aapl_ret, spy_ret))
colnames(returns) <- c("AAPL", "SPY")

# Run regression
model <- lm(AAPL ~ SPY, data = returns)

# Beta
beta <- coef(model)[2]

beta