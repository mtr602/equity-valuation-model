library(quantmod)
library(PerformanceAnalytics)

# Get price data
getSymbols(c("AAPL", "SPY"), src = "yahoo", from = "2018-01-01")

# Convert daily prices to weekly prices
aapl_weekly <- to.weekly(AAPL)
spy_weekly  <- to.weekly(SPY)

# Compute weekly returns from adjusted close
aapl_ret <- weeklyReturn(Ad(aapl_weekly))
spy_ret  <- weeklyReturn(Ad(spy_weekly))

# Align datasets
returns <- na.omit(merge(aapl_ret, spy_ret))
colnames(returns) <- c("AAPL", "SPY")

# Use last 150 weeks (~3 years) to avoid too much COVID distortion
returns_recent <- tail(returns, 150)

# Run regression
model <- lm(AAPL ~ SPY, data = returns_recent)

# Beta
beta <- coef(model)[2]

# Print beta
print(beta)

# Optional regression summary
summary(model)