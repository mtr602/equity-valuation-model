
library(tidyverse)
library(tidyquant)
library(quantmod)
library(ggplot2)
library(lubridate)
library(PerformanceAnalytics)
library(dplyr)
library(httr)
library(jsonlite)
library(purrr)

# Core Calculation 


financials_clean <- financials_clean %>%
  mutate(
    tax_rate = tax_expense / ebit,
    nopat = ebit * (1 - tax_rate),
    fcff = nopat + depreciation - capex - change_wc
  )

print(financials_clean)

# Bullish market-based FCFF forecast


forecast_years <- 5
last_year <- max(as.numeric(financials_clean$year), na.rm = TRUE)
last_revenue <- financials_clean %>% slice_tail(n = 1) %>% pull(revenue)

forecast_tbl <- tibble(
  year = (last_year + 1):(last_year + forecast_years),
  forecast_index = 1:forecast_years,
  
  # Revenue growth: bullish, but tapering toward maturity
  revenue_growth = c(0.08, 0.07, 0.06, 0.05, 0.04),
  
  # Margin expansion: slight improvement, not unrealistic
  ebit_margin = c(0.31, 0.315, 0.320, 0.322, 0.325),
  
  # Stable effective tax rate
  tax_rate = c(0.17, 0.17, 0.17, 0.17, 0.17),
  
  # Reinvestment assumptions
  depreciation_ratio = c(0.025, 0.025, 0.024, 0.024, 0.023),
  capex_ratio        = c(0.030, 0.029, 0.028, 0.028, 0.027),
  wc_ratio           = c(0.010, 0.010, 0.009, 0.009, 0.008)
) %>%
  mutate(
    revenue = accumulate(revenue_growth, ~ .x * (1 + .y), .init = last_revenue)[-1],
    ebit = revenue * ebit_margin,
    nopat = ebit * (1 - tax_rate),
    depreciation = revenue * depreciation_ratio,
    capex = revenue * capex_ratio,
    change_wc = revenue * wc_ratio,
    fcff = nopat + depreciation - capex - change_wc
  ) %>%
  select(
    year,
    revenue_growth,
    revenue,
    ebit_margin,
    ebit,
    tax_rate,
    nopat,
    depreciation_ratio,
    depreciation,
    capex_ratio,
    capex,
    wc_ratio,
    change_wc,
    fcff
  )

# Cost of Equity

risk_free_rate <- 0.04      # 4% risk-free rate (10-year treasury approx)
market_return  <- 0.08      # assumed long-run market return 9%

cost_of_equity <- risk_free_rate + beta * (market_return - risk_free_rate)

print(cost_of_equity)


# WACC Calculation

# Assumptions

interest_expense <- tail(income_data$interestExpense, 1)
debt_value <- tail(financials_clean$total_debt, 1)

cost_of_debt <- interest_expense / debt_value
print(cost_of_debt)

tax_rate_avg <- mean(financials_clean$tax_rate, na.rm = TRUE)

# Market value of equity
current_price <- as.numeric(last(Ad(AAPL)))
shares_outstanding <- tail(financials_clean$shares_outstanding, 1)

equity_value <- current_price * shares_outstanding

# Total debt
debt_value <- tail(financials_clean$total_debt, 1)

# Capital structure
V <- equity_value + debt_value
E_weight <- equity_value / V
D_weight <- debt_value / V

# WACC
wacc <- (E_weight * cost_of_equity) + (D_weight * cost_of_debt * (1 - tax_rate_avg))

print(wacc)



# Discounted Future Cash flow

forecast_tbl <- forecast_tbl %>%
  mutate(
    year_index = 1:n(),
    pv_fcff = fcff / (1 + wacc)^year_index
  )


