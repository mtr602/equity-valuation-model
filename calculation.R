
library(tidyverse)
library(tidyquant)
library(quantmod)
library(ggplot2)
library(lubridate)
library(PerformanceAnalytics)
library(dplyr)
library(httr)
library(jsonlite)

# Core Calculation 


financials_clean <- financials_clean %>%
  mutate(
    tax_rate = tax_expense / ebit,
    nopat = ebit * (1 - tax_rate),
    fcff = nopat + depreciation - capex - change_wc
  )

print(financials_clean)

# Forecast Future FCFF 


forecast_years <- 5

# Historical assumptions
last_year <- max(as.numeric(financials_clean$year), na.rm = TRUE)
last_revenue <- financials_clean %>% slice_tail(n = 1) %>% pull(revenue)

revenue_first <- financials_clean %>% slice_head(n = 1) %>% pull(revenue)
revenue_last  <- financials_clean %>% slice_tail(n = 1) %>% pull(revenue)
n_periods <- nrow(financials_clean) - 1

revenue_cagr <- (revenue_last / revenue_first)^(1 / n_periods) - 1

ebit_margin <- financials_clean %>%
  summarise(value = mean(ebit / revenue, na.rm = TRUE)) %>%
  pull(value)

tax_rate_avg <- financials_clean %>%
  summarise(value = mean(tax_rate, na.rm = TRUE)) %>%
  pull(value)

depreciation_ratio <- financials_clean %>%
  summarise(value = mean(depreciation / revenue, na.rm = TRUE)) %>%
  pull(value)

capex_ratio <- financials_clean %>%
  summarise(value = mean(capex / revenue, na.rm = TRUE)) %>%
  pull(value)

wc_ratio <- financials_clean %>%
  summarise(value = mean(change_wc / revenue, na.rm = TRUE)) %>%
  pull(value)

# Forecast table
forecast_tbl <- tibble(
  year = (last_year + 1):(last_year + forecast_years),
  forecast_index = 1:forecast_years
) %>%
  mutate(
    revenue = last_revenue * (1 + revenue_cagr)^forecast_index,
    ebit = revenue * ebit_margin,
    tax_rate = tax_rate_avg,
    nopat = ebit * (1 - tax_rate),
    depreciation = revenue * depreciation_ratio,
    capex = revenue * capex_ratio,
    change_wc = revenue * wc_ratio,
    fcff = nopat + depreciation - capex - change_wc
  ) %>%
  select(
    year,
    revenue,
    ebit,
    tax_rate,
    nopat,
    depreciation,
    capex,
    change_wc,
    fcff
  )

# Cost of Equity

risk_free_rate <- 0.04      # 4% risk-free rate (10-year treasury approx)
market_return  <- 0.09      # assumed long-run market return 9%

cost_of_equity <- risk_free_rate + beta * (market_return - risk_free_rate)

print(cost_of_equity)


# WACC Calculation

# Assumptions
cost_of_debt <- 0.04   # assumed cost of debt (can refine later)
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


